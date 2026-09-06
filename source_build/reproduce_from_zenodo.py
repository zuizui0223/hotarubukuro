#!/usr/bin/env python3
"""Rebuild the canonical analysis table from the frozen image-bearing Zenodo workbook.

Canonical chain:

Zenodo XLSX -> embedded images -> colour extraction -> public-table materialization
-> exact source-contract validation -> retained publication pipeline.

No derived CSV is committed as an input.  The generated table is accepted only
when it matches the frozen exact-output identity in
``reproducibility/source_contract.json``.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import subprocess
import sys
import tempfile
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from source_build.source_contract import (  # noqa: E402
    SourceContractError,
    load_contract,
    validate_public_table,
)

CONTRACT = load_contract()
ZENODO_RECORD = str(CONTRACT["zenodo_record"])
ZENODO_DOI = str(CONTRACT["zenodo_doi"])
ZENODO_FILENAME = str(CONTRACT["zenodo_filename"])
ZENODO_MD5 = str(CONTRACT["zenodo_md5"])
EXPECTED_ROWS = int(CONTRACT["expected_rows"])
ZENODO_URL = f"https://zenodo.org/records/{ZENODO_RECORD}/files/{ZENODO_FILENAME}?download=1"

DEFAULT_CACHE = ROOT / ".repro_cache" / "zenodo" / ZENODO_FILENAME
DEFAULT_EXTRACTION = ROOT / "results" / "source_reconstruction" / "colour_extraction_from_zenodo.csv"
DEFAULT_OUTPUT = ROOT / "results" / "source_reconstruction" / "Data_S1_from_zenodo.csv"
DEFAULT_QC_DIR = ROOT / "results" / "source_reconstruction" / "qc"
DEFAULT_REPORT = ROOT / "results" / "source_reconstruction" / "zenodo_rebuild_audit.json"

EXTRACTOR = ROOT / "source_build" / "extract_color.py"
BUILDER = ROOT / "source_build" / "build_data_s1.py"
PIPELINE = ROOT / "run_pipeline.py"


class BootstrapError(RuntimeError):
    pass


def checksum(path: Path, algorithm: str) -> str:
    digest = hashlib.new(algorithm)
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(4 * 1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def download(url: str, destination: Path, expected_md5: str, overwrite: bool = False) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)
    if destination.exists() and not overwrite:
        observed = checksum(destination, "md5")
        if observed == expected_md5:
            print(f"Zenodo workbook already cached and verified: {destination}")
            return
        raise BootstrapError(
            f"cached workbook checksum mismatch: {observed} != {expected_md5}; "
            "use --overwrite-download to replace it"
        )

    with tempfile.NamedTemporaryFile(
        prefix=f".{destination.name}.", suffix=".part", dir=destination.parent, delete=False
    ) as temporary:
        temp_path = Path(temporary.name)
    try:
        print(f"Downloading {url}")
        request = urllib.request.Request(
            url, headers={"User-Agent": "hotarubukuro-reproducibility/2"}
        )
        with urllib.request.urlopen(request, timeout=1800) as response, temp_path.open("wb") as output:
            shutil.copyfileobj(response, output, length=4 * 1024 * 1024)
        observed = checksum(temp_path, "md5")
        if observed != expected_md5:
            raise BootstrapError(f"Zenodo MD5 mismatch: {observed} != {expected_md5}")
        temp_path.replace(destination)
    finally:
        temp_path.unlink(missing_ok=True)


def run_command(command: list[str], dry_run: bool = False) -> None:
    print("$ " + " ".join(command))
    if not dry_run:
        subprocess.run(command, cwd=ROOT, check=True)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workbook", type=Path, help="Use a local workbook instead of downloading Zenodo")
    parser.add_argument("--cache", type=Path, default=DEFAULT_CACHE, help="Zenodo workbook cache path")
    parser.add_argument("--extraction", type=Path, default=DEFAULT_EXTRACTION, help="Intermediate colour extraction CSV")
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT, help="Generated canonical public table")
    parser.add_argument("--qc-dir", type=Path, default=DEFAULT_QC_DIR, help="Colour-extraction QC directory")
    parser.add_argument("--report", type=Path, default=DEFAULT_REPORT, help="Source-contract audit JSON")
    parser.add_argument("--sheet", help="Workbook sheet name; default is the first sheet")
    parser.add_argument("--header-row", type=int, default=1)
    parser.add_argument("--image-column", default="petal")
    parser.add_argument("--id-column", default="observation_id")
    parser.add_argument("--overwrite-download", action="store_true")
    parser.add_argument("--overwrite-output", action="store_true")
    parser.add_argument("--run-analysis", action="store_true", help="Run the retained analysis after exact contract validation")
    parser.add_argument("--no-resume-analysis", action="store_true", help="Pass --no-resume to the downstream pipeline")
    parser.add_argument("--skip-analysis-setup", action="store_true", help="Pass --skip-setup downstream")
    parser.add_argument("--dry-run", action="store_true", help="Print the complete command graph without network or execution")
    return parser


def downstream_command(args: argparse.Namespace) -> list[str]:
    command = [sys.executable, str(PIPELINE), "reproduce"]
    if args.no_resume_analysis:
        command.append("--no-resume")
    if args.skip_analysis_setup:
        command.append("--skip-setup")
    return command


def _display(path: Path) -> str:
    try:
        return str(path.relative_to(ROOT))
    except ValueError:
        return str(path)


def main() -> int:
    args = build_parser().parse_args()
    workbook = args.workbook.expanduser().resolve() if args.workbook else args.cache.expanduser().resolve()
    extraction = args.extraction.expanduser().resolve()
    output = args.output.expanduser().resolve()
    qc_dir = args.qc_dir.expanduser().resolve()
    report_path = args.report.expanduser().resolve()

    if len({workbook, extraction, output, report_path}) != 4:
        raise SystemExit("workbook, extraction, output, and report paths must be distinct")

    if args.dry_run:
        print(f"Canonical source: Zenodo record {ZENODO_RECORD} ({ZENODO_DOI})")
        print(f"Workbook: {ZENODO_FILENAME}")
        print(f"Expected MD5: {ZENODO_MD5}")
        print(f"Expected generated-table Git blob: {CONTRACT['expected_public_table_git_blob']}")
    elif args.workbook:
        if not workbook.is_file():
            raise SystemExit(f"workbook not found: {workbook}")
        observed = checksum(workbook, "md5")
        if observed != ZENODO_MD5:
            raise SystemExit(f"local workbook MD5 mismatch: {observed} != {ZENODO_MD5}")
    else:
        try:
            download(ZENODO_URL, workbook, ZENODO_MD5, overwrite=args.overwrite_download)
        except Exception as error:
            raise SystemExit(str(error)) from error

    extract_command = [
        sys.executable,
        str(EXTRACTOR),
        "--input-workbook", str(workbook),
        "--output", str(extraction),
        "--qc-dir", str(qc_dir),
        "--header-row", str(args.header_row),
        "--image-column", args.image_column,
        "--id-column", args.id_column,
    ]
    if args.sheet:
        extract_command.extend(["--sheet", args.sheet])
    if args.overwrite_output:
        extract_command.append("--overwrite")
    run_command(extract_command, dry_run=args.dry_run)

    build_command = [
        sys.executable,
        str(BUILDER),
        "--extraction", str(extraction),
        "--output", str(output),
    ]
    if args.overwrite_output:
        build_command.append("--overwrite")
    run_command(build_command, dry_run=args.dry_run)

    if args.dry_run:
        print(f"Contract validation: {_display(output)} -> reproducibility/source_contract.json")
        if args.run_analysis:
            run_command(downstream_command(args), dry_run=True)
        return 0

    if not extraction.is_file():
        raise SystemExit(f"colour extractor did not create {extraction}")
    if not output.is_file():
        raise SystemExit(f"public-table builder did not create {output}")

    try:
        validation = validate_public_table(output, CONTRACT, require_exact_blob=True)
    except SourceContractError as error:
        raise SystemExit(str(error)) from error

    report = {
        "canonical_source": "zenodo_image_workbook",
        "zenodo_record": ZENODO_RECORD,
        "zenodo_doi": ZENODO_DOI,
        "zenodo_filename": ZENODO_FILENAME,
        "zenodo_md5_expected": ZENODO_MD5,
        "zenodo_md5_observed": checksum(workbook, "md5"),
        "extraction_path": _display(extraction),
        "extraction_sha256": checksum(extraction, "sha256"),
        "generated_table_path": _display(output),
        "source_contract_path": "reproducibility/source_contract.json",
        "validation": validation,
    }
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(json.dumps(report, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    print(json.dumps({
        "exact_public_contract": validation["exact_public_contract"],
        "rows": validation["rows"],
        "git_blob": validation["git_blob"],
        "report": _display(report_path),
    }, ensure_ascii=False))

    if args.run_analysis:
        run_command(downstream_command(args))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
