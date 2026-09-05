#!/usr/bin/env python3
"""Rebuild the publication colour table from the raw Zenodo workbook.

This is the zero-to-analysis bootstrap for the public repository. It downloads
(or accepts a local copy of) the image-bearing Supplementary Table S1 workbook,
verifies the frozen Zenodo checksum, runs the deterministic colour extractor,
and compares the rebuilt table with the committed ``Data_S1.csv`` by immutable
``observation_id`` before optionally entering ``run_pipeline.py reproduce``.

The downstream publication pipeline deliberately keeps its frozen Data_S1.csv
contract. Therefore full analysis is launched only after the raw rebuild has
passed the equivalence audit; a mismatch stops the chain rather than silently
changing the paper input.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
import shutil
import subprocess
import sys
import tempfile
import urllib.request
from pathlib import Path
from typing import Iterable

ROOT = Path(__file__).resolve().parents[1]
ZENODO_RECORD = "22334596"
ZENODO_DOI = "10.5281/zenodo.22334596"
ZENODO_FILENAME = "Supplementary_Table_S1.xlsx"
ZENODO_URL = f"https://zenodo.org/records/{ZENODO_RECORD}/files/{ZENODO_FILENAME}?download=1"
ZENODO_MD5 = "a923616e45f10f24a5463eefd09b06dd"
EXPECTED_ROWS = 1965
DEFAULT_CACHE = ROOT / ".repro_cache" / "zenodo" / ZENODO_FILENAME
DEFAULT_OUTPUT = ROOT / "results" / "source_reconstruction" / "Data_S1_from_zenodo.csv"
DEFAULT_QC_DIR = ROOT / "results" / "source_reconstruction" / "qc"
DEFAULT_REPORT = ROOT / "results" / "source_reconstruction" / "zenodo_rebuild_audit.json"
REFERENCE = ROOT / "Data_S1.csv"
EXTRACTOR = ROOT / "source_build" / "extract_color.py"
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
        request = urllib.request.Request(url, headers={"User-Agent": "hotarubukuro-reproducibility/1"})
        with urllib.request.urlopen(request, timeout=1800) as response, temp_path.open("wb") as output:
            shutil.copyfileobj(response, output, length=4 * 1024 * 1024)
        observed = checksum(temp_path, "md5")
        if observed != expected_md5:
            raise BootstrapError(f"Zenodo MD5 mismatch: {observed} != {expected_md5}")
        temp_path.replace(destination)
    finally:
        temp_path.unlink(missing_ok=True)


def read_csv(path: Path) -> tuple[list[str], list[dict[str, str]]]:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        if reader.fieldnames is None:
            raise BootstrapError(f"CSV has no header: {path}")
        rows = list(reader)
        return list(reader.fieldnames), rows


def _as_float(value: str | None) -> float | None:
    if value is None or value.strip() == "":
        return None
    try:
        return float(value)
    except ValueError:
        return None


def audit_rebuild(rebuilt: Path, reference: Path, tolerance: float) -> dict[str, object]:
    rebuilt_header, rebuilt_rows = read_csv(rebuilt)
    reference_header, reference_rows = read_csv(reference)
    required = {"observation_id", "R", "G", "B", "latitude", "longitude"}
    missing_rebuilt = sorted(required.difference(rebuilt_header))
    missing_reference = sorted(required.difference(reference_header))
    if missing_rebuilt or missing_reference:
        raise BootstrapError(
            "required columns missing; rebuilt=%s reference=%s"
            % (missing_rebuilt, missing_reference)
        )

    if len(rebuilt_rows) != EXPECTED_ROWS:
        raise BootstrapError(f"rebuilt row count {len(rebuilt_rows)} != expected {EXPECTED_ROWS}")
    if len(reference_rows) != EXPECTED_ROWS:
        raise BootstrapError(f"reference row count {len(reference_rows)} != expected {EXPECTED_ROWS}")

    def by_id(rows: Iterable[dict[str, str]], label: str) -> dict[str, dict[str, str]]:
        out: dict[str, dict[str, str]] = {}
        for row in rows:
            key = row["observation_id"].strip()
            if not key:
                raise BootstrapError(f"empty observation_id in {label}")
            if key in out:
                raise BootstrapError(f"duplicate observation_id {key!r} in {label}")
            out[key] = row
        return out

    rebuilt_by_id = by_id(rebuilt_rows, "rebuilt table")
    reference_by_id = by_id(reference_rows, "reference table")
    missing_ids = sorted(set(reference_by_id).difference(rebuilt_by_id))
    extra_ids = sorted(set(rebuilt_by_id).difference(reference_by_id))

    numeric_columns = ["R", "G", "B", "latitude", "longitude"]
    mismatches: list[dict[str, object]] = []
    maxima = {column: 0.0 for column in numeric_columns}
    for observation_id in sorted(set(rebuilt_by_id).intersection(reference_by_id)):
        left = rebuilt_by_id[observation_id]
        right = reference_by_id[observation_id]
        for column in numeric_columns:
            a = _as_float(left.get(column))
            b = _as_float(right.get(column))
            if a is None and b is None:
                continue
            if a is None or b is None or not (math.isfinite(a) and math.isfinite(b)):
                mismatches.append({"observation_id": observation_id, "column": column, "rebuilt": a, "reference": b})
                continue
            delta = abs(a - b)
            maxima[column] = max(maxima[column], delta)
            if delta > tolerance:
                mismatches.append(
                    {"observation_id": observation_id, "column": column, "rebuilt": a, "reference": b, "abs_delta": delta}
                )

    qc_counts: dict[str, int] = {}
    if "qc_status" in rebuilt_header:
        for row in rebuilt_rows:
            status = row.get("qc_status", "") or "(blank)"
            qc_counts[status] = qc_counts.get(status, 0) + 1

    return {
        "zenodo_record": ZENODO_RECORD,
        "zenodo_doi": ZENODO_DOI,
        "zenodo_filename": ZENODO_FILENAME,
        "zenodo_md5_expected": ZENODO_MD5,
        "rebuilt_path": str(rebuilt.relative_to(ROOT)),
        "reference_path": str(reference.relative_to(ROOT)),
        "expected_rows": EXPECTED_ROWS,
        "rebuilt_rows": len(rebuilt_rows),
        "reference_rows": len(reference_rows),
        "missing_observation_ids": missing_ids[:50],
        "extra_observation_ids": extra_ids[:50],
        "missing_observation_id_count": len(missing_ids),
        "extra_observation_id_count": len(extra_ids),
        "numeric_tolerance": tolerance,
        "max_abs_delta": maxima,
        "numeric_mismatch_count": len(mismatches),
        "numeric_mismatch_examples": mismatches[:50],
        "rebuilt_qc_status_counts": qc_counts,
        "equivalent_core_input": not missing_ids and not extra_ids and not mismatches,
    }


def run_command(command: list[str], dry_run: bool = False) -> None:
    print("$ " + " ".join(command))
    if not dry_run:
        subprocess.run(command, cwd=ROOT, check=True)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workbook", type=Path, help="Use a local workbook instead of downloading Zenodo")
    parser.add_argument("--cache", type=Path, default=DEFAULT_CACHE, help="Zenodo workbook cache path")
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT, help="Rebuilt colour CSV")
    parser.add_argument("--qc-dir", type=Path, default=DEFAULT_QC_DIR, help="Colour-extraction QC directory")
    parser.add_argument("--report", type=Path, default=DEFAULT_REPORT, help="Equivalence audit JSON")
    parser.add_argument("--sheet", help="Workbook sheet name; default is the first sheet")
    parser.add_argument("--header-row", type=int, default=1)
    parser.add_argument("--image-column", default="petal")
    parser.add_argument("--id-column", default="observation_id")
    parser.add_argument("--tolerance", type=float, default=1e-9, help="Absolute numeric audit tolerance")
    parser.add_argument("--overwrite-download", action="store_true")
    parser.add_argument("--overwrite-output", action="store_true")
    parser.add_argument("--allow-mismatch", action="store_true", help="Write report but do not fail on core mismatch")
    parser.add_argument("--run-analysis", action="store_true", help="After equivalence audit, run run_pipeline.py reproduce")
    parser.add_argument("--resume-analysis", action="store_true", help="Pass --resume to downstream pipeline")
    parser.add_argument("--skip-analysis-setup", action="store_true", help="Pass --skip-setup downstream")
    parser.add_argument("--dry-run", action="store_true", help="Print extraction/downstream commands without network or execution")
    return parser


def main() -> int:
    args = build_parser().parse_args()
    workbook = args.workbook.expanduser().resolve() if args.workbook else args.cache.expanduser().resolve()
    output = args.output.expanduser().resolve()
    qc_dir = args.qc_dir.expanduser().resolve()
    report_path = args.report.expanduser().resolve()

    if args.dry_run:
        print(f"Zenodo source: {ZENODO_URL}")
        print(f"Expected MD5: {ZENODO_MD5}")
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

    command = [
        sys.executable,
        str(EXTRACTOR),
        "--input-workbook",
        str(workbook),
        "--output",
        str(output),
        "--qc-dir",
        str(qc_dir),
        "--header-row",
        str(args.header_row),
        "--image-column",
        args.image_column,
        "--id-column",
        args.id_column,
    ]
    if args.sheet:
        command.extend(["--sheet", args.sheet])
    if args.overwrite_output:
        command.append("--overwrite")
    run_command(command, dry_run=args.dry_run)

    if args.dry_run:
        if args.run_analysis:
            downstream = [sys.executable, str(PIPELINE), "reproduce"]
            if args.resume_analysis:
                downstream.append("--resume")
            if args.skip_analysis_setup:
                downstream.append("--skip-setup")
            run_command(downstream, dry_run=True)
        return 0

    if not output.is_file():
        raise SystemExit(f"colour extractor did not create {output}")

    try:
        report = audit_rebuild(output, REFERENCE, args.tolerance)
    except BootstrapError as error:
        raise SystemExit(str(error)) from error
    report["zenodo_md5_observed"] = checksum(workbook, "md5")
    report["rebuilt_sha256"] = checksum(output, "sha256")
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(json.dumps(report, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    print(json.dumps({
        "equivalent_core_input": report["equivalent_core_input"],
        "rows": report["rebuilt_rows"],
        "numeric_mismatch_count": report["numeric_mismatch_count"],
        "report": str(report_path),
    }, ensure_ascii=False))

    if not report["equivalent_core_input"] and not args.allow_mismatch:
        raise SystemExit(
            "raw Zenodo rebuild does not match the frozen Data_S1 core input; "
            f"see {report_path} (use --allow-mismatch only for diagnosis)"
        )

    if args.run_analysis:
        if not report["equivalent_core_input"]:
            raise SystemExit("refusing full analysis after a mismatched raw rebuild")
        downstream = [sys.executable, str(PIPELINE), "reproduce"]
        if args.resume_analysis:
            downstream.append("--resume")
        if args.skip_analysis_setup:
            downstream.append("--skip-setup")
        run_command(downstream)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
