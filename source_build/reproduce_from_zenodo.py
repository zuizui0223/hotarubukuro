#!/usr/bin/env python3
"""Rebuild the publication Data_S1 table from the raw Zenodo workbook.

The public zero-to-analysis chain is deliberately explicit:

Zenodo XLSX -> embedded images -> colour extraction -> Data_S1 materialization
-> equivalence audit -> retained publication pipeline.

The final analysis receives the rebuilt table itself through ``--data-s1``.
The committed ``Data_S1.csv`` remains an immutable reference contract and is
never silently replaced.
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
DEFAULT_EXTRACTION = ROOT / "results" / "source_reconstruction" / "colour_extraction_from_zenodo.csv"
DEFAULT_OUTPUT = ROOT / "results" / "source_reconstruction" / "Data_S1_from_zenodo.csv"
DEFAULT_QC_DIR = ROOT / "results" / "source_reconstruction" / "qc"
DEFAULT_REPORT = ROOT / "results" / "source_reconstruction" / "zenodo_rebuild_audit.json"

REFERENCE = ROOT / "Data_S1.csv"
EXTRACTOR = ROOT / "source_build" / "extract_color.py"
BUILDER = ROOT / "source_build" / "build_data_s1.py"
PIPELINE = ROOT / "run_pipeline.py"

CORE_NUMERIC_COLUMNS = ("R", "G", "B", "latitude", "longitude")
DOWNSTREAM_NUMERIC_COLUMNS = (
    "R", "G", "B", "median_R", "median_G", "median_B",
    "latitude", "longitude", "mask_pixels", "mask_fraction_visible",
)
DOWNSTREAM_EXACT_COLUMNS = (
    "date",
    "qc_status",
    "manual_review_status",
    "duplicate_image_sha256",
    "possible_overexposure",
    "image_sha256",
    "exact_site_id",
    "grid_30s_id",
    "source_reference_type",
    "coordinate_source",
    "coordinate_crs_assumed",
    "coordinate_recomputed",
    "coordinate_qc_status",
    "photo_coordinate_qc_status",
    "colour_measurement_scope",
)


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
            url, headers={"User-Agent": "hotarubukuro-reproducibility/1"}
        )
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
        return list(reader.fieldnames), list(reader)


def _as_float(value: str | None) -> float | None:
    if value is None or value.strip() == "":
        return None
    try:
        return float(value)
    except ValueError:
        return None


def _normalise_exact(value: str | None, column: str) -> str:
    text = "" if value is None else str(value).strip()
    if column in {
        "qc_status", "manual_review_status", "possible_overexposure",
        "coordinate_recomputed",
    }:
        return text.casefold()
    return text


def _indexed_rows(rows: Iterable[dict[str, str]], label: str) -> dict[str, dict[str, str]]:
    indexed: dict[str, dict[str, str]] = {}
    for row in rows:
        key = (row.get("observation_id") or "").strip()
        if not key:
            raise BootstrapError(f"empty observation_id in {label}")
        if key in indexed:
            raise BootstrapError(f"duplicate observation_id {key!r} in {label}")
        indexed[key] = row
    return indexed


def audit_rebuild(
    rebuilt: Path,
    reference: Path,
    tolerance: float,
    *,
    expected_rows: int = EXPECTED_ROWS,
) -> dict[str, object]:
    rebuilt_header, rebuilt_rows = read_csv(rebuilt)
    reference_header, reference_rows = read_csv(reference)
    rebuilt_columns = set(rebuilt_header)
    reference_columns = set(reference_header)

    core_required = {"observation_id", *CORE_NUMERIC_COLUMNS}
    missing_rebuilt = sorted(core_required.difference(rebuilt_columns))
    missing_reference = sorted(core_required.difference(reference_columns))
    if missing_rebuilt or missing_reference:
        raise BootstrapError(
            "required columns missing; rebuilt=%s reference=%s"
            % (missing_rebuilt, missing_reference)
        )
    if len(rebuilt_rows) != expected_rows:
        raise BootstrapError(f"rebuilt row count {len(rebuilt_rows)} != expected {expected_rows}")
    if len(reference_rows) != expected_rows:
        raise BootstrapError(f"reference row count {len(reference_rows)} != expected {expected_rows}")

    rebuilt_by_id = _indexed_rows(rebuilt_rows, "rebuilt table")
    reference_by_id = _indexed_rows(reference_rows, "reference table")
    missing_ids = sorted(set(reference_by_id).difference(rebuilt_by_id))
    extra_ids = sorted(set(rebuilt_by_id).difference(reference_by_id))
    shared_ids = sorted(set(rebuilt_by_id).intersection(reference_by_id))

    numeric_contract = [column for column in DOWNSTREAM_NUMERIC_COLUMNS if column in reference_columns]
    exact_contract = [column for column in DOWNSTREAM_EXACT_COLUMNS if column in reference_columns]
    missing_downstream_columns = sorted(
        set(numeric_contract + exact_contract).difference(rebuilt_columns)
    )
    comparable_numeric = [column for column in numeric_contract if column in rebuilt_columns]
    comparable_exact = [column for column in exact_contract if column in rebuilt_columns]

    core_mismatches: list[dict[str, object]] = []
    downstream_numeric_mismatches: list[dict[str, object]] = []
    downstream_exact_mismatches: list[dict[str, object]] = []
    maxima = {column: 0.0 for column in comparable_numeric}

    # Compare every available contract column even when another column is
    # missing. One missing field must never suppress diagnostics for the rest.
    for observation_id in shared_ids:
        left = rebuilt_by_id[observation_id]
        right = reference_by_id[observation_id]
        for column in comparable_numeric:
            a = _as_float(left.get(column))
            b = _as_float(right.get(column))
            if a is None and b is None:
                continue
            if a is None or b is None or not (math.isfinite(a) and math.isfinite(b)):
                mismatch = {
                    "observation_id": observation_id,
                    "column": column,
                    "rebuilt": a,
                    "reference": b,
                }
                downstream_numeric_mismatches.append(mismatch)
                if column in CORE_NUMERIC_COLUMNS:
                    core_mismatches.append(mismatch)
                continue
            delta = abs(a - b)
            maxima[column] = max(maxima[column], delta)
            if delta > tolerance:
                mismatch = {
                    "observation_id": observation_id,
                    "column": column,
                    "rebuilt": a,
                    "reference": b,
                    "abs_delta": delta,
                }
                downstream_numeric_mismatches.append(mismatch)
                if column in CORE_NUMERIC_COLUMNS:
                    core_mismatches.append(mismatch)

        for column in comparable_exact:
            a = _normalise_exact(left.get(column), column)
            b = _normalise_exact(right.get(column), column)
            if a != b:
                downstream_exact_mismatches.append(
                    {
                        "observation_id": observation_id,
                        "column": column,
                        "rebuilt": a,
                        "reference": b,
                    }
                )

    qc_counts: dict[str, int] = {}
    review_counts: dict[str, int] = {}
    for row in rebuilt_rows:
        qc = row.get("qc_status", "") or "(blank)"
        review = row.get("manual_review_status", "") or "(blank)"
        qc_counts[qc] = qc_counts.get(qc, 0) + 1
        review_counts[review] = review_counts.get(review, 0) + 1

    equivalent_core = not missing_ids and not extra_ids and not core_mismatches
    equivalent_downstream = (
        equivalent_core
        and not missing_downstream_columns
        and not downstream_numeric_mismatches
        and not downstream_exact_mismatches
    )

    return {
        "zenodo_record": ZENODO_RECORD,
        "zenodo_doi": ZENODO_DOI,
        "zenodo_filename": ZENODO_FILENAME,
        "zenodo_md5_expected": ZENODO_MD5,
        "rebuilt_path": str(rebuilt.relative_to(ROOT)) if rebuilt.is_relative_to(ROOT) else str(rebuilt),
        "reference_path": str(reference.relative_to(ROOT)) if reference.is_relative_to(ROOT) else str(reference),
        "expected_rows": expected_rows,
        "rebuilt_rows": len(rebuilt_rows),
        "reference_rows": len(reference_rows),
        "missing_observation_ids": missing_ids[:50],
        "extra_observation_ids": extra_ids[:50],
        "missing_observation_id_count": len(missing_ids),
        "extra_observation_id_count": len(extra_ids),
        "numeric_tolerance": tolerance,
        "downstream_numeric_contract": numeric_contract,
        "downstream_exact_contract": exact_contract,
        "downstream_numeric_columns_checked": comparable_numeric,
        "downstream_exact_columns_checked": comparable_exact,
        "missing_downstream_columns_in_rebuild": missing_downstream_columns,
        "max_abs_delta": maxima,
        "core_numeric_mismatch_count": len(core_mismatches),
        "core_numeric_mismatch_examples": core_mismatches[:50],
        "downstream_numeric_mismatch_count": len(downstream_numeric_mismatches),
        "downstream_numeric_mismatch_examples": downstream_numeric_mismatches[:50],
        "downstream_exact_mismatch_count": len(downstream_exact_mismatches),
        "downstream_exact_mismatch_examples": downstream_exact_mismatches[:50],
        "rebuilt_qc_status_counts": qc_counts,
        "rebuilt_manual_review_status_counts": review_counts,
        "equivalent_core_input": equivalent_core,
        "equivalent_downstream_input": equivalent_downstream,
    }


def run_command(command: list[str], dry_run: bool = False) -> None:
    print("$ " + " ".join(command))
    if not dry_run:
        subprocess.run(command, cwd=ROOT, check=True)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workbook", type=Path, help="Use a local workbook instead of downloading Zenodo")
    parser.add_argument("--cache", type=Path, default=DEFAULT_CACHE, help="Zenodo workbook cache path")
    parser.add_argument("--extraction", type=Path, default=DEFAULT_EXTRACTION, help="Intermediate colour extraction CSV")
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT, help="Rebuilt public Data_S1 CSV")
    parser.add_argument("--qc-dir", type=Path, default=DEFAULT_QC_DIR, help="Colour-extraction QC directory")
    parser.add_argument("--report", type=Path, default=DEFAULT_REPORT, help="Equivalence audit JSON")
    parser.add_argument("--sheet", help="Workbook sheet name; default is the first sheet")
    parser.add_argument("--header-row", type=int, default=1)
    parser.add_argument("--image-column", default="petal")
    parser.add_argument("--id-column", default="observation_id")
    parser.add_argument("--tolerance", type=float, default=1e-9, help="Absolute numeric audit tolerance")
    parser.add_argument("--overwrite-download", action="store_true")
    parser.add_argument("--overwrite-output", action="store_true")
    parser.add_argument("--allow-mismatch", action="store_true", help="Write report but do not fail on downstream mismatch")
    parser.add_argument("--run-analysis", action="store_true", help="After audit, run the rebuilt Data_S1 through run_pipeline.py reproduce")
    parser.add_argument("--no-resume-analysis", action="store_true", help="Pass --no-resume to downstream pipeline")
    parser.add_argument("--skip-analysis-setup", action="store_true", help="Pass --skip-setup downstream")
    parser.add_argument("--dry-run", action="store_true", help="Print commands without network or execution")
    return parser


def downstream_command(args: argparse.Namespace, data_s1: Path) -> list[str]:
    try:
        data_arg = str(data_s1.relative_to(ROOT))
    except ValueError:
        data_arg = str(data_s1)
    command = [sys.executable, str(PIPELINE), "reproduce", "--data-s1", data_arg]
    if args.no_resume_analysis:
        command.append("--no-resume")
    if args.skip_analysis_setup:
        command.append("--skip-setup")
    return command


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
        if args.run_analysis:
            run_command(downstream_command(args, output), dry_run=True)
        return 0

    if not extraction.is_file():
        raise SystemExit(f"colour extractor did not create {extraction}")
    if not output.is_file():
        raise SystemExit(f"Data_S1 builder did not create {output}")

    try:
        report = audit_rebuild(output, REFERENCE, args.tolerance)
    except BootstrapError as error:
        raise SystemExit(str(error)) from error
    report["zenodo_md5_observed"] = checksum(workbook, "md5")
    report["extraction_sha256"] = checksum(extraction, "sha256")
    report["rebuilt_sha256"] = checksum(output, "sha256")
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(json.dumps(report, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    print(json.dumps({
        "equivalent_core_input": report["equivalent_core_input"],
        "equivalent_downstream_input": report["equivalent_downstream_input"],
        "rows": report["rebuilt_rows"],
        "downstream_numeric_mismatch_count": report["downstream_numeric_mismatch_count"],
        "downstream_exact_mismatch_count": report["downstream_exact_mismatch_count"],
        "missing_downstream_columns": report["missing_downstream_columns_in_rebuild"],
        "report": str(report_path),
    }, ensure_ascii=False))

    if not report["equivalent_downstream_input"] and not args.allow_mismatch:
        raise SystemExit(
            "raw Zenodo rebuild does not match the frozen Data_S1 downstream input contract; "
            f"see {report_path} (use --allow-mismatch only for diagnosis)"
        )

    if args.run_analysis:
        if not report["equivalent_downstream_input"]:
            raise SystemExit("refusing full analysis after a mismatched raw rebuild")
        run_command(downstream_command(args, output))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
