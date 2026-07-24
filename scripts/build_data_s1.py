#!/usr/bin/env python3
"""Build the public Data_S1 table from a verified colour-extraction result.

The export retains observation, image-hash, colour-method, and QC provenance,
but removes local filesystem and QC-image paths.  Coordinates are carried from
the source workbook; they are never presented as newly georeferenced unless a
separate GPX-derived result is supplied in a future workflow.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
import os
import platform
import tempfile
from collections import Counter
from datetime import date, datetime
from pathlib import Path
from typing import Any, Dict, Iterable, List, Mapping, Optional, Sequence, Tuple

import numpy as np

from scripts.extract_color import srgb_to_cielab


REQUIRED_COLUMNS = {
    "observation_id",
    "source_row",
    "photo_id",
    "image_sha256",
    "date",
    "latitude",
    "longitude",
    "median_R",
    "median_G",
    "median_B",
    "R",
    "G",
    "B",
    "primary_colour_method",
    "extraction_version",
    "qc_status",
}

EXCLUDED_PUBLIC_COLUMNS = {
    "source_path",
    "mask_path",
    "overlay_path",
    "qc_panel_path",
    # Raw activity links are retained in the ignored local extraction workbook
    # for provenance checks, but publishing them together with exact dates and
    # coordinates would create an unnecessary linkage risk.
    "url",
}

DERIVED_COLUMNS = [
    "source_reference_type",
    "coordinate_source",
    "coordinate_crs_assumed",
    "coordinate_recomputed",
    "coordinate_qc_status",
    "photo_coordinate_qc_status",
    "exact_site_id",
    "grid_30s_id",
    "colour_measurement_scope",
    "manual_review_status",
]


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(4 * 1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _normalise_value(value: Any) -> Any:
    if value is None:
        return None
    if isinstance(value, datetime):
        return value.date().isoformat() if value.time().isoformat() == "00:00:00" else value.isoformat()
    if isinstance(value, date):
        return value.isoformat()
    if isinstance(value, str):
        stripped = value.strip()
        return stripped if stripped else None
    return value


def _normalise_date(value: Any) -> str:
    value = _normalise_value(value)
    if value is None:
        raise ValueError("date must be non-missing")
    if isinstance(value, str):
        for format_string in (
            "%Y-%m-%d",
            "%Y-%m-%d %H:%M:%S",
            "%Y/%m/%d",
            "%Y/%m/%d %H:%M:%S",
        ):
            try:
                return datetime.strptime(value, format_string).date().isoformat()
            except ValueError:
                continue
    raise ValueError("date is not a supported calendar date: %r" % value)


def read_table(path: Path) -> Tuple[List[str], List[Dict[str, Any]]]:
    suffix = path.suffix.casefold()
    if suffix == ".csv":
        with path.open(newline="", encoding="utf-8-sig") as handle:
            reader = csv.DictReader(handle)
            if reader.fieldnames is None:
                raise ValueError("Input CSV has no header")
            headers = [str(value).strip() for value in reader.fieldnames]
            rows = [
                {key: _normalise_value(value) for key, value in row.items()}
                for row in reader
            ]
        return headers, rows
    if suffix != ".xlsx":
        raise ValueError("Extraction input must be .csv or .xlsx")

    try:
        import openpyxl
    except ImportError as exc:
        raise RuntimeError("XLSX input requires openpyxl; install .[excel]") from exc

    workbook = openpyxl.load_workbook(path, read_only=True, data_only=True)
    try:
        worksheet = workbook.active
        iterator = worksheet.iter_rows(values_only=True)
        try:
            raw_headers = next(iterator)
        except StopIteration as exc:
            raise ValueError("Input workbook is empty") from exc
        headers = [str(value).strip() if value is not None else "" for value in raw_headers]
        if any(not value for value in headers):
            raise ValueError("Input workbook contains a blank header")
        rows = [
            {header: _normalise_value(value) for header, value in zip(headers, values)}
            for values in iterator
        ]
    finally:
        workbook.close()
    return headers, rows


def _finite_float(value: Any, column: str) -> float:
    try:
        number = float(value)
    except (TypeError, ValueError) as exc:
        raise ValueError("Column %s contains a non-numeric value: %r" % (column, value)) from exc
    if not math.isfinite(number):
        raise ValueError("Column %s must be finite" % column)
    return number


def _truthy(value: Any) -> bool:
    if isinstance(value, bool):
        return value
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        return bool(value)
    return str(value or "").strip().casefold() in {"1", "true", "yes", "y"}


def _site_identifiers(latitude: float, longitude: float) -> Tuple[str, str]:
    coordinate_key = "%.10f,%.10f" % (latitude, longitude)
    exact_site_id = "site-" + hashlib.sha256(coordinate_key.encode("ascii")).hexdigest()[:16]
    # Global EPSG:4326 grid anchored at (-180, -90), with 120 cells per degree.
    column = int(math.floor((longitude + 180.0) * 120.0 + 1e-10))
    row = int(math.floor((latitude + 90.0) * 120.0 + 1e-10))
    return exact_site_id, "grid30s-r%05d-c%05d" % (row, column)


def validate_rows(headers: Sequence[str], rows: Sequence[Mapping[str, Any]]) -> None:
    if len(headers) != len(set(headers)):
        raise ValueError("Extraction columns must be unique")
    missing = sorted(REQUIRED_COLUMNS.difference(headers))
    if missing:
        raise ValueError("Extraction is missing columns: %s" % ", ".join(missing))
    if not rows:
        raise ValueError("Extraction contains no observations")

    observation_ids: List[str] = []
    source_rows: List[int] = []
    for row in rows:
        observation_id = str(row.get("observation_id") or "").strip()
        if not observation_id:
            raise ValueError("observation_id must be non-empty")
        observation_ids.append(observation_id)
        try:
            source_rows.append(int(row.get("source_row")))
        except (TypeError, ValueError) as exc:
            raise ValueError("source_row must contain integers") from exc

        latitude = _finite_float(row.get("latitude"), "latitude")
        longitude = _finite_float(row.get("longitude"), "longitude")
        if not (-90 <= latitude <= 90 and -180 <= longitude <= 180):
            raise ValueError("Coordinates are outside geographic bounds")
        for column in ("R", "G", "B", "median_R", "median_G", "median_B"):
            value = _finite_float(row.get(column), column)
            if not 0 <= value <= 255:
                raise ValueError("Column %s is outside 0--255" % column)
        digest = str(row.get("image_sha256") or "")
        if len(digest) != 64 or any(character not in "0123456789abcdef" for character in digest):
            raise ValueError("image_sha256 must be a lowercase SHA-256 digest")

    if len(observation_ids) != len(set(observation_ids)):
        raise ValueError("observation_id must be unique")
    if len(source_rows) != len(set(source_rows)):
        raise ValueError("source_row must be unique")


def build_public_rows(
    headers: Sequence[str], rows: Sequence[Mapping[str, Any]]
) -> Tuple[List[str], List[Dict[str, Any]]]:
    validate_rows(headers, rows)
    output_headers = [
        "observation_id",
        "source_row",
        "photo_id",
        "image_sha256",
        "duplicate_image_sha256",
        "date",
        "latitude",
        "longitude",
        *DERIVED_COLUMNS,
    ]
    output_headers.extend(
        column
        for column in headers
        if column not in EXCLUDED_PUBLIC_COLUMNS and column not in output_headers
    )

    output_rows: List[Dict[str, Any]] = []
    for source in rows:
        row = {
            column: source.get(column)
            for column in headers
            if column not in EXCLUDED_PUBLIC_COLUMNS
        }
        latitude = _finite_float(source.get("latitude"), "latitude")
        longitude = _finite_float(source.get("longitude"), "longitude")
        row["date"] = _normalise_date(source.get("date"))
        exact_site_id, grid_id = _site_identifiers(latitude, longitude)
        source_reference = str(source.get("url") or "")
        is_yamap = source_reference.startswith("https://yamap.com/activities/")
        duplicate = _truthy(source.get("duplicate_image_sha256"))
        row.update(
            {
                "source_reference_type": "yamap_activity" if is_yamap else "field_survey_or_other",
                "coordinate_source": "source_workbook",
                "coordinate_crs_assumed": "EPSG:4326",
                "coordinate_recomputed": False,
                "coordinate_qc_status": "source_value_not_independently_recomputed",
                "photo_coordinate_qc_status": (
                    "manual_review_required_duplicate_photo_at_multiple_coordinates"
                    if duplicate
                    else "mapped_by_workbook_cell_and_image_hash"
                ),
                "exact_site_id": exact_site_id,
                "grid_30s_id": grid_id,
                "colour_measurement_scope": "uncalibrated_display_referred_sRGB",
                "manual_review_status": (
                    "pending" if str(source.get("qc_status")) != "ok" else "not_required_by_automated_qc"
                ),
            }
        )
        output_rows.append({column: row.get(column) for column in output_headers})
    return output_headers, output_rows


def _blankable(value: Any) -> Any:
    if value is None:
        return ""
    if isinstance(value, (float, np.floating)) and not math.isfinite(float(value)):
        return ""
    return value


def atomic_write_csv(
    headers: Sequence[str], rows: Iterable[Mapping[str, Any]], path: Path, *, overwrite: bool
) -> None:
    if path.exists() and not overwrite:
        raise FileExistsError("Refusing to overwrite existing output: %s" % path)
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=".%s." % path.name, suffix=".part", dir=path.parent
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(
                handle,
                fieldnames=list(headers),
                extrasaction="raise",
                lineterminator="\n",
            )
            writer.writeheader()
            for row in rows:
                writer.writerow({column: _blankable(row.get(column)) for column in headers})
            handle.flush()
            os.fsync(handle.fileno())
        if path.exists() and not overwrite:
            raise FileExistsError("Refusing to overwrite existing output: %s" % path)
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def _rgb_matrix(rows: Sequence[Mapping[str, Any]], columns: Sequence[str]) -> np.ndarray:
    values = []
    for row in rows:
        try:
            triplet = [_finite_float(row.get(column), column) for column in columns]
        except ValueError:
            triplet = [math.nan, math.nan, math.nan]
        values.append(triplet)
    return np.asarray(values, dtype=np.float64)


def comparison_rows(rows: Sequence[Mapping[str, Any]]) -> List[Dict[str, Any]]:
    comparisons = [
        ("legacy_vs_primary", ("legacy_R", "legacy_G", "legacy_B"), ("R", "G", "B")),
        ("mean_vs_median", ("mean_R", "mean_G", "mean_B"), ("median_R", "median_G", "median_B")),
        ("median_vs_hsv_joint_peak", ("median_R", "median_G", "median_B"), ("hsv_peak_R", "hsv_peak_G", "hsv_peak_B")),
        (
            "median_vs_exposure_filtered_peak",
            ("median_R", "median_G", "median_B"),
            (
                "hsv_exposure_filtered_peak_R",
                "hsv_exposure_filtered_peak_G",
                "hsv_exposure_filtered_peak_B",
            ),
        ),
        ("hsv_vs_alpha_joint_peak", ("hsv_peak_R", "hsv_peak_G", "hsv_peak_B"), ("alpha_peak_R", "alpha_peak_G", "alpha_peak_B")),
    ]
    output: List[Dict[str, Any]] = []
    for name, left_columns, right_columns in comparisons:
        left = _rgb_matrix(rows, left_columns)
        right = _rgb_matrix(rows, right_columns)
        complete = np.all(np.isfinite(left), axis=1) & np.all(np.isfinite(right), axis=1)
        left_lab = srgb_to_cielab(left[complete])
        right_lab = srgb_to_cielab(right[complete])
        signed_delta = right_lab - left_lab
        distance = np.linalg.norm(signed_delta, axis=1)
        quantiles = np.quantile(distance, [0, 0.5, 0.95, 0.99, 1]) if len(distance) else [math.nan] * 5
        output.append(
            {
                "comparison": name,
                "metric": "DeltaE76",
                "n": int(len(distance)),
                "delta_L_mean": float(np.mean(signed_delta[:, 0])) if len(distance) else math.nan,
                "delta_a_mean": float(np.mean(signed_delta[:, 1])) if len(distance) else math.nan,
                "delta_b_mean": float(np.mean(signed_delta[:, 2])) if len(distance) else math.nan,
                "mean": float(np.mean(distance)) if len(distance) else math.nan,
                "min": float(quantiles[0]),
                "median": float(quantiles[1]),
                "p95": float(quantiles[2]),
                "p99": float(quantiles[3]),
                "max": float(quantiles[4]),
            }
        )
    return output


def build_manifest(
    *,
    extraction: Path,
    output: Path,
    headers: Sequence[str],
    rows: Sequence[Mapping[str, Any]],
    source_rows: Sequence[Mapping[str, Any]],
    source_workbook: Optional[Path],
    legacy_input: Optional[Path],
    method_config: Optional[Path],
) -> Dict[str, Any]:
    coordinate_pairs = {(float(row["latitude"]), float(row["longitude"])) for row in rows}
    image_hashes = [str(row["image_sha256"]) for row in rows]
    references = [str(row.get("url") or "") for row in source_rows]
    reference_types = Counter(str(row.get("source_reference_type") or "") for row in rows)
    try:
        import PIL

        pillow_version = PIL.__version__
    except ImportError:
        pillow_version = None
    try:
        import openpyxl

        openpyxl_version = openpyxl.__version__
    except ImportError:
        openpyxl_version = None

    manifest: Dict[str, Any] = {
        "schema_version": 1,
        "output_file": output.name,
        "output_sha256": sha256_file(output),
        "extraction_file": extraction.name,
        "extraction_sha256": sha256_file(extraction),
        "extraction_versions": dict(Counter(str(row["extraction_version"]) for row in rows)),
        "primary_colour_methods": dict(Counter(str(row["primary_colour_method"]) for row in rows)),
        "records": len(rows),
        "columns": list(headers),
        "unique_observation_ids": len({str(row["observation_id"]) for row in rows}),
        "unique_image_sha256": len(set(image_hashes)),
        "duplicate_image_records": sum(_truthy(row.get("duplicate_image_sha256")) for row in rows),
        "unique_exact_coordinate_pairs": len(coordinate_pairs),
        "qc_status_counts": dict(Counter(str(row["qc_status"]) for row in rows)),
        "source_reference_counts": {
            "yamap_activity": reference_types["yamap_activity"],
            "field_survey_or_other": reference_types["field_survey_or_other"],
            "unique": len(set(references)),
        },
        "coordinate_provenance": {
            "source": "source_workbook",
            "recomputed": False,
            "crs_assumed": "EPSG:4326",
            "note": "No GPX/photo timestamp manifest was available; values were mapped to workbook images by cell and SHA-256, not independently re-georeferenced.",
        },
        "colour_scope": "uncalibrated_display_referred_sRGB",
        "white_balance": "not_applied_no_camera_metadata_or_neutral_reference",
        "excluded_public_columns": sorted(EXCLUDED_PUBLIC_COLUMNS),
        "build_environment": {
            "python": platform.python_version(),
            "numpy": np.__version__,
            "Pillow": pillow_version,
            "openpyxl": openpyxl_version,
        },
    }
    if source_workbook is not None:
        manifest["source_workbook"] = {
            "file": source_workbook.name,
            "sha256": sha256_file(source_workbook),
            "size_bytes": source_workbook.stat().st_size,
        }
    if legacy_input is not None:
        manifest["legacy_input"] = {
            "file": legacy_input.name,
            "sha256": sha256_file(legacy_input),
            "size_bytes": legacy_input.stat().st_size,
        }
    if method_config is not None:
        manifest["colour_method_config"] = {
            "file": method_config.name,
            "sha256": sha256_file(method_config),
            "configuration": json.loads(method_config.read_text(encoding="utf-8")),
        }
    return manifest


def atomic_write_json(value: Mapping[str, Any], path: Path, *, overwrite: bool) -> None:
    if path.exists() and not overwrite:
        raise FileExistsError("Refusing to overwrite existing output: %s" % path)
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=".%s." % path.name, suffix=".part", dir=path.parent
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
            json.dump(value, handle, ensure_ascii=False, indent=2, sort_keys=True)
            handle.write("\n")
            handle.flush()
            os.fsync(handle.fileno())
        if path.exists() and not overwrite:
            raise FileExistsError("Refusing to overwrite existing output: %s" % path)
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--extraction", type=Path, required=True, help="Verified v2 colour CSV/XLSX")
    parser.add_argument("--output", type=Path, required=True, help="Public Data_S1-compatible CSV")
    parser.add_argument("--manifest", type=Path, required=True, help="Provenance JSON output")
    parser.add_argument("--comparison", type=Path, required=True, help="Colour-method comparison CSV")
    parser.add_argument("--source-workbook", type=Path, help="Optional read-only source workbook for hashing")
    parser.add_argument("--legacy-input", type=Path, help="Optional preserved legacy CSV for hashing")
    parser.add_argument("--method-config", type=Path, help="Optional colour-method JSON for hashing")
    parser.add_argument("--overwrite", action="store_true", help="Explicitly replace derived outputs")
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = build_parser().parse_args(argv)
    extraction = args.extraction.expanduser().resolve()
    output = args.output.expanduser().resolve()
    manifest_path = args.manifest.expanduser().resolve()
    comparison_path = args.comparison.expanduser().resolve()
    for path in (extraction, args.source_workbook, args.legacy_input, args.method_config):
        if path is not None and not Path(path).expanduser().exists():
            raise SystemExit("Input does not exist: %s" % path)
    outputs = [output, manifest_path, comparison_path]
    if len(set(outputs)) != len(outputs):
        raise SystemExit("Output, manifest, and comparison paths must be distinct")
    if extraction in outputs:
        raise SystemExit("Derived output must differ from extraction input")
    if not args.overwrite:
        existing = [path for path in outputs if path.exists()]
        if existing:
            raise FileExistsError(
                "Refusing to overwrite existing output: %s"
                % ", ".join(str(path) for path in existing)
            )

    headers, rows = read_table(extraction)
    public_headers, public_rows = build_public_rows(headers, rows)
    atomic_write_csv(public_headers, public_rows, output, overwrite=args.overwrite)
    comparisons = comparison_rows(rows)
    atomic_write_csv(
        list(comparisons[0]), comparisons, comparison_path, overwrite=args.overwrite
    )
    source_workbook = args.source_workbook.expanduser().resolve() if args.source_workbook else None
    legacy_input = args.legacy_input.expanduser().resolve() if args.legacy_input else None
    method_config = args.method_config.expanduser().resolve() if args.method_config else None
    manifest = build_manifest(
        extraction=extraction,
        output=output,
        headers=public_headers,
        rows=public_rows,
        source_rows=rows,
        source_workbook=source_workbook,
        legacy_input=legacy_input,
        method_config=method_config,
    )
    atomic_write_json(manifest, manifest_path, overwrite=args.overwrite)
    print(
        json.dumps(
            {
                "records": len(public_rows),
                "output": str(output),
                "manifest": str(manifest_path),
                "comparison": str(comparison_path),
            },
            ensure_ascii=False,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
