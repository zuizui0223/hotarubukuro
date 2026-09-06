#!/usr/bin/env python3
"""Materialize the deterministic analysis-input table from verified colour extraction.

The full extractor CSV intentionally remains rich: it contains candidate colour
statistics, sensitivity diagnostics, QC notes and the run-time ``processed_at``
field. The downstream publication analysis does not need that entire technical
surface. This builder therefore emits a small, explicit schema containing only
observation identity/provenance, the primary colour measurement, eligibility/QC
fields and the image-quality covariates actually read downstream.

No row-order join is used. Each record keeps its immutable ``observation_id``.
The resulting table contains no run-time timestamp, sorts by observation ID and
normalises numeric text, so the same raw workbook produces one deterministic
analysis input.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import math
import os
import tempfile
from datetime import datetime
from pathlib import Path
from typing import Any, Mapping, Sequence

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

PUBLIC_ANALYSIS_COLUMNS = [
    "observation_id",
    "source_row",
    "photo_id",
    "image_sha256",
    "duplicate_image_sha256",
    "date",
    "latitude",
    "longitude",
    *DERIVED_COLUMNS,
    "source_sheet",
    "source_image",
    "image_width",
    "image_height",
    "visible_pixels",
    "mask_pixels",
    "mask_fraction_visible",
    "mask_component_count",
    "exposure_filtered_fraction",
    "possible_overexposure",
    "median_R",
    "median_G",
    "median_B",
    "R",
    "G",
    "B",
    "primary_colour_method",
    "extraction_version",
    "qc_status",
    "qc_flags",
]

NUMERIC_COLUMNS = {
    "latitude",
    "longitude",
    "image_width",
    "image_height",
    "visible_pixels",
    "mask_pixels",
    "mask_fraction_visible",
    "mask_component_count",
    "exposure_filtered_fraction",
    "median_R",
    "median_G",
    "median_B",
    "R",
    "G",
    "B",
}

INTEGER_COLUMNS = {
    "source_row",
    "image_width",
    "image_height",
    "visible_pixels",
    "mask_pixels",
    "mask_component_count",
}

REQUIRED_EXTRACTION_COLUMNS = (
    set(PUBLIC_ANALYSIS_COLUMNS)
    .difference(DERIVED_COLUMNS)
    .union({"url"})
)


def _normalise(value: Any) -> str:
    return "" if value is None else str(value).strip()


def _finite_float(value: Any, column: str) -> float:
    try:
        number = float(value)
    except (TypeError, ValueError) as error:
        raise ValueError(f"{column} contains a non-numeric value: {value!r}") from error
    if not math.isfinite(number):
        raise ValueError(f"{column} must be finite")
    return number


def canonical_numeric(value: Any, column: str) -> str:
    """Return stable text for a numeric analysis-table value."""
    text = _normalise(value)
    if not text:
        return ""
    number = _finite_float(text, column)
    if column in INTEGER_COLUMNS:
        rounded = round(number)
        if abs(number - rounded) > 1e-9:
            raise ValueError(f"{column} must be integer-valued: {value!r}")
        return str(int(rounded))
    # Twelve significant digits retain far more precision than any downstream
    # ecological estimand while removing irrelevant binary-float text jitter.
    return format(number, ".12g")


def _truthy(value: Any) -> bool:
    text = _normalise(value).casefold()
    if not text:
        return False
    if text in {"0", "false", "no", "n"}:
        return False
    return True


def _normalise_date(value: Any) -> str:
    text = _normalise(value)
    if not text:
        raise ValueError("date must be non-missing")
    for format_string in (
        "%Y-%m-%d",
        "%Y-%m-%d %H:%M:%S",
        "%Y/%m/%d",
        "%Y/%m/%d %H:%M:%S",
    ):
        try:
            return datetime.strptime(text, format_string).date().isoformat()
        except ValueError:
            continue
    raise ValueError(f"date is not a supported calendar date: {text!r}")


def site_identifiers(latitude: float, longitude: float) -> tuple[str, str]:
    coordinate_key = "%.10f,%.10f" % (latitude, longitude)
    exact_site_id = "site-" + hashlib.sha256(coordinate_key.encode("ascii")).hexdigest()[:16]
    column = int(math.floor((longitude + 180.0) * 120.0 + 1e-10))
    row = int(math.floor((latitude + 90.0) * 120.0 + 1e-10))
    return exact_site_id, "grid30s-r%05d-c%05d" % (row, column)


def read_rows(path: Path) -> tuple[list[str], list[dict[str, str]]]:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        if reader.fieldnames is None:
            raise ValueError("extraction CSV has no header")
        headers = [str(value).strip() for value in reader.fieldnames]
        if len(headers) != len(set(headers)):
            raise ValueError("extraction columns must be unique")
        missing = sorted(REQUIRED_EXTRACTION_COLUMNS.difference(headers))
        if missing:
            raise ValueError("extraction is missing analysis-contract columns: " + ", ".join(missing))
        rows = list(reader)
    if not rows:
        raise ValueError("extraction contains no observations")
    return headers, rows


def materialize_rows(
    headers: Sequence[str], rows: Sequence[Mapping[str, Any]]
) -> tuple[list[str], list[dict[str, str]]]:
    del headers  # the public output schema is explicit, never inherited dynamically
    observation_ids: set[str] = set()
    source_rows: set[int] = set()
    output_rows: list[dict[str, str]] = []

    for source in rows:
        observation_id = _normalise(source.get("observation_id"))
        if not observation_id:
            raise ValueError("observation_id must be non-empty")
        if observation_id in observation_ids:
            raise ValueError(f"duplicate observation_id: {observation_id}")
        observation_ids.add(observation_id)

        source_row_text = canonical_numeric(source.get("source_row"), "source_row")
        source_row = int(source_row_text)
        if source_row in source_rows:
            raise ValueError(f"duplicate source_row: {source_row}")
        source_rows.add(source_row)

        latitude = _finite_float(source.get("latitude"), "latitude")
        longitude = _finite_float(source.get("longitude"), "longitude")
        if not (-90 <= latitude <= 90 and -180 <= longitude <= 180):
            raise ValueError("coordinates are outside geographic bounds")
        for column in ("R", "G", "B", "median_R", "median_G", "median_B"):
            value = _finite_float(source.get(column), column)
            if not 0 <= value <= 255:
                raise ValueError(f"{column} is outside 0--255")

        digest = _normalise(source.get("image_sha256"))
        if len(digest) != 64 or any(character not in "0123456789abcdef" for character in digest):
            raise ValueError("image_sha256 must be a lowercase SHA-256 digest")

        exact_site_id, grid_id = site_identifiers(latitude, longitude)
        source_reference = _normalise(source.get("url"))
        qc_status = _normalise(source.get("qc_status"))
        duplicate = _truthy(source.get("duplicate_image_sha256"))

        row = {column: _normalise(source.get(column)) for column in PUBLIC_ANALYSIS_COLUMNS}
        row.update(
            observation_id=observation_id,
            source_row=source_row_text,
            image_sha256=digest,
            date=_normalise_date(source.get("date")),
            source_reference_type=(
                "yamap_activity"
                if source_reference.startswith("https://yamap.com/activities/")
                else "field_survey_or_other"
            ),
            coordinate_source="source_workbook",
            coordinate_crs_assumed="EPSG:4326",
            coordinate_recomputed="False",
            coordinate_qc_status="source_value_not_independently_recomputed",
            photo_coordinate_qc_status=(
                "manual_review_required_duplicate_photo_at_multiple_coordinates"
                if duplicate
                else "mapped_by_workbook_cell_and_image_hash"
            ),
            exact_site_id=exact_site_id,
            grid_30s_id=grid_id,
            colour_measurement_scope="uncalibrated_display_referred_sRGB",
            manual_review_status=(
                "not_required_by_automated_qc" if qc_status == "ok" else "pending"
            ),
        )
        for column in NUMERIC_COLUMNS:
            row[column] = canonical_numeric(source.get(column), column)
        output_rows.append({column: row.get(column, "") for column in PUBLIC_ANALYSIS_COLUMNS})

    output_rows.sort(key=lambda row: row["observation_id"])
    return list(PUBLIC_ANALYSIS_COLUMNS), output_rows


def write_csv(
    headers: Sequence[str], rows: Sequence[Mapping[str, Any]], path: Path, *, overwrite: bool
) -> None:
    if path.exists() and not overwrite:
        raise FileExistsError(f"refusing to overwrite existing output: {path}")
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{path.name}.", suffix=".part", dir=path.parent
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=list(headers), extrasaction="raise", lineterminator="\n")
            writer.writeheader()
            writer.writerows(rows)
            handle.flush()
            os.fsync(handle.fileno())
        if path.exists() and not overwrite:
            raise FileExistsError(f"refusing to overwrite existing output: {path}")
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--extraction", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--overwrite", action="store_true")
    return parser


def main() -> int:
    args = build_parser().parse_args()
    extraction = args.extraction.expanduser().resolve()
    output = args.output.expanduser().resolve()
    if extraction == output:
        raise SystemExit("output must differ from extraction input")
    try:
        headers, rows = read_rows(extraction)
        public_headers, public_rows = materialize_rows(headers, rows)
        write_csv(public_headers, public_rows, output, overwrite=args.overwrite)
    except (FileExistsError, OSError, ValueError) as error:
        raise SystemExit(str(error)) from error
    print(f"materialized analysis table rows={len(public_rows)} output={output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
