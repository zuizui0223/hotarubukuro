import argparse
import csv
from pathlib import Path

import pytest

from run_pipeline import CANONICAL_DATA, build_parser as build_pipeline_parser
from source_build.build_data_s1 import PUBLIC_ANALYSIS_COLUMNS
from source_build.reproduce_from_zenodo import (
    EXPECTED_ROWS,
    ZENODO_MD5,
    ZENODO_RECORD,
    build_parser,
    downstream_command,
)
from source_build.source_contract import (
    SourceContractError,
    git_blob_sha,
    load_contract,
    sha256,
    validate_analysis_table,
)


def test_zenodo_contract_is_frozen() -> None:
    contract = load_contract()
    assert contract["contract_version"] == 2
    assert ZENODO_RECORD == "22334596"
    assert ZENODO_MD5 == "a923616e45f10f24a5463eefd09b06dd"
    assert EXPECTED_ROWS == 1965
    assert contract["historical_full_table_git_blob"] == "74b951898814f4ed15f314061e3129d8b05823d5"
    assert contract["expected_analysis_table_git_blob"] == "e119137efac89cbcfd789236f3d6a3c9599575af"
    assert contract["expected_analysis_table_sha256"] == "9e543b64a824aff82dbb55da1bca8843fb337a51399bfd60ad0a09c9bca3c33c"
    assert contract["analysis_columns"] == PUBLIC_ANALYSIS_COLUMNS
    assert contract["qc_status_counts"] == {"ok": 1180, "manual_review_required": 785}
    assert contract["manual_review_status_counts"] == {
        "not_required_by_automated_qc": 1180,
        "pending": 785,
    }


def test_raw_bootstrap_cli_exposes_one_zero_reproduction_route() -> None:
    options = build_parser().format_help()
    assert "--workbook" in options
    assert "--run-analysis" in options
    assert "--no-resume-analysis" in options
    assert "--dry-run" in options
    assert "allow-mismatch" not in options


def test_downstream_command_uses_fixed_canonical_pipeline() -> None:
    args = argparse.Namespace(no_resume_analysis=True, skip_analysis_setup=True)
    command = downstream_command(args)
    assert command[2] == "reproduce"
    assert "--data-s1" not in command
    assert "--no-resume" in command
    assert "--skip-setup" in command


def test_run_pipeline_has_no_alternative_data_input() -> None:
    help_text = build_pipeline_parser().format_help()
    assert "--data-s1" not in help_text
    args = build_pipeline_parser().parse_args(["reproduce", "--dry-run", "--skip-setup"])
    assert args.command == "reproduce"
    assert CANONICAL_DATA.as_posix().endswith(
        "results/source_reconstruction/Data_S1_from_zenodo.csv"
    )


def _base_row(observation_id: str, status: str, review: str, offset: int) -> dict[str, str]:
    row = {column: "" for column in PUBLIC_ANALYSIS_COLUMNS}
    row.update(
        observation_id=observation_id,
        source_row=str(offset + 2),
        photo_id=f"photo-{offset}",
        image_sha256=("a" if offset == 0 else "b") * 64,
        duplicate_image_sha256="False",
        date="2024-06-01",
        latitude=str(38.0 + offset),
        longitude=str(140.0 + offset),
        source_reference_type="yamap_activity",
        coordinate_source="source_workbook",
        coordinate_crs_assumed="EPSG:4326",
        coordinate_recomputed="False",
        coordinate_qc_status="source_value_not_independently_recomputed",
        photo_coordinate_qc_status="mapped_by_workbook_cell_and_image_hash",
        exact_site_id=f"site-{offset}",
        grid_30s_id=f"grid-{offset}",
        colour_measurement_scope="uncalibrated_display_referred_sRGB",
        manual_review_status=review,
        source_sheet="Sheet1",
        source_image=f"book.xlsx#image{offset}.png",
        image_width="100",
        image_height="100",
        visible_pixels="10000",
        mask_pixels="9000",
        mask_fraction_visible="0.9",
        mask_component_count="1",
        exposure_filtered_fraction="0.8",
        possible_overexposure="False",
        median_R=str(120 + offset * 10),
        median_G=str(80 + offset * 10),
        median_B=str(90 + offset * 10),
        R=str(120 + offset * 10),
        G=str(80 + offset * 10),
        B=str(90 + offset * 10),
        primary_colour_method="median_hsv_mask_v2_1_compatible",
        extraction_version="2.2.2",
        qc_status=status,
        qc_flags="",
    )
    return row


def _write_table(path: Path, *, second_status: str = "manual_review_required") -> None:
    rows = [
        _base_row("obs-1", "ok", "not_required_by_automated_qc", 0),
        _base_row("obs-2", second_status, "pending", 1),
    ]
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=PUBLIC_ANALYSIS_COLUMNS, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def _synthetic_contract(path: Path) -> dict:
    return {
        "contract_version": 2,
        "zenodo_record": "x",
        "zenodo_doi": "x",
        "zenodo_filename": "x",
        "zenodo_md5": "x",
        "expected_rows": 2,
        "historical_full_table_git_blob": "historical",
        "expected_analysis_table_git_blob": git_blob_sha(path),
        "expected_analysis_table_sha256": sha256(path),
        "analysis_columns": PUBLIC_ANALYSIS_COLUMNS,
        "qc_status_counts": {"ok": 1, "manual_review_required": 1},
        "manual_review_status_counts": {
            "not_required_by_automated_qc": 1,
            "pending": 1,
        },
    }


def test_exact_lean_analysis_contract_accepts_identical_table(tmp_path: Path) -> None:
    table = tmp_path / "table.csv"
    _write_table(table)
    report = validate_analysis_table(table, _synthetic_contract(table))
    assert report["exact_analysis_contract"] is True
    assert report["rows"] == 2


def test_exact_lean_analysis_contract_rejects_changed_colour(tmp_path: Path) -> None:
    table = tmp_path / "table.csv"
    _write_table(table)
    contract = _synthetic_contract(table)
    table.write_text(table.read_text(encoding="utf-8").replace(",120,80,90,120,80,90,", ",121,80,90,120,80,90,", 1), encoding="utf-8")
    with pytest.raises(SourceContractError, match="frozen lean contract"):
        validate_analysis_table(table, contract)


def test_analysis_contract_rejects_qc_count_drift(tmp_path: Path) -> None:
    table = tmp_path / "table.csv"
    _write_table(table, second_status="ok")
    contract = _synthetic_contract(table)
    with pytest.raises(SourceContractError, match="qc_status counts changed"):
        validate_analysis_table(table, contract)


def test_analysis_contract_rejects_schema_expansion(tmp_path: Path) -> None:
    table = tmp_path / "table.csv"
    _write_table(table)
    contract = _synthetic_contract(table)
    text = table.read_text(encoding="utf-8")
    first, rest = text.split("\n", 1)
    table.write_text(first + ",processed_at\n" + rest, encoding="utf-8")
    with pytest.raises(SourceContractError, match="schema changed"):
        validate_analysis_table(table, contract, require_exact_blob=False)
