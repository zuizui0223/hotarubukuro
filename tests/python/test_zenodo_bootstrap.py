import argparse
import csv
from pathlib import Path

import pytest

from run_pipeline import CANONICAL_DATA, build_parser as build_pipeline_parser
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
    validate_public_table,
)


def test_zenodo_contract_is_frozen() -> None:
    contract = load_contract()
    assert ZENODO_RECORD == "22334596"
    assert ZENODO_MD5 == "a923616e45f10f24a5463eefd09b06dd"
    assert EXPECTED_ROWS == 1965
    assert contract["expected_public_table_git_blob"] == "74b951898814f4ed15f314061e3129d8b05823d5"
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


def _write_table(path: Path, *, second_status: str = "manual_review_required") -> None:
    rows = [
        {
            "observation_id": "obs-1",
            "latitude": "38.0",
            "longitude": "140.0",
            "R": "120",
            "G": "80",
            "B": "90",
            "qc_status": "ok",
            "manual_review_status": "not_required_by_automated_qc",
            "image_sha256": "a" * 64,
            "exact_site_id": "site-1",
            "grid_30s_id": "grid-1",
        },
        {
            "observation_id": "obs-2",
            "latitude": "39.0",
            "longitude": "141.0",
            "R": "130",
            "G": "90",
            "B": "100",
            "qc_status": second_status,
            "manual_review_status": "pending",
            "image_sha256": "b" * 64,
            "exact_site_id": "site-2",
            "grid_30s_id": "grid-2",
        },
    ]
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def _synthetic_contract(path: Path) -> dict:
    return {
        "zenodo_record": "x",
        "zenodo_doi": "x",
        "zenodo_filename": "x",
        "zenodo_md5": "x",
        "expected_rows": 2,
        "expected_public_table_git_blob": git_blob_sha(path),
        "required_public_columns": [
            "observation_id", "latitude", "longitude", "R", "G", "B",
            "qc_status", "manual_review_status", "image_sha256",
            "exact_site_id", "grid_30s_id",
        ],
        "qc_status_counts": {"ok": 1, "manual_review_required": 1},
        "manual_review_status_counts": {
            "not_required_by_automated_qc": 1,
            "pending": 1,
        },
    }


def test_exact_generated_table_contract_accepts_identical_bytes(tmp_path: Path) -> None:
    table = tmp_path / "table.csv"
    _write_table(table)
    report = validate_public_table(table, _synthetic_contract(table))
    assert report["exact_public_contract"] is True
    assert report["rows"] == 2


def test_exact_generated_table_contract_rejects_changed_bytes(tmp_path: Path) -> None:
    table = tmp_path / "table.csv"
    _write_table(table)
    contract = _synthetic_contract(table)
    table.write_text(table.read_text(encoding="utf-8").replace("120", "121", 1), encoding="utf-8")
    with pytest.raises(SourceContractError, match="exact-output contract"):
        validate_public_table(table, contract)


def test_generated_table_contract_rejects_qc_count_drift(tmp_path: Path) -> None:
    table = tmp_path / "table.csv"
    _write_table(table, second_status="ok")
    contract = _synthetic_contract(table)
    with pytest.raises(SourceContractError, match="qc_status counts changed"):
        validate_public_table(table, contract)
