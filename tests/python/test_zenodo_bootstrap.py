import argparse
import csv
from pathlib import Path

from run_pipeline import build_parser as build_pipeline_parser
from source_build.reproduce_from_zenodo import (
    EXPECTED_ROWS,
    ZENODO_MD5,
    ZENODO_RECORD,
    audit_rebuild,
    build_parser,
    downstream_command,
)


def test_zenodo_contract_is_frozen() -> None:
    assert ZENODO_RECORD == "22334596"
    assert ZENODO_MD5 == "a923616e45f10f24a5463eefd09b06dd"
    assert EXPECTED_ROWS == 1965


def test_raw_bootstrap_cli_exposes_zero_reproduction_controls() -> None:
    options = build_parser().format_help()
    assert "--workbook" in options
    assert "--run-analysis" in options
    assert "--no-resume-analysis" in options
    assert "--dry-run" in options


def test_downstream_command_passes_rebuilt_table_to_pipeline(tmp_path: Path) -> None:
    args = argparse.Namespace(no_resume_analysis=True, skip_analysis_setup=True)
    rebuilt = tmp_path / "Data_S1_from_zenodo.csv"
    command = downstream_command(args, rebuilt)
    assert command[2] == "reproduce"
    assert "--data-s1" in command
    assert command[command.index("--data-s1") + 1] == str(rebuilt)
    assert "--no-resume" in command
    assert "--skip-setup" in command


def test_run_pipeline_cli_accepts_explicit_data_s1() -> None:
    args = build_pipeline_parser().parse_args(
        ["reproduce", "--data-s1", "results/source_reconstruction/Data_S1_from_zenodo.csv"]
    )
    assert args.data_s1 == Path("results/source_reconstruction/Data_S1_from_zenodo.csv")


def _write_contract_csv(path: Path, *, date: str = "2024-06-01", mask_fraction: str = "0.8") -> None:
    row = {
        "observation_id": "obs-1",
        "R": "120",
        "G": "80",
        "B": "90",
        "median_R": "120",
        "median_G": "80",
        "median_B": "90",
        "latitude": "38.0",
        "longitude": "140.0",
        "date": date,
        "qc_status": "ok",
        "duplicate_image_sha256": "",
        "possible_overexposure": "false",
        "image_sha256": "abc123",
        "mask_pixels": "1000",
        "mask_fraction_visible": mask_fraction,
    }
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(row))
        writer.writeheader()
        writer.writerow(row)


def test_downstream_audit_catches_date_change_without_core_rgb_change(tmp_path: Path) -> None:
    reference = tmp_path / "reference.csv"
    rebuilt = tmp_path / "rebuilt.csv"
    _write_contract_csv(reference)
    _write_contract_csv(rebuilt, date="2024-06-02")

    report = audit_rebuild(rebuilt, reference, 1e-9, expected_rows=1)
    assert report["equivalent_core_input"] is True
    assert report["equivalent_downstream_input"] is False
    assert report["downstream_exact_mismatch_count"] == 1


def test_downstream_audit_catches_qc_numeric_change(tmp_path: Path) -> None:
    reference = tmp_path / "reference.csv"
    rebuilt = tmp_path / "rebuilt.csv"
    _write_contract_csv(reference)
    _write_contract_csv(rebuilt, mask_fraction="0.7")

    report = audit_rebuild(rebuilt, reference, 1e-9, expected_rows=1)
    assert report["equivalent_core_input"] is True
    assert report["equivalent_downstream_input"] is False
    assert report["downstream_numeric_mismatch_count"] == 1
