import argparse

from source_build.reproduce_from_zenodo import (
    EXPECTED_ROWS,
    ZENODO_MD5,
    ZENODO_RECORD,
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


def test_downstream_command_matches_run_pipeline_cli() -> None:
    args = argparse.Namespace(no_resume_analysis=True, skip_analysis_setup=True)
    command = downstream_command(args)
    assert command[-3:] == ["reproduce", "--no-resume", "--skip-setup"]
