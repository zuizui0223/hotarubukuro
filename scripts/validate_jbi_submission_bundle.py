#!/usr/bin/env python3
"""Validate the generated JBI review/submission bundle independently."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import zipfile
from pathlib import Path
from typing import Sequence


EXPECTED_DOCX = (
    "01_Main_Manuscript_Anonymized.docx",
    "02_Supporting_Information_Appendices_S1-S6.docx",
    "03_Title_Page_TEMPLATE.docx",
    "04_Cover_Letter_TEMPLATE.docx",
    "05_Translated_Abstract_Japanese.docx",
    "06_JBI_SDM_Model_Building_Checklist.docx",
)
EXPECTED_FIGURES = tuple(
    f"Figure_{index}_{stem}{suffix}"
    for index, stem in (
        (1, "measurement_two_part_phenotype"),
        (2, "broad_environment_spatial_template"),
        (3, "local_focal_bombus_boundaries"),
        (4, "calibrated_local_departures"),
    )
    for suffix in (".png", ".pdf")
)
FORBIDDEN_IDENTIFIERS = (
    "zuizui0223",
    "rachelzhang",
    "ZHANG Ruiqi",
    "張瑞琪",
)


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(4 * 1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def assert_true(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def docx_xml(path: Path) -> tuple[str, list[str]]:
    assert_true(path.read_bytes()[:4] == b"PK\x03\x04", f"Not a DOCX/ZIP signature: {path}")
    with zipfile.ZipFile(path) as archive:
        names = archive.namelist()
        assert_true("word/document.xml" in names, f"DOCX lacks word/document.xml: {path}")
        text = "\n".join(
            archive.read(name).decode("utf-8", errors="replace")
            for name in names
            if name.startswith("word/") and name.endswith(".xml")
        )
    return text, names


def validate_manifest(package: Path) -> int:
    manifest = package / "Submission_File_Manifest.csv"
    assert_true(manifest.is_file(), "Missing Submission_File_Manifest.csv")
    with manifest.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    assert_true(len(rows) >= 20, f"Unexpectedly short submission manifest: {len(rows)}")
    seen: set[str] = set()
    for row in rows:
        relative = row["path"]
        assert_true(relative not in seen, f"Duplicate manifest path: {relative}")
        seen.add(relative)
        path = package / relative
        assert_true(path.is_file(), f"Manifest file is absent: {relative}")
        assert_true(int(row["size_bytes"]) == path.stat().st_size, f"Size mismatch: {relative}")
        assert_true(row["sha256"] == sha256_file(path), f"SHA-256 mismatch: {relative}")
    return len(rows)


def validate_bundle(root: Path) -> dict[str, object]:
    package = root / "package"
    assert_true(package.is_dir(), f"Missing package directory: {package}")
    for name in EXPECTED_DOCX:
        assert_true((package / name).is_file(), f"Missing DOCX: {name}")
    for name in EXPECTED_FIGURES:
        path = package / "Figures" / name
        assert_true(path.is_file(), f"Missing separate figure: {name}")
        signature = path.read_bytes()[:8]
        if path.suffix == ".png":
            assert_true(signature == b"\x89PNG\r\n\x1a\n", f"Invalid PNG signature: {name}")
        else:
            assert_true(signature.startswith(b"%PDF"), f"Invalid PDF signature: {name}")
        assert_true(path.stat().st_size > 10_000, f"Figure is unexpectedly small: {name}")

    main_xml, main_names = docx_xml(package / EXPECTED_DOCX[0])
    supporting_xml, _ = docx_xml(package / EXPECTED_DOCX[1])
    for name in EXPECTED_DOCX[2:]:
        docx_xml(package / name)

    leaked = [token for token in FORBIDDEN_IDENTIFIERS if token.casefold() in main_xml.casefold()]
    assert_true(not leaked, "Identifying strings in anonymized Main DOCX: " + ", ".join(leaked))
    assert_true("**" not in main_xml, "Generated Main DOCX exposes raw Markdown bold markers")
    assert_true("**" not in supporting_xml, "Generated Supporting Information exposes raw Markdown bold markers")
    assert_true(len([name for name in main_names if name.startswith("word/media/")]) == 4,
                "Main DOCX must embed exactly four Main figures")
    for heading in (
        "Introduction",
        "Materials and Methods",
        "Results",
        "Discussion",
        "References",
        "Data Accessibility Statement",
        "Figure legends and embedded figures",
    ):
        assert_true(heading in main_xml, f"Main DOCX lacks required section: {heading}")
    for index in range(1, 7):
        assert_true(f"Appendix S{index}" in supporting_xml,
                    f"Combined Supporting Information lacks Appendix S{index}")

    readiness_path = package / "Submission_Readiness.json"
    assert_true(readiness_path.is_file(), "Missing Submission_Readiness.json")
    readiness = json.loads(readiness_path.read_text(encoding="utf-8"))
    assert_true(readiness["review_science_bundle_complete"] is True,
                "Anonymous scientific bundle is not marked complete")
    assert_true(readiness["portal_ready"] is False,
                "Current template package should remain portal-incomplete until author-controlled fields are filled")
    blocker_ids = {item["id"] for item in readiness["blockers"]}
    required_blockers = {
        "authors_and_orcids",
        "corresponding_author",
        "taxon_image",
        "private_review_data_code_link",
    }
    assert_true(required_blockers.issubset(blocker_ids),
                "Readiness report failed to retain known portal blockers")

    manifest_rows = validate_manifest(package)

    archive = root / "JBI_Review_Submission_Bundle.zip"
    assert_true(archive.is_file(), "Missing JBI_Review_Submission_Bundle.zip")
    assert_true(archive.read_bytes()[:4] == b"PK\x03\x04", "Submission bundle is not a ZIP archive")
    with zipfile.ZipFile(archive) as handle:
        archived = set(handle.namelist())
    for name in EXPECTED_DOCX:
        assert_true(name in archived, f"ZIP lacks {name}")
    for name in EXPECTED_FIGURES:
        assert_true(f"Figures/{name}" in archived, f"ZIP lacks figure {name}")

    summary_path = root / "JBI_Submission_Bundle_Summary.json"
    assert_true(summary_path.is_file(), "Missing bundle summary JSON")
    summary = json.loads(summary_path.read_text(encoding="utf-8"))
    assert_true(summary["archive_sha256"] == sha256_file(archive), "Archive SHA-256 mismatch")
    assert_true(summary["main_figures"] == 4, "Summary Main-figure count mismatch")
    assert_true(summary["supporting_appendices"] == 6, "Summary appendix count mismatch")

    report = {
        "status": "PASS",
        "docx_files": len(EXPECTED_DOCX),
        "separate_figure_files": len(EXPECTED_FIGURES),
        "embedded_main_figures": 4,
        "supporting_appendices": 6,
        "manifest_rows": manifest_rows,
        "portal_ready": readiness["portal_ready"],
        "archive_sha256": summary["archive_sha256"],
        "archive_size_bytes": archive.stat().st_size,
    }
    (root / "JBI_Submission_Bundle_Validation.json").write_text(
        json.dumps(report, ensure_ascii=False, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return report


def parser() -> argparse.ArgumentParser:
    value = argparse.ArgumentParser(description=__doc__)
    value.add_argument(
        "--root",
        type=Path,
        default=Path("results/jbi_submission_bundle"),
        help="Directory containing package/, bundle ZIP and summary JSON.",
    )
    return value


def main(argv: Sequence[str] | None = None) -> int:
    arguments = parser().parse_args(argv)
    report = validate_bundle(arguments.root)
    print(json.dumps(report, ensure_ascii=False, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
