import csv
import json
from pathlib import Path

import pytest

from scripts.build_data_s1 import build_public_rows, main, read_table, sha256_file


def extraction_row(**overrides):
    row = {
        "observation_id": "obs-000001",
        "source_row": 2,
        "photo_id": "image1",
        "source_path": "/private/source.xlsx#xl/media/image1.png",
        "mask_path": "/private/qc/mask.png",
        "overlay_path": "/private/qc/overlay.png",
        "qc_panel_path": "/private/qc/panel.png",
        "url": "https://yamap.com/activities/1",
        "date": "2024-07-01",
        "latitude": 35.0,
        "longitude": 138.0,
        "legacy_R": 180,
        "legacy_G": 170,
        "legacy_B": 175,
        "mean_R": 201,
        "mean_G": 190,
        "mean_B": 195,
        "median_R": 200,
        "median_G": 189,
        "median_B": 194,
        "hsv_peak_R": 198,
        "hsv_peak_G": 188,
        "hsv_peak_B": 193,
        "hsv_exposure_filtered_peak_R": 198,
        "hsv_exposure_filtered_peak_G": 188,
        "hsv_exposure_filtered_peak_B": 193,
        "alpha_peak_R": 197,
        "alpha_peak_G": 187,
        "alpha_peak_B": 192,
        "R": 200,
        "G": 189,
        "B": 194,
        "image_sha256": "a" * 64,
        "duplicate_image_sha256": False,
        "primary_colour_method": "median_hsv_mask_v2_1_compatible",
        "extraction_version": "2.2.2",
        "qc_status": "ok",
    }
    row.update(overrides)
    return row


def test_public_export_removes_private_linkage_and_adds_coordinate_provenance():
    row = extraction_row()
    headers, rows = build_public_rows(list(row), [row])
    result = rows[0]

    assert not {
        "source_path", "mask_path", "overlay_path", "qc_panel_path", "url"
    }.intersection(headers)
    assert result["coordinate_recomputed"] is False
    assert result["coordinate_qc_status"] == "source_value_not_independently_recomputed"
    assert result["source_reference_type"] == "yamap_activity"
    assert result["date"] == "2024-07-01"
    assert result["exact_site_id"].startswith("site-")
    assert result["grid_30s_id"].startswith("grid30s-")
    assert result["R"] == result["median_R"]
    assert result["manual_review_status"] == "not_required_by_automated_qc"


def test_duplicate_photo_coordinate_mapping_is_never_silently_accepted():
    row = extraction_row(
        duplicate_image_sha256=True,
        qc_status="manual_review_required",
    )
    _, rows = build_public_rows(list(row), [row])
    assert "duplicate_photo" in rows[0]["photo_coordinate_qc_status"]
    assert rows[0]["manual_review_status"] == "pending"


def test_cli_writes_sanitized_data_manifest_and_comparison(tmp_path: Path):
    extraction = tmp_path / "extraction.csv"
    row = extraction_row()
    with extraction.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(row))
        writer.writeheader()
        writer.writerow(row)

    output = tmp_path / "Data_S1.csv"
    manifest = tmp_path / "manifest.json"
    comparison = tmp_path / "comparison.csv"
    assert main(
        [
            "--extraction",
            str(extraction),
            "--output",
            str(output),
            "--manifest",
            str(manifest),
            "--comparison",
            str(comparison),
        ]
    ) == 0

    with output.open(newline="", encoding="utf-8") as handle:
        written = list(csv.DictReader(handle))
    assert len(written) == 1
    assert "/private/" not in output.read_text(encoding="utf-8")
    assert "yamap.com/activities" not in output.read_text(encoding="utf-8")
    manifest_data = json.loads(manifest.read_text(encoding="utf-8"))
    assert manifest_data["records"] == 1
    assert "url" in manifest_data["excluded_public_columns"]
    assert manifest_data["source_reference_counts"] == {
        "field_survey_or_other": 0,
        "unique": 1,
        "yamap_activity": 1,
    }
    with comparison.open(newline="", encoding="utf-8") as handle:
        comparison_data = list(csv.DictReader(handle))
    assert len(comparison_data) == 5
    assert {row["metric"] for row in comparison_data} == {"DeltaE76"}
    assert all(
        key in comparison_data[0]
        for key in ("delta_L_mean", "delta_a_mean", "delta_b_mean")
    )

    with pytest.raises(FileExistsError, match="Refusing to overwrite"):
        main(
            [
                "--extraction",
                str(extraction),
                "--output",
                str(output),
                "--manifest",
                str(manifest),
                "--comparison",
                str(comparison),
            ]
        )


def test_committed_standard_input_matches_its_manifest():
    root = Path(__file__).resolve().parents[2]
    data_path = root / "Data_S1.csv"
    manifest = json.loads(
        (root / "data" / "processed" / "Data_S1_v2_manifest.json").read_text(
            encoding="utf-8"
        )
    )
    headers, rows = read_table(data_path)

    assert sha256_file(data_path) == manifest["output_sha256"]
    assert headers == manifest["columns"]
    assert len(rows) == manifest["records"] == 1965
    assert "url" not in headers
    assert manifest["extraction_versions"] == {"2.2.2": 1965}
    assert manifest["qc_status_counts"] == {
        "manual_review_required": 785,
        "ok": 1180,
    }
    assert manifest["source_reference_counts"] == {
        "field_survey_or_other": 1,
        "unique": 1881,
        "yamap_activity": 1964,
    }
