from source_build.build_data_s1 import (
    PUBLIC_ANALYSIS_COLUMNS,
    materialize_rows,
    site_identifiers,
)


HEADERS = [
    "observation_id",
    "source_row",
    "photo_id",
    "image_sha256",
    "duplicate_image_sha256",
    "date",
    "latitude",
    "longitude",
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
    "processed_at",
    "qc_status",
    "qc_flags",
    "legacy_R",
    "url",
]


def _row(observation_id: str, source_row: int, qc_status: str) -> dict[str, str]:
    return {
        "observation_id": observation_id,
        "source_row": str(source_row),
        "photo_id": f"photo-{source_row}",
        "image_sha256": ("a" if source_row == 2 else "b") * 64,
        "duplicate_image_sha256": "False",
        "date": "2024/06/01",
        "latitude": "38.12345678900001",
        "longitude": "140.987654321",
        "source_sheet": "Sheet1",
        "source_image": f"book.xlsx#image{source_row}.png",
        "image_width": "100.0",
        "image_height": "200",
        "visible_pixels": "10000.0",
        "mask_pixels": "9000",
        "mask_fraction_visible": "0.9000000000000001",
        "mask_component_count": "1.0",
        "exposure_filtered_fraction": "0.80000000000000004",
        "possible_overexposure": "False",
        "median_R": "120.0",
        "median_G": "80",
        "median_B": "90.0",
        "R": "120.0",
        "G": "80.0",
        "B": "90",
        "primary_colour_method": "channelwise_median",
        "extraction_version": "2.2.2",
        "processed_at": "2099-01-01T00:00:00Z",
        "qc_status": qc_status,
        "qc_flags": "",
        "legacy_R": "999",
        "url": "https://yamap.com/activities/123",
    }


def test_site_identifiers_are_deterministic() -> None:
    assert site_identifiers(38.123456789, 140.987654321) == site_identifiers(
        38.123456789, 140.987654321
    )
    exact, grid = site_identifiers(38.123456789, 140.987654321)
    assert exact.startswith("site-")
    assert grid.startswith("grid30s-")


def test_materializer_restores_public_qc_and_provenance_contract() -> None:
    headers, rows = materialize_rows(
        HEADERS,
        [_row("obs-ok", 2, "ok"), _row("obs-review", 3, "manual_review_required")],
    )
    assert headers == PUBLIC_ANALYSIS_COLUMNS
    assert rows[0]["manual_review_status"] == "not_required_by_automated_qc"
    assert rows[1]["manual_review_status"] == "pending"
    assert rows[0]["source_reference_type"] == "yamap_activity"
    assert rows[0]["coordinate_source"] == "source_workbook"
    assert rows[0]["coordinate_recomputed"] == "False"
    assert rows[0]["date"] == "2024-06-01"
    assert rows[0]["exact_site_id"] == rows[1]["exact_site_id"]
    assert rows[0]["grid_30s_id"] == rows[1]["grid_30s_id"]


def test_analysis_table_is_lean_and_numeric_text_is_canonical() -> None:
    headers, rows = materialize_rows(HEADERS, [_row("obs-ok", 2, "ok")])
    assert "processed_at" not in headers
    assert "legacy_R" not in headers
    assert rows[0]["R"] == "120"
    assert rows[0]["median_R"] == "120"
    assert rows[0]["mask_fraction_visible"] == "0.9"
    assert rows[0]["exposure_filtered_fraction"] == "0.8"
    assert rows[0]["image_width"] == "100"
