from source_build.build_data_s1 import materialize_rows, site_identifiers


HEADERS = [
    "observation_id",
    "source_row",
    "photo_id",
    "image_sha256",
    "duplicate_image_sha256",
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
    "url",
]


def _row(observation_id: str, source_row: int, qc_status: str) -> dict[str, str]:
    return {
        "observation_id": observation_id,
        "source_row": str(source_row),
        "photo_id": f"photo-{source_row}",
        "image_sha256": ("a" if source_row == 2 else "b") * 64,
        "duplicate_image_sha256": "",
        "date": "2024/06/01",
        "latitude": "38.123456789",
        "longitude": "140.987654321",
        "median_R": "120",
        "median_G": "80",
        "median_B": "90",
        "R": "120",
        "G": "80",
        "B": "90",
        "primary_colour_method": "channelwise_median",
        "extraction_version": "2.2.2",
        "qc_status": qc_status,
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
    _, rows = materialize_rows(
        HEADERS,
        [_row("obs-ok", 2, "ok"), _row("obs-review", 3, "manual_review_required")],
    )
    assert rows[0]["manual_review_status"] == "not_required_by_automated_qc"
    assert rows[1]["manual_review_status"] == "pending"
    assert rows[0]["source_reference_type"] == "yamap_activity"
    assert rows[0]["coordinate_source"] == "source_workbook"
    assert rows[0]["coordinate_recomputed"] is False
    assert rows[0]["date"] == "2024-06-01"
    assert rows[0]["exact_site_id"] == rows[1]["exact_site_id"]
    assert rows[0]["grid_30s_id"] == rows[1]["grid_30s_id"]
