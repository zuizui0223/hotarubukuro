from datetime import datetime, timezone
from pathlib import Path

import pytest

from argparse import Namespace

from Code_S1 import match_track, parse_gpx, validate_output_path, write_csv


GPX = """<?xml version="1.0"?>
<gpx xmlns="http://www.topografix.com/GPX/1/1">
  <trk>
    <trkseg>
      <trkpt lat="35" lon="139"><ele>10</ele><time>2024-01-01T00:00:00Z</time></trkpt>
      <trkpt lat="36" lon="140"><ele>20</ele><time>2024-01-01T00:01:00Z</time></trkpt>
    </trkseg>
    <trkseg>
      <trkpt lat="40" lon="145"><time>2024-01-01T00:01:01Z</time></trkpt>
    </trkseg>
  </trk>
</gpx>
"""


def test_gpx_interpolation_stays_within_segment(tmp_path: Path) -> None:
    path = tmp_path / "track.gpx"
    path.write_text(GPX, encoding="utf-8")
    match = match_track(
        parse_gpx(path),
        datetime(2024, 1, 1, 0, 0, 30, tzinfo=timezone.utc),
    )
    assert match.method == "linear_interpolation"
    assert match.segment_id == "track1:segment1"
    assert match.latitude == pytest.approx(35.5)
    assert match.longitude == pytest.approx(139.5)


def test_gpx_rejects_distant_photo_time(tmp_path: Path) -> None:
    path = tmp_path / "track.gpx"
    path.write_text(GPX, encoding="utf-8")
    with pytest.raises(ValueError, match="limit"):
        match_track(
            parse_gpx(path),
            datetime(2024, 1, 2, tzinfo=timezone.utc),
            max_time_gap_seconds=60,
        )


def test_output_refuses_existing_file_without_overwrite(tmp_path: Path) -> None:
    output = tmp_path / "coordinates.csv"
    output.write_text("old\n", encoding="utf-8")
    with pytest.raises(FileExistsError, match="refusing to overwrite"):
        write_csv([{"observation_id": "obs-1"}], output)
    assert output.read_text(encoding="utf-8") == "old\n"


def test_output_cannot_be_the_manifest(tmp_path: Path) -> None:
    manifest = tmp_path / "manifest.csv"
    manifest.write_text("observation_id,photo_time,gpx_path\n", encoding="utf-8")
    args = Namespace(
        output=manifest,
        manifest=manifest,
        gpx=None,
        overwrite=True,
    )
    with pytest.raises(ValueError, match="must differ"):
        validate_output_path(args)
