#!/usr/bin/env python3
"""Match photo timestamps to the nearest point in a GPX track.

The script is intentionally dependency-light and uses only the Python standard
library. GPX timestamps are interpreted from their explicit UTC offsets. Photo
timestamps are interpreted in the timezone supplied with ``--timezone``.
"""

from __future__ import annotations

import argparse
import csv
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Iterable
from zoneinfo import ZoneInfo


@dataclass(frozen=True)
class TrackPoint:
    time: datetime
    latitude: float
    longitude: float
    elevation_m: float | None


def parse_gpx(path: Path) -> list[TrackPoint]:
    root = ET.parse(path).getroot()
    namespace = {"gpx": "http://www.topografix.com/GPX/1/1"}
    points: list[TrackPoint] = []

    for trkpt in root.findall(".//gpx:trkpt", namespace):
        time_node = trkpt.find("gpx:time", namespace)
        if time_node is None or not time_node.text:
            continue
        timestamp = datetime.fromisoformat(time_node.text.replace("Z", "+00:00"))
        if timestamp.tzinfo is None:
            raise ValueError(f"GPX timestamp lacks timezone: {time_node.text}")
        ele_node = trkpt.find("gpx:ele", namespace)
        elevation = float(ele_node.text) if ele_node is not None and ele_node.text else None
        points.append(
            TrackPoint(
                time=timestamp,
                latitude=float(trkpt.attrib["lat"]),
                longitude=float(trkpt.attrib["lon"]),
                elevation_m=elevation,
            )
        )

    if not points:
        raise ValueError(f"No timestamped track points found in {path}")
    return sorted(points, key=lambda point: point.time)


def parse_local_time(value: str, timezone: ZoneInfo) -> datetime:
    parsed = datetime.fromisoformat(value)
    return parsed.replace(tzinfo=timezone) if parsed.tzinfo is None else parsed


def nearest_track_point(points: Iterable[TrackPoint], target: datetime) -> tuple[TrackPoint, float]:
    point = min(points, key=lambda candidate: abs((candidate.time - target).total_seconds()))
    delta_seconds = abs((point.time - target).total_seconds())
    return point, delta_seconds


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("gpx", type=Path, help="Input GPX file")
    parser.add_argument("target_time", help="Photo time, e.g. 2023-08-20T15:27:00")
    parser.add_argument("--timezone", default="Asia/Tokyo", help="Timezone for naive photo timestamps")
    parser.add_argument("--max-gap-seconds", type=float, default=300.0)
    parser.add_argument("--output", type=Path, help="Optional one-row CSV output")
    return parser


def main() -> int:
    args = build_parser().parse_args()
    timezone = ZoneInfo(args.timezone)
    target = parse_local_time(args.target_time, timezone)
    point, gap = nearest_track_point(parse_gpx(args.gpx), target)

    if gap > args.max_gap_seconds:
        raise SystemExit(
            f"Nearest GPX point is {gap:.1f} s away, exceeding --max-gap-seconds={args.max_gap_seconds}"
        )

    row = {
        "target_time": target.isoformat(),
        "matched_time": point.time.astimezone(timezone).isoformat(),
        "time_gap_seconds": gap,
        "latitude": point.latitude,
        "longitude": point.longitude,
        "elevation_m": point.elevation_m,
    }

    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        with args.output.open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=row.keys())
            writer.writeheader()
            writer.writerow(row)
    else:
        for key, value in row.items():
            print(f"{key}: {value}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
