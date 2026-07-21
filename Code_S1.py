#!/usr/bin/env python3
"""Georeference photo times against GPX tracks without relying on row order.

For a full batch, provide a CSV manifest with ``observation_id``,
``photo_time`` and ``gpx_path``.  Paths in the manifest are resolved relative
to the manifest itself.  A single photo can instead be processed with
``--gpx`` and ``--time``.  Timestamps without an offset use ``--timezone``.
"""

from __future__ import annotations

import argparse
import bisect
import csv
import hashlib
import json
import os
import tempfile
import xml.etree.ElementTree as ET
from dataclasses import asdict, dataclass
from datetime import datetime
from pathlib import Path
from typing import Optional
from zoneinfo import ZoneInfo


@dataclass(frozen=True)
class TrackPoint:
    time: datetime
    latitude: float
    longitude: float
    elevation_m: Optional[float]
    segment_id: str


@dataclass(frozen=True)
class TrackMatch:
    latitude: float
    longitude: float
    elevation_m: Optional[float]
    segment_id: str
    method: str
    before_time: str
    after_time: str
    nearest_time_gap_seconds: float
    bracket_seconds: float


def _local_name(tag: str) -> str:
    return tag.rsplit("}", 1)[-1]


def _child_text(element: ET.Element, name: str) -> Optional[str]:
    for child in element:
        if _local_name(child.tag) == name:
            return child.text
    return None


def _parse_timestamp(value: str) -> datetime:
    parsed = datetime.fromisoformat(value.strip().replace("Z", "+00:00"))
    if parsed.tzinfo is None:
        raise ValueError(f"GPX timestamp lacks a UTC offset: {value!r}")
    return parsed


def parse_photo_time(value: str, timezone: str) -> datetime:
    parsed = datetime.fromisoformat(value.strip().replace("Z", "+00:00"))
    if parsed.tzinfo is None:
        parsed = parsed.replace(tzinfo=ZoneInfo(timezone))
    return parsed


def parse_gpx(path: Path) -> dict[str, list[TrackPoint]]:
    """Return sorted, timestamped points without joining separate segments."""
    root = ET.parse(path).getroot()
    segments: dict[str, list[TrackPoint]] = {}
    tracks = [node for node in root.iter() if _local_name(node.tag) == "trk"]
    for track_index, track in enumerate(tracks, start=1):
        track_segments = [node for node in track if _local_name(node.tag) == "trkseg"]
        for segment_index, segment in enumerate(track_segments, start=1):
            segment_id = f"track{track_index}:segment{segment_index}"
            points: list[TrackPoint] = []
            for node in segment:
                if _local_name(node.tag) != "trkpt":
                    continue
                timestamp = _child_text(node, "time")
                if not timestamp:
                    continue
                elevation = _child_text(node, "ele")
                points.append(
                    TrackPoint(
                        time=_parse_timestamp(timestamp),
                        latitude=float(node.attrib["lat"]),
                        longitude=float(node.attrib["lon"]),
                        elevation_m=float(elevation) if elevation not in (None, "") else None,
                        segment_id=segment_id,
                    )
                )
            if points:
                segments[segment_id] = sorted(points, key=lambda point: point.time)
    if not segments:
        raise ValueError(f"No timestamped track segments found in {path}")
    return segments


def _linear_value(before: Optional[float], after: Optional[float], fraction: float) -> Optional[float]:
    if before is None or after is None:
        return before if fraction < 0.5 else after
    return before + fraction * (after - before)


def match_track(
    segments: dict[str, list[TrackPoint]],
    target: datetime,
    *,
    max_time_gap_seconds: float = 300.0,
    max_interpolation_gap_seconds: float = 120.0,
) -> TrackMatch:
    """Match within one GPX segment and interpolate only across short gaps."""
    candidates: list[tuple[float, TrackPoint, TrackPoint]] = []
    target_epoch = target.timestamp()
    for points in segments.values():
        epochs = [point.time.timestamp() for point in points]
        position = bisect.bisect_left(epochs, target_epoch)
        before = points[max(0, position - 1)]
        after = points[min(len(points) - 1, position)]
        nearest_gap = min(abs((target - before.time).total_seconds()), abs((after.time - target).total_seconds()))
        candidates.append((nearest_gap, before, after))
    nearest_gap, before, after = min(candidates, key=lambda item: item[0])
    if nearest_gap > max_time_gap_seconds:
        raise ValueError(
            f"nearest GPX point is {nearest_gap:.1f}s away; limit is {max_time_gap_seconds:.1f}s"
        )

    bracket = (after.time - before.time).total_seconds()
    inside = before.time <= target <= after.time
    if inside and 0 < bracket <= max_interpolation_gap_seconds:
        fraction = (target - before.time).total_seconds() / bracket
        return TrackMatch(
            latitude=_linear_value(before.latitude, after.latitude, fraction),
            longitude=_linear_value(before.longitude, after.longitude, fraction),
            elevation_m=_linear_value(before.elevation_m, after.elevation_m, fraction),
            segment_id=before.segment_id,
            method="linear_interpolation",
            before_time=before.time.isoformat(),
            after_time=after.time.isoformat(),
            nearest_time_gap_seconds=nearest_gap,
            bracket_seconds=bracket,
        )

    nearest = before if abs((target - before.time).total_seconds()) <= abs((after.time - target).total_seconds()) else after
    return TrackMatch(
        latitude=nearest.latitude,
        longitude=nearest.longitude,
        elevation_m=nearest.elevation_m,
        segment_id=nearest.segment_id,
        method="nearest_point",
        before_time=nearest.time.isoformat(),
        after_time=nearest.time.isoformat(),
        nearest_time_gap_seconds=nearest_gap,
        bracket_seconds=0.0,
    )


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def process_manifest(args: argparse.Namespace) -> list[dict[str, object]]:
    manifest = args.manifest.resolve()
    with manifest.open(newline="", encoding="utf-8-sig") as handle:
        rows = list(csv.DictReader(handle))
    required = {"observation_id", "photo_time", "gpx_path"}
    if not rows or not required.issubset(rows[0]):
        raise ValueError(f"manifest requires columns: {', '.join(sorted(required))}")
    identifiers = [row["observation_id"].strip() for row in rows]
    if any(not identifier for identifier in identifiers):
        raise ValueError("observation_id must be non-empty")
    if len(identifiers) != len(set(identifiers)):
        raise ValueError("observation_id must be unique")

    cache: dict[Path, tuple[dict[str, list[TrackPoint]], str]] = {}
    output: list[dict[str, object]] = []
    for row in rows:
        base = {
            "observation_id": row["observation_id"],
            "photo_time": row["photo_time"],
            "gpx_path": row["gpx_path"],
        }
        try:
            gpx_path = (manifest.parent / row["gpx_path"]).resolve()
            if gpx_path not in cache:
                cache[gpx_path] = (parse_gpx(gpx_path), sha256_file(gpx_path))
            segments, digest = cache[gpx_path]
            timezone = row.get("timezone") or args.timezone
            match = match_track(
                segments,
                parse_photo_time(row["photo_time"], timezone),
                max_time_gap_seconds=args.max_time_gap_seconds,
                max_interpolation_gap_seconds=args.max_interpolation_gap_seconds,
            )
            output.append({**base, "gpx_sha256": digest, **asdict(match), "qc_status": "ok", "qc_note": ""})
        except Exception as error:  # retain the observation and its failure reason
            output.append({**base, "gpx_sha256": "", "qc_status": "no_match", "qc_note": str(error)})
    return output


def _canonical_path(path: Path) -> Path:
    return path.expanduser().resolve(strict=False)


def validate_output_path(args: argparse.Namespace) -> None:
    """Prevent accidental replacement of an input or an existing result."""

    output = _canonical_path(args.output)
    inputs = [path for path in (args.manifest, args.gpx) if path is not None]
    if any(output == _canonical_path(path) for path in inputs):
        raise ValueError("output path must differ from the input path")
    if output.exists() and not args.overwrite:
        raise FileExistsError(f"refusing to overwrite existing output: {output}")


def write_csv(rows: list[dict[str, object]], path: Path, *, overwrite: bool = False) -> None:
    if path.exists() and not overwrite:
        raise FileExistsError(f"refusing to overwrite existing output: {path}")
    path.parent.mkdir(parents=True, exist_ok=True)
    fieldnames: list[str] = []
    for row in rows:
        for key in row:
            if key not in fieldnames:
                fieldnames.append(key)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{path.name}.", suffix=".part", dir=path.parent
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=fieldnames, extrasaction="ignore")
            writer.writeheader()
            writer.writerows(rows)
            handle.flush()
            os.fsync(handle.fileno())
        if path.exists() and not overwrite:
            raise FileExistsError(f"refusing to overwrite existing output: {path}")
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, help="Batch CSV input")
    parser.add_argument("--gpx", type=Path, help="GPX input for a single timestamp")
    parser.add_argument("--time", help="Photo timestamp for --gpx")
    parser.add_argument("--observation-id", default="observation_1")
    parser.add_argument("--timezone", default="Asia/Tokyo")
    parser.add_argument("--max-time-gap-seconds", type=float, default=300.0)
    parser.add_argument("--max-interpolation-gap-seconds", type=float, default=120.0)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Explicitly allow replacement of an existing output file",
    )
    return parser


def main() -> int:
    args = build_parser().parse_args()
    try:
        validate_output_path(args)
    except (FileExistsError, ValueError) as error:
        raise SystemExit(str(error)) from error
    if args.manifest:
        if args.gpx or args.time:
            raise SystemExit("use either --manifest or --gpx/--time")
        rows = process_manifest(args)
    else:
        if not (args.gpx and args.time):
            raise SystemExit("provide --manifest or both --gpx and --time")
        path = args.gpx.resolve()
        try:
            match = match_track(
                parse_gpx(path),
                parse_photo_time(args.time, args.timezone),
                max_time_gap_seconds=args.max_time_gap_seconds,
                max_interpolation_gap_seconds=args.max_interpolation_gap_seconds,
            )
        except ValueError as error:
            raise SystemExit(str(error)) from error
        rows = [{
            "observation_id": args.observation_id,
            "photo_time": args.time,
            "gpx_path": str(path),
            "gpx_sha256": sha256_file(path),
            **asdict(match),
            "qc_status": "ok",
            "qc_note": "",
        }]
    write_csv(rows, args.output, overwrite=args.overwrite)
    summary = {"records": len(rows), "ok": sum(row.get("qc_status") == "ok" for row in rows), "output": str(args.output.resolve())}
    print(json.dumps(summary, ensure_ascii=False))
    return 0 if summary["ok"] == summary["records"] else 2


if __name__ == "__main__":
    raise SystemExit(main())
