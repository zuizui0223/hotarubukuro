#!/usr/bin/env python3
"""Materialize one checksum-locked exact-reproduction input from Git history."""

from __future__ import annotations

import argparse
import hashlib
import os
import shutil
import zipfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

FROZEN = {
    "broad": {
        "archive": "reproducibility/frozen_inputs/broad-minimal.zip",
        "sha256": "f9c54ff940a6eb4cf5ced7ae5049bd95fe15bc700bb72c364c95162d8755820a",
        "destination": ".",
        "merge": True,
        "required": [
            "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv",
            "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv",
            "results/ecological_v21_local_human_neighbourhood/human_neighbourhood_cell_features.csv",
            "results/ecological_v22_did_human_context/did_cell_context.csv",
        ],
    },
    "bombus_sdm": {
        "archive": "reproducibility/frozen_inputs/bombus-sdm-minimal.zip",
        "sha256": "9c3993e3b42307ed3780f3b235740b6dc9a33f3962e07b6137a61217a4cabf52",
        "destination": "source_bombus",
        "merge": False,
        "required": [
            "source_bombus/results/bombus_sdm_rebuild_A/models/ardens_ENMeval.rds",
            "source_bombus/results/bombus_sdm_rebuild_A/models/diversus_ENMeval.rds",
            "source_bombus/results/bombus_sdm_rebuild_A/evaluation/ardens_selected_model.csv",
            "source_bombus/results/bombus_sdm_rebuild_A/evaluation/diversus_selected_model.csv",
        ],
    },
    "final8_draws": {
        "archive": "reproducibility/frozen_inputs/final8-draws-minimal.zip",
        "sha256": "4c4456bc5fdca96db3cc7279f75eb146d1e8f828b2c8cc6e00bd7c9f9db8226e",
        "destination": "final8-artifact",
        "merge": False,
        "required": ["final8-artifact/final8_presence_draws10000.rds"],
    },
}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(4 * 1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def safe_extract(archive: Path, destination: Path) -> None:
    destination = destination.resolve()
    with zipfile.ZipFile(archive) as handle:
        for member in handle.infolist():
            target = (destination / member.filename).resolve()
            if os.path.commonpath((str(destination), str(target))) != str(destination):
                raise SystemExit(f"Unsafe ZIP member: {member.filename}")
        handle.extractall(destination)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("name", choices=sorted(FROZEN))
    args = parser.parse_args()
    spec = FROZEN[args.name]
    archive = ROOT / spec["archive"]
    if not archive.is_file():
        raise SystemExit(f"Frozen archive is missing: {archive.relative_to(ROOT)}")
    actual = sha256(archive)
    if actual != spec["sha256"]:
        raise SystemExit(
            f"Frozen archive checksum mismatch for {args.name}: expected {spec['sha256']}, observed {actual}"
        )
    destination = (ROOT / spec["destination"]).resolve()
    if not spec["merge"] and destination.exists():
        shutil.rmtree(destination)
    destination.mkdir(parents=True, exist_ok=True)
    safe_extract(archive, destination)
    missing = [path for path in spec["required"] if not (ROOT / path).is_file()]
    if missing:
        raise SystemExit("Materialization missing required files: " + ", ".join(missing))
    print(f"PASS {args.name} sha256={actual}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
