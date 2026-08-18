from __future__ import annotations

import hashlib
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
EXPECTED = {
    "broad-minimal.zip": "f9c54ff940a6eb4cf5ced7ae5049bd95fe15bc700bb72c364c95162d8755820a",
    "bombus-sdm-minimal.zip": "9c3993e3b42307ed3780f3b235740b6dc9a33f3962e07b6137a61217a4cabf52",
    "final8-draws-minimal.zip": "4c4456bc5fdca96db3cc7279f75eb146d1e8f828b2c8cc6e00bd7c9f9db8226e",
}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(4 * 1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def test_frozen_archive_checksums() -> None:
    root = ROOT / "reproducibility" / "frozen_inputs"
    assert {path.name for path in root.glob("*.zip")} == set(EXPECTED)
    for name, expected in EXPECTED.items():
        assert sha256(root / name) == expected


def test_frozen_archives_materialize_without_network() -> None:
    for name in ("broad", "bombus_sdm", "final8_draws"):
        completed = subprocess.run(
            [sys.executable, "scripts/materialize_frozen_input.py", name],
            cwd=ROOT,
            text=True,
            capture_output=True,
            check=True,
        )
        assert completed.stdout.startswith(f"PASS {name} sha256=")
