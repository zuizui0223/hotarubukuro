from __future__ import annotations

import json
import subprocess
import sys
import urllib.request
import zipfile
from pathlib import Path

import pytest

import run_pipeline


ROOT = Path(__file__).resolve().parents[2]
LOCK_PATH = ROOT / "config" / "paper_pipeline.lock.json"


def lock() -> dict:
    return json.loads(LOCK_PATH.read_text(encoding="utf-8"))


def test_lock_has_one_exact_reproduction_profile() -> None:
    data = lock()
    assert data["schema_version"] == 1
    assert set(data["profiles"]) == {"audit", "reproduce"}
    reproduce = data["profiles"]["reproduce"]
    assert reproduce[0:3] == ["bootstrap_python", "bootstrap_r", "runtime_check"]
    assert reproduce[-2:] == ["validate_alignment", "write_manifest"]
    assert "fetch_bombus_occurrences" not in " ".join(reproduce)
    assert "download_rasters" not in " ".join(reproduce)


def test_every_declared_command_source_exists() -> None:
    data = lock()
    missing: list[str] = []
    for name, stage in data["stages"].items():
        if stage.get("kind") != "command":
            continue
        for token in stage["command"]:
            if token.endswith((".R", ".py")) and not (ROOT / token).is_file():
                missing.append(f"{name}:{token}")
    assert not missing


def test_artifact_locks_are_well_formed_and_unique() -> None:
    data = lock()
    ids = []
    for artifact in data["artifacts"].values():
        ids.append(artifact["id"])
        assert isinstance(artifact["id"], int) and artifact["id"] > 0
        assert len(artifact["sha256"]) == 64
        int(artifact["sha256"], 16)
        assert artifact["references"]
    assert len(ids) == len(set(ids))


def test_local_bombus_stage_uses_rebuilt_support_explicitly() -> None:
    command = lock()["stages"]["run_local_bombus"]["command"]
    support_index = command.index("--support") + 1
    assert command[support_index] == (
        "results/bombus_occurrence_reference_support/"
        "cell_effective_bombus_support.csv"
    )


def test_audit_dry_run_needs_no_external_runtime() -> None:
    result = subprocess.run(
        [sys.executable, "run_pipeline.py", "audit", "--dry-run", "--no-resume"],
        cwd=ROOT,
        text=True,
        capture_output=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert "profile=audit" in result.stdout
    assert "validate_alignment" in result.stdout
    assert "would write results/paper_pipeline/run_manifest.json" in result.stdout


def test_cross_host_redirect_drops_repository_credentials() -> None:
    request = urllib.request.Request(
        "https://api.github.com/repos/example/repo/actions/artifacts/1/zip",
        headers={
            "Authorization": "Bearer secret",
            "Cookie": "session=secret",
            "Accept": "application/vnd.github+json",
        },
    )
    handler = run_pipeline.StripAuthorizationOnCrossHostRedirect()
    redirected = handler.redirect_request(
        request,
        None,
        302,
        "Found",
        {},
        "https://results-receiver.actions.githubusercontent.com/artifact.zip?sig=temporary",
    )
    assert redirected is not None
    assert redirected.get_header("Authorization") is None
    assert redirected.get_header("Cookie") is None
    assert redirected.get_header("Accept") == "application/vnd.github+json"


def test_same_host_redirect_retains_repository_credentials() -> None:
    request = urllib.request.Request(
        "https://api.github.com/repos/example/repo/actions/artifacts/1/zip",
        headers={"Authorization": "Bearer secret"},
    )
    handler = run_pipeline.StripAuthorizationOnCrossHostRedirect()
    redirected = handler.redirect_request(
        request,
        None,
        302,
        "Found",
        {},
        "https://api.github.com/repos/example/repo/actions/artifacts/1/redirect",
    )
    assert redirected is not None
    assert redirected.get_header("Authorization") == "Bearer secret"


def test_safe_extract_rejects_path_traversal(tmp_path: Path) -> None:
    archive = tmp_path / "bad.zip"
    with zipfile.ZipFile(archive, "w") as handle:
        handle.writestr("../escape.txt", "bad")
    destination = tmp_path / "destination"
    destination.mkdir()
    with pytest.raises(run_pipeline.PipelineError, match="Unsafe ZIP member"):
        run_pipeline.Pipeline.safe_extract(archive, destination)
    assert not (tmp_path / "escape.txt").exists()
