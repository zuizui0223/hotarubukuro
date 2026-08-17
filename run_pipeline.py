#!/usr/bin/env python3
"""Canonical one-command JBI paper audit and reproduction entry point.

The exact ``reproduce`` profile intentionally starts from checksum-locked,
accepted evidence artifacts. It does not refresh live occurrence sources or
claim to rebuild non-redistributed raw YAMAP photographs.
"""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import importlib.util
import json
import os
import shutil
import subprocess
import sys
import tempfile
import urllib.error
import urllib.request
import zipfile
from pathlib import Path
from typing import Any, Iterable, Sequence

ROOT = Path(__file__).resolve().parent
DEFAULT_LOCK = ROOT / "config" / "paper_pipeline.lock.json"
DEFAULT_CACHE = ROOT / ".paper-pipeline" / "cache"
MANIFEST_PATH = ROOT / "results" / "paper_pipeline" / "run_manifest.json"
THREAD_ENV = {
    "HOTARUBUKURO_R_SEED": "20260725",
    "OMP_NUM_THREADS": "1",
    "OPENBLAS_NUM_THREADS": "1",
    "MKL_NUM_THREADS": "1",
    "VECLIB_MAXIMUM_THREADS": "1",
    "NUMEXPR_NUM_THREADS": "1",
    "GOTO_NUM_THREADS": "1",
    "BLIS_NUM_THREADS": "1",
    "INLA_NUM_THREADS": "1",
    "OMP_DYNAMIC": "FALSE",
}


class PipelineError(RuntimeError):
    """Raised when a reproducibility contract is violated."""


def utc_now() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds")


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(4 * 1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def load_lock(path: Path) -> dict[str, Any]:
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError as error:
        raise PipelineError(f"Pipeline lock is missing: {path}") from error
    except json.JSONDecodeError as error:
        raise PipelineError(f"Pipeline lock is invalid JSON: {error}") from error
    if data.get("schema_version") != 1:
        raise PipelineError("Unsupported paper pipeline lock schema.")
    if not data.get("profiles") or not data.get("stages"):
        raise PipelineError("Pipeline lock must define profiles and stages.")
    return data


def safe_resolve(root: Path, relative: str) -> Path:
    target = (root / relative).resolve()
    try:
        common = os.path.commonpath((str(root.resolve()), str(target)))
    except ValueError as error:
        raise PipelineError(f"Path escapes repository root: {relative}") from error
    if common != str(root.resolve()):
        raise PipelineError(f"Path escapes repository root: {relative}")
    return target


def command_display(command: Sequence[str]) -> str:
    return " ".join(subprocess.list2cmdline([part]) for part in command)


class Pipeline:
    def __init__(
        self,
        *,
        lock_path: Path,
        profile: str,
        cache_dir: Path,
        dry_run: bool,
        resume: bool,
        skip_bootstrap: bool,
        token: str | None,
        only_stages: list[str] | None,
        from_stage: str | None,
        to_stage: str | None,
    ) -> None:
        self.root = ROOT
        self.lock_path = lock_path.resolve()
        self.lock = load_lock(self.lock_path)
        self.profile = profile
        self.cache_dir = cache_dir.resolve()
        self.dry_run = dry_run
        self.resume = resume
        self.skip_bootstrap = skip_bootstrap
        self.token = token
        self.started_at = utc_now()
        self.records: list[dict[str, Any]] = []
        self.current_stage: str | None = None
        self.status = "RUNNING"
        self.failure: str | None = None
        self.stage_names = self._select_stages(only_stages, from_stage, to_stage)
        self.environment = os.environ.copy()
        self.environment.update(THREAD_ENV)
        self.environment.setdefault("GITHUB_REPOSITORY", self.lock["repository"])

    def _select_stages(
        self,
        only_stages: list[str] | None,
        from_stage: str | None,
        to_stage: str | None,
    ) -> list[str]:
        profiles = self.lock["profiles"]
        if self.profile not in profiles:
            raise PipelineError(
                f"Unknown profile {self.profile!r}; choose from {', '.join(sorted(profiles))}."
            )
        stages = list(profiles[self.profile])
        if self.skip_bootstrap:
            stages = [name for name in stages if not name.startswith("bootstrap_")]
        if only_stages:
            unknown = sorted(set(only_stages) - set(stages))
            if unknown:
                raise PipelineError(
                    "Requested stage is not in the selected profile: " + ", ".join(unknown)
                )
            stages = [name for name in stages if name in set(only_stages)]
        if from_stage:
            if from_stage not in stages:
                raise PipelineError(f"--from-stage is not selected: {from_stage}")
            stages = stages[stages.index(from_stage) :]
        if to_stage:
            if to_stage not in stages:
                raise PipelineError(f"--to-stage is not selected: {to_stage}")
            stages = stages[: stages.index(to_stage) + 1]
        missing = [name for name in stages if name not in self.lock["stages"]]
        if missing:
            raise PipelineError("Profile references undefined stages: " + ", ".join(missing))
        return stages

    def run(self) -> None:
        print(f"profile={self.profile}")
        print("stages=" + ",".join(self.stage_names))
        print("provenance_boundary=" + self.lock["provenance_boundary"]["exact_reproduction_starts_from"])
        try:
            for name in self.stage_names:
                self.run_stage(name)
            self.status = "SUCCESS"
        except Exception as error:
            self.status = "FAILED"
            self.failure = f"{type(error).__name__}: {error}"
            if self.current_stage:
                self.records.append(
                    {
                        "stage": self.current_stage,
                        "status": "FAILED",
                        "finished_at": utc_now(),
                        "error": self.failure,
                    }
                )
            if not self.dry_run:
                self.write_manifest()
            raise
        finally:
            self.current_stage = None

    def run_stage(self, name: str) -> None:
        self.current_stage = name
        spec = self.lock["stages"][name]
        started = utc_now()
        print(f"\n==> {name}")
        if name != "write_manifest" and self.resume and self.stage_complete(name, spec):
            print("    already complete; skipping (--resume)")
            self.records.append(
                {
                    "stage": name,
                    "status": "SKIPPED_COMPLETE",
                    "started_at": started,
                    "finished_at": utc_now(),
                }
            )
            self.current_stage = None
            return

        kind = spec.get("kind")
        if kind == "command":
            command = [self.expand(part) for part in spec["command"]]
            self.run_command(command)
        elif kind == "artifact":
            self.restore_artifact(spec["artifact"])
        elif kind == "builtin":
            self.run_builtin(spec["builtin"], spec)
        else:
            raise PipelineError(f"Unknown stage kind for {name}: {kind}")

        if not self.dry_run:
            self.assert_outputs(name, spec)
        self.records.append(
            {
                "stage": name,
                "status": "DRY_RUN" if self.dry_run else "SUCCESS",
                "started_at": started,
                "finished_at": utc_now(),
            }
        )
        self.current_stage = None

    def expand(self, value: str) -> str:
        return value.replace("{python}", sys.executable)

    def run_command(self, command: Sequence[str], *, capture: bool = False) -> subprocess.CompletedProcess[str]:
        print("    $ " + command_display(command))
        if self.dry_run:
            return subprocess.CompletedProcess(command, 0, "", "")
        try:
            return subprocess.run(
                list(command),
                cwd=self.root,
                env=self.environment,
                check=True,
                text=True,
                capture_output=capture,
            )
        except FileNotFoundError as error:
            raise PipelineError(f"Executable not found: {command[0]}") from error
        except subprocess.CalledProcessError as error:
            if capture:
                if error.stdout:
                    print(error.stdout, file=sys.stderr)
                if error.stderr:
                    print(error.stderr, file=sys.stderr)
            raise PipelineError(
                f"Command failed with exit code {error.returncode}: {command_display(command)}"
            ) from error

    def stage_complete(self, name: str, spec: dict[str, Any]) -> bool:
        if spec.get("always_run", False):
            return False
        if spec.get("kind") == "artifact":
            artifact = self.lock["artifacts"][spec["artifact"]]
            return self.paths_exist(artifact.get("required", []))
        if spec.get("builtin") == "render_submission":
            package = safe_resolve(self.root, spec["package_dir"])
            rendered = safe_resolve(self.root, spec["output_dir"])
            docx = sorted(package.glob("*.docx")) if package.is_dir() else []
            if len(docx) != int(spec["expected_docx"]):
                return False
            return all(
                (rendered / f"{item.stem}.pdf").is_file()
                and (rendered / f"{item.stem}_first_page.png").is_file()
                for item in docx
            )
        outputs = spec.get("outputs", [])
        return bool(outputs) and self.paths_exist(outputs)

    def paths_exist(self, paths: Iterable[str]) -> bool:
        return all(safe_resolve(self.root, item).exists() for item in paths)

    def assert_outputs(self, name: str, spec: dict[str, Any]) -> None:
        outputs = list(spec.get("outputs", []))
        if spec.get("kind") == "artifact":
            outputs = list(self.lock["artifacts"][spec["artifact"]].get("required", []))
        missing = [item for item in outputs if not safe_resolve(self.root, item).exists()]
        if missing:
            raise PipelineError(f"Stage {name} did not create required outputs: {', '.join(missing)}")

    def restore_artifact(self, name: str) -> None:
        artifact = self.lock["artifacts"].get(name)
        if not artifact:
            raise PipelineError(f"Artifact is not defined in lock: {name}")
        artifact_id = int(artifact["id"])
        expected = artifact["sha256"].lower()
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        archive = self.cache_dir / f"{name}-{artifact_id}.zip"

        if archive.exists() and sha256_file(archive) != expected:
            print(f"    cached checksum mismatch; deleting {archive}")
            archive.unlink()
        if not archive.exists():
            if self.dry_run:
                print(f"    would download artifact {artifact_id} -> {archive}")
                return
            if not self.token:
                raise PipelineError(
                    "A GitHub token is required to restore Actions artifacts. "
                    "Set GITHUB_TOKEN or GH_TOKEN, or run the canonical workflow."
                )
            repository = self.lock["repository"]
            url = f"https://api.github.com/repos/{repository}/actions/artifacts/{artifact_id}/zip"
            print(f"    downloading locked artifact {name} ({artifact_id})")
            request = urllib.request.Request(
                url,
                headers={
                    "Authorization": f"Bearer {self.token}",
                    "Accept": "application/vnd.github+json",
                    "User-Agent": "hotarubukuro-paper-pipeline/1",
                },
            )
            with tempfile.NamedTemporaryFile(
                prefix=f"{name}-", suffix=".zip", dir=self.cache_dir, delete=False
            ) as temporary:
                temporary_path = Path(temporary.name)
                try:
                    with urllib.request.urlopen(request, timeout=180) as response:
                        shutil.copyfileobj(response, temporary)
                except (urllib.error.URLError, TimeoutError) as error:
                    temporary_path.unlink(missing_ok=True)
                    raise PipelineError(f"Could not download artifact {artifact_id}: {error}") from error
            actual = sha256_file(temporary_path)
            if actual != expected:
                temporary_path.unlink(missing_ok=True)
                raise PipelineError(
                    f"Checksum mismatch for artifact {name}: expected {expected}, observed {actual}"
                )
            temporary_path.replace(archive)
        else:
            print(f"    using checksum-verified cache {archive}")

        actual = sha256_file(archive)
        if actual != expected:
            raise PipelineError(
                f"Checksum mismatch for artifact {name}: expected {expected}, observed {actual}"
            )
        destination = safe_resolve(self.root, artifact["destination"])
        if not artifact.get("merge", False) and destination.exists():
            shutil.rmtree(destination)
        destination.mkdir(parents=True, exist_ok=True)
        self.safe_extract(archive, destination)
        print(f"    restored into {destination.relative_to(self.root) if destination != self.root else '.'}")

    @staticmethod
    def safe_extract(archive: Path, destination: Path) -> None:
        destination = destination.resolve()
        with zipfile.ZipFile(archive) as handle:
            for member in handle.infolist():
                target = (destination / member.filename).resolve()
                try:
                    common = os.path.commonpath((str(destination), str(target)))
                except ValueError as error:
                    raise PipelineError(f"Unsafe ZIP member: {member.filename}") from error
                if common != str(destination):
                    raise PipelineError(f"Unsafe ZIP member: {member.filename}")
            handle.extractall(destination)

    def run_builtin(self, name: str, spec: dict[str, Any]) -> None:
        if name == "runtime_check":
            self.runtime_check(spec)
        elif name == "render_submission":
            self.render_submission(spec)
        elif name == "write_manifest":
            self.status = "SUCCESS"
            self.records.append(
                {
                    "stage": "write_manifest",
                    "status": "DRY_RUN" if self.dry_run else "SUCCESS",
                    "started_at": utc_now(),
                    "finished_at": utc_now(),
                }
            )
            if self.dry_run:
                print(f"    would write {MANIFEST_PATH.relative_to(self.root)}")
            else:
                self.write_manifest()
            self.records.pop()
        else:
            raise PipelineError(f"Unknown builtin stage: {name}")

    def runtime_check(self, spec: dict[str, Any]) -> None:
        missing_exec = [name for name in spec.get("executables", []) if shutil.which(name) is None]
        missing_modules = [
            name for name in spec.get("python_modules", []) if importlib.util.find_spec(name) is None
        ]
        if missing_exec or missing_modules:
            details = []
            if missing_exec:
                details.append("executables=" + ",".join(missing_exec))
            if missing_modules:
                details.append("python_modules=" + ",".join(missing_modules))
            raise PipelineError(
                "Runtime is incomplete (" + "; ".join(details) + "). "
                "Use the canonical GitHub Actions workflow or install the declared dependencies."
            )
        print("    runtime dependencies are available")

    def render_submission(self, spec: dict[str, Any]) -> None:
        package = safe_resolve(self.root, spec["package_dir"])
        output = safe_resolve(self.root, spec["output_dir"])
        docx_files = sorted(package.glob("*.docx"))
        expected = int(spec["expected_docx"])
        if len(docx_files) != expected:
            raise PipelineError(f"Expected {expected} DOCX files, found {len(docx_files)} in {package}")
        if self.dry_run:
            print(f"    would render {expected} DOCX files into {output}")
            return
        output.mkdir(parents=True, exist_ok=True)
        home = output / ".libreoffice-home"
        home.mkdir(parents=True, exist_ok=True)
        env = self.environment.copy()
        env["HOME"] = str(home)
        for item in docx_files:
            profile = output / f".lo-profile-{item.stem}"
            shutil.rmtree(profile, ignore_errors=True)
            command = [
                "libreoffice",
                f"-env:UserInstallation=file://{profile.resolve()}",
                "--headless",
                "--convert-to",
                "pdf",
                "--outdir",
                str(output),
                str(item),
            ]
            print("    $ " + command_display(command))
            subprocess.run(command, cwd=self.root, env=env, check=True)
            pdf = output / f"{item.stem}.pdf"
            if not pdf.is_file() or pdf.stat().st_size <= 1000:
                raise PipelineError(f"LibreOffice did not render a valid PDF: {pdf}")
            info = subprocess.run(
                ["pdfinfo", str(pdf)], cwd=self.root, env=env, check=True, text=True, capture_output=True
            )
            if "Pages:" not in info.stdout:
                raise PipelineError(f"pdfinfo did not report a page count: {pdf}")
            (output / f"{item.stem}.pdfinfo.txt").write_text(info.stdout, encoding="utf-8")
            prefix = output / f"{item.stem}_first_page"
            subprocess.run(
                ["pdftoppm", "-f", "1", "-singlefile", "-png", "-r", "110", str(pdf), str(prefix)],
                cwd=self.root,
                env=env,
                check=True,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            if not prefix.with_suffix(".png").is_file():
                raise PipelineError(f"First-page rendering failed: {pdf}")

    def git_commit(self) -> str | None:
        if os.environ.get("GITHUB_SHA"):
            return os.environ["GITHUB_SHA"]
        try:
            result = subprocess.run(
                ["git", "rev-parse", "HEAD"],
                cwd=self.root,
                check=True,
                text=True,
                capture_output=True,
            )
            return result.stdout.strip() or None
        except (FileNotFoundError, subprocess.CalledProcessError):
            return None

    def write_manifest(self) -> None:
        path = MANIFEST_PATH
        path.parent.mkdir(parents=True, exist_ok=True)
        payload = {
            "schema_version": 1,
            "status": self.status,
            "profile": self.profile,
            "started_at": self.started_at,
            "finished_at": utc_now(),
            "failure": self.failure,
            "repository": self.lock["repository"],
            "commit": self.git_commit(),
            "lock_path": str(self.lock_path.relative_to(self.root)),
            "lock_sha256": sha256_file(self.lock_path),
            "provenance_boundary": self.lock["provenance_boundary"],
            "artifact_locks": {
                name: {"id": value["id"], "sha256": value["sha256"], "role": value["role"]}
                for name, value in self.lock["artifacts"].items()
            },
            "stages": self.records,
            "deterministic_environment": THREAD_ENV,
        }
        path.write_text(json.dumps(payload, ensure_ascii=False, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        print(f"    wrote {path.relative_to(self.root)}")


def parser(lock: dict[str, Any] | None = None) -> argparse.ArgumentParser:
    available = sorted(lock["profiles"]) if lock else None
    value = argparse.ArgumentParser(description=__doc__)
    value.add_argument(
        "profile",
        nargs="?",
        choices=available,
        default=(lock or {}).get("default_profile", "audit"),
        help="audit repository/manuscript alignment or reproduce the locked JBI paper package",
    )
    value.add_argument("--lock", type=Path, default=DEFAULT_LOCK)
    value.add_argument("--cache-dir", type=Path, default=DEFAULT_CACHE)
    value.add_argument("--dry-run", action="store_true", help="print the selected execution without changing files")
    value.add_argument(
        "--resume",
        action=argparse.BooleanOptionalAction,
        default=False,
        help="skip stages whose declared outputs already exist (default: false)",
    )
    value.add_argument("--skip-bootstrap", action="store_true", help="do not install declared Python/R packages")
    value.add_argument("--only-stage", action="append", default=None)
    value.add_argument("--from-stage")
    value.add_argument("--to-stage")
    value.add_argument("--list-stages", action="store_true")
    return value


def main(argv: Sequence[str] | None = None) -> int:
    preliminary = argparse.ArgumentParser(add_help=False)
    preliminary.add_argument("--lock", type=Path, default=DEFAULT_LOCK)
    known, _ = preliminary.parse_known_args(argv)
    lock = load_lock(known.lock.resolve())
    arguments = parser(lock).parse_args(argv)
    if arguments.list_stages:
        for profile, stages in lock["profiles"].items():
            print(f"{profile}: " + ", ".join(stages))
        return 0
    token = os.environ.get("GITHUB_TOKEN") or os.environ.get("GH_TOKEN")
    pipeline = Pipeline(
        lock_path=arguments.lock,
        profile=arguments.profile,
        cache_dir=arguments.cache_dir,
        dry_run=arguments.dry_run,
        resume=arguments.resume,
        skip_bootstrap=arguments.skip_bootstrap,
        token=token,
        only_stages=arguments.only_stage,
        from_stage=arguments.from_stage,
        to_stage=arguments.to_stage,
    )
    try:
        pipeline.run()
    except PipelineError as error:
        print(f"pipeline error: {error}", file=sys.stderr)
        return 2
    except subprocess.CalledProcessError as error:
        print(f"pipeline command failed: {error}", file=sys.stderr)
        return int(error.returncode or 1)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
