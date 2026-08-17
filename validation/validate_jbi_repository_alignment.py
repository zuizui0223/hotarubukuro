#!/usr/bin/env python3
"""Validate alignment between the JBI manuscript, repository map, and evidence locks."""

from __future__ import annotations

import argparse
import csv
import json
import re
from pathlib import Path
from typing import Any, Sequence


class AlignmentError(AssertionError):
    """Raised when manuscript and repository contracts diverge."""


def require(condition: bool, message: str) -> None:
    if not condition:
        raise AlignmentError(message)


def load_json(path: Path) -> dict[str, Any]:
    require(path.is_file(), f"Missing JSON file: {path}")
    return json.loads(path.read_text(encoding="utf-8"))


def validate_active_map(root: Path) -> dict[str, Any]:
    path = root / "paper" / "active-file-map.csv"
    require(path.is_file(), f"Missing paper map: {path}")
    with path.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    require(rows, "paper/active-file-map.csv is empty")
    required_columns = {"role", "section", "path", "status"}
    require(required_columns.issubset(rows[0]), "paper map has an invalid header")
    paths = [row["path"].strip() for row in rows]
    require(len(paths) == len(set(paths)), "paper map contains duplicate paths")
    missing = [item for item in paths if not (root / item).is_file()]
    require(not missing, "paper map points to missing files: " + ", ".join(missing))
    primary = {row["path"].strip() for row in rows if row["status"].strip() == "primary"}
    expected_primary = {
        "README.md",
        "run_pipeline.py",
        "config/paper_pipeline.lock.json",
        ".github/workflows/paper-pipeline.yml",
        "docs/reproduction-guide.md",
        "paper/README.md",
        "paper/analysis-map.md",
        "submission/jbi/JBI_main_manuscript_anonymized.md",
        "validation/validate_jbi_repository_alignment.py",
    }
    require(
        expected_primary.issubset(primary),
        "paper map omits primary entry points: " + ", ".join(sorted(expected_primary - primary)),
    )
    require(
        "FINAL_PIPELINE_SUPP_MANUSCRIPT_AUDIT_2026-08-12.md" not in paths,
        "superseded root-level dated audit remains in the active paper map",
    )
    return {"rows": len(rows), "primary": len(primary)}


def validate_alignment(root: Path, lock_path: Path) -> dict[str, Any]:
    lock = load_json(lock_path)
    require(lock.get("schema_version") == 1, "unsupported paper pipeline lock schema")
    required_files = lock["alignment"]["required_files"]
    missing = [item for item in required_files if not (root / item).is_file()]
    require(not missing, "alignment inputs are missing: " + ", ".join(missing))

    checks: list[dict[str, Any]] = []
    for specification in lock["alignment"]["checks"]:
        relative = specification["path"]
        text = (root / relative).read_text(encoding="utf-8")
        absent = [
            pattern
            for pattern in specification["patterns"]
            if re.search(pattern, text, flags=re.IGNORECASE | re.MULTILINE) is None
        ]
        require(
            not absent,
            f"{specification['label']} is not aligned in {relative}; missing patterns: "
            + ", ".join(absent),
        )
        checks.append(
            {
                "label": specification["label"],
                "path": relative,
                "patterns": len(specification["patterns"]),
                "status": "PASS",
            }
        )

    artifact_checks: list[dict[str, Any]] = []
    for name, artifact in lock["artifacts"].items():
        artifact_id = str(artifact["id"])
        digest = artifact["sha256"]
        references = artifact.get("references", [])
        require(references, f"artifact {name} has no declared repository references")
        for relative in references:
            path = root / relative
            require(path.is_file(), f"artifact reference file is missing: {relative}")
            text = path.read_text(encoding="utf-8")
            require(
                artifact_id in text,
                f"artifact id {artifact_id} for {name} is absent from {relative}",
            )
            require(
                digest in text,
                f"artifact checksum for {name} is absent from {relative}",
            )
        artifact_checks.append(
            {
                "artifact": name,
                "id": int(artifact_id),
                "references": len(references),
                "status": "PASS",
            }
        )

    manuscript = (root / "submission/jbi/JBI_main_manuscript_anonymized.md").read_text(
        encoding="utf-8"
    )
    claim_ceiling_patterns = {
        "Bombus availability is not direct pollination evidence": (
            r"not evidence of pollinator-mediated selection"
        ),
        "local departures are not proof of human origin": r"not enough to claim human origin",
        "high-elevation overlap is not an independent mechanism": r"disappeared after controlling elevation",
    }
    for label, pattern in claim_ceiling_patterns.items():
        require(
            re.search(pattern, manuscript, flags=re.IGNORECASE) is not None,
            f"manuscript lost claim ceiling: {label}",
        )

    active_map = validate_active_map(root)
    return {
        "status": "PASS",
        "paper_version": lock["paper_version"],
        "manuscript": "submission/jbi/JBI_main_manuscript_anonymized.md",
        "alignment_checks": checks,
        "artifact_checks": artifact_checks,
        "active_file_map": active_map,
        "claim_ceilings": sorted(claim_ceiling_patterns),
        "provenance_boundary": lock["provenance_boundary"],
        "durability_warning": (
            "The evidence is checksum locked but currently referenced through retention-bound "
            "GitHub Actions artifacts. A release or external archival deposit is still required "
            "for long-term reproducibility."
        ),
    }


def parser() -> argparse.ArgumentParser:
    value = argparse.ArgumentParser(description=__doc__)
    value.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    value.add_argument("--lock", type=Path, default=Path("config/paper_pipeline.lock.json"))
    value.add_argument(
        "--output",
        type=Path,
        default=Path("results/paper_pipeline/jbi_repository_alignment.json"),
    )
    return value


def main(argv: Sequence[str] | None = None) -> int:
    arguments = parser().parse_args(argv)
    root = arguments.root.resolve()
    lock_path = arguments.lock if arguments.lock.is_absolute() else root / arguments.lock
    output = arguments.output if arguments.output.is_absolute() else root / arguments.output
    report = validate_alignment(root, lock_path)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(
        json.dumps(report, ensure_ascii=False, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    print(json.dumps(report, ensure_ascii=False, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
