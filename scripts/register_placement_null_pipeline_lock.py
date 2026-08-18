#!/usr/bin/env python3
"""Idempotently register the accepted v22 placement-null output in the paper lock."""

from __future__ import annotations

import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
LOCK = ROOT / "config" / "paper_pipeline.lock.json"

ARTIFACT_NAME = "accepted_placement_null_human_context"
STAGE_NAME = "run_placement_null_human_context"
ARTIFACT_ID = 9312738211
ARTIFACT_SHA256 = "4f726c2e346ec24e2e287919e4bcd017d69c50613d8ee2b3697300d5d053f362"
RESULT_ROOT = "results/placement_null_human_context"

OUTPUT_CHECKSUMS = {
    "RESULT_SUMMARY.md": "195964ff518e6466a4778c46116a243dce67111d907551e0685e3a38c6b1cceb",
    "placement_null_detector_opportunity.csv": "b7bcb47246c0aa61417de7490bd5f2c2381d1d2f66c17719c3bccf1fcdf303a7",
    "placement_null_draws.csv": "45eb9126ff228c43f134f2467771a5fc97f7c4fcd1c74d03942598ce34c0aec3",
    "placement_null_forward_candidates.csv": "c04c5959a71183e3e1acb4cb12aca3b51d72df62d7c93dd8d8fd3961b4bdb6cd",
    "placement_null_graph_registry.csv": "7329e515f8dcf543ddc5cb8dadaf003d61ec028a2c6f0eb92ff1b7f75e1b0107",
    "placement_null_neighbour_replay_summary.csv": "5751326e3b73042f0c496440c5a9069429ef6c0870df9ae5ec62c538cccf212e",
    "placement_null_population_exposure.csv": "5ef31fa1af77862ec23e6ca52ba3fc7cd67d61ea0cba0085c22f152be43c06e8",
    "placement_null_retained_v20_calibration.csv": "e72c48ee098f63935628c55b8321baafff38194f1828c34d8722094ceb58f0b7",
    "placement_null_reverse_candidates.csv": "61dff4f9dcb80905ad4a20e3a43cbd1851e269c4475e7f02a37dc1353740f8e7",
    "placement_null_scale_profile.csv": "1604082cf837dbb354914a6b030dd47312d38b117370d8688642f764d6bd901e",
    "placement_null_scale_profile.pdf": "3472a09a6ca9be2994fc7296ec0aa0c07f74138314240aeaf2b91f1acae98f96",
    "placement_null_scale_profile.png": "4fc1dbcf02c64cc841424eb0bcc4d3c20e338688c33b9c0ed9bf1be828ad9289",
    "placement_null_summary.csv": "862cdb5828647c1f14c465d25aea305d6c0292838bb89cc7c55fd0df0a3cdbf0",
    "placement_null_validation.json": "27d5878a648fdd3a7a1d281728afc7d24f39698896788d888b492a0216db91ef",
    "placement_null_worldpop_reconstruction_audit.csv": "53486172f750a1458544ae1bb3f40e0a47917aee57ce586e80dba710963d6c83",
}


def prefixed_outputs() -> list[str]:
    return [f"{RESULT_ROOT}/{name}" for name in OUTPUT_CHECKSUMS]


def insert_after(items: list[str], anchor: str, value: str) -> list[str]:
    result = [item for item in items if item != value]
    if anchor in result:
        result.insert(result.index(anchor) + 1, value)
    else:
        result.append(value)
    return result


def main() -> int:
    lock = json.loads(LOCK.read_text(encoding="utf-8"))
    if lock.get("schema_version") != 1:
        raise SystemExit("Unsupported paper-pipeline lock schema")

    lock["paper_version"] = "jbi-2026-08-18-v22-placement-null-integrated"
    lock.setdefault("profiles", {}).setdefault("reproduce", [])
    lock["profiles"]["reproduce"] = insert_after(
        list(lock["profiles"]["reproduce"]),
        "run_human_context",
        STAGE_NAME,
    )

    lock.setdefault("artifacts", {})[ARTIFACT_NAME] = {
        "id": ARTIFACT_ID,
        "sha256": ARTIFACT_SHA256,
        "destination": RESULT_ROOT,
        "merge": False,
        "required": prefixed_outputs(),
        "role": (
            "accepted predeclared v22 placement-null human-context output; "
            "the dedicated workflow performs full recomputation, whereas the "
            "paper pipeline restores this checksum-locked accepted result"
        ),
        "references": [
            ".github/workflows/placement-null-human-context.yml",
            "analysis_sensitivity/run_placement_null_human_context.R",
            "reproducibility/local_departure_placement_null_spec_2026-08-18.md",
            "reproducibility/local_departure_placement_null_input_amendment_2026-08-18.md",
            "reproducibility/placement_null_human_context_result_2026-08-18.md",
        ],
    }

    lock.setdefault("stages", {})[STAGE_NAME] = {
        "kind": "artifact",
        "artifact": ARTIFACT_NAME,
        "outputs": prefixed_outputs(),
        "analysis_command": [
            "Rscript",
            "analysis_sensitivity/run_placement_null_human_context.R",
            "--reference-root",
            "reference-artifact",
            "--final8-root",
            "final8-artifact",
            "--human-replay-root",
            "human-replay-artifact",
            "--worldpop-raster",
            "worldpop/population_count_Japan_crop.tif",
            "--output",
            RESULT_ROOT,
            "--draws",
            "10000",
            "--seed",
            "20260725",
        ],
        "seed": 20260725,
        "draws": 10000,
        "validated_run_id": 32104531917,
        "validated_head": "9c6920c98e56d4c6dd7c53970026a7605ff64f37",
        "preregistration_commit": "5247cfd1c60a7600fc6a10e2238ff5689c2bf67b",
        "inputs": {
            "broad_artifact": {
                "id": 9022276431,
                "sha256": "0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240",
            },
            "final8_draws_artifact": {
                "id": 9094339466,
                "sha256": "413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1",
            },
            "human_replay_artifact": {
                "id": 9119306089,
                "sha256": "f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4",
            },
            "worldpop_2020_japan_md5": "59eb41f5984239526e036e62c1f0a9cc",
        },
        "output_checksums": {
            f"{RESULT_ROOT}/{name}": checksum
            for name, checksum in OUTPUT_CHECKSUMS.items()
        },
    }

    alignment = lock.setdefault("alignment", {})
    required = list(alignment.setdefault("required_files", []))
    for path in [
        "reproducibility/local_departure_placement_null_spec_2026-08-18.md",
        "reproducibility/local_departure_placement_null_input_amendment_2026-08-18.md",
        "reproducibility/placement_null_human_context_result_2026-08-18.md",
        "analysis_sensitivity/run_placement_null_human_context.R",
        ".github/workflows/placement-null-human-context.yml",
    ]:
        if path not in required:
            required.append(path)
    alignment["required_files"] = required

    checks = list(alignment.setdefault("checks", []))
    checks = [
        check for check in checks
        if check.get("label") != "placement-null human-context result lock"
    ]
    checks.append({
        "label": "placement-null human-context result lock",
        "path": "reproducibility/placement_null_human_context_result_2026-08-18.md",
        "patterns": [
            "\\+0\\.090370",
            "0\\.107889",
            "0\\.172983",
            "non-monotone",
            "-0\\.049931",
            "does not establish horticultural origin",
        ],
    })
    alignment["checks"] = checks

    LOCK.write_text(
        json.dumps(lock, ensure_ascii=False, separators=(",", ":")) + "\n",
        encoding="utf-8",
    )
    print(
        f"PASS registered {STAGE_NAME} artifact={ARTIFACT_ID} "
        f"sha256={ARTIFACT_SHA256}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
