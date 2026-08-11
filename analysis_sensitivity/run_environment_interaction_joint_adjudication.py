#!/usr/bin/env python3
"""Generate and execute the narrow exact INLA-SPDE joint adjudication.

This wrapper reuses scripts/run_environment_interaction_inla_screen.R and only
replaces its interaction/model registry. All model-fitting, mesh, prior,
geographic-fold, CPO/WAIC, VIF and spatial-block-bootstrap logic therefore stays
identical to the completed exact screens.
"""

from __future__ import annotations

import argparse
import csv
import json
import subprocess
import tempfile
from pathlib import Path


CUSTOM_REGISTRY = r"""
registry <- data.frame(
  interaction_id = c(
    "thermal_variability",
    "moisture_thermal_variability",
    "dryness_precip_variability"
  ),
  term = c(
    "int_thermal_variability",
    "int_moisture_thermal_variability",
    "int_dryness_precip_variability"
  ),
  x = c(
    "env_Temperature_PC1",
    "env_precip_PC1",
    "env_Dryness"
  ),
  y = c(
    "env_TemperatureSeasonality",
    "env_TemperatureSeasonality",
    "env_PrecipSeasonality"
  ),
  mechanism_family = c(
    "thermal_regulation",
    "hydrothermal_context",
    "stress_variability"
  ),
  directional_expectation = c(
    "temperature_slope_changes_with_thermal_seasonality",
    "moisture_slope_changes_with_thermal_seasonality",
    "dryness_effect_changes_with_precipitation_intermittency"
  ),
  rationale = c(
    "Predeclared interaction that passed the exact coefficient-plus-geographical-transfer rule.",
    "Exhaustive-only signal sharing the same temperature-seasonality axis; tested for independent contribution.",
    "Predeclared competing stress-variability interaction retained for parsimony adjudication."
  ),
  stringsAsFactors = FALSE
)

model_registry <- data.frame(
  model_id = c(
    "base_additive",
    "single_thermal_variability",
    "single_moisture_thermal_variability",
    "joint_thermal_and_moisture_variability",
    "predeclared_stress_variability_bundle",
    "joint_core_plus_precip_variability"
  ),
  model_class = c(
    "base",
    "single_interaction",
    "single_interaction",
    "joint_core",
    "predeclared_bundle",
    "joint_core_plus_predeclared"
  ),
  interaction_terms = c(
    "",
    "int_thermal_variability",
    "int_moisture_thermal_variability",
    "int_thermal_variability;int_moisture_thermal_variability",
    "int_thermal_variability;int_dryness_precip_variability",
    "int_thermal_variability;int_moisture_thermal_variability;int_dryness_precip_variability"
  ),
  stringsAsFactors = FALSE
)

"""


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--base-script",
        type=Path,
        default=Path("scripts/run_environment_interaction_inla_screen.R"),
    )
    parser.add_argument(
        "--input-csv",
        type=Path,
        default=Path(
            "reference-artifact/results/ecological_v11_pigmentation_hurdle/"
            "analysis_data_pigmentation_hurdle.csv"
        ),
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path(
            "results/environment_interaction_joint_adjudication/"
            "conditional_intensity"
        ),
    )
    parser.add_argument("--bootstrap-reps", type=int, default=4000)
    parser.add_argument("--seed", type=int, default=20260813)
    return parser.parse_args()


def patch_script(source: str) -> str:
    registry_start = source.index("registry <- data.frame(")
    split_start = source.index("split_terms <- function")
    patched = source[:registry_start] + CUSTOM_REGISTRY + source[split_start:]

    lock_needle = "lock_rows <- lapply(seq_len(nrow(lock_targets)), function(i) {"
    if patched.count(lock_needle) != 1:
        raise RuntimeError("Unable to locate frozen-lock evaluation point")
    filter_line = (
        "lock_targets <- lock_targets["
        "lock_targets$outcome %in% names(outcomes), , drop = FALSE]\n"
    )
    return patched.replace(lock_needle, filter_line + lock_needle)


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def write_adjudication_summary(output_dir: Path) -> None:
    metrics = read_csv(output_dir / "model_comparison_summary.csv")
    fixed = read_csv(output_dir / "fixed_effect_posteriors.csv")

    expected = {
        "base_additive",
        "single_thermal_variability",
        "single_moisture_thermal_variability",
        "joint_thermal_and_moisture_variability",
        "predeclared_stress_variability_bundle",
        "joint_core_plus_precip_variability",
    }
    observed = {row["model_id"] for row in metrics}
    if observed != expected:
        raise RuntimeError(
            f"Model registry mismatch; expected {sorted(expected)}, "
            f"observed {sorted(observed)}"
        )

    interaction_terms = {
        "int_thermal_variability",
        "int_moisture_thermal_variability",
        "int_dryness_precip_variability",
    }
    retained = [
        row for row in fixed
        if row.get("term") in interaction_terms
    ]

    payload = {
        "status": "PASS",
        "outcome": "conditional_intensity",
        "models": len(metrics),
        "geographical_folds": 5,
        "cluster_bootstrap_reps": 4000,
        "frozen_additive_model_replaced": False,
        "interaction_rows": retained,
        "decision_question": (
            "Does the exhaustive-only precipitation-PC1 × "
            "temperature-seasonality term contribute independently of the "
            "predeclared Temperature-PC1 × temperature-seasonality term?"
        ),
    }
    (output_dir / "joint_adjudication_index.json").write_text(
        json.dumps(payload, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )


def main() -> int:
    args = parse_args()
    if not args.base_script.is_file():
        raise FileNotFoundError(args.base_script)
    if not args.input_csv.is_file():
        raise FileNotFoundError(args.input_csv)
    if args.bootstrap_reps < 1000:
        raise ValueError("--bootstrap-reps must be at least 1000")

    patched = patch_script(args.base_script.read_text(encoding="utf-8"))
    args.output_dir.mkdir(parents=True, exist_ok=True)

    with tempfile.NamedTemporaryFile(
        mode="w",
        suffix=".R",
        encoding="utf-8",
        delete=False,
    ) as handle:
        handle.write(patched)
        generated = Path(handle.name)

    try:
        subprocess.run(
            [
                "Rscript",
                str(generated),
                "--input-csv",
                str(args.input_csv),
                "--outcome",
                "conditional_intensity",
                "--output-dir",
                str(args.output_dir),
                "--bootstrap-reps",
                str(args.bootstrap_reps),
                "--seed",
                str(args.seed),
            ],
            check=True,
        )
        write_adjudication_summary(args.output_dir)
    finally:
        generated.unlink(missing_ok=True)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
