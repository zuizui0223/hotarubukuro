#!/usr/bin/env python3
"""One-time non-workflow cleanup for the current JBI paper branch.

Workflow files are handled separately through the GitHub connector because the
Actions token is not allowed to modify .github/workflows/.
"""
from __future__ import annotations

import subprocess
from pathlib import Path

ROOT = Path.cwd()


def run(*args: str) -> None:
    subprocess.run(args, check=True)


def mv(src: str, dst: str) -> None:
    s = ROOT / src
    if not s.exists():
        raise SystemExit(f"missing move source: {src}")
    (ROOT / dst).parent.mkdir(parents=True, exist_ok=True)
    run("git", "mv", src, dst)


for d in [
    "legacy/method-development/scripts",
    "legacy/method-development/R",
    "legacy/method-development/validation",
    "legacy/method-development/tests",
    "legacy/reproducibility-development/docs",
    "legacy/reproducibility-development/inputs",
    "legacy/reproducibility-development/config",
]:
    (ROOT / d).mkdir(parents=True, exist_ok=True)

# Previous manuscript / reporting architectures.
mv("manuscript", "legacy/manuscript-development")
mv("reports", "legacy/reporting")

# Superseded scripts and estimands.
for f in [
    "run_analysis_1909.sh",
    "run_bombus_limitation_gate.R",
    "run_bombus_limitation_gate_current_inputs.R",
    "run_bombus_relaxation_local_contrast.R",
    "resume_reanalysis_from_fresh_v16.sh",
    "report_analysis_arc.R",
    "run_publication_pipeline.R",
    "build_publication_figures.R",
    "run_local_bombus_turnover.R",
]:
    mv(f"scripts/{f}", f"legacy/method-development/scripts/{f}")
mv("scripts/internal", "legacy/method-development/scripts/internal")

for f in ["local_bombus_turnover.R", "final_registry.R"]:
    mv(f"R/{f}", f"legacy/method-development/R/{f}")

for f in [
    "audit_bombus_limitation_gate.R",
    "validate_bombus_limitation_gate.R",
    "audit_local_bombus_turnover.R",
    "validate_local_bombus_turnover.R",
    "audit_publication_claims.R",
    "validate_publication_pipeline.R",
    "validate_submission_analysis_lock.R",
    "validate_submission_isolate_null.R",
]:
    mv(f"validation/{f}", f"legacy/method-development/validation/{f}")

for f in ["test-local-bombus-turnover.R", "test-publication-pipeline.R"]:
    mv(f"tests/testthat/{f}", f"legacy/method-development/tests/{f}")

for f in ["analysis-1909.md", "bombus-limitation-gate-results.md", "pipeline-dag.md"]:
    mv(f"docs/{f}", f"legacy/reproducibility-development/docs/{f}")
mv("inputs/analysis_1909_expectations.csv", "legacy/reproducibility-development/inputs/analysis_1909_expectations.csv")
mv("config/code_manifest.csv", "legacy/reproducibility-development/config/code_manifest.csv")

for f in [
    "bombus_relaxation_analysis_spec_2026-08-08.md",
    "bombus_relaxation_results_2026-08-08.md",
    "phenology_removal_candidate_identity.md",
    "pipeline_stage_registry.csv",
]:
    mv(f"reproducibility/{f}", f"legacy/reproducibility-development/{f}")
mv("reproducibility/patches", "legacy/reproducibility-development/patches")

# The upstream current-input driver now stops after producing the fresh v11/v15 boundary.
p = ROOT / "scripts/run_reanalysis_current_inputs.sh"
text = p.read_text(encoding="utf-8")
marker = """# ---------------------------------------------------------------------------
# 6. Replace only the active pipeline's upstream boundary and run the same
#    downstream stages in reconstruction mode. Historical n identity checks are
#    explicitly not applicable; structural/scientific validations still run.
# ---------------------------------------------------------------------------
"""
if marker not in text:
    raise SystemExit("section 6 marker missing in run_reanalysis_current_inputs.sh")
prefix = text.split(marker, 1)[0]
clean_tail = """# ---------------------------------------------------------------------------
# 6. Publish only the freshly rebuilt broad-analysis boundary.
#    Downstream anomaly analyses are run once by run_downstream_current_inputs.sh;
#    the manuscript-facing Bombus test is a separate local-transition pipeline.
# ---------------------------------------------------------------------------
for stage in \\
  environment_v3 \\
  ecological_v9_final_public_HRNA_50km \\
  ecological_v10_final_mechanism_HRNA \\
  ecological_v11_pigmentation_hurdle \\
  ecological_v15_multiscale_hotspots; do
  rm -rf "results/${stage}"
  cp -a "${UPSTREAM_DIR}/results/${stage}" results/
done

echo "=== fresh current-input broad-analysis boundary complete ==="
"""
p.write_text(prefix + clean_tail, encoding="utf-8")

# Remove obsolete all-five limitation-gate stage from current broad/anomaly downstream work.
p = ROOT / "scripts/run_downstream_current_inputs.sh"
text = p.read_text(encoding="utf-8")
start = text.find("# Same stage-03 analysis definition.")
end = text.find("run_stage 04_run_human_landscape_features", start)
if start < 0 or end < 0:
    raise SystemExit("obsolete downstream stage-03 block not found")
text = text[:start] + (
    "# The manuscript-facing Bombus analysis is intentionally separate from this broad/anomaly\n"
    "# reconstruction and is evaluated only at sharp local colour boundaries.\n\n"
) + text[end:]
p.write_text(text, encoding="utf-8")

# Remove obsolete limitation-gate/final-lock reporting from the fresh summary.
p = ROOT / "scripts/report_reanalysis_current_inputs.R"
text = p.read_text(encoding="utf-8")
for block in [
    '''gate <- read_csv("results/ecological_v17_bombus_limitation_gate/bombus_limitation_gate_summary.csv")
gate_support <- read_csv(
  "results/ecological_v17_bombus_limitation_gate/bombus_limitation_gate_support_diagnostics.csv",
  required = FALSE
)
gate_interpretation <- read_csv(
  "results/ecological_v17_bombus_limitation_gate/interpretation_summary.csv",
  required = FALSE
)
''',
    '''final_results <- read_csv("results/final_analysis_pipeline/final_result_registry.csv", required = FALSE)
final_claims <- read_csv("results/final_analysis_pipeline/final_claim_registry.csv", required = FALSE)
''',
]:
    if block not in text:
        raise SystemExit("expected obsolete report input block not found")
    text = text.replace(block, "")

s = text.find('if (all(c("is_primary_gate", "response") %in% names(gate))) {')
e = text.find('add("local_isolates", "candidate_count"', s)
if s < 0 or e < 0:
    raise SystemExit("gate report section not found")
text = text[:s] + text[e:]
for line in [
    'write_csv(gate, "bombus_limitation_gate_summary.csv")\n',
    'write_csv(gate_support, "bombus_limitation_gate_support_diagnostics.csv")\n',
    'write_csv(final_results, "final_result_registry.csv")\n',
    'write_csv(final_claims, "final_claim_registry.csv")\n',
]:
    text = text.replace(line, "")
s = text.find("primary_gate <- gate[gate$is_primary_gate")
e = text.find("markdown <- c(", s)
if s < 0 or e < 0:
    raise SystemExit("gate-line block not found")
text = text[:s] + text[e:]
text = text.replace("  gate_line,\n", "")
old_lines = '''  "Scientific stage order, response definitions, natural model, fixed Bombus gate grid,",
  "local-isolate definition, natural-null procedure, and human-context analyses were retained.",
  "Only the upstream flower/environment reconstruction and Bombus source build were replaced.",
  "A fixed gate that has no fresh-SDM support is reported as not estimable rather than retuned."
'''
new_lines = '''  "The report summarizes the fresh broad natural template and event-based anomaly/human-context stages.",
  "The focal Bombus availability test is intentionally run in a separate local-transition pipeline."
'''
if old_lines not in text:
    raise SystemExit("old report README wording not found")
text = text.replace(old_lines, new_lines)
p.write_text(text, encoding="utf-8")

# Short manuscript-facing script guide.
(ROOT / "scripts/README.md").write_text(
    """# Current manuscript-facing scripts

Start with [`../paper/README.md`](../paper/README.md). This directory contains current reusable infrastructure plus scripts feeding the active broad/anomaly or local-pollinator analyses. Superseded runners and estimands are under `legacy/method-development/`.

## Broad natural template + event-based departures

- `run_reanalysis_current_inputs.sh` — rebuild the fresh 1,965-row upstream environment/phenotype boundary only.
- `run_downstream_current_inputs.sh` — natural predictive reference, event-based departure calibration and post-selection human context.
- `report_reanalysis_current_inputs.R` — fresh 1,922-observation manuscript summary.
- `run_natural_predictive_model.R`
- `run_local_pigmented_isolates.R`
- `run_joint_submission_isolate_ppc.R`
- `run_human_landscape_features.R`
- `run_local_human_context.R`
- `run_did_sensitivity.R`
- `run_candidate_doy_check.R`

## Local pollinator analysis

- `run_bombus_effective_availability_refined.R` — occurrence-reference calibration for focal broad pollinators.
- `run_bombus_local_sharp_transition.R` — manuscript-facing 5-km white-pigmented boundary test.
- `run_bombus_spatial_replication_test.R` — Supporting Information community-boundary and montane/elevation guardrails.

## Infrastructure

Snapshot/environment/setup helpers remain outside `legacy/` only when used by a current workflow.
""",
    encoding="utf-8",
)

(ROOT / "legacy/MOVED_2026-08-09.md").write_text(
    """# Repository cleanup inventory — 2026-08-09

This cleanup separates the current JBI paper from method-development history.

- former `manuscript/` -> `legacy/manuscript-development/`
- former `reports/` -> `legacy/reporting/`
- 1,909 runner / old publication orchestrator -> `legacy/method-development/`
- old all-five Bombus limitation gate -> `legacy/method-development/`
- relaxation/local-contrast and old local-turnover variants -> `legacy/method-development/`
- old E&E figure builder and final-lock code -> `legacy/method-development/`
- old code manifest, stage registry, 1,909 expectations, relaxation specs and one-time patches -> `legacy/reproducibility-development/`

Current manuscript evidence is listed in `paper/active-file-map.csv`. Historical workflow definitions are archived separately under `legacy/workflows/` by the repository cleanup commit sequence.
""",
    encoding="utf-8",
)

# Self-delete only this temporary script. Workflow cleanup is performed through the connector.
Path(__file__).unlink()
print("Non-workflow repository cleanup staged successfully.")
