#!/usr/bin/env bash
set -euo pipefail

SUBMISSION_DRAWS="${HOTARUBUKURO_SUBMISSION_DRAWS:-10000}"
JOINT_LATENT_DRAWS="${HOTARUBUKURO_JOINT_LATENT_DRAWS:-10000}"
JOINT_OBS_REPS="${HOTARUBUKURO_JOINT_OBS_REPS:-20}"
mkdir -p reanalysis_status

for path in \
  reanalysis_inputs/upstream_data_manifest.json \
  results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv \
  results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv \
  results/ecological_v16_predictive_replication/checkpoints/national_environment_spde_presence_draws1000.rds \
  results/ecological_v16_predictive_replication/checkpoints/national_environment_spde_intensity_draws1000.rds \
  results/ecological_v25_submission_presence/checkpoints/national_environment_spde_presence_draws${SUBMISSION_DRAWS}.rds; do
  [[ -s "$path" ]] || { echo "Fresh completed prerequisite missing: $path" >&2; exit 1; }
done

run_stage() {
  local label="$1"
  shift
  echo "=== ${label} ==="
  "$@" 2>&1 | tee "reanalysis_status/${label}.log"
}

# Delete only outputs below the already-completed fresh v16 model. The fresh
# phenotype, v15 cells, v16 cross-fit and 10k presence checkpoint are retained.
rm -rf \
  results/ecological_v17_bombus_limitation_gate \
  results/ecological_v18_* \
  results/ecological_v19_human_landscape_extremes \
  results/ecological_v20_local_white_isolates \
  results/ecological_v21_local_human_neighbourhood \
  results/ecological_v22_did_human_context \
  results/ecological_v25_submission_isolate_null \
  results/ecological_v26_joint_submission_isolate_ppc \
  results/reanalysis_current_inputs \
  results/final_analysis_pipeline

# Stage 03 uses exactly the same exposure, thresholds, radius, environmental
# caliper, pair orientation and null. The current-input wrapper only adds a
# defined zero-pair output when the fresh SDM gives no support for the fixed gate.
run_stage 03_run_bombus_limitation_gate_current_inputs \
  Rscript scripts/run_bombus_limitation_gate_current_inputs.R

# Stage 04 and 05 are the unchanged active computational scripts.
run_stage 04_run_human_landscape_features \
  Rscript scripts/run_human_landscape_features.R

run_stage 04_define_local_pigmented_isolates \
  Rscript scripts/run_local_pigmented_isolates.R

run_stage S1_run_candidate_doy_check \
  Rscript scripts/run_candidate_doy_check.R

run_stage 05_run_local_human_context \
  Rscript scripts/run_local_human_context.R

run_stage 05_run_did_sensitivity \
  Rscript scripts/run_did_sensitivity.R

run_stage 05_refine_submission_isolate_null \
  Rscript scripts/refine_submission_isolate_null.R \
    --draws="$SUBMISSION_DRAWS" \
    --seed=20260725 \
    --presence-checkpoint="results/ecological_v25_submission_presence/checkpoints/national_environment_spde_presence_draws${SUBMISSION_DRAWS}.rds"

# Same joint full-data PPC sensitivity; no legacy candidate-count validator is
# used because historical count identities are not analysis invariants.
run_stage joint_submission_isolate_ppc \
  Rscript scripts/run_joint_submission_isolate_ppc.R \
    --latent-draws="$JOINT_LATENT_DRAWS" \
    --observation-replicates="$JOINT_OBS_REPS"

run_stage report_reanalysis \
  Rscript scripts/report_reanalysis_current_inputs.R \
    --output results/reanalysis_current_inputs

echo "=== fresh-input downstream analysis complete ==="
