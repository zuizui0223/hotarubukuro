#!/usr/bin/env bash
set -euo pipefail

SUBMISSION_DRAWS="${HOTARUBUKURO_SUBMISSION_DRAWS:-10000}"
JOINT_LATENT_DRAWS="${HOTARUBUKURO_JOINT_LATENT_DRAWS:-10000}"
JOINT_OBS_REPS="${HOTARUBUKURO_JOINT_OBS_REPS:-20}"

run_stage() {
  local label="$1"
  shift
  echo "=== ${label} ==="
  "$@" 2>&1 | tee "reanalysis_status/${label}.log"
}

# Same computational stage order as scripts/run_publication_pipeline.R.
run_stage 02_run_natural_predictive_model \
  Rscript scripts/run_natural_predictive_model.R

run_stage 02_run_submission_presence_checkpoint \
  Rscript scripts/run_natural_predictive_model.R \
    --components=national_environment_spde_presence \
    --draws="$SUBMISSION_DRAWS" \
    --seed=20260725 \
    --force=true \
    --output=results/ecological_v25_submission_presence

run_stage 03_run_bombus_limitation_gate \
  Rscript scripts/run_bombus_limitation_gate.R

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

run_stage 06_write_publication_lock \
  Rscript -e 'source("R/pipeline_support.R"); hb_load_modules("final_registry"); final_write_lock(".", "results/final_analysis_pipeline")'

run_stage joint_submission_isolate_ppc \
  Rscript scripts/run_joint_submission_isolate_ppc.R \
    --latent-draws="$JOINT_LATENT_DRAWS" \
    --observation-replicates="$JOINT_OBS_REPS"

run_stage build_publication_figures \
  Rscript scripts/build_publication_figures.R

run_stage report_reanalysis \
  Rscript scripts/report_reanalysis_current_inputs.R \
    --output results/reanalysis_current_inputs

echo "=== unchanged downstream computation complete ==="
