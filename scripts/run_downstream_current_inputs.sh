#!/usr/bin/env bash
set -euo pipefail

ROOT="$(pwd)"
SUBMISSION_DRAWS="${HOTARUBUKURO_SUBMISSION_DRAWS:-10000}"
JOINT_LATENT_DRAWS="${HOTARUBUKURO_JOINT_LATENT_DRAWS:-10000}"
JOINT_OBS_REPS="${HOTARUBUKURO_JOINT_OBS_REPS:-20}"

# Never fall back to the repository's historical frozen phenotype boundary. This
# script is allowed to run only after the fresh 1,965-row upstream reconstruction
# has produced and copied its v11/v15 outputs into the active results tree.
for path in \
  reanalysis_inputs/upstream_data_manifest.json \
  reanalysis_status/upstream_run_phenotype_hurdle.log \
  reanalysis_status/upstream_run_multiscale_hotspots.log \
  results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv \
  results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv; do
  [[ -s "$path" ]] || { echo "Fresh upstream prerequisite missing: $path" >&2; exit 1; }
done
grep -q "Completed two-part phenotype model" reanalysis_status/upstream_run_phenotype_hurdle.log || {
  echo "Fresh phenotype reconstruction did not complete." >&2; exit 1;
}

# Static human-context inputs are intentionally unchanged by this reanalysis.
# Restore/export their canonical locations in the same shell that runs stages
# 04-05 so those stages never depend on a previous workflow step's environment.
BASE_INPUTS="${ROOT}/reproduction_inputs/base_snapshot/analysis_inputs"
if [[ ! -d "$BASE_INPUTS" ]]; then
  bash scripts/canonical_snapshot.sh restore \
    inputs/canonical_snapshot.json reproduction_inputs/base_snapshot
fi
export HOTARUBUKURO_INPUT_ROOT="$BASE_INPUTS"
export HOTARUBUKURO_MLIT_CACHE="${BASE_INPUTS}/mlit_l03_2021"
export HOTARUBUKURO_DID_CACHE="${BASE_INPUTS}/mlit_did_2015"
export HOTARUBUKURO_WORLDPOP_RASTER="${BASE_INPUTS}/rasters/population_count_Japan_crop.tif"
for path in "$HOTARUBUKURO_WORLDPOP_RASTER" "$HOTARUBUKURO_MLIT_CACHE" "$HOTARUBUKURO_DID_CACHE"; do
  [[ -e "$path" ]] || { echo "Static analysis input missing: $path" >&2; exit 1; }
done

run_stage() {
  local label="$1"
  shift
  echo "=== ${label} ==="
  "$@" 2>&1 | tee "reanalysis_status/${label}.log"
}

# Same computational stage order as the active publication pipeline.
run_stage 02_run_natural_predictive_model \
  Rscript scripts/run_natural_predictive_model.R

run_stage 02_run_submission_presence_checkpoint \
  Rscript scripts/run_natural_predictive_model.R \
    --components=national_environment_spde_presence \
    --draws="$SUBMISSION_DRAWS" \
    --seed=20260725 \
    --force=true \
    --output=results/ecological_v25_submission_presence

# The manuscript-facing Bombus analysis is intentionally separate from this broad/anomaly
# reconstruction and is evaluated only at sharp local colour boundaries.

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

run_stage joint_submission_isolate_ppc \
  Rscript scripts/run_joint_submission_isolate_ppc.R \
    --latent-draws="$JOINT_LATENT_DRAWS" \
    --observation-replicates="$JOINT_OBS_REPS"

# Historical submission/final-lock validators encode old result identities
# (including an estimable stage-03 gate and old candidate counts). They are not
# acceptance criteria for a current-input reconstruction. The dynamic report
# instead checks the fresh candidate identity and joint-PPC boundary directly.
run_stage report_reanalysis \
  Rscript scripts/report_reanalysis_current_inputs.R \
    --output results/reanalysis_current_inputs

echo "=== unchanged current-input downstream computation complete ==="
