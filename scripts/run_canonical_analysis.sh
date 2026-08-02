#!/usr/bin/env bash
#
# Canonical analysis reproduction.
#
# Starts from the immutable, checksummed analysis-input snapshot and regenerates
# the 1-km cell table, the 1,000 cross-fitted natural predictive maps, the
# bidirectional local colour-state asymmetry outputs, and the validation report.
#
# The stage checkpoints make a rerun resumable, and CLEAN_RUN=true forces a
# completely fresh reconstruction from the snapshot.

set -euo pipefail

SNAPSHOT_DIR="${SNAPSHOT_DIR:-reproduction_inputs/snapshot}"
STATUS_DIR="${STATUS_DIR:-reproduction_status}"
REPORT_DIR="${REPORT_DIR:-reproducibility}"
REQUIRED_MAPS="${REQUIRED_MAPS:-1000}"
PREDICTIVE_SEED="${PREDICTIVE_SEED:-20260725}"
PSEUDOCOUNT="${PSEUDOCOUNT:-0.5}"
CLEAN_RUN="${CLEAN_RUN:-false}"

CELLS_DIR="results/ecological_v15_multiscale_hotspots"
PREDICTIVE_DIR="results/ecological_v16_predictive_replication"
ASYMMETRY_DIR="results/ecological_v23_local_state_asymmetry"

CELLS="${CELLS_DIR}/multiscale_hotspot_cells_1km.csv"
PRESENCE_CHECKPOINT="${PREDICTIVE_DIR}/checkpoints/national_environment_spde_presence_draws${REQUIRED_MAPS}.rds"

OBSERVATIONS="${SNAPSHOT_DIR}/analysis_inputs/analysis_data_pigmentation_hurdle.csv"
RASTERS="${SNAPSHOT_DIR}/analysis_inputs/rasters"
WORLDPOP="${RASTERS}/population_count_Japan_crop.tif"

mkdir -p "$STATUS_DIR" "$REPORT_DIR"

run_logged() {
  local label="$1"
  shift
  echo "=== ${label} ==="
  "$@" 2>&1 | tee "${STATUS_DIR}/${label}.log"
}

if [[ "$CLEAN_RUN" == "true" ]]; then
  echo "CLEAN_RUN=true: discarding stage checkpoints before reconstruction"
  rm -rf "$CELLS_DIR" "$PREDICTIVE_DIR" "$ASYMMETRY_DIR"
fi

# --- Snapshot ---------------------------------------------------------------
run_logged verify_canonical_snapshot \
  Rscript scripts/verify_canonical_snapshot.R \
    --descriptor inputs/canonical_snapshot.json \
    --root "$SNAPSHOT_DIR" \
    --report-dir "$REPORT_DIR"

# --- Preflight --------------------------------------------------------------
run_logged preflight \
  Rscript scripts/preflight.R \
    --scope canonical \
    --report-dir "$REPORT_DIR" \
    --inputs "${OBSERVATIONS},${RASTERS}/elevation_Japan_crop.tif,${RASTERS}/bio10_Japan_crop_30s.tif,${RASTERS}/bio12_Japan_crop_30s.tif,${RASTERS}/RSDS_Japan_crop_30s.tif,${WORLDPOP}"

# --- 1-km cell table --------------------------------------------------------
if [[ -s "$CELLS" ]]; then
  echo "=== resuming from existing 1-km cell table: ${CELLS} ==="
else
  run_logged run_multiscale_hotspots \
    Rscript scripts/run_multiscale_hotspots.R \
      --input="$OBSERVATIONS" \
      --output-dir="$CELLS_DIR" \
      --environment-cache="$RASTERS" \
      --worldpop-raster="$WORLDPOP" \
      --bootstrap=1000
fi
test -s "$CELLS"

# --- 1,000 cross-fitted natural predictive maps -----------------------------
if [[ -s "$PRESENCE_CHECKPOINT" ]]; then
  echo "=== resuming from existing presence checkpoint: ${PRESENCE_CHECKPOINT} ==="
else
  run_logged run_natural_predictive_model \
    Rscript scripts/run_natural_predictive_model.R \
      --observations="$OBSERVATIONS" \
      --cells="$CELLS" \
      --output="$PREDICTIVE_DIR" \
      --components=national_environment_spde_presence \
      --draws="$REQUIRED_MAPS" \
      --seed="$PREDICTIVE_SEED"
fi
test -s "$PRESENCE_CHECKPOINT"

# --- Bidirectional asymmetry diagnostic -------------------------------------
run_logged run_local_state_asymmetry \
  Rscript scripts/run_local_state_asymmetry.R \
    --cells="$CELLS" \
    --presence-checkpoint="$PRESENCE_CHECKPOINT" \
    --required-maps="$REQUIRED_MAPS" \
    --pseudocount="$PSEUDOCOUNT" \
    --seed="$PREDICTIVE_SEED" \
    --output="$ASYMMETRY_DIR"

run_logged validate_local_state_asymmetry \
  Rscript validation/validate_local_state_asymmetry.R \
    "$ASYMMETRY_DIR" "--required-maps=${REQUIRED_MAPS}"

# --- Reproducibility record -------------------------------------------------
run_logged write_reproducibility_report \
  Rscript scripts/write_reproducibility_report.R \
    --report-dir "$REPORT_DIR" \
    --workflow canonical-analysis \
    --inputs "${SNAPSHOT_DIR}/SNAPSHOT_MANIFEST.csv,${OBSERVATIONS},${RASTERS},inputs/canonical_snapshot.json,Data_S1.csv" \
    --outputs "${CELLS},${PRESENCE_CHECKPOINT},${ASYMMETRY_DIR},${PREDICTIVE_DIR}/predictive_replication_component_scope.csv"

# --- Headline numbers into the job log and the step summary -----------------
Rscript - <<'RS' | tee "${STATUS_DIR}/asymmetry_primary_results.txt"
directory <- "results/ecological_v23_local_state_asymmetry"
summary <- utils::read.csv(
  file.path(directory, "local_state_asymmetry_summary.csv"), check.names = FALSE
)
columns <- intersect(c(
  "state_rule", "metric", "observed_value", "null_mean", "null_sd",
  "lower_95", "upper_95", "upper_p", "lower_p", "two_sided_p",
  "percentile", "n_natural_maps"
), names(summary))
wanted <- summary$metric %in% c(
  "pigmented_in_white_count", "white_in_pigmented_count",
  "pigmented_in_white_rate", "white_in_pigmented_rate", "log_rate_ratio"
)
print(summary[wanted, columns, drop = FALSE], row.names = FALSE)

null <- utils::read.csv(
  file.path(directory, "local_state_asymmetry_null.csv"), check.names = FALSE
)
opportunity <- do.call(rbind, lapply(split(null, null$state_rule), function(block) {
  data.frame(
    state_rule = block$state_rule[[1L]],
    mean_eligible_pigmented_focals = mean(block$eligible_pigmented_focals),
    mean_eligible_white_focals = mean(block$eligible_white_focals),
    stringsAsFactors = FALSE
  )
}))
cat("\nNull opportunity denominators\n")
print(opportunity, row.names = FALSE)
RS

echo "=== canonical analysis complete ==="
find results/ecological_v23_local_state_asymmetry -type f -printf '%p\t%s\n' | sort

# Print the manifests as well as recording them. Two clean runs of this workflow
# should produce identical output hashes, and that is only checkable if the
# hashes are visible in the run log rather than only inside the artifact.
echo "=== input manifest ==="
cat "${REPORT_DIR}/input_manifest.csv"
echo "=== output manifest ==="
cat "${REPORT_DIR}/output_manifest.csv"
