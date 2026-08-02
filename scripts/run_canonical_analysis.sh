#!/usr/bin/env bash
#
# Canonical analysis reproduction.
#
# Starts from the verified, materialised analysis-input snapshot and runs the
# established publication pipeline exactly as scripts/run_publication_pipeline.R
# defines it, then builds the manuscript-facing figures and writes the
# provenance record.
#
# This driver adds no analysis of its own. Stage order, model definitions,
# fold structure, draw counts and thresholds all come from the locked runner.

set -euo pipefail

SNAPSHOT_DIR="${SNAPSHOT_DIR:-reproduction_inputs/snapshot}"
STATUS_DIR="${STATUS_DIR:-reproduction_status}"
REPORT_DIR="${REPORT_DIR:-reproducibility}"
PIPELINE_MODE="${PIPELINE_MODE:-full}"
BUILD_FIGURES="${BUILD_FIGURES:-true}"
LOCK_DIR="results/final_analysis_pipeline"

export HOTARUBUKURO_MLIT_CACHE="${HOTARUBUKURO_MLIT_CACHE:-reproduction_inputs/mlit_l03_2021}"
export HOTARUBUKURO_DID_CACHE="${HOTARUBUKURO_DID_CACHE:-reproduction_inputs/mlit_did_2015}"
export HOTARUBUKURO_WORLDPOP_RASTER="${HOTARUBUKURO_WORLDPOP_RASTER:-${SNAPSHOT_DIR}/analysis_inputs/rasters/population_count_Japan_crop.tif}"
export HOTARUBUKURO_INPUT_ROOT="${HOTARUBUKURO_INPUT_ROOT:-${SNAPSHOT_DIR}/analysis_inputs/rasters}"

mkdir -p "$STATUS_DIR" "$REPORT_DIR"

run_logged() {
  local label="$1"
  shift
  echo "=== ${label} ==="
  "$@" 2>&1 | tee "${STATUS_DIR}/${label}.log"
}

# --- Snapshot: verify checksums, then place inputs where the runner looks ----
run_logged verify_canonical_snapshot \
  Rscript scripts/verify_canonical_snapshot.R \
    --descriptor inputs/canonical_snapshot.json \
    --root "$SNAPSHOT_DIR" \
    --report-dir "$REPORT_DIR" \
    --materialise true \
    --mlit-cache "$HOTARUBUKURO_MLIT_CACHE" \
    --did-cache "$HOTARUBUKURO_DID_CACHE"

# --- Established-input fidelity ----------------------------------------------
# Reported before anything is fitted, so a run against inputs that are not the
# published ones says so in its first minute instead of looking like a crash.
run_logged check_input_fidelity \
  Rscript scripts/check_input_fidelity.R \
    --expectations inputs/established_input_expectations.csv \
    --report-dir "$REPORT_DIR" \
    --strict false

# --- Preflight ---------------------------------------------------------------
run_logged preflight \
  Rscript scripts/preflight.R \
    --scope canonical \
    --report-dir "$REPORT_DIR" \
    --inputs "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv,results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv,results/public_rasters/mlit_human_forest_edge_2021/mlit_human_forest_edge_1km.tif,${HOTARUBUKURO_WORLDPOP_RASTER},Data_S1.csv"

# --- The established publication pipeline ------------------------------------
# scripts/run_publication_pipeline.R is the authoritative stage sequence. It
# writes results/final_analysis_pipeline/final_stage_manifest.csv as it goes, so
# a failure is attributable to a named stage even when this driver aborts.
run_logged run_publication_pipeline \
  Rscript scripts/run_publication_pipeline.R \
    --mode="$PIPELINE_MODE" \
    --tests=true \
    --output="$LOCK_DIR"

# --- Manuscript-facing figures ----------------------------------------------
if [[ "$BUILD_FIGURES" == "true" ]]; then
  run_logged build_publication_figures \
    Rscript scripts/build_publication_figures.R
fi

# --- Numerical regression ----------------------------------------------------
# Written non-strictly here so that the report, the manifests and the logs all
# exist before anything aborts. The validation summary below is what turns a
# regression failure into a failed run.
run_logged check_numerical_regression \
  Rscript scripts/check_numerical_regression.R \
    --reference inputs/numerical_reference.csv \
    --registry "${LOCK_DIR}/final_result_registry.csv" \
    --report-dir "$REPORT_DIR" \
    --strict false

# --- Reproducibility record --------------------------------------------------
run_logged write_reproducibility_report \
  Rscript scripts/write_reproducibility_report.R \
    --report-dir "$REPORT_DIR" \
    --workflow canonical-analysis \
    --inputs "${SNAPSHOT_DIR}/SNAPSHOT_MANIFEST.csv,inputs/canonical_snapshot.json,Data_S1.csv,results/ecological_v11_pigmentation_hurdle,results/ecological_v15_multiscale_hotspots" \
    --outputs "${LOCK_DIR},results/ecological_v16_predictive_replication,results/ecological_v17_local_pair_turnover,results/ecological_v19_human_landscape_extremes,results/ecological_v20_local_white_isolates,results/ecological_v21_local_human_neighbourhood,results/ecological_v22_did_human_context,manuscript/figures"

# --- Validation summary ------------------------------------------------------
Rscript - <<'RS' | tee "${STATUS_DIR}/validation_summary.txt"
source("R/reproducibility.R")
lock <- "results/final_analysis_pipeline"

read_optional <- function(path) {
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

sections <- list(
  structural = read_optional(file.path(lock, "final_stage_manifest.csv")),
  provenance = read_optional(file.path(lock, "final_input_checksums.csv")),
  exact = read_optional(file.path(lock, "final_independent_validation.csv")),
  claim = read_optional(file.path(lock, "final_claim_audit.csv")),
  numerical = read_optional("reproducibility/numerical_regression_report.csv")
)

rows <- list()
for (name in names(sections)) {
  table <- sections[[name]]
  if (is.null(table)) {
    rows[[length(rows) + 1L]] <- data.frame(
      check_class = name, checks = 0L, passed = 0L, failed = 0L,
      status = "ABSENT", stringsAsFactors = FALSE
    )
    next
  }
  status_column <- intersect(c("status", "result"), names(table))
  if (!length(status_column)) {
    rows[[length(rows) + 1L]] <- data.frame(
      check_class = name, checks = nrow(table), passed = NA_integer_,
      failed = NA_integer_, status = "RECORDED", stringsAsFactors = FALSE
    )
    next
  }
  values <- table[[status_column[[1L]]]]
  # The numerical report carries two verdicts per row: whether the value stayed
  # inside its tolerance, and whether the qualitative invariant still holds.
  # Both have to count, so neither can fail quietly.
  if ("invariant_status" %in% names(table)) {
    invariants <- table$invariant_status
    values <- c(values, invariants[invariants != "NONE"])
  }
  passed <- sum(values == "PASS")
  rows[[length(rows) + 1L]] <- data.frame(
    check_class = name, checks = length(values), passed = passed,
    failed = length(values) - passed,
    status = if (passed == length(values)) "PASS" else "FAIL",
    stringsAsFactors = FALSE
  )
}
summary <- do.call(rbind, rows)
print(summary, row.names = FALSE)
rp_write_csv_atomic(summary, "reproducibility/validation_summary.csv")
if (any(summary$status == "FAIL")) {
  stop("Canonical validation reported failures.", call. = FALSE)
}
RS

echo "=== canonical analysis complete ==="
echo "=== input manifest ==="
cat "${REPORT_DIR}/canonical_input_manifest.csv" | head -30
echo "=== output manifest ==="
cat "${REPORT_DIR}/output_manifest.csv"
