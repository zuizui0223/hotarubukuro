#!/usr/bin/env bash
#
# Build the PUBLIC RECONSTRUCTION. This is not the paper's analysis.
#
# The published manuscript analysis is canonical, on 1,923 observations. This
# driver runs the same locked pipeline on the population rebuilt from public
# sources, which has 1,909, and then describes the two side by side. It issues
# no robustness verdict: the two are different observation sets by
# construction, because the published analysis-input tables are lost.
#
# It adds no model, formula, fold, threshold or draw count of its own. Stage
# order and model definitions all come from the locked runner. Every stage must
# pass in sequence.

set -euo pipefail

SNAPSHOT_DIR="${SNAPSHOT_DIR:-reproduction_inputs/snapshot}"
STATUS_DIR="${STATUS_DIR:-reproduction_status}"
REPORT_DIR="${REPORT_DIR:-reproducibility}"
BUILD_FIGURES="${BUILD_FIGURES:-true}"
LOCK_DIR="results/final_analysis_pipeline"


export HOTARUBUKURO_MLIT_CACHE="${HOTARUBUKURO_MLIT_CACHE:-reproduction_inputs/mlit_l03_2021}"
export HOTARUBUKURO_DID_CACHE="${HOTARUBUKURO_DID_CACHE:-reproduction_inputs/mlit_did_2015}"
export HOTARUBUKURO_WORLDPOP_RASTER="${HOTARUBUKURO_WORLDPOP_RASTER:-${SNAPSHOT_DIR}/analysis_inputs/rasters/population_count_Japan_crop.tif}"
export HOTARUBUKURO_INPUT_ROOT="${HOTARUBUKURO_INPUT_ROOT:-${SNAPSHOT_DIR}/analysis_inputs/rasters}"
export HOTARUBUKURO_RUN_STARTED="${HOTARUBUKURO_RUN_STARTED:-$(date +%s)}"

mkdir -p "$STATUS_DIR" "$REPORT_DIR"

run_logged() {
  local label="$1"
  shift
  echo "=== ${label} ==="
  "$@" 2>&1 | tee "${STATUS_DIR}/${label}.log"
}

# Print a compact, analysis-neutral signature on both success and failure. The
# purpose is to separate three possibilities on repeated runs of one commit:
# different INLA checkpoints, identical checkpoints with different downstream
# artifacts, or bit-level differences that leave candidates and reported
# statistics unchanged. This changes no model input or analysis output.
print_reconstruction_signature() (
  set +e
  local v16="results/ecological_v16_predictive_replication"
  local v20="results/ecological_v20_local_white_isolates"

  echo "=== reconstruction run signature ==="
  echo "commit=${GITHUB_SHA:-unknown}"
  echo "run_id=${GITHUB_RUN_ID:-unknown}"

  echo "--- SHA256: predictive checkpoints and v20 artifacts ---"
  find \
    "${v16}/checkpoints" \
    "${v20}" \
    -maxdepth 1 -type f \
    \( -name '*.rds' -o -name 'local_isolate_candidates.csv' \
       -o -name 'local_isolate_natural_null.csv' \
       -o -name 'local_isolate_natural_null_summary.csv' \
       -o -name 'local_isolate_observed_pairs.csv' \
       -o -name 'local_isolate_independent_validation.csv' \) \
    -print0 2>/dev/null | sort -z | xargs -0 -r sha256sum

  echo "--- v20 validation table ---"
  cat "${v20}/local_isolate_independent_validation.csv" 2>/dev/null \
    || echo "not produced"

  echo "--- candidate and primary-null signature ---"
  Rscript - <<'RS'
read_optional <- function(path) {
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}
v20 <- "results/ecological_v20_local_white_isolates"
candidates <- read_optional(file.path(v20, "local_isolate_candidates.csv"))
summary <- read_optional(file.path(v20, "local_isolate_natural_null_summary.csv"))
registry <- read_optional(
  "results/final_analysis_pipeline/final_result_registry.csv"
)
if (is.null(candidates)) {
  cat("candidate_table=not_produced\n")
} else {
  ids <- sort(as.character(candidates$exact_site_id))
  cat("candidate_count=", length(ids), "\n", sep = "")
  cat("candidate_ids=", paste(ids, collapse = "|"), "\n", sep = "")
}
if (is.null(summary)) {
  cat("primary_null_summary=not_produced\n")
} else {
  rows <- summary[
    summary$configuration == "primary_10km_env1_all_white" &
      summary$metric %in% c("candidate_count", "candidate_fraction"),
    , drop = FALSE
  ]
  if (nrow(rows)) {
    print(rows, row.names = FALSE)
  } else {
    cat("primary_null_rows=absent\n")
  }
}
if (!is.null(registry)) {
  rows <- registry[
    registry$result_id %in% c("local_isolate_count", "local_isolate_fraction"),
    , drop = FALSE
  ]
  if (nrow(rows)) {
    cat("--- final registry isolate rows ---\n")
    print(rows, row.names = FALSE)
  }
}
RS
)

# --- Snapshot: verify checksums, then place inputs where the runner looks ----
run_logged verify_canonical_snapshot \
  Rscript scripts/verify_canonical_snapshot.R \
    --descriptor inputs/canonical_snapshot.json \
    --root "$SNAPSHOT_DIR" \
    --report-dir "$REPORT_DIR" \
    --materialise true \
    --mlit-cache "$HOTARUBUKURO_MLIT_CACHE" \
    --did-cache "$HOTARUBUKURO_DID_CACHE"

# --- Establish what the analysis population is -------------------------------
# Reported, not enforced. The fidelity check is expected to differ from the
# published counts here; recording it keeps the difference in the run's own
# provenance instead of only in prose.
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

# --- The complete pipeline, plus the discordance diagnostic ------------------
pipeline_status=0
run_logged run_pipeline_on_reconstruction \
  Rscript scripts/run_publication_pipeline.R \
    --mode=full \
    --tests=true \
    --baseline=reconstruction \
    --output="$LOCK_DIR" || pipeline_status=$?
echo "pipeline_status=${pipeline_status}" | tee "${STATUS_DIR}/pipeline_status.txt"

# --- Manuscript-facing figures ----------------------------------------------
figures_status=0
if [[ "$BUILD_FIGURES" == "true" && "$pipeline_status" -eq 0 ]]; then
  run_logged build_publication_figures \
    Rscript scripts/build_publication_figures.R || figures_status=$?
elif [[ "$BUILD_FIGURES" == "true" ]]; then
  echo "=== build_publication_figures skipped: the pipeline did not complete ==="
fi

# --- The robustness comparison -----------------------------------------------
# The point of the whole workflow. Run even when a later stage failed, so a
# partial run still reports whatever it did establish.
comparison_status=0
run_logged compare_reconstruction_to_published \
  Rscript scripts/compare_reconstruction_to_published.R \
    --published inputs/published_reference \
    --lock "$LOCK_DIR" \
    --report-dir "$REPORT_DIR" || comparison_status=$?

# --- Reproducibility record --------------------------------------------------
report_status=0
run_logged write_reproducibility_report \
  Rscript scripts/write_reproducibility_report.R \
    --report-dir "$REPORT_DIR" \
    --workflow reconstruction-analysis \
    --inputs "${SNAPSHOT_DIR}/SNAPSHOT_MANIFEST.csv,inputs/canonical_snapshot.json,inputs/published_reference,Data_S1.csv" \
    --outputs "${LOCK_DIR},results/ecological_v23_local_state_asymmetry,results/ecological_v24_candidate_doy_check,results/ecological_v16_predictive_replication,results/ecological_v17_local_pair_turnover,results/ecological_v19_human_landscape_extremes,results/ecological_v20_local_white_isolates,results/ecological_v21_local_human_neighbourhood,results/ecological_v22_did_human_context,manuscript/figures" \
  || report_status=$?

echo "=== reproducibility outputs ==="
ls -1 "$REPORT_DIR" || true
echo "=== robustness comparison ==="
cat "${REPORT_DIR}/reconstruction_vs_published.md" || true

# Always leave the scientific and bit-level signature at the end of the job log.
print_reconstruction_signature

# --- Outcome -----------------------------------------------------------------
if [[ "$pipeline_status" -ne 0 ]]; then
  # Printed last, and printed here rather than only where it happened, because
  # the comparison and the manifests above are long enough to push the original
  # stage dump out of a workflow log tail.
  echo "=== reconstruction analysis FAILED: the pipeline stopped ===" >&2
  if [[ -s "${LOCK_DIR}/final_stage_manifest.csv" ]]; then
    echo "--- stages attempted ---" >&2
    cut -d, -f1,4 "${LOCK_DIR}/final_stage_manifest.csv" >&2
    # Every failing stage, not just the last. In survey mode there is more than
    # one, and the point of that mode is to see all of them in a single run.
    failed_stages="$(awk -F'","' 'NR > 1 && $4 == "FAIL" { gsub(/"/, "", $1); print $1 }' \
      "${LOCK_DIR}/final_stage_manifest.csv")"
    echo "--- stages that failed: $(echo "$failed_stages" | tr '\n' ' ') ---" >&2
    for failed_stage in $failed_stages; do
      for stream in stderr stdout; do
        log="${LOCK_DIR}/logs/${failed_stage}_${stream}.log"
        if [[ -s "$log" ]]; then
          echo "--- ${failed_stage} ${stream} (complete log) ---" >&2
          cat "$log" >&2
        fi
      done
    done
  fi
  exit "$pipeline_status"
fi
if [[ "$figures_status" -ne 0 ]]; then
  echo "=== reconstruction analysis FAILED: figure generation ===" >&2
  exit "$figures_status"
fi
if [[ "$comparison_status" -ne 0 ]]; then
  echo "=== reconstruction analysis FAILED: the robustness comparison ===" >&2
  exit "$comparison_status"
fi
if [[ "$report_status" -ne 0 ]]; then
  echo "=== reconstruction analysis FAILED: the reproducibility record ===" >&2
  exit "$report_status"
fi
echo "=== reconstruction analysis complete ==="
