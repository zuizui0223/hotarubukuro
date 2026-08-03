#!/usr/bin/env bash
#
# Survey the reconstruction pipeline for downstream failures.
#
# A DEVELOPMENT DIAGNOSTIC. Not a reconstruction, not a reference, and not
# something any result may be drawn from.
#
# Every stage after 02 consumes stage 02's cross-fitted checkpoints, so no stage
# after 02 has ever executed on the reconstruction population. Finding their
# failures one run at a time costs a full CI cycle each. This runs the pipeline
# with --continue-on-failure so that a single run reports the whole queue.
#
# It is deliberately a separate driver from scripts/run_reconstruction_analysis.sh
# rather than a flag on it. The canonical driver has no survey switch at all, so
# the reference reconstruction cannot be produced by a run that tolerated a
# failing stage. Correspondingly, this driver:
#
#   - does not run the published-versus-reconstruction comparison;
#   - does not build figures;
#   - does not write a reproducibility report.
#
# Those three are what turn a run into a citable artifact, and a survey run must
# not be one. The comparison additionally refuses a survey run outright, by
# reading run_mode.txt, so the separation does not rest on this script alone.

set -uo pipefail

SNAPSHOT_DIR="${SNAPSHOT_DIR:-reproduction_inputs/snapshot}"
STATUS_DIR="${STATUS_DIR:-reproduction_status}"
REPORT_DIR="${REPORT_DIR:-reproducibility}"
LOCK_DIR="results/final_analysis_pipeline"
PHENOLOGY_DIAGONAL="${PHENOLOGY_DIAGONAL:-0}"
# Numerical reproducibility for the phenology component only, explicit and
# identical in the canonical and survey drivers.
#
# INLA's inference stage is multi-threaded by default, so the stored
# hyperparameter configurations can differ between runs of identical inputs.
# inla.qsample factorises whichever were stored, which is why the same fold at
# the same diagonal survived on one run and aborted on another. Pinning the
# thread count makes the configurations a function of the inputs. It is a
# reproducibility setting, not a scientific one: no formula, prior, fold, draw
# count, seed, diagonal, threshold or downstream definition changes, and no
# other component is affected. The sampling call was already single-threaded;
# this covers the fit, which was not.
PHENOLOGY_NUM_THREADS="${PHENOLOGY_NUM_THREADS:-1:1}"

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

echo "=============================================================="
echo " SURVEY RUN — development diagnostic"
echo " Outputs in ${LOCK_DIR} are not a reference reconstruction."
echo " No comparison, no figures, no reproducibility report."
echo "=============================================================="

run_logged verify_canonical_snapshot \
  Rscript scripts/verify_canonical_snapshot.R \
    --descriptor inputs/canonical_snapshot.json \
    --root "$SNAPSHOT_DIR" \
    --report-dir "$REPORT_DIR" \
    --materialise true \
    --mlit-cache "$HOTARUBUKURO_MLIT_CACHE" \
    --did-cache "$HOTARUBUKURO_DID_CACHE"

pipeline_status=0
run_logged survey_pipeline \
  Rscript scripts/run_publication_pipeline.R \
    --mode=full \
    --tests=true \
    --baseline=reconstruction \
    --discordance=true \
    --phenology-diagonal="$PHENOLOGY_DIAGONAL" \
    --phenology-num-threads="$PHENOLOGY_NUM_THREADS" \
    --continue-on-failure=true \
    --output="$LOCK_DIR" || pipeline_status=$?

echo
echo "=== stage outcomes ==="
if [[ -s "${LOCK_DIR}/final_stage_manifest.csv" ]]; then
  cut -d, -f1,4 "${LOCK_DIR}/final_stage_manifest.csv"
else
  echo "No stage manifest was produced."
fi

echo
echo "=== run mode ==="
cat "${LOCK_DIR}/run_mode.txt" 2>/dev/null || echo "no run_mode.txt"

# Printed last so the queue of blockers is the final thing in the log.
if [[ "$pipeline_status" -ne 0 && -s "${LOCK_DIR}/final_stage_manifest.csv" ]]; then
  failed_stages="$(awk -F'","' 'NR > 1 && $4 == "FAIL" { gsub(/"/, "", $1); print $1 }' \
    "${LOCK_DIR}/final_stage_manifest.csv")"
  echo >&2
  echo "=== blockers found by this survey ===" >&2
  echo "$failed_stages" >&2
  for failed_stage in $failed_stages; do
    log="${LOCK_DIR}/logs/${failed_stage}_stderr.log"
    if [[ -s "$log" ]]; then
      echo >&2
      echo "--- ${failed_stage} (last 30 lines of stderr) ---" >&2
      tail -30 "$log" >&2
    fi
  done
fi

# A survey run reports; it does not certify. Its exit status is the pipeline's,
# so a survey that found blockers is red.
exit "$pipeline_status"
