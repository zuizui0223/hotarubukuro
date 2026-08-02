#!/usr/bin/env bash
#
# Sweep the phenology numerical stabilisation over folds and diagonal values.
#
# The abort is a SIGABRT inside the INLA subprocess that takes the whole R
# session down, so every attempt has to be its own process. This runs one
# scripts/diagnose_phenology_stabilisation.R per attempt and records the exit
# status, which is the only reliable signal: a surviving attempt exits 0, an
# aborted one exits non-zero after R fails to reopen the sampler's RNG file.
#
# The whole grid is measured — every fold at every value, with no early exit.
#
# An earlier version stopped at the first value each fold survived, which
# assumes the response is monotone in the value. It is not. At 1000 draws the
# pipeline cleared fold 1 with 1e-8 and aborted on it with 1e-7: a larger
# diagonal changes the fitted hyperparameter configurations themselves, so it
# changes which precision matrices get factorised rather than simply making all
# of them better conditioned. "Smallest value that works for this fold" is
# therefore not a quantity that can be found by walking upward and stopping.
#
# What the pipeline needs is one value that survives every fold at once, so
# that is what the summary reports: the smallest value in the ladder whose
# whole column survived.
#
# This fits no analysis output and writes into its own directory. It is a
# diagnostic, not a stage of the pipeline.

set -uo pipefail

CELLS="${CELLS:-results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv}"
OUTPUT_DIR="${OUTPUT_DIR:-results/phenology_stabilisation_diagnostic}"
# The analysis draw count, not a cost knob — see the comment in
# scripts/diagnose_phenology_stabilisation.R. Fewer draws leave low-weight
# posterior configurations unvisited and report survival the pipeline does not
# reproduce.
DRAWS="${DRAWS:-1000}"
FOLDS="${FOLDS:-1 2 3 4 5}"
# Ascending. 0 is included so the unstabilised behaviour of every fold is
# measured rather than assumed from the two folds a pipeline run reached.
LADDER="${LADDER:-0 1e-8 1e-7 1e-6 1e-5 1e-4 1e-3 1e-2}"

mkdir -p "$OUTPUT_DIR" "$OUTPUT_DIR/logs"
results="${OUTPUT_DIR}/phenology_stabilisation_sweep.csv"
echo "fold,diagonal,status,exit_code,elapsed_seconds" > "$results"
setup_failed=0

# Publish each row as it is measured, not only at the end.
#
# The sweep takes tens of minutes and its results live only inside the job until
# the artifact upload runs. Run 30771243504 lost its runner during finalisation:
# the upload was skipped, the logs never became downloadable, and every
# measurement it had made was discarded. The step summary is persisted
# server-side as it is written, so appending to it per attempt makes a lost
# runner cost only the attempts not yet made.
publish_row() {
  [[ -n "${GITHUB_STEP_SUMMARY:-}" ]] || return 0
  printf '%s\n' "$1" >> "$GITHUB_STEP_SUMMARY"
}
publish_row "## Phenology stabilisation sweep (streaming)"
publish_row ""
publish_row "| fold | diagonal | status | elapsed |"
publish_row "|---|---|---|---:|"

for fold in $FOLDS; do
  for diagonal in $LADDER; do
    log="${OUTPUT_DIR}/logs/fold${fold}_diagonal${diagonal}.log"
    echo "=== phenology fold ${fold}, diagonal ${diagonal} ==="
    started=$(date +%s)
    Rscript scripts/diagnose_phenology_stabilisation.R \
      --cells "$CELLS" \
      --fold "$fold" \
      --diagonal "$diagonal" \
      --draws "$DRAWS" > "$log" 2>&1
    code=$?
    ended=$(date +%s)
    # Status 2 is reserved for an attempt that never reached the model. Without
    # this distinction a missing input file records as "aborted" and a sweep
    # that measured nothing at all reads as a grid of genuine INLA failures —
    # which is exactly what run 30769694286 produced.
    case "$code" in
      0) status=survived ;;
      2) status=setup_error ;;
      *) status=aborted ;;
    esac
    echo "${fold},${diagonal},${status},${code},$((ended - started))" >> "$results"
    echo "  -> ${status} (exit ${code}, $((ended - started))s)"
    publish_row "| ${fold} | ${diagonal} | ${status} | $((ended - started))s |"
    tail -5 "$log"
    if [[ "$status" == "setup_error" ]]; then
      echo "  -> setup failure; the sweep cannot measure anything. Stopping." >&2
      setup_failed=1
      break 2
    fi
  done
done

echo
echo "=== phenology stabilisation sweep ==="
cat "$results"

# A sweep that measured nothing must not exit 0. The previous version reported
# success while every attempt had failed to open its input, and the workflow
# went green on a grid of results that described a missing file.
if [[ "$setup_failed" -ne 0 ]]; then
  echo >&2
  echo "=== sweep FAILED: no attempt reached the model ===" >&2
  echo "The cell table was not present. Materialise the canonical snapshot" >&2
  echo "into results/ before sweeping; restoring it is not sufficient." >&2
  exit 2
fi

n_folds=$(echo $FOLDS | wc -w)

echo
echo "=== survival by value, across all ${n_folds} folds ==="
# A value is usable by the pipeline only if every fold survives it. Reporting
# per-value totals rather than per-fold minima avoids the monotonicity
# assumption entirely: it reads the answer straight off the grid.
awk -F, -v n="$n_folds" '
  NR > 1 && $3 == "survived" { survived[$2]++ }
  NR > 1 { seen[$2] = 1 }
  END {
    for (d in seen) {
      printf "%s\t%d/%d folds%s\n", d, survived[d] + 0, n,
        (survived[d] + 0 == n ? "\tUSABLE" : "")
    }
  }' "$results" | sort -g

usable=$(awk -F, -v n="$n_folds" '
  NR > 1 && $3 == "survived" { survived[$2]++ }
  END { for (d in survived) if (survived[d] == n) print d }' "$results" \
  | sort -g | head -1)

echo
if [[ -n "$usable" ]]; then
  echo "smallest value surviving every fold: ${usable}"
else
  echo "=== no value in the ladder survives every fold ===" >&2
  echo "The diagonal is not a sufficient remedy at these values. Report the" >&2
  echo "grid rather than escalating past the measured range." >&2
  exit 3
fi
