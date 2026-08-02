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
# For each fold the ladder is walked from the smallest value upward and stops at
# the first value that survives. That is exactly the rule the stabilisation is
# meant to follow — the smallest value that allows the model to complete — and
# it makes the answer a measurement rather than a guess.
#
# This fits no analysis output and writes into its own directory. It is a
# diagnostic, not a stage of the pipeline.

set -uo pipefail

CELLS="${CELLS:-results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv}"
OUTPUT_DIR="${OUTPUT_DIR:-results/phenology_stabilisation_diagnostic}"
DRAWS="${DRAWS:-20}"
FOLDS="${FOLDS:-1 2 3 4 5}"
# Ascending. 0 is included so the unstabilised behaviour of every fold is
# measured rather than assumed from the two folds a pipeline run reached.
LADDER="${LADDER:-0 1e-8 1e-7 1e-6 1e-5 1e-4 1e-3 1e-2}"

mkdir -p "$OUTPUT_DIR" "$OUTPUT_DIR/logs"
results="${OUTPUT_DIR}/phenology_stabilisation_sweep.csv"
echo "fold,diagonal,status,exit_code,elapsed_seconds" > "$results"
setup_failed=0

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
    tail -5 "$log"
    if [[ "$status" == "setup_error" ]]; then
      echo "  -> setup failure; the sweep cannot measure anything. Stopping." >&2
      setup_failed=1
      break 2
    fi
    if [[ "$status" == "survived" ]]; then
      echo "  -> smallest surviving value for fold ${fold}: ${diagonal}"
      break
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

echo
echo "=== smallest surviving diagonal per fold ==="
# The binding value for the pipeline is the largest of the per-fold minima: any
# smaller value leaves at least one fold aborting.
awk -F, 'NR > 1 && $3 == "survived" && !seen[$1]++ { print "fold " $1 ": " $2 }' \
  "$results"
awk -F, 'NR > 1 && $3 == "survived" && !seen[$1]++ { print $2 }' "$results" \
  | sort -g | tail -1 \
  | sed 's/^/binding value for all measured folds: /'

missing=$(awk -F, 'NR > 1 { seen[$1] = seen[$1] || ($3 == "survived") }
  END { for (f in seen) if (!seen[f]) printf "%s ", f }' "$results")
if [[ -n "${missing// /}" ]]; then
  echo "folds with no surviving value in the ladder: ${missing}"
fi
