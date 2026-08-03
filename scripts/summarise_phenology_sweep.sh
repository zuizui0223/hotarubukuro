#!/usr/bin/env bash
#
# Assemble the phenology stabilisation grid and apply the selection rule.
#
# Reads one CSV row per measured cell — written by whatever ran the attempts —
# and answers the only question that matters: what is the smallest diagonal for
# which every fold completes?
#
# Kept separate from the code that runs the attempts because the attempts are
# now one CI job each. A sweep of 40 cells in a single job was lost twice to
# runner failure (30771243504 at 17 minutes, 30772489489 at 70), and in both
# cases every measurement went with it — the step summary is only published
# when a step completes, so streaming rows into it does not survive a step that
# never ends. One job per cell means a lost runner costs one cell.
#
# Exit status:
#   0  complete grid, at least one value survived every fold
#   3  complete grid, no value survived every fold — diagonal stabilisation
#      alone is insufficient; the ladder must not be extended past the measured
#      range in search of one
#   4  partial grid; no value may be adopted from it

set -uo pipefail

CELL_DIR="${1:-sweep-cells}"
FOLDS="${FOLDS:-1 2 3 4 5}"
LADDER="${LADDER:-0 1e-8 1e-7 1e-6 1e-5 1e-4 1e-3 1e-2}"
OUTPUT="${OUTPUT:-results/phenology_stabilisation_diagnostic/phenology_stabilisation_sweep.csv}"

mkdir -p "$(dirname "$OUTPUT")"
echo "fold,diagonal,status,exit_code,elapsed_seconds" > "$OUTPUT"
# Cell files are named so the sort is stable and the grid reads in fold order.
find "$CELL_DIR" -name 'cell_*.csv' -print0 2>/dev/null \
  | sort -z \
  | xargs -0 -r cat >> "$OUTPUT"

n_folds=$(echo $FOLDS | wc -w)
n_values=$(echo $LADDER | wc -w)
expected=$(( n_folds * n_values ))
measured=$(( $(wc -l < "$OUTPUT") - 1 ))

echo "=== phenology stabilisation grid ==="
cat "$OUTPUT"
echo

setup_errors=$(awk -F, 'NR > 1 && $3 == "setup_error"' "$OUTPUT" | wc -l)
if [[ "$setup_errors" -gt 0 ]]; then
  echo "=== ${setup_errors} cell(s) never reached the model ===" >&2
  awk -F, 'NR > 1 && $3 == "setup_error" { print "  fold " $1 ", diagonal " $2 }' \
    "$OUTPUT" >&2
  exit 2
fi

# No value may be adopted from a partial grid. The rule is "the smallest value
# for which all five folds complete", and a truncated grid cannot answer it: a
# value whose measured cells all survived may still have an unmeasured fold
# that aborts, and a smaller usable value may not have been measured at all.
if [[ "$measured" -ne "$expected" ]]; then
  echo "=== grid INCOMPLETE: ${measured} of ${expected} cells ===" >&2
  echo "No value may be adopted from a partial grid. The missing cells:" >&2
  for fold in $FOLDS; do
    for diagonal in $LADDER; do
      if ! awk -F, -v f="$fold" -v d="$diagonal" \
        'NR > 1 && $1 == f && $2 == d { found = 1 } END { exit !found }' "$OUTPUT"
      then
        echo "  fold ${fold}, diagonal ${diagonal}" >&2
      fi
    done
  done
  exit 4
fi

echo "grid complete: ${measured} of ${expected} cells"
echo
echo "=== survival by value, across all ${n_folds} folds ==="
# Per-value totals, not per-fold minima. The response is not monotone in the
# value — the pipeline cleared fold 1 at 1e-8 and aborted on it at 1e-7 —
# because a larger diagonal changes the fitted hyperparameter configurations
# and therefore which precision matrices get factorised. Reading survival per
# value avoids the monotonicity assumption entirely.
awk -F, -v n="$n_folds" '
  NR > 1 && $3 == "survived" { survived[$2]++ }
  NR > 1 { seen[$2] = 1 }
  END {
    for (d in seen) {
      printf "%s\t%d/%d folds%s\n", d, survived[d] + 0, n,
        (survived[d] + 0 == n ? "\tUSABLE" : "")
    }
  }' "$OUTPUT" | sort -g

usable=$(awk -F, -v n="$n_folds" '
  NR > 1 && $3 == "survived" { survived[$2]++ }
  END { for (d in survived) if (survived[d] == n) print d }' "$OUTPUT" \
  | sort -g | head -1)

echo
if [[ -n "$usable" ]]; then
  echo "smallest value surviving every fold: ${usable}"
  echo "$usable" > "$(dirname "$OUTPUT")/selected_diagonal.txt"
else
  echo "=== no value in the ladder survives every fold ===" >&2
  echo "Diagonal stabilisation alone is insufficient. Report the grid rather" >&2
  echo "than extending the ladder past the measured range." >&2
  exit 3
fi
