#!/usr/bin/env bash
#
# Assemble the phenology threading replication and decide whether pinning the
# thread count made the component reproducible.
#
# The question is not "did it succeed once". The measured grid showed the same
# fold at the same diagonal surviving on one run and aborting on another under
# INLA's default threading, so a single success proves nothing. This asks
# whether repeated runs of identical inputs agree.
#
# Two things have to hold, and they are reported separately because they fail
# for different reasons:
#
#   completion — every fold completes in every repeat. If not, threading was not
#                sufficient, and that is the answer rather than a prompt to try
#                something else.
#   determinism — every repeat of a fold produced the same posterior draw range.
#                Completion alone would still permit run-to-run variation; an
#                identical range under a fixed seed is evidence the stored
#                configurations really are a function of the inputs now.
#
# Exit status:
#   0  every fold completed in every repeat, and repeats agree
#   3  some fold aborted in some repeat — threading was not sufficient
#   5  all completed but repeats disagree — reproducible completion not achieved
#   4  the replication is incomplete; no conclusion may be drawn from it

set -uo pipefail

CELL_DIR="${1:-threading-cells}"
FOLDS="${FOLDS:-1 2 3 4 5}"
REPEATS="${REPEATS:-1 2 3}"
OUTPUT="${OUTPUT:-results/phenology_threading_check/phenology_threading_check.csv}"

mkdir -p "$(dirname "$OUTPUT")"
echo "fold,repeat,status,exit_code,elapsed_seconds,draw_range" > "$OUTPUT"
find "$CELL_DIR" -name 'cell_*.csv' -print0 2>/dev/null \
  | sort -z | xargs -0 -r cat >> "$OUTPUT"

n_folds=$(echo $FOLDS | wc -w)
n_repeats=$(echo $REPEATS | wc -w)
expected=$(( n_folds * n_repeats ))
measured=$(( $(wc -l < "$OUTPUT") - 1 ))

echo "=== phenology threading replication (num.threads pinned) ==="
cat "$OUTPUT"
echo

if [[ "$measured" -ne "$expected" ]]; then
  echo "=== replication INCOMPLETE: ${measured} of ${expected} cells ===" >&2
  echo "No conclusion may be drawn. Re-run the missing cells." >&2
  exit 4
fi

aborted=$(awk -F, 'NR > 1 && $3 != "survived"' "$OUTPUT" | wc -l)
if [[ "$aborted" -gt 0 ]]; then
  echo "=== threading was NOT sufficient ===" >&2
  echo "${aborted} of ${expected} attempts did not complete:" >&2
  awk -F, 'NR > 1 && $3 != "survived" { print "  fold " $1 " repeat " $2 ": " $3 }' \
    "$OUTPUT" >&2
  echo >&2
  echo "Pinning the thread count does not make the phenology component" >&2
  echo "complete. Report this rather than moving on to model changes." >&2
  exit 3
fi

echo "completion: ${expected} of ${expected} attempts completed"
echo
echo "=== determinism: do repeats of a fold agree? ==="
# One distinct draw range per fold means every repeat produced the same draws.
disagreements=0
for fold in $FOLDS; do
  ranges=$(awk -F, -v f="$fold" 'NR > 1 && $1 == f { print $6 }' "$OUTPUT" \
    | sort -u)
  count=$(echo "$ranges" | wc -l)
  if [[ "$count" -eq 1 ]]; then
    echo "  fold ${fold}: identical across ${n_repeats} repeats  (${ranges})"
  else
    echo "  fold ${fold}: ${count} DIFFERENT draw ranges across ${n_repeats} repeats" >&2
    echo "$ranges" | sed 's/^/      /' >&2
    disagreements=$((disagreements + 1))
  fi
done

echo
if [[ "$disagreements" -gt 0 ]]; then
  echo "=== completion is reproducible but the draws are not ===" >&2
  echo "${disagreements} fold(s) produced different draws across repeats of" >&2
  echo "identical inputs. Pinning the thread count made the component" >&2
  echo "complete but did not make it deterministic." >&2
  exit 5
fi

echo "=== phenology is reproducible under the pinned thread count ==="
echo "All ${n_folds} folds completed in all ${n_repeats} repeats, and every"
echo "fold produced identical draws across repeats."
