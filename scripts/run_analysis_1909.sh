#!/usr/bin/env bash
set -euo pipefail

export TZ=UTC
export LC_ALL=C
export OMP_NUM_THREADS="${OMP_NUM_THREADS:-1}"
export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-1}"
export MKL_NUM_THREADS="${MKL_NUM_THREADS:-1}"
export VECLIB_MAXIMUM_THREADS="${VECLIB_MAXIMUM_THREADS:-1}"
export NUMEXPR_NUM_THREADS="${NUMEXPR_NUM_THREADS:-1}"

SNAPSHOT_DESCRIPTOR="${SNAPSHOT_DESCRIPTOR:-inputs/canonical_snapshot.json}"
SNAPSHOT_DIR="${SNAPSHOT_DIR:-${PWD}/reproduction_inputs/snapshot}"
REPORT_DIR="${REPORT_DIR:-${PWD}/reproducibility}"
STATUS_DIR="${STATUS_DIR:-${PWD}/reproduction_status}"
RUN_TESTS="${RUN_TESTS:-true}"
BUILD_FIGURES="${BUILD_FIGURES:-true}"

mkdir -p "$SNAPSHOT_DIR" "$REPORT_DIR" "$STATUS_DIR"
export HOTARUBUKURO_RUN_STARTED="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
printf '%s\n' "$HOTARUBUKURO_RUN_STARTED" > "$STATUS_DIR/run_started_utc.txt"

failure_stage="initialization"
show_failure() {
  status=$?
  if [[ $status -ne 0 ]]; then
    echo "1909 analysis failed during: ${failure_stage}" >&2
  fi
  exit $status
}
trap show_failure EXIT

failure_stage="restore immutable 1909 snapshot"
bash scripts/canonical_snapshot.sh restore \
  "$SNAPSHOT_DESCRIPTOR" "$SNAPSHOT_DIR"

analysis_inputs="$SNAPSHOT_DIR/analysis_inputs"
test -d "$analysis_inputs"
for root in results rasters; do
  if [[ -d "$analysis_inputs/$root" ]]; then
    mkdir -p "$root"
    cp -a "$analysis_inputs/$root/." "$root/"
  fi
done

failure_stage="verify analysis population"
Rscript scripts/check_analysis_population.R \
  --expectations inputs/analysis_1909_expectations.csv \
  --report-dir "$REPORT_DIR" \
  --strict true

failure_stage="preflight environment"
Rscript scripts/preflight.R \
  --scope canonical \
  --report-dir "$REPORT_DIR"

failure_stage="run model and validation stages"
Rscript scripts/run_publication_pipeline.R \
  --mode full \
  --baseline analysis_1909 \
  --tests "$RUN_TESTS"

failure_stage="write analysis arc"
Rscript scripts/report_analysis_arc.R --report-dir "$REPORT_DIR"

if [[ "$BUILD_FIGURES" == "true" ]]; then
  failure_stage="build fresh figures"
  Rscript scripts/build_publication_figures.R
fi

failure_stage="write reproducibility report"
Rscript scripts/write_reproducibility_report.R \
  --root . \
  --stage-registry reproducibility/pipeline_stage_registry.csv \
  --output-dir "$REPORT_DIR" \
  --inputs "Data_S1.csv,inputs/canonical_snapshot.json,inputs/analysis_1909_expectations.csv" \
  --outputs "results,manuscript/figures,reproducibility"

failure_stage="write run summary"
python3 - <<'PY'
from pathlib import Path
import csv, os
p = Path("reproducibility/reproduction_summary.md")
p.parent.mkdir(parents=True, exist_ok=True)
pop = list(csv.DictReader(open("reproducibility/analysis_population_check.csv")))
stages = list(csv.DictReader(open("results/final_analysis_pipeline/final_stage_manifest.csv")))
results = list(csv.DictReader(open("results/final_analysis_pipeline/final_result_registry.csv")))
lookup = {r.get("result_id"): r for r in results}
lines = [
    "# 1,909 analysis reproduction summary", "",
    f"- commit: `{os.getenv('GITHUB_SHA', 'local')}`",
    f"- started UTC: `{os.getenv('HOTARUBUKURO_RUN_STARTED', '')}`",
    f"- population checks: {sum(r['status']=='PASS' for r in pop)}/{len(pop)} PASS",
    f"- stages: {sum(r['status']=='PASS' for r in stages)}/{len(stages)} PASS",
    "", "## Key generated quantities", "",
]
for result_id in [
    "local_bombus_presence", "local_bombus_intensity",
    "local_isolate_count", "local_isolate_fraction",
    "local_population_5km"
]:
    row = lookup.get(result_id)
    if row:
        lines.append(
            f"- `{result_id}`: estimate={row.get('estimate','')}, "
            f"raw_p={row.get('raw_p','')}, corrected_p={row.get('corrected_p','')}"
        )
lines += [
    "", "The active pipeline is statistically reproducible; INLA posterior-sample",
    "hashes are not guaranteed to be bit-identical across runs. Threshold-adjacent",
    "Monte Carlo quantities must be reported with the run commit and uncertainty.",
]
p.write_text("\n".join(lines) + "\n", encoding="utf-8")
PY

failure_stage="inventory outputs"
find results reproducibility manuscript/figures \
  -type f -printf '%p\t%s\n' 2>/dev/null | sort \
  > "$STATUS_DIR/result_inventory.tsv"

trap - EXIT
echo "1909 analysis completed successfully."
