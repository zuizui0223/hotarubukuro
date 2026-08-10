#!/usr/bin/env bash
set -euo pipefail

export TZ=UTC
export LC_ALL=C
export OMP_NUM_THREADS="1"
export OPENBLAS_NUM_THREADS="1"
export MKL_NUM_THREADS="1"
export VECLIB_MAXIMUM_THREADS="1"
export NUMEXPR_NUM_THREADS="1"
export GOTO_NUM_THREADS="1"
export BLIS_NUM_THREADS="1"
export INLA_NUM_THREADS="1"
export OMP_DYNAMIC="FALSE"

SNAPSHOT_DESCRIPTOR="${SNAPSHOT_DESCRIPTOR:-inputs/canonical_snapshot.json}"
SNAPSHOT_DIR="${SNAPSHOT_DIR:-${PWD}/reproduction_inputs/snapshot}"
REPORT_DIR="${REPORT_DIR:-${PWD}/reproducibility}"
STATUS_DIR="${STATUS_DIR:-${PWD}/reproduction_status}"
RUN_TESTS="${RUN_TESTS:-true}"
BUILD_FIGURES="${BUILD_FIGURES:-true}"
HOTARUBUKURO_SUBMISSION_DRAWS="${HOTARUBUKURO_SUBMISSION_DRAWS:-10000}"
export HOTARUBUKURO_SUBMISSION_DRAWS

if [[ ! "$HOTARUBUKURO_SUBMISSION_DRAWS" =~ ^[0-9]+$ ]] || \
   (( HOTARUBUKURO_SUBMISSION_DRAWS < 5000 )); then
  echo "HOTARUBUKURO_SUBMISSION_DRAWS must be an integer >= 5000" >&2
  exit 2
fi

mkdir -p results reproducibility manuscript
find results -mindepth 1 -depth ! -path 'results/README.md' -delete
find reproducibility -mindepth 1 -type f \
  ! -name 'pipeline_stage_registry.csv' \
  ! -name 'phenology_removal_candidate_identity.md' -delete
find reproducibility -mindepth 1 -depth -type d -empty -delete
rm -rf "$SNAPSHOT_DIR" "$STATUS_DIR" manuscript/figures rasters
mkdir -p "$SNAPSHOT_DIR" "$REPORT_DIR" "$STATUS_DIR" manuscript/figures

export HOTARUBUKURO_RUN_STARTED="$(date -u +%s)"
export HOTARUBUKURO_RUN_STARTED_ISO="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
printf '%s\n' "$HOTARUBUKURO_RUN_STARTED_ISO" > "$STATUS_DIR/run_started_utc.txt"
printf '%s\n' "$HOTARUBUKURO_RUN_STARTED" > "$STATUS_DIR/run_started_epoch.txt"

{
  echo "OMP_NUM_THREADS=$OMP_NUM_THREADS"
  echo "OPENBLAS_NUM_THREADS=$OPENBLAS_NUM_THREADS"
  echo "MKL_NUM_THREADS=$MKL_NUM_THREADS"
  echo "VECLIB_MAXIMUM_THREADS=$VECLIB_MAXIMUM_THREADS"
  echo "NUMEXPR_NUM_THREADS=$NUMEXPR_NUM_THREADS"
  echo "GOTO_NUM_THREADS=$GOTO_NUM_THREADS"
  echo "BLIS_NUM_THREADS=$BLIS_NUM_THREADS"
  echo "INLA_NUM_THREADS=$INLA_NUM_THREADS"
  echo "OMP_DYNAMIC=$OMP_DYNAMIC"
  echo "HOTARUBUKURO_SUBMISSION_DRAWS=$HOTARUBUKURO_SUBMISSION_DRAWS"
} > "$STATUS_DIR/submission_compute_lock.txt"

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
bash scripts/canonical_snapshot.sh restore "$SNAPSHOT_DESCRIPTOR" "$SNAPSHOT_DIR"
analysis_inputs="$SNAPSHOT_DIR/analysis_inputs"
test -d "$analysis_inputs"

export HOTARUBUKURO_INPUT_ROOT="$analysis_inputs"
export HOTARUBUKURO_MLIT_CACHE="$analysis_inputs/mlit_l03_2021"
export HOTARUBUKURO_DID_CACHE="$analysis_inputs/mlit_did_2015"
test -d "$HOTARUBUKURO_MLIT_CACHE"
test -d "$HOTARUBUKURO_DID_CACHE"

for root in results rasters; do
  if [[ -d "$analysis_inputs/$root" ]]; then
    mkdir -p "$root"
    cp -a "$analysis_inputs/$root/." "$root/"
  fi
done

failure_stage="verify analysis population"
Rscript scripts/check_analysis_population.R \
  --expectations inputs/analysis_1909_expectations.csv \
  --report-dir "$REPORT_DIR" --strict true

failure_stage="preflight environment"
Rscript scripts/preflight.R --scope canonical --report-dir "$REPORT_DIR"

failure_stage="run model and validation stages"
Rscript scripts/run_publication_pipeline.R \
  --mode full --baseline analysis_1909 \
  --submission-draws "$HOTARUBUKURO_SUBMISSION_DRAWS" --tests "$RUN_TESTS"

failure_stage="write analysis arc"
Rscript scripts/report_analysis_arc.R --report-dir "$REPORT_DIR"

if [[ "$BUILD_FIGURES" == "true" ]]; then
  failure_stage="build fresh figures"
  Rscript scripts/build_publication_figures.R
fi

failure_stage="write reproducibility report"
Rscript scripts/write_reproducibility_report.R \
  --report-dir "$REPORT_DIR" --workflow analysis-1909 \
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
submission = list(csv.DictReader(open(
    "results/ecological_v25_submission_isolate_null/"
    "submission_isolate_natural_null_summary.csv"
)))
lookup = {r.get("result_id"): r for r in results}
primary = {r["metric"]: r for r in submission
           if r["configuration"] == "primary_10km_env1_all_white"}
lines = [
    "# 1,909 analysis reproduction summary", "",
    f"- commit: `{os.getenv('GITHUB_SHA', 'local')}`",
    f"- started UTC: `{os.getenv('HOTARUBUKURO_RUN_STARTED_ISO', '')}`",
    f"- population checks: {sum(r['status']=='PASS' for r in pop)}/{len(pop)} PASS",
    f"- stages: {sum(r['status']=='PASS' for r in stages)}/{len(stages)} PASS",
    f"- submission natural maps: {os.getenv('HOTARUBUKURO_SUBMISSION_DRAWS', '')}",
    "", "## Key generated quantities", "",
]
for result_id in [
    "local_bombus_limitation_presence", "local_bombus_limitation_intensity",
    "local_isolate_count", "local_isolate_fraction",
    "local_population_5km"
]:
    row = lookup.get(result_id)
    if row:
        lines.append(
            f"- `{result_id}`: estimate={row.get('estimate','')}, "
            f"raw_p={row.get('raw_p','')}, corrected_p={row.get('corrected_p','')}"
        )
for metric in ("candidate_count", "candidate_fraction"):
    row = primary.get(metric)
    if row:
        lines.append(
            f"- submission `{metric}`: observed={row['observed_value']}, "
            f"null_mean={row['null_mean']}, p={row['empirical_p']}, "
            f"MCSE={row['monte_carlo_se']}"
        )
lines += [
    "", "Stage 03 uses environmentally matched one-to-one Bombus-limitation pairs.",
    "The lower-third gate history and full threshold-grid multiplicity are retained.",
    "Bombus SDM support is predicted availability, not abundance or visitation.",
    "", "The observed isolate candidate set is fixed before the natural-map comparison.",
    "Submission isolate p-values use the higher-precision single-thread reference."
]
p.write_text("\n".join(lines) + "\n", encoding="utf-8")
PY

failure_stage="inventory outputs"
find results reproducibility manuscript/figures \
  -type f -printf '%p\t%s\n' 2>/dev/null | sort \
  > "$STATUS_DIR/result_inventory.tsv"

trap - EXIT
echo "1909 analysis completed successfully."
