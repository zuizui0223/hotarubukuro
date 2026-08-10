#!/usr/bin/env bash
set -euo pipefail

# Full current-input reanalysis.
#
# Scientific design is intentionally unchanged. This script only replaces the
# frozen upstream boundary with a fresh reconstruction from the current 1,965-row
# Data_S1 table and the validated seeded Bombus SDM artifact, then hands the new
# v11 phenotype and v15 1-km cell tables to the active downstream publication
# pipeline in reconstruction mode.

ROOT="$(pwd)"
UPSTREAM_DIR="${1:-${ROOT}/upstream}"
SOURCE_ARTIFACT="${2:-${ROOT}/source-artifact}"
UPSTREAM_COMMIT="1d7c575f1191e4e035db01a617f52b75eeaca313"
SUBMISSION_DRAWS="${HOTARUBUKURO_SUBMISSION_DRAWS:-10000}"
JOINT_LATENT_DRAWS="${HOTARUBUKURO_JOINT_LATENT_DRAWS:-10000}"
JOINT_OBS_REPS="${HOTARUBUKURO_JOINT_OBS_REPS:-20}"

for variable in ROOT UPSTREAM_DIR SOURCE_ARTIFACT; do
  value="${!variable}"
  [[ -d "$value" ]] || { echo "Missing directory ${variable}=${value}" >&2; exit 1; }
done
[[ -f Data_S1.csv ]] || { echo "Data_S1.csv is missing" >&2; exit 1; }
[[ -f "${SOURCE_ARTIFACT}/results/bombus_sdm_rebuild_A/flower_prediction_coverage.csv" ]] || {
  echo "The validated Bombus source artifact is incomplete." >&2
  exit 1
}

mkdir -p reanalysis_inputs reanalysis_status results data/processed/rasters data/cache/rasters

run_logged() {
  local label="$1"
  shift
  echo "=== ${label} ==="
  "$@" 2>&1 | tee "${ROOT}/reanalysis_status/${label}.log"
}

# ---------------------------------------------------------------------------
# 1. Restore only static public inputs from the old immutable snapshot.
#    Its frozen phenotype/cell tables and archived Bombus surfaces are NOT used.
# ---------------------------------------------------------------------------
rm -rf reproduction_inputs/base_snapshot
run_logged restore_static_snapshot \
  bash scripts/canonical_snapshot.sh restore \
    inputs/canonical_snapshot.json reproduction_inputs/base_snapshot

BASE_INPUTS="${ROOT}/reproduction_inputs/base_snapshot/analysis_inputs"
WORLDPOP="${BASE_INPUTS}/rasters/population_count_Japan_crop.tif"
MLIT_CACHE="${BASE_INPUTS}/mlit_l03_2021"
DID_CACHE="${BASE_INPUTS}/mlit_did_2015"
STATIC_HUMAN="${BASE_INPUTS}/results/public_rasters/mlit_human_forest_edge_2021"
for path in "$WORLDPOP" "$MLIT_CACHE" "$DID_CACHE" "$STATIC_HUMAN"; do
  [[ -e "$path" ]] || { echo "Static snapshot input missing: $path" >&2; exit 1; }
done

rm -rf results/public_rasters/mlit_human_forest_edge_2021
mkdir -p results/public_rasters
cp -a "$STATIC_HUMAN" results/public_rasters/

# ---------------------------------------------------------------------------
# 2. Use the rasters already frozen inside the new SDM artifact, and freshly
#    acquire only the environmental layers needed by the unchanged flower-colour
#    upstream stage but not by the SDM build.
# ---------------------------------------------------------------------------
cp -a "${SOURCE_ARTIFACT}/data/processed/rasters/." data/processed/rasters/
if [[ -f "${SOURCE_ARTIFACT}/data/processed/raster_manifest.csv" ]]; then
  cp "${SOURCE_ARTIFACT}/data/processed/raster_manifest.csv" data/processed/raster_manifest.csv
fi
if [[ -f "${SOURCE_ARTIFACT}/data/processed/raster_download_manifest.csv" ]]; then
  cp "${SOURCE_ARTIFACT}/data/processed/raster_download_manifest.csv" data/processed/raster_download_manifest.csv
fi

MISSING_DOWNLOAD="chelsa_bio05,chelsa_bio10,chelsa_gdd5,chelsa_bio12,chelsa_bio14,chelsa_bio15,soilgrids_ocd_0_5cm,soilgrids_bdod_0_5cm"
MISSING_PREPARE="chelsa_bio05,chelsa_bio10,chelsa_gdd5,chelsa_bio12,chelsa_bio14,chelsa_bio15,soilgrids_ocd_0_5cm"
run_logged download_flower_environment_layers \
  Rscript source_build/download_rasters.R --only="$MISSING_DOWNLOAD"
run_logged prepare_flower_environment_layers \
  Rscript source_build/prepare_rasters.R --only="$MISSING_PREPARE" --force

PUBLIC_CACHE="${ROOT}/reproduction_inputs/public_environment_cache_current"
rm -rf "$PUBLIC_CACHE"
mkdir -p "$PUBLIC_CACHE"
python3 - "$ROOT" "$PUBLIC_CACHE" <<'PY'
from pathlib import Path
import shutil, sys
root = Path(sys.argv[1])
cache = Path(sys.argv[2])
processed = root / "data" / "processed" / "rasters"
aliases = {
    "chelsa_bio05.tif": "bio5_Japan_crop_30s.tif",
    "chelsa_bio10.tif": "bio10_Japan_crop_30s.tif",
    "chelsa_bio04.tif": "bio4_Japan_crop_30s.tif",
    "chelsa_gdd5.tif": "gdd5_Japan_crop_30s.tif",
    "chelsa_cmimean.tif": "CMI_Japan_crop_30s.tif",
    "chelsa_bio12.tif": "bio12_Japan_crop_30s.tif",
    "chelsa_bio14.tif": "bio14_Japan_crop_30s.tif",
    "chelsa_bio15.tif": "bio15_Japan_crop_30s.tif",
    "chelsa_rsdsmean.tif": "RSDS_Japan_crop_30s.tif",
    "soilgrids_bdod_0_5cm_mean.tif": "bdod_0-5cm_mean_30s.tif",
    "soilgrids_cfvo_0_5cm_mean.tif": "cfvo_0-5cm_mean_30s.tif",
    "soilgrids_sand_0_5cm_mean.tif": "sand_0-5cm_mean_30s.tif",
    "soilgrids_silt_0_5cm_mean.tif": "silt_0-5cm_mean_30s.tif",
    "soilgrids_nitrogen_0_5cm_mean.tif": "nitrogen_0-5cm_mean_30s.tif",
    "soilgrids_ocd_0_5cm_mean.tif": "ocd_0-5cm_mean_30s.tif",
    "soilgrids_soc_0_5cm_mean.tif": "soc_0-5cm_mean_30s.tif",
    "soilgrids_phh2o_0_5cm_mean.tif": "phh2o_0-5cm_mean_30s.tif",
    "elevation_30s.tif": "elevation_Japan_crop.tif",
}
missing = [src for src in aliases if not (processed / src).is_file()]
if missing:
    raise SystemExit("Missing prepared rasters: " + ", ".join(missing))
for src, dst in aliases.items():
    shutil.copy2(processed / src, cache / dst)
print(f"Aliased {len(aliases)} current prepared rasters into {cache}")
PY
cp "$WORLDPOP" "${PUBLIC_CACHE}/population_count_Japan_crop.tif"

# ---------------------------------------------------------------------------
# 3. Preserve all 1,965 source rows, but make the duplicate indicator mean the
#    actual exclusion operation: stable source-row order, retain the first image
#    for each exact SHA-256 and mark only later copies as duplicate extras.
#    The original boolean is retained in a separate provenance column.
# ---------------------------------------------------------------------------
python3 - <<'PY'
import csv, json, math, hashlib
from pathlib import Path

source = Path("Data_S1.csv")
out = Path("reanalysis_inputs/Data_S1_current_analysis.csv")
changed = Path("reanalysis_inputs/duplicate_semantics_changes.csv")
manifest_path = Path("reanalysis_inputs/upstream_data_manifest.json")

with source.open(newline="", encoding="utf-8-sig") as handle:
    reader = csv.DictReader(handle)
    fields = list(reader.fieldnames or [])
    rows = list(reader)

if len(rows) != 1965:
    raise SystemExit(f"Expected 1965 source rows; found {len(rows)}")
for key in ("source_row", "image_sha256", "duplicate_image_sha256", "observation_id"):
    if key not in fields:
        raise SystemExit(f"Missing required Data_S1 column: {key}")

def truthy(value):
    return str(value).strip().lower() in {"1", "true", "t", "yes", "y"}

def source_order_value(value, fallback):
    try:
        x = float(value)
        if math.isfinite(x):
            return (0, x, fallback)
    except Exception:
        pass
    return (1, fallback, fallback)

order = sorted(range(len(rows)), key=lambda i: source_order_value(rows[i]["source_row"], i))
seen = set()
canonical_extra = [False] * len(rows)
for i in order:
    image_hash = str(rows[i]["image_sha256"]).strip()
    if image_hash and image_hash in seen:
        canonical_extra[i] = True
    elif image_hash:
        seen.add(image_hash)

if "duplicate_image_sha256_original" not in fields:
    fields.append("duplicate_image_sha256_original")
if "duplicate_image_sha256_semantics" not in fields:
    fields.append("duplicate_image_sha256_semantics")

change_rows = []
original_true = 0
for i, row in enumerate(rows):
    original = truthy(row["duplicate_image_sha256"])
    original_true += int(original)
    row["duplicate_image_sha256_original"] = "True" if original else "False"
    row["duplicate_image_sha256"] = "True" if canonical_extra[i] else "False"
    row["duplicate_image_sha256_semantics"] = "stable_source_order_extra_exact_hash_only"
    if original != canonical_extra[i]:
        change_rows.append({
            "observation_id": row["observation_id"],
            "source_row": row["source_row"],
            "image_sha256": row["image_sha256"],
            "original_duplicate_flag": original,
            "canonical_extra_duplicate": canonical_extra[i],
        })

with out.open("w", newline="", encoding="utf-8") as handle:
    writer = csv.DictWriter(handle, fieldnames=fields, extrasaction="ignore")
    writer.writeheader()
    writer.writerows(rows)
with changed.open("w", newline="", encoding="utf-8") as handle:
    writer = csv.DictWriter(handle, fieldnames=list(change_rows[0]) if change_rows else [
        "observation_id", "source_row", "image_sha256", "original_duplicate_flag", "canonical_extra_duplicate"
    ])
    writer.writeheader()
    writer.writerows(change_rows)

sha = hashlib.sha256(out.read_bytes()).hexdigest()
manifest = {
    "source_file": str(source),
    "source_rows": len(rows),
    "derived_analysis_file": str(out),
    "derived_sha256": sha,
    "original_duplicate_flag_true": original_true,
    "canonical_extra_duplicate_rows": sum(canonical_extra),
    "changed_duplicate_flag_rows": len(change_rows),
    "duplicate_rule": "sort by numeric source_row then row order; retain first exact image_sha256; mark later copies only",
    "response_warning_flags_used_for_exclusion": False,
}
manifest_path.write_text(json.dumps(manifest, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
print(json.dumps(manifest, ensure_ascii=False))
PY

cp reanalysis_inputs/Data_S1_current_analysis.csv "${UPSTREAM_DIR}/Data_S1.csv"

# ---------------------------------------------------------------------------
# 4. Insert the validated fresh Bombus predictions and their matching frozen
#    occurrence snapshot into the exact upstream implementation that originally
#    generated v11 and v15. No historical Bombus TIFF is restored.
# ---------------------------------------------------------------------------
rm -rf "${UPSTREAM_DIR}/results"
mkdir -p \
  "${UPSTREAM_DIR}/results/enmeval_aicc_reselected/predictions" \
  "${UPSTREAM_DIR}/results/bombus_occurrence_phenology_cache" \
  "${UPSTREAM_DIR}/results/public_rasters"
cp "${SOURCE_ARTIFACT}/results/bombus_sdm_rebuild_A/predictions/"*.tif \
  "${UPSTREAM_DIR}/results/enmeval_aicc_reselected/predictions/"
cp "${SOURCE_ARTIFACT}/results/bombus_occurrence_snapshot/"*_gbif.csv \
  "${UPSTREAM_DIR}/results/bombus_occurrence_phenology_cache/"
cp -a "$STATIC_HUMAN" "${UPSTREAM_DIR}/results/public_rasters/"

mkdir -p results/enmeval_aicc_reselected
rm -rf results/enmeval_aicc_reselected/predictions
cp -a "${SOURCE_ARTIFACT}/results/bombus_sdm_rebuild_A/predictions" \
  results/enmeval_aicc_reselected/
rm -rf results/bombus_sdm_rebuild_A results/bombus_occurrence_snapshot
cp -a "${SOURCE_ARTIFACT}/results/bombus_sdm_rebuild_A" results/
cp -a "${SOURCE_ARTIFACT}/results/bombus_occurrence_snapshot" results/

# ---------------------------------------------------------------------------
# 5. Rebuild the upstream v11/v15 boundary in the original order and with the
#    original analysis implementation from the snapshot-producing commit.
# ---------------------------------------------------------------------------
pushd "$UPSTREAM_DIR" >/dev/null
mkdir -p results/environment_v3
run_logged upstream_build_environment_input \
  Rscript scripts/build_environment_input.R \
    --raw-colour-csv=Data_S1.csv \
    --cache-root="$PUBLIC_CACHE" \
    --output-csv=results/environment_v3/ecological_input_v2.csv

run_logged upstream_run_environment_spatial \
  Rscript scripts/run_environment_spatial.R \
    --anomaly-csv=results/environment_v3/ecological_input_v2.csv \
    --raw-colour-csv=Data_S1.csv \
    --bombus-dir=results/enmeval_aicc_reselected/predictions \
    --H-raster="$WORLDPOP" \
    --R-raster=results/public_rasters/mlit_human_forest_edge_2021/mlit_human_forest_edge_1km.tif \
    --N-raster="${PUBLIC_CACHE}/gdd5_Japan_crop_30s.tif" \
    --A-raster=results/public_rasters/mlit_human_forest_edge_2021/mlit_major_road_distance_1km.tif \
    --output-dir=results/ecological_v9_final_public_HRNA_50km \
    --run-inla=false \
    --author-review-confirmed=true

run_logged upstream_run_natural_biotic_covariates \
  Rscript scripts/run_natural_biotic_covariates.R \
    --analysis-data=results/ecological_v9_final_public_HRNA_50km/analysis_data.csv \
    --occurrence-dir=results/bombus_occurrence_phenology_cache \
    --raw-colour-csv=Data_S1.csv \
    --output-dir=results/ecological_v10_final_mechanism_HRNA \
    --tail-bootstraps=199

run_logged upstream_run_phenotype_hurdle \
  Rscript scripts/run_phenotype_hurdle.R \
    --analysis-data=results/ecological_v10_final_mechanism_HRNA/analysis_data_mechanism_v3.csv \
    --output-dir=results/ecological_v11_pigmentation_hurdle \
    --run-inla=true

run_logged upstream_run_multiscale_hotspots \
  Rscript scripts/run_multiscale_hotspots.R \
    --input=results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv \
    --output-dir=results/ecological_v15_multiscale_hotspots \
    --environment-cache="$PUBLIC_CACHE" \
    --worldpop-raster="$WORLDPOP" \
    --bootstrap=1000
popd >/dev/null

# ---------------------------------------------------------------------------
# 6. Publish only the freshly rebuilt broad-analysis boundary.
#    Downstream anomaly analyses are run once by run_downstream_current_inputs.sh;
#    the manuscript-facing Bombus test is a separate local-transition pipeline.
# ---------------------------------------------------------------------------
for stage in \
  environment_v3 \
  ecological_v9_final_public_HRNA_50km \
  ecological_v10_final_mechanism_HRNA \
  ecological_v11_pigmentation_hurdle \
  ecological_v15_multiscale_hotspots; do
  rm -rf "results/${stage}"
  cp -a "${UPSTREAM_DIR}/results/${stage}" results/
done

echo "=== fresh current-input broad-analysis boundary complete ==="
