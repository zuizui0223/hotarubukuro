#!/usr/bin/env bash
#
# Raw external-data reconstruction.
#
# Rebuilds everything upstream of the canonical analysis-input snapshot from the
# pinned public sources: CHELSA climatologies, SoilGrids soil properties,
# WorldClim elevation, the WorldPop population layer, MLIT land-use data, and
# GBIF occurrences. It then stages, validates, and (optionally) publishes the
# snapshot that the canonical analysis workflow starts from.
#
# This is deliberately a manually dispatched workflow. Those services are slow
# and occasionally unavailable, and a public data server being down must not
# turn every pull request red.

set -euo pipefail

PUBLIC_CACHE="${PUBLIC_CACHE:?PUBLIC_CACHE is required}"
MLIT_CACHE="${MLIT_CACHE:?MLIT_CACHE is required}"
GBIF_CACHE="${GBIF_CACHE:?GBIF_CACHE is required}"
BOMBUS_DIR="${BOMBUS_DIR:?BOMBUS_DIR is required}"
STATUS_DIR="${STATUS_DIR:?STATUS_DIR is required}"
REPORT_DIR="${REPORT_DIR:-reproducibility}"
HISTORICAL_PUBLICATION_COMMIT="${HISTORICAL_PUBLICATION_COMMIT:?HISTORICAL_PUBLICATION_COMMIT is required}"
SNAPSHOT_STAGING="${SNAPSHOT_STAGING:-snapshot_staging}"
SNAPSHOT_ID="${SNAPSHOT_ID:-analysis-input-snapshot-v1}"
WORLDPOP_URL="${WORLDPOP_URL:-https://data.worldpop.org/GIS/Population/Global_2000_2020_1km/2020/JPN/jpn_ppp_2020_1km_Aggregated.tif}"

mkdir -p "$STATUS_DIR" "$PUBLIC_CACHE" "$MLIT_CACHE" "$GBIF_CACHE" \
         "$BOMBUS_DIR" "$REPORT_DIR"

run_logged() {
  local label="$1"
  shift
  echo "=== ${label} ==="
  "$@" 2>&1 | tee "${STATUS_DIR}/${label}.log"
}

# --- Preflight --------------------------------------------------------------
run_logged preflight \
  Rscript scripts/preflight.R --scope raw --report-dir "$REPORT_DIR" \
    --inputs "Data_S1.csv,config/raster_sources.csv,config/pipeline.yml"

# --- Public rasters from their pinned sources -------------------------------
run_logged download_rasters Rscript scripts/download_rasters.R
run_logged prepare_rasters Rscript scripts/prepare_rasters.R --force --no-download

# Map the registry output names onto the filenames the analysis stages look up.
python3 - <<'PY'
from pathlib import Path
import os
import shutil

processed = Path("data/processed/rasters")
cache = Path(os.environ["PUBLIC_CACHE"])
cache.mkdir(parents=True, exist_ok=True)
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
missing = [name for name in aliases if not (processed / name).exists()]
if missing:
    raise SystemExit(
        "Raster preparation did not produce: " + ", ".join(sorted(missing))
    )
for source, destination in aliases.items():
    shutil.copy2(processed / source, cache / destination)
print(f"Aliased {len(aliases)} prepared rasters into {cache}")
PY

# WorldPop is a single published GeoTIFF used at its native grain, so it is
# fetched directly rather than through the alignment registry. Its URL,
# retrieval time, and checksum are recorded.
if [[ ! -s "${PUBLIC_CACHE}/population_count_Japan_crop.tif" ]]; then
  curl --fail --location --retry 5 --retry-all-errors \
    --connect-timeout 30 --max-time 900 \
    "$WORLDPOP_URL" \
    -o "${PUBLIC_CACHE}/population_count_Japan_crop.tif"
fi
{
  echo "source_url=${WORLDPOP_URL}"
  echo "retrieved_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "sha256=$(sha256sum "${PUBLIC_CACHE}/population_count_Japan_crop.tif" | awk '{print $1}')"
  echo "bytes=$(stat -c%s "${PUBLIC_CACHE}/population_count_Japan_crop.tif")"
} > "${STATUS_DIR}/worldpop_provenance.txt"
cat "${STATUS_DIR}/worldpop_provenance.txt"

find "$PUBLIC_CACHE" -maxdepth 1 -type f -name '*.tif' -printf '%f\n' \
  | sort > "${STATUS_DIR}/public_raster_inventory.txt"

# --- Bombus prediction surfaces ---------------------------------------------
# The repository's ENMeval reselection script rebuilds predictions from already
# fitted candidate objects, and those objects are not part of the versioned
# repository, so a fresh AICc reselection cannot run here. The reconstruction
# therefore uses the *Bombus* prediction surfaces committed at the recorded
# publication commit, and records that explicitly. These surfaces do not enter
# the environmental terms of the natural presence model or the neighbourhood
# graph used by the asymmetry diagnostic.
git fetch --no-tags origin "$HISTORICAL_PUBLICATION_COMMIT"
for species in ardens beaticola consobrinus diversus honshuensis; do
  git show "${HISTORICAL_PUBLICATION_COMMIT}:sdm/${species}.tif" \
    > "${BOMBUS_DIR}/${species}.tif"
  test -s "${BOMBUS_DIR}/${species}.tif"
done
{
  echo "mode=historical_committed_prediction_surfaces"
  echo "source_commit=${HISTORICAL_PUBLICATION_COMMIT}"
  echo "fresh_enmeval_reselection=false"
  echo "reason=fitted ENMeval candidate objects are not versioned in this repository"
  echo "affects_asymmetry_graph=false"
} > "${STATUS_DIR}/bombus_input_mode.txt"
sha256sum "${BOMBUS_DIR}"/*.tif > "${STATUS_DIR}/bombus_prediction_sha256.txt"

# --- Human-landscape rasters ------------------------------------------------
run_logged build_human_raster \
  Rscript scripts/build_human_raster.R \
    --observation-csv=Data_S1.csv \
    --cache-dir="$MLIT_CACHE" \
    --output-dir=results/public_rasters/mlit_human_forest_edge_2021

# --- Environmental input and analysis table ---------------------------------
mkdir -p results/environment_v3
run_logged build_environment_input \
  Rscript scripts/build_environment_input.R \
    --raw-colour-csv=Data_S1.csv \
    --cache-root="$PUBLIC_CACHE" \
    --output-csv=results/environment_v3/ecological_input_v2.csv

run_logged run_environment_spatial \
  Rscript scripts/run_environment_spatial.R \
    --anomaly-csv=results/environment_v3/ecological_input_v2.csv \
    --raw-colour-csv=Data_S1.csv \
    --bombus-dir="$BOMBUS_DIR" \
    --H-raster="${PUBLIC_CACHE}/population_count_Japan_crop.tif" \
    --R-raster=results/public_rasters/mlit_human_forest_edge_2021/mlit_human_forest_edge_1km.tif \
    --N-raster="${PUBLIC_CACHE}/gdd5_Japan_crop_30s.tif" \
    --A-raster=results/public_rasters/mlit_human_forest_edge_2021/mlit_major_road_distance_1km.tif \
    --output-dir=results/ecological_v9_final_public_HRNA_50km \
    --run-inla=false \
    --author-review-confirmed=true

# --- Bombus occurrences and biotic covariates -------------------------------
refresh=false
for species in ardens diversus beaticola consobrinus honshuensis; do
  [[ -s "${GBIF_CACHE}/${species}_gbif.csv" ]] || refresh=true
done
run_logged fetch_bombus_occurrences \
  Rscript scripts/fetch_bombus_occurrences.R \
    --output-dir="$GBIF_CACHE" \
    --refresh="$refresh"
echo "refresh=${refresh}" > "${STATUS_DIR}/bombus_occurrence_input_mode.txt"

run_logged run_natural_biotic_covariates \
  Rscript scripts/run_natural_biotic_covariates.R \
    --analysis-data=results/ecological_v9_final_public_HRNA_50km/analysis_data.csv \
    --occurrence-dir="$GBIF_CACHE" \
    --raw-colour-csv=Data_S1.csv \
    --output-dir=results/ecological_v10_final_mechanism_HRNA \
    --tail-bootstraps=199

# --- Two-part pigmentation models -------------------------------------------
run_logged run_phenotype_hurdle \
  Rscript scripts/run_phenotype_hurdle.R \
    --analysis-data=results/ecological_v10_final_mechanism_HRNA/analysis_data_mechanism_v3.csv \
    --output-dir=results/ecological_v11_pigmentation_hurdle \
    --run-inla=true

# --- Stage the canonical analysis-input snapshot ----------------------------
run_logged stage_canonical_snapshot \
  Rscript scripts/stage_canonical_snapshot.R \
    --observations=results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv \
    --raster-cache="$PUBLIC_CACHE" \
    --worldpop="${PUBLIC_CACHE}/population_count_Japan_crop.tif" \
    --staging="$SNAPSHOT_STAGING" \
    --snapshot-id="$SNAPSHOT_ID" \
    --descriptor=inputs/canonical_snapshot.json \
    --worldpop-url="$WORLDPOP_URL"

run_logged write_reproducibility_report \
  Rscript scripts/write_reproducibility_report.R \
    --report-dir "$REPORT_DIR" \
    --workflow raw-data-reconstruction \
    --inputs "Data_S1.csv,config/raster_sources.csv,config/pipeline.yml,data/processed/raster_download_manifest.csv,data/processed/raster_manifest.csv" \
    --outputs "${SNAPSHOT_STAGING},results/ecological_v11_pigmentation_hurdle,results/environment_v3"

echo "=== raw reconstruction complete ==="
