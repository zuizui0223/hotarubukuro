#!/usr/bin/env bash
set -euo pipefail

: "${PUBLIC_CACHE:?PUBLIC_CACHE is required}"
: "${RASTER_ARTIFACT:?RASTER_ARTIFACT is required}"
: "${MLIT_CACHE:?MLIT_CACHE is required}"
: "${GBIF_CACHE:?GBIF_CACHE is required}"
: "${BOMBUS_DIR:?BOMBUS_DIR is required}"
: "${STATUS_DIR:?STATUS_DIR is required}"
: "${HISTORICAL_PUBLICATION_COMMIT:?HISTORICAL_PUBLICATION_COMMIT is required}"

mkdir -p \
  "$STATUS_DIR" \
  "$PUBLIC_CACHE" \
  "$MLIT_CACHE" \
  "$GBIF_CACHE" \
  "$BOMBUS_DIR" \
  reproduction_inputs/worldpop

run_logged() {
  local label="$1"
  shift
  echo "=== ${label} ==="
  set -o pipefail
  "$@" 2>&1 | tee "$STATUS_DIR/${label}.log"
}

run_optional() {
  local label="$1"
  shift
  echo "=== optional: ${label} ==="
  set +e
  "$@" > "$STATUS_DIR/${label}.log" 2>&1
  local status=$?
  set -e
  printf '%s\t%s\n' "$label" "$status" \
    >> "$STATUS_DIR/downstream_stage_status.tsv"
  cat "$STATUS_DIR/${label}.log"
  return 0
}

prepare_public_rasters() {
  find "$RASTER_ARTIFACT" -type f -name '*.tif' \
    -exec cp -f '{}' "$PUBLIC_CACHE/" ';'
  find "$RASTER_ARTIFACT" -type f \
    \( -name 'raster_manifest.csv' -o -name 'raster_download_manifest.csv' \) \
    -exec cp -f '{}' "$PUBLIC_CACHE/" ';'

  python3 - <<'PY'
from pathlib import Path
import shutil
import os

root = Path(os.environ["PUBLIC_CACHE"])
aliases = {
    "bio5_Japan_crop_30s.tif": "chelsa_bio05.tif",
    "bio10_Japan_crop_30s.tif": "chelsa_bio10.tif",
    "gdd5_Japan_crop_30s.tif": "chelsa_gdd5.tif",
    "CMI_Japan_crop_30s.tif": "chelsa_cmimean.tif",
    "bio12_Japan_crop_30s.tif": "chelsa_bio12.tif",
    "bio14_Japan_crop_30s.tif": "chelsa_bio14.tif",
    "bio15_Japan_crop_30s.tif": "chelsa_bio15.tif",
    "RSDS_Japan_crop_30s.tif": "chelsa_rsdsmean.tif",
    "bdod_0-5cm_mean_30s.tif": "soilgrids_bdod_0_5cm_mean.tif",
    "cfvo_0-5cm_mean_30s.tif": "soilgrids_cfvo_0_5cm_mean.tif",
    "sand_0-5cm_mean_30s.tif": "soilgrids_sand_0_5cm_mean.tif",
    "silt_0-5cm_mean_30s.tif": "soilgrids_silt_0_5cm_mean.tif",
    "nitrogen_0-5cm_mean_30s.tif": "soilgrids_nitrogen_0_5cm_mean.tif",
    "ocd_0-5cm_mean_30s.tif": "soilgrids_ocd_0_5cm_mean.tif",
    "soc_0-5cm_mean_30s.tif": "soilgrids_soc_0_5cm_mean.tif",
    "phh2o_0-5cm_mean_30s.tif": "soilgrids_phh2o_0_5cm_mean.tif",
    "elevation_Japan_crop.tif": "elevation_30s.tif",
}
missing = []
for destination, source in aliases.items():
    source_path = root / source
    if not source_path.exists():
        missing.append(str(source_path))
    else:
        shutil.copy2(source_path, root / destination)
if missing:
    raise SystemExit("Missing retained raster inputs: " + ", ".join(missing))
PY

  if [[ ! -s "$PUBLIC_CACHE/bio4_Japan_crop_30s.tif" ]]; then
    local bio4_url
    bio4_url="https://os.unil.cloud.switch.ch/chelsa02/chelsa/global/bioclim/bio04/1981-2010/CHELSA_bio04_1981-2010_V.2.1.tif"
    gdal_translate -q \
      --config GDAL_HTTP_MAX_RETRY 5 \
      --config GDAL_HTTP_RETRY_DELAY 5 \
      -projwin 120 50 150 20 \
      -of GTiff -co COMPRESS=DEFLATE -co TILED=YES \
      "/vsicurl/${bio4_url}" \
      "$PUBLIC_CACHE/bio4_Japan_crop_30s.tif"
  fi

  if [[ ! -s "$PUBLIC_CACHE/population_count_Japan_crop.tif" ]]; then
    local population_url
    population_url="https://data.worldpop.org/GIS/Population/Global_2000_2020_1km/2020/JPN/jpn_ppp_2020_1km_Aggregated.tif"
    curl --fail --location --retry 5 --retry-all-errors \
      --connect-timeout 30 --max-time 600 \
      "$population_url" \
      -o "$PUBLIC_CACHE/population_count_Japan_crop.tif"
  fi

  test -s "$PUBLIC_CACHE/bio4_Japan_crop_30s.tif"
  test -s "$PUBLIC_CACHE/population_count_Japan_crop.tif"
  gdalinfo -json "$PUBLIC_CACHE/population_count_Japan_crop.tif" \
    > "$STATUS_DIR/worldpop_gdalinfo.json"
  find "$PUBLIC_CACHE" -maxdepth 1 -type f -printf '%f\t%s\n' \
    | sort > "$STATUS_DIR/public_raster_inventory.tsv"
  {
    echo "raster_source=retained_actions_artifact"
    echo "population_source=WorldPop_Global_2000_2020_1km_adjusted_2020_JPN"
    echo "population_url=https://data.worldpop.org/GIS/Population/Global_2000_2020_1km/2020/JPN/jpn_ppp_2020_1km_Aggregated.tif"
  } > "$STATUS_DIR/public_input_mode.txt"
}

recover_bombus_surfaces() {
  mkdir -p "$BOMBUS_DIR" "$(dirname "$BOMBUS_DIR")"
  git fetch --no-tags origin "$HISTORICAL_PUBLICATION_COMMIT"
  local species
  for species in ardens beaticola consobrinus diversus honshuensis; do
    git show "$HISTORICAL_PUBLICATION_COMMIT:sdm/${species}.tif" \
      > "$BOMBUS_DIR/${species}.tif"
    test -s "$BOMBUS_DIR/${species}.tif"
  done
  cat > "$(dirname "$BOMBUS_DIR")/ENMeval_AICc_selection.csv" <<'CSV'
species,selected_row,selection_rule,reconstruction_role
ardens,NA,historical_committed_prediction,legacy_surface_fallback
beaticola,NA,historical_committed_prediction,legacy_surface_fallback
consobrinus,NA,historical_committed_prediction,legacy_surface_fallback
diversus,NA,historical_committed_prediction,legacy_surface_fallback
honshuensis,NA,historical_committed_prediction,legacy_surface_fallback
CSV
  {
    echo "mode=legacy_committed_prediction_fallback"
    echo "source_commit=$HISTORICAL_PUBLICATION_COMMIT"
    echo "strict_current_enmeval_reselection=false"
    echo "reason=fitted ENMeval candidate objects are unavailable"
  } > "$STATUS_DIR/bombus_input_mode.txt"
  sha256sum "$BOMBUS_DIR"/*.tif \
    > "$STATUS_DIR/bombus_prediction_sha256.txt"
}

run_logged prepare_public_rasters prepare_public_rasters
run_logged recover_bombus_surfaces recover_bombus_surfaces

run_logged build_human_raster \
  Rscript scripts/build_human_raster.R \
    --observation-csv=Data_S1.csv \
    --cache-dir="$MLIT_CACHE" \
    --output-dir=results/public_rasters/mlit_human_forest_edge_2021

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
    --H-raster="$PUBLIC_CACHE/population_count_Japan_crop.tif" \
    --R-raster=results/public_rasters/mlit_human_forest_edge_2021/mlit_human_forest_edge_1km.tif \
    --N-raster="$PUBLIC_CACHE/gdd5_Japan_crop_30s.tif" \
    --A-raster=results/public_rasters/mlit_human_forest_edge_2021/mlit_major_road_distance_1km.tif \
    --output-dir=results/ecological_v9_final_public_HRNA_50km \
    --run-inla=false \
    --author-review-confirmed=true

refresh=false
for species in ardens diversus beaticola consobrinus honshuensis; do
  [[ -s "$GBIF_CACHE/${species}_gbif.csv" ]] || refresh=true
done
run_logged fetch_bombus_occurrences \
  Rscript scripts/fetch_bombus_occurrences.R \
    --output-dir="$GBIF_CACHE" \
    --refresh="$refresh"
echo "refresh=$refresh" > "$STATUS_DIR/bombus_occurrence_input_mode.txt"

run_logged run_natural_biotic_covariates \
  Rscript scripts/run_natural_biotic_covariates.R \
    --analysis-data=results/ecological_v9_final_public_HRNA_50km/analysis_data.csv \
    --occurrence-dir="$GBIF_CACHE" \
    --raw-colour-csv=Data_S1.csv \
    --output-dir=results/ecological_v10_final_mechanism_HRNA \
    --tail-bootstraps=199

run_logged run_phenotype_hurdle \
  Rscript scripts/run_phenotype_hurdle.R \
    --analysis-data=results/ecological_v10_final_mechanism_HRNA/analysis_data_mechanism_v3.csv \
    --output-dir=results/ecological_v11_pigmentation_hurdle \
    --run-inla=true

run_logged run_multiscale_hotspots \
  Rscript scripts/run_multiscale_hotspots.R \
    --input=results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv \
    --output-dir=results/ecological_v15_multiscale_hotspots \
    --environment-cache="$PUBLIC_CACHE" \
    --worldpop-raster="$PUBLIC_CACHE/population_count_Japan_crop.tif" \
    --bootstrap=1000

run_logged run_natural_predictive_model \
  Rscript scripts/run_natural_predictive_model.R \
    --observations=results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv \
    --cells=results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv \
    --output=results/ecological_v16_predictive_replication \
    --draws=1000 \
    --force=true \
    --seed=20260725

for audit in \
  validation/audit_phenotype.R \
  validation/audit_multiscale_hotspots.R \
  validation/validate_natural_predictive_model.R \
  validation/audit_natural_predictive_model.R; do
  label="$(basename "$audit" .R)"
  run_optional "$label" Rscript "$audit"
done

run_optional local_bombus_turnover Rscript scripts/run_local_bombus_turnover.R
run_optional human_landscape_features Rscript scripts/run_human_landscape_features.R
run_optional local_pigmented_isolates Rscript scripts/run_local_pigmented_isolates.R
run_optional local_human_context Rscript scripts/run_local_human_context.R

run_logged run_local_state_asymmetry \
  Rscript scripts/run_local_state_asymmetry.R \
    --cells=results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv \
    --presence-checkpoint=results/ecological_v16_predictive_replication/checkpoints/national_environment_spde_presence_draws1000.rds \
    --max-draws=1000 \
    --output=results/ecological_v23_local_state_asymmetry

run_logged validate_local_state_asymmetry \
  Rscript validation/validate_local_state_asymmetry.R \
    results/ecological_v23_local_state_asymmetry

Rscript - <<'RS' | tee "$STATUS_DIR/asymmetry_primary_results.txt"
x <- read.csv(
  "results/ecological_v23_local_state_asymmetry/local_state_asymmetry_summary.csv",
  check.names = FALSE
)
wanted <- x$metric %in% c(
  "pigmented_in_white_count", "white_in_pigmented_count",
  "pigmented_in_white_rate", "white_in_pigmented_rate",
  "log_rate_ratio"
)
columns <- intersect(c(
  "state_rule", "metric", "observed_value", "null_mean",
  "lower_95", "upper_95", "upper_p", "lower_p",
  "two_sided_p", "percentile", "n_natural_maps"
), names(x))
print(x[wanted, columns, drop = FALSE], row.names = FALSE)
RS

find results -maxdepth 4 -type f -printf '%p\t%s\n' \
  | sort > "$STATUS_DIR/result_inventory.tsv"
sha256sum \
  Data_S1.csv \
  results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv \
  results/ecological_v16_predictive_replication/checkpoints/national_environment_spde_presence_draws1000.rds \
  results/ecological_v23_local_state_asymmetry/local_state_asymmetry_summary.csv \
  > "$STATUS_DIR/core_output_sha256.txt"
