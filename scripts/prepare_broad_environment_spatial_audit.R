#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  if (hit[length(hit)] == length(args)) stop("Missing value after ", flag, call. = FALSE)
  args[hit[length(hit)] + 1L]
}

analysis_csv <- arg_value(
  "--analysis-csv",
  "reference-artifact/results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
)
landscape_csv <- arg_value(
  "--landscape-csv",
  "reference-artifact/results/ecological_v19_human_landscape_extremes/landscape_cell_features.csv"
)
raster_dir <- arg_value("--raster-dir", "data/processed/rasters")
output_csv <- arg_value(
  "--output-csv",
  "results/broad_environment_spatial_finalization/extended_analysis_data.csv"
)
output_dir <- dirname(output_csv)

required_packages <- c("terra", "jsonlite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}
if (!file.exists(analysis_csv)) stop("Missing analysis CSV: ", analysis_csv, call. = FALSE)
if (!file.exists(landscape_csv)) stop("Missing landscape CSV: ", landscape_csv, call. = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

safe_z <- function(x) {
  x <- as.numeric(x)
  keep <- is.finite(x)
  out <- rep(NA_real_, length(x))
  if (!any(keep)) return(out)
  sx <- stats::sd(x[keep])
  if (!is.finite(sx) || sx == 0) {
    out[keep] <- 0
  } else {
    out[keep] <- (x[keep] - mean(x[keep])) / sx
  }
  out
}

extract_one <- function(path, points, name) {
  if (!file.exists(path) || file.info(path)$size <= 0) {
    stop("Missing prepared raster for ", name, ": ", path, call. = FALSE)
  }
  raster <- terra::rast(path)
  values <- terra::extract(raster, points, method = "bilinear")
  out <- as.numeric(values[, ncol(values)])
  if (any(!is.finite(out))) {
    nearest <- terra::extract(raster, points[!is.finite(out), ], method = "simple")
    out[!is.finite(out)] <- as.numeric(nearest[, ncol(nearest)])
  }
  out
}

d <- utils::read.csv(analysis_csv, check.names = FALSE, stringsAsFactors = FALSE)
landscape <- utils::read.csv(
  landscape_csv, check.names = FALSE, stringsAsFactors = FALSE
)
needed <- c(
  "observation_id", "exact_site_id", "longitude", "latitude", "elevation",
  "env_Temperature_PC1", "env_precip_PC1", "env_TemperatureSeasonality",
  "env_PrecipSeasonality", "env_topo_PC1", "env_soil_PC1", "env_soil_PC2",
  "env_RSDS", "pigmented_mixture50", "pigment_intensity_z", "region",
  "x_km", "y_km", "spatial_fold", "spatial_block", "DOY", "year",
  "soil_phys_PC1", "soil_nutrient_PC1", "soil_pH"
)
missing <- setdiff(needed, names(d))
if (length(missing)) {
  stop("Analysis data lack columns: ", paste(missing, collapse = ", "), call. = FALSE)
}
landscape_needed <- c("exact_site_id", "forest_fraction")
landscape_missing <- setdiff(landscape_needed, names(landscape))
if (length(landscape_missing)) {
  stop("Landscape data lack columns: ", paste(landscape_missing, collapse = ", "), call. = FALSE)
}
original_order <- d$observation_id
d$cell_id_1km <- sprintf(
  "cell-1km-%d_%d", floor(as.numeric(d$x_km)), floor(as.numeric(d$y_km))
)
landscape <- landscape[!duplicated(landscape$exact_site_id), landscape_needed, drop = FALSE]
names(landscape)[names(landscape) == "exact_site_id"] <- "cell_id_1km"
d <- merge(d, landscape, by = "cell_id_1km", all.x = TRUE, sort = FALSE)
d <- d[match(original_order, d$observation_id), , drop = FALSE]
if (anyNA(d$observation_id) || !identical(d$observation_id, original_order)) {
  stop("Landscape join disturbed observation identity or order.", call. = FALSE)
}
if (any(!is.finite(d$forest_fraction))) {
  stop("Forest fraction did not map to every current 1-km observation cell.", call. = FALSE)
}

points <- terra::vect(d, geom = c("longitude", "latitude"), crs = "EPSG:4326")
raster_paths <- c(
  vpd = file.path(raster_dir, "chelsa_vpdmean.tif"),
  swb = file.path(raster_dir, "chelsa_swb.tif"),
  bio6 = file.path(raster_dir, "chelsa_bio06.tif"),
  bio13 = file.path(raster_dir, "chelsa_bio13.tif"),
  snow_cover_days = file.path(raster_dir, "chelsa_scd.tif"),
  wind = file.path(raster_dir, "chelsa_sfcWindmean.tif"),
  cec = file.path(raster_dir, "soilgrids_cec_0_5cm_mean.tif"),
  elevation = file.path(raster_dir, "elevation_30s.tif")
)
for (name in names(raster_paths)) {
  d[[paste0("audit_raw_", name)]] <- extract_one(raster_paths[[name]], points, name)
}

elevation_raster <- terra::rast(raster_paths[["elevation"]])
aspect_raster <- terra::terrain(elevation_raster, v = "aspect", unit = "radians")
tpi_raster <- terra::terrain(elevation_raster, v = "TPI")
aspect_values <- terra::extract(aspect_raster, points, method = "bilinear")
tpi_values <- terra::extract(tpi_raster, points, method = "bilinear")
d$audit_raw_aspect_radians <- as.numeric(aspect_values[, ncol(aspect_values)])
d$audit_raw_TPI <- as.numeric(tpi_values[, ncol(tpi_values)])
if (any(!is.finite(d$audit_raw_aspect_radians))) {
  nearest <- terra::extract(
    aspect_raster, points[!is.finite(d$audit_raw_aspect_radians), ], method = "simple"
  )
  d$audit_raw_aspect_radians[!is.finite(d$audit_raw_aspect_radians)] <-
    as.numeric(nearest[, ncol(nearest)])
}
if (any(!is.finite(d$audit_raw_TPI))) {
  nearest <- terra::extract(
    tpi_raster, points[!is.finite(d$audit_raw_TPI), ], method = "simple"
  )
  d$audit_raw_TPI[!is.finite(d$audit_raw_TPI)] <-
    as.numeric(nearest[, ncol(nearest)])
}

sea_source <- terra::ifel(is.na(elevation_raster), 1, NA)
coast_distance <- terra::distance(sea_source)
coast_values <- terra::extract(coast_distance, points, method = "bilinear")
d$audit_raw_coast_distance_km <- as.numeric(coast_values[, ncol(coast_values)]) / 1000

d$env_VPD <- safe_z(d$audit_raw_vpd)
d$env_SWB <- safe_z(d$audit_raw_swb)
d$env_BIO6 <- safe_z(d$audit_raw_bio6)
d$env_BIO13 <- safe_z(d$audit_raw_bio13)
d$env_snow_cover_days <- safe_z(d$audit_raw_snow_cover_days)
d$env_wind <- safe_z(d$audit_raw_wind)
d$env_CEC <- safe_z(d$audit_raw_cec)
d$env_northness <- safe_z(cos(d$audit_raw_aspect_radians))
d$env_TPI <- safe_z(d$audit_raw_TPI)
d$env_forest_fraction <- safe_z(d$forest_fraction)
d$env_log_coast_distance <- safe_z(log1p(d$audit_raw_coast_distance_km))
d$env_elevation_benchmark <- safe_z(d$elevation)
d$env_soil_physical_alt <- safe_z(d$soil_phys_PC1)
d$env_soil_nutrient_alt <- safe_z(d$soil_nutrient_PC1)
d$env_soil_pH_alt <- safe_z(d$soil_pH)
d$env_latitude_benchmark <- safe_z(d$latitude)

d$int_thermal_variability <- safe_z(
  d$env_Temperature_PC1 * d$env_TemperatureSeasonality
)
d$int_moisture_thermal_variability <- safe_z(
  d$env_precip_PC1 * d$env_TemperatureSeasonality
)
d$int_dryness_radiation <- safe_z(
  (-d$env_precip_PC1) * d$env_RSDS
)
d$int_dryness_precip_variability <- safe_z(
  (-d$env_precip_PC1) * d$env_PrecipSeasonality
)

d$guard_DOY <- safe_z(d$DOY)
d$guard_year2024 <- as.integer(d$year == 2024)
d$guard_year2025 <- as.integer(d$year == 2025)
d$guard_overexposure <- if ("possible_overexposure" %in% names(d)) {
  as.integer(tolower(as.character(d$possible_overexposure)) %in% c("1", "true", "yes"))
} else {
  0L
}
d$guard_mask_fraction <- if ("mask_fraction_visible" %in% names(d)) {
  safe_z(d$mask_fraction_visible)
} else {
  0
}

audit_variables <- c(
  "audit_raw_vpd", "audit_raw_swb", "audit_raw_bio6", "audit_raw_bio13",
  "audit_raw_snow_cover_days", "audit_raw_wind", "audit_raw_cec",
  "audit_raw_aspect_radians", "audit_raw_TPI",
  "forest_fraction", "audit_raw_coast_distance_km",
  "env_VPD", "env_SWB", "env_BIO6", "env_BIO13",
  "env_snow_cover_days", "env_wind", "env_CEC", "env_northness", "env_TPI",
  "env_forest_fraction", "env_log_coast_distance",
  "env_elevation_benchmark", "env_latitude_benchmark",
  "env_soil_physical_alt", "env_soil_nutrient_alt", "env_soil_pH_alt",
  "int_thermal_variability", "int_moisture_thermal_variability",
  "int_dryness_radiation", "int_dryness_precip_variability",
  "guard_DOY", "guard_year2024", "guard_year2025",
  "guard_overexposure", "guard_mask_fraction"
)
coverage <- data.frame(
  variable = audit_variables,
  n_finite = vapply(d[audit_variables], function(x) sum(is.finite(x)), integer(1)),
  n_missing = vapply(d[audit_variables], function(x) sum(!is.finite(x)), integer(1)),
  stringsAsFactors = FALSE
)
if (any(coverage$n_finite < nrow(d))) {
  bad <- coverage$variable[coverage$n_finite < nrow(d)]
  stop("Insufficient audit-variable coverage: ", paste(bad, collapse = ", "), call. = FALSE)
}

utils::write.csv(d, output_csv, row.names = FALSE, na = "")
utils::write.csv(
  coverage, file.path(output_dir, "extended_variable_coverage.csv"),
  row.names = FALSE, na = ""
)
utils::write.csv(
  data.frame(
    source = names(raster_paths),
    path = unname(raster_paths),
    md5 = unname(tools::md5sum(raster_paths)),
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "extended_raster_manifest.csv"),
  row.names = FALSE, na = ""
)
jsonlite::write_json(
  list(
    status = "PASS",
    n = nrow(d),
    analysis_csv = normalizePath(analysis_csv, winslash = "/", mustWork = TRUE),
    landscape_csv = normalizePath(landscape_csv, winslash = "/", mustWork = TRUE),
    output_csv = normalizePath(output_csv, winslash = "/", mustWork = TRUE),
    standardization_population = "all 1922 observations before response subsetting",
    coast_distance_definition = paste(
      "log1p distance in km to nearest sea cell on the canonical",
      "30-arc-second WorldClim elevation grid"
    ),
    forest_fraction_role = paste(
      "natural-light/habitat sensitivity only; MLIT land cover is not",
      "treated as direct canopy irradiance"
    )
  ),
  file.path(output_dir, "extended_data_manifest.json"),
  pretty = TRUE, auto_unbox = TRUE
)
cat("Wrote ", nrow(d), " extended observations to ", output_csv, "\n", sep = "")
