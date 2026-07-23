# Build the natural-environment input without filtering observations on colour,
# mask shape, multimodality, or extreme colour values.

source("scripts/ecological_analysis_v2.R")
require_packages(c("terra", "jsonlite"))

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = "") {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1L]
}

raw_csv <- arg_value("--raw-colour-csv", "Data_S1.csv")
cache_root <- arg_value("--cache-root", Sys.getenv("HOTARUBUKURO_PUBLIC_CACHE"))
output_csv <- arg_value("--output-csv", "results/ecological_input_v2.csv")
if (!dir.exists(cache_root)) stop("Public environment cache not found: ", cache_root, call. = FALSE)

find_one <- function(pattern) {
  hits <- list.files(cache_root, pattern = pattern, recursive = TRUE, full.names = TRUE)
  if (length(hits) != 1L) {
    stop("Expected one cached raster for pattern ", pattern, "; found ", length(hits), call. = FALSE)
  }
  hits
}

files <- c(
  bio5 = find_one("^bio5_Japan_crop_30s\\.tif$"),
  bio10 = find_one("^bio10_Japan_crop_30s\\.tif$"),
  gdd5 = find_one("^gdd5_Japan_crop_30s\\.tif$"),
  CMI = find_one("^CMI_Japan_crop_30s\\.tif$"),
  bio12 = find_one("^bio12_Japan_crop_30s\\.tif$"),
  bio14 = find_one("^bio14_Japan_crop_30s\\.tif$"),
  bio15 = find_one("^bio15_Japan_crop_30s\\.tif$"),
  bio4 = find_one("^bio4_Japan_crop_30s\\.tif$"),
  RSDS = find_one("^RSDS_Japan_crop_30s\\.tif$"),
  bdod = find_one("^bdod_0-5cm_mean_30s\\.tif$"),
  cfvo = find_one("^cfvo_0-5cm_mean_30s\\.tif$"),
  sand = find_one("^sand_0-5cm_mean_30s\\.tif$"),
  silt = find_one("^silt_0-5cm_mean_30s\\.tif$"),
  nitrogen = find_one("^nitrogen_0-5cm_mean_30s\\.tif$"),
  ocd = find_one("^ocd_0-5cm_mean_30s\\.tif$"),
  soc = find_one("^soc_0-5cm_mean_30s\\.tif$"),
  phh2o = find_one("^phh2o_0-5cm_mean_30s\\.tif$"),
  elevation = find_one("^elevation_Japan_crop\\.tif$")
)

raw <- utils::read.csv(raw_csv, check.names = FALSE, stringsAsFactors = FALSE)
qc <- audit_colour_qc(raw, author_review_confirmed = TRUE)
d <- qc$data[qc$data$colour_qc_primary, , drop = FALSE]
d$sample_id <- d$observation_id

pts <- terra::vect(d, geom = c("longitude", "latitude"), crs = "EPSG:4326")
for (nm in names(files)) {
  r <- terra::rast(files[[nm]])
  v <- terra::extract(r, pts, method = "bilinear")
  d[[nm]] <- as.numeric(v[, ncol(v)])
}

dem <- terra::rast(files[["elevation"]])
topo <- c(
  terra::terrain(dem, v = "slope", unit = "radians"),
  terra::terrain(dem, v = "TRI"),
  terra::terrain(dem, v = "roughness")
)
names(topo) <- c("slope", "TRI", "roughness")
topo_values <- terra::extract(topo, pts, method = "bilinear")
for (nm in names(topo)) d[[nm]] <- as.numeric(topo_values[[nm]])

pca_axis <- function(data, variables, axis_name) {
  X <- data[variables]
  keep <- vapply(X, function(x) is.numeric(x) && stats::sd(x, na.rm = TRUE) > 0, logical(1))
  X <- X[keep]
  score <- rep(NA_real_, nrow(data))
  if (ncol(X) == 1L) {
    score <- safe_z(X[[1]])
    fit <- NULL
  } else {
    cc <- stats::complete.cases(X)
    fit <- stats::prcomp(X[cc, , drop = FALSE], center = TRUE, scale. = TRUE)
    score[cc] <- fit$x[, 1]
    # Fix arbitrary PCA sign so the first named variable has positive loading.
    if (fit$rotation[1, 1] < 0) {
      score <- -score
      fit$rotation[, 1] <- -fit$rotation[, 1]
      fit$x[, 1] <- -fit$x[, 1]
    }
  }
  data[[axis_name]] <- score
  list(data = data, fit = fit, variables = names(X))
}

pca_components <- function(data, variables, axis_names) {
  X <- data[variables]
  keep <- vapply(X, function(x) is.numeric(x) && stats::sd(x, na.rm = TRUE) > 0, logical(1))
  X <- X[keep]
  cc <- stats::complete.cases(X)
  if (ncol(X) < length(axis_names) || sum(cc) < 3L) {
    stop("Too few complete soil variables for the requested joint PCA.", call. = FALSE)
  }
  fit <- stats::prcomp(X[cc, , drop = FALSE], center = TRUE, scale. = TRUE)
  for (j in seq_along(axis_names)) {
    # Fix each arbitrary component sign by making its largest absolute loading
    # positive; this changes labels, never fitted values or model comparisons.
    anchor <- which.max(abs(fit$rotation[, j]))
    if (fit$rotation[anchor, j] < 0) {
      fit$rotation[, j] <- -fit$rotation[, j]
      fit$x[, j] <- -fit$x[, j]
    }
    score <- rep(NA_real_, nrow(data))
    score[cc] <- fit$x[, j]
    data[[axis_names[j]]] <- score
  }
  list(data = data, fit = fit, variables = names(X))
}

axes <- list(
  Temperature_PC1 = c("bio5", "bio10", "gdd5"),
  precip_PC1 = c("CMI", "bio12", "bio14"),
  soil_phys_PC1 = c("bdod", "cfvo", "sand", "silt"),
  soil_nutrient_PC1 = c("nitrogen", "ocd", "soc"),
  topo_PC1 = c("roughness", "slope", "TRI")
)
pca_fits <- list()
for (axis in names(axes)) {
  ans <- pca_axis(d, axes[[axis]], axis)
  d <- ans$data
  pca_fits[[axis]] <- ans$fit
}
soil_joint <- pca_components(
  d,
  c("bdod", "cfvo", "sand", "silt", "nitrogen", "ocd", "soc", "phh2o"),
  c("soil_PC1", "soil_PC2")
)
d <- soil_joint$data
pca_fits$soil_joint <- soil_joint$fit
d$TemperatureSeasonality <- safe_z(d$bio4)
d$PrecipSeasonality <- safe_z(d$bio15)
d$soil_pH <- d$phh2o
d$Pigment <- NA_real_

keep <- unique(c(
  "sample_id", "observation_id", "exact_site_id", "col_hex", "date",
  "longitude", "latitude", "elevation", "Pigment", names(axes),
  "TemperatureSeasonality", "PrecipSeasonality", "soil_pH",
  "soil_PC1", "soil_PC2", "RSDS"
))
keep <- intersect(keep, names(d))
dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
write_csv_safe(d[keep], output_csv)
write_csv_safe(qc$audit, file.path(dirname(output_csv), "environment_input_colour_qc_audit.csv"))

loadings <- do.call(rbind, lapply(names(pca_fits), function(axis) {
  fit <- pca_fits[[axis]]
  if (is.null(fit)) return(data.frame())
  data.frame(
    axis = axis,
    variable = rownames(fit$rotation),
    PC1_loading = fit$rotation[, 1],
    PC2_loading = if (ncol(fit$rotation) >= 2L) fit$rotation[, 2] else NA_real_
  )
}))
write_csv_safe(loadings, file.path(dirname(output_csv), "environment_input_pca_loadings.csv"))

soil_variance <- summary(soil_joint$fit)$importance
write_csv_safe(data.frame(
  component = colnames(soil_variance),
  proportion_variance = as.numeric(soil_variance["Proportion of Variance", ]),
  cumulative_variance = as.numeric(soil_variance["Cumulative Proportion", ])
), file.path(dirname(output_csv), "environment_input_soil_joint_pca_variance.csv"))

manifest <- list(
  raw_input = normalizePath(raw_csv, winslash = "/", mustWork = TRUE),
  raw_input_md5 = unname(tools::md5sum(raw_csv)),
  n_raw = nrow(raw),
  n_analysis_eligible = nrow(d),
  exclusion_rule = "only non-formable colour measurement or exact duplicate image",
  colour_outliers_removed = FALSE,
  author_review_confirmed = TRUE,
  raster_files = as.list(normalizePath(files, winslash = "/", mustWork = TRUE))
)
jsonlite::write_json(manifest, file.path(dirname(output_csv), "environment_input_manifest.json"),
                     pretty = TRUE, auto_unbox = TRUE)
cat("Wrote ", nrow(d), " colour-inclusive observations to ", output_csv, "\n", sep = "")
