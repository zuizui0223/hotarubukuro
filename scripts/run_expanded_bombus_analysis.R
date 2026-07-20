#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else "scripts/run_expanded_bombus_analysis.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "analysis_core.R"))
source(file.path(repo_root, "R", "sdm.R"))
source(file.path(repo_root, "R", "reanalysis.R"))
source(file.path(repo_root, "R", "bombus_availability.R"))

if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.", call. = FALSE)

options <- list(
  input = file.path(repo_root, "results", "ci", "pr4_reanalysis", "inputs", "inclusive__primary.csv"),
  config = file.path(repo_root, "config", "reanalysis.yml"),
  raster_dir = file.path(repo_root, "data", "processed", "rasters"),
  output_dir = file.path(repo_root, "results", "ci", "expanded_bombus")
)
map <- c("--input" = "input", "--config" = "config", "--raster-dir" = "raster_dir", "--output-dir" = "output_dir")
i <- 1L
while (i <= length(args)) {
  if (!args[[i]] %in% names(map) || i == length(args)) stop("Invalid command-line arguments.", call. = FALSE)
  options[[map[[args[[i]]]]]] <- args[[i + 1L]]
  i <- i + 2L
}
for (nm in c("input", "config", "raster_dir")) options[[nm]] <- normalizePath(options[[nm]], winslash = "/", mustWork = TRUE)
if (dir.exists(options$output_dir) && length(list.files(options$output_dir, all.files = TRUE, no.. = TRUE))) stop("Output directory is not empty.", call. = FALSE)
dir.create(options$output_dir, recursive = TRUE, showWarnings = FALSE)

config <- read_reanalysis_config(options$config)
d <- utils::read.csv(options$input, stringsAsFactors = FALSE, check.names = FALSE)
d <- add_reanalysis_transforms(d, config)
d <- add_bombus_availability_indices(d)

# Derive topographic variables from the validated PR4 elevation raster.
elev_path <- file.path(options$raster_dir, "elevation_30s.tif")
if (!file.exists(elev_path)) stop("Missing elevation raster: ", elev_path, call. = FALSE)
elev <- terra::rast(elev_path)
terra::crs(elev) <- "EPSG:4326"
topo <- c(
  terra::terrain(elev, "roughness"),
  terra::terrain(elev, "slope", unit = "radians"),
  terra::terrain(elev, "TRI")
)
names(topo) <- c("roughness", "slope", "TRI")
points <- terra::vect(d, geom = c("longitude", "latitude"), crs = "EPSG:4326")
topo_values <- as.data.frame(terra::extract(topo, points, ID = FALSE), check.names = FALSE)
stopifnot(nrow(topo_values) == nrow(d))
d <- cbind(d, topo_values)

pca1 <- function(data, variables, label) {
  missing <- setdiff(variables, names(data))
  if (length(missing)) stop(label, " missing variables: ", paste(missing, collapse = ", "), call. = FALSE)
  x <- data[variables]
  usable <- vapply(x, function(v) is.numeric(v) && is.finite(stats::sd(v, na.rm = TRUE)) && stats::sd(v, na.rm = TRUE) > 0, logical(1))
  x <- x[usable]
  complete <- stats::complete.cases(x)
  if (ncol(x) < 2L || sum(complete) < 50L) stop("Invalid PCA group: ", label, call. = FALSE)
  fit <- stats::prcomp(x[complete, , drop = FALSE], center = TRUE, scale. = TRUE)
  score <- rep(NA_real_, nrow(data))
  score[complete] <- fit$x[, 1L]
  list(
    score = score,
    loadings = data.frame(
      group = label,
      variable = rownames(fit$rotation),
      PC1_loading = fit$rotation[, 1L],
      complete_rows = sum(complete),
      stringsAsFactors = FALSE
    )
  )
}

pc_temp <- pca1(d, c("chelsa_bio05", "chelsa_bio10", "chelsa_gdd5"), "Temperature_PC1")
pc_moist <- pca1(d, c("chelsa_cmimean", "chelsa_vpdmean", "chelsa_bio12", "chelsa_bio14", "chelsa_bio15", "chelsa_swb"), "precip_PC1")
pc_phys <- pca1(d, c("soilgrids_bdod_0_5cm", "soilgrids_cfvo_0_5cm", "soilgrids_sand_0_5cm", "soilgrids_silt_0_5cm"), "soil_phys_PC1")
pc_nutr <- pca1(d, c("soilgrids_nitrogen_0_5cm", "soilgrids_ocd_0_5cm", "soilgrids_soc_0_5cm"), "soil_nutrient_PC1")
pc_topo <- pca1(d, c("roughness", "slope", "TRI"), "topo_PC1")

d$Temperature_PC1 <- pc_temp$score
d$precip_PC1 <- pc_moist$score
d$soil_phys_PC1 <- pc_phys$score
d$soil_nutrient_PC1 <- pc_nutr$score
d$topo_PC1 <- pc_topo$score
d$soil_pH <- d$soilgrids_phh2o_0_5cm
d$RSDS <- d$chelsa_rsdsmean
if (!"DOY" %in% names(d)) {
  date_name <- if ("date" %in% names(d)) "date" else stop("A date column is required to derive DOY.", call. = FALSE)
  d$DOY <- as.integer(format(as.Date(d[[date_name]]), "%j"))
}

outcome <- as.character(config$primary_outcome)
environment <- c("DOY", "topo_PC1", "Temperature_PC1", "precip_PC1", "soil_nutrient_PC1", "soil_phys_PC1", "soil_pH", "RSDS")
availability <- bombus_availability_predictors()
required <- unique(c("analysis_id", "longitude", "latitude", outcome, environment, availability))
missing <- setdiff(required, names(d))
if (length(missing)) stop("Expanded analysis is missing: ", paste(missing, collapse = ", "), call. = FALSE)

common <- finite_complete_rows(d, c(outcome, environment, availability))
analysis <- d[common, , drop = FALSE]
if (nrow(analysis) < 50L) stop("Too few common complete rows.", call. = FALSE)
folds <- make_spatial_block_folds(analysis, folds = config$spatial_block_cv$folds)
analysis$spatial_fold <- folds$spatial_fold[match(analysis$analysis_id, folds$analysis_id)]

variants <- c(environment_only = NA_character_, setNames(availability, availability))
coefficient_rows <- list()
performance_rows <- list()
fold_rows <- list()
for (variant in names(variants)) {
  bombus_term <- variants[[variant]]
  predictors <- if (is.na(bombus_term)) environment else c(environment, bombus_term)
  fit <- fit_standardized_association(analysis, outcome, predictors)
  cv <- blocked_cv_association(
    analysis,
    outcome,
    predictors,
    configured_folds = seq_len(as.integer(config$spatial_block_cv$folds))
  )
  meta <- list(
    model_variant = variant,
    bombus_index = if (is.na(bombus_term)) "none" else bombus_term,
    records_common = nrow(analysis)
  )
  add_meta <- function(x) {
    for (nm in names(meta)) x[[nm]] <- meta[[nm]]
    x
  }
  coefficient_rows[[variant]] <- add_meta(fit$coefficients)
  cv_performance <- cv$performance
  names(cv_performance)[names(cv_performance) == "records_complete"] <- "records_complete_cv"
  performance_rows[[variant]] <- add_meta(cbind(fit$performance, cv_performance))
  fold_rows[[variant]] <- add_meta(cv$fold_performance)
}

coefficients <- do.call(rbind, coefficient_rows)
performance <- do.call(rbind, performance_rows)
performance_by_fold <- do.call(rbind, fold_rows)
baseline <- performance$q_squared_cv[performance$model_variant == "environment_only"]
performance$delta_q_squared_vs_environment <- performance$q_squared_cv - baseline
performance <- performance[order(-performance$q_squared_cv), , drop = FALSE]
loadings <- do.call(rbind, lapply(list(pc_temp, pc_moist, pc_phys, pc_nutr, pc_topo), `[[`, "loadings"))

utils::write.csv(coefficients, file.path(options$output_dir, "coefficients.csv"), row.names = FALSE, na = "")
utils::write.csv(performance, file.path(options$output_dir, "performance.csv"), row.names = FALSE, na = "")
utils::write.csv(performance_by_fold, file.path(options$output_dir, "performance_by_fold.csv"), row.names = FALSE, na = "")
utils::write.csv(loadings, file.path(options$output_dir, "environment_pca_loadings.csv"), row.names = FALSE, na = "")
utils::write.csv(analysis, file.path(options$output_dir, "analysis_data.csv"), row.names = FALSE, na = "")
utils::write.csv(data.frame(stage = c("input", "common_complete"), n = c(nrow(d), nrow(analysis))), file.path(options$output_dir, "row_flow.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(options$output_dir, "sessionInfo.txt"))
print(performance[c("model_variant", "records_common", "q_squared_cv", "delta_q_squared_vs_environment", "rmse", "mae")], row.names = FALSE)
