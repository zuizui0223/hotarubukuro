#!/usr/bin/env Rscript

# Minimal, reproducible publication analysis for the rebuilt flower-colour data.
#
# Run from the repository root in RStudio:
#   source("Code_S3.R")
#
# Optional overrides before sourcing:
#   input_csv <- "path/to/new_data.csv"
#   output_dir <- "results/publication_minimal"
#   force_raster_download <- FALSE
#
# The script intentionally keeps only the reviewer-facing analysis:
#   1. normalize the rebuilt data columns;
#   2. derive the reviewed colour variables and CFA pigment score;
#   3. download and prepare public environmental rasters;
#   4. use summed Bombus suitability as potential availability (primary);
#   5. fit one integrated SPDE-INLA model plus distribution-type sensitivities;
#   6. fit the anthropogenic upper-tail qGAM to natural-model residuals.

options(stringsAsFactors = FALSE)
set.seed(42)

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
required_repo_files <- c(
  "R/pigmentation_workflow.R",
  "scripts/download_rasters.R",
  "scripts/prepare_rasters.R",
  "scripts/download_human_predictors.R",
  "analysis/run_publication_workflow.R"
)
missing_repo_files <- required_repo_files[!file.exists(required_repo_files)]
if (length(missing_repo_files)) {
  stop(
    "Open the repository root in RStudio before running Code_S3.R. Missing: ",
    paste(missing_repo_files, collapse = ", "),
    call. = FALSE
  )
}

input_csv <- if (exists("input_csv", inherits = FALSE)) input_csv else "Data_S1.csv"
output_dir <- if (exists("output_dir", inherits = FALSE)) output_dir else "results/publication_minimal"
force_raster_download <- if (exists("force_raster_download", inherits = FALSE)) {
  isTRUE(force_raster_download)
} else {
  FALSE
}

processed_dir <- "data/processed"
raster_dir <- file.path(processed_dir, "rasters")
human_dir <- "data/cache/human"
sdm_dir <- "sdm"
normalized_csv <- file.path(processed_dir, "Data_S1_Code_S3.csv")
log_dir <- file.path(output_dir, "logs")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

message_step <- function(...) {
  cat("\n=== ", paste0(..., collapse = ""), " ===\n", sep = "")
}

run_rscript <- function(script, args = character(), log_name) {
  log_path <- file.path(log_dir, log_name)
  command_args <- c(script, args)
  status <- system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = command_args,
    stdout = log_path,
    stderr = log_path
  )
  if (!identical(status, 0L)) {
    log_text <- if (file.exists(log_path)) readLines(log_path, warn = FALSE) else character()
    tail_text <- tail(log_text, 120)
    cat(paste(tail_text, collapse = "\n"), "\n")
    stop("Command failed: Rscript ", paste(command_args, collapse = " "), call. = FALSE)
  }
  invisible(log_path)
}

message_step("1/7 Install and validate packages")
cran_repo <- c(CRAN = "https://cloud.r-project.org")
options(repos = cran_repo)

base_packages <- c(
  "terra", "sf", "lavaan", "yaml", "geodata", "digest", "jsonlite"
)
missing_packages <- base_packages[
  !vapply(base_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  install.packages(missing_packages, dependencies = c("Depends", "Imports", "LinkingTo"))
}

# qgam >= 2 requires a recent mgcv; merely finding the bundled mgcv is not enough.
if (!requireNamespace("mgcv", quietly = TRUE) || packageVersion("mgcv") < "1.9.0") {
  install.packages("mgcv", dependencies = c("Depends", "Imports", "LinkingTo"))
}
if (!requireNamespace("qgam", quietly = TRUE)) {
  install.packages("qgam", dependencies = c("Depends", "Imports", "LinkingTo"))
}
if (!requireNamespace("INLA", quietly = TRUE)) {
  install.packages(
    "INLA",
    repos = c(cran_repo, INLA = "https://inla.r-inla-download.org/R/stable"),
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
}

required_packages <- c(base_packages, "mgcv", "qgam", "INLA")
package_ok <- vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
if (!all(package_ok)) {
  stop("Could not load: ", paste(names(package_ok)[!package_ok], collapse = ", "), call. = FALSE)
}
stopifnot(packageVersion("mgcv") >= "1.9.0")

message_step("2/7 Normalize rebuilt data columns")
if (!file.exists(input_csv)) stop("Input CSV not found: ", input_csv, call. = FALSE)
raw <- read.csv(input_csv, check.names = FALSE, fileEncoding = "UTF-8-BOM")

# Canonical name -> accepted aliases in old and rebuilt tables.
aliases <- list(
  observation_id = c("observation_id", "sample_id", "id", "ID"),
  date = c("date", "Date", "photo_date", "撮影日"),
  latitude = c("latitude", "Latitude", "lat", "緯度"),
  longitude = c("longitude", "Longitude", "lon", "lng", "経度"),
  R = c("R", "red", "mean_R", "rgb_r"),
  G = c("G", "green", "mean_G", "rgb_g"),
  B = c("B", "blue", "mean_B", "rgb_b")
)

resolve_column <- function(canonical, candidates, data_names) {
  hit <- candidates[candidates %in% data_names]
  if (!length(hit)) return(NA_character_)
  hit[[1]]
}

mapping <- vapply(
  names(aliases),
  function(nm) resolve_column(nm, aliases[[nm]], names(raw)),
  character(1)
)
missing_core <- names(mapping)[is.na(mapping)]
if (length(missing_core)) {
  stop(
    "The rebuilt CSV is missing required fields: ",
    paste(missing_core, collapse = ", "),
    ". Edit only the aliases list near the top of Code_S3.R if your headings differ.",
    call. = FALSE
  )
}

normalized <- raw
for (canonical in names(mapping)) {
  source_name <- mapping[[canonical]]
  if (source_name != canonical) normalized[[canonical]] <- raw[[source_name]]
}
if (anyDuplicated(normalized$observation_id)) {
  stop("observation_id is not unique after column mapping.", call. = FALSE)
}
for (nm in c("latitude", "longitude", "R", "G", "B")) {
  normalized[[nm]] <- suppressWarnings(as.numeric(normalized[[nm]]))
}
if (any(!is.finite(normalized$longitude) | !is.finite(normalized$latitude))) {
  stop("Non-finite longitude/latitude values remain after column mapping.", call. = FALSE)
}
write.csv(normalized, normalized_csv, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(
  data.frame(canonical = names(mapping), source_column = unname(mapping)),
  file.path(processed_dir, "Code_S3_column_mapping.csv"),
  row.names = FALSE
)
cat("Mapped ", nrow(normalized), " observations to canonical columns.\n", sep = "")

message_step("3/7 Download and prepare public environmental rasters")
required_rasters <- c(
  "chelsa_bio05.tif", "chelsa_bio10.tif", "chelsa_gdd5.tif",
  "chelsa_cmimean.tif", "chelsa_vpdmean.tif", "chelsa_bio12.tif",
  "chelsa_bio14.tif", "chelsa_bio15.tif", "chelsa_swb.tif",
  "chelsa_rsdsmean.tif", "soilgrids_bdod_0_5cm_mean.tif",
  "soilgrids_cfvo_0_5cm_mean.tif", "soilgrids_sand_0_5cm_mean.tif",
  "soilgrids_silt_0_5cm_mean.tif", "soilgrids_nitrogen_0_5cm_mean.tif",
  "soilgrids_ocd_0_5cm_mean.tif", "soilgrids_soc_0_5cm_mean.tif",
  "soilgrids_phh2o_0_5cm_mean.tif", "elevation_30s.tif"
)
raster_paths <- file.path(raster_dir, required_rasters)
rasters_ready <- all(file.exists(raster_paths) & file.info(raster_paths)$size > 0)
if (!rasters_ready || force_raster_download) {
  download_args <- if (force_raster_download) "--force" else character()
  run_rscript("scripts/download_rasters.R", download_args, "01_download_rasters.log")
  run_rscript(
    "scripts/prepare_rasters.R",
    c("--force", "--no-download"),
    "02_prepare_rasters.log"
  )
}
if (!all(file.exists(raster_paths) & file.info(raster_paths)$size > 0)) {
  stop("The complete 19-layer public raster set was not prepared.", call. = FALSE)
}

message_step("4/7 Validate environmental and Bombus extraction")
if (!dir.exists(sdm_dir)) stop("Bombus SDM directory not found: ", sdm_dir, call. = FALSE)
required_bees <- file.path(
  sdm_dir,
  paste0(c("ardens", "beaticola", "consobrinus", "diversus", "honshuensis"), ".tif")
)
if (!all(file.exists(required_bees))) {
  stop("Missing Bombus suitability raster(s): ", paste(required_bees[!file.exists(required_bees)], collapse = ", "), call. = FALSE)
}
run_rscript(
  "analysis/diagnose_all_extraction.R",
  c(normalized_csv, raster_dir, sdm_dir, file.path(output_dir, "extraction_diagnostic")),
  "03_validate_extraction.log"
)

message_step("5/7 Download public anthropogenic predictors")
run_rscript(
  "scripts/download_human_predictors.R",
  human_dir,
  "04_download_human_predictors.log"
)

message_step("6/7 Run the minimal publication model")
# The reviewed implementation uses:
# - CFA on a, -L, and chroma;
# - summed Bombus suitability as the primary potential-availability predictor;
# - one integrated natural SPDE-INLA model;
# - widespread and montane Bombus sensitivity models;
# - an upper-tail qGAM on natural-model residuals for anthropogenic context.
run_rscript(
  "analysis/run_publication_workflow.R",
  c(normalized_csv, raster_dir, sdm_dir, human_dir, output_dir),
  "05_publication_analysis.log"
)

message_step("7/7 Validate reviewer-facing outputs")
required_outputs <- file.path(
  output_dir,
  c(
    "cfa_standardized_loadings.csv",
    "environment_pca_loadings.csv",
    "bombus_environment_correlations.csv",
    "integrated_spde_model_diagnostics.csv",
    "integrated_spde_fixed_effects.csv",
    "integrated_spde_hyperparameters.csv",
    "qgam_model_summary.csv",
    "anthropogenic_outlier_review.csv",
    "analysis_data_with_predictions.csv",
    "sessionInfo.txt"
  )
)
missing_outputs <- required_outputs[
  !file.exists(required_outputs) | file.info(required_outputs)$size <= 0
]
if (length(missing_outputs)) {
  stop("Missing/empty final output(s): ", paste(missing_outputs, collapse = ", "), call. = FALSE)
}

cat(
  "\nCompleted. Main results are in:\n  ",
  normalizePath(output_dir, winslash = "/", mustWork = TRUE),
  "\n\nPrimary model: summed Bombus suitability as potential availability in the integrated SPDE-INLA model.\n",
  "Sensitivity models: widespread and montane Bombus groups.\n",
  "Anthropogenic qGAM: interpretation of upper-tail residuals only, not a second causal model.\n",
  sep = ""
)
