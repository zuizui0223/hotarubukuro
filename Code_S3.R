#!/usr/bin/env Rscript

# Minimal local publication analysis for the rebuilt flower-colour data.
#
# RStudio usage:
#   1. Open the hotarubukuro repository folder.
#   2. Run: source("Code_S3.R")
#
# Default input:
#   /Users/rachelzhang/Desktop/Data_S1.csv
#
# Optional overrides before sourcing:
#   input_csv <- "/path/to/Data_S1.csv"
#   output_dir <- "results/publication_minimal"
#   force_raster_download <- FALSE
#
# Reviewer-facing scope only:
#   - canonicalise the rebuilt CSV columns;
#   - derive the reviewed colour/CFA pigment response;
#   - obtain environmental and anthropogenic rasters from public sources;
#   - use summed Bombus suitability as potential availability in the primary model;
#   - fit an integrated SPDE-INLA model and two Bombus sensitivity models;
#   - use qGAM only to describe the upper tail of natural-model residuals.

options(stringsAsFactors = FALSE)
set.seed(42)

find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    markers <- c(
      file.path(current, "R", "pigmentation_workflow.R"),
      file.path(current, "analysis", "run_publication_workflow.R"),
      file.path(current, "scripts", "download_rasters.R")
    )
    if (all(file.exists(markers))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  stop(
    "Could not locate the hotarubukuro repository. Open its folder in RStudio, then run source(\"Code_S3.R\").",
    call. = FALSE
  )
}

repo_root <- find_repo_root()
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(repo_root)

input_csv <- if (exists("input_csv", inherits = FALSE)) {
  path.expand(input_csv)
} else {
  "/Users/rachelzhang/Desktop/Data_S1.csv"
}
output_dir <- if (exists("output_dir", inherits = FALSE)) {
  output_dir
} else {
  "results/publication_minimal"
}
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
  status <- system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c(script, args),
    stdout = log_path,
    stderr = log_path
  )
  if (!identical(status, 0L)) {
    log_text <- if (file.exists(log_path)) readLines(log_path, warn = FALSE) else character()
    cat(paste(tail(log_text, 150), collapse = "\n"), "\n")
    stop(
      "Analysis step failed. Full log: ", normalizePath(log_path, winslash = "/", mustWork = FALSE),
      call. = FALSE
    )
  }
  invisible(log_path)
}

message_step("1/7 Validate packages")
cran_repo <- c(CRAN = "https://cloud.r-project.org")
options(repos = cran_repo)
base_packages <- c("terra", "sf", "lavaan", "yaml", "geodata", "digest", "jsonlite")
missing_packages <- base_packages[!vapply(base_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  install.packages(missing_packages, dependencies = c("Depends", "Imports", "LinkingTo"))
}
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

message_step("2/7 Read and canonicalise rebuilt CSV")
if (!file.exists(input_csv)) stop("Input CSV not found: ", input_csv, call. = FALSE)
raw <- read.csv(input_csv, check.names = FALSE, fileEncoding = "UTF-8-BOM")

aliases <- list(
  observation_id = c("observation_id", "sample_id", "id", "ID", "source_row"),
  date = c("date", "Date", "photo_date", "撮影日"),
  latitude = c("latitude", "Latitude", "lat", "緯度"),
  longitude = c("longitude", "Longitude", "lon", "lng", "経度"),
  R = c("R", "red", "mean_R", "rgb_r"),
  G = c("G", "green", "mean_G", "rgb_g"),
  B = c("B", "blue", "mean_B", "rgb_b")
)
resolve_column <- function(candidates, data_names) {
  hit <- candidates[candidates %in% data_names]
  if (length(hit)) hit[[1]] else NA_character_
}
mapping <- vapply(aliases, resolve_column, character(1), data_names = names(raw))
missing_core <- names(mapping)[is.na(mapping)]
if (length(missing_core)) {
  stop(
    "Required column(s) not found: ", paste(missing_core, collapse = ", "),
    "\nAvailable columns: ", paste(names(raw), collapse = ", "),
    call. = FALSE
  )
}
normalized <- raw
for (canonical in names(mapping)) normalized[[canonical]] <- raw[[mapping[[canonical]]]]
if (anyDuplicated(normalized$observation_id)) stop("observation_id must be unique.", call. = FALSE)
for (nm in c("latitude", "longitude", "R", "G", "B")) {
  normalized[[nm]] <- suppressWarnings(as.numeric(normalized[[nm]]))
}
normalized$date <- as.Date(gsub("/", "-", trimws(as.character(normalized$date))))
core_valid <- complete.cases(normalized[c("observation_id", "date", "latitude", "longitude", "R", "G", "B")]) &
  is.finite(normalized$latitude) & is.finite(normalized$longitude)
if (!all(core_valid)) {
  write.csv(normalized[!core_valid, , drop = FALSE], file.path(processed_dir, "Code_S3_excluded_invalid_rows.csv"), row.names = FALSE)
  normalized <- normalized[core_valid, , drop = FALSE]
}
if (!nrow(normalized)) stop("No valid records remain after canonicalisation.", call. = FALSE)
write.csv(normalized, normalized_csv, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(
  data.frame(canonical = names(mapping), source_column = unname(mapping)),
  file.path(processed_dir, "Code_S3_column_mapping.csv"),
  row.names = FALSE
)
cat("Input: ", input_csv, "\nValid observations: ", nrow(normalized), "\n", sep = "")

message_step("3/7 Obtain and prepare public environmental rasters")
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
  run_rscript(
    "scripts/download_rasters.R",
    if (force_raster_download) "--force" else character(),
    "01_download_rasters.log"
  )
  run_rscript("scripts/prepare_rasters.R", c("--force", "--no-download"), "02_prepare_rasters.log")
}
if (!all(file.exists(raster_paths) & file.info(raster_paths)$size > 0)) {
  stop("The complete public environmental raster set was not prepared.", call. = FALSE)
}

message_step("4/7 Validate environmental and Bombus rasters")
required_bees <- file.path(
  sdm_dir,
  paste0(c("ardens", "beaticola", "consobrinus", "diversus", "honshuensis"), ".tif")
)
if (!all(file.exists(required_bees))) {
  stop(
    "Missing Bombus suitability raster(s): ",
    paste(required_bees[!file.exists(required_bees)], collapse = ", "),
    call. = FALSE
  )
}
run_rscript(
  "analysis/diagnose_all_extraction.R",
  c(normalized_csv, raster_dir, sdm_dir, file.path(output_dir, "extraction_diagnostic")),
  "03_validate_extraction.log"
)

message_step("5/7 Obtain public anthropogenic predictors")
run_rscript("scripts/download_human_predictors.R", human_dir, "04_download_human_predictors.log")

message_step("6/7 Fit minimal publication models")
run_rscript(
  "analysis/run_publication_workflow.R",
  c(normalized_csv, raster_dir, sdm_dir, human_dir, output_dir),
  "05_publication_analysis.log"
)

message_step("7/7 Validate outputs")
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
  stop("Missing or empty output(s): ", paste(missing_outputs, collapse = ", "), call. = FALSE)
}

cat(
  "\nCompleted successfully.\n",
  "Input: ", input_csv, "\n",
  "Results: ", normalizePath(output_dir, winslash = "/", mustWork = TRUE), "\n",
  "Primary analysis: integrated SPDE-INLA with summed Bombus suitability as potential availability.\n",
  "Sensitivity: widespread and montane Bombus availability.\n",
  "qGAM: upper-tail residual interpretation only.\n",
  sep = ""
)
