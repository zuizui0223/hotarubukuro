#!/usr/bin/env Rscript

options(repos = c(CRAN = "https://cloud.r-project.org"))

say <- function(...) message(sprintf(...))
run <- function(args, log = NULL) {
  say("Running: Rscript %s", paste(args, collapse = " "))
  status <- if (is.null(log)) {
    system2("Rscript", args)
  } else {
    dir.create(dirname(log), recursive = TRUE, showWarnings = FALSE)
    system2("Rscript", args, stdout = log, stderr = log)
  }
  if (!identical(status, 0L)) {
    if (!is.null(log) && file.exists(log)) {
      lines <- readLines(log, warn = FALSE)
      cat(tail(lines, 200), sep = "\n")
    }
    stop("Command failed: Rscript ", paste(args, collapse = " "), call. = FALSE)
  }
}

stopifnot(
  file.exists("Data_S1.csv"),
  file.exists("analysis/diagnose_all_extraction.R"),
  file.exists("analysis/run_publication_workflow.R"),
  dir.exists("sdm")
)

say("1/5 Installing required R packages")
base_needed <- c("terra", "sf", "lavaan", "yaml", "geodata", "digest", "jsonlite")
missing <- base_needed[!vapply(base_needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, dependencies = TRUE)

if (!requireNamespace("mgcv", quietly = TRUE) || packageVersion("mgcv") < "1.9.0") {
  install.packages("mgcv", dependencies = TRUE)
}
if (!requireNamespace("qgam", quietly = TRUE)) install.packages("qgam", dependencies = TRUE)
if (!requireNamespace("INLA", quietly = TRUE)) {
  install.packages(
    "INLA",
    repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"),
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
}

needed <- c(base_needed, "mgcv", "qgam", "INLA")
ok <- vapply(needed, requireNamespace, logical(1), quietly = TRUE)
if (!all(ok)) stop("Missing packages: ", paste(names(ok)[!ok], collapse = ", "), call. = FALSE)
stopifnot(packageVersion("mgcv") >= "1.9.0")

say("2/5 Checking prepared rasters")
raster_dir <- "data/processed/rasters"
raster_files <- if (dir.exists(raster_dir)) list.files(raster_dir, pattern = "[.]tif$", full.names = TRUE) else character()
if (length(raster_files) < 19L) {
  say("Prepared rasters are missing; downloading and preparing them now")
  run(c("scripts/download_rasters.R"))
  run(c("scripts/prepare_rasters.R", "--force", "--no-download"))
}

raster_files <- list.files(raster_dir, pattern = "[.]tif$", full.names = TRUE)
stopifnot(length(raster_files) >= 19L, all(file.info(raster_files)$size > 0))

say("3/5 Validating environmental and Bombus extraction")
run(c(
  "analysis/diagnose_all_extraction.R",
  "Data_S1.csv",
  raster_dir,
  "sdm",
  "results/all_extraction_diagnostic"
))

say("4/5 Downloading missing human predictors")
run(c("scripts/download_human_predictors.R", "data/cache/human"))

say("5/5 Running CFA, SPDE-INLA and qGAM")
run(c(
  "analysis/run_publication_workflow.R",
  "Data_S1.csv",
  raster_dir,
  "sdm",
  "data/cache/human",
  "results/publication"
), log = "results/publication/local-run.log")

required <- file.path("results/publication", c(
  "cfa_standardized_loadings.csv",
  "environment_pca_loadings.csv",
  "bombus_environment_correlations.csv",
  "integrated_spde_model_diagnostics.csv",
  "integrated_spde_fixed_effects.csv",
  "qgam_model_summary.csv",
  "anthropogenic_outlier_review.csv",
  "analysis_data_with_predictions.csv",
  "sessionInfo.txt"
))
missing_outputs <- required[!file.exists(required) | file.info(required)$size <= 0]
if (length(missing_outputs)) stop("Missing outputs: ", paste(missing_outputs, collapse = ", "), call. = FALSE)

say("Done. Results are in results/publication")
