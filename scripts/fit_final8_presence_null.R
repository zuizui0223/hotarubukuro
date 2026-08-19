#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/natural_predictive_model.R")
source("R/reproducibility.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
cells_path <- arg_value("--cells", "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv")
output_dir <- arg_value("--output", "results/final8_presence_null")
n_draws <- as.integer(arg_value("--draws", "10000"))
seed <- as.integer(arg_value("--seed", "20260725"))

if (!file.exists(cells_path)) stop("Missing cell table: ", cells_path, call. = FALSE)
if (!is.finite(n_draws) || n_draws < 100L) stop("--draws must be >= 100", call. = FALSE)

hb_require_stage_packages("natural_predictive_model")
cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
terms <- c(
  "env_Temperature_PC1", "env_precip_PC1", "env_TemperatureSeasonality",
  "env_PrecipSeasonality", "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS"
)
v16_assert_columns(
  cells,
  c("exact_site_id", "spatial_fold", "x_km", "y_km", "n_pigmented", "n_observations", terms),
  "cells"
)
eligible <- stats::complete.cases(cells[, c(terms, "n_pigmented", "n_observations", "x_km", "y_km", "spatial_fold")])
if (!all(eligible)) {
  stop("Final-eight presence null requires complete final-eight support for every cell.", call. = FALSE)
}

Sys.setenv(
  OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1", GOTO_NUM_THREADS = "1",
  BLIS_NUM_THREADS = "1", INLA_NUM_THREADS = "1", OMP_DYNAMIC = "FALSE"
)
set.seed(seed)
result <- v16_crossfit_spde(
  cells,
  response = "n_pigmented",
  family = "binomial",
  environment_terms = terms,
  trials = "n_observations",
  model = "final8_environment_spde_presence",
  n_draws = n_draws,
  seed = seed
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
path <- file.path(output_dir, paste0("final8_presence_draws", n_draws, ".rds"))
rp_save_rds_atomic(result, path)
utils::write.csv(
  data.frame(
    field = c("model", "environment_terms", "draws", "seed", "cells"),
    value = c(
      "cross-fitted final-eight environment + Matern SPDE presence model",
      paste(terms, collapse = ";"), n_draws, seed, nrow(cells)
    ),
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "analysis_metadata.csv"),
  row.names = FALSE
)
cat("Completed final-eight cross-fitted presence null: ", path, "\n", sep = "")
