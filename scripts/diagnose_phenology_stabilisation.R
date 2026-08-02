# Measure one (fold, diagonal) attempt of the phenology component.
#
# Why this exists.
#
# The phenology component of stage 02 aborts inside inla.qsample with "Matrix is
# not (numerical) positive definite". Two escalation rounds measured one value
# each and disagreed about which fold fails: 1e-8 cleared fold 1 and aborted at
# fold 2, 1e-7 aborted at fold 1. The response to the stabilisation is therefore
# not monotone in the value, and choosing a value by rerunning the whole pipeline
# one guess at a time is both slow and unsound.
#
# This script performs exactly one attempt and reports whether it survived. The
# abort is a SIGABRT that kills the whole R process, so a sweep cannot be a loop
# inside one session; scripts/sweep_phenology_stabilisation.sh runs one of these
# per attempt and records the exit status.
#
# It changes nothing. It fits the same model, on the same folds, with the same
# predictors, mesh, priors and seeds as R/natural_predictive_model.R, by calling
# that module's own functions. Its only output is a status line.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

input_cells <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
target_fold <- as.integer(arg_value("--fold", "1"))
diagonal <- as.numeric(arg_value("--diagonal", "0"))
# The failure is in the Cholesky factorisation of the stored configuration, which
# happens once regardless of how many draws are requested. A small count is
# therefore a faithful test of the same code path at a fraction of the cost.
n_draws <- as.integer(arg_value("--draws", "20"))
seed <- as.integer(arg_value("--seed", "20260725"))

hb_require_stage_packages("natural_predictive_model")
hb_load_modules("natural_predictive_model")

cells <- utils::read.csv(input_cells, check.names = FALSE, stringsAsFactors = FALSE)
cells$median_year_centered <- cells$median_year - 2024
cells$median_year_centered_squared <- cells$median_year_centered^2

response <- "median_DOY"
environment_terms <- c(
  v16_environment_terms(50), "median_year_centered",
  "median_year_centered_squared"
)

# The same population, ordering and eligibility rule v16_crossfit_spde applies,
# so the fold this script fits is the fold the pipeline fits.
training_eligible <- is.finite(cells[[response]])
folds <- sort(unique(as.integer(cells$spatial_fold)))
if (!target_fold %in% folds) {
  stop("Fold ", target_fold, " is not one of: ", paste(folds, collapse = ", "),
       call. = FALSE)
}
test_index <- which(as.integer(cells$spatial_fold) == target_fold)
train_index <- which(
  as.integer(cells$spatial_fold) != target_fold & training_eligible
)
train <- cells[train_index, , drop = FALSE]
test <- cells[test_index, , drop = FALSE]

mesh_objects <- v16_make_mesh(cells)
basis <- v16_fold_predictors(train, test, environment_terms, character())

# The phenology component's base seed, then the fold offset. Identical to
# v16_crossfit_spde, so the sampler seed matches the one a pipeline run reports.
fold_seed <- as.integer(
  (seed + 200000L) + 1000L * match(target_fold, folds)
)

cat(
  "[diagnostic] phenology fold ", target_fold,
  ": train=", nrow(train), ", test=", nrow(test),
  ", draws=", n_draws, ", sampler seed=", fold_seed,
  ", diagonal=", format(diagonal, scientific = TRUE), "\n",
  sep = ""
)

elapsed <- system.time({
  result <- v16_fit_fold(
    train, test, response, basis, mesh_objects$mesh,
    family = "gaussian", trials = NULL, n_draws = n_draws,
    seed = fold_seed, model = "national_environment_year_spde_phenology",
    fold = target_fold, inla_verbose = FALSE, diagonal = diagonal
  )
})[["elapsed"]]

if (any(!is.finite(result$draws))) {
  stop("Sampling returned non-finite draws.", call. = FALSE)
}

cat(
  "[diagnostic] SURVIVED fold=", target_fold,
  " diagonal=", format(diagonal, scientific = TRUE),
  " elapsed=", round(elapsed, 1), "s",
  " draw_range=", paste(round(range(result$draws), 3), collapse = ".."),
  "\n", sep = ""
)
