#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/natural_predictive_model.R")

arg_value <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", name, "="), "", hit[[1L]])
}

cells_path <- arg_value("--cells", "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv")
output_dir <- arg_value("--output", "results/broad_space_null_geobin_sensitivity_v23")
n_samples <- as.integer(arg_value("--samples", "500"))
seed <- as.integer(arg_value("--seed", "20260725"))
max_pairs <- as.integer(arg_value("--max-pairs-per-fold", "15000"))

if (!file.exists(cells_path)) stop("Missing cell table: ", cells_path, call. = FALSE)
if (!requireNamespace("INLA", quietly = TRUE)) stop("Package 'INLA' is required.", call. = FALSE)
if (!requireNamespace("Matrix", quietly = TRUE)) stop("Package 'Matrix' is required.", call. = FALSE)
if (!is.finite(n_samples) || n_samples < 100L) stop("--samples must be >= 100", call. = FALSE)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
v16_assert_columns(
  cells,
  c("exact_site_id", "x_km", "y_km", "spatial_fold", "n_pigmented", "n_observations", "conditional_intensity_median"),
  "cells"
)

sample_space_null <- function(train, test, response, family, mesh, trials = NULL, seed_value) {
  train_X <- data.frame(Intercept = rep(1, nrow(train)))
  test_X <- data.frame(Intercept = rep(1, nrow(test)))
  A_train <- INLA::inla.spde.make.A(mesh, loc = as.matrix(train[c("x_km", "y_km")]))
  A_test <- INLA::inla.spde.make.A(mesh, loc = as.matrix(test[c("x_km", "y_km")]))
  spde <- v16_make_spde(mesh, A_train)
  est_data <- list(y = train[[response]])
  pred_data <- list(y = rep(NA_real_, nrow(test)))
  if (family == "binomial") {
    est_data$Ntrials <- train[[trials]]
    pred_data$Ntrials <- test[[trials]]
  }
  est <- INLA::inla.stack(
    data = est_data, A = list(A_train, 1),
    effects = list(spatial = seq_len(spde$n.spde), X = train_X),
    tag = "est", compress = FALSE, remove.unused = FALSE
  )
  pred <- INLA::inla.stack(
    data = pred_data, A = list(A_test, 1),
    effects = list(spatial = seq_len(spde$n.spde), X = test_X),
    tag = "pred", compress = FALSE, remove.unused = FALSE
  )
  stack <- INLA::inla.stack(est, pred)
  dat <- INLA::inla.stack.data(stack)
  formula <- stats::as.formula("y ~ -1 + Intercept + f(spatial, model = spde)", env = environment())
  fit_args <- list(
    formula = formula, data = dat, family = family,
    control.predictor = list(A = INLA::inla.stack.A(stack), compute = TRUE, link = 1),
    control.compute = list(config = TRUE), verbose = FALSE
  )
  if (family == "binomial") fit_args$Ntrials <- dat$Ntrials
  fit <- do.call(INLA::inla, fit_args)
  pred_idx <- INLA::inla.stack.index(stack, "pred")$data
  samples <- INLA::inla.posterior.sample(
    n = n_samples, result = fit, selection = list(APredictor = pred_idx),
    seed = as.integer(seed_value), num.threads = 1, parallel.configs = FALSE, add.names = TRUE
  )
  ord <- v16_predictor_row_order(samples[[1L]], pred_idx, "APredictor")
  eta <- vapply(samples, function(s) as.numeric(s$latent[ord, 1L]), numeric(length(pred_idx)))
  if (!is.matrix(eta)) eta <- matrix(eta, nrow = length(pred_idx))
  if (family == "binomial") {
    p <- stats::plogis(eta)
    trials_test <- as.integer(test[[trials]])
    simulated <- vapply(seq_len(n_samples), function(k) {
      stats::rbinom(nrow(test), size = trials_test, prob = p[, k]) / trials_test
    }, numeric(nrow(test)))
    expected <- rowMeans(p)
  } else {
    precision <- vapply(samples, v16_observation_precision, numeric(1))
    simulated <- vapply(seq_len(n_samples), function(k) {
      stats::rnorm(nrow(test), mean = eta[, k], sd = sqrt(1 / precision[[k]]))
    }, numeric(nrow(test)))
    expected <- rowMeans(eta)
  }
  if (!is.matrix(simulated)) simulated <- matrix(simulated, nrow = nrow(test))
  list(simulated = simulated, expected = expected)
}

make_pairs <- function(n, max_pairs, seed_value) {
  if (n < 2L) return(matrix(integer(), ncol = 2L))
  total <- choose(n, 2)
  if (total <= max_pairs) return(t(utils::combn(n, 2L)))
  set.seed(seed_value)
  pairs <- matrix(integer(), nrow = 0L, ncol = 2L)
  while (nrow(pairs) < max_pairs) {
    need <- max_pairs - nrow(pairs)
    a <- sample.int(n, max(need * 3L, 1000L), replace = TRUE)
    b <- sample.int(n, length(a), replace = TRUE)
    keep <- a != b
    candidate <- cbind(pmin(a[keep], b[keep]), pmax(a[keep], b[keep]))
    pairs <- unique(rbind(pairs, candidate))
    if (nrow(pairs) > max_pairs) pairs <- pairs[seq_len(max_pairs), , drop = FALSE]
  }
  pairs
}

assign_bins <- function(x, n_bins = 5L) {
  ranks <- rank(x, ties.method = "average")
  pmin(n_bins, pmax(1L, as.integer(ceiling(ranks / length(x) * n_bins))))
}

responses <- list(
  pigmentation_state = list(
    response = "n_pigmented", family = "binomial", trials = "n_observations",
    eligible = rep(TRUE, nrow(cells)), observed = function(d) d$n_pigmented / d$n_observations
  ),
  conditional_intensity = list(
    response = "conditional_intensity_median", family = "gaussian", trials = NULL,
    eligible = is.finite(cells$conditional_intensity_median), observed = function(d) d$conditional_intensity_median
  )
)

Sys.setenv(
  OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1", GOTO_NUM_THREADS = "1",
  BLIS_NUM_THREADS = "1", INLA_NUM_THREADS = "1", OMP_DYNAMIC = "FALSE"
)
set.seed(seed)
folds <- sort(unique(as.integer(cells$spatial_fold)))
mesh <- v16_make_mesh(cells)$mesh
pair_rows <- list()
heldout_rows <- list()
site_draw_store <- setNames(vector("list", length(responses)), names(responses))

for (response_name in names(responses)) {
  spec <- responses[[response_name]]
  site_draw_store[[response_name]] <- list()
  response_index <- match(response_name, names(responses))
  for (fold in folds) {
    test <- cells[as.integer(cells$spatial_fold) == fold & spec$eligible, , drop = FALSE]
    train <- cells[as.integer(cells$spatial_fold) != fold & spec$eligible, , drop = FALSE]
    if (nrow(test) < 4L) next
    sampled <- sample_space_null(
      train, test, spec$response, spec$family, mesh, spec$trials,
      seed + 100000L * response_index + 1000L * fold
    )
    observed <- spec$observed(test)
    pairs <- make_pairs(nrow(test), max_pairs, seed + 200000L * response_index + 1000L * fold)
    dx <- test$x_km[pairs[, 1L]] - test$x_km[pairs[, 2L]]
    dy <- test$y_km[pairs[, 1L]] - test$y_km[pairs[, 2L]]
    geo <- sqrt(dx^2 + dy^2)
    pair_rows[[length(pair_rows) + 1L]] <- data.frame(
      response = response_name,
      fold = fold,
      site_i = test$exact_site_id[pairs[, 1L]],
      site_j = test$exact_site_id[pairs[, 2L]],
      geographic_distance_km = geo,
      observed_phenotype_divergence = abs(observed[pairs[, 1L]] - observed[pairs[, 2L]]),
      geo_bin_5 = assign_bins(geo, 5L),
      stringsAsFactors = FALSE
    )
    site_draw_store[[response_name]][[as.character(fold)]] <- list(
      exact_site_id = test$exact_site_id,
      observed = observed,
      expected = sampled$expected,
      simulated = sampled$simulated
    )
    heldout_rows[[length(heldout_rows) + 1L]] <- data.frame(
      response = response_name, fold = fold, exact_site_id = test$exact_site_id,
      observed = observed, space_null_expected = sampled$expected,
      stringsAsFactors = FALSE
    )
    rm(sampled)
    invisible(gc())
  }
}

pair_table <- do.call(rbind, pair_rows)
heldout_table <- do.call(rbind, heldout_rows)
utils::write.csv(pair_table, file.path(output_dir, "fixed_heldout_pair_table.csv"), row.names = FALSE)
utils::write.csv(heldout_table, file.path(output_dir, "heldout_space_null_expected.csv"), row.names = FALSE)
saveRDS(site_draw_store, file.path(output_dir, "heldout_space_null_site_draws.rds"), compress = "xz")
utils::write.csv(
  data.frame(
    field = c("analysis", "space_null", "cross_fitting", "spatial_folds", "posterior_samples", "seed", "pair_policy", "geographic_distance_bins", "claim_boundary"),
    value = c(
      "fixed_pair_space_only_reference_cache",
      "intercept + Matern SPDE only",
      "each pair lies wholly within one held-out geographical fold",
      paste(folds, collapse = ";"), n_samples, seed,
      "one deterministic pair sample per response and fold",
      "5 equal-count geographic-distance strata",
      "spatial-continuity reference only; not a genetic, selection, adaptation, or unique-causation test"
    ),
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "analysis_metadata.csv"), row.names = FALSE
)
cat("Completed fixed-pair space-only reference cache.\n")
