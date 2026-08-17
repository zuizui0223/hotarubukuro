args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/natural_predictive_model.R")

arg_value <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", name, "="), "", hit[[1L]])
}

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
output_dir <- arg_value("--output", "results/broad_space_null_phenotype_excess")
n_samples <- as.integer(arg_value("--samples", "500"))
seed <- as.integer(arg_value("--seed", "20260725"))
max_pairs_per_fold <- as.integer(arg_value("--max-pairs-per-fold", "15000"))
n_geo_bins <- as.integer(arg_value("--geo-bins", "5"))
inla_verbose <- identical(tolower(arg_value("--inla-verbose", "false")), "true")

if (!file.exists(cells_path)) stop("Missing cell table: ", cells_path, call. = FALSE)
if (!is.finite(n_samples) || n_samples < 100L) stop("--samples must be >= 100", call. = FALSE)
if (!is.finite(max_pairs_per_fold) || max_pairs_per_fold < 100L) stop("--max-pairs-per-fold must be >= 100", call. = FALSE)
if (!is.finite(n_geo_bins) || n_geo_bins < 2L) stop("--geo-bins must be >= 2", call. = FALSE)
if (!requireNamespace("INLA", quietly = TRUE)) stop("Package 'INLA' is required.", call. = FALSE)
if (!requireNamespace("Matrix", quietly = TRUE)) stop("Package 'Matrix' is required.", call. = FALSE)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
environment_terms <- v16_environment_terms(50)
v16_assert_columns(
  cells,
  c("exact_site_id", "x_km", "y_km", "spatial_fold", "n_pigmented",
    "n_observations", "conditional_intensity_median", environment_terms),
  "cells"
)

empty_basis <- function(train, test) {
  list(
    train = data.frame(row.names = seq_len(nrow(train))),
    test = data.frame(row.names = seq_len(nrow(test)))
  )
}

sample_space_null <- function(train, test, response, family, mesh,
                              trials = NULL, seed_value) {
  basis <- empty_basis(train, test)
  train_X <- cbind(Intercept = 1, basis$train)
  test_X <- cbind(Intercept = 1, basis$test)
  A_train <- INLA::inla.spde.make.A(mesh, loc = as.matrix(train[c("x_km", "y_km")]))
  A_test <- INLA::inla.spde.make.A(mesh, loc = as.matrix(test[c("x_km", "y_km")]))
  spde <- v16_make_spde(mesh, A_train)
  estimation_data <- list(y = train[[response]])
  prediction_data <- list(y = rep(NA_real_, nrow(test)))
  if (family == "binomial") {
    estimation_data$Ntrials <- train[[trials]]
    prediction_data$Ntrials <- test[[trials]]
  }
  stack_estimation <- INLA::inla.stack(
    data = estimation_data, A = list(A_train, 1),
    effects = list(spatial = seq_len(spde$n.spde), X = train_X),
    tag = "est", compress = FALSE, remove.unused = FALSE
  )
  stack_prediction <- INLA::inla.stack(
    data = prediction_data, A = list(A_test, 1),
    effects = list(spatial = seq_len(spde$n.spde), X = test_X),
    tag = "pred", compress = FALSE, remove.unused = FALSE
  )
  stack <- INLA::inla.stack(stack_estimation, stack_prediction)
  stack_data <- INLA::inla.stack.data(stack)
  formula <- stats::as.formula("y ~ -1 + Intercept + f(spatial, model = spde)", env = environment())
  inla_args <- list(
    formula = formula, data = stack_data, family = family,
    control.predictor = list(A = INLA::inla.stack.A(stack), compute = TRUE, link = 1),
    control.compute = list(config = TRUE), verbose = inla_verbose
  )
  if (family == "binomial") inla_args$Ntrials <- stack_data$Ntrials
  fit <- do.call(INLA::inla, inla_args)
  prediction_index <- INLA::inla.stack.index(stack, "pred")$data
  samples <- INLA::inla.posterior.sample(
    n = n_samples, result = fit,
    selection = list(APredictor = prediction_index),
    seed = as.integer(seed_value), num.threads = 1,
    parallel.configs = FALSE, add.names = TRUE
  )
  order <- v16_predictor_row_order(samples[[1L]], prediction_index, "APredictor")
  eta <- vapply(
    samples, function(sample) as.numeric(sample$latent[order, 1L]),
    numeric(length(prediction_index))
  )
  if (!is.matrix(eta)) eta <- matrix(eta, nrow = length(prediction_index))
  if (family == "binomial") {
    probability <- stats::plogis(eta)
    trials_test <- as.integer(test[[trials]])
    simulated <- vapply(seq_len(n_samples), function(k) {
      stats::rbinom(nrow(test), size = trials_test, prob = probability[, k]) / trials_test
    }, numeric(nrow(test)))
    expected <- rowMeans(probability)
    precision <- NULL
  } else {
    precision <- vapply(samples, v16_observation_precision, numeric(1))
    simulated <- vapply(seq_len(n_samples), function(k) {
      stats::rnorm(nrow(test), mean = eta[, k], sd = sqrt(1 / precision[[k]]))
    }, numeric(nrow(test)))
    expected <- rowMeans(eta)
  }
  if (!is.matrix(simulated)) simulated <- matrix(simulated, nrow = nrow(test))
  list(simulated = simulated, expected = expected, precision = precision)
}

make_pairs <- function(n, max_pairs, seed_value) {
  if (n < 2L) return(matrix(integer(), ncol = 2L))
  total <- choose(n, 2)
  if (total <= max_pairs) return(t(utils::combn(n, 2L)))
  set.seed(seed_value)
  pairs <- matrix(integer(), nrow = 0L, ncol = 2L)
  target <- as.integer(max_pairs)
  while (nrow(pairs) < target) {
    need <- target - nrow(pairs)
    draw_n <- max(need * 3L, 1000L)
    a <- sample.int(n, draw_n, replace = TRUE)
    b <- sample.int(n, draw_n, replace = TRUE)
    keep <- a != b
    if (!any(keep)) next
    candidate <- cbind(pmin(a[keep], b[keep]), pmax(a[keep], b[keep]))
    pairs <- unique(rbind(pairs, candidate))
    if (nrow(pairs) > target) pairs <- pairs[seq_len(target), , drop = FALSE]
  }
  pairs
}

assign_equal_count_bins <- function(x, n_bins) {
  if (!length(x)) return(integer())
  ranks <- rank(x, ties.method = "average")
  pmin(n_bins, pmax(1L, as.integer(ceiling(ranks / length(x) * n_bins))))
}

pair_environment_distance <- function(z, pairs) {
  if (!nrow(pairs)) return(numeric())
  delta <- as.matrix(z[pairs[, 1L], , drop = FALSE]) -
    as.matrix(z[pairs[, 2L], , drop = FALSE])
  sqrt(rowSums(delta^2))
}

pair_geo_distance <- function(test, pairs) {
  dx <- test$x_km[pairs[, 1L]] - test$x_km[pairs[, 2L]]
  dy <- test$y_km[pairs[, 1L]] - test$y_km[pairs[, 2L]]
  sqrt(dx^2 + dy^2)
}

pair_divergence_vector <- function(values, pairs) {
  abs(values[pairs[, 1L]] - values[pairs[, 2L]])
}

pair_divergence_matrix <- function(values, pairs) {
  abs(values[pairs[, 1L], , drop = FALSE] - values[pairs[, 2L], , drop = FALSE])
}

contrast_from_groups <- function(observed_div, null_div, geo_bin, env_dist,
                                 response_name, fold) {
  rows <- list()
  null_contrasts <- list()
  for (bin in sort(unique(geo_bin))) {
    idx <- which(geo_bin == bin & is.finite(env_dist) & is.finite(observed_div))
    if (length(idx) < 8L) next
    q <- stats::quantile(env_dist[idx], probs = c(0.25, 0.75), names = FALSE, type = 7)
    low <- idx[env_dist[idx] <= q[[1L]]]
    high <- idx[env_dist[idx] >= q[[2L]]]
    if (length(low) < 2L || length(high) < 2L) next
    observed_contrast <- mean(observed_div[high]) - mean(observed_div[low])
    null_contrast <- colMeans(null_div[high, , drop = FALSE]) -
      colMeans(null_div[low, , drop = FALSE])
    rows[[length(rows) + 1L]] <- data.frame(
      response = response_name, fold = fold, geo_bin = bin,
      n_pairs = length(idx), n_low_environment = length(low),
      n_high_environment = length(high),
      geo_distance_median = NA_real_, env_low_cut = q[[1L]], env_high_cut = q[[2L]],
      observed_high_minus_low = observed_contrast,
      null_median = stats::median(null_contrast),
      null_q025 = stats::quantile(null_contrast, 0.025, names = FALSE),
      null_q975 = stats::quantile(null_contrast, 0.975, names = FALSE),
      excess_over_null_median = observed_contrast - stats::median(null_contrast),
      stringsAsFactors = FALSE
    )
    null_contrasts[[length(null_contrasts) + 1L]] <- null_contrast
  }
  list(
    summary = if (length(rows)) do.call(rbind, rows) else data.frame(),
    null = null_contrasts
  )
}

responses <- list(
  pigmentation_state = list(
    response = "n_pigmented", family = "binomial", trials = "n_observations",
    eligible = rep(TRUE, nrow(cells)),
    observed = function(d) d$n_pigmented / d$n_observations
  ),
  conditional_intensity = list(
    response = "conditional_intensity_median", family = "gaussian", trials = NULL,
    eligible = is.finite(cells$conditional_intensity_median),
    observed = function(d) d$conditional_intensity_median
  )
)
folds <- sort(unique(as.integer(cells$spatial_fold)))
mesh <- v16_make_mesh(cells)$mesh

pair_rows <- list()
stratum_rows <- list()
response_null_contrasts <- setNames(vector("list", length(responses)), names(responses))
response_observed_contrasts <- setNames(vector("list", length(responses)), names(responses))
heldout_rows <- list()

for (response_name in names(responses)) {
  specification <- responses[[response_name]]
  response_null_contrasts[[response_name]] <- list()
  response_observed_contrasts[[response_name]] <- numeric()
  for (fold in folds) {
    test_index <- which(as.integer(cells$spatial_fold) == fold & specification$eligible)
    train_index <- which(as.integer(cells$spatial_fold) != fold & specification$eligible)
    train <- cells[train_index, , drop = FALSE]
    test <- cells[test_index, , drop = FALSE]
    if (nrow(test) < 4L) next
    message("[space-null excess] ", response_name, " fold=", fold,
            " train=", nrow(train), " test=", nrow(test))
    env_basis <- v16_fold_predictors(train, test, environment_terms)
    sampled <- sample_space_null(
      train, test, specification$response, specification$family, mesh,
      specification$trials,
      seed + 100000L * match(response_name, names(responses)) + 1000L * fold
    )
    observed <- specification$observed(test)
    pairs <- make_pairs(
      nrow(test), max_pairs_per_fold,
      seed + 200000L * match(response_name, names(responses)) + 1000L * fold
    )
    geo_distance <- pair_geo_distance(test, pairs)
    env_distance <- pair_environment_distance(env_basis$test, pairs)
    observed_div <- pair_divergence_vector(observed, pairs)
    null_div <- pair_divergence_matrix(sampled$simulated, pairs)
    null_pair_median <- apply(null_div, 1L, stats::median)
    null_pair_q025 <- apply(null_div, 1L, stats::quantile, probs = 0.025, names = FALSE)
    null_pair_q975 <- apply(null_div, 1L, stats::quantile, probs = 0.975, names = FALSE)
    geo_bin <- assign_equal_count_bins(geo_distance, n_geo_bins)

    pair_rows[[length(pair_rows) + 1L]] <- data.frame(
      response = response_name, fold = fold,
      site_i = test$exact_site_id[pairs[, 1L]],
      site_j = test$exact_site_id[pairs[, 2L]],
      geographic_distance_km = geo_distance,
      environmental_distance_z = env_distance,
      observed_phenotype_divergence = observed_div,
      space_null_median_divergence = null_pair_median,
      space_null_q025 = null_pair_q025,
      space_null_q975 = null_pair_q975,
      phenotype_excess = observed_div - null_pair_median,
      above_space_null_q975 = observed_div > null_pair_q975,
      geo_bin = geo_bin,
      stringsAsFactors = FALSE
    )

    contrasted <- contrast_from_groups(
      observed_div, null_div, geo_bin, env_distance, response_name, fold
    )
    if (nrow(contrasted$summary)) {
      for (row_index in seq_len(nrow(contrasted$summary))) {
        bin <- contrasted$summary$geo_bin[[row_index]]
        contrasted$summary$geo_distance_median[[row_index]] <-
          stats::median(geo_distance[geo_bin == bin])
      }
      stratum_rows[[length(stratum_rows) + 1L]] <- contrasted$summary
      response_observed_contrasts[[response_name]] <- c(
        response_observed_contrasts[[response_name]],
        contrasted$summary$observed_high_minus_low
      )
      response_null_contrasts[[response_name]] <- c(
        response_null_contrasts[[response_name]], contrasted$null
      )
    }
    heldout_rows[[length(heldout_rows) + 1L]] <- data.frame(
      response = response_name, fold = fold, exact_site_id = test$exact_site_id,
      observed = observed, space_null_expected = sampled$expected,
      stringsAsFactors = FALSE
    )
    rm(sampled, null_div)
    invisible(gc())
  }
}

pair_table <- do.call(rbind, pair_rows)
stratum_table <- do.call(rbind, stratum_rows)
heldout_table <- do.call(rbind, heldout_rows)

primary_rows <- lapply(names(responses), function(response_name) {
  observed_components <- response_observed_contrasts[[response_name]]
  null_components <- response_null_contrasts[[response_name]]
  if (!length(observed_components) || !length(null_components)) {
    stop("No valid matched-distance strata for ", response_name, call. = FALSE)
  }
  null_matrix <- do.call(rbind, null_components)
  null_global <- colMeans(null_matrix)
  observed_global <- mean(observed_components)
  p_upper <- (1 + sum(null_global >= observed_global)) / (length(null_global) + 1)
  data.frame(
    response = response_name,
    estimand = "matched_geographic_distance_high_minus_low_environment_phenotype_divergence",
    n_fold_geo_strata = length(observed_components),
    observed_contrast = observed_global,
    space_null_median = stats::median(null_global),
    space_null_q025 = stats::quantile(null_global, 0.025, names = FALSE),
    space_null_q975 = stats::quantile(null_global, 0.975, names = FALSE),
    phenotype_excess_over_space_null = observed_global - stats::median(null_global),
    posterior_predictive_p_upper = p_upper,
    exceeds_space_null_q975 = observed_global > stats::quantile(null_global, 0.975, names = FALSE),
    stringsAsFactors = FALSE
  )
})
primary_table <- do.call(rbind, primary_rows)

secondary_rows <- lapply(names(responses), function(response_name) {
  x <- pair_table[pair_table$response == response_name, , drop = FALSE]
  data.frame(
    response = response_name,
    n_pairs = nrow(x),
    median_phenotype_excess = stats::median(x$phenotype_excess, na.rm = TRUE),
    fraction_pairs_above_space_null_q975 = mean(x$above_space_null_q975, na.rm = TRUE),
    spearman_env_distance_vs_excess = suppressWarnings(stats::cor(
      x$environmental_distance_z, x$phenotype_excess,
      method = "spearman", use = "complete.obs"
    )),
    stringsAsFactors = FALSE
  )
})
secondary_table <- do.call(rbind, secondary_rows)

utils::write.csv(pair_table, file.path(output_dir, "heldout_pair_space_null_excess.csv"), row.names = FALSE)
utils::write.csv(stratum_table, file.path(output_dir, "matched_distance_stratum_contrasts.csv"), row.names = FALSE)
utils::write.csv(primary_table, file.path(output_dir, "primary_space_null_excess_test.csv"), row.names = FALSE)
utils::write.csv(secondary_table, file.path(output_dir, "secondary_pair_diagnostics.csv"), row.names = FALSE)
utils::write.csv(heldout_table, file.path(output_dir, "heldout_space_null_predictions.csv"), row.names = FALSE)

metadata <- data.frame(
  field = c(
    "analysis", "source_cells", "space_null", "cross_fitting", "spatial_folds",
    "posterior_samples", "seed", "max_pairs_per_fold", "geographic_distance_bins",
    "environment_terms", "environment_distance", "primary_estimand", "claim_boundary"
  ),
  value = c(
    "broad_cross_fitted_space_null_phenotype_excess",
    cells_path,
    "intercept + Matern SPDE only",
    "each tested pair lies wholly inside a held-out geographical fold",
    paste(folds, collapse = ";"), n_samples, seed, max_pairs_per_fold, n_geo_bins,
    paste(environment_terms, collapse = ";"),
    "Euclidean distance in the frozen response-blind environmental basis, scaled on each training fold",
    "mean across fold-by-geographic-distance strata of observed(high-environment - low-environment phenotype divergence), compared with the same statistic under cross-fitted space-only posterior prediction",
    "F_ST/P_ST-inspired non-genetic null test; not F_ST, P_ST, Q_ST, drift, selection, or local adaptation",
    stringsAsFactors = FALSE
  )
)
utils::write.csv(metadata, file.path(output_dir, "analysis_metadata.csv"), row.names = FALSE)

cat("Completed cross-fitted space-null phenotype-excess analysis.\n")
print(primary_table)
print(secondary_table)
