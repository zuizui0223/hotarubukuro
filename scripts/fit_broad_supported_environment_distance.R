#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/natural_predictive_model.R")

arg_value <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", name, "="), "", hit[[1L]])
}

observations_path <- arg_value(
  "--observations",
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
)
cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
output_dir <- arg_value(
  "--output", "results/broad_supported_environment_distance"
)
n_samples <- as.integer(arg_value("--samples", "500"))
seed <- as.integer(arg_value("--seed", "20260725"))
max_pairs_per_fold <- as.integer(arg_value("--max-pairs-per-fold", "15000"))
n_geo_bins <- as.integer(arg_value("--geo-bins", "5"))
inla_verbose <- identical(tolower(arg_value("--inla-verbose", "false")), "true")

if (!file.exists(observations_path)) stop("Missing observations: ", observations_path, call. = FALSE)
if (!file.exists(cells_path)) stop("Missing cells: ", cells_path, call. = FALSE)
if (!is.finite(n_samples) || n_samples < 100L) stop("--samples must be >=100", call. = FALSE)
if (!is.finite(max_pairs_per_fold) || max_pairs_per_fold < 100L) stop("--max-pairs-per-fold must be >=100", call. = FALSE)
if (!is.finite(n_geo_bins) || n_geo_bins < 2L) stop("--geo-bins must be >=2", call. = FALSE)
if (!requireNamespace("INLA", quietly = TRUE)) stop("Package 'INLA' is required.", call. = FALSE)
if (!requireNamespace("Matrix", quietly = TRUE)) stop("Package 'Matrix' is required.", call. = FALSE)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
observations <- utils::read.csv(observations_path, check.names = FALSE, stringsAsFactors = FALSE)
cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)

base_environment_terms <- c(
  "env_Temperature_PC1", "env_precip_PC1",
  "env_TemperatureSeasonality", "env_PrecipSeasonality",
  "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS"
)
v16_assert_columns(
  observations,
  c(base_environment_terms, "pigmented_mixture50", "pigment_intensity_z"),
  "observations"
)
v16_assert_columns(
  cells,
  c(
    base_environment_terms, "exact_site_id", "x_km", "y_km", "spatial_fold",
    "n_pigmented", "n_observations", "conditional_intensity_median"
  ),
  "cells"
)

# Reproduce the fixed transformation used by the final intensity model. The
# final model itself is not refitted and its coefficients do not weight the
# environmental distance.
interaction_reference <- as.numeric(
  observations$env_Temperature_PC1 * observations$env_TemperatureSeasonality
)
interaction_centre <- mean(interaction_reference, na.rm = TRUE)
interaction_spread <- stats::sd(interaction_reference, na.rm = TRUE)
if (!is.finite(interaction_spread) || interaction_spread <= 1e-12) {
  stop("Invalid interaction scaling reference.", call. = FALSE)
}
cells$int_thermal_variability <- (
  cells$env_Temperature_PC1 * cells$env_TemperatureSeasonality - interaction_centre
) / interaction_spread

supported_terms <- list(
  pigmentation_state = c("env_Temperature_PC1"),
  conditional_intensity = c(
    "env_precip_PC1", "env_TemperatureSeasonality",
    "env_topo_PC1", "int_thermal_variability"
  )
)
for (response_name in names(supported_terms)) {
  if (any(!stats::complete.cases(cells[supported_terms[[response_name]]]))) {
    stop(response_name, " has incomplete supported environmental terms.", call. = FALSE)
  }
}

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
  formula <- stats::as.formula(
    "y ~ -1 + Intercept + f(spatial, model = spde)", env = environment()
  )
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
  predictor_order <- v16_predictor_row_order(
    samples[[1L]], prediction_index, "APredictor"
  )
  eta <- vapply(
    samples,
    function(sample) as.numeric(sample$latent[predictor_order, 1L]),
    numeric(length(prediction_index))
  )
  if (!is.matrix(eta)) eta <- matrix(eta, nrow = length(prediction_index))
  if (family == "binomial") {
    probability <- stats::plogis(eta)
    trials_test <- as.integer(test[[trials]])
    simulated <- vapply(seq_len(n_samples), function(k) {
      stats::rbinom(nrow(test), size = trials_test, prob = probability[, k]) /
        trials_test
    }, numeric(nrow(test)))
    expected <- rowMeans(probability)
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
  ranks <- rank(x, ties.method = "average")
  pmin(n_bins, pmax(1L, as.integer(ceiling(ranks / length(x) * n_bins))))
}

pair_geo_distance <- function(test, pairs) {
  dx <- test$x_km[pairs[, 1L]] - test$x_km[pairs[, 2L]]
  dy <- test$y_km[pairs[, 1L]] - test$y_km[pairs[, 2L]]
  sqrt(dx^2 + dy^2)
}

pair_environment_distance <- function(test, pairs, terms) {
  first <- as.matrix(test[pairs[, 1L], terms, drop = FALSE])
  second <- as.matrix(test[pairs[, 2L], terms, drop = FALSE])
  sqrt(rowSums((first - second)^2))
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
    cuts <- stats::quantile(
      env_dist[idx], probs = c(0.25, 0.75), names = FALSE, type = 7
    )
    low <- idx[env_dist[idx] <= cuts[[1L]]]
    high <- idx[env_dist[idx] >= cuts[[2L]]]
    if (length(low) < 2L || length(high) < 2L) next
    observed_contrast <- mean(observed_div[high]) - mean(observed_div[low])
    null_contrast <- colMeans(null_div[high, , drop = FALSE]) -
      colMeans(null_div[low, , drop = FALSE])
    rows[[length(rows) + 1L]] <- data.frame(
      response = response_name, fold = fold, geo_bin = bin,
      n_pairs = length(idx), n_low_environment = length(low),
      n_high_environment = length(high),
      environment_low_cut = cuts[[1L]], environment_high_cut = cuts[[2L]],
      observed_high_minus_low = observed_contrast,
      space_null_median = stats::median(null_contrast),
      space_null_q025 = stats::quantile(null_contrast, 0.025, names = FALSE),
      space_null_q975 = stats::quantile(null_contrast, 0.975, names = FALSE),
      excess_over_space_null = observed_contrast - stats::median(null_contrast),
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
heldout_rows <- list()
response_null <- setNames(vector("list", length(responses)), names(responses))
response_observed <- setNames(vector("list", length(responses)), names(responses))

for (response_name in names(responses)) {
  specification <- responses[[response_name]]
  terms <- supported_terms[[response_name]]
  response_null[[response_name]] <- list()
  response_observed[[response_name]] <- numeric()
  for (fold in folds) {
    test_index <- which(as.integer(cells$spatial_fold) == fold & specification$eligible)
    train_index <- which(as.integer(cells$spatial_fold) != fold & specification$eligible)
    train <- cells[train_index, , drop = FALSE]
    test <- cells[test_index, , drop = FALSE]
    if (nrow(test) < 4L) next
    message(
      "[supported environment distance] ", response_name,
      " fold=", fold, " train=", nrow(train), " test=", nrow(test)
    )
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
    env_distance <- pair_environment_distance(test, pairs, terms)
    observed_div <- pair_divergence_vector(observed, pairs)
    null_div <- pair_divergence_matrix(sampled$simulated, pairs)
    geo_bin <- assign_equal_count_bins(geo_distance, n_geo_bins)
    null_pair_median <- apply(null_div, 1L, stats::median)

    pair_rows[[length(pair_rows) + 1L]] <- data.frame(
      response = response_name, fold = fold,
      site_i = test$exact_site_id[pairs[, 1L]],
      site_j = test$exact_site_id[pairs[, 2L]],
      geographic_distance_km = geo_distance,
      supported_environmental_distance = env_distance,
      observed_phenotype_divergence = observed_div,
      space_null_median_divergence = null_pair_median,
      phenotype_excess = observed_div - null_pair_median,
      geo_bin = geo_bin, stringsAsFactors = FALSE
    )

    contrasted <- contrast_from_groups(
      observed_div, null_div, geo_bin, env_distance, response_name, fold
    )
    if (nrow(contrasted$summary)) {
      stratum_rows[[length(stratum_rows) + 1L]] <- contrasted$summary
      response_observed[[response_name]] <- c(
        response_observed[[response_name]],
        contrasted$summary$observed_high_minus_low
      )
      response_null[[response_name]] <- c(
        response_null[[response_name]], contrasted$null
      )
    }
    heldout_rows[[length(heldout_rows) + 1L]] <- data.frame(
      response = response_name, fold = fold,
      exact_site_id = test$exact_site_id,
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
primary_rows <- list()
draw_rows <- list()

for (response_name in names(responses)) {
  observed_components <- response_observed[[response_name]]
  null_components <- response_null[[response_name]]
  if (!length(observed_components) || !length(null_components)) {
    stop("No valid strata for ", response_name, call. = FALSE)
  }
  null_matrix <- do.call(rbind, null_components)
  null_global <- colMeans(null_matrix)
  observed_global <- mean(observed_components)
  primary_rows[[length(primary_rows) + 1L]] <- data.frame(
    response = response_name,
    supported_environment_terms = paste(supported_terms[[response_name]], collapse = ";"),
    n_fold_geo_strata = length(observed_components),
    observed_contrast = observed_global,
    space_null_median = stats::median(null_global),
    space_null_q025 = stats::quantile(null_global, 0.025, names = FALSE),
    space_null_q975 = stats::quantile(null_global, 0.975, names = FALSE),
    phenotype_excess_over_space_null = observed_global - stats::median(null_global),
    posterior_predictive_p_upper =
      (1 + sum(null_global >= observed_global)) / (length(null_global) + 1),
    stringsAsFactors = FALSE
  )
  draw_rows[[length(draw_rows) + 1L]] <- data.frame(
    response = response_name, draw = seq_along(null_global),
    space_null_global_contrast = null_global, stringsAsFactors = FALSE
  )
}

primary_table <- do.call(rbind, primary_rows)
draw_table <- do.call(rbind, draw_rows)
term_table <- do.call(rbind, lapply(names(supported_terms), function(response_name) {
  data.frame(
    response = response_name,
    environmental_term = supported_terms[[response_name]],
    distance_weight = 1,
    source = "final model 95% credible interval excludes zero",
    stringsAsFactors = FALSE
  )
}))

utils::write.csv(primary_table, file.path(output_dir, "primary_supported_environment_distance_test.csv"), row.names = FALSE)
utils::write.csv(stratum_table, file.path(output_dir, "matched_distance_stratum_contrasts.csv"), row.names = FALSE)
utils::write.csv(pair_table, file.path(output_dir, "heldout_pair_supported_environment_distance.csv"), row.names = FALSE)
utils::write.csv(heldout_table, file.path(output_dir, "heldout_space_null_predictions.csv"), row.names = FALSE)
utils::write.csv(draw_table, file.path(output_dir, "space_null_global_contrast_draws.csv"), row.names = FALSE)
utils::write.csv(term_table, file.path(output_dir, "supported_environment_terms.csv"), row.names = FALSE)

metadata <- data.frame(
  field = c(
    "analysis", "observations", "cells", "final_model_refit", "space_null",
    "spatial_folds", "posterior_samples", "seed", "max_pairs_per_fold",
    "geographic_distance_bins", "pigmentation_state_terms",
    "conditional_intensity_terms", "environment_distance", "primary_estimand",
    "claim_boundary"
  ),
  value = c(
    "broad_supported_environment_distance_beyond_space",
    observations_path, cells_path,
    "no; final model is used only to freeze the supported term sets",
    "cross-fitted intercept + Matern SPDE",
    paste(folds, collapse = ";"), n_samples, seed, max_pairs_per_fold,
    n_geo_bins,
    paste(supported_terms$pigmentation_state, collapse = ";"),
    paste(supported_terms$conditional_intensity, collapse = ";"),
    "unweighted Euclidean distance in the frozen standardized supported terms",
    "mean across 25 fold-by-geographic-distance strata of high-minus-low supported-environment-distance phenotype divergence, compared with the identical statistic under space-only posterior prediction",
    "supporting spatial-null comparison; not selection, adaptation, genetics, drift or direct physiology"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(metadata, file.path(output_dir, "analysis_metadata.csv"), row.names = FALSE)

summary_lines <- c(
  "# Supported environmental distance beyond spatial continuity", "",
  vapply(seq_len(nrow(primary_table)), function(i) {
    row <- primary_table[i, ]
    paste0(
      "- ", row$response,
      ": observed=", sprintf("%.6f", row$observed_contrast),
      "; space-null median=", sprintf("%.6f", row$space_null_median),
      "; excess=", sprintf("%.6f", row$phenotype_excess_over_space_null),
      "; one-sided P=", sprintf("%.6f", row$posterior_predictive_p_upper),
      "; terms=", row$supported_environment_terms
    )
  }, character(1)),
  "",
  "The final environment + SPDE model was not refitted. It only fixed which standardized environmental terms define distance. Direction remains supplied by the final model coefficients."
)
writeLines(summary_lines, file.path(output_dir, "RESULT_SUMMARY.md"))

cat("Completed supported-term environmental-distance spatial-null test.\n")
print(primary_table)
