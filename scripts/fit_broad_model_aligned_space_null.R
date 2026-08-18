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
  "--output",
  "results/broad_model_aligned_space_null"
)
n_samples <- as.integer(arg_value("--samples", "500"))
seed <- as.integer(arg_value("--seed", "20260725"))
max_pairs_per_fold <- as.integer(arg_value("--max-pairs-per-fold", "15000"))
n_geo_bins <- as.integer(arg_value("--geo-bins", "5"))
inla_verbose <- identical(tolower(arg_value("--inla-verbose", "false")), "true")

if (!file.exists(observations_path)) {
  stop("Missing observation table: ", observations_path, call. = FALSE)
}
if (!file.exists(cells_path)) {
  stop("Missing cell table: ", cells_path, call. = FALSE)
}
if (!is.finite(n_samples) || n_samples < 100L) {
  stop("--samples must be >= 100", call. = FALSE)
}
if (!is.finite(max_pairs_per_fold) || max_pairs_per_fold < 100L) {
  stop("--max-pairs-per-fold must be >= 100", call. = FALSE)
}
if (!is.finite(n_geo_bins) || n_geo_bins < 2L) {
  stop("--geo-bins must be >= 2", call. = FALSE)
}
if (!requireNamespace("INLA", quietly = TRUE)) {
  stop("Package 'INLA' is required.", call. = FALSE)
}
if (!requireNamespace("Matrix", quietly = TRUE)) {
  stop("Package 'Matrix' is required.", call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
observations <- utils::read.csv(
  observations_path, check.names = FALSE, stringsAsFactors = FALSE
)
cells <- utils::read.csv(
  cells_path, check.names = FALSE, stringsAsFactors = FALSE
)

base_terms <- c(
  "env_Temperature_PC1",
  "env_precip_PC1",
  "env_TemperatureSeasonality",
  "env_PrecipSeasonality",
  "env_topo_PC1",
  "env_soil_PC1",
  "env_soil_PC2",
  "env_RSDS"
)
interaction_term <- "int_thermal_variability"

v16_assert_columns(
  observations,
  c(
    "observation_id", "x_km", "y_km", "region", "spatial_fold",
    "pigmented_mixture50", "pigment_intensity_z", base_terms
  ),
  "observations"
)
v16_assert_columns(
  cells,
  c(
    "exact_site_id", "x_km", "y_km", "spatial_fold",
    "n_pigmented", "n_observations", "conditional_intensity_median",
    base_terms
  ),
  "cells"
)

observations$region <- factor(observations$region, levels = c("West", "East"))
if (anyNA(observations$region)) {
  stop("Observation region contains values outside West/East.", call. = FALSE)
}

interaction_raw <- as.numeric(
  observations$env_Temperature_PC1 * observations$env_TemperatureSeasonality
)
interaction_centre <- mean(interaction_raw, na.rm = TRUE)
interaction_spread <- stats::sd(interaction_raw, na.rm = TRUE)
if (!is.finite(interaction_spread) || interaction_spread <= 1e-10) {
  stop("Thermal-variability interaction has zero/invalid spread.", call. = FALSE)
}
observations[[interaction_term]] <-
  (interaction_raw - interaction_centre) / interaction_spread
cells[[interaction_term]] <- (
  cells$env_Temperature_PC1 * cells$env_TemperatureSeasonality -
    interaction_centre
) / interaction_spread

make_fixed <- function(data, terms, include_region = TRUE) {
  out <- data.frame(intercept = rep(1, nrow(data)))
  if (include_region) {
    out$regionEast <- as.integer(data$region == "East")
  }
  for (term in terms) out[[term]] <- as.numeric(data[[term]])
  out
}

build_observation_spde <- function(mesh) {
  INLA::inla.spde2.pcmatern(
    mesh,
    alpha = 2,
    prior.range = c(100, 0.05),
    prior.sigma = c(1, 0.05)
  )
}

fit_environment_spde <- function(data, response, family, terms, mesh,
                                 train_rows, label) {
  fixed <- make_fixed(data, terms, include_region = TRUE)
  A <- INLA::inla.spde.make.A(
    mesh, loc = as.matrix(data[c("x_km", "y_km")])
  )
  spatial_model <- build_observation_spde(mesh)
  spatial_index <- list(spatial_field = seq_len(mesh$n))
  estimation_stack <- INLA::inla.stack(
    data = list(y = data[[response]][train_rows]),
    A = list(1, A[train_rows, , drop = FALSE]),
    effects = list(
      fixed[train_rows, , drop = FALSE],
      spatial_index
    ),
    tag = "est",
    compress = FALSE,
    remove.unused = FALSE
  )
  stack_data <- INLA::inla.stack.data(estimation_stack)
  rhs <- paste(
    c(names(fixed), "f(spatial_field, model = spatial_model)"),
    collapse = " + "
  )
  formula <- stats::as.formula(
    paste0("y ~ -1 + ", rhs), env = environment()
  )
  message(
    "[model-aligned full model] ", label,
    " train=", length(train_rows)
  )
  fit <- INLA::inla(
    formula,
    family = family,
    data = stack_data,
    control.predictor = list(
      A = INLA::inla.stack.A(estimation_stack),
      compute = TRUE,
      link = 1
    ),
    control.compute = list(
      dic = TRUE,
      waic = TRUE,
      cpo = TRUE,
      config = FALSE
    ),
    control.inla = list(strategy = "adaptive"),
    num.threads = "1:1",
    verbose = inla_verbose
  )
  coefficient_table <- fit$summary.fixed
  coefficients <- coefficient_table[, "mean"]
  names(coefficients) <- rownames(coefficient_table)
  list(
    fit = fit,
    coefficients = coefficients,
    coefficient_table = coefficient_table,
    formula = paste(deparse(formula), collapse = " ")
  )
}

score_cells <- function(data, coefficients, score_terms) {
  missing <- setdiff(score_terms, names(coefficients))
  if (length(missing)) {
    stop(
      "Fitted model lacks score coefficient(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  score <- rep(0, nrow(data))
  for (term in score_terms) {
    score <- score + as.numeric(coefficients[[term]]) * as.numeric(data[[term]])
  }
  score
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
  A_train <- INLA::inla.spde.make.A(
    mesh, loc = as.matrix(train[c("x_km", "y_km")])
  )
  A_test <- INLA::inla.spde.make.A(
    mesh, loc = as.matrix(test[c("x_km", "y_km")])
  )
  spde <- v16_make_spde(mesh, A_train)
  estimation_data <- list(y = train[[response]])
  prediction_data <- list(y = rep(NA_real_, nrow(test)))
  if (family == "binomial") {
    estimation_data$Ntrials <- train[[trials]]
    prediction_data$Ntrials <- test[[trials]]
  }
  estimation_stack <- INLA::inla.stack(
    data = estimation_data,
    A = list(A_train, 1),
    effects = list(spatial = seq_len(spde$n.spde), X = train_X),
    tag = "est",
    compress = FALSE,
    remove.unused = FALSE
  )
  prediction_stack <- INLA::inla.stack(
    data = prediction_data,
    A = list(A_test, 1),
    effects = list(spatial = seq_len(spde$n.spde), X = test_X),
    tag = "pred",
    compress = FALSE,
    remove.unused = FALSE
  )
  stack <- INLA::inla.stack(estimation_stack, prediction_stack)
  stack_data <- INLA::inla.stack.data(stack)
  formula <- stats::as.formula(
    "y ~ -1 + Intercept + f(spatial, model = spde)",
    env = environment()
  )
  arguments <- list(
    formula = formula,
    data = stack_data,
    family = family,
    control.predictor = list(
      A = INLA::inla.stack.A(stack), compute = TRUE, link = 1
    ),
    control.compute = list(config = TRUE),
    verbose = inla_verbose
  )
  if (family == "binomial") arguments$Ntrials <- stack_data$Ntrials
  fit <- do.call(INLA::inla, arguments)
  prediction_index <- INLA::inla.stack.index(stack, "pred")$data
  samples <- INLA::inla.posterior.sample(
    n = n_samples,
    result = fit,
    selection = list(APredictor = prediction_index),
    seed = as.integer(seed_value),
    num.threads = 1,
    parallel.configs = FALSE,
    add.names = TRUE
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
    simulated <- vapply(seq_len(n_samples), function(index) {
      stats::rbinom(
        nrow(test), size = trials_test, prob = probability[, index]
      ) / trials_test
    }, numeric(nrow(test)))
    expected <- rowMeans(probability)
  } else {
    precision <- vapply(samples, v16_observation_precision, numeric(1))
    simulated <- vapply(seq_len(n_samples), function(index) {
      stats::rnorm(
        nrow(test),
        mean = eta[, index],
        sd = sqrt(1 / precision[[index]])
      )
    }, numeric(nrow(test)))
    expected <- rowMeans(eta)
  }
  if (!is.matrix(simulated)) {
    simulated <- matrix(simulated, nrow = nrow(test))
  }
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
    first <- sample.int(n, draw_n, replace = TRUE)
    second <- sample.int(n, draw_n, replace = TRUE)
    keep <- first != second
    if (!any(keep)) next
    candidate <- cbind(
      pmin(first[keep], second[keep]),
      pmax(first[keep], second[keep])
    )
    pairs <- unique(rbind(pairs, candidate))
    if (nrow(pairs) > target) {
      pairs <- pairs[seq_len(target), , drop = FALSE]
    }
  }
  pairs
}

assign_equal_count_bins <- function(values, n_bins) {
  if (!length(values)) return(integer())
  ranks <- rank(values, ties.method = "average")
  pmin(
    n_bins,
    pmax(1L, as.integer(ceiling(ranks / length(values) * n_bins)))
  )
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
  abs(
    values[pairs[, 1L], , drop = FALSE] -
      values[pairs[, 2L], , drop = FALSE]
  )
}

contrast_from_groups <- function(observed_divergence, null_divergence,
                                 geographic_bin, score_distance,
                                 response_name, fold) {
  rows <- list()
  null_contrasts <- list()
  for (bin in sort(unique(geographic_bin))) {
    index <- which(
      geographic_bin == bin &
        is.finite(score_distance) &
        is.finite(observed_divergence)
    )
    if (length(index) < 8L) next
    cuts <- stats::quantile(
      score_distance[index],
      probs = c(0.25, 0.75),
      names = FALSE,
      type = 7
    )
    low <- index[score_distance[index] <= cuts[[1L]]]
    high <- index[score_distance[index] >= cuts[[2L]]]
    if (length(low) < 2L || length(high) < 2L) next
    observed_contrast <-
      mean(observed_divergence[high]) - mean(observed_divergence[low])
    null_contrast <-
      colMeans(null_divergence[high, , drop = FALSE]) -
      colMeans(null_divergence[low, , drop = FALSE])
    rows[[length(rows) + 1L]] <- data.frame(
      response = response_name,
      fold = fold,
      geo_bin = bin,
      n_pairs = length(index),
      n_low_score_distance = length(low),
      n_high_score_distance = length(high),
      geographic_distance_median_km = NA_real_,
      score_distance_low_cut = cuts[[1L]],
      score_distance_high_cut = cuts[[2L]],
      observed_high_minus_low = observed_contrast,
      space_null_median = stats::median(null_contrast),
      space_null_q025 = stats::quantile(
        null_contrast, 0.025, names = FALSE
      ),
      space_null_q975 = stats::quantile(
        null_contrast, 0.975, names = FALSE
      ),
      excess_over_space_null_median =
        observed_contrast - stats::median(null_contrast),
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
    observation_response = "pigmented_mixture50",
    observation_family = "binomial",
    observation_eligible = is.finite(observations$pigmented_mixture50),
    full_terms = base_terms,
    score_terms = c("env_Temperature_PC1"),
    cell_response = "n_pigmented",
    cell_family = "binomial",
    cell_trials = "n_observations",
    cell_eligible = rep(TRUE, nrow(cells)),
    observed_cell = function(data) data$n_pigmented / data$n_observations
  ),
  conditional_intensity = list(
    observation_response = "pigment_intensity_z",
    observation_family = "gaussian",
    observation_eligible = is.finite(observations$pigment_intensity_z),
    full_terms = c(base_terms, interaction_term),
    score_terms = c(
      "env_Temperature_PC1",
      "env_precip_PC1",
      "env_TemperatureSeasonality",
      "env_topo_PC1",
      interaction_term
    ),
    cell_response = "conditional_intensity_median",
    cell_family = "gaussian",
    cell_trials = NULL,
    cell_eligible = is.finite(cells$conditional_intensity_median),
    observed_cell = function(data) data$conditional_intensity_median
  )
)

folds <- sort(unique(as.integer(cells$spatial_fold)))
if (!identical(folds, sort(unique(as.integer(observations$spatial_fold))))) {
  stop("Observation and cell fold identities differ.", call. = FALSE)
}
cell_mesh <- v16_make_mesh(cells)$mesh

full_data_coefficient_rows <- list()
crossfit_coefficient_rows <- list()
heldout_score_rows <- list()
heldout_space_rows <- list()
pair_rows <- list()
stratum_rows <- list()
response_observed_contrasts <- setNames(
  vector("list", length(responses)), names(responses)
)
response_null_contrasts <- setNames(
  vector("list", length(responses)), names(responses)
)
fit_log_rows <- list()

for (response_name in names(responses)) {
  specification <- responses[[response_name]]
  eligible_observations <- which(
    specification$observation_eligible &
      stats::complete.cases(
        observations[c(
          specification$observation_response,
          specification$full_terms,
          "region", "x_km", "y_km", "spatial_fold"
        )]
      )
  )
  observation_mesh <- v16_make_mesh(
    observations[eligible_observations, , drop = FALSE]
  )$mesh

  full_fit <- fit_environment_spde(
    observations,
    specification$observation_response,
    specification$observation_family,
    specification$full_terms,
    observation_mesh,
    eligible_observations,
    paste0(response_name, " full-data")
  )
  full_table <- full_fit$coefficient_table
  full_data_coefficient_rows[[length(full_data_coefficient_rows) + 1L]] <-
    data.frame(
      response = response_name,
      term = rownames(full_table),
      mean = full_table[, "mean"],
      sd = full_table[, "sd"],
      lower_95 = full_table[, "0.025quant"],
      upper_95 = full_table[, "0.975quant"],
      used_in_environment_score =
        rownames(full_table) %in% specification$score_terms,
      stringsAsFactors = FALSE
    )
  rm(full_fit)
  invisible(gc())

  response_observed_contrasts[[response_name]] <- numeric()
  response_null_contrasts[[response_name]] <- list()

  for (fold in folds) {
    observation_train_rows <- eligible_observations[
      as.integer(observations$spatial_fold[eligible_observations]) != fold
    ]
    cell_test_index <- which(
      as.integer(cells$spatial_fold) == fold &
        specification$cell_eligible
    )
    cell_train_index <- which(
      as.integer(cells$spatial_fold) != fold &
        specification$cell_eligible
    )
    train_cells <- cells[cell_train_index, , drop = FALSE]
    test_cells <- cells[cell_test_index, , drop = FALSE]
    if (nrow(test_cells) < 4L) next

    environment_fit <- fit_environment_spde(
      observations,
      specification$observation_response,
      specification$observation_family,
      specification$full_terms,
      observation_mesh,
      observation_train_rows,
      paste0(response_name, " fold=", fold)
    )
    score <- score_cells(
      test_cells,
      environment_fit$coefficients,
      specification$score_terms
    )
    coefficient_table <- environment_fit$coefficient_table
    crossfit_coefficient_rows[[length(crossfit_coefficient_rows) + 1L]] <-
      data.frame(
        response = response_name,
        heldout_fold = fold,
        term = rownames(coefficient_table),
        mean = coefficient_table[, "mean"],
        sd = coefficient_table[, "sd"],
        lower_95 = coefficient_table[, "0.025quant"],
        upper_95 = coefficient_table[, "0.975quant"],
        used_in_environment_score =
          rownames(coefficient_table) %in% specification$score_terms,
        stringsAsFactors = FALSE
      )

    message(
      "[model-aligned space null] ", response_name,
      " fold=", fold,
      " cell_train=", nrow(train_cells),
      " cell_test=", nrow(test_cells)
    )
    sampled <- sample_space_null(
      train_cells,
      test_cells,
      specification$cell_response,
      specification$cell_family,
      cell_mesh,
      specification$cell_trials,
      seed + 100000L * match(response_name, names(responses)) +
        1000L * fold
    )
    observed <- specification$observed_cell(test_cells)
    pairs <- make_pairs(
      nrow(test_cells),
      max_pairs_per_fold,
      seed + 200000L * match(response_name, names(responses)) +
        1000L * fold
    )
    geographic_distance <- pair_geo_distance(test_cells, pairs)
    score_distance <- pair_divergence_vector(score, pairs)
    observed_divergence <- pair_divergence_vector(observed, pairs)
    null_divergence <- pair_divergence_matrix(sampled$simulated, pairs)
    null_pair_median <- apply(
      null_divergence, 1L, stats::median
    )
    null_pair_q025 <- apply(
      null_divergence, 1L, stats::quantile,
      probs = 0.025, names = FALSE
    )
    null_pair_q975 <- apply(
      null_divergence, 1L, stats::quantile,
      probs = 0.975, names = FALSE
    )
    geographic_bin <- assign_equal_count_bins(
      geographic_distance, n_geo_bins
    )

    pair_rows[[length(pair_rows) + 1L]] <- data.frame(
      response = response_name,
      fold = fold,
      site_i = test_cells$exact_site_id[pairs[, 1L]],
      site_j = test_cells$exact_site_id[pairs[, 2L]],
      geographic_distance_km = geographic_distance,
      environment_score_i = score[pairs[, 1L]],
      environment_score_j = score[pairs[, 2L]],
      model_environment_score_distance = score_distance,
      observed_phenotype_divergence = observed_divergence,
      space_null_median_divergence = null_pair_median,
      space_null_q025 = null_pair_q025,
      space_null_q975 = null_pair_q975,
      phenotype_excess = observed_divergence - null_pair_median,
      above_space_null_q975 = observed_divergence > null_pair_q975,
      geo_bin = geographic_bin,
      stringsAsFactors = FALSE
    )

    contrasted <- contrast_from_groups(
      observed_divergence,
      null_divergence,
      geographic_bin,
      score_distance,
      response_name,
      fold
    )
    if (nrow(contrasted$summary)) {
      for (row_index in seq_len(nrow(contrasted$summary))) {
        bin <- contrasted$summary$geo_bin[[row_index]]
        contrasted$summary$geographic_distance_median_km[[row_index]] <-
          stats::median(geographic_distance[geographic_bin == bin])
      }
      stratum_rows[[length(stratum_rows) + 1L]] <- contrasted$summary
      response_observed_contrasts[[response_name]] <- c(
        response_observed_contrasts[[response_name]],
        contrasted$summary$observed_high_minus_low
      )
      response_null_contrasts[[response_name]] <- c(
        response_null_contrasts[[response_name]],
        contrasted$null
      )
    }

    heldout_score_rows[[length(heldout_score_rows) + 1L]] <- data.frame(
      response = response_name,
      fold = fold,
      exact_site_id = test_cells$exact_site_id,
      observed = observed,
      model_environment_score = score,
      score_terms = paste(specification$score_terms, collapse = ";"),
      stringsAsFactors = FALSE
    )
    heldout_space_rows[[length(heldout_space_rows) + 1L]] <- data.frame(
      response = response_name,
      fold = fold,
      exact_site_id = test_cells$exact_site_id,
      observed = observed,
      space_null_expected = sampled$expected,
      stringsAsFactors = FALSE
    )
    fit_log_rows[[length(fit_log_rows) + 1L]] <- data.frame(
      response = response_name,
      heldout_fold = fold,
      n_observation_train = length(observation_train_rows),
      n_cell_train = nrow(train_cells),
      n_cell_test = nrow(test_cells),
      n_pairs = nrow(pairs),
      full_model_formula = environment_fit$formula,
      score_terms = paste(specification$score_terms, collapse = ";"),
      stringsAsFactors = FALSE
    )

    rm(environment_fit, sampled, null_divergence)
    invisible(gc())
  }
}

full_data_coefficients <- do.call(rbind, full_data_coefficient_rows)
crossfit_coefficients <- do.call(rbind, crossfit_coefficient_rows)
heldout_scores <- do.call(rbind, heldout_score_rows)
heldout_space <- do.call(rbind, heldout_space_rows)
pair_table <- do.call(rbind, pair_rows)
stratum_table <- do.call(rbind, stratum_rows)
fit_log <- do.call(rbind, fit_log_rows)

primary_rows <- list()
null_draw_rows <- list()
for (response_name in names(responses)) {
  observed_components <- response_observed_contrasts[[response_name]]
  null_components <- response_null_contrasts[[response_name]]
  if (!length(observed_components) || !length(null_components)) {
    stop("No valid matched-distance strata for ", response_name, call. = FALSE)
  }
  null_matrix <- do.call(rbind, null_components)
  null_global <- colMeans(null_matrix)
  observed_global <- mean(observed_components)
  p_upper <-
    (1 + sum(null_global >= observed_global)) /
    (length(null_global) + 1)
  primary_rows[[length(primary_rows) + 1L]] <- data.frame(
    response = response_name,
    estimand = paste0(
      "matched_geographic_distance_high_minus_low_crossfitted_",
      "final_model_environment_score_phenotype_divergence"
    ),
    score_terms = paste(
      responses[[response_name]]$score_terms, collapse = ";"
    ),
    n_fold_geo_strata = length(observed_components),
    observed_contrast = observed_global,
    space_null_mean = mean(null_global),
    space_null_median = stats::median(null_global),
    space_null_q025 = stats::quantile(
      null_global, 0.025, names = FALSE
    ),
    space_null_q975 = stats::quantile(
      null_global, 0.975, names = FALSE
    ),
    phenotype_excess_over_space_null =
      observed_global - stats::median(null_global),
    posterior_predictive_p_upper = p_upper,
    exceeds_space_null_q975 =
      observed_global > stats::quantile(
        null_global, 0.975, names = FALSE
      ),
    stringsAsFactors = FALSE
  )
  null_draw_rows[[length(null_draw_rows) + 1L]] <- data.frame(
    response = response_name,
    draw = seq_along(null_global),
    space_null_global_contrast = null_global,
    stringsAsFactors = FALSE
  )
}
primary_table <- do.call(rbind, primary_rows)
null_draw_table <- do.call(rbind, null_draw_rows)

utils::write.csv(
  primary_table,
  file.path(output_dir, "primary_model_aligned_space_null_test.csv"),
  row.names = FALSE
)
utils::write.csv(
  stratum_table,
  file.path(output_dir, "matched_distance_stratum_contrasts.csv"),
  row.names = FALSE
)
utils::write.csv(
  pair_table,
  file.path(output_dir, "heldout_pair_model_score_excess.csv"),
  row.names = FALSE
)
utils::write.csv(
  heldout_scores,
  file.path(output_dir, "heldout_model_environment_scores.csv"),
  row.names = FALSE
)
utils::write.csv(
  heldout_space,
  file.path(output_dir, "heldout_space_null_predictions.csv"),
  row.names = FALSE
)
utils::write.csv(
  full_data_coefficients,
  file.path(output_dir, "full_data_fixed_effects.csv"),
  row.names = FALSE
)
utils::write.csv(
  crossfit_coefficients,
  file.path(output_dir, "crossfit_fixed_effects.csv"),
  row.names = FALSE
)
utils::write.csv(
  fit_log,
  file.path(output_dir, "fit_log.csv"),
  row.names = FALSE
)
utils::write.csv(
  null_draw_table,
  file.path(output_dir, "space_null_global_contrast_draws.csv"),
  row.names = FALSE
)

metadata <- data.frame(
  field = c(
    "analysis",
    "observations_source",
    "cells_source",
    "full_model",
    "score_selection",
    "pigmentation_state_score_terms",
    "conditional_intensity_score_terms",
    "interaction_scaling_centre",
    "interaction_scaling_spread",
    "cross_fitting",
    "space_null",
    "spatial_folds",
    "posterior_samples",
    "seed",
    "max_pairs_per_fold",
    "geographic_distance_bins",
    "primary_estimand",
    "claim_boundary"
  ),
  value = c(
    "broad_model_aligned_environment_score_beyond_space",
    observations_path,
    cells_path,
    paste0(
      "final eight-axis environment + East/West + stationary Matern SPDE; ",
      "intensity additionally includes standardized Temperature PC1 x ",
      "temperature-seasonality"
    ),
    paste0(
      "score terms fixed from full-data final-model 95% credible intervals; ",
      "temperature main effect retained with the intensity interaction by ",
      "hierarchy"
    ),
    paste(responses$pigmentation_state$score_terms, collapse = ";"),
    paste(responses$conditional_intensity$score_terms, collapse = ";"),
    format(interaction_centre, digits = 17),
    format(interaction_spread, digits = 17),
    paste0(
      "environment coefficients fitted without the held-out geographical ",
      "fold; phenotype divergence evaluated only among held-out cells"
    ),
    "intercept + Matern SPDE only at the 1-km-cell level",
    paste(folds, collapse = ";"),
    n_samples,
    seed,
    max_pairs_per_fold,
    n_geo_bins,
    paste0(
      "mean across fold-by-geographic-distance strata of observed phenotype ",
      "divergence for upper versus lower quartiles of absolute difference in ",
      "the cross-fitted environmental fixed-effect score, compared with the ",
      "same statistic under space-only posterior prediction"
    ),
    paste0(
      "post-selection model-aligned corroboration; not independent model ",
      "validation, selection, local adaptation, plasticity, genetics, or ",
      "direct anthocyanin physiology"
    )
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  metadata,
  file.path(output_dir, "analysis_metadata.csv"),
  row.names = FALSE
)

summary_lines <- c(
  "# Model-aligned environmental score beyond spatial continuity",
  "",
  paste0("Date: ", Sys.Date()),
  "",
  "The environment score is built from the supported environmental terms of each final observation-level environment + SPDE model. Coefficients are re-estimated without each held-out geographical fold. The test then asks whether held-out cell pairs that differ more on that score are more phenotypically divergent than a cross-fitted space-only SPDE predicts at comparable geographical separation.",
  "",
  "| Response | Observed contrast | Space-null median | Excess | One-sided P |",
  "|---|---:|---:|---:|---:|"
)
for (row_index in seq_len(nrow(primary_table))) {
  row <- primary_table[row_index, ]
  summary_lines <- c(
    summary_lines,
    sprintf(
      "| %s | %.6f | %.6f | %+.6f | %.5f |",
      row$response,
      row$observed_contrast,
      row$space_null_median,
      row$phenotype_excess_over_space_null,
      row$posterior_predictive_p_upper
    )
  )
}
summary_lines <- c(
  summary_lines,
  "",
  "## Score definitions",
  "",
  paste0(
    "- Pigmentation state: ",
    paste(responses$pigmentation_state$score_terms, collapse = " + ")
  ),
  paste0(
    "- Conditional intensity: ",
    paste(responses$conditional_intensity$score_terms, collapse = " + ")
  ),
  "",
  "The score-distance test is unsigned. Directional ecological interpretation comes from the full-model coefficient signs. The analysis is a model-aligned supporting test because the score terms were selected from the final full-data model."
)
writeLines(
  summary_lines,
  con = file.path(output_dir, "RESULT_SUMMARY.md")
)

cat("Completed model-aligned environmental-score spatial-null test.\n")
print(primary_table)
