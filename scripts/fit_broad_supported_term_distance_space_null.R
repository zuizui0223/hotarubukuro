#!/usr/bin/env Rscript

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
output_dir <- arg_value(
  "--output",
  "results/broad_supported_term_distance_space_null"
)
n_samples <- as.integer(arg_value("--samples", "500"))
seed <- as.integer(arg_value("--seed", "20260725"))
max_pairs_per_fold <- as.integer(arg_value("--max-pairs-per-fold", "15000"))
n_geo_bins <- as.integer(arg_value("--geo-bins", "5"))
inla_verbose <- identical(
  tolower(arg_value("--inla-verbose", "false")),
  "true"
)

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
cells <- utils::read.csv(
  cells_path,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

final_environment_columns <- c(
  "env_Temperature_PC1",
  "env_precip_PC1",
  "env_TemperatureSeasonality",
  "env_topo_PC1"
)

v16_assert_columns(
  cells,
  c(
    "exact_site_id", "x_km", "y_km", "spatial_fold",
    "n_pigmented", "n_observations", "conditional_intensity_median",
    final_environment_columns
  ),
  "cells"
)

cells$int_thermal_variability <-
  cells$env_Temperature_PC1 * cells$env_TemperatureSeasonality

response_registry <- list(
  pigmentation_state = list(
    response = "n_pigmented",
    family = "binomial",
    trials = "n_observations",
    eligible = rep(TRUE, nrow(cells)),
    observed = function(d) d$n_pigmented / d$n_observations,
    environment_terms = c("env_Temperature_PC1"),
    environment_labels = c("Temperature PC1")
  ),
  conditional_intensity = list(
    response = "conditional_intensity_median",
    family = "gaussian",
    trials = NULL,
    eligible = is.finite(cells$conditional_intensity_median),
    observed = function(d) d$conditional_intensity_median,
    environment_terms = c(
      "env_precip_PC1",
      "env_TemperatureSeasonality",
      "env_topo_PC1",
      "int_thermal_variability"
    ),
    environment_labels = c(
      "Precipitation PC1",
      "Temperature seasonality",
      "Topography PC1",
      "Temperature PC1 x temperature seasonality"
    )
  )
)

standardize_terms <- function(train, test, terms) {
  train_matrix <- as.matrix(train[terms])
  test_matrix <- as.matrix(test[terms])
  centre <- colMeans(train_matrix)
  spread <- apply(train_matrix, 2L, stats::sd)
  spread[!is.finite(spread) | spread <= 1e-10] <- 1
  list(
    train = sweep(sweep(train_matrix, 2L, centre, "-"), 2L, spread, "/"),
    test = sweep(sweep(test_matrix, 2L, centre, "-"), 2L, spread, "/"),
    centre = centre,
    spread = spread
  )
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
    mesh,
    loc = as.matrix(train[c("x_km", "y_km")])
  )
  A_test <- INLA::inla.spde.make.A(
    mesh,
    loc = as.matrix(test[c("x_km", "y_km")])
  )
  spde <- v16_make_spde(mesh, A_train)

  estimation_data <- list(y = train[[response]])
  prediction_data <- list(y = rep(NA_real_, nrow(test)))
  if (family == "binomial") {
    estimation_data$Ntrials <- train[[trials]]
    prediction_data$Ntrials <- test[[trials]]
  }

  stack_estimation <- INLA::inla.stack(
    data = estimation_data,
    A = list(A_train, 1),
    effects = list(spatial = seq_len(spde$n.spde), X = train_X),
    tag = "est",
    compress = FALSE,
    remove.unused = FALSE
  )
  stack_prediction <- INLA::inla.stack(
    data = prediction_data,
    A = list(A_test, 1),
    effects = list(spatial = seq_len(spde$n.spde), X = test_X),
    tag = "pred",
    compress = FALSE,
    remove.unused = FALSE
  )

  stack <- INLA::inla.stack(stack_estimation, stack_prediction)
  stack_data <- INLA::inla.stack.data(stack)
  formula <- stats::as.formula(
    "y ~ -1 + Intercept + f(spatial, model = spde)",
    env = environment()
  )

  inla_args <- list(
    formula = formula,
    data = stack_data,
    family = family,
    control.predictor = list(
      A = INLA::inla.stack.A(stack),
      compute = TRUE,
      link = 1
    ),
    control.compute = list(config = TRUE),
    verbose = inla_verbose
  )
  if (family == "binomial") inla_args$Ntrials <- stack_data$Ntrials

  fit <- do.call(INLA::inla, inla_args)
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
    samples[[1L]],
    prediction_index,
    "APredictor"
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
      stats::rbinom(
        nrow(test),
        size = trials_test,
        prob = probability[, k]
      ) / trials_test
    }, numeric(nrow(test)))
    expected <- rowMeans(probability)
  } else {
    precision <- vapply(samples, v16_observation_precision, numeric(1))
    simulated <- vapply(seq_len(n_samples), function(k) {
      stats::rnorm(
        nrow(test),
        mean = eta[, k],
        sd = sqrt(1 / precision[[k]])
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
    a <- sample.int(n, draw_n, replace = TRUE)
    b <- sample.int(n, draw_n, replace = TRUE)
    keep <- a != b
    if (!any(keep)) next
    candidate <- cbind(
      pmin(a[keep], b[keep]),
      pmax(a[keep], b[keep])
    )
    pairs <- unique(rbind(pairs, candidate))
    if (nrow(pairs) > target) {
      pairs <- pairs[seq_len(target), , drop = FALSE]
    }
  }
  pairs
}

assign_equal_count_bins <- function(x, n_bins) {
  ranks <- rank(x, ties.method = "average")
  pmin(
    n_bins,
    pmax(1L, as.integer(ceiling(ranks / length(x) * n_bins)))
  )
}

pair_geo_distance <- function(test, pairs) {
  dx <- test$x_km[pairs[, 1L]] - test$x_km[pairs[, 2L]]
  dy <- test$y_km[pairs[, 1L]] - test$y_km[pairs[, 2L]]
  sqrt(dx^2 + dy^2)
}

pair_environment_distance <- function(z, pairs) {
  delta <- z[pairs[, 1L], , drop = FALSE] -
    z[pairs[, 2L], , drop = FALSE]
  sqrt(rowSums(delta^2))
}

pair_component_distances <- function(z, pairs, terms) {
  delta <- abs(
    z[pairs[, 1L], , drop = FALSE] -
      z[pairs[, 2L], , drop = FALSE]
  )
  colnames(delta) <- paste0("distance_", terms)
  as.data.frame(delta)
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

contrast_from_groups <- function(observed_div, null_div, geo_bin, env_dist,
                                 response_name, fold) {
  rows <- list()
  null_contrasts <- list()

  for (bin in sort(unique(geo_bin))) {
    index <- which(
      geo_bin == bin &
        is.finite(env_dist) &
        is.finite(observed_div)
    )
    if (length(index) < 8L) next

    cuts <- stats::quantile(
      env_dist[index],
      probs = c(0.25, 0.75),
      names = FALSE,
      type = 7
    )
    low <- index[env_dist[index] <= cuts[[1L]]]
    high <- index[env_dist[index] >= cuts[[2L]]]
    if (length(low) < 2L || length(high) < 2L) next

    observed_contrast <-
      mean(observed_div[high]) - mean(observed_div[low])
    null_contrast <-
      colMeans(null_div[high, , drop = FALSE]) -
      colMeans(null_div[low, , drop = FALSE])

    rows[[length(rows) + 1L]] <- data.frame(
      response = response_name,
      fold = fold,
      geo_bin = bin,
      n_pairs = length(index),
      n_low_environment = length(low),
      n_high_environment = length(high),
      geo_distance_median = NA_real_,
      environment_distance_low_cut = cuts[[1L]],
      environment_distance_high_cut = cuts[[2L]],
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

folds <- sort(unique(as.integer(cells$spatial_fold)))
mesh <- v16_make_mesh(cells)$mesh

pair_rows <- list()
stratum_rows <- list()
heldout_rows <- list()
response_observed_contrasts <- setNames(
  vector("list", length(response_registry)),
  names(response_registry)
)
response_null_contrasts <- setNames(
  vector("list", length(response_registry)),
  names(response_registry)
)

for (response_name in names(response_registry)) {
  specification <- response_registry[[response_name]]
  response_observed_contrasts[[response_name]] <- numeric()
  response_null_contrasts[[response_name]] <- list()

  for (fold in folds) {
    test_index <- which(
      as.integer(cells$spatial_fold) == fold &
        specification$eligible
    )
    train_index <- which(
      as.integer(cells$spatial_fold) != fold &
        specification$eligible
    )
    train <- cells[train_index, , drop = FALSE]
    test <- cells[test_index, , drop = FALSE]
    if (nrow(test) < 4L) next

    message(
      "[supported-term distance] ", response_name,
      " fold=", fold,
      " train=", nrow(train),
      " test=", nrow(test)
    )

    environment_basis <- standardize_terms(
      train,
      test,
      specification$environment_terms
    )
    sampled <- sample_space_null(
      train,
      test,
      specification$response,
      specification$family,
      mesh,
      specification$trials,
      seed + 100000L * match(response_name, names(response_registry)) +
        1000L * fold
    )

    observed <- specification$observed(test)
    pairs <- make_pairs(
      nrow(test),
      max_pairs_per_fold,
      seed + 200000L * match(response_name, names(response_registry)) +
        1000L * fold
    )

    geographic_distance <- pair_geo_distance(test, pairs)
    environmental_distance <- pair_environment_distance(
      environment_basis$test,
      pairs
    )
    component_distance <- pair_component_distances(
      environment_basis$test,
      pairs,
      specification$environment_terms
    )
    observed_divergence <- pair_divergence_vector(observed, pairs)
    null_divergence <- pair_divergence_matrix(sampled$simulated, pairs)
    geo_bin <- assign_equal_count_bins(
      geographic_distance,
      n_geo_bins
    )

    null_pair_median <- apply(
      null_divergence,
      1L,
      stats::median
    )
    null_pair_q025 <- apply(
      null_divergence,
      1L,
      stats::quantile,
      probs = 0.025,
      names = FALSE
    )
    null_pair_q975 <- apply(
      null_divergence,
      1L,
      stats::quantile,
      probs = 0.975,
      names = FALSE
    )

    pair_block <- data.frame(
      response = response_name,
      fold = fold,
      site_i = test$exact_site_id[pairs[, 1L]],
      site_j = test$exact_site_id[pairs[, 2L]],
      geographic_distance_km = geographic_distance,
      supported_term_environmental_distance = environmental_distance,
      observed_phenotype_divergence = observed_divergence,
      space_null_median_divergence = null_pair_median,
      space_null_q025 = null_pair_q025,
      space_null_q975 = null_pair_q975,
      phenotype_excess = observed_divergence - null_pair_median,
      above_space_null_q975 = observed_divergence > null_pair_q975,
      geo_bin = geo_bin,
      stringsAsFactors = FALSE
    )
    pair_block <- cbind(pair_block, component_distance)
    pair_rows[[length(pair_rows) + 1L]] <- pair_block

    contrasted <- contrast_from_groups(
      observed_divergence,
      null_divergence,
      geo_bin,
      environmental_distance,
      response_name,
      fold
    )
    if (nrow(contrasted$summary)) {
      for (row_index in seq_len(nrow(contrasted$summary))) {
        bin <- contrasted$summary$geo_bin[[row_index]]
        contrasted$summary$geo_distance_median[[row_index]] <-
          stats::median(geographic_distance[geo_bin == bin])
      }
      stratum_rows[[length(stratum_rows) + 1L]] <-
        contrasted$summary
      response_observed_contrasts[[response_name]] <- c(
        response_observed_contrasts[[response_name]],
        contrasted$summary$observed_high_minus_low
      )
      response_null_contrasts[[response_name]] <- c(
        response_null_contrasts[[response_name]],
        contrasted$null
      )
    }

    heldout_rows[[length(heldout_rows) + 1L]] <- data.frame(
      response = response_name,
      fold = fold,
      exact_site_id = test$exact_site_id,
      observed = observed,
      space_null_expected = sampled$expected,
      stringsAsFactors = FALSE
    )

    rm(sampled, null_divergence)
    invisible(gc())
  }
}

pair_table <- do.call(rbind, pair_rows)
stratum_table <- do.call(rbind, stratum_rows)
heldout_table <- do.call(rbind, heldout_rows)

primary_rows <- list()
null_draw_rows <- list()

for (response_name in names(response_registry)) {
  observed_components <- response_observed_contrasts[[response_name]]
  null_components <- response_null_contrasts[[response_name]]
  if (!length(observed_components) || !length(null_components)) {
    stop(
      "No valid strata for ", response_name,
      call. = FALSE
    )
  }

  null_matrix <- do.call(rbind, null_components)
  null_global <- colMeans(null_matrix)
  observed_global <- mean(observed_components)
  p_upper <-
    (1 + sum(null_global >= observed_global)) /
    (length(null_global) + 1)

  primary_rows[[length(primary_rows) + 1L]] <- data.frame(
    response = response_name,
    environmental_terms = paste(
      response_registry[[response_name]]$environment_labels,
      collapse = ";"
    ),
    n_fold_geo_strata = length(observed_components),
    observed_contrast = observed_global,
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
  file.path(output_dir, "primary_supported_term_distance_test.csv"),
  row.names = FALSE
)
utils::write.csv(
  stratum_table,
  file.path(output_dir, "matched_distance_stratum_contrasts.csv"),
  row.names = FALSE
)
utils::write.csv(
  pair_table,
  file.path(output_dir, "heldout_pair_supported_term_distance.csv"),
  row.names = FALSE
)
utils::write.csv(
  heldout_table,
  file.path(output_dir, "heldout_space_null_predictions.csv"),
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
    "source_cells",
    "space_null",
    "cross_fitting",
    "spatial_folds",
    "posterior_samples",
    "seed",
    "max_pairs_per_fold",
    "geographic_distance_bins",
    "pigmentation_state_environment_terms",
    "conditional_intensity_environment_terms",
    "environmental_distance",
    "primary_estimand",
    "claim_boundary"
  ),
  value = c(
    "broad_supported_term_environmental_distance_beyond_space",
    cells_path,
    "intercept + Matern SPDE only",
    "all tested pairs lie wholly inside held-out geographical folds",
    paste(folds, collapse = ";"),
    n_samples,
    seed,
    max_pairs_per_fold,
    n_geo_bins,
    paste(
      response_registry$pigmentation_state$environment_terms,
      collapse = ";"
    ),
    paste(
      response_registry$conditional_intensity$environment_terms,
      collapse = ";"
    ),
    "Euclidean distance across training-fold-standardized environmental terms retained by the final response-specific model; no coefficient weighting and no refitting of the full model",
    "mean across fold-by-geographical-distance strata of observed high-minus-low supported-term environmental-distance phenotype divergence, compared with the identical statistic under cross-fitted space-only posterior prediction",
    "model-informed supporting test; not independent variable discovery, selection, local adaptation, genetic differentiation, drift, or direct anthocyanin physiology"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  metadata,
  file.path(output_dir, "analysis_metadata.csv"),
  row.names = FALSE
)

summary_lines <- c(
  "# Supported environmental-term distance versus spatial continuity",
  "",
  "The final observation-level model remains primary. This supporting analysis uses only pairwise environmental distances among the terms whose final-model credible intervals exclude zero; it does not rebuild or coefficient-weight the full model.",
  ""
)
for (row_index in seq_len(nrow(primary_table))) {
  row <- primary_table[row_index, ]
  summary_lines <- c(
    summary_lines,
    paste0(
      "- ", row$response,
      ": terms=", row$environmental_terms,
      "; observed contrast=", sprintf("%.6f", row$observed_contrast),
      "; space-only median=", sprintf("%.6f", row$space_null_median),
      "; excess=", sprintf(
        "%.6f", row$phenotype_excess_over_space_null
      ),
      "; one-sided P=", sprintf(
        "%.6f", row$posterior_predictive_p_upper
      )
    )
  )
}
summary_lines <- c(
  summary_lines,
  "",
  "Interpretation boundary: a positive excess shows that separation along the final model's supported environmental terms orders held-out phenotype divergence beyond fitted spatial continuity. It does not identify a causal mechanism or demonstrate adaptation."
)
writeLines(
  summary_lines,
  file.path(output_dir, "RESULT_SUMMARY.md")
)

cat("Completed supported-term environmental-distance analysis.\n")
print(primary_table)
