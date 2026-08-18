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
  "results/broad_final8_axis_space_null_attribution"
)
n_samples <- as.integer(arg_value("--samples", "500"))
seed <- as.integer(arg_value("--seed", "20260725"))
max_pairs_per_fold <- as.integer(arg_value("--max-pairs-per-fold", "15000"))
n_geo_bins <- as.integer(arg_value("--geo-bins", "5"))
inla_verbose <- identical(tolower(arg_value("--inla-verbose", "false")), "true")

if (!file.exists(cells_path)) stop("Missing cell table: ", cells_path, call. = FALSE)
if (!is.finite(n_samples) || n_samples < 100L) stop("--samples must be >= 100", call. = FALSE)
if (!is.finite(max_pairs_per_fold) || max_pairs_per_fold < 100L) {
  stop("--max-pairs-per-fold must be >= 100", call. = FALSE)
}
if (!is.finite(n_geo_bins) || n_geo_bins < 2L) stop("--geo-bins must be >= 2", call. = FALSE)
if (!requireNamespace("INLA", quietly = TRUE)) stop("Package 'INLA' is required.", call. = FALSE)
if (!requireNamespace("Matrix", quietly = TRUE)) stop("Package 'Matrix' is required.", call. = FALSE)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)

final8_terms <- c(
  "env_Temperature_PC1",
  "env_precip_PC1",
  "env_TemperatureSeasonality",
  "env_PrecipSeasonality",
  "env_topo_PC1",
  "env_soil_PC1",
  "env_soil_PC2",
  "env_RSDS"
)
final8_labels <- c(
  env_Temperature_PC1 = "Temperature PC1",
  env_precip_PC1 = "Precipitation PC1",
  env_TemperatureSeasonality = "Temperature seasonality",
  env_PrecipSeasonality = "Precipitation seasonality",
  env_topo_PC1 = "Topography PC1",
  env_soil_PC1 = "Soil PC1",
  env_soil_PC2 = "Soil PC2",
  env_RSDS = "RSDS"
)

v16_assert_columns(
  cells,
  c(
    "exact_site_id", "x_km", "y_km", "spatial_fold", "n_pigmented",
    "n_observations", "conditional_intensity_median", final8_terms
  ),
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
  if (!length(x)) return(integer())
  ranks <- rank(x, ties.method = "average")
  pmin(n_bins, pmax(1L, as.integer(ceiling(ranks / length(x) * n_bins))))
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

pair_environment_components <- function(z, pairs) {
  delta <- abs(
    as.matrix(z[pairs[, 1L], , drop = FALSE]) -
      as.matrix(z[pairs[, 2L], , drop = FALSE])
  )
  colnames(delta) <- final8_terms
  delta
}

contrast_from_groups <- function(observed_div, null_div, geo_bin, env_dist,
                                 response_name, fold, component) {
  rows <- list()
  null_contrasts <- list()
  for (bin in sort(unique(geo_bin))) {
    idx <- which(geo_bin == bin & is.finite(env_dist) & is.finite(observed_div))
    if (length(idx) < 8L) next
    cuts <- stats::quantile(
      env_dist[idx],
      probs = c(0.25, 0.75),
      names = FALSE,
      type = 7
    )
    low <- idx[env_dist[idx] <= cuts[[1L]]]
    high <- idx[env_dist[idx] >= cuts[[2L]]]
    if (length(low) < 2L || length(high) < 2L) next
    observed_contrast <- mean(observed_div[high]) - mean(observed_div[low])
    null_contrast <- colMeans(null_div[high, , drop = FALSE]) -
      colMeans(null_div[low, , drop = FALSE])
    rows[[length(rows) + 1L]] <- data.frame(
      response = response_name,
      fold = fold,
      geo_bin = bin,
      component = component,
      n_pairs = length(idx),
      n_low_environment = length(low),
      n_high_environment = length(high),
      geo_distance_median = NA_real_,
      env_low_cut = cuts[[1L]],
      env_high_cut = cuts[[2L]],
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

summarize_components <- function(observed_store, null_store, response_name) {
  axis_rows <- list()
  null_globals <- list()
  observed_globals <- numeric(length(final8_terms))
  names(observed_globals) <- final8_terms
  for (term in final8_terms) {
    observed_components <- observed_store[[term]]
    null_components <- null_store[[term]]
    if (!length(observed_components) || !length(null_components)) {
      stop("No valid axis-specific strata for ", response_name, " / ", term, call. = FALSE)
    }
    null_matrix <- do.call(rbind, null_components)
    null_global <- colMeans(null_matrix)
    observed_global <- mean(observed_components)
    observed_globals[[term]] <- observed_global
    null_globals[[term]] <- null_global
    axis_rows[[length(axis_rows) + 1L]] <- data.frame(
      response = response_name,
      environmental_axis = term,
      environmental_label = unname(final8_labels[[term]]),
      n_fold_geo_strata = length(observed_components),
      observed_contrast = observed_global,
      space_null_mean = mean(null_global),
      space_null_median = stats::median(null_global),
      space_null_q025 = stats::quantile(null_global, 0.025, names = FALSE),
      space_null_q975 = stats::quantile(null_global, 0.975, names = FALSE),
      phenotype_excess_over_space_null = observed_global - stats::median(null_global),
      posterior_predictive_p_upper =
        (1 + sum(null_global >= observed_global)) / (length(null_global) + 1),
      stringsAsFactors = FALSE
    )
  }
  table <- do.call(rbind, axis_rows)
  null_global_matrix <- do.call(cbind, null_globals)
  colnames(null_global_matrix) <- final8_terms
  null_centres <- colMeans(null_global_matrix)
  null_scales <- apply(null_global_matrix, 2L, stats::sd)
  null_scales[!is.finite(null_scales) | null_scales <= 1e-12] <- 1
  observed_t <- (observed_globals - null_centres) / null_scales
  null_t <- sweep(
    sweep(null_global_matrix, 2L, null_centres, "-"),
    2L,
    null_scales,
    "/"
  )
  max_null_t <- apply(null_t, 1L, max)
  table$standardized_excess_t <- unname(observed_t[table$environmental_axis])
  table$BH_q <- stats::p.adjust(table$posterior_predictive_p_upper, method = "BH")
  table$maxT_FWER_p <- vapply(
    table$standardized_excess_t,
    function(value) (1 + sum(max_null_t >= value)) / (length(max_null_t) + 1),
    numeric(1)
  )
  table$raw_directional_5pct <- table$posterior_predictive_p_upper < 0.05 &
    table$observed_contrast > table$space_null_median
  table$maxT_FWER_5pct <- table$maxT_FWER_p < 0.05 &
    table$observed_contrast > table$space_null_median
  table
}

responses <- list(
  pigmentation_state = list(
    response = "n_pigmented",
    family = "binomial",
    trials = "n_observations",
    eligible = rep(TRUE, nrow(cells)),
    observed = function(d) d$n_pigmented / d$n_observations
  ),
  conditional_intensity = list(
    response = "conditional_intensity_median",
    family = "gaussian",
    trials = NULL,
    eligible = is.finite(cells$conditional_intensity_median),
    observed = function(d) d$conditional_intensity_median
  )
)

folds <- sort(unique(as.integer(cells$spatial_fold)))
mesh <- v16_make_mesh(cells)$mesh

pair_rows <- list()
omnibus_stratum_rows <- list()
axis_stratum_rows <- list()
heldout_rows <- list()
omnibus_null_store <- setNames(vector("list", length(responses)), names(responses))
omnibus_observed_store <- setNames(vector("list", length(responses)), names(responses))
axis_null_store <- setNames(vector("list", length(responses)), names(responses))
axis_observed_store <- setNames(vector("list", length(responses)), names(responses))

for (response_name in names(responses)) {
  specification <- responses[[response_name]]
  omnibus_null_store[[response_name]] <- list()
  omnibus_observed_store[[response_name]] <- numeric()
  axis_null_store[[response_name]] <- setNames(
    vector("list", length(final8_terms)),
    final8_terms
  )
  axis_observed_store[[response_name]] <- setNames(
    vector("list", length(final8_terms)),
    final8_terms
  )
  for (term in final8_terms) {
    axis_null_store[[response_name]][[term]] <- list()
    axis_observed_store[[response_name]][[term]] <- numeric()
  }

  for (fold in folds) {
    test_index <- which(
      as.integer(cells$spatial_fold) == fold & specification$eligible
    )
    train_index <- which(
      as.integer(cells$spatial_fold) != fold & specification$eligible
    )
    train <- cells[train_index, , drop = FALSE]
    test <- cells[test_index, , drop = FALSE]
    if (nrow(test) < 4L) next
    message(
      "[final8 axis space-null] ", response_name,
      " fold=", fold,
      " train=", nrow(train),
      " test=", nrow(test)
    )

    env_basis <- v16_fold_predictors(train, test, final8_terms)
    sampled <- sample_space_null(
      train,
      test,
      specification$response,
      specification$family,
      mesh,
      specification$trials,
      seed + 100000L * match(response_name, names(responses)) + 1000L * fold
    )
    observed <- specification$observed(test)
    pairs <- make_pairs(
      nrow(test),
      max_pairs_per_fold,
      seed + 200000L * match(response_name, names(responses)) + 1000L * fold
    )
    geo_distance <- pair_geo_distance(test, pairs)
    env_components <- pair_environment_components(env_basis$test, pairs)
    omnibus_distance <- sqrt(rowSums(env_components^2))
    observed_div <- pair_divergence_vector(observed, pairs)
    null_div <- pair_divergence_matrix(sampled$simulated, pairs)
    null_pair_median <- apply(null_div, 1L, stats::median)
    null_pair_q025 <- apply(
      null_div,
      1L,
      stats::quantile,
      probs = 0.025,
      names = FALSE
    )
    null_pair_q975 <- apply(
      null_div,
      1L,
      stats::quantile,
      probs = 0.975,
      names = FALSE
    )
    geo_bin <- assign_equal_count_bins(geo_distance, n_geo_bins)

    pair_block <- data.frame(
      response = response_name,
      fold = fold,
      site_i = test$exact_site_id[pairs[, 1L]],
      site_j = test$exact_site_id[pairs[, 2L]],
      geographic_distance_km = geo_distance,
      final8_environmental_distance_z = omnibus_distance,
      observed_phenotype_divergence = observed_div,
      space_null_median_divergence = null_pair_median,
      space_null_q025 = null_pair_q025,
      space_null_q975 = null_pair_q975,
      phenotype_excess = observed_div - null_pair_median,
      above_space_null_q975 = observed_div > null_pair_q975,
      geo_bin = geo_bin,
      stringsAsFactors = FALSE
    )
    for (term in final8_terms) {
      pair_block[[paste0("distance_", term)]] <- env_components[, term]
    }
    pair_rows[[length(pair_rows) + 1L]] <- pair_block

    omnibus <- contrast_from_groups(
      observed_div,
      null_div,
      geo_bin,
      omnibus_distance,
      response_name,
      fold,
      "final8_omnibus"
    )
    if (nrow(omnibus$summary)) {
      for (row_index in seq_len(nrow(omnibus$summary))) {
        bin <- omnibus$summary$geo_bin[[row_index]]
        omnibus$summary$geo_distance_median[[row_index]] <-
          stats::median(geo_distance[geo_bin == bin])
      }
      omnibus_stratum_rows[[length(omnibus_stratum_rows) + 1L]] <-
        omnibus$summary
      omnibus_observed_store[[response_name]] <- c(
        omnibus_observed_store[[response_name]],
        omnibus$summary$observed_high_minus_low
      )
      omnibus_null_store[[response_name]] <- c(
        omnibus_null_store[[response_name]],
        omnibus$null
      )
    }

    for (term in final8_terms) {
      axis_result <- contrast_from_groups(
        observed_div,
        null_div,
        geo_bin,
        env_components[, term],
        response_name,
        fold,
        term
      )
      if (!nrow(axis_result$summary)) next
      for (row_index in seq_len(nrow(axis_result$summary))) {
        bin <- axis_result$summary$geo_bin[[row_index]]
        axis_result$summary$geo_distance_median[[row_index]] <-
          stats::median(geo_distance[geo_bin == bin])
      }
      axis_stratum_rows[[length(axis_stratum_rows) + 1L]] <-
        axis_result$summary
      axis_observed_store[[response_name]][[term]] <- c(
        axis_observed_store[[response_name]][[term]],
        axis_result$summary$observed_high_minus_low
      )
      axis_null_store[[response_name]][[term]] <- c(
        axis_null_store[[response_name]][[term]],
        axis_result$null
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
    rm(sampled, null_div)
    invisible(gc())
  }
}

pair_table <- do.call(rbind, pair_rows)
omnibus_stratum_table <- do.call(rbind, omnibus_stratum_rows)
axis_stratum_table <- do.call(rbind, axis_stratum_rows)
heldout_table <- do.call(rbind, heldout_rows)

omnibus_rows <- lapply(names(responses), function(response_name) {
  observed_components <- omnibus_observed_store[[response_name]]
  null_components <- omnibus_null_store[[response_name]]
  if (!length(observed_components) || !length(null_components)) {
    stop("No valid omnibus strata for ", response_name, call. = FALSE)
  }
  null_matrix <- do.call(rbind, null_components)
  null_global <- colMeans(null_matrix)
  observed_global <- mean(observed_components)
  data.frame(
    response = response_name,
    environmental_basis = "final8",
    estimand =
      "matched_geographic_distance_high_minus_low_final8_distance_phenotype_divergence",
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
})
omnibus_table <- do.call(rbind, omnibus_rows)

axis_table <- do.call(rbind, lapply(names(responses), function(response_name) {
  summarize_components(
    axis_observed_store[[response_name]],
    axis_null_store[[response_name]],
    response_name
  )
}))

utils::write.csv(
  pair_table,
  file.path(output_dir, "heldout_pair_final8_space_null_excess.csv"),
  row.names = FALSE
)
utils::write.csv(
  omnibus_stratum_table,
  file.path(output_dir, "final8_omnibus_stratum_contrasts.csv"),
  row.names = FALSE
)
utils::write.csv(
  axis_stratum_table,
  file.path(output_dir, "final8_axis_stratum_contrasts.csv"),
  row.names = FALSE
)
utils::write.csv(
  omnibus_table,
  file.path(output_dir, "final8_omnibus_space_null_excess_test.csv"),
  row.names = FALSE
)
utils::write.csv(
  axis_table,
  file.path(output_dir, "final8_axis_space_null_excess_test.csv"),
  row.names = FALSE
)
utils::write.csv(
  heldout_table,
  file.path(output_dir, "heldout_final8_space_null_predictions.csv"),
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
    "environment_basis",
    "environment_terms",
    "omnibus_distance",
    "axis_distance",
    "axis_multiplicity",
    "claim_boundary"
  ),
  value = c(
    "broad_final8_axis_space_null_attribution",
    cells_path,
    "intercept + Matern SPDE only",
    "each tested pair lies wholly inside a held-out geographical fold",
    paste(folds, collapse = ";"),
    n_samples,
    seed,
    max_pairs_per_fold,
    n_geo_bins,
    "the same final eight measured abiotic axes used by the observation-level Broad model",
    paste(final8_terms, collapse = ";"),
    "Euclidean distance across the eight training-fold-standardized axes",
    "absolute training-fold-standardized difference on one axis",
    "raw one-sided posterior-predictive P, BH q, and shared-draw single-step maxT FWER P within each response",
    "axis distance is unsigned; coefficient direction must come from the observation-level environment + SPDE model"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  metadata,
  file.path(output_dir, "analysis_metadata.csv"),
  row.names = FALSE
)

summary_lines <- c(
  "# Final-eight-axis environmental attribution beyond a spatial null",
  "",
  "## Omnibus final-eight-axis distance",
  "",
  apply(omnibus_table, 1L, function(row) {
    sprintf(
      "- %s: observed=%.6f; space-null median=%.6f; excess=%.6f; one-sided P=%.5f.",
      row[["response"]],
      as.numeric(row[["observed_contrast"]]),
      as.numeric(row[["space_null_median"]]),
      as.numeric(row[["phenotype_excess_over_space_null"]]),
      as.numeric(row[["posterior_predictive_p_upper"]])
    )
  }),
  "",
  "## Axis-specific attribution",
  "",
  apply(axis_table, 1L, function(row) {
    sprintf(
      "- %s / %s: excess=%.6f; raw P=%.5f; BH q=%.5f; maxT FWER P=%.5f.",
      row[["response"]],
      row[["environmental_label"]],
      as.numeric(row[["phenotype_excess_over_space_null"]]),
      as.numeric(row[["posterior_predictive_p_upper"]]),
      as.numeric(row[["BH_q"]]),
      as.numeric(row[["maxT_FWER_p"]])
    )
  }),
  "",
  paste0(
    "Interpretation boundary: this analysis attributes an unsigned divergence signal. ",
    "It does not estimate whether warmer, wetter or more rugged sites have higher trait values; ",
    "those directions belong to the observation-level full environment + SPDE model."
  )
)
writeLines(summary_lines, file.path(output_dir, "RESULT_SUMMARY.md"))

cat("Completed final-eight-axis space-null attribution.\n")
print(omnibus_table)
print(axis_table)
