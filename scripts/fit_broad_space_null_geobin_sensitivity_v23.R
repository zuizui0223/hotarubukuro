#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/natural_predictive_model.R")

arg_value <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", name, "="), "", hit[[1L]])
}

parse_integer_list <- function(value) {
  out <- as.integer(strsplit(value, ",", fixed = TRUE)[[1L]])
  out <- unique(out[is.finite(out)])
  if (!length(out) || any(out < 2L)) {
    stop("Geographical-bin list must contain integers >= 2.", call. = FALSE)
  }
  sort(out)
}

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
output_dir <- arg_value(
  "--output",
  "results/broad_space_null_geobin_sensitivity_v23"
)
n_samples <- as.integer(arg_value("--samples", "500"))
seed <- as.integer(arg_value("--seed", "20260725"))
max_pairs_per_fold <- as.integer(arg_value("--max-pairs-per-fold", "15000"))
geo_bins_values <- parse_integer_list(arg_value("--geo-bins-list", "5,10,20"))
inla_verbose <- identical(tolower(arg_value("--inla-verbose", "false")), "true")

if (!file.exists(cells_path)) stop("Missing cell table: ", cells_path, call. = FALSE)
if (!is.finite(n_samples) || n_samples < 100L) stop("--samples must be >= 100", call. = FALSE)
if (!is.finite(max_pairs_per_fold) || max_pairs_per_fold < 100L) {
  stop("--max-pairs-per-fold must be >= 100", call. = FALSE)
}
if (!5L %in% geo_bins_values) {
  stop("The published five-bin profile must remain in --geo-bins-list.", call. = FALSE)
}
if (!requireNamespace("INLA", quietly = TRUE)) stop("Package 'INLA' is required.", call. = FALSE)
if (!requireNamespace("Matrix", quietly = TRUE)) stop("Package 'Matrix' is required.", call. = FALSE)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)

legacy4_terms <- v16_environment_terms(50)
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
    "n_observations", "conditional_intensity_median",
    legacy4_terms, final8_terms
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
  abs(
    values[pairs[, 1L], , drop = FALSE] -
      values[pairs[, 2L], , drop = FALSE]
  )
}

pair_delta_matrix <- function(z, pairs, labels) {
  out <- abs(
    as.matrix(z[pairs[, 1L], , drop = FALSE]) -
      as.matrix(z[pairs[, 2L], , drop = FALSE])
  )
  colnames(out) <- labels
  out
}

profile_key <- function(response, component, n_bins, subset_rule) {
  paste(response, component, n_bins, subset_rule, sep = "__")
}

run_label_for <- function(component, n_bins, subset_rule) {
  if (!identical(component, "legacy4_omnibus")) return(NA_character_)
  key <- paste(n_bins, subset_rule, sep = ":")
  labels <- c(
    "5:all" = "A",
    "5:drop_resolution_nearest" = "B",
    "10:all" = "C",
    "10:drop_resolution_nearest" = "D",
    "20:all" = "E",
    "20:drop_resolution_nearest" = "F"
  )
  if (key %in% names(labels)) unname(labels[[key]]) else NA_character_
}

component_label <- function(component) {
  if (identical(component, "legacy4_omnibus")) {
    return("Legacy four-PC omnibus distance")
  }
  if (identical(component, "final8_omnibus")) {
    return("Final-eight-axis omnibus distance")
  }
  axis <- sub("^final8_axis_", "", component)
  if (axis %in% names(final8_labels)) {
    return(paste0("Final-eight-axis: ", unname(final8_labels[[axis]])))
  }
  component
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
subset_rules <- c(
  "all",
  "drop_resolution_nearest",
  "drop_published_nearest"
)

pair_rows <- list()
stratum_summary_rows <- list()
confound_rows <- list()
heldout_rows <- list()
profile_observed_store <- list()
profile_null_store <- list()
profile_meta <- list()
stratum_null_store <- list()
site_draw_store <- setNames(vector("list", length(responses)), names(responses))

for (response_name in names(responses)) {
  specification <- responses[[response_name]]
  site_draw_store[[response_name]] <- list()
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
      "[v23 geobin] ", response_name,
      " fold=", fold,
      " train=", nrow(train),
      " test=", nrow(test)
    )

    legacy_basis <- v16_fold_predictors(train, test, legacy4_terms)
    final8_basis <- v16_fold_predictors(train, test, final8_terms)
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
    observed_div <- pair_divergence_vector(observed, pairs)
    null_div <- pair_divergence_matrix(sampled$simulated, pairs)

    legacy_delta <- pair_delta_matrix(
      legacy_basis$test,
      pairs,
      legacy4_terms
    )
    final8_delta <- pair_delta_matrix(
      final8_basis$test,
      pairs,
      final8_terms
    )
    component_distances <- list(
      legacy4_omnibus = sqrt(rowSums(legacy_delta^2)),
      final8_omnibus = sqrt(rowSums(final8_delta^2))
    )
    for (term in final8_terms) {
      component_distances[[paste0("final8_axis_", term)]] <- final8_delta[, term]
    }

    published_bins <- assign_equal_count_bins(geo_distance, 5L)
    pair_block <- data.frame(
      response = response_name,
      fold = fold,
      site_i = test$exact_site_id[pairs[, 1L]],
      site_j = test$exact_site_id[pairs[, 2L]],
      geographic_distance_km = geo_distance,
      observed_phenotype_divergence = observed_div,
      published_five_bin = published_bins,
      published_nearest = published_bins == 1L,
      stringsAsFactors = FALSE
    )
    for (component in names(component_distances)) {
      pair_block[[paste0("distance_", component)]] <- component_distances[[component]]
    }
    for (n_bins in geo_bins_values) {
      pair_block[[paste0("geo_bin_", n_bins)]] <-
        assign_equal_count_bins(geo_distance, n_bins)
    }
    pair_rows[[length(pair_rows) + 1L]] <- pair_block

    site_draw_store[[response_name]][[as.character(fold)]] <- list(
      exact_site_id = test$exact_site_id,
      observed = observed,
      expected = sampled$expected,
      simulated = sampled$simulated
    )
    heldout_rows[[length(heldout_rows) + 1L]] <- data.frame(
      response = response_name,
      fold = fold,
      exact_site_id = test$exact_site_id,
      observed = observed,
      space_null_expected = sampled$expected,
      stringsAsFactors = FALSE
    )

    for (n_bins in geo_bins_values) {
      geo_bin <- assign_equal_count_bins(geo_distance, n_bins)
      for (component in names(component_distances)) {
        env_distance <- component_distances[[component]]

        for (bin in sort(unique(geo_bin))) {
          idx <- which(
            geo_bin == bin &
              is.finite(env_distance) &
              is.finite(observed_div)
          )
          if (length(idx) < 8L) next
          cuts <- stats::quantile(
            env_distance[idx],
            probs = c(0.25, 0.75),
            names = FALSE,
            type = 7
          )
          low <- idx[env_distance[idx] <= cuts[[1L]]]
          high <- idx[env_distance[idx] >= cuts[[2L]]]
          rho <- suppressWarnings(stats::cor(
            geo_distance[idx],
            env_distance[idx],
            method = "spearman",
            use = "complete.obs"
          ))
          confound_rows[[length(confound_rows) + 1L]] <- data.frame(
            response = response_name,
            fold = fold,
            component = component,
            component_label = component_label(component),
            geographic_bins = n_bins,
            geo_bin = bin,
            n_pairs = length(idx),
            geo_distance_min = min(geo_distance[idx]),
            geo_distance_max = max(geo_distance[idx]),
            geo_distance_median = stats::median(geo_distance[idx]),
            spearman_geo_vs_environment = rho,
            high_minus_low_geo_mean = if (length(low) && length(high)) {
              mean(geo_distance[high]) - mean(geo_distance[low])
            } else NA_real_,
            high_minus_low_geo_median = if (length(low) && length(high)) {
              stats::median(geo_distance[high]) - stats::median(geo_distance[low])
            } else NA_real_,
            stringsAsFactors = FALSE
          )
        }

        for (subset_rule in subset_rules) {
          eligible_pair <- rep(TRUE, length(geo_bin))
          if (identical(subset_rule, "drop_resolution_nearest")) {
            eligible_pair <- geo_bin != 1L
          } else if (identical(subset_rule, "drop_published_nearest")) {
            eligible_pair <- published_bins != 1L
          }
          key <- profile_key(response_name, component, n_bins, subset_rule)
          if (is.null(profile_observed_store[[key]])) {
            profile_observed_store[[key]] <- numeric()
            profile_null_store[[key]] <- list()
            profile_meta[[key]] <- data.frame(
              profile_id = key,
              response = response_name,
              component = component,
              component_label = component_label(component),
              geographic_bins = n_bins,
              subset_rule = subset_rule,
              run_label = run_label_for(component, n_bins, subset_rule),
              stringsAsFactors = FALSE
            )
          }

          for (bin in sort(unique(geo_bin[eligible_pair]))) {
            idx <- which(
              eligible_pair &
                geo_bin == bin &
                is.finite(env_distance) &
                is.finite(observed_div)
            )
            if (length(idx) < 8L) next
            cuts <- stats::quantile(
              env_distance[idx],
              probs = c(0.25, 0.75),
              names = FALSE,
              type = 7
            )
            low <- idx[env_distance[idx] <= cuts[[1L]]]
            high <- idx[env_distance[idx] >= cuts[[2L]]]
            if (length(low) < 2L || length(high) < 2L) next
            observed_contrast <-
              mean(observed_div[high]) - mean(observed_div[low])
            null_contrast <-
              colMeans(null_div[high, , drop = FALSE]) -
              colMeans(null_div[low, , drop = FALSE])
            profile_observed_store[[key]] <- c(
              profile_observed_store[[key]],
              observed_contrast
            )
            profile_null_store[[key]][[length(profile_null_store[[key]]) + 1L]] <-
              null_contrast
            stratum_id <- paste(key, fold, bin, sep = "__")
            stratum_summary_rows[[length(stratum_summary_rows) + 1L]] <- data.frame(
              stratum_id = stratum_id,
              profile_id = key,
              response = response_name,
              fold = fold,
              component = component,
              component_label = component_label(component),
              geographic_bins = n_bins,
              subset_rule = subset_rule,
              geo_bin = bin,
              n_pairs = length(idx),
              n_low_environment = length(low),
              n_high_environment = length(high),
              geo_distance_median = stats::median(geo_distance[idx]),
              env_low_cut = cuts[[1L]],
              env_high_cut = cuts[[2L]],
              observed_high_minus_low = observed_contrast,
              null_mean = mean(null_contrast),
              null_median = stats::median(null_contrast),
              null_q025 = stats::quantile(
                null_contrast, 0.025, names = FALSE
              ),
              null_q975 = stats::quantile(
                null_contrast, 0.975, names = FALSE
              ),
              excess_over_null_median =
                observed_contrast - stats::median(null_contrast),
              stringsAsFactors = FALSE
            )
            stratum_null_store[[stratum_id]] <- null_contrast
          }
        }
      }
    }
    rm(sampled, null_div)
    invisible(gc())
  }
}

pair_table <- do.call(rbind, pair_rows)
stratum_summary <- do.call(rbind, stratum_summary_rows)
confound_table <- do.call(rbind, confound_rows)
heldout_table <- do.call(rbind, heldout_rows)

profile_rows <- list()
profile_null_global_store <- list()
for (key in names(profile_observed_store)) {
  observed_components <- profile_observed_store[[key]]
  null_components <- profile_null_store[[key]]
  if (!length(observed_components) || !length(null_components)) next
  null_matrix <- do.call(rbind, null_components)
  null_global <- colMeans(null_matrix)
  observed_global <- mean(observed_components)
  meta <- profile_meta[[key]]
  profile_rows[[length(profile_rows) + 1L]] <- data.frame(
    meta,
    n_fold_geo_strata = length(observed_components),
    observed_contrast = observed_global,
    space_null_mean = mean(null_global),
    space_null_median = stats::median(null_global),
    space_null_q025 = stats::quantile(null_global, 0.025, names = FALSE),
    space_null_q975 = stats::quantile(null_global, 0.975, names = FALSE),
    phenotype_excess_over_space_null =
      observed_global - stats::median(null_global),
    posterior_predictive_p_upper =
      (1 + sum(null_global >= observed_global)) /
      (length(null_global) + 1),
    stringsAsFactors = FALSE
  )
  profile_null_global_store[[key]] <- null_global
}
profile_table <- do.call(rbind, profile_rows)
profile_table$BH_q <- NA_real_
profile_table$maxT_FWER_p <- NA_real_

axis_pattern <- "^final8_axis_"
axis_profile <- grepl(axis_pattern, profile_table$component)
axis_groups <- split(
  which(axis_profile),
  interaction(
    profile_table$response[axis_profile],
    profile_table$geographic_bins[axis_profile],
    profile_table$subset_rule[axis_profile],
    drop = TRUE
  )
)
for (indices in axis_groups) {
  keys <- profile_table$profile_id[indices]
  components <- profile_table$component[indices]
  if (length(indices) != length(final8_terms)) next
  null_matrix <- do.call(cbind, profile_null_global_store[keys])
  colnames(null_matrix) <- components
  observed <- profile_table$observed_contrast[indices]
  names(observed) <- components
  centres <- colMeans(null_matrix)
  scales <- apply(null_matrix, 2L, stats::sd)
  scales[!is.finite(scales) | scales <= 1e-12] <- 1
  observed_t <- (observed - centres[components]) / scales[components]
  null_t <- sweep(
    sweep(null_matrix, 2L, centres, "-"),
    2L, scales, "/"
  )
  max_null_t <- apply(null_t, 1L, max)
  profile_table$BH_q[indices] <- stats::p.adjust(
    profile_table$posterior_predictive_p_upper[indices],
    method = "BH"
  )
  profile_table$maxT_FWER_p[indices] <- vapply(
    observed_t,
    function(value) {
      (1 + sum(max_null_t >= value)) / (length(max_null_t) + 1)
    },
    numeric(1)
  )
}
profile_table$raw_directional_5pct <-
  profile_table$posterior_predictive_p_upper < 0.05 &
  profile_table$phenotype_excess_over_space_null > 0
profile_table$maxT_FWER_5pct <-
  !is.na(profile_table$maxT_FWER_p) &
  profile_table$maxT_FWER_p < 0.05 &
  profile_table$phenotype_excess_over_space_null > 0

null_profile_rows <- lapply(names(profile_null_global_store), function(key) {
  values <- profile_null_global_store[[key]]
  data.frame(
    profile_id = key,
    draw = seq_along(values),
    null_global_contrast = values,
    stringsAsFactors = FALSE
  )
})
null_profile_table <- do.call(rbind, null_profile_rows)

confound_summary <- aggregate(
  cbind(
    spearman_geo_vs_environment,
    high_minus_low_geo_mean,
    high_minus_low_geo_median
  ) ~ response + component + component_label + geographic_bins + geo_bin,
  data = confound_table,
  FUN = mean,
  na.rm = TRUE
)
confound_abs <- transform(
  confound_table,
  abs_high_minus_low_geo_mean = abs(high_minus_low_geo_mean)
)
confound_max <- aggregate(
  abs_high_minus_low_geo_mean ~
    response + component + component_label + geographic_bins + geo_bin,
  data = confound_abs,
  FUN = max,
  na.rm = TRUE
)
names(confound_max)[names(confound_max) == "abs_high_minus_low_geo_mean"] <-
  "max_abs_high_minus_low_geo_mean"
confound_summary <- merge(
  confound_summary,
  confound_max,
  by = c(
    "response", "component", "component_label",
    "geographic_bins", "geo_bin"
  ),
  all.x = TRUE,
  sort = FALSE
)

utils::write.csv(
  profile_table,
  file.path(output_dir, "geobin_sensitivity_profile.csv"),
  row.names = FALSE
)
utils::write.csv(
  null_profile_table,
  file.path(output_dir, "geobin_sensitivity_null_draws.csv"),
  row.names = FALSE
)
utils::write.csv(
  stratum_summary,
  file.path(output_dir, "geobin_sensitivity_stratum_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  confound_table,
  file.path(output_dir, "exact_basis_confound_diagnostic_by_fold.csv"),
  row.names = FALSE
)
utils::write.csv(
  confound_summary,
  file.path(output_dir, "exact_basis_confound_diagnostic_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  pair_table,
  file.path(output_dir, "fixed_heldout_pair_table.csv"),
  row.names = FALSE
)
utils::write.csv(
  heldout_table,
  file.path(output_dir, "heldout_space_null_expected.csv"),
  row.names = FALSE
)
saveRDS(
  stratum_null_store,
  file.path(output_dir, "per_draw_per_stratum_null_contrasts.rds"),
  compress = "xz"
)
saveRDS(
  site_draw_store,
  file.path(output_dir, "heldout_space_null_site_draws.rds"),
  compress = "xz"
)

metadata <- data.frame(
  field = c(
    "analysis",
    "specification",
    "source_cells",
    "space_null",
    "space_null_refit_policy",
    "cross_fitting",
    "spatial_folds",
    "posterior_samples",
    "seed",
    "max_pairs_per_fold",
    "geographic_distance_bins",
    "legacy_environment_terms",
    "final8_environment_terms",
    "pair_policy",
    "subset_rules",
    "primary_estimand",
    "claim_boundary"
  ),
  value = c(
    "broad_space_null_geobin_sensitivity_v23",
    "v23.0 source specification plus explicit v23.1 implementation amendment",
    cells_path,
    "intercept + Matern SPDE only",
    "one fit per response and held-out fold; identical posterior-predictive maps are reused because binning is a post-fit aggregation rule",
    "every tested pair lies wholly inside a held-out geographical fold",
    paste(folds, collapse = ";"),
    n_samples,
    seed,
    max_pairs_per_fold,
    paste(geo_bins_values, collapse = ";"),
    paste(legacy4_terms, collapse = ";"),
    paste(final8_terms, collapse = ";"),
    "the published pair seed rule is reused once per response and fold; pair identities remain fixed across every profile",
    paste(subset_rules, collapse = ";"),
    "mean across retained fold-by-distance strata of observed high-minus-low environmental-distance phenotype divergence, compared with the identical statistic in each space-only posterior-predictive map",
    "environmental alignment beyond fitted spatial continuity; not FST, PST, QST, drift, selection, local adaptation, plasticity, or a unique causal environmental mechanism"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  metadata,
  file.path(output_dir, "analysis_metadata.csv"),
  row.names = FALSE
)

legacy_profile <- profile_table[
  profile_table$component == "legacy4_omnibus" &
    profile_table$subset_rule %in% c("all", "drop_resolution_nearest") &
    !is.na(profile_table$run_label),
  ,
  drop = FALSE
]
legacy_profile <- legacy_profile[
  order(legacy_profile$response, legacy_profile$run_label),
  ,
  drop = FALSE
]
state_temperature <- profile_table[
  profile_table$response == "pigmentation_state" &
    profile_table$component == "final8_axis_env_Temperature_PC1" &
    profile_table$subset_rule %in% c("all", "drop_resolution_nearest"),
  ,
  drop = FALSE
]
state_temperature <- state_temperature[
  order(state_temperature$geographic_bins, state_temperature$subset_rule),
  ,
  drop = FALSE
]

summary_lines <- c(
  "# v23 geographical-stratification sensitivity",
  "",
  "## Legacy four-PC specification runs A-F",
  "",
  apply(legacy_profile, 1L, function(row) {
    sprintf(
      "- Run %s / %s: bins=%s; subset=%s; observed=%.6f; null median=%.6f; excess=%.6f; one-sided P=%.5f.",
      row[["run_label"]],
      row[["response"]],
      row[["geographic_bins"]],
      row[["subset_rule"]],
      as.numeric(row[["observed_contrast"]]),
      as.numeric(row[["space_null_median"]]),
      as.numeric(row[["phenotype_excess_over_space_null"]]),
      as.numeric(row[["posterior_predictive_p_upper"]])
    )
  }),
  "",
  "## Final-eight Temperature PC1 profile for pigmentation state",
  "",
  apply(state_temperature, 1L, function(row) {
    sprintf(
      "- bins=%s; subset=%s; observed=%.6f; null median=%.6f; excess=%.6f; raw P=%.5f; BH q=%.5f; maxT FWER P=%.5f.",
      row[["geographic_bins"]],
      row[["subset_rule"]],
      as.numeric(row[["observed_contrast"]]),
      as.numeric(row[["space_null_median"]]),
      as.numeric(row[["phenotype_excess_over_space_null"]]),
      as.numeric(row[["posterior_predictive_p_upper"]]),
      as.numeric(row[["BH_q"]]),
      as.numeric(row[["maxT_FWER_p"]])
    )
  }),
  "",
  paste0(
    "Run A remains the historical primary for the legacy four-PC basis. ",
    "The final-eight omnibus and axis profiles are attribution and robustness analyses, ",
    "not replacements chosen by P value."
  ),
  paste0(
    "Claim boundary: all distance tests are unsigned. Direction must be read from ",
    "the observation-level environment + SPDE coefficients."
  )
)
writeLines(summary_lines, file.path(output_dir, "RESULT_SUMMARY.md"))

cat("Completed v23 geographical-bin sensitivity and exact-basis diagnostics.\n")
print(legacy_profile)
print(state_temperature)
