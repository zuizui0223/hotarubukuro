#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", name, "="), "", hit[[1L]])
}

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
cache_dir <- arg_value(
  "--cache-dir",
  "results/broad_space_null_geobin_sensitivity_v23"
)
output_dir <- arg_value(
  "--output",
  "results/broad_supported_term_distance_space_null"
)

pair_path <- file.path(cache_dir, "fixed_heldout_pair_table.csv")
draw_path <- file.path(cache_dir, "heldout_space_null_site_draws.rds")

for (path in c(cells_path, pair_path, draw_path)) {
  if (!file.exists(path)) stop("Missing required input: ", path, call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
pairs <- utils::read.csv(pair_path, check.names = FALSE, stringsAsFactors = FALSE)
space_null <- readRDS(draw_path)

required_cells <- c(
  "exact_site_id", "spatial_fold", "conditional_intensity_median",
  "env_Temperature_PC1", "env_precip_PC1",
  "env_TemperatureSeasonality", "env_topo_PC1"
)
required_pairs <- c(
  "response", "fold", "site_i", "site_j", "geo_bin_5",
  "observed_phenotype_divergence"
)
missing_cells <- setdiff(required_cells, names(cells))
missing_pairs <- setdiff(required_pairs, names(pairs))
if (length(missing_cells)) {
  stop("Cell table is missing: ", paste(missing_cells, collapse = ", "), call. = FALSE)
}
if (length(missing_pairs)) {
  stop("Pair table is missing: ", paste(missing_pairs, collapse = ", "), call. = FALSE)
}

cells$int_thermal_variability <-
  cells$env_Temperature_PC1 * cells$env_TemperatureSeasonality

registry <- list(
  pigmentation_state = list(
    terms = c("env_Temperature_PC1"),
    labels = c("Temperature PC1"),
    eligible = rep(TRUE, nrow(cells))
  ),
  conditional_intensity = list(
    terms = c(
      "env_precip_PC1",
      "env_TemperatureSeasonality",
      "env_topo_PC1",
      "int_thermal_variability"
    ),
    labels = c(
      "Precipitation PC1",
      "Temperature seasonality",
      "Topography PC1",
      "Temperature PC1 x temperature seasonality"
    ),
    eligible = is.finite(cells$conditional_intensity_median)
  )
)

standardize_on_training <- function(train, test, terms) {
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

pair_environment_distance <- function(z, site_ids, site_i, site_j) {
  i <- match(site_i, site_ids)
  j <- match(site_j, site_ids)
  if (anyNA(i) || anyNA(j)) stop("Pair sites do not match held-out cells.", call. = FALSE)
  delta <- z[i, , drop = FALSE] - z[j, , drop = FALSE]
  sqrt(rowSums(delta^2))
}

pair_component_distance <- function(z, site_ids, site_i, site_j, terms) {
  i <- match(site_i, site_ids)
  j <- match(site_j, site_ids)
  delta <- abs(z[i, , drop = FALSE] - z[j, , drop = FALSE])
  colnames(delta) <- paste0("distance_", terms)
  as.data.frame(delta, stringsAsFactors = FALSE)
}

stratum_rows <- list()
pair_rows <- list()
primary_rows <- list()
null_draw_rows <- list()

for (response_name in names(registry)) {
  specification <- registry[[response_name]]
  response_pairs <- pairs[pairs$response == response_name, , drop = FALSE]
  observed_components <- numeric()
  null_components <- list()

  for (fold in sort(unique(as.integer(response_pairs$fold)))) {
    train <- cells[
      as.integer(cells$spatial_fold) != fold & specification$eligible,
      , drop = FALSE
    ]
    test <- cells[
      as.integer(cells$spatial_fold) == fold & specification$eligible,
      , drop = FALSE
    ]
    pair_fold <- response_pairs[
      as.integer(response_pairs$fold) == fold,
      , drop = FALSE
    ]

    scaled <- standardize_on_training(train, test, specification$terms)
    environmental_distance <- pair_environment_distance(
      scaled$test,
      test$exact_site_id,
      pair_fold$site_i,
      pair_fold$site_j
    )
    components <- pair_component_distance(
      scaled$test,
      test$exact_site_id,
      pair_fold$site_i,
      pair_fold$site_j,
      specification$terms
    )

    cached <- space_null[[response_name]][[as.character(fold)]]
    if (is.null(cached)) stop("Missing cached response/fold: ", response_name, "/", fold)
    draw_i <- match(pair_fold$site_i, cached$exact_site_id)
    draw_j <- match(pair_fold$site_j, cached$exact_site_id)
    if (anyNA(draw_i) || anyNA(draw_j)) {
      stop("Cached draws do not contain all pair sites for ", response_name, "/", fold)
    }
    simulated <- as.matrix(cached$simulated)
    if (ncol(simulated) != 500L) {
      stop("Expected 500 cached posterior-predictive maps; found ", ncol(simulated))
    }

    pair_block <- data.frame(
      response = response_name,
      fold = fold,
      site_i = pair_fold$site_i,
      site_j = pair_fold$site_j,
      geographic_distance_km = pair_fold$geographic_distance_km,
      geo_bin = as.integer(pair_fold$geo_bin_5),
      supported_term_environmental_distance = environmental_distance,
      observed_phenotype_divergence = pair_fold$observed_phenotype_divergence,
      stringsAsFactors = FALSE
    )
    pair_block <- cbind(pair_block, components)
    pair_rows[[length(pair_rows) + 1L]] <- pair_block

    for (bin in sort(unique(as.integer(pair_fold$geo_bin_5)))) {
      index <- which(
        as.integer(pair_fold$geo_bin_5) == bin &
          is.finite(environmental_distance) &
          is.finite(pair_fold$observed_phenotype_divergence)
      )
      if (length(index) < 8L) next

      cuts <- stats::quantile(
        environmental_distance[index],
        probs = c(0.25, 0.75),
        names = FALSE,
        type = 7
      )
      low <- index[environmental_distance[index] <= cuts[[1L]]]
      high <- index[environmental_distance[index] >= cuts[[2L]]]
      if (length(low) < 2L || length(high) < 2L) next

      observed_contrast <-
        mean(pair_fold$observed_phenotype_divergence[high]) -
        mean(pair_fold$observed_phenotype_divergence[low])

      high_divergence <- abs(
        simulated[draw_i[high], , drop = FALSE] -
          simulated[draw_j[high], , drop = FALSE]
      )
      low_divergence <- abs(
        simulated[draw_i[low], , drop = FALSE] -
          simulated[draw_j[low], , drop = FALSE]
      )
      null_contrast <-
        colMeans(high_divergence) - colMeans(low_divergence)

      observed_components <- c(observed_components, observed_contrast)
      null_components[[length(null_components) + 1L]] <- null_contrast
      stratum_rows[[length(stratum_rows) + 1L]] <- data.frame(
        response = response_name,
        fold = fold,
        geo_bin = bin,
        n_pairs = length(index),
        n_low_environment = length(low),
        n_high_environment = length(high),
        geographic_distance_median_km = stats::median(
          pair_fold$geographic_distance_km[index]
        ),
        environment_distance_low_cut = cuts[[1L]],
        environment_distance_high_cut = cuts[[2L]],
        observed_high_minus_low = observed_contrast,
        space_null_median = stats::median(null_contrast),
        space_null_q025 = stats::quantile(null_contrast, 0.025, names = FALSE),
        space_null_q975 = stats::quantile(null_contrast, 0.975, names = FALSE),
        excess_over_space_null_median =
          observed_contrast - stats::median(null_contrast),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(observed_components) != 25L || length(null_components) != 25L) {
    stop("Expected 25 fold-by-distance strata for ", response_name)
  }
  null_matrix <- do.call(rbind, null_components)
  null_global <- colMeans(null_matrix)
  observed_global <- mean(observed_components)
  p_upper <-
    (1 + sum(null_global >= observed_global)) /
    (length(null_global) + 1)

  primary_rows[[length(primary_rows) + 1L]] <- data.frame(
    response = response_name,
    environmental_terms = paste(specification$labels, collapse = ";"),
    n_fold_geo_strata = length(observed_components),
    observed_contrast = observed_global,
    space_null_median = stats::median(null_global),
    space_null_q025 = stats::quantile(null_global, 0.025, names = FALSE),
    space_null_q975 = stats::quantile(null_global, 0.975, names = FALSE),
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

bind_rows_fill <- function(rows) {
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  aligned <- lapply(rows, function(row) {
    missing <- setdiff(columns, names(row))
    for (column in missing) row[[column]] <- NA_real_
    row[columns]
  })
  do.call(rbind, aligned)
}

primary <- do.call(rbind, primary_rows)
strata <- do.call(rbind, stratum_rows)
pair_output <- bind_rows_fill(pair_rows)
null_draws <- do.call(rbind, null_draw_rows)

utils::write.csv(
  primary,
  file.path(output_dir, "primary_supported_term_distance_test.csv"),
  row.names = FALSE
)
utils::write.csv(
  strata,
  file.path(output_dir, "matched_distance_stratum_contrasts.csv"),
  row.names = FALSE
)
utils::write.csv(
  pair_output,
  file.path(output_dir, "heldout_pair_supported_term_distance.csv"),
  row.names = FALSE
)
utils::write.csv(
  null_draws,
  file.path(output_dir, "space_null_global_contrast_draws.csv"),
  row.names = FALSE
)

metadata <- data.frame(
  field = c(
    "analysis",
    "source_cells",
    "source_pair_table",
    "source_site_draws",
    "space_null_operation",
    "posterior_predictive_maps_reused",
    "pigmentation_state_environment_terms",
    "conditional_intensity_environment_terms",
    "environmental_distance",
    "primary_estimand",
    "claim_boundary"
  ),
  value = c(
    "supported_environmental_term_distance_reaggregation",
    cells_path,
    pair_path,
    draw_path,
    "No spatial model was refitted; fixed held-out pairs and cached cross-fitted intercept + Matern SPDE posterior-predictive maps were reaggregated.",
    500,
    paste(registry$pigmentation_state$terms, collapse = ";"),
    paste(registry$conditional_intensity$terms, collapse = ";"),
    "Euclidean distance across training-fold-standardised final-model-supported terms; no coefficient weighting.",
    "Mean across 25 fold-by-geographical-distance strata of high-minus-low environmental-distance phenotype divergence, compared with the identical statistic from fixed space-only posterior-predictive maps.",
    "Model-informed supporting reaggregation; not independent variable discovery, selection, adaptation, genetic differentiation, drift, plasticity, or direct anthocyanin physiology."
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
  "No environment or spatial model was refitted. The analysis reuses the fixed held-out pairs and 500 cross-fitted space-only posterior-predictive maps, changing only the environmental distance used to group pairs.",
  ""
)
for (index in seq_len(nrow(primary))) {
  row <- primary[index, ]
  summary_lines <- c(
    summary_lines,
    paste0(
      "- ", row$response,
      ": terms=", row$environmental_terms,
      "; observed=", sprintf("%.6f", row$observed_contrast),
      "; space-null median=", sprintf("%.6f", row$space_null_median),
      "; excess=", sprintf("%.6f", row$phenotype_excess_over_space_null),
      "; one-sided P=", sprintf("%.6f", row$posterior_predictive_p_upper)
    )
  )
}
summary_lines <- c(
  summary_lines,
  "",
  "A positive excess means that separation along the final model's supported environmental terms orders held-out phenotype divergence beyond the fixed spatial-continuity expectation."
)
writeLines(summary_lines, file.path(output_dir, "RESULT_SUMMARY.md"))

cat("Completed supported-term distance reaggregation.\n")
print(primary)
