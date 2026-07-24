v19_analysis_spec_version <- "v19.1_landscape_signature_natural_null"

v19_require_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

v19_rank01 <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (sum(keep) == 1L) {
    out[keep] <- 1
  } else if (sum(keep) > 1L) {
    out[keep] <- (rank(x[keep], ties.method = "average") - 1) /
      (sum(keep) - 1)
  }
  out
}

v19_mean_finite <- function(x) {
  x <- as.numeric(x)
  if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
}

v19_median_finite <- function(x) {
  x <- as.numeric(x)
  if (any(is.finite(x))) stats::median(x[is.finite(x)]) else NA_real_
}

v19_lookup_mlit_landscape <- function(
    observations, mlit_cells, edge_raster) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required.", call. = FALSE)
  }
  observation_terms <- c(
    "exact_site_id", "longitude", "latitude", "x_km", "y_km"
  )
  lookup_terms <- c(
    "longitude", "latitude",
    "human_forest_edge_km_per_nominal_km2",
    "forest_fraction", "human_managed_fraction", "represented_fraction",
    "primary_mesh_boundary", "major_road_distance_km"
  )
  v19_require_columns(observations, observation_terms, "observations")
  v19_require_columns(mlit_cells, lookup_terms, "MLIT cells")

  raster <- if (inherits(edge_raster, "SpatRaster")) {
    edge_raster
  } else {
    terra::rast(edge_raster)
  }
  coordinates <- as.matrix(observations[, c("longitude", "latitude")])
  raster_cell <- terra::cellFromXY(raster, coordinates)
  raster_center <- terra::xyFromCell(raster, raster_cell)
  observation_key <- paste(
    round(raster_center[, 1L], 7), round(raster_center[, 2L], 7)
  )
  lookup_key <- paste(
    round(as.numeric(mlit_cells$longitude), 7),
    round(as.numeric(mlit_cells$latitude), 7)
  )
  lookup_index <- match(observation_key, lookup_key)

  linked <- data.frame(
    exact_site_id = as.character(observations$exact_site_id),
    cell_id_v19 = paste0(
      "cell-1km-", floor(as.numeric(observations$x_km)), "_",
      floor(as.numeric(observations$y_km))
    ),
    mlit_lookup_index = lookup_index,
    forest_human_edge_1km = as.numeric(
      mlit_cells$human_forest_edge_km_per_nominal_km2[lookup_index]
    ),
    forest_fraction_1km = as.numeric(
      mlit_cells$forest_fraction[lookup_index]
    ),
    human_managed_fraction_1km = as.numeric(
      mlit_cells$human_managed_fraction[lookup_index]
    ),
    represented_fraction_1km = as.numeric(
      mlit_cells$represented_fraction[lookup_index]
    ),
    major_road_distance_km = as.numeric(
      mlit_cells$major_road_distance_km[lookup_index]
    ),
    primary_mesh_boundary = as.logical(
      mlit_cells$primary_mesh_boundary[lookup_index]
    ),
    stringsAsFactors = FALSE
  )

  site_groups <- split(
    seq_len(nrow(linked)),
    paste(linked$cell_id_v19, linked$exact_site_id, sep = "::")
  )
  site_rows <- lapply(site_groups, function(index) {
    block <- linked[index, , drop = FALSE]
    data.frame(
      exact_site_id = block$cell_id_v19[1L],
      source_exact_site_id = block$exact_site_id[1L],
      forest_human_edge_1km =
        v19_median_finite(block$forest_human_edge_1km),
      forest_fraction_1km =
        v19_median_finite(block$forest_fraction_1km),
      human_managed_fraction_1km =
        v19_median_finite(block$human_managed_fraction_1km),
      represented_fraction_1km =
        v19_median_finite(block$represented_fraction_1km),
      major_road_distance_km =
        v19_median_finite(block$major_road_distance_km),
      primary_mesh_boundary = any(
        block$primary_mesh_boundary %in% TRUE, na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  })
  sites <- do.call(rbind, site_rows)

  cell_groups <- split(seq_len(nrow(sites)), sites$exact_site_id)
  cell_rows <- lapply(cell_groups, function(index) {
    block <- sites[index, , drop = FALSE]
    data.frame(
      exact_site_id = block$exact_site_id[1L],
      n_landscape_sites = nrow(block),
      forest_human_edge_1km =
        v19_mean_finite(block$forest_human_edge_1km),
      forest_fraction_1km =
        v19_mean_finite(block$forest_fraction_1km),
      human_managed_fraction_1km =
        v19_mean_finite(block$human_managed_fraction_1km),
      represented_fraction_1km =
        v19_mean_finite(block$represented_fraction_1km),
      major_road_distance_km =
        v19_mean_finite(block$major_road_distance_km),
      primary_mesh_boundary = any(
        block$primary_mesh_boundary %in% TRUE, na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  })
  cells <- do.call(rbind, cell_rows)
  rownames(cells) <- NULL

  audit <- data.frame(
    metric = c(
      "n_observations", "n_observations_linked",
      "n_independent_sites", "n_v19_cells",
      "n_cells_complete_landscape", "n_primary_mesh_boundary_cells",
      "minimum_represented_fraction"
    ),
    value = c(
      nrow(observations), sum(!is.na(lookup_index)),
      nrow(sites), nrow(cells),
      sum(stats::complete.cases(cells[, c(
        "forest_human_edge_1km", "forest_fraction_1km",
        "human_managed_fraction_1km", "major_road_distance_km"
      )])),
      sum(cells$primary_mesh_boundary %in% TRUE, na.rm = TRUE),
      min(cells$represented_fraction_1km, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  list(cells = cells, sites = sites, linked = linked, audit = audit)
}

v19_landscape_features <- function(cells, landscape_cells) {
  required <- c(
    "exact_site_id", "human_population", "log_population_sum_25km",
    "elevation_within_25km", "spatial_fold"
  )
  landscape_required <- c(
    "exact_site_id", "forest_human_edge_1km", "forest_fraction_1km",
    "human_managed_fraction_1km", "major_road_distance_km",
    "primary_mesh_boundary"
  )
  v19_require_columns(cells, required, "cells")
  v19_require_columns(landscape_cells, landscape_required, "landscape cells")
  if (anyDuplicated(cells$exact_site_id) ||
      anyDuplicated(landscape_cells$exact_site_id)) {
    stop("Cell identifiers must be unique before the landscape join.",
         call. = FALSE)
  }
  index <- match(cells$exact_site_id, landscape_cells$exact_site_id)
  out <- data.frame(
    exact_site_id = as.character(cells$exact_site_id),
    spatial_fold = as.integer(cells$spatial_fold),
    local_population = log1p(pmax(as.numeric(cells$human_population), 0)),
    regional_population = as.numeric(cells$log_population_sum_25km),
    forest_human_edge = as.numeric(
      landscape_cells$forest_human_edge_1km[index]
    ),
    forest_fraction = as.numeric(
      landscape_cells$forest_fraction_1km[index]
    ),
    human_managed_fraction = as.numeric(
      landscape_cells$human_managed_fraction_1km[index]
    ),
    road_remoteness = log1p(pmax(as.numeric(
      landscape_cells$major_road_distance_km[index]
    ), 0)),
    relative_elevation = as.numeric(cells$elevation_within_25km),
    primary_mesh_boundary = as.logical(
      landscape_cells$primary_mesh_boundary[index]
    ),
    stringsAsFactors = FALSE
  )
  out$local_population_rank <- v19_rank01(out$local_population)
  out$regional_population_rank <- v19_rank01(out$regional_population)
  out$forest_human_edge_rank <- v19_rank01(out$forest_human_edge)
  out$forest_cover_rank <- v19_rank01(out$forest_fraction)
  out$managed_land_rank <- v19_rank01(out$human_managed_fraction)
  out$road_remoteness_rank <- v19_rank01(out$road_remoteness)
  out$road_proximity_rank <- 1 - out$road_remoteness_rank
  out$mountainness_rank <- v19_rank01(out$relative_elevation)

  land_total <- out$forest_fraction + out$human_managed_fraction
  out$forest_managed_balance <- ifelse(
    is.finite(land_total) & land_total > 0,
    1 - abs(out$forest_fraction - out$human_managed_fraction) / land_total,
    0
  )
  out$urban_adjacency_score <- rowMeans(out[, c(
    "local_population_rank", "regional_population_rank",
    "road_proximity_rank"
  )])
  out$human_reach_score <- rowMeans(out[, c(
    "local_population_rank", "regional_population_rank",
    "road_proximity_rank", "forest_human_edge_rank", "managed_land_rank"
  )])
  out$urban_mountain_score <- sqrt(
    pmax(out$urban_adjacency_score, 0) *
      pmax(out$mountainness_rank, 0)
  )
  out$satoyama_interface_score <- rowMeans(out[, c(
    "forest_human_edge_rank", "forest_managed_balance",
    "road_proximity_rank"
  )])
  out$remote_mountain_score <- rowMeans(cbind(
    out$mountainness_rank,
    out$forest_cover_rank,
    out$road_remoteness_rank,
    1 - out$regional_population_rank,
    1 - out$managed_land_rank
  ))
  profiles <- cbind(
    urban_mountain = out$urban_mountain_score,
    satoyama_interface = out$satoyama_interface_score,
    remote_mountain = out$remote_mountain_score
  )
  out$dominant_landscape_profile <- colnames(profiles)[
    max.col(profiles, ties.method = "first")
  ]
  out
}

v19_rf_predictors <- function() {
  c(
    "local_population_rank", "regional_population_rank",
    "forest_human_edge_rank", "forest_cover_rank", "managed_land_rank",
    "road_remoteness_rank", "mountainness_rank"
  )
}

v19_contrast_definitions <- function() {
  data.frame(
    feature = c(
      v19_rf_predictors(),
      "road_proximity_rank", "human_reach_score",
      "urban_mountain_score", "satoyama_interface_score",
      "remote_mountain_score"
    ),
    role = c(
      "human_exposure", "human_exposure", "land_use_interface",
      "natural_alternative", "land_use_context", "access",
      "natural_alternative", "access", "human_composite",
      "human_mountain_composite", "interface_composite",
      "natural_alternative_composite"
    ),
    hypothesis_direction = c(
      "greater", "greater", "greater", "two_sided", "greater", "less",
      "two_sided", "greater", "greater", "greater", "greater", "less"
    ),
    stringsAsFactors = FALSE
  )
}

v19_pair_contrasts <- function(pairs, features, feature_names) {
  out <- setNames(rep(NA_real_, length(feature_names)), feature_names)
  if (!nrow(pairs)) return(out)
  for (feature in feature_names) {
    value <- as.numeric(features[[feature]])
    difference <- value[pairs$case_index] - value[pairs$control_index]
    difference <- difference[is.finite(difference)]
    if (length(difference)) out[feature] <- mean(difference)
  }
  out
}

v19_pair_details <- function(pairs, features, feature_names, tier) {
  if (!nrow(pairs)) return(data.frame())
  rows <- pairs
  rows$tier <- tier
  rows$spatial_fold <- features$spatial_fold[rows$case_index]
  rows$case_profile <-
    features$dominant_landscape_profile[rows$case_index]
  rows$control_profile <-
    features$dominant_landscape_profile[rows$control_index]
  for (feature in feature_names) {
    rows[[paste0("case_", feature)]] <-
      features[[feature]][rows$case_index]
    rows[[paste0("control_", feature)]] <-
      features[[feature]][rows$control_index]
    rows[[paste0("difference_", feature)]] <-
      rows[[paste0("case_", feature)]] -
      rows[[paste0("control_", feature)]]
  }
  rows
}

v19_pair_dataset <- function(pairs, features, predictor_names) {
  if (!nrow(pairs)) return(data.frame())
  case <- features[pairs$case_index, predictor_names, drop = FALSE]
  control <- features[pairs$control_index, predictor_names, drop = FALSE]
  data.frame(
    outcome = factor(
      c(rep("case", nrow(pairs)), rep("control", nrow(pairs))),
      levels = c("control", "case")
    ),
    pair_id = rep(seq_len(nrow(pairs)), 2),
    spatial_fold = rep(
      features$spatial_fold[pairs$case_index], 2
    ),
    rbind(case, control),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

v19_auc <- function(outcome, probability) {
  outcome <- as.character(outcome)
  keep <- outcome %in% c("case", "control") & is.finite(probability)
  outcome <- outcome[keep]
  probability <- probability[keep]
  positive <- probability[outcome == "case"]
  negative <- probability[outcome == "control"]
  if (!length(positive) || !length(negative)) return(NA_real_)
  comparisons <- outer(positive, negative, "-")
  mean(comparisons > 0) + 0.5 * mean(comparisons == 0)
}

v19_spatial_rf <- function(pair_data, predictor_names, seed = 1901L,
                           num_trees = 200L) {
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop("Package 'ranger' is required.", call. = FALSE)
  }
  required <- c("outcome", "pair_id", "spatial_fold", predictor_names)
  v19_require_columns(pair_data, required, "pair data")
  complete <- stats::complete.cases(pair_data[, required, drop = FALSE])
  data <- pair_data[complete, , drop = FALSE]
  folds <- sort(unique(data$spatial_fold))
  probability <- rep(NA_real_, nrow(data))
  importance_rows <- list()
  if (nrow(data) < 12L || length(folds) < 2L) {
    return(list(
      metrics = c(
        spatial_cv_auc = NA_real_,
        paired_concordance = NA_real_,
        brier_score = NA_real_
      ),
      importance = setNames(rep(NA_real_, length(predictor_names)),
                            predictor_names),
      predictions = data.frame()
    ))
  }
  formula <- stats::reformulate(predictor_names, response = "outcome")
  for (fold in folds) {
    train <- data$spatial_fold != fold
    test <- !train
    if (sum(train) < 10L || !all(c("case", "control") %in%
                                  as.character(data$outcome[train]))) {
      next
    }
    fit <- ranger::ranger(
      formula, data = data[train, c("outcome", predictor_names), drop = FALSE],
      probability = TRUE, num.trees = num_trees,
      mtry = max(1L, floor(sqrt(length(predictor_names)))),
      min.node.size = 3L, importance = "permutation",
      seed = as.integer(seed + fold), num.threads = 1L
    )
    prediction <- predict(
      fit, data = data[test, predictor_names, drop = FALSE]
    )$predictions
    probability[test] <- prediction[, "case"]
    importance_rows[[length(importance_rows) + 1L]] <-
      fit$variable.importance[predictor_names]
  }
  case_probability <- tapply(
    probability[data$outcome == "case"],
    data$pair_id[data$outcome == "case"], v19_mean_finite
  )
  control_probability <- tapply(
    probability[data$outcome == "control"],
    data$pair_id[data$outcome == "control"], v19_mean_finite
  )
  common_pairs <- intersect(names(case_probability), names(control_probability))
  difference <- case_probability[common_pairs] -
    control_probability[common_pairs]
  paired_concordance <- if (length(difference)) {
    mean(difference > 0, na.rm = TRUE) +
      0.5 * mean(difference == 0, na.rm = TRUE)
  } else NA_real_
  truth <- as.numeric(data$outcome == "case")
  importance <- if (length(importance_rows)) {
    colMeans(do.call(rbind, importance_rows), na.rm = TRUE)
  } else {
    setNames(rep(NA_real_, length(predictor_names)), predictor_names)
  }
  list(
    metrics = c(
      spatial_cv_auc = v19_auc(data$outcome, probability),
      paired_concordance = paired_concordance,
      brier_score = mean((truth - probability)^2, na.rm = TRUE)
    ),
    importance = importance,
    predictions = data.frame(
      outcome = data$outcome, pair_id = data$pair_id,
      spatial_fold = data$spatial_fold, probability = probability,
      stringsAsFactors = FALSE
    )
  )
}

v19_regularized_distance <- function(matrix, center = NULL,
                                     covariance = NULL) {
  matrix <- as.matrix(matrix)
  if (is.null(center)) center <- colMeans(matrix, na.rm = TRUE)
  if (is.null(covariance)) covariance <- stats::cov(matrix, use = "pairwise")
  eigenvalue <- eigen(covariance, symmetric = TRUE, only.values = TRUE)$values
  ridge <- max(max(eigenvalue, na.rm = TRUE) * 1e-6, 1e-8)
  inverse <- solve(covariance + diag(ridge, ncol(covariance)))
  centered <- sweep(matrix, 2, center, "-")
  distance <- rowSums((centered %*% inverse) * centered)
  list(
    distance = distance, center = center, covariance = covariance,
    ridge = ridge
  )
}

v19_contrast_summary <- function(observed, null_matrix, definitions, tier) {
  null_matrix <- as.matrix(null_matrix[, definitions$feature, drop = FALSE])
  center <- colMeans(null_matrix, na.rm = TRUE)
  spread <- apply(null_matrix, 2, stats::sd, na.rm = TRUE)
  spread[!is.finite(spread) | spread <= 1e-12] <- 1
  null_z <- sweep(sweep(null_matrix, 2, center, "-"), 2, spread, "/")
  observed_z <- (observed[definitions$feature] - center) / spread
  direction_sign <- ifelse(
    definitions$hypothesis_direction == "less", -1, 1
  )
  directional_null <- sweep(null_z, 2, direction_sign, "*")
  directional_observed <- observed_z * direction_sign
  maximum_directional <- apply(directional_null, 1, max, na.rm = TRUE)
  maximum_absolute <- apply(abs(null_z), 1, max, na.rm = TRUE)

  rows <- lapply(seq_len(nrow(definitions)), function(index) {
    feature <- definitions$feature[index]
    simulated <- null_matrix[, feature]
    simulated <- simulated[is.finite(simulated)]
    value <- observed[feature]
    p_upper <- (1 + sum(simulated >= value)) / (length(simulated) + 1)
    p_lower <- (1 + sum(simulated <= value)) / (length(simulated) + 1)
    direction <- definitions$hypothesis_direction[index]
    directional_p <- if (direction == "less") {
      p_lower
    } else if (direction == "greater") {
      p_upper
    } else {
      min(1, 2 * min(p_upper, p_lower))
    }
    fwer_p <- if (direction == "two_sided") {
      (1 + sum(maximum_absolute >= abs(observed_z[index]))) /
        (sum(is.finite(maximum_absolute)) + 1)
    } else {
      (1 + sum(maximum_directional >= directional_observed[index])) /
        (sum(is.finite(maximum_directional)) + 1)
    }
    data.frame(
      tier = tier, feature = feature, role = definitions$role[index],
      hypothesis_direction = direction,
      observed_case_control_difference = value,
      null_mean = mean(simulated), null_sd = stats::sd(simulated),
      lower_95 = unname(stats::quantile(simulated, 0.025)),
      upper_95 = unname(stats::quantile(simulated, 0.975)),
      directional_or_two_sided_p = directional_p,
      two_sided_p = min(1, 2 * min(p_upper, p_lower)),
      maxT_FWER_p = fwer_p,
      observed_standardized_departure = observed_z[index],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

v19_global_landscape_test <- function(
    observed, null_matrix, raw_features, tier) {
  matrix <- as.matrix(null_matrix[, raw_features, drop = FALSE])
  keep <- stats::complete.cases(matrix)
  matrix <- matrix[keep, , drop = FALSE]
  fitted <- v19_regularized_distance(matrix)
  observed_distance <- v19_regularized_distance(
    matrix(observed[raw_features], nrow = 1L),
    center = fitted$center, covariance = fitted$covariance
  )$distance
  p <- (1 + sum(fitted$distance >= observed_distance)) /
    (length(fitted$distance) + 1)
  data.frame(
    tier = tier,
    metric = "regularized_Mahalanobis_landscape_departure",
    observed_value = observed_distance,
    null_mean = mean(fitted$distance),
    lower_95 = unname(stats::quantile(fitted$distance, 0.025)),
    upper_95 = unname(stats::quantile(fitted$distance, 0.975)),
    empirical_p = p,
    n_null_draws = nrow(matrix),
    covariance_ridge = fitted$ridge,
    stringsAsFactors = FALSE
  )
}

v19_profile_counts <- function(pairs, features, tier) {
  levels <- c("urban_mountain", "satoyama_interface", "remote_mountain")
  case_profile <- factor(
    features$dominant_landscape_profile[pairs$case_index], levels = levels
  )
  control_profile <- factor(
    features$dominant_landscape_profile[pairs$control_index], levels = levels
  )
  data.frame(
    tier = tier,
    landscape_profile = levels,
    case_count = as.integer(table(case_profile)),
    control_count = as.integer(table(control_profile)),
    case_fraction = as.numeric(table(case_profile)) / max(length(case_profile), 1),
    control_fraction =
      as.numeric(table(control_profile)) / max(length(control_profile), 1),
    stringsAsFactors = FALSE
  )
}

v19_collinearity <- function(features, predictor_names) {
  matrix <- as.matrix(features[, predictor_names, drop = FALSE])
  correlation <- stats::cor(matrix, use = "pairwise.complete.obs",
                            method = "spearman")
  rows <- which(upper.tri(correlation), arr.ind = TRUE)
  data.frame(
    feature_1 = rownames(correlation)[rows[, 1L]],
    feature_2 = colnames(correlation)[rows[, 2L]],
    spearman_rho = correlation[rows],
    absolute_spearman_rho = abs(correlation[rows]),
    stringsAsFactors = FALSE
  )
}
