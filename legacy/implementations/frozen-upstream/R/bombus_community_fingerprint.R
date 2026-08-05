fingerprint_species <- function() {
  c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
}

fingerprint_add_axes <- function(sites) {
  species <- fingerprint_species()
  source_columns <- paste0("bee_", species)
  missing <- setdiff(source_columns, names(sites))
  if (length(missing)) {
    stop("Missing ENMeval columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  suitability <- as.matrix(sites[source_columns])
  storage.mode(suitability) <- "double"
  common <- apply(suitability, 1, function(x) all(is.finite(x)))
  ranks <- apply(suitability, 2, community_positive_rank)
  if (is.null(dim(ranks))) ranks <- matrix(ranks, ncol = length(species))
  colnames(ranks) <- paste0(species, "_within_species_rank")

  total_support <- rep(NA_real_, nrow(sites))
  total_support[common] <- rowMeans(ranks[common, , drop = FALSE])
  relative <- matrix(NA_real_, nrow(sites), length(species))
  colnames(relative) <- paste0(species, "_relative_weight")
  totals <- rowSums(ranks[common, , drop = FALSE])
  relative[common, ] <- ranks[common, , drop = FALSE] / pmax(totals, 1e-12)
  hellinger <- sqrt(relative)

  fit <- stats::prcomp(
    hellinger[common, , drop = FALSE], center = TRUE, scale. = FALSE
  )
  scores <- fit$x[, seq_len(min(2L, ncol(fit$x))), drop = FALSE]
  rotation <- fit$rotation[, seq_len(ncol(scores)), drop = FALSE]
  for (component in seq_len(ncol(scores))) {
    anchor <- which.max(abs(rotation[, component]))
    if (rotation[anchor, component] < 0) {
      scores[, component] <- -scores[, component]
      rotation[, component] <- -rotation[, component]
    }
  }
  pc1 <- pc2 <- rep(NA_real_, nrow(sites))
  pc1[common] <- scores[, 1]
  if (ncol(scores) >= 2L) pc2[common] <- scores[, 2]
  variance <- summary(fit)$importance

  out <- cbind(sites, as.data.frame(ranks), as.data.frame(relative))
  out$bombus_fingerprint_common_support <- common
  out$bombus_total_habitat_support <- total_support
  out$bombus_composition_pc1 <- pc1
  out$bombus_composition_pc2 <- pc2
  loadings <- do.call(rbind, lapply(seq_len(ncol(rotation)), function(j) {
    data.frame(
      component = paste0("PC", j), species = species,
      loading = as.numeric(rotation[, j]), stringsAsFactors = FALSE
    )
  }))
  variance_table <- data.frame(
    component = colnames(variance),
    proportion_variance = as.numeric(variance["Proportion of Variance", ]),
    cumulative_variance = as.numeric(variance["Cumulative Proportion", ]),
    stringsAsFactors = FALSE
  )
  list(data = out, loadings = loadings, variance = variance_table)
}

fingerprint_spatial_k <- function(n, requested = 40L) {
  max(10L, min(as.integer(requested), floor(n / 20)))
}

fingerprint_formula <- function(response, model, env_terms, n, k_space = 40L) {
  spatial <- paste0("s(x_km, y_km, k = ", fingerprint_spatial_k(n, k_space), ")")
  fingerprint <- c(
    "s(bombus_total_habitat_support, k = 5)",
    "s(bombus_composition_pc1, k = 5)",
    "s(bombus_composition_pc2, k = 5)"
  )
  rhs <- switch(
    model,
    intercept_only = "1",
    elevation_only = "s(elevation, k = 5)",
    environment_only = env_terms,
    space_only = spatial,
    elevation_space = c("s(elevation, k = 5)", spatial),
    environment_space = c(env_terms, spatial),
    environment_space_fingerprint = c(env_terms, spatial, fingerprint),
    stop("Unknown v14 model: ", model, call. = FALSE)
  )
  stats::as.formula(
    paste(response, "~", paste(rhs, collapse = " + ")),
    env = asNamespace("mgcv")
  )
}

fingerprint_model_required <- function(model, env_terms) {
  switch(
    model,
    intercept_only = character(),
    elevation_only = "elevation",
    environment_only = env_terms,
    space_only = c("x_km", "y_km"),
    elevation_space = c("elevation", "x_km", "y_km"),
    environment_space = c(env_terms, "x_km", "y_km"),
    environment_space_fingerprint = c(
      env_terms, "x_km", "y_km", "bombus_total_habitat_support",
      "bombus_composition_pc1", "bombus_composition_pc2"
    ),
    stop("Unknown v14 model: ", model, call. = FALSE)
  )
}

fingerprint_crossvalidate <- function(sites, outcome = c("presence", "intensity"),
                                      k_space = 40L) {
  outcome <- match.arg(outcome)
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required.", call. = FALSE)
  }
  env_terms <- community_environment_terms(sites)
  if (outcome == "presence") {
    response <- "pigment_share"
    weight <- "n_observations"
    family <- stats::binomial()
    models <- c("environment_space", "environment_space_fingerprint")
  } else {
    response <- "pigment_intensity_z"
    weight <- "n_pigmented"
    family <- stats::gaussian()
    models <- c(
      "intercept_only", "elevation_only", "environment_only", "space_only",
      "elevation_space", "environment_space", "environment_space_fingerprint"
    )
  }
  base_required <- c(
    response, weight, "spatial_fold", "exact_site_id",
    "bombus_fingerprint_common_support"
  )
  output <- list()
  logs <- list()
  for (model in models) {
    required <- unique(c(base_required, fingerprint_model_required(model, env_terms)))
    data <- sites[
      sites$bombus_fingerprint_common_support &
        stats::complete.cases(sites[required]) & as.numeric(sites[[weight]]) > 0,
      , drop = FALSE
    ]
    for (fold in sort(unique(as.integer(round(data$spatial_fold))))) {
      train <- data[data$spatial_fold != fold, , drop = FALSE]
      test <- data[data$spatial_fold == fold, , drop = FALSE]
      train$.weight <- as.numeric(train[[weight]])
      formula <- fingerprint_formula(response, model, env_terms, nrow(train), k_space)
      captured <- community_capture_fit(mgcv::gam(
        formula, data = train, weights = .weight,
        family = family, method = "REML"
      ))
      if (!inherits(captured$value, "gam")) {
        stop("v14 model failed: ", outcome, " / ", model, call. = FALSE)
      }
      prediction <- as.numeric(stats::predict(
        captured$value, newdata = test, type = "response"
      ))
      output[[length(output) + 1L]] <- data.frame(
        outcome = outcome, model = model, heldout_spatial_fold = fold,
        exact_site_id = test$exact_site_id,
        observed = as.numeric(test[[response]]), prediction = prediction,
        weight = as.numeric(test[[weight]]),
        n_pigmented = as.numeric(test$n_pigmented),
        n_white = as.numeric(test$n_white), stringsAsFactors = FALSE
      )
      logs[[length(logs) + 1L]] <- data.frame(
        outcome = outcome, model = model, heldout_spatial_fold = fold,
        n_train = nrow(train), n_test = nrow(test),
        formula = paste(deparse(formula), collapse = " "),
        warnings = captured$warnings, stringsAsFactors = FALSE
      )
    }
  }
  list(predictions = do.call(rbind, output), log = do.call(rbind, logs))
}

fingerprint_cv_summary <- function(predictions) {
  keys <- unique(predictions[c("outcome", "model")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    data <- merge(keys[i, , drop = FALSE], predictions, by = c("outcome", "model"))
    if (keys$outcome[i] == "presence") {
      probability <- pmin(pmax(data$prediction, 1e-8), 1 - 1e-8)
      log_loss <- -sum(
        data$n_pigmented * log(probability) + data$n_white * log(1 - probability)
      ) / sum(data$n_pigmented + data$n_white)
      homogeneous <- data$n_pigmented == 0 | data$n_white == 0
      auc <- transition_binary_auc(
        as.integer(data$n_pigmented[homogeneous] > 0), probability[homogeneous]
      )
      brier <- sum(
        data$weight * (data$observed - probability)^2
      ) / sum(data$weight)
      data.frame(
        outcome = keys$outcome[i], model = keys$model[i], n = nrow(data),
        RMSE = NA_real_, MAE = NA_real_, heldout_R2 = NA_real_,
        log_loss = log_loss, brier = brier, AUC = auc
      )
    } else {
      residual <- data$observed - data$prediction
      centre <- stats::weighted.mean(data$observed, data$weight)
      sse <- sum(data$weight * residual^2)
      sst <- sum(data$weight * (data$observed - centre)^2)
      data.frame(
        outcome = keys$outcome[i], model = keys$model[i], n = nrow(data),
        RMSE = sqrt(sse / sum(data$weight)),
        MAE = sum(data$weight * abs(residual)) / sum(data$weight),
        heldout_R2 = 1 - sse / sst,
        log_loss = NA_real_, brier = NA_real_, AUC = NA_real_
      )
    }
  })
  do.call(rbind, rows)
}

fingerprint_cv_contrasts <- function(summary) {
  comparisons <- data.frame(
    outcome = c("presence", rep("intensity", 4)),
    contrast = c(
      "community_fingerprint_beyond_environment_space",
      "total_elevation_cline", "elevation_beyond_space",
      "environment_beyond_space",
      "community_fingerprint_beyond_environment_space"
    ),
    baseline = c(
      "environment_space", "intercept_only", "space_only", "space_only",
      "environment_space"
    ),
    added = c(
      "environment_space_fingerprint", "elevation_only", "elevation_space",
      "environment_space", "environment_space_fingerprint"
    ), stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(comparisons)), function(i) {
    spec <- comparisons[i, ]
    base <- summary[summary$outcome == spec$outcome & summary$model == spec$baseline, ]
    added <- summary[summary$outcome == spec$outcome & summary$model == spec$added, ]
    if (!nrow(base) || !nrow(added)) return(data.frame())
    data.frame(
      spec,
      delta_RMSE = base$RMSE - added$RMSE,
      delta_MAE = base$MAE - added$MAE,
      delta_heldout_R2 = added$heldout_R2 - base$heldout_R2,
      delta_log_loss = base$log_loss - added$log_loss,
      delta_brier = base$brier - added$brier,
      delta_AUC = added$AUC - base$AUC,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

fingerprint_fit_curves <- function(sites, k_space = 40L) {
  env_terms <- community_environment_terms(sites)
  required <- unique(c(
    "pigment_intensity_z", "n_pigmented", "elevation", "x_km", "y_km",
    env_terms, "bombus_total_habitat_support", "bombus_composition_pc1",
    "bombus_composition_pc2"
  ))
  data <- sites[
    sites$bombus_fingerprint_common_support &
      stats::complete.cases(sites[required]) & sites$n_pigmented > 0,
    , drop = FALSE
  ]
  data$.weight <- data$n_pigmented
  models <- c("elevation_only", "elevation_space", "environment_space_fingerprint")
  fits <- lapply(models, function(model) {
    mgcv::gam(
      fingerprint_formula(
        "pigment_intensity_z", model, env_terms, nrow(data), k_space
      ), data = data, weights = .weight, family = stats::gaussian(), method = "REML"
    )
  })
  names(fits) <- models
  template <- data[rep(1L, 80L), , drop = FALSE]
  for (column in names(template)) {
    if (is.numeric(template[[column]])) {
      template[[column]] <- stats::median(data[[column]], na.rm = TRUE)
    }
  }
  elevation_grid <- seq(
    stats::quantile(data$elevation, 0.02),
    stats::quantile(data$elevation, 0.98), length.out = nrow(template)
  )
  elevation_curves <- do.call(rbind, lapply(c("elevation_only", "elevation_space"), function(model) {
    newdata <- template
    newdata$elevation <- elevation_grid
    data.frame(
      model = model, elevation_m = elevation_grid,
      predicted_intensity_z = as.numeric(stats::predict(fits[[model]], newdata)),
      stringsAsFactors = FALSE
    )
  }))
  support_grid <- seq(
    stats::quantile(data$bombus_total_habitat_support, 0.02),
    stats::quantile(data$bombus_total_habitat_support, 0.98),
    length.out = nrow(template)
  )
  newdata <- template
  newdata$bombus_total_habitat_support <- support_grid
  community_curve <- data.frame(
    bombus_total_habitat_support = support_grid,
    predicted_intensity_z = as.numeric(stats::predict(
      fits$environment_space_fingerprint, newdata
    )), stringsAsFactors = FALSE
  )
  list(elevation = elevation_curves, community = community_curve)
}

fingerprint_extract_population_context <- function(sites, raster_path,
                                                   radii_km = c(10, 25, 50)) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required.", call. = FALSE)
  }
  if (!file.exists(raster_path)) stop("WorldPop raster not found: ", raster_path)
  raster <- terra::rast(raster_path)
  points <- terra::vect(
    sites, geom = c("longitude", "latitude"), crs = "EPSG:4326"
  )
  out <- sites[c("exact_site_id", "longitude", "latitude")]
  for (radius in radii_km) {
    extracted <- terra::extract(
      raster, points, buffer = radius * 1000, fun = sum, na.rm = TRUE
    )
    value <- as.numeric(extracted[[ncol(extracted)]])
    out[[paste0("population_sum_", radius, "km")]] <- value
    out[[paste0("log_population_sum_", radius, "km")]] <- log1p(pmax(value, 0))
    out[[paste0("z_population_sum_", radius, "km")]] <-
      transition_z(out[[paste0("log_population_sum_", radius, "km")]])
  }
  out
}

fingerprint_anomaly_ranks <- function(sites, natural_probability,
                                      intensity_residual, phenology_residual) {
  neighbourhoods <- transition_local_neighbourhoods(sites)
  local <- neighbourhoods[neighbourhoods$radius_km == 25, , drop = FALSE]
  out <- sites
  out$natural_presence_probability_v14 <- natural_probability
  out$intensity_natural_residual_v14 <- intensity_residual
  out$early_natural_score_v14 <- -phenology_residual
  index <- match(out$exact_site_id, local$exact_site_id)
  out$n_observed_neighbours_25km <- local$n_neighbours[index]
  out$neighbour_pigment_share_25km <- local$neighbour_pigment_share[index]
  out$observation_effort_z <- transition_z(log1p(out$n_observed_neighbours_25km))
  out$direction <- ifelse(
    out$site_class == "pigmented", "unexpected_pigmented",
    ifelse(out$site_class == "white", "unexpected_white", NA_character_)
  )
  out$natural_surprise <- ifelse(
    out$site_class == "pigmented", 1 - natural_probability,
    ifelse(out$site_class == "white", natural_probability, NA_real_)
  )
  out$directional_rank <- NA_real_
  for (direction in c("unexpected_pigmented", "unexpected_white")) {
    keep <- out$direction == direction & is.finite(out$natural_surprise)
    out$directional_rank[keep] <- transition_rank01(out$natural_surprise[keep])
  }
  out$top_05 <- out$directional_rank >= 0.95
  out$top_10 <- out$directional_rank >= 0.90
  out$top_20 <- out$directional_rank >= 0.80
  out[!is.na(out$direction), , drop = FALSE]
}

fingerprint_cluster_slope <- function(data, facet, repetitions = 2000L,
                                      adjust_effort = TRUE) {
  required <- c("natural_surprise", facet, "block_x", "block_y")
  if (adjust_effort) required <- c(required, "observation_effort_z")
  data <- data[stats::complete.cases(data[required]), , drop = FALSE]
  data$anomaly_z <- transition_z(data$natural_surprise)
  data$cluster <- paste(data$block_x, data$block_y, sep = "_")
  formula <- stats::as.formula(paste(
    facet, "~ anomaly_z",
    if (adjust_effort) "+ observation_effort_z" else ""
  ))
  estimate <- unname(stats::coef(stats::lm(formula, data = data))["anomaly_z"])
  clusters <- unique(data$cluster)
  set.seed(20260723L)
  bootstrap <- replicate(repetitions, {
    sampled <- sample(clusters, length(clusters), replace = TRUE)
    boot <- do.call(rbind, lapply(seq_along(sampled), function(i) {
      block <- data[data$cluster == sampled[i], , drop = FALSE]
      block$.bootstrap_cluster <- i
      block
    }))
    fit <- try(stats::lm(formula, data = boot), silent = TRUE)
    if (inherits(fit, "try-error")) NA_real_ else unname(stats::coef(fit)["anomaly_z"])
  })
  bootstrap <- bootstrap[is.finite(bootstrap)]
  data.frame(
    facet = facet, n = nrow(data), n_spatial_blocks = length(clusters),
    slope_per_1sd_natural_surprise = estimate,
    lower_95 = stats::quantile(bootstrap, 0.025, na.rm = TRUE),
    upper_95 = stats::quantile(bootstrap, 0.975, na.rm = TRUE),
    bootstrap_p = 2 * min(mean(bootstrap <= 0), mean(bootstrap >= 0)),
    effort_adjusted = adjust_effort, stringsAsFactors = FALSE
  )
}

fingerprint_horticulture_facets <- function(anomalies, repetitions = 2000L) {
  positive <- anomalies[anomalies$direction == "unexpected_pigmented", , drop = FALSE]
  facets <- intersect(c(
    "z_population_sum_10km", "z_population_sum_25km", "z_population_sum_50km",
    "intensity_natural_residual_v14", "early_natural_score_v14",
    "neighbour_pigment_share_25km"
  ), names(positive))
  positive_rows <- do.call(rbind, lapply(facets, function(facet) {
    fingerprint_cluster_slope(positive, facet, repetitions, adjust_effort = TRUE)
  }))
  positive_rows$direction <- "unexpected_pigmented"

  negative <- anomalies[anomalies$direction == "unexpected_white", , drop = FALSE]
  negative_facets <- intersect(c(
    "z_population_sum_10km", "z_population_sum_25km", "z_population_sum_50km"
  ), names(negative))
  negative_rows <- do.call(rbind, lapply(negative_facets, function(facet) {
    fingerprint_cluster_slope(negative, facet, repetitions, adjust_effort = TRUE)
  }))
  negative_rows$direction <- "unexpected_white_negative_control"
  out <- rbind(positive_rows, negative_rows)
  out$BH_q <- stats::p.adjust(out$bootstrap_p, method = "BH")
  out
}
