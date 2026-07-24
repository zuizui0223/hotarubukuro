multiscale_environment_sources <- function(cache_root) {
  wanted <- c(
    elevation = "elevation_Japan_crop.tif",
    temperature = "bio10_Japan_crop_30s.tif",
    precipitation = "bio12_Japan_crop_30s.tif",
    radiation = "RSDS_Japan_crop_30s.tif"
  )
  paths <- vapply(wanted, function(filename) {
    hits <- list.files(
      cache_root, pattern = paste0("^", gsub("\\.", "\\\\.", filename), "$"),
      recursive = TRUE, full.names = TRUE
    )
    if (length(hits) != 1L) {
      stop("Expected one raster named ", filename, "; found ", length(hits),
           call. = FALSE)
    }
    hits
  }, character(1))
  paths
}

multiscale_point_context <- function(raster, sites, radii_km,
                                     summary_function = c("mean", "sum")) {
  summary_function <- match.arg(summary_function)
  raster_values <- as.matrix(raster, wide = TRUE)
  x_centres <- terra::xFromCol(raster, seq_len(terra::ncol(raster)))
  y_centres <- terra::yFromRow(raster, seq_len(terra::nrow(raster)))
  result <- matrix(
    NA_real_, nrow = nrow(sites), ncol = length(radii_km),
    dimnames = list(NULL, as.character(radii_km))
  )
  maximum_radius <- max(radii_km)
  for (index in seq_len(nrow(sites))) {
    longitude <- sites$longitude[index]
    latitude <- sites$latitude[index]
    longitude_km <- 111.32 * cos(latitude * pi / 180)
    rows <- which(abs(y_centres - latitude) * 110.57 <= maximum_radius)
    columns <- which(abs(x_centres - longitude) * longitude_km <= maximum_radius)
    if (!length(rows) || !length(columns)) next
    y_distance <- (y_centres[rows] - latitude) * 110.57
    x_distance <- (x_centres[columns] - longitude) * longitude_km
    distance <- sqrt(outer(y_distance^2, x_distance^2, "+"))
    values <- raster_values[rows, columns, drop = FALSE]
    for (radius_index in seq_along(radii_km)) {
      selected <- values[distance <= radii_km[radius_index] & is.finite(values)]
      if (!length(selected)) next
      result[index, radius_index] <- if (summary_function == "mean") {
        mean(selected)
      } else {
        sum(selected)
      }
    }
  }
  result
}

multiscale_environment_context <- function(sites, raster_paths,
                                           radii_km = c(25, 50, 100),
                                           aggregate_factor = 6L) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required.", call. = FALSE)
  }
  points <- terra::vect(
    sites, geom = c("longitude", "latitude"), crs = "EPSG:4326"
  )
  out <- sites[c("exact_site_id", "longitude", "latitude")]
  provenance <- list()
  for (variable in names(raster_paths)) {
    raster <- terra::rast(raster_paths[[variable]])
    fine <- terra::extract(raster, points, method = "bilinear")
    out[[paste0(variable, "_cell")]] <- as.numeric(fine[[ncol(fine)]])
    coarse <- terra::aggregate(
      raster, fact = aggregate_factor, fun = mean, na.rm = TRUE
    )
    broad_values <- multiscale_point_context(
      coarse, sites, radii_km, summary_function = "mean"
    )
    for (radius in radii_km) {
      broad_name <- paste0(variable, "_broad_", radius, "km")
      local_name <- paste0(variable, "_within_", radius, "km")
      out[[broad_name]] <- broad_values[, as.character(radius)]
      out[[local_name]] <- out[[paste0(variable, "_cell")]] - out[[broad_name]]
    }
    provenance[[length(provenance) + 1L]] <- data.frame(
      variable = variable,
      raster_path = normalizePath(raster_paths[[variable]], winslash = "/"),
      raster_md5 = unname(tools::md5sum(raster_paths[[variable]])),
      aggregate_factor = aggregate_factor,
      broad_context_method = "site-latitude distance mean on 0.05-degree cell centres",
      stringsAsFactors = FALSE
    )
  }
  list(context = out, provenance = do.call(rbind, provenance))
}

multiscale_population_context <- function(sites, raster_path,
                                          radii_km = 25,
                                          aggregate_factor = 6L) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required.", call. = FALSE)
  }
  if (!file.exists(raster_path)) {
    stop("WorldPop raster not found: ", raster_path, call. = FALSE)
  }
  raster <- terra::rast(raster_path)
  points <- terra::vect(
    sites, geom = c("longitude", "latitude"), crs = "EPSG:4326"
  )
  out <- sites[c("exact_site_id", "longitude", "latitude")]
  coarse <- terra::aggregate(
    raster, fact = aggregate_factor, fun = sum, na.rm = TRUE
  )
  summed_values <- multiscale_point_context(
    coarse, sites, radii_km, summary_function = "sum"
  )
  for (radius in radii_km) {
    value <- summed_values[, as.character(radius)]
    out[[paste0("population_sum_", radius, "km")]] <- value
    out[[paste0("log_population_sum_", radius, "km")]] <- log1p(pmax(value, 0))
  }
  list(
    context = out,
    provenance = data.frame(
      variable = "WorldPop_2020_population_count",
      raster_path = normalizePath(raster_path, winslash = "/"),
      raster_md5 = unname(tools::md5sum(raster_path)),
      aggregate_factor = aggregate_factor,
      broad_context_method = "site-latitude distance sum on 0.05-degree cell centres",
      stringsAsFactors = FALSE
    )
  )
}

multiscale_oriented_pca <- function(data, columns, prefix, n_components = 2L) {
  matrix <- as.matrix(data[columns])
  storage.mode(matrix) <- "double"
  complete <- stats::complete.cases(matrix)
  fit <- stats::prcomp(matrix[complete, , drop = FALSE], center = TRUE, scale. = TRUE)
  n_components <- min(n_components, ncol(fit$x))
  scores <- fit$x[, seq_len(n_components), drop = FALSE]
  rotation <- fit$rotation[, seq_len(n_components), drop = FALSE]
  for (component in seq_len(n_components)) {
    anchor <- which.max(abs(rotation[, component]))
    if (rotation[anchor, component] < 0) {
      scores[, component] <- -scores[, component]
      rotation[, component] <- -rotation[, component]
    }
  }
  score_data <- data.frame(matrix(NA_real_, nrow(data), n_components))
  names(score_data) <- paste0(prefix, "_pc", seq_len(n_components))
  score_data[complete, ] <- scores
  loadings <- do.call(rbind, lapply(seq_len(n_components), function(component) {
    data.frame(
      axis = prefix, component = paste0("PC", component),
      variable = rownames(rotation), loading = as.numeric(rotation[, component]),
      proportion_variance = summary(fit)$importance[
        "Proportion of Variance", component
      ], stringsAsFactors = FALSE
    )
  }))
  list(scores = score_data, loadings = loadings)
}

multiscale_add_environment_axes <- function(context, radii_km = c(25, 50, 100)) {
  out <- context
  loadings <- list()
  variables <- c("elevation", "temperature", "precipitation", "radiation")
  for (radius in radii_km) {
    broad_columns <- paste0(variables, "_broad_", radius, "km")
    within_columns <- paste0(variables, "_within_", radius, "km")
    broad <- multiscale_oriented_pca(
      out, broad_columns, paste0("broad", radius, "km")
    )
    within <- multiscale_oriented_pca(
      out, within_columns, paste0("within", radius, "km")
    )
    out <- cbind(out, broad$scores, within$scores)
    loadings[[length(loadings) + 1L]] <- broad$loadings
    loadings[[length(loadings) + 1L]] <- within$loadings
  }
  list(data = out, loadings = do.call(rbind, loadings))
}

multiscale_cell_phenotypes <- function(observations, cells) {
  required <- c(
    "exact_site_id", "x_km", "y_km", "pigmented_mixture50",
    "pigment_intensity_z", "DOY", "year"
  )
  missing <- setdiff(required, names(observations))
  if (length(missing)) {
    stop("Missing phenotype columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  observations$cell_id_v15 <- paste0(
    "cell-1km-", floor(observations$x_km), "_", floor(observations$y_km)
  )
  site_groups <- split(
    seq_len(nrow(observations)),
    paste(observations$cell_id_v15, observations$exact_site_id, sep = "::")
  )
  site_rows <- lapply(site_groups, function(index) {
    block <- observations[index, , drop = FALSE]
    pigmented <- block$pigmented_mixture50 == 1L
    data.frame(
      cell_id_v15 = block$cell_id_v15[1],
      source_exact_site_id = as.character(block$exact_site_id[1]),
      site_pigment_share = mean(block$pigmented_mixture50, na.rm = TRUE),
      site_intensity = if (any(pigmented)) {
        stats::median(block$pigment_intensity_z[pigmented], na.rm = TRUE)
      } else NA_real_,
      site_DOY = stats::median(block$DOY, na.rm = TRUE),
      site_year = stats::median(block$year, na.rm = TRUE),
      n_images = nrow(block), stringsAsFactors = FALSE
    )
  })
  site_data <- do.call(rbind, site_rows)
  groups <- split(seq_len(nrow(site_data)), site_data$cell_id_v15)
  rows <- lapply(groups, function(index) {
    block <- site_data[index, , drop = FALSE]
    intensity <- block$site_intensity[is.finite(block$site_intensity)]
    n_sites <- nrow(block)
    share <- mean(block$site_pigment_share)
    gini <- if (n_sites >= 2L) {
      min(1, n_sites / (n_sites - 1) * 2 * share * (1 - share))
    } else NA_real_
    pairwise <- if (length(intensity) >= 2L) {
      as.numeric(stats::dist(intensity))
    } else numeric()
    data.frame(
      exact_site_id = block$cell_id_v15[1],
      n_independent_sites = n_sites,
      n_images = sum(block$n_images),
      n_pigmented_sites = length(intensity),
      site_weighted_pigment_share = share,
      binary_heterogeneity = gini,
      mixed_hotspot_observed = as.integer(n_sites >= 2L && share > 0 && share < 1),
      conditional_intensity_median = if (length(intensity)) {
        stats::median(intensity)
      } else NA_real_,
      conditional_intensity_pairwise_median = if (length(pairwise)) {
        stats::median(pairwise)
      } else NA_real_,
      conditional_intensity_pairwise_max = if (length(pairwise)) {
        max(pairwise)
      } else NA_real_,
      median_DOY = stats::median(block$site_DOY, na.rm = TRUE),
      median_year = stats::median(block$site_year, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  phenotype <- do.call(rbind, rows)
  phenotype$log_independent_sites <- log1p(phenotype$n_independent_sites)
  phenotype$log_pigmented_sites <- log1p(phenotype$n_pigmented_sites)
  phenotype$log_intensity_dispersion <- log1p(
    phenotype$conditional_intensity_pairwise_median
  )
  merged <- merge(cells, phenotype, by = "exact_site_id", all.x = TRUE, sort = FALSE)
  list(cells = merged, exact_sites = site_data)
}

multiscale_environment_terms <- function(radius_km) {
  c(
    paste0("broad", radius_km, "km_pc1"),
    paste0("broad", radius_km, "km_pc2"),
    paste0("within", radius_km, "km_pc1"),
    paste0("within", radius_km, "km_pc2")
  )
}

multiscale_fingerprint_terms <- function() {
  c(
    "bombus_total_habitat_support",
    "bombus_composition_pc1", "bombus_composition_pc2"
  )
}

multiscale_orthogonal_fingerprint_terms <- function() {
  paste0(multiscale_fingerprint_terms(), "_environment_orthogonal")
}

multiscale_orthogonalize_fingerprint <- function(train, test, radius_km) {
  environment <- multiscale_environment_terms(radius_km)
  fingerprint <- multiscale_fingerprint_terms()
  orthogonal <- multiscale_orthogonal_fingerprint_terms()
  previous <- character()
  for (index in seq_along(fingerprint)) {
    formula <- stats::reformulate(
      c(environment, previous), response = fingerprint[index]
    )
    fit <- stats::lm(formula, data = train)
    train[[orthogonal[index]]] <- as.numeric(stats::residuals(fit))
    test[[orthogonal[index]]] <- as.numeric(
      test[[fingerprint[index]]] - stats::predict(fit, newdata = test)
    )
    previous <- c(previous, fingerprint[index])
  }
  list(train = train, test = test)
}

multiscale_outcome_spec <- function(outcome) {
  switch(
    outcome,
    mixed_hotspot = list(
      response = "mixed_hotspot_observed", family = stats::binomial(),
      eligibility = function(data) data$n_independent_sites >= 2L,
      effort = "log_independent_sites"
    ),
    intensity_dispersion = list(
      response = "log_intensity_dispersion", family = stats::gaussian(),
      eligibility = function(data) data$n_pigmented_sites >= 2L,
      effort = "log_pigmented_sites"
    ),
    intensity_median = list(
      response = "conditional_intensity_median", family = stats::gaussian(),
      eligibility = function(data) data$n_pigmented_sites >= 1L,
      effort = "log_pigmented_sites"
    ),
    stop("Unknown v15 outcome: ", outcome, call. = FALSE)
  )
}

multiscale_hotspot_formula <- function(response, model, radius_km, n,
                                       k_space = 30L, effort) {
  environment <- multiscale_environment_terms(radius_km)
  fingerprint <- multiscale_fingerprint_terms()
  orthogonal_fingerprint <- multiscale_orthogonal_fingerprint_terms()
  k_use <- max(8L, min(as.integer(k_space), floor(n / 15)))
  spatial <- paste0("s(x_km, y_km, k = ", k_use, ")")
  rhs <- switch(
    model,
    effort_only = effort,
    scale_environment = c(effort, environment),
    scale_environment_fingerprint = c(effort, environment, fingerprint),
    scale_environment_spatial = c(effort, environment, spatial),
    scale_environment_spatial_fingerprint = c(
      effort, environment, spatial, fingerprint
    ),
    scale_environment_spatial_fingerprint_orthogonal = c(
      effort, environment, spatial, orthogonal_fingerprint
    ),
    stop("Unknown v15 model: ", model, call. = FALSE)
  )
  stats::as.formula(
    paste(response, "~", paste(rhs, collapse = " + ")),
    env = asNamespace("mgcv")
  )
}

multiscale_hotspot_cv <- function(cells, radii_km = c(25, 50, 100),
                                  outcomes = c(
                                    "mixed_hotspot", "intensity_dispersion",
                                    "intensity_median"
                                  ), k_space = 30L) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required.", call. = FALSE)
  }
  models <- c(
    "effort_only", "scale_environment", "scale_environment_fingerprint",
    "scale_environment_spatial", "scale_environment_spatial_fingerprint",
    "scale_environment_spatial_fingerprint_orthogonal"
  )
  predictions <- list()
  logs <- list()
  for (outcome in outcomes) {
    spec <- multiscale_outcome_spec(outcome)
    for (radius in radii_km) {
      required <- unique(c(
        spec$response, spec$effort, "spatial_fold", "x_km", "y_km",
        "exact_site_id", multiscale_environment_terms(radius),
        multiscale_fingerprint_terms(), "bombus_fingerprint_common_support"
      ))
      data <- cells[
        spec$eligibility(cells) & cells$bombus_fingerprint_common_support &
          stats::complete.cases(cells[required]), , drop = FALSE
      ]
      for (model in models) {
        for (fold in sort(unique(as.integer(round(data$spatial_fold))))) {
          train <- data[data$spatial_fold != fold, , drop = FALSE]
          test <- data[data$spatial_fold == fold, , drop = FALSE]
          if (nrow(train) < 30L || nrow(test) < 3L) next
          if (model == "scale_environment_spatial_fingerprint_orthogonal") {
            orthogonalized <- multiscale_orthogonalize_fingerprint(
              train, test, radius
            )
            train <- orthogonalized$train
            test <- orthogonalized$test
          }
          formula <- multiscale_hotspot_formula(
            spec$response, model, radius, nrow(train), k_space, spec$effort
          )
          captured <- community_capture_fit(mgcv::gam(
            formula, data = train, family = spec$family, method = "REML"
          ))
          if (!inherits(captured$value, "gam")) next
          probability <- as.numeric(stats::predict(
            captured$value, newdata = test, type = "response"
          ))
          predictions[[length(predictions) + 1L]] <- data.frame(
            outcome = outcome, radius_km = radius, model = model,
            heldout_spatial_fold = fold, exact_site_id = test$exact_site_id,
            observed = as.numeric(test[[spec$response]]), prediction = probability,
            stringsAsFactors = FALSE
          )
          logs[[length(logs) + 1L]] <- data.frame(
            outcome = outcome, radius_km = radius, model = model,
            heldout_spatial_fold = fold, n_train = nrow(train), n_test = nrow(test),
            formula = paste(deparse(formula), collapse = " "),
            warnings = captured$warnings, stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  list(predictions = do.call(rbind, predictions), log = do.call(rbind, logs))
}

multiscale_hotspot_cv_summary <- function(predictions) {
  keys <- unique(predictions[c("outcome", "radius_km", "model")])
  rows <- lapply(seq_len(nrow(keys)), function(index) {
    key <- keys[index, ]
    data <- predictions[
      predictions$outcome == key$outcome &
        predictions$radius_km == key$radius_km &
        predictions$model == key$model, , drop = FALSE
    ]
    if (key$outcome == "mixed_hotspot") {
      probability <- pmin(pmax(data$prediction, 1e-8), 1 - 1e-8)
      data.frame(
        key, n = nrow(data), n_folds = length(unique(data$heldout_spatial_fold)),
        log_loss = -mean(
          data$observed * log(probability) +
            (1 - data$observed) * log(1 - probability)
        ),
        brier = mean((data$observed - probability)^2),
        AUC = transition_binary_auc(data$observed, probability),
        RMSE = NA_real_, MAE = NA_real_, heldout_R2 = NA_real_,
        stringsAsFactors = FALSE
      )
    } else {
      residual <- data$observed - data$prediction
      sse <- sum(residual^2)
      sst <- sum((data$observed - mean(data$observed))^2)
      data.frame(
        key, n = nrow(data), n_folds = length(unique(data$heldout_spatial_fold)),
        log_loss = NA_real_, brier = NA_real_, AUC = NA_real_,
        RMSE = sqrt(mean(residual^2)), MAE = mean(abs(residual)),
        heldout_R2 = 1 - sse / sst, stringsAsFactors = FALSE
      )
    }
  })
  do.call(rbind, rows)
}

multiscale_hotspot_contrasts <- function(summary) {
  comparisons <- data.frame(
    contrast = c(
      "environment_beyond_effort", "fingerprint_beyond_scaled_environment",
      "space_beyond_scaled_environment",
      "fingerprint_beyond_scaled_environment_and_space",
      "orthogonal_fingerprint_beyond_scaled_environment_and_space"
    ),
    baseline = c(
      "effort_only", "scale_environment", "scale_environment",
      "scale_environment_spatial", "scale_environment_spatial"
    ),
    added = c(
      "scale_environment", "scale_environment_fingerprint",
      "scale_environment_spatial", "scale_environment_spatial_fingerprint",
      "scale_environment_spatial_fingerprint_orthogonal"
    ), stringsAsFactors = FALSE
  )
  keys <- unique(summary[c("outcome", "radius_km")])
  rows <- list()
  for (key_index in seq_len(nrow(keys))) {
    key <- keys[key_index, ]
    for (comparison_index in seq_len(nrow(comparisons))) {
      comparison <- comparisons[comparison_index, ]
      base <- summary[
        summary$outcome == key$outcome & summary$radius_km == key$radius_km &
          summary$model == comparison$baseline, , drop = FALSE
      ]
      added <- summary[
        summary$outcome == key$outcome & summary$radius_km == key$radius_km &
          summary$model == comparison$added, , drop = FALSE
      ]
      if (!nrow(base) || !nrow(added)) next
      rows[[length(rows) + 1L]] <- data.frame(
        key, comparison,
        delta_log_loss = base$log_loss - added$log_loss,
        delta_brier = base$brier - added$brier,
        delta_AUC = added$AUC - base$AUC,
        delta_RMSE = base$RMSE - added$RMSE,
        delta_MAE = base$MAE - added$MAE,
        delta_heldout_R2 = added$heldout_R2 - base$heldout_R2,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

multiscale_hotspot_fold_metrics <- function(predictions) {
  keys <- unique(predictions[c(
    "outcome", "radius_km", "model", "heldout_spatial_fold"
  )])
  rows <- lapply(seq_len(nrow(keys)), function(index) {
    key <- keys[index, ]
    data <- predictions[
      predictions$outcome == key$outcome &
        predictions$radius_km == key$radius_km &
        predictions$model == key$model &
        predictions$heldout_spatial_fold == key$heldout_spatial_fold,
      , drop = FALSE
    ]
    if (key$outcome == "mixed_hotspot") {
      probability <- pmin(pmax(data$prediction, 1e-8), 1 - 1e-8)
      data.frame(
        key, n = nrow(data),
        log_loss = -mean(
          data$observed * log(probability) +
            (1 - data$observed) * log(1 - probability)
        ),
        AUC = transition_binary_auc(data$observed, probability),
        RMSE = NA_real_, stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        key, n = nrow(data), log_loss = NA_real_, AUC = NA_real_,
        RMSE = sqrt(mean((data$observed - data$prediction)^2)),
        stringsAsFactors = FALSE
      )
    }
  })
  do.call(rbind, rows)
}

multiscale_hotspot_fold_contrasts <- function(fold_metrics) {
  comparisons <- data.frame(
    contrast = c(
      "environment_beyond_effort", "fingerprint_beyond_scaled_environment",
      "space_beyond_scaled_environment",
      "fingerprint_beyond_scaled_environment_and_space",
      "orthogonal_fingerprint_beyond_scaled_environment_and_space"
    ),
    baseline = c(
      "effort_only", "scale_environment", "scale_environment",
      "scale_environment_spatial", "scale_environment_spatial"
    ),
    added = c(
      "scale_environment", "scale_environment_fingerprint",
      "scale_environment_spatial", "scale_environment_spatial_fingerprint",
      "scale_environment_spatial_fingerprint_orthogonal"
    ), stringsAsFactors = FALSE
  )
  keys <- unique(fold_metrics[c("outcome", "radius_km")])
  rows <- list()
  for (key_index in seq_len(nrow(keys))) {
    key <- keys[key_index, ]
    for (comparison_index in seq_len(nrow(comparisons))) {
      comparison <- comparisons[comparison_index, ]
      base <- fold_metrics[
        fold_metrics$outcome == key$outcome &
          fold_metrics$radius_km == key$radius_km &
          fold_metrics$model == comparison$baseline, , drop = FALSE
      ]
      added <- fold_metrics[
        fold_metrics$outcome == key$outcome &
          fold_metrics$radius_km == key$radius_km &
          fold_metrics$model == comparison$added, , drop = FALSE
      ]
      paired <- merge(
        base, added, by = "heldout_spatial_fold",
        suffixes = c("_base", "_added")
      )
      if (!nrow(paired)) next
      improvement <- if (key$outcome == "mixed_hotspot") {
        paired$log_loss_base - paired$log_loss_added
      } else {
        paired$RMSE_base - paired$RMSE_added
      }
      rows[[length(rows) + 1L]] <- data.frame(
        key, comparison, n_folds = length(improvement),
        n_folds_improved = sum(improvement > 0),
        median_primary_metric_improvement = stats::median(improvement),
        minimum_primary_metric_improvement = min(improvement),
        maximum_primary_metric_improvement = max(improvement),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

multiscale_collinearity_audit <- function(cells, radii_km = c(25, 50, 100)) {
  rows <- list()
  for (radius in radii_km) {
    environment <- multiscale_environment_terms(radius)
    fingerprint <- multiscale_fingerprint_terms()
    required <- c(environment, fingerprint, "bombus_fingerprint_common_support")
    data <- cells[
      cells$bombus_fingerprint_common_support &
        stats::complete.cases(cells[required]), , drop = FALSE
    ]
    orthogonalized <- multiscale_orthogonalize_fingerprint(data, data, radius)
    sets <- list(
      raw = c(environment, fingerprint),
      environment_orthogonal = c(
        environment, multiscale_orthogonal_fingerprint_terms()
      )
    )
    bases <- list(raw = data, environment_orthogonal = orthogonalized$train)
    for (basis in names(sets)) {
      columns <- sets[[basis]]
      values <- bases[[basis]][columns]
      correlation <- stats::cor(values)
      diag(correlation) <- NA_real_
      vif <- vapply(seq_along(columns), function(index) {
        fit <- stats::lm(
          values[[index]] ~ ., data = values[, -index, drop = FALSE]
        )
        1 / (1 - summary(fit)$r.squared)
      }, numeric(1))
      rows[[length(rows) + 1L]] <- data.frame(
        radius_km = radius, basis = basis, n = nrow(values), term = columns,
        VIF = vif,
        maximum_absolute_pairwise_correlation = max(
          abs(correlation), na.rm = TRUE
        ), stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

multiscale_crossfit_presence_fingerprint <- function(cells, radius_km = 50,
                                                     k_space = 30L) {
  required <- unique(c(
    "pigment_share", "n_observations", "spatial_fold", "x_km", "y_km",
    multiscale_environment_terms(radius_km), multiscale_fingerprint_terms(),
    "bombus_fingerprint_common_support"
  ))
  data <- cells[
    cells$bombus_fingerprint_common_support &
      stats::complete.cases(cells[required]), , drop = FALSE
  ]
  probability <- rep(NA_real_, nrow(cells))
  logs <- list()
  for (fold in sort(unique(as.integer(round(data$spatial_fold))))) {
    train <- data[data$spatial_fold != fold, , drop = FALSE]
    test <- data[data$spatial_fold == fold, , drop = FALSE]
    k_use <- max(8L, min(as.integer(k_space), floor(nrow(train) / 15)))
    rhs <- c(
      multiscale_environment_terms(radius_km),
      paste0("s(x_km, y_km, k = ", k_use, ")"),
      multiscale_fingerprint_terms()
    )
    formula <- stats::as.formula(
      paste("pigment_share ~", paste(rhs, collapse = " + ")),
      env = asNamespace("mgcv")
    )
    captured <- community_capture_fit(mgcv::gam(
      formula, data = train, weights = n_observations,
      family = stats::binomial(), method = "REML"
    ))
    if (!inherits(captured$value, "gam")) next
    index <- match(test$exact_site_id, cells$exact_site_id)
    probability[index] <- pmin(pmax(as.numeric(stats::predict(
      captured$value, newdata = test, type = "response"
    )), 1e-8), 1 - 1e-8)
    logs[[length(logs) + 1L]] <- data.frame(
      heldout_spatial_fold = fold, n_train = nrow(train), n_test = nrow(test),
      formula = paste(deparse(formula), collapse = " "),
      warnings = captured$warnings, stringsAsFactors = FALSE
    )
  }
  list(probability = probability, log = do.call(rbind, logs))
}

multiscale_crossfit_phenology <- function(cells, radius_km = 50,
                                         k_space = 30L) {
  required <- unique(c(
    "median_DOY", "median_year", "spatial_fold", "x_km", "y_km",
    multiscale_environment_terms(radius_km)
  ))
  data <- cells[stats::complete.cases(cells[required]), , drop = FALSE]
  prediction <- rep(NA_real_, nrow(cells))
  logs <- list()
  for (fold in sort(unique(as.integer(round(data$spatial_fold))))) {
    train <- data[data$spatial_fold != fold, , drop = FALSE]
    test <- data[data$spatial_fold == fold, , drop = FALSE]
    k_use <- max(8L, min(as.integer(k_space), floor(nrow(train) / 15)))
    rhs <- c(
      multiscale_environment_terms(radius_km), "s(median_year, k = 5)",
      paste0("s(x_km, y_km, k = ", k_use, ")")
    )
    formula <- stats::as.formula(
      paste("median_DOY ~", paste(rhs, collapse = " + ")),
      env = asNamespace("mgcv")
    )
    captured <- community_capture_fit(mgcv::gam(
      formula, data = train, family = stats::gaussian(), method = "REML"
    ))
    if (!inherits(captured$value, "gam")) next
    index <- match(test$exact_site_id, cells$exact_site_id)
    prediction[index] <- as.numeric(stats::predict(
      captured$value, newdata = test, type = "response"
    ))
    logs[[length(logs) + 1L]] <- data.frame(
      heldout_spatial_fold = fold, n_train = nrow(train), n_test = nrow(test),
      formula = paste(deparse(formula), collapse = " "),
      warnings = captured$warnings, stringsAsFactors = FALSE
    )
  }
  list(prediction = prediction, log = do.call(rbind, logs))
}

multiscale_natural_ranks <- function(cells, natural_probability) {
  out <- cells
  out$natural_presence_probability_v15 <- natural_probability
  out$natural_surprise_direction <- ifelse(
    out$site_class == "pigmented", "unexpected_pigmented",
    ifelse(out$site_class == "white", "unexpected_white", NA_character_)
  )
  out$natural_surprise <- ifelse(
    out$site_class == "pigmented", 1 - natural_probability,
    ifelse(out$site_class == "white", natural_probability, NA_real_)
  )
  out$natural_surprise_rank <- NA_real_
  for (direction in c("unexpected_pigmented", "unexpected_white")) {
    keep <- out$natural_surprise_direction == direction &
      is.finite(out$natural_surprise)
    out$natural_surprise_rank[keep] <- transition_rank01(out$natural_surprise[keep])
  }
  out$top_05 <- out$natural_surprise_rank >= 0.95
  out$top_10 <- out$natural_surprise_rank >= 0.90
  out$top_20 <- out$natural_surprise_rank >= 0.80
  out
}

multiscale_candidate_diagnostics <- function(ranked_cells, cv_predictions,
                                             phenology_prediction,
                                             fingerprint_probability,
                                             radius_km = 50) {
  out <- ranked_cells
  intensity <- cv_predictions[
    cv_predictions$outcome == "intensity_median" &
      cv_predictions$radius_km == radius_km &
      cv_predictions$model == "scale_environment_spatial_fingerprint",
    c("exact_site_id", "prediction"), drop = FALSE
  ]
  intensity <- intensity[!duplicated(intensity$exact_site_id), , drop = FALSE]
  out$natural_intensity_prediction_v15 <- intensity$prediction[
    match(out$exact_site_id, intensity$exact_site_id)
  ]
  out$natural_intensity_surprise_v15 <- out$conditional_intensity_median -
    out$natural_intensity_prediction_v15
  out$natural_DOY_prediction_v15 <- phenology_prediction
  out$early_phenology_surprise_v15 <- phenology_prediction - out$median_DOY
  out$fingerprint_presence_probability_v15 <- fingerprint_probability
  out$fingerprint_adjusted_surprise_v15 <- ifelse(
    out$site_class == "pigmented", 1 - fingerprint_probability,
    ifelse(out$site_class == "white", fingerprint_probability, NA_real_)
  )
  out$fingerprint_adjusted_rank_v15 <- NA_real_
  for (direction in c("unexpected_pigmented", "unexpected_white")) {
    keep <- out$natural_surprise_direction == direction &
      is.finite(out$fingerprint_adjusted_surprise_v15)
    out$fingerprint_adjusted_rank_v15[keep] <- transition_rank01(
      out$fingerprint_adjusted_surprise_v15[keep]
    )
  }
  out
}

multiscale_cluster_tier_difference <- function(data, facet, tier,
                                               repetitions = 1000L) {
  required <- c(facet, tier, "block_x", "block_y")
  data <- data[stats::complete.cases(data[required]), , drop = FALSE]
  top <- as.logical(data[[tier]])
  data$cluster <- paste(data$block_x, data$block_y, sep = "_")
  clusters <- unique(data$cluster)
  if (nrow(data) < 2L || !any(top) || all(top) || length(clusters) < 2L) {
    return(data.frame(
      facet = facet, tier = tier, n = nrow(data), n_top = sum(top),
      n_spatial_blocks = length(clusters), mean_top_minus_other = NA_real_,
      lower_95 = NA_real_, upper_95 = NA_real_, bootstrap_p = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  value <- as.numeric(data[[facet]])
  estimate <- mean(value[top]) - mean(value[!top])
  cluster_factor <- factor(data$cluster, levels = clusters)
  n_top <- as.numeric(rowsum(as.integer(top), cluster_factor, reorder = FALSE))
  n_other <- as.numeric(rowsum(as.integer(!top), cluster_factor, reorder = FALSE))
  sum_top <- as.numeric(rowsum(ifelse(top, value, 0), cluster_factor,
                               reorder = FALSE))
  sum_other <- as.numeric(rowsum(ifelse(!top, value, 0), cluster_factor,
                                 reorder = FALSE))
  set.seed(20260724L)
  frequency <- stats::rmultinom(
    repetitions, size = length(clusters),
    prob = rep(1 / length(clusters), length(clusters))
  )
  top_denominator <- colSums(frequency * n_top)
  other_denominator <- colSums(frequency * n_other)
  bootstrap <- colSums(frequency * sum_top) / top_denominator -
    colSums(frequency * sum_other) / other_denominator
  bootstrap <- bootstrap[is.finite(bootstrap)]
  p_lower <- (sum(bootstrap <= 0) + 1) / (length(bootstrap) + 1)
  p_upper <- (sum(bootstrap >= 0) + 1) / (length(bootstrap) + 1)
  data.frame(
    facet = facet, tier = tier, n = nrow(data), n_top = sum(top),
    n_spatial_blocks = length(clusters), mean_top_minus_other = estimate,
    lower_95 = unname(stats::quantile(bootstrap, 0.025)),
    upper_95 = unname(stats::quantile(bootstrap, 0.975)),
    bootstrap_p = min(1, 2 * min(p_lower, p_upper)),
    stringsAsFactors = FALSE
  )
}

multiscale_horticulture_facets <- function(ranked_cells, repetitions = 1000L) {
  facets <- intersect(c(
    "conditional_intensity_median", "natural_intensity_surprise_v15",
    "log_intensity_dispersion", "median_DOY", "early_phenology_surprise_v15",
    "log_population_sum_25km", "n_independent_sites", "n_years"
  ), names(ranked_cells))
  tiers <- c("top_05", "top_10", "top_20")
  rows <- list()
  for (direction in c("unexpected_pigmented", "unexpected_white")) {
    data <- ranked_cells[
      ranked_cells$natural_surprise_direction == direction, , drop = FALSE
    ]
    direction_facets <- if (direction == "unexpected_white") {
      intersect(c(
        "median_DOY", "early_phenology_surprise_v15",
        "log_population_sum_25km", "n_independent_sites", "n_years"
      ), facets)
    } else facets
    for (facet in direction_facets) {
      for (tier in tiers) {
        row <- multiscale_cluster_tier_difference(
          data, facet, tier, repetitions
        )
        row$direction <- direction
        rows[[length(rows) + 1L]] <- row
      }
    }
  }
  out <- do.call(rbind, rows)
  out$BH_q <- stats::p.adjust(out$bootstrap_p, method = "BH")
  out
}

multiscale_horticulture_evidence_matrix <- function(ranked_cells) {
  data <- ranked_cells[
    ranked_cells$natural_surprise_direction == "unexpected_pigmented" &
      is.finite(ranked_cells$natural_surprise_rank), , drop = FALSE
  ]
  rank_available <- function(x) {
    out <- rep(NA_real_, length(x))
    keep <- is.finite(x)
    if (any(keep)) out[keep] <- transition_rank01(x[keep])
    out
  }
  data$intensity_excess_rank <- rank_available(
    data$natural_intensity_surprise_v15
  )
  data$early_phenology_rank <- rank_available(
    data$early_phenology_surprise_v15
  )
  data$population_context_rank <- rank_available(
    data$log_population_sum_25km
  )
  data$distribution_top20 <- data$natural_surprise_rank >= 0.8
  data$intensity_top20 <- data$intensity_excess_rank >= 0.8
  data$early_top20 <- data$early_phenology_rank >= 0.8
  data$population_top20 <- data$population_context_rank >= 0.8
  facet_flags <- c("intensity_top20", "early_top20", "population_top20")
  facet_values <- as.data.frame(lapply(data[facet_flags], function(x) {
    ifelse(is.na(x), FALSE, x)
  }))
  data$n_independent_positive_facets <- rowSums(facet_values)
  data$n_available_positive_facets <- rowSums(cbind(
    is.finite(data$intensity_excess_rank),
    is.finite(data$early_phenology_rank),
    is.finite(data$population_context_rank)
  ))
  data$fingerprint_top20_concordant <-
    data$fingerprint_adjusted_rank_v15 >= 0.8
  data$replication_adequate <- data$n_independent_sites >= 2L | data$n_years >= 2L
  data$followup_tier <- ifelse(
    data$distribution_top20 & data$n_independent_positive_facets >= 2L &
      data$replication_adequate,
    "A_convergent_replicated",
    ifelse(
      data$distribution_top20 & data$n_independent_positive_facets >= 1L &
        data$replication_adequate,
      "B_partial_replicated",
      ifelse(
        data$distribution_top20,
        "C_distribution_only_or_sparse", "D_not_distribution_tail"
      )
    )
  )
  order_tier <- match(data$followup_tier, c(
    "A_convergent_replicated", "B_partial_replicated",
    "C_distribution_only_or_sparse", "D_not_distribution_tail"
  ))
  data[order(
    order_tier, -data$n_independent_positive_facets,
    -data$natural_surprise_rank
  ), , drop = FALSE]
}
