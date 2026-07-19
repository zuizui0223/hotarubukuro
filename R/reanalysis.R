# Deterministic descriptive and sensitivity-analysis helpers.
#
# These functions deliberately avoid causal language and do not revive the
# legacy CFA, INLA, or residual qGAM stages. Standard errors from the simple
# linear models are retained only as naive diagnostics; spatially blocked
# prediction is reported separately.

read_reanalysis_config <- function(path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required for the reanalysis configuration.", call. = FALSE)
  }
  if (!file.exists(path)) stop("Reanalysis config not found: ", path, call. = FALSE)
  config <- yaml::read_yaml(path)
  required <- c(
    "version", "analysis_version", "colour_methods", "qc_scenarios",
    "outcomes", "candidate_environment_predictors",
    "environment_predictors", "excluded_candidate_predictors", "bombus_predictor",
    "population_density_source", "model_variants", "spatial_block_cv",
    "primary_outcome", "secondary_composite", "interpretation"
  )
  missing <- setdiff(required, names(config))
  if (length(missing)) {
    stop("Reanalysis config is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!identical(
    as.character(unlist(config$colour_methods)),
    analysis_colour_methods()
  )) {
    stop("Configured colour methods do not match the reviewed selector.", call. = FALSE)
  }
  expected_qc <- list(
    inclusive = c("ok", "manual_review_required"),
    strict_ok = "ok"
  )
  qc_names <- names(config$qc_scenarios)
  qc_values <- lapply(config$qc_scenarios, function(value) {
    as.character(unlist(value, use.names = FALSE))
  })
  if (!identical(sort(qc_names), sort(names(expected_qc))) ||
      any(vapply(qc_values, function(value) {
        !length(value) || anyNA(value) || any(!nzchar(value)) ||
          anyDuplicated(value) > 0L
      }, logical(1))) ||
      !setequal(qc_values$inclusive, expected_qc$inclusive) ||
      !setequal(qc_values$strict_ok, expected_qc$strict_ok) ||
      !all(qc_values$strict_ok %in% qc_values$inclusive) ||
      length(qc_values$strict_ok) >= length(qc_values$inclusive)) {
    stop(
      paste(
        "qc_scenarios must define inclusive = ok + manual_review_required",
        "and strict_ok = ok as a proper subset."
      ),
      call. = FALSE
    )
  }
  outcomes <- as.character(unlist(config$outcomes))
  allowed_predictive_outcomes <- c("a", "L", "b")
  if (!length(outcomes) || anyDuplicated(outcomes) ||
      anyNA(outcomes) || any(!nzchar(outcomes)) ||
      !identical(as.character(config$primary_outcome), "a") ||
      !identical(
        as.character(config$secondary_composite),
        "primary_reference_PC1"
      ) ||
      !("a" %in% outcomes) ||
      any(!outcomes %in% allowed_predictive_outcomes)) {
    stop(
      paste(
        "Reviewed primary and secondary roles require unique predictive",
        "outcomes drawn from a, L, b and including a;",
        "primary_reference_PC1 is descriptive only."
      ),
      call. = FALSE
    )
  }
  predictors <- as.character(unlist(config$environment_predictors))
  if (!length(predictors) || anyDuplicated(predictors) ||
      anyNA(predictors) || any(!nzchar(predictors))) {
    stop("Environment predictors must be non-empty and unique.", call. = FALSE)
  }
  candidate_predictors <- as.character(unlist(
    config$candidate_environment_predictors,
    use.names = FALSE
  ))
  if (!length(candidate_predictors) || anyDuplicated(candidate_predictors) ||
      anyNA(candidate_predictors) || any(!nzchar(candidate_predictors))) {
    stop(
      "Candidate predictors must be non-empty and unique.",
      call. = FALSE
    )
  }
  bombus_predictor <- as.character(config$bombus_predictor)
  population_source <- as.character(config$population_density_source)
  if (!identical(bombus_predictor, "Bombus_suitability_sum")) {
    stop(
      "bombus_predictor must be the reviewed Bombus_suitability_sum variable.",
      call. = FALSE
    )
  }
  if (!identical(population_source, "worldpop_2020_density")) {
    stop(
      "population_density_source must be worldpop_2020_density.",
      call. = FALSE
    )
  }
  predictor_contract <- unique(c(
    candidate_predictors,
    predictors,
    population_source
  ))
  overlap <- unique(c(
    intersect(outcomes, predictor_contract),
    intersect(outcomes, bombus_predictor),
    intersect(predictor_contract, bombus_predictor),
    intersect(c(candidate_predictors, predictors), population_source)
  ))
  if (length(overlap)) {
    stop(
      "Outcomes, environmental/population predictors, and Bombus predictor must not overlap: ",
      paste(overlap, collapse = ", "),
      call. = FALSE
    )
  }
  reviewed_candidates <- c(
    "chelsa_bio10", "chelsa_bio12", "chelsa_cmimean", "chelsa_vpdmean",
    "soilgrids_bdod_0_5cm", "soilgrids_nitrogen_0_5cm",
    "soilgrids_phh2o_0_5cm", "worldclim_elevation_30s",
    "log_worldpop_2020_density"
  )
  reviewed_selected <- c(
    "chelsa_bio10", "chelsa_bio12", "soilgrids_bdod_0_5cm",
    "soilgrids_nitrogen_0_5cm", "soilgrids_phh2o_0_5cm",
    "log_worldpop_2020_density"
  )
  excluded <- config$excluded_candidate_predictors
  excluded_names <- names(excluded)
  excluded_reasons <- as.character(unlist(excluded, use.names = FALSE))
  if (!identical(candidate_predictors, reviewed_candidates) ||
      !identical(predictors, reviewed_selected) ||
      !setequal(excluded_names, setdiff(reviewed_candidates, reviewed_selected)) ||
      length(excluded_reasons) != length(excluded_names) ||
      anyNA(excluded_reasons) || any(!nzchar(trimws(excluded_reasons)))) {
    stop(
      paste(
        "The candidate, selected, and excluded predictor sets must match",
        "the reviewed non-redundant specification."
      ),
      call. = FALSE
    )
  }

  cv_names <- names(config$spatial_block_cv)
  allowed_cv_names <- c("method", "folds", "seed")
  if (!all(c("method", "folds") %in% cv_names) ||
      length(setdiff(cv_names, allowed_cv_names))) {
    stop(
      "spatial_block_cv must contain method and folds, with optional seed only.",
      call. = FALSE
    )
  }
  fold_value <- config$spatial_block_cv$folds
  folds <- suppressWarnings(as.integer(fold_value))
  if (!is.numeric(fold_value) || is.logical(fold_value) ||
      length(folds) != 1L || is.na(folds) || folds < 2L ||
      !identical(as.numeric(folds), as.numeric(config$spatial_block_cv$folds))) {
    stop("At least two spatial folds are required.", call. = FALSE)
  }
  if (!is.null(config$spatial_block_cv$seed)) {
    seed_value <- config$spatial_block_cv$seed
    seed <- suppressWarnings(as.integer(seed_value))
    if (!is.numeric(seed_value) || is.logical(seed_value) ||
        length(seed) != 1L || is.na(seed) || seed < 0L ||
        !identical(
          as.numeric(seed),
          as.numeric(seed_value)
        )) {
      stop("spatial_block_cv seed must be a single non-negative integer.", call. = FALSE)
    }
  }
  allowed_variants <- c(
    "environment_only_full",
    "environment_only_bombus_common",
    "environment_plus_bombus"
  )
  if (!identical(sort(names(config$model_variants)), sort(allowed_variants)) ||
      !all(vapply(config$model_variants, isTRUE, logical(1)))) {
    stop(
      "All reviewed environment-only and Bombus-comparison variants must be enabled.",
      call. = FALSE
    )
  }
  if (!identical(
    as.character(config$spatial_block_cv$method),
    "principal_axis_equal_site_bands"
  )) {
    stop("Unsupported spatial block method.", call. = FALSE)
  }
  config
}

reviewed_logical <- function(value, field) {
  if (is.logical(value)) {
    if (anyNA(value)) stop(field, " cannot contain missing values.", call. = FALSE)
    return(value)
  }
  text <- tolower(trimws(as.character(value)))
  result <- rep(NA, length(text))
  result[text %in% c("true", "t", "1")] <- TRUE
  result[text %in% c("false", "f", "0")] <- FALSE
  if (anyNA(result)) stop(field, " must contain explicit true/false values.", call. = FALSE)
  result
}

validate_reviewed_data_s1 <- function(data, expected_extraction_version = "2.2.2") {
  required <- c(
    "observation_id", "source_row", "image_sha256", "duplicate_image_sha256",
    "date", "latitude", "longitude", "coordinate_source",
    "coordinate_crs_assumed", "coordinate_recomputed", "coordinate_qc_status",
    "photo_coordinate_qc_status", "exact_site_id", "colour_measurement_scope",
    "neutral_reference_available", "illumination_correction_status",
    "median_R", "median_G", "median_B", "R", "G", "B",
    "white_balance_applied", "white_balance_method", "white_balance_reliability",
    "color_statistic", "primary_colour_method", "extraction_version",
    "processed_at", "qc_status"
  )
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(
      "Reviewed Data_S1 contract is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(data)) stop("Reviewed Data_S1 cannot be empty.", call. = FALSE)
  scalar_contract <- list(
    coordinate_source = "source_workbook",
    coordinate_crs_assumed = "EPSG:4326",
    coordinate_qc_status = "source_value_not_independently_recomputed",
    colour_measurement_scope = "uncalibrated_display_referred_sRGB",
    illumination_correction_status = "not_identifiable",
    white_balance_method = "none",
    white_balance_reliability = "unavailable",
    color_statistic = "median",
    primary_colour_method = "median_hsv_mask_v2_1_compatible",
    extraction_version = expected_extraction_version
  )
  for (field in names(scalar_contract)) {
    value <- unique(as.character(data[[field]]))
    if (length(value) != 1L || is.na(value) ||
        !identical(value, as.character(scalar_contract[[field]]))) {
      stop(
        "Reviewed Data_S1 has unsupported ", field, "; expected ",
        scalar_contract[[field]], ".",
        call. = FALSE
      )
    }
  }
  if (any(reviewed_logical(data$coordinate_recomputed, "coordinate_recomputed")) ||
      any(reviewed_logical(data$neutral_reference_available, "neutral_reference_available")) ||
      any(reviewed_logical(data$white_balance_applied, "white_balance_applied"))) {
    stop(
      "Reviewed Data_S1 must record unrecomputed coordinates and no identifiable white balance.",
      call. = FALSE
    )
  }
  ids <- as.character(data$observation_id)
  site_ids <- as.character(data$exact_site_id)
  hashes <- tolower(as.character(data$image_sha256))
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids) ||
      anyNA(data$source_row) || anyDuplicated(data$source_row) ||
      anyNA(site_ids) || any(!nzchar(site_ids)) ||
      any(!grepl("^[0-9a-f]{64}$", hashes))) {
    stop("Reviewed Data_S1 identity columns are invalid.", call. = FALSE)
  }
  duplicated_hash <- duplicated(hashes) | duplicated(hashes, fromLast = TRUE)
  declared_duplicate <- reviewed_logical(
    data$duplicate_image_sha256,
    "duplicate_image_sha256"
  )
  if (!identical(declared_duplicate, duplicated_hash)) {
    stop("duplicate_image_sha256 does not match the image hashes.", call. = FALSE)
  }
  coordinate_key <- paste(
    sprintf("%.10f", as.numeric(data$longitude)),
    sprintf("%.10f", as.numeric(data$latitude)),
    sep = "|"
  )
  if (any(!is.finite(as.numeric(data$longitude))) ||
      any(!is.finite(as.numeric(data$latitude)))) {
    stop("Reviewed Data_S1 coordinates must be finite.", call. = FALSE)
  }
  if (any(vapply(split(site_ids, coordinate_key), function(value) {
    length(unique(value)) != 1L
  }, logical(1))) || any(vapply(split(coordinate_key, site_ids), function(value) {
    length(unique(value)) != 1L
  }, logical(1)))) {
    stop("exact_site_id is not in one-to-one correspondence with exact coordinates.", call. = FALSE)
  }
  expected_photo_status <- ifelse(
    duplicated_hash,
    "manual_review_required_duplicate_photo_at_multiple_coordinates",
    "mapped_by_workbook_cell_and_image_hash"
  )
  if (!identical(as.character(data$photo_coordinate_qc_status), expected_photo_status)) {
    stop("Photo-coordinate mapping status is inconsistent with image hashes.", call. = FALSE)
  }
  rgb <- as.matrix(data[c("R", "G", "B")])
  median_rgb <- as.matrix(data[c("median_R", "median_G", "median_B")])
  storage.mode(rgb) <- "double"
  storage.mode(median_rgb) <- "double"
  if (any(!is.finite(rgb)) || any(!is.finite(median_rgb)) ||
      !isTRUE(all.equal(rgb, median_rgb, tolerance = 0, check.attributes = FALSE))) {
    stop("Reviewed Data_S1 primary RGB must equal median_R/G/B exactly.", call. = FALSE)
  }
  qc <- as.character(data$qc_status)
  if (anyNA(qc) || any(!qc %in% c("ok", "manual_review_required"))) {
    stop("Reviewed Data_S1 has unsupported qc_status values.", call. = FALSE)
  }
  processed_at <- unique(as.character(data$processed_at))
  if (length(processed_at) != 1L || is.na(processed_at) || !nzchar(processed_at)) {
    stop("Reviewed Data_S1 must record one extraction timestamp.", call. = FALSE)
  }
  data.frame(
    records = nrow(data),
    extraction_version = expected_extraction_version,
    extraction_timestamp = processed_at,
    duplicate_image_records = sum(duplicated_hash),
    coordinate_recomputed = FALSE,
    white_balance_applied = FALSE,
    stringsAsFactors = FALSE
  )
}

add_reanalysis_transforms <- function(data, config) {
  population_source <- as.character(config$population_density_source)
  if (!(population_source %in% names(data))) {
    stop("Population-density predictor is missing: ", population_source, call. = FALSE)
  }
  population <- as.numeric(data[[population_source]])
  if (any(is.finite(population) & population < 0)) {
    stop("Population density cannot be negative.", call. = FALSE)
  }
  data$log_worldpop_2020_density <- ifelse(
    is.finite(population),
    log1p(population),
    NA_real_
  )
  data
}

finite_complete_rows <- function(data, columns) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop("Model columns are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  numeric <- vapply(data[columns], is.numeric, logical(1))
  if (any(!numeric)) {
    stop("Model columns must be numeric: ", paste(columns[!numeric], collapse = ", "), call. = FALSE)
  }
  matrix <- as.matrix(data[columns])
  stats::complete.cases(matrix) & apply(matrix, 1L, function(row) all(is.finite(row)))
}

standardize_training_matrix <- function(train, test = NULL) {
  center <- colMeans(train)
  scale <- apply(train, 2L, stats::sd)
  invalid <- !is.finite(scale) | scale <= 0
  if (any(invalid)) {
    stop("Zero-variance predictors: ", paste(colnames(train)[invalid], collapse = ", "), call. = FALSE)
  }
  train_scaled <- sweep(sweep(train, 2L, center, "-"), 2L, scale, "/")
  test_scaled <- if (is.null(test)) NULL else {
    sweep(sweep(test, 2L, center, "-"), 2L, scale, "/")
  }
  list(train = train_scaled, test = test_scaled, center = center, scale = scale)
}

variance_inflation_factors <- function(x) {
  x <- as.data.frame(x, check.names = FALSE)
  if (ncol(x) == 1L) return(setNames(1, names(x)))
  result <- vapply(seq_along(x), function(index) {
    response <- x[[index]]
    others <- x[-index]
    r_squared <- summary(stats::lm(response ~ ., data = others))$r.squared
    if (!is.finite(r_squared) || r_squared >= 1) Inf else 1 / (1 - r_squared)
  }, numeric(1), USE.NAMES = FALSE)
  names(result) <- names(x)
  result
}

fit_standardized_association <- function(data, outcome, predictors) {
  columns <- c(outcome, predictors)
  complete <- finite_complete_rows(data, columns)
  model_data <- data[complete, columns, drop = FALSE]
  if (nrow(model_data) <= length(predictors) + 2L) {
    stop("Too few complete rows for association model.", call. = FALSE)
  }
  x <- as.matrix(model_data[predictors])
  x_scaled <- standardize_training_matrix(x)$train
  y <- model_data[[outcome]]
  y_center <- mean(y)
  y_scale <- stats::sd(y)
  if (!is.finite(y_scale) || y_scale <= 0) stop("Outcome has zero variance.", call. = FALSE)
  frame <- data.frame(y = (y - y_center) / y_scale, x_scaled, check.names = FALSE)
  fit <- stats::lm(y ~ ., data = frame)
  model_matrix <- stats::model.matrix(fit)
  if (fit$rank < ncol(model_matrix)) {
    stop("Association model has aliased predictors; revise the predictor set.", call. = FALSE)
  }
  summary_fit <- summary(fit)
  coefficient_matrix <- summary_fit$coefficients
  terms <- setdiff(rownames(coefficient_matrix), "(Intercept)")
  vif <- variance_inflation_factors(x_scaled)
  coefficients <- data.frame(
    term = terms,
    standardized_estimate = coefficient_matrix[terms, "Estimate"],
    naive_standard_error = coefficient_matrix[terms, "Std. Error"],
    naive_t_value = coefficient_matrix[terms, "t value"],
    naive_p_value = coefficient_matrix[terms, "Pr(>|t|)"],
    vif = unname(vif[terms]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  performance <- data.frame(
    records_total = nrow(data),
    records_complete = sum(complete),
    records_missing = sum(!complete),
    r_squared_in_sample = summary_fit$r.squared,
    adjusted_r_squared_in_sample = summary_fit$adj.r.squared,
    residual_sigma_standardized = summary_fit$sigma,
    model_matrix_condition_number = kappa(model_matrix),
    stringsAsFactors = FALSE
  )
  list(
    fit = fit,
    complete = complete,
    coefficients = coefficients,
    performance = performance
  )
}

coordinate_key <- function(longitude, latitude) {
  paste(sprintf("%.10f", longitude), sprintf("%.10f", latitude), sep = "|")
}

make_spatial_block_folds <- function(data, folds = 5L,
                                     id_col = "analysis_id") {
  required <- c(id_col, "longitude", "latitude")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Spatial-fold columns are missing.", call. = FALSE)
  if (anyDuplicated(data[[id_col]])) stop("Spatial fold IDs must be unique.", call. = FALSE)
  coordinates <- as.matrix(data[c("longitude", "latitude")])
  if (any(!is.finite(coordinates))) stop("Spatial folds require finite coordinates.", call. = FALSE)
  keys <- coordinate_key(coordinates[, 1L], coordinates[, 2L])
  unique_index <- !duplicated(keys)
  unique_coordinates <- coordinates[unique_index, , drop = FALSE]
  unique_keys <- keys[unique_index]
  stable <- order(unique_keys)
  unique_coordinates <- unique_coordinates[stable, , drop = FALSE]
  unique_keys <- unique_keys[stable]
  configured_folds <- suppressWarnings(as.integer(folds))
  if (!is.numeric(folds) || is.logical(folds) || length(configured_folds) != 1L ||
      is.na(configured_folds) || configured_folds < 2L ||
      !identical(as.numeric(configured_folds), as.numeric(folds))) {
    stop("folds must be a single integer of at least two.", call. = FALSE)
  }
  if (nrow(unique_coordinates) < configured_folds) {
    stop(
      "Too few distinct coordinates for the configured spatial folds: ",
      nrow(unique_coordinates), " coordinates for ", configured_folds,
      " folds.",
      call. = FALSE
    )
  }
  folds <- configured_folds
  mean_latitude <- mean(unique_coordinates[, 2L]) * pi / 180
  coordinates_km <- cbind(
    x = unique_coordinates[, 1L] * 111.320 * cos(mean_latitude),
    y = unique_coordinates[, 2L] * 110.574
  )
  centered <- scale(coordinates_km, center = TRUE, scale = FALSE)
  axis <- stats::prcomp(centered, center = FALSE, scale. = FALSE)$x[, 1L]
  # Stabilize the arbitrary PCA sign so fold 1 is always the south-west end.
  if (stats::cor(axis, coordinates_km[, 1L] + coordinates_km[, 2L]) < 0) {
    axis <- -axis
  }
  along_axis <- order(axis, unique_coordinates[, 1L], unique_coordinates[, 2L])
  fold_sizes <- rep(nrow(unique_coordinates) %/% folds, folds)
  fold_sizes[seq_len(nrow(unique_coordinates) %% folds)] <-
    fold_sizes[seq_len(nrow(unique_coordinates) %% folds)] + 1L
  unique_fold <- integer(nrow(unique_coordinates))
  unique_fold[along_axis] <- rep(seq_len(folds), times = fold_sizes)
  fold_lookup <- setNames(unique_fold, unique_keys)
  result <- data.frame(
    analysis_id = as.character(data[[id_col]]),
    spatial_fold = unname(fold_lookup[keys]),
    longitude = coordinates[, 1L],
    latitude = coordinates[, 2L],
    stringsAsFactors = FALSE
  )
  names(result)[1L] <- id_col
  result
}

assign_spatial_folds <- function(data, coordinate_fold_map,
                                 id_col = "analysis_id") {
  required_data <- c(id_col, "longitude", "latitude")
  required_map <- c("longitude", "latitude", "spatial_fold")
  if (length(setdiff(required_data, names(data))) ||
      length(setdiff(required_map, names(coordinate_fold_map)))) {
    stop("Spatial-fold assignment columns are missing.", call. = FALSE)
  }
  map_keys <- coordinate_key(
    coordinate_fold_map$longitude,
    coordinate_fold_map$latitude
  )
  if (anyDuplicated(map_keys)) {
    duplicated_keys <- unique(map_keys[duplicated(map_keys)])
    for (key in duplicated_keys) {
      if (length(unique(coordinate_fold_map$spatial_fold[map_keys == key])) != 1L) {
        stop("One coordinate maps to multiple spatial folds.", call. = FALSE)
      }
    }
    coordinate_fold_map <- coordinate_fold_map[!duplicated(map_keys), , drop = FALSE]
    map_keys <- map_keys[!duplicated(map_keys)]
  }
  keys <- coordinate_key(data$longitude, data$latitude)
  index <- match(keys, map_keys)
  if (anyNA(index)) stop("Spatial fold map does not cover every coordinate.", call. = FALSE)
  data.frame(
    analysis_id = as.character(data[[id_col]]),
    spatial_fold = coordinate_fold_map$spatial_fold[index],
    longitude = data$longitude,
    latitude = data$latitude,
    stringsAsFactors = FALSE
  )
}

blocked_cv_association <- function(data, outcome, predictors,
                                   fold_col = "spatial_fold",
                                   id_col = "analysis_id",
                                   configured_folds = NULL) {
  required <- c(id_col, fold_col, outcome, predictors)
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Blocked-CV columns are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  if (length(outcome) != 1L || !is.character(outcome) || anyNA(outcome) ||
      !nzchar(outcome) ||
      !length(predictors) || !is.character(predictors) ||
      anyNA(predictors) || any(!nzchar(predictors)) ||
      anyDuplicated(predictors) || outcome %in% predictors) {
    stop(
      "Blocked CV requires one outcome and unique, non-overlapping predictors.",
      call. = FALSE
    )
  }
  if (anyDuplicated(data[[id_col]])) {
    stop("Blocked-CV analysis IDs must be unique.", call. = FALSE)
  }
  if (anyNA(data[[id_col]]) || any(!nzchar(as.character(data[[id_col]])))) {
    stop("Blocked-CV analysis IDs must be non-missing and non-empty.", call. = FALSE)
  }
  raw_folds <- data[[fold_col]]
  raw_folds_integer <- suppressWarnings(as.integer(raw_folds))
  if (!is.numeric(raw_folds) || is.logical(raw_folds) ||
      any(!is.finite(raw_folds)) || anyNA(raw_folds_integer) ||
      any(raw_folds < 1 | raw_folds != raw_folds_integer)) {
    stop("Spatial folds must be finite positive integers.", call. = FALSE)
  }
  input_folds <- sort(unique(as.integer(raw_folds)))
  if (is.null(configured_folds)) {
    configured_folds <- input_folds
  } else {
    configured_numeric <- suppressWarnings(as.integer(configured_folds))
    if (!is.numeric(configured_folds) || !length(configured_folds) ||
        anyNA(configured_numeric) ||
        any(configured_numeric < 1L) || anyDuplicated(configured_numeric) ||
        !identical(as.numeric(configured_numeric), as.numeric(configured_folds))) {
      stop("configured_folds must contain unique positive integers.", call. = FALSE)
    }
    configured_folds <- sort(configured_numeric)
  }
  if (length(configured_folds) < 2L) {
    stop("Blocked CV requires at least two configured folds.", call. = FALSE)
  }
  if (!identical(input_folds, configured_folds)) {
    stop(
      "Input data do not contain exactly the configured spatial-fold set.",
      call. = FALSE
    )
  }

  complete <- finite_complete_rows(data, c(outcome, predictors))
  model_data <- data[complete, required, drop = FALSE]
  fold_values <- sort(unique(as.integer(model_data[[fold_col]])))
  if (!identical(fold_values, configured_folds)) {
    absent <- setdiff(configured_folds, fold_values)
    stop(
      "Complete-case filtering removed configured spatial fold(s): ",
      paste(absent, collapse = ", "),
      call. = FALSE
    )
  }
  fold_counts <- table(factor(
    as.integer(model_data[[fold_col]]),
    levels = configured_folds
  ))
  test_minimum <- 2L
  train_minimum <- length(predictors) + 3L
  test_too_small <- configured_folds[fold_counts < test_minimum]
  train_counts <- nrow(model_data) - as.integer(fold_counts)
  train_too_small <- configured_folds[train_counts < train_minimum]
  if (length(test_too_small)) {
    stop(
      "Spatial test fold(s) have fewer than two complete records: ",
      paste(test_too_small, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(train_too_small)) {
    stop(
      "Spatial training fold(s) have fewer than ", train_minimum,
      " complete records required for ", length(predictors),
      " predictor(s): ", paste(train_too_small, collapse = ", "),
      call. = FALSE
    )
  }

  predictions <- vector("list", length(configured_folds))
  for (index in seq_along(fold_values)) {
    fold <- fold_values[[index]]
    test_index <- model_data[[fold_col]] == fold
    train_index <- !test_index
    x_train <- as.matrix(model_data[train_index, predictors, drop = FALSE])
    x_test <- as.matrix(model_data[test_index, predictors, drop = FALSE])
    scaled <- standardize_training_matrix(x_train, x_test)
    train_frame <- data.frame(
      y = model_data[[outcome]][train_index],
      scaled$train,
      check.names = FALSE
    )
    test_frame <- as.data.frame(scaled$test, check.names = FALSE)
    fit <- stats::lm(y ~ ., data = train_frame)
    if (fit$rank < ncol(stats::model.matrix(fit))) {
      stop("A spatial training fold has aliased predictors.", call. = FALSE)
    }
    training_mean <- mean(train_frame$y)
    predictions[[index]] <- data.frame(
      analysis_id = as.character(model_data[[id_col]][test_index]),
      spatial_fold = fold,
      observed = model_data[[outcome]][test_index],
      predicted = as.numeric(stats::predict(fit, newdata = test_frame)),
      training_mean = training_mean,
      train_records = sum(train_index),
      test_records = sum(test_index),
      stringsAsFactors = FALSE
    )
    names(predictions[[index]])[1L] <- id_col
  }
  predictions <- do.call(rbind, predictions)
  residual <- predictions$observed - predictions$predicted
  global_denominator <- sum((predictions$observed - mean(predictions$observed))^2)
  training_mean_denominator <- sum(
    (predictions$observed - predictions$training_mean)^2
  )
  fold_performance <- do.call(rbind, lapply(
    split(predictions, predictions$spatial_fold),
    function(fold_data) {
      fold_residual <- fold_data$observed - fold_data$predicted
      fold_denominator <- sum(
        (fold_data$observed - fold_data$training_mean)^2
      )
      data.frame(
        spatial_fold = unique(fold_data$spatial_fold),
        train_records = unique(fold_data$train_records),
        test_records = nrow(fold_data),
        rmse = sqrt(mean(fold_residual^2)),
        mae = mean(abs(fold_residual)),
        q_squared = if (fold_denominator > 0) {
          1 - sum(fold_residual^2) / fold_denominator
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
    }
  ))
  performance <- data.frame(
    records_complete = nrow(predictions),
    folds = length(fold_values),
    folds_configured = length(configured_folds),
    folds_actual = length(unique(predictions$spatial_fold)),
    rmse = sqrt(mean(residual^2)),
    mae = mean(abs(residual)),
    q_squared_cv = if (training_mean_denominator > 0) {
      1 - sum(residual^2) / training_mean_denominator
    } else {
      NA_real_
    },
    r_squared_cv_global_mean = if (global_denominator > 0) {
      1 - sum(residual^2) / global_denominator
    } else {
      NA_real_
    },
    macro_rmse = mean(fold_performance$rmse),
    macro_mae = mean(fold_performance$mae),
    macro_q_squared = mean(fold_performance$q_squared, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  list(
    predictions = predictions,
    performance = performance,
    fold_performance = fold_performance
  )
}

summarize_predictor_missingness <- function(data, predictors, outcomes) {
  rows <- lapply(predictors, function(predictor) {
    missing <- !is.finite(data[[predictor]])
    base <- data.frame(
      predictor = predictor,
      records = nrow(data),
      observed_n = sum(!missing),
      missing_n = sum(missing),
      missing_fraction = mean(missing),
      stringsAsFactors = FALSE
    )
    for (outcome in outcomes) {
      observed <- data[[outcome]][!missing]
      missing_values <- data[[outcome]][missing]
      base[[paste0(outcome, "_mean_observed")]] <- if (any(is.finite(observed))) {
        mean(observed[is.finite(observed)])
      } else {
        NA_real_
      }
      base[[paste0(outcome, "_mean_missing")]] <- if (any(is.finite(missing_values))) {
        mean(missing_values[is.finite(missing_values)])
      } else {
        NA_real_
      }
    }
    base
  })
  do.call(rbind, rows)
}

pairwise_colour_method_summary <- function(primary, candidate, method) {
  common <- intersect(primary$analysis_id, candidate$analysis_id)
  primary <- primary[match(common, primary$analysis_id), , drop = FALSE]
  candidate <- candidate[match(common, candidate$analysis_id), , drop = FALSE]
  delta_l <- candidate$L - primary$L
  delta_a <- candidate$a - primary$a
  delta_b <- candidate$b - primary$b
  delta_e <- sqrt(delta_l^2 + delta_a^2 + delta_b^2)
  data.frame(
    method = method,
    common_records = length(common),
    delta_L_mean = mean(delta_l),
    delta_L_median = stats::median(delta_l),
    delta_a_mean = mean(delta_a),
    delta_a_median = stats::median(delta_a),
    delta_b_mean = mean(delta_b),
    delta_b_median = stats::median(delta_b),
    deltaE76_median = stats::median(delta_e),
    deltaE76_p95 = unname(stats::quantile(delta_e, 0.95)),
    deltaE76_max = max(delta_e),
    a_pearson = stats::cor(primary$a, candidate$a, method = "pearson"),
    a_spearman = stats::cor(primary$a, candidate$a, method = "spearman"),
    primary_reference_PC1_pearson = stats::cor(
      primary$primary_reference_PC1,
      candidate$primary_reference_PC1,
      method = "pearson"
    ),
    stringsAsFactors = FALSE
  )
}
