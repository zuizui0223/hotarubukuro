v16_analysis_spec_version <- "v16.5_centered_observation_year"

v16_environment_terms <- function(radius_km = 50) {
  c(
    paste0("broad", radius_km, "km_pc1"),
    paste0("broad", radius_km, "km_pc2"),
    paste0("within", radius_km, "km_pc1"),
    paste0("within", radius_km, "km_pc2")
  )
}

v16_fingerprint_terms <- function() {
  c(
    "bombus_total_habitat_support",
    "bombus_composition_pc1",
    "bombus_composition_pc2"
  )
}

v16_assert_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

v16_rank01 <- function(x, decreasing = FALSE) {
  out <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (!any(keep)) return(out)
  value <- if (decreasing) -x[keep] else x[keep]
  if (sum(keep) == 1L) {
    out[keep] <- 1
  } else {
    out[keep] <- (rank(value, ties.method = "average") - 1) / (sum(keep) - 1)
  }
  out
}

v16_cell_id <- function(x_km, y_km) {
  paste0("cell-1km-", floor(as.numeric(x_km)), "_", floor(as.numeric(y_km)))
}

v16_data_quality <- function(observations, cells) {
  observation_required <- c(
    "x_km", "y_km", "pigmented_mixture50", "pigment_intensity_z"
  )
  cell_required <- c(
    "exact_site_id", "n_observations", "n_pigmented", "n_white",
    "pigment_share", "x_km", "y_km", "spatial_fold",
    v16_environment_terms(), v16_fingerprint_terms(),
    "bombus_fingerprint_common_support", "conditional_intensity_median",
    "median_DOY", "median_year", "log_population_sum_25km"
  )
  v16_assert_columns(observations, observation_required, "observations")
  v16_assert_columns(cells, cell_required, "cells")
  raw_cell <- v16_cell_id(observations$x_km, observations$y_km)
  raw_pigmented <- as.integer(observations$pigmented_mixture50)
  valid_counts <- cells$n_pigmented >= 0 & cells$n_white >= 0 &
    cells$n_pigmented + cells$n_white == cells$n_observations
  raw_by_cell <- table(raw_cell)
  raw_pigmented_by_cell <- tapply(raw_pigmented, raw_cell, sum, na.rm = TRUE)
  cell_index <- match(cells$exact_site_id, names(raw_by_cell))
  aggregation_match <- !is.na(cell_index) &
    cells$n_observations == as.integer(raw_by_cell[cell_index]) &
    cells$n_pigmented == as.integer(raw_pigmented_by_cell[cells$exact_site_id])
  env <- v16_environment_terms()
  fp <- v16_fingerprint_terms()
  data.frame(
    metric = c(
      "n_raw_observations", "n_cells_1km", "n_unique_cell_ids",
      "n_duplicate_cell_ids", "sum_cell_observations", "n_raw_pigmented",
      "sum_cell_pigmented", "n_invalid_cell_counts",
      "n_cell_aggregation_mismatches", "n_cells_missing_environment",
      "n_common_five_species_support", "n_common_support_missing_fingerprint",
      "n_cells_with_conditional_intensity", "n_white_cells_with_intensity",
      "n_mixed_cells", "n_cells_missing_DOY", "n_cells_missing_population",
      "n_spatial_folds"
    ),
    value = c(
      nrow(observations), nrow(cells), length(unique(cells$exact_site_id)),
      sum(duplicated(cells$exact_site_id)), sum(cells$n_observations),
      sum(raw_pigmented == 1L, na.rm = TRUE), sum(cells$n_pigmented),
      sum(!valid_counts, na.rm = TRUE),
      sum(!aggregation_match, na.rm = TRUE) + sum(is.na(aggregation_match)),
      sum(!stats::complete.cases(cells[env])),
      sum(cells$bombus_fingerprint_common_support, na.rm = TRUE),
      sum(cells$bombus_fingerprint_common_support &
            !stats::complete.cases(cells[fp])),
      sum(is.finite(cells$conditional_intensity_median)),
      sum(cells$n_pigmented == 0 & is.finite(cells$conditional_intensity_median)),
      sum(cells$n_pigmented > 0 & cells$n_white > 0),
      sum(!is.finite(cells$median_DOY)),
      sum(!is.finite(cells$log_population_sum_25km)),
      length(unique(cells$spatial_fold))
    ),
    stringsAsFactors = FALSE
  )
}

v16_safe_scale <- function(train, test) {
  centre <- colMeans(train)
  spread <- apply(train, 2, stats::sd)
  spread[!is.finite(spread) | spread <= 1e-10] <- 1
  list(
    train = sweep(sweep(train, 2, centre, "-"), 2, spread, "/"),
    test = sweep(sweep(test, 2, centre, "-"), 2, spread, "/"),
    centre = centre, spread = spread
  )
}

v16_fold_predictors <- function(train, test, environment_terms,
                                fingerprint_terms = character()) {
  v16_assert_columns(train, c(environment_terms, fingerprint_terms), "train")
  v16_assert_columns(test, c(environment_terms, fingerprint_terms), "test")
  raw_train <- as.data.frame(train[environment_terms])
  raw_test <- as.data.frame(test[environment_terms])
  names(raw_train) <- paste0("environment_", seq_along(environment_terms))
  names(raw_test) <- names(raw_train)
  audit_rows <- list()
  if (length(fingerprint_terms)) {
    for (index in seq_along(fingerprint_terms)) {
      response <- fingerprint_terms[index]
      formula <- stats::reformulate(names(raw_train), response = ".fingerprint")
      fit_data <- cbind(.fingerprint = train[[response]], raw_train)
      fit <- stats::lm(formula, data = fit_data)
      train_residual <- as.numeric(stats::residuals(fit))
      test_data <- cbind(.fingerprint = test[[response]], raw_test)
      test_residual <- test_data$.fingerprint -
        as.numeric(stats::predict(fit, newdata = test_data))
      name <- paste0("fingerprint_orthogonal_", index)
      raw_train[[name]] <- train_residual
      raw_test[[name]] <- test_residual
      audit_rows[[length(audit_rows) + 1L]] <- data.frame(
        source_term = response,
        training_R2_against_environment = summary(fit)$r.squared,
        stringsAsFactors = FALSE
      )
    }
  }
  scaled <- v16_safe_scale(as.matrix(raw_train), as.matrix(raw_test))
  colnames(scaled$train) <- paste0("z", seq_len(ncol(scaled$train)))
  colnames(scaled$test) <- colnames(scaled$train)
  vif <- vapply(seq_len(ncol(scaled$train)), function(index) {
    if (ncol(scaled$train) == 1L) return(1)
    fit <- stats::lm(
      scaled$train[, index] ~ .,
      data = as.data.frame(scaled$train[, -index, drop = FALSE])
    )
    1 / max(1e-12, 1 - summary(fit)$r.squared)
  }, numeric(1))
  list(
    train = as.data.frame(scaled$train),
    test = as.data.frame(scaled$test),
    terms = colnames(scaled$train),
    audit = if (length(audit_rows)) do.call(rbind, audit_rows) else data.frame(),
    maximum_VIF = max(vif),
    maximum_absolute_correlation = if (ncol(scaled$train) > 1L) {
      correlation <- stats::cor(scaled$train)
      diag(correlation) <- NA_real_
      max(abs(correlation), na.rm = TRUE)
    } else 0
  )
}

v16_make_mesh <- function(data) {
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("Package 'INLA' is required.", call. = FALSE)
  }
  coordinates <- as.matrix(data[c("x_km", "y_km")])
  mesh <- INLA::inla.mesh.2d(
    loc = coordinates, max.edge = c(20, 100), cutoff = 5
  )
  list(mesh = mesh)
}

v16_make_spde <- function(mesh, training_projection) {
  constraint <- matrix(
    Matrix::colSums(training_projection), nrow = 1L
  )
  constraint <- constraint / sum(constraint)
  INLA::inla.spde2.pcmatern(
    mesh, alpha = 2, constr = FALSE,
    extraconstr = list(A = constraint, e = matrix(0, 1L, 1L)),
    prior.range = c(100, 0.05),
    prior.sigma = c(1, 0.05)
  )
}

v16_observation_precision <- function(sample) {
  values <- sample$hyperpar
  index <- grep("Precision for the Gaussian observations", names(values),
                ignore.case = TRUE)
  if (!length(index)) {
    index <- grep("precision.*Gaussian", names(values), ignore.case = TRUE)
  }
  if (!length(index)) stop("Gaussian observation precision not found.", call. = FALSE)
  as.numeric(values[index[1]])
}

v16_named_section_order <- function(row_names, section, requested_index) {
  if (is.null(row_names)) {
    stop("INLA result has no usable row names.", call. = FALSE)
  }
  section_rows <- grepl(paste0("^", section), row_names)
  usable <- section_rows & grepl("[0-9]+$", row_names)
  sampled_index <- rep(NA_integer_, length(row_names))
  sampled_index[usable] <- as.integer(sub(".*[^0-9]", "", row_names[usable]))
  for (offset in c(0L, -1L, 1L)) {
    order <- match(as.integer(requested_index) + offset, sampled_index)
    if (!anyNA(order)) return(order)
  }
  stop(
    "Could not align INLA ", section,
    " row names to requested stack indices.",
    call. = FALSE
  )
}

v16_predictor_row_order <- function(sample, requested_index,
                                    section = "APredictor") {
  if (is.null(rownames(sample$latent)) ||
      length(rownames(sample$latent)) != nrow(sample$latent)) {
    stop("Posterior samples have no usable row names.", call. = FALSE)
  }
  v16_named_section_order(rownames(sample$latent), section, requested_index)
}

v16_fit_fold <- function(train, test, response, predictor_basis, mesh,
                         family = c("binomial", "gaussian"),
                         trials = NULL, n_draws = 1000L, seed = 20260725L,
                         model = "model", fold = NA_integer_,
                         inla_verbose = TRUE) {
  family <- match.arg(family)
  train_X <- cbind(Intercept = 1, predictor_basis$train)
  test_X <- cbind(Intercept = 1, predictor_basis$test)
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
  stack_estimation <- INLA::inla.stack(
    data = estimation_data,
    A = list(A_train, 1),
    effects = list(spatial = seq_len(spde$n.spde), X = train_X),
    tag = "est", compress = FALSE, remove.unused = FALSE
  )
  stack_prediction <- INLA::inla.stack(
    data = prediction_data,
    A = list(A_test, 1),
    effects = list(spatial = seq_len(spde$n.spde), X = test_X),
    tag = "pred", compress = FALSE, remove.unused = FALSE
  )
  stack <- INLA::inla.stack(stack_estimation, stack_prediction)
  stack_data <- INLA::inla.stack.data(stack)
  fixed_terms <- colnames(train_X)
  formula <- stats::as.formula(
    paste(
      "y ~ -1 +", paste(fixed_terms, collapse = " + "),
      "+ f(spatial, model = spde)"
    ),
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
    verbose = isTRUE(inla_verbose)
  )
  if (family == "binomial") arguments$Ntrials <- stack_data$Ntrials
  fit_time <- system.time({
    fit <- do.call(INLA::inla, arguments)
  })[["elapsed"]]
  prediction_index <- INLA::inla.stack.index(stack, "pred")$data
  summary_order <- v16_named_section_order(
    rownames(fit$summary.linear.predictor), "APredictor", prediction_index
  )
  inla_fitted_mean <- as.numeric(
    fit$summary.fitted.values[summary_order, "mean"]
  )
  inla_linear_mean <- as.numeric(
    fit$summary.linear.predictor[summary_order, "mean"]
  )
  sample_time <- system.time({
    samples <- INLA::inla.posterior.sample(
      n = as.integer(n_draws), result = fit,
      selection = list(APredictor = prediction_index),
      seed = as.integer(seed), num.threads = 1,
      parallel.configs = FALSE, add.names = TRUE
    )
  })[["elapsed"]]
  predictor_order <- v16_predictor_row_order(
    samples[[1]], prediction_index, section = "APredictor"
  )
  eta <- vapply(
    samples, function(sample) as.numeric(sample$latent[predictor_order, 1]),
    numeric(nrow(test))
  )
  if (!is.matrix(eta)) eta <- matrix(eta, nrow = nrow(test))
  set.seed(as.integer(seed) + 1L)
  if (family == "binomial") {
    probability <- stats::plogis(eta)
    draws <- vapply(seq_len(ncol(probability)), function(index) {
      stats::rbinom(
        nrow(test), size = as.integer(test[[trials]]),
        prob = probability[, index]
      )
    }, integer(nrow(test)))
    latent_mean <- rowMeans(probability)
  } else {
    precision <- vapply(samples, v16_observation_precision, numeric(1))
    draws <- vapply(seq_len(ncol(eta)), function(index) {
      stats::rnorm(
        nrow(test), mean = eta[, index], sd = sqrt(1 / precision[index])
      )
    }, numeric(nrow(test)))
    latent_mean <- rowMeans(eta)
  }
  if (!is.matrix(draws)) draws <- matrix(draws, nrow = nrow(test))
  list(
    draws = draws,
    latent_mean = latent_mean,
    log = data.frame(
      model = model, heldout_spatial_fold = fold,
      family = family, n_train = nrow(train), n_test = nrow(test),
      mesh_vertices = mesh$n, n_draws = n_draws,
      fit_elapsed_seconds = fit_time,
      sample_elapsed_seconds = sample_time,
      training_response_mean = if (family == "binomial") {
        sum(train[[response]]) / sum(train[[trials]])
      } else mean(train[[response]]),
      heldout_predictive_mean = if (family == "binomial") {
        sum(latent_mean * test[[trials]]) / sum(test[[trials]])
      } else mean(latent_mean),
      heldout_INLA_fitted_mean = if (family == "binomial") {
        sum(inla_fitted_mean * test[[trials]]) / sum(test[[trials]])
      } else mean(inla_fitted_mean),
      posterior_vs_INLA_fitted_correlation = stats::cor(
        latent_mean, inla_fitted_mean
      ),
      posterior_eta_vs_INLA_linear_correlation = stats::cor(
        rowMeans(eta), inla_linear_mean
      ),
      spde_training_location_mean_zero_constraint = TRUE,
      maximum_predictor_VIF = predictor_basis$maximum_VIF,
      maximum_absolute_predictor_correlation =
        predictor_basis$maximum_absolute_correlation,
      formula = paste(deparse(formula), collapse = " "),
      stringsAsFactors = FALSE
    ),
    inla_fitted_mean = inla_fitted_mean,
    inla_linear_mean = inla_linear_mean,
    posterior_eta_mean = rowMeans(eta),
    posterior_predictor_names = rownames(samples[[1]]$latent),
    requested_predictor_index = prediction_index,
    posterior_predictor_order = predictor_order,
    orthogonalization = predictor_basis$audit
  )
}

v16_crossfit_spde <- function(data, response, family,
                              environment_terms = v16_environment_terms(),
                              fingerprint_terms = character(), trials = NULL,
                              training_eligible = NULL,
                              model = "model", n_draws = 1000L,
                              seed = 20260725L) {
  required <- unique(c(
    "exact_site_id", "x_km", "y_km", "spatial_fold", response,
    environment_terms, fingerprint_terms, trials
  ))
  v16_assert_columns(data, required, model)
  if (is.null(training_eligible)) training_eligible <- is.finite(data[[response]])
  complete_predictors <- stats::complete.cases(
    data[c("x_km", "y_km", "spatial_fold", environment_terms,
           fingerprint_terms, trials)]
  )
  if (!all(complete_predictors)) {
    stop(model, " has incomplete prediction rows.", call. = FALSE)
  }
  training_eligible <- training_eligible & is.finite(data[[response]])
  mesh_objects <- v16_make_mesh(data)
  draws <- matrix(NA_real_, nrow(data), as.integer(n_draws))
  latent_mean <- rep(NA_real_, nrow(data))
  logs <- list()
  orthogonalization <- list()
  folds <- sort(unique(as.integer(data$spatial_fold)))
  for (fold in folds) {
    test_index <- which(as.integer(data$spatial_fold) == fold)
    train_index <- which(as.integer(data$spatial_fold) != fold & training_eligible)
    train <- data[train_index, , drop = FALSE]
    test <- data[test_index, , drop = FALSE]
    basis <- v16_fold_predictors(
      train, test, environment_terms, fingerprint_terms
    )
    message(
      "[v16] ", model, " fold ", fold, ": train=", nrow(train),
      ", test=", nrow(test), ", draws=", n_draws
    )
    result <- v16_fit_fold(
      train, test, response, basis,
      mesh_objects$mesh,
      family = family, trials = trials, n_draws = n_draws,
      seed = as.integer(seed + 1000L * match(fold, folds)),
      model = model, fold = fold
    )
    draws[test_index, ] <- result$draws
    latent_mean[test_index] <- result$latent_mean
    logs[[length(logs) + 1L]] <- result$log
    if (nrow(result$orthogonalization)) {
      result$orthogonalization$model <- model
      result$orthogonalization$heldout_spatial_fold <- fold
      orthogonalization[[length(orthogonalization) + 1L]] <-
        result$orthogonalization
    }
    rm(result)
    invisible(gc())
  }
  if (any(!is.finite(draws))) {
    stop(model, " produced non-finite predictive draws.", call. = FALSE)
  }
  list(
    analysis_spec_version = v16_analysis_spec_version,
    model = model,
    cell_id = as.character(data$exact_site_id),
    observed = as.numeric(data[[response]]),
    trials = if (!is.null(trials)) as.integer(data[[trials]]) else NULL,
    draws = draws,
    latent_mean = latent_mean,
    log = do.call(rbind, logs),
    orthogonalization = if (length(orthogonalization)) {
      do.call(rbind, orthogonalization)
    } else data.frame()
  )
}

v16_auc <- function(labels, scores) {
  keep <- is.finite(labels) & is.finite(scores)
  labels <- as.integer(labels[keep])
  scores <- as.numeric(scores[keep])
  n_positive <- sum(labels == 1L)
  n_negative <- sum(labels == 0L)
  if (!n_positive || !n_negative) return(NA_real_)
  ranks <- rank(scores, ties.method = "average")
  (sum(ranks[labels == 1L]) - n_positive * (n_positive + 1) / 2) /
    (n_positive * n_negative)
}

v16_model_performance <- function(result, data, response, family,
                                  trials = NULL) {
  index <- match(result$cell_id, data$exact_site_id)
  observed <- data[[response]][index]
  draws <- result$draws
  lower <- apply(draws, 1, stats::quantile, probs = 0.025, names = FALSE)
  upper <- apply(draws, 1, stats::quantile, probs = 0.975, names = FALSE)
  if (family == "binomial") {
    n <- as.integer(data[[trials]][index])
    predicted_probability <- rowMeans(draws / n)
    calibration_x <- stats::qlogis(pmin(pmax(
      predicted_probability, 1e-6
    ), 1 - 1e-6))
    calibration <- suppressWarnings(stats::glm(
      cbind(observed, n - observed) ~ calibration_x,
      family = stats::binomial()
    ))
    calibration_coefficients <- stats::coef(calibration)
    labels <- unlist(lapply(seq_along(n), function(i) {
      c(rep.int(1L, observed[i]), rep.int(0L, n[i] - observed[i]))
    }), use.names = FALSE)
    scores <- unlist(lapply(seq_along(n), function(i) {
      rep.int(predicted_probability[i], n[i])
    }), use.names = FALSE)
    exact_mass <- (1 + rowSums(draws == observed)) / (ncol(draws) + 1)
    data.frame(
      model = result$model, family = family, n = length(n),
      n_observations = sum(n),
      primary_metric = "mean_negative_log_predictive_mass",
      primary_value = mean(-log(exact_mass)),
      RMSE = sqrt(stats::weighted.mean(
        (observed / n - predicted_probability)^2, n
      )),
      MAE = stats::weighted.mean(abs(observed / n - predicted_probability), n),
      AUC = v16_auc(labels, scores),
      cell_any_pigmented_AUC = v16_auc(
        as.integer(observed > 0), predicted_probability
      ),
      cell_majority_pigmented_AUC = v16_auc(
        as.integer(observed / n >= 0.5), predicted_probability
      ),
      coverage_95 = mean(observed >= lower & observed <= upper),
      observed_prevalence = sum(observed) / sum(n),
      predicted_prevalence = stats::weighted.mean(predicted_probability, n),
      calibration_intercept = unname(calibration_coefficients[1]),
      calibration_slope = unname(calibration_coefficients[2]),
      Bernoulli_Brier_score = stats::weighted.mean(
        (observed / n) * (1 - predicted_probability)^2 +
          (1 - observed / n) * predicted_probability^2,
        n
      ),
      stringsAsFactors = FALSE
    )
  } else {
    keep <- is.finite(observed)
    prediction <- result$latent_mean
    data.frame(
      model = result$model, family = family, n = sum(keep),
      n_observations = sum(keep), primary_metric = "RMSE",
      primary_value = sqrt(mean((observed[keep] - prediction[keep])^2)),
      RMSE = sqrt(mean((observed[keep] - prediction[keep])^2)),
      MAE = mean(abs(observed[keep] - prediction[keep])), AUC = NA_real_,
      cell_any_pigmented_AUC = NA_real_,
      cell_majority_pigmented_AUC = NA_real_,
      coverage_95 = mean(observed[keep] >= lower[keep] &
                           observed[keep] <= upper[keep]),
      observed_prevalence = NA_real_, predicted_prevalence = NA_real_,
      calibration_intercept = NA_real_, calibration_slope = NA_real_,
      Bernoulli_Brier_score = NA_real_,
      stringsAsFactors = FALSE
    )
  }
}

v16_presence_calibration <- function(result, data, trials = "n_observations") {
  index <- match(result$cell_id, data$exact_site_id)
  observed <- as.integer(data$n_pigmented[index])
  n <- as.integer(data[[trials]][index])
  probability <- rowMeans(result$draws / n)
  decile <- pmin(10L, ceiling(
    rank(probability, ties.method = "average") / length(probability) * 10
  ))
  rows <- lapply(sort(unique(decile)), function(group) {
    keep <- decile == group
    data.frame(
      model = result$model, predictive_decile = group,
      n_cells = sum(keep), n_observations = sum(n[keep]),
      observed_pigment_share = sum(observed[keep]) / sum(n[keep]),
      predicted_pigment_share = stats::weighted.mean(
        probability[keep], n[keep]
      ),
      minimum_prediction = min(probability[keep]),
      maximum_prediction = max(probability[keep]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

v16_fold_performance <- function(result, data, response, family,
                                 trials = NULL) {
  data_index <- match(result$cell_id, data$exact_site_id)
  fold <- as.integer(data$spatial_fold[data_index])
  rows <- lapply(sort(unique(fold)), function(heldout) {
    keep <- fold == heldout
    subset <- result
    subset$cell_id <- result$cell_id[keep]
    subset$observed <- result$observed[keep]
    subset$draws <- result$draws[keep, , drop = FALSE]
    subset$latent_mean <- result$latent_mean[keep]
    if (!is.null(result$trials)) subset$trials <- result$trials[keep]
    value <- v16_model_performance(
      subset, data[data_index[keep], , drop = FALSE],
      response, family, trials
    )
    value$heldout_spatial_fold <- heldout
    value
  })
  do.call(rbind, rows)
}

v16_presence_scores <- function(result, cells) {
  index <- match(result$cell_id, cells$exact_site_id)
  observed <- as.integer(cells$n_pigmented[index])
  trials <- as.integer(cells$n_observations[index])
  draws <- result$draws
  upper_q <- (1 + rowSums(draws >= observed)) / (ncol(draws) + 1)
  lower_q <- (1 + rowSums(draws <= observed)) / (ncol(draws) + 1)
  predicted_share <- rowMeans(draws / trials)
  predictive_sd <- apply(draws / trials, 1, stats::sd)
  predictive_sd[!is.finite(predictive_sd) | predictive_sd <= 1e-10] <- NA_real_
  upper_z <- (observed / trials - predicted_share) / predictive_sd
  lower_z <- (predicted_share - observed / trials) / predictive_sd
  upper_eligible <- observed > 0L
  lower_eligible <- observed < trials
  upper_order <- order(
    upper_q, -ifelse(is.finite(upper_z), upper_z, -Inf),
    as.character(result$cell_id)
  )
  lower_order <- order(
    lower_q, -ifelse(is.finite(lower_z), lower_z, -Inf),
    as.character(result$cell_id)
  )
  upper_position <- rep(NA_integer_, length(observed))
  upper_selected_order <- upper_order[upper_eligible[upper_order]]
  upper_position[upper_selected_order] <- seq_along(upper_selected_order)
  lower_position <- rep(NA_integer_, length(observed))
  lower_selected_order <- lower_order[lower_eligible[lower_order]]
  lower_position[lower_selected_order] <- seq_along(lower_selected_order)
  rank_score <- function(position, eligible) {
    out <- rep(NA_real_, length(position))
    n <- sum(eligible)
    if (n == 1L) out[eligible] <- 1
    if (n > 1L) out[eligible] <- 1 - (position[eligible] - 1) / (n - 1)
    out
  }
  data.frame(
    model = result$model,
    exact_site_id = result$cell_id,
    n_observations = trials,
    n_pigmented = observed,
    observed_pigment_share = observed / trials,
    predicted_pigment_share = predicted_share,
    predictive_share_sd = predictive_sd,
    unexpected_pigmented_q = upper_q,
    unexpected_white_q = lower_q,
    unexpected_pigmented_z = upper_z,
    unexpected_white_z = lower_z,
    unexpected_pigmented_rank = rank_score(upper_position, upper_eligible),
    unexpected_white_rank = rank_score(lower_position, lower_eligible),
    unexpected_pigmented_top05 = upper_position <= ceiling(0.05 * sum(upper_eligible)),
    unexpected_pigmented_top10 = upper_position <= ceiling(0.10 * sum(upper_eligible)),
    unexpected_pigmented_top20 = upper_position <= ceiling(0.20 * sum(upper_eligible)),
    unexpected_white_top05 = lower_position <= ceiling(0.05 * sum(lower_eligible)),
    unexpected_white_top10 = lower_position <= ceiling(0.10 * sum(lower_eligible)),
    unexpected_white_top20 = lower_position <= ceiling(0.20 * sum(lower_eligible)),
    stringsAsFactors = FALSE
  )
}

v16_simulation_tail_q <- function(draws, direction = c("pigmented", "white")) {
  direction <- match.arg(direction)
  ranked <- t(apply(draws, 1, function(value) {
    if (direction == "pigmented") {
      rank(-value, ties.method = "max") / length(value)
    } else {
      rank(value, ties.method = "max") / length(value)
    }
  }))
  if (!is.matrix(ranked)) ranked <- matrix(ranked, nrow = nrow(draws))
  ranked
}

v16_neighbour_structure <- function(cells, k = 5L, maximum_km = 50,
                                    isolation_km = 25,
                                    same_fold_only = TRUE) {
  coordinates <- as.matrix(cells[c("x_km", "y_km")])
  dx <- outer(coordinates[, 1], coordinates[, 1], "-")
  dy <- outer(coordinates[, 2], coordinates[, 2], "-")
  distance <- sqrt(dx^2 + dy^2)
  diag(distance) <- Inf
  neighbours <- vector("list", nrow(cells))
  weights <- vector("list", nrow(cells))
  for (index in seq_len(nrow(cells))) {
    eligible <- is.finite(distance[index, ]) &
      distance[index, ] <= maximum_km
    if (same_fold_only) {
      eligible <- eligible & cells$spatial_fold == cells$spatial_fold[index]
    }
    candidates <- which(eligible)
    if (length(candidates)) {
      candidates <- candidates[order(distance[index, candidates])]
      candidates <- head(candidates, as.integer(k))
      weight <- 1 / pmax(distance[index, candidates], 0.5)
      neighbours[[index]] <- candidates
      weights[[index]] <- weight / sum(weight)
    } else {
      neighbours[[index]] <- integer()
      weights[[index]] <- numeric()
    }
  }
  list(
    distance = distance,
    neighbours = neighbours,
    weights = weights,
    isolation_km = isolation_km,
    same_fold_only = same_fold_only
  )
}

v16_local_isolation <- function(share, neighbours) {
  if (is.vector(share)) share <- matrix(share, ncol = 1L)
  out <- matrix(NA_real_, nrow(share), ncol(share))
  for (index in seq_len(nrow(share))) {
    adjacent <- neighbours$neighbours[[index]]
    if (!length(adjacent)) next
    out[index, ] <- share[index, ] - as.numeric(
      neighbours$weights[[index]] %*% share[adjacent, , drop = FALSE]
    )
  }
  out
}

v16_top_indices <- function(q, z, eligible, fraction, ids) {
  candidates <- which(eligible & is.finite(q))
  if (!length(candidates)) return(integer())
  z_order <- ifelse(is.finite(z[candidates]), z[candidates], -Inf)
  candidates <- candidates[order(q[candidates], -z_order, ids[candidates])]
  head(candidates, max(1L, ceiling(fraction * length(candidates))))
}

v16_isolated_fraction <- function(selected, neighbours) {
  if (!length(selected)) return(NA_real_)
  if (length(selected) == 1L) return(1)
  pair_distance <- neighbours$distance[selected, selected, drop = FALSE]
  diag(pair_distance) <- Inf
  mean(rowSums(pair_distance <= neighbours$isolation_km) == 0L)
}

v16_null_comparison <- function(observed, simulated, alternative = "greater") {
  simulated <- simulated[is.finite(simulated)]
  if (!is.finite(observed) || !length(simulated)) {
    return(c(
      null_mean = NA_real_, null_sd = NA_real_, lower_95 = NA_real_,
      upper_95 = NA_real_, empirical_p = NA_real_,
      empirical_two_sided_p = NA_real_, percentile = NA_real_,
      monte_carlo_se = NA_real_
    ))
  }
  p_upper <- (1 + sum(simulated >= observed)) / (length(simulated) + 1)
  p_lower <- (1 + sum(simulated <= observed)) / (length(simulated) + 1)
  p <- if (alternative == "less") p_lower else p_upper
  c(
    null_mean = mean(simulated), null_sd = stats::sd(simulated),
    lower_95 = unname(stats::quantile(simulated, 0.025)),
    upper_95 = unname(stats::quantile(simulated, 0.975)),
    empirical_p = p,
    empirical_two_sided_p = min(1, 2 * min(p_upper, p_lower)),
    percentile = mean(simulated <= observed),
    monte_carlo_se = sqrt(p * (1 - p) / (length(simulated) + 1))
  )
}

v16_candidate_null <- function(presence_result, cells,
                               intensity_result = NULL,
                               phenology_result = NULL,
                               tiers = c(0.05, 0.10, 0.20),
                               maximum_neighbour_km = 50,
                               isolation_km = 25) {
  index <- match(presence_result$cell_id, cells$exact_site_id)
  d <- cells[index, , drop = FALSE]
  counts <- presence_result$draws
  trials <- as.integer(d$n_observations)
  shares <- sweep(counts, 1, trials, "/")
  observed_share <- d$n_pigmented / trials
  mean_share <- rowMeans(shares)
  sd_share <- apply(shares, 1, stats::sd)
  sd_share[!is.finite(sd_share) | sd_share <= 1e-10] <- NA_real_
  scores <- v16_presence_scores(presence_result, cells)
  upper_q_sim <- v16_simulation_tail_q(counts, "pigmented")
  lower_q_sim <- v16_simulation_tail_q(counts, "white")
  upper_z_sim <- sweep(shares, 1, mean_share, "-")
  lower_z_sim <- -upper_z_sim
  upper_z_sim <- sweep(upper_z_sim, 1, sd_share, "/")
  lower_z_sim <- sweep(lower_z_sim, 1, sd_share, "/")
  neighbours <- v16_neighbour_structure(
    d, maximum_km = maximum_neighbour_km,
    isolation_km = isolation_km, same_fold_only = TRUE
  )
  observed_isolation <- as.numeric(
    v16_local_isolation(observed_share, neighbours)[, 1]
  )
  simulated_isolation <- v16_local_isolation(shares, neighbours)
  population <- as.numeric(d$log_population_sum_25km)
  effort <- log1p(as.numeric(d$n_observations))
  independent_sites <- log1p(as.numeric(d$n_independent_sites))
  observed_early <- rep(NA_real_, nrow(d))
  simulated_early <- matrix(NA_real_, nrow(d), ncol(counts))
  if (!is.null(phenology_result)) {
    phenology_index <- match(d$exact_site_id, phenology_result$cell_id)
    observed_early <- phenology_result$latent_mean[phenology_index] - d$median_DOY
    simulated_early <- matrix(observed_early, nrow(d), ncol(counts))
  }
  observed_intensity <- rep(NA_real_, nrow(d))
  simulated_intensity <- matrix(NA_real_, nrow(d), ncol(counts))
  if (!is.null(intensity_result)) {
    intensity_index <- match(d$exact_site_id, intensity_result$cell_id)
    observed_intensity <- d$conditional_intensity_median -
      intensity_result$latent_mean[intensity_index]
    simulated_intensity <- matrix(observed_intensity, nrow(d), ncol(counts))
  }
  metric_definition <- list(
    mean_population_context = list(
      observed = population,
      simulated = matrix(population, nrow(d), ncol(counts)),
      alternative = "greater"
    ),
    mean_early_phenology_surprise = list(
      observed = observed_early, simulated = simulated_early,
      alternative = "greater"
    ),
    mean_intensity_surprise = list(
      observed = observed_intensity, simulated = simulated_intensity,
      alternative = "greater", pigmented_only = TRUE,
      analysis_support = is.finite(observed_intensity)
    ),
    mean_local_colour_isolation = list(
      observed = observed_isolation, simulated = simulated_isolation,
      alternative = "greater", directional = TRUE
    ),
    mean_log_observation_effort = list(
      observed = effort,
      simulated = matrix(effort, nrow(d), ncol(counts)),
      alternative = "less"
    ),
    mean_log_independent_sites = list(
      observed = independent_sites,
      simulated = matrix(independent_sites, nrow(d), ncol(counts)),
      alternative = "less"
    )
  )
  rows <- list()
  ids <- as.character(d$exact_site_id)
  for (direction in c("pigmented", "white")) {
    observed_q <- if (direction == "pigmented") {
      scores$unexpected_pigmented_q
    } else scores$unexpected_white_q
    observed_z <- if (direction == "pigmented") {
      scores$unexpected_pigmented_z
    } else scores$unexpected_white_z
    observed_eligible <- if (direction == "pigmented") {
      d$n_pigmented > 0
    } else d$n_white > 0
    q_sim <- if (direction == "pigmented") upper_q_sim else lower_q_sim
    z_sim <- if (direction == "pigmented") upper_z_sim else lower_z_sim
    simulated_eligible <- if (direction == "pigmented") counts > 0 else counts < trials
    observed_absolute <- sum(observed_eligible & observed_q <= 0.05, na.rm = TRUE)
    simulated_absolute <- colSums(simulated_eligible & q_sim <= 0.05, na.rm = TRUE)
    comparison <- v16_null_comparison(
      observed_absolute, simulated_absolute, alternative = "greater"
    )
    rows[[length(rows) + 1L]] <- data.frame(
      model = presence_result$model, direction = direction,
      tier = "absolute_q05", metric = "candidate_count",
      observed_value = observed_absolute, n_observed_selected = observed_absolute,
      n_null_draws = ncol(counts), t(comparison), stringsAsFactors = FALSE
    )
    gradient_cache <- list()
    for (fraction in tiers) {
      tier <- paste0("top_", sprintf("%02d", round(100 * fraction)))
      observed_selected <- v16_top_indices(
        observed_q, observed_z, observed_eligible, fraction, ids
      )
      simulated_selected <- lapply(seq_len(ncol(counts)), function(draw) {
        v16_top_indices(
          q_sim[, draw], z_sim[, draw], simulated_eligible[, draw],
          fraction, ids
        )
      })
      observed_isolated_fraction <- v16_isolated_fraction(
        observed_selected, neighbours
      )
      simulated_isolated_fraction <- vapply(
        simulated_selected, v16_isolated_fraction, numeric(1),
        neighbours = neighbours
      )
      comparison <- v16_null_comparison(
        observed_isolated_fraction, simulated_isolated_fraction, "greater"
      )
      rows[[length(rows) + 1L]] <- data.frame(
        model = presence_result$model, direction = direction, tier = tier,
        metric = "isolated_candidate_fraction_25km",
        observed_value = observed_isolated_fraction,
        n_observed_selected = length(observed_selected),
        n_null_draws = ncol(counts), t(comparison), stringsAsFactors = FALSE
      )
      gradient_cache[["isolated_candidate_fraction_25km"]][[tier]] <- list(
        observed = observed_isolated_fraction,
        null = simulated_isolated_fraction,
        alternative = "greater"
      )
      for (metric in names(metric_definition)) {
        definition <- metric_definition[[metric]]
        if (isTRUE(definition$pigmented_only) && direction == "white") next
        metric_observed_selected <- observed_selected
        metric_simulated_selected <- simulated_selected
        if (!is.null(definition$analysis_support)) {
          support <- as.logical(definition$analysis_support)
          metric_observed_selected <- v16_top_indices(
            observed_q, observed_z, observed_eligible & support,
            fraction, ids
          )
          metric_simulated_selected <- lapply(
            seq_len(ncol(counts)), function(draw) {
              v16_top_indices(
                q_sim[, draw], z_sim[, draw],
                simulated_eligible[, draw] & support,
                fraction, ids
              )
            }
          )
        }
        observed_values <- definition$observed
        simulated_values <- definition$simulated
        if (isTRUE(definition$directional) && direction == "white") {
          observed_values <- -observed_values
          simulated_values <- -simulated_values
        }
        observed_value <- mean(
          observed_values[metric_observed_selected], na.rm = TRUE
        )
        if (!is.finite(observed_value)) observed_value <- NA_real_
        null_value <- vapply(seq_along(metric_simulated_selected), function(draw) {
          selected <- metric_simulated_selected[[draw]]
          value <- mean(simulated_values[selected, draw], na.rm = TRUE)
          if (is.finite(value)) value else NA_real_
        }, numeric(1))
        comparison <- v16_null_comparison(
          observed_value, null_value, definition$alternative
        )
        rows[[length(rows) + 1L]] <- data.frame(
          model = presence_result$model, direction = direction, tier = tier,
          metric = metric, observed_value = observed_value,
          n_observed_selected = length(metric_observed_selected),
          n_null_draws = sum(is.finite(null_value)), t(comparison),
          stringsAsFactors = FALSE
        )
        gradient_cache[[metric]][[tier]] <- list(
          observed = observed_value, null = null_value,
          alternative = definition$alternative
        )
      }
    }
    for (metric in names(gradient_cache)) {
      cache <- gradient_cache[[metric]]
      if (!all(c("top_05", "top_20") %in% names(cache))) next
      direction_multiplier <- if (
        identical(cache$top_05$alternative, "less")
      ) -1 else 1
      observed_gradient <- direction_multiplier * (
        cache$top_05$observed - cache$top_20$observed
      )
      null_gradient <- direction_multiplier * (
        cache$top_05$null - cache$top_20$null
      )
      comparison <- v16_null_comparison(
        observed_gradient, null_gradient, "greater"
      )
      rows[[length(rows) + 1L]] <- data.frame(
        model = presence_result$model, direction = direction,
        tier = "top_05_minus_top_20",
        metric = paste0(metric, "_tier_gradient"),
        observed_value = observed_gradient,
        n_observed_selected = NA_integer_,
        n_null_draws = sum(is.finite(null_gradient)),
        t(comparison), stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  out$BH_q <- stats::p.adjust(out$empirical_p, method = "BH")
  list(
    summary = out,
    scores = scores,
    observed_local_isolation = observed_isolation,
    observed_early_surprise = observed_early,
    observed_intensity_surprise = observed_intensity
  )
}

v16_rank_sensitivity <- function(score_tables) {
  pairs <- combn(names(score_tables), 2, simplify = FALSE)
  rows <- list()
  for (pair in pairs) {
    left <- score_tables[[pair[1]]]
    right <- score_tables[[pair[2]]]
    merged <- merge(
      left, right, by = "exact_site_id", suffixes = c("_left", "_right")
    )
    for (direction in c("unexpected_pigmented", "unexpected_white")) {
      rank_name <- paste0(direction, "_rank")
      top_name <- paste0(direction, "_top20")
      keep <- is.finite(merged[[paste0(rank_name, "_left")]]) &
        is.finite(merged[[paste0(rank_name, "_right")]])
      left_top <- merged[[paste0(top_name, "_left")]][keep]
      right_top <- merged[[paste0(top_name, "_right")]][keep]
      union <- sum(left_top | right_top)
      rows[[length(rows) + 1L]] <- data.frame(
        model_left = pair[1], model_right = pair[2], direction = direction,
        n_common = sum(keep),
        spearman_rank_correlation = if (sum(keep) >= 3L) {
          stats::cor(
            merged[[paste0(rank_name, "_left")]][keep],
            merged[[paste0(rank_name, "_right")]][keep],
            method = "spearman"
          )
        } else NA_real_,
        top20_jaccard = if (union) sum(left_top & right_top) / union else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

v16_simulation_stability <- function(result, cells) {
  n_draws <- ncol(result$draws)
  midpoint <- floor(n_draws / 2)
  if (midpoint < 50L || n_draws - midpoint < 50L) return(data.frame())
  halves <- list(
    first = result$draws[, seq_len(midpoint), drop = FALSE],
    second = result$draws[, (midpoint + 1L):n_draws, drop = FALSE]
  )
  score <- lapply(halves, function(draws) {
    copy <- result
    copy$draws <- draws
    v16_presence_scores(copy, cells)
  })
  rows <- list()
  for (direction in c("unexpected_pigmented", "unexpected_white")) {
    q <- paste0(direction, "_q")
    rank_name <- paste0(direction, "_rank")
    top <- paste0(direction, "_top20")
    keep <- is.finite(score$first[[rank_name]]) &
      is.finite(score$second[[rank_name]])
    union <- sum(score$first[[top]][keep] | score$second[[top]][keep])
    rows[[length(rows) + 1L]] <- data.frame(
      model = result$model, direction = direction,
      n_draws_first = midpoint, n_draws_second = n_draws - midpoint,
      maximum_absolute_tail_probability_difference = max(
        abs(score$first[[q]] - score$second[[q]]), na.rm = TRUE
      ),
      median_absolute_tail_probability_difference = stats::median(
        abs(score$first[[q]] - score$second[[q]]), na.rm = TRUE
      ),
      spearman_rank_correlation = stats::cor(
        score$first[[rank_name]][keep], score$second[[rank_name]][keep],
        method = "spearman"
      ),
      top20_jaccard = if (union) {
        sum(score$first[[top]][keep] & score$second[[top]][keep]) / union
      } else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}
