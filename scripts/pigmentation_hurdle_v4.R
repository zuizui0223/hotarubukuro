largest_mean_gap_split <- function(means) {
  means <- as.numeric(means)
  if (length(means) < 2L || any(!is.finite(means))) {
    stop("Pigmentation mixture requires at least two finite component means.", call. = FALSE)
  }
  order_index <- order(means)
  gap_index <- which.max(diff(means[order_index]))
  list(
    white_components = order_index[seq_len(gap_index)],
    pigmented_components = order_index[seq.int(gap_index + 1L, length(order_index))],
    lower_mean = means[order_index[gap_index]],
    upper_mean = means[order_index[gap_index + 1L]],
    gap = means[order_index[gap_index + 1L]] - means[order_index[gap_index]]
  )
}

capture_warnings_v4 <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = unique(messages))
}

collapse_warnings_v4 <- function(messages) {
  messages <- unique(messages[nzchar(messages)])
  if (length(messages)) paste(messages, collapse = " | ") else ""
}

mixture_decision_boundary <- function(fit, pigmented_components, lower, upper,
                                      grid_n = 20001L) {
  grid <- seq(lower, upper, length.out = grid_n)
  posterior <- predict(fit, newdata = grid)$z
  pigmented_probability <- rowSums(
    posterior[, pigmented_components, drop = FALSE]
  )
  grid[which.min(abs(pigmented_probability - 0.5))]
}

fit_pigmentation_measurement <- function(colour_a, colour_L = NULL, colour_C = NULL,
                                         max_components = 8L,
                                         confidence_cut = 0.8) {
  require_packages("mclust")
  # Mclust evaluates a few internal helpers through the attached package
  # environment in mclust 6.1.2. Attach it explicitly for script/test parity.
  if (!("package:mclust" %in% search())) {
    suppressPackageStartupMessages(
      base::require("mclust", character.only = TRUE)
    )
  }
  colour_a <- as.numeric(colour_a)
  if (any(!is.finite(colour_a))) {
    stop("Primary pigmentation measurement requires finite CIELAB a*.", call. = FALSE)
  }
  fit <- mclust::Mclust(
    colour_a, G = seq_len(as.integer(max_components)),
    modelNames = c("E", "V"), verbose = FALSE
  )
  if (is.null(fit) || fit$G < 2L) {
    stop("CIELAB a* did not support a two-regime pigmentation mixture.", call. = FALSE)
  }
  means <- as.numeric(fit$parameters$mean)
  split <- largest_mean_gap_split(means)
  pigmented_probability <- rowSums(
    fit$z[, split$pigmented_components, drop = FALSE]
  )
  boundary <- mixture_decision_boundary(
    fit, split$pigmented_components, split$lower_mean, split$upper_mean
  )
  pigmented <- pigmented_probability >= 0.5
  high_confidence <- ifelse(
    pigmented_probability >= confidence_cut, 1L,
    ifelse(pigmented_probability <= 1 - confidence_cut, 0L, NA_integer_)
  )
  excess <- pmax(colour_a - boundary, 0)
  intensity_z <- rep(NA_real_, length(colour_a))
  intensity_z[pigmented] <- safe_z(excess[pigmented])

  variance <- as.numeric(fit$parameters$variance$sigmasq)
  if (length(variance) == 1L) variance <- rep(variance, fit$G)
  components <- data.frame(
    component = seq_len(fit$G),
    mean_a = means,
    sd_a = sqrt(variance),
    proportion = as.numeric(fit$parameters$pro),
    measurement_regime = ifelse(
      seq_len(fit$G) %in% split$pigmented_components,
      "pigmented", "white_colour_noise"
    ),
    stringsAsFactors = FALSE
  )

  joint_probability <- rep(NA_real_, length(colour_a))
  joint_summary <- data.frame()
  if (!is.null(colour_L) && !is.null(colour_C) &&
      all(is.finite(colour_L)) && all(is.finite(colour_C))) {
    raw_joint <- cbind(
      a = colour_a, darkness = -as.numeric(colour_L), chroma = as.numeric(colour_C)
    )
    joint_scaled <- scale(raw_joint)
    joint_fit <- mclust::Mclust(
      joint_scaled, G = seq_len(min(6L, as.integer(max_components))), verbose = FALSE
    )
    joint_a_means <- as.numeric(
      attr(joint_scaled, "scaled:center")[1] +
        attr(joint_scaled, "scaled:scale")[1] * joint_fit$parameters$mean[1, ]
    )
    joint_split <- largest_mean_gap_split(joint_a_means)
    joint_probability <- rowSums(
      joint_fit$z[, joint_split$pigmented_components, drop = FALSE]
    )
    joint_summary <- data.frame(
      component = seq_len(joint_fit$G),
      mean_a = joint_a_means,
      proportion = as.numeric(joint_fit$parameters$pro),
      measurement_regime = ifelse(
        seq_len(joint_fit$G) %in% joint_split$pigmented_components,
        "pigmented", "white_colour_noise"
      ),
      model = joint_fit$modelName,
      stringsAsFactors = FALSE
    )
  }

  observations <- data.frame(
    pigment_probability_a = pigmented_probability,
    pigmented_mixture50 = as.integer(pigmented),
    pigmented_high_confidence = high_confidence,
    pigmentation_class = ifelse(pigmented, "pigmented", "white"),
    pigment_boundary_a = boundary,
    pigment_excess_a = excess,
    pigment_intensity_z = intensity_z,
    pigmented_zero_rule = as.integer(colour_a > 0),
    pigment_probability_joint_lab = joint_probability,
    pigmented_joint_lab50 = ifelse(
      is.finite(joint_probability), as.integer(joint_probability >= 0.5), NA_integer_
    ),
    stringsAsFactors = FALSE
  )
  zero_mask <- observations$pigmented_zero_rule == 1L
  high_confidence_mask <- observations$pigmented_high_confidence == 1L &
    !is.na(observations$pigmented_high_confidence)
  joint_mask <- observations$pigmented_joint_lab50 == 1L &
    !is.na(observations$pigmented_joint_lab50)
  observations$pigment_intensity_zero_z <- NA_real_
  observations$pigment_intensity_high_confidence_z <- NA_real_
  observations$pigment_intensity_joint_lab_z <- NA_real_
  observations$pigment_intensity_zero_z[zero_mask] <- safe_z(colour_a[zero_mask])
  observations$pigment_intensity_high_confidence_z[high_confidence_mask] <-
    safe_z(colour_a[high_confidence_mask])
  observations$pigment_intensity_joint_lab_z[joint_mask] <-
    safe_z(colour_a[joint_mask])
  summary <- data.frame(
    measurement_model = "response-blind univariate Gaussian mixture of CIELAB a*",
    mixture_model = fit$modelName,
    components = fit$G,
    largest_component_mean_gap = split$gap,
    decision_boundary_a = boundary,
    confidence_cut = confidence_cut,
    n = length(colour_a),
    n_white = sum(!pigmented),
    n_pigmented = sum(pigmented),
    n_ambiguous = sum(is.na(high_confidence)),
    n_negative_a = sum(colour_a < 0),
    n_nonnegative_below_boundary = sum(colour_a >= 0 & colour_a < boundary),
    interpretation = paste(
      "white-flower a* is treated as colour-measurement variation, not anthocyanin amount;",
      "the boundary is estimated without geography, environment, Bombus, H, R, or residuals"
    ),
    stringsAsFactors = FALSE
  )
  list(
    observations = observations, summary = summary, components = components,
    joint_components = joint_summary, fit = fit
  )
}

binary_auc <- function(y, probability) {
  keep <- is.finite(y) & is.finite(probability)
  y <- as.integer(y[keep])
  probability <- probability[keep]
  n1 <- sum(y == 1L)
  n0 <- sum(y == 0L)
  if (!n1 || !n0) return(NA_real_)
  ranks <- rank(probability, ties.method = "average")
  (sum(ranks[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

hurdle_prediction_metrics <- function(observed, predicted, family) {
  if (family == "binomial") {
    predicted <- pmin(pmax(predicted, 1e-8), 1 - 1e-8)
    data.frame(
      log_loss = -mean(observed * log(predicted) + (1 - observed) * log(1 - predicted)),
      brier = mean((observed - predicted)^2),
      auc = binary_auc(observed, predicted),
      RMSE = NA_real_, R2 = NA_real_
    )
  } else {
    denom <- sum((observed - mean(observed))^2)
    data.frame(
      log_loss = NA_real_, brier = NA_real_, auc = NA_real_,
      RMSE = sqrt(mean((observed - predicted)^2)),
      R2 = 1 - sum((observed - predicted)^2) / denom
    )
  }
}

crossfit_hurdle_increment <- function(data, response, base_terms, added_terms,
                                      family = c("gaussian", "binomial"),
                                      spatial = FALSE, k_space = 40,
                                      fold_col = "spatial_fold") {
  require_packages("mgcv")
  family <- match.arg(family)
  terms <- unique(c(base_terms, added_terms))
  required <- unique(c(
    response, terms, fold_col, if (spatial) c("x_km", "y_km")
  ))
  d <- data[stats::complete.cases(data[required]), , drop = FALSE]
  folds <- sort(unique(d[[fold_col]]))
  base_prediction <- full_prediction <- rep(NA_real_, nrow(d))
  logs <- vector("list", length(folds))
  family_object <- if (family == "binomial") stats::binomial() else stats::gaussian()
  for (i in seq_along(folds)) {
    fold <- folds[i]
    test <- which(d[[fold_col]] == fold)
    train <- which(d[[fold_col]] != fold)
    k_use <- max(10L, min(as.integer(k_space), floor(length(train) / 20)))
    base_formula <- gam_formula_v3(response, base_terms, spatial, k_use)
    full_formula <- gam_formula_v3(response, terms, spatial, k_use)
    base_result <- capture_warnings_v4(mgcv::gam(
      base_formula, data = d[train, , drop = FALSE], family = family_object,
      method = "REML"
    ))
    full_result <- capture_warnings_v4(mgcv::gam(
      full_formula, data = d[train, , drop = FALSE], family = family_object,
      method = "REML"
    ))
    base_fit <- base_result$value
    full_fit <- full_result$value
    base_prediction[test] <- as.numeric(stats::predict(
      base_fit, newdata = d[test, , drop = FALSE], type = "response"
    ))
    full_prediction[test] <- as.numeric(stats::predict(
      full_fit, newdata = d[test, , drop = FALSE], type = "response"
    ))
    logs[[i]] <- data.frame(
      fold = fold, n_train = length(train), n_test = length(test),
      base_formula = paste(deparse(base_formula), collapse = " "),
      full_formula = paste(deparse(full_formula), collapse = " "),
      base_warnings = collapse_warnings_v4(base_result$warnings),
      full_warnings = collapse_warnings_v4(full_result$warnings),
      stringsAsFactors = FALSE
    )
  }
  base_metrics <- hurdle_prediction_metrics(d[[response]], base_prediction, family)
  full_metrics <- hurdle_prediction_metrics(d[[response]], full_prediction, family)
  metrics <- data.frame(
    family = family, n = nrow(d),
    base_log_loss = base_metrics$log_loss,
    full_log_loss = full_metrics$log_loss,
    delta_log_loss = base_metrics$log_loss - full_metrics$log_loss,
    base_brier = base_metrics$brier,
    full_brier = full_metrics$brier,
    delta_brier = base_metrics$brier - full_metrics$brier,
    base_auc = base_metrics$auc,
    full_auc = full_metrics$auc,
    delta_auc = full_metrics$auc - base_metrics$auc,
    base_RMSE = base_metrics$RMSE,
    full_RMSE = full_metrics$RMSE,
    delta_R2 = full_metrics$R2 - base_metrics$R2,
    base_R2 = base_metrics$R2,
    full_R2 = full_metrics$R2,
    n_warning_folds = sum(vapply(
      logs,
      function(x) nzchar(x$base_warnings) || nzchar(x$full_warnings),
      logical(1)
    )),
    warning_messages = collapse_warnings_v4(c(
      vapply(logs, `[[`, character(1), "base_warnings"),
      vapply(logs, `[[`, character(1), "full_warnings")
    )),
    stringsAsFactors = FALSE
  )
  list(
    metrics = metrics,
    predictions = data.frame(
      source_row = as.integer(rownames(d)), observed = d[[response]],
      base = base_prediction, full = full_prediction
    ),
    log = do.call(rbind, logs)
  )
}

crossfit_hurdle_prediction <- function(data, response, terms,
                                       family = c("gaussian", "binomial"),
                                       spatial = TRUE, k_space = 40,
                                       fold_col = "spatial_fold") {
  require_packages("mgcv")
  family <- match.arg(family)
  required <- unique(c(
    response, terms, fold_col, if (spatial) c("x_km", "y_km")
  ))
  d <- data[stats::complete.cases(data[required]), , drop = FALSE]
  folds <- sort(unique(d[[fold_col]]))
  prediction <- rep(NA_real_, nrow(d))
  logs <- vector("list", length(folds))
  family_object <- if (family == "binomial") stats::binomial() else stats::gaussian()
  for (i in seq_along(folds)) {
    fold <- folds[i]
    test <- which(d[[fold_col]] == fold)
    train <- which(d[[fold_col]] != fold)
    k_use <- max(10L, min(as.integer(k_space), floor(length(train) / 20)))
    formula <- gam_formula_v3(response, terms, spatial, k_use)
    fit_result <- capture_warnings_v4(mgcv::gam(
      formula, data = d[train, , drop = FALSE], family = family_object,
      method = "REML"
    ))
    prediction[test] <- as.numeric(stats::predict(
      fit_result$value, newdata = d[test, , drop = FALSE], type = "response"
    ))
    logs[[i]] <- data.frame(
      fold = fold, n_train = length(train), n_test = length(test),
      formula = paste(deparse(formula), collapse = " "),
      warnings = collapse_warnings_v4(fit_result$warnings),
      stringsAsFactors = FALSE
    )
  }
  if (any(!is.finite(prediction))) {
    stop("Cross-fitted hurdle natural model produced missing predictions.", call. = FALSE)
  }
  list(
    source_row = as.integer(rownames(d)), observed = d[[response]],
    prediction = prediction, log = do.call(rbind, logs), family = family
  )
}

rank01_complete <- function(x) {
  out <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (!any(keep)) return(out)
  if (sum(keep) == 1L) {
    out[keep] <- 1
  } else {
    out[keep] <- (rank(x[keep], ties.method = "average") - 1) / (sum(keep) - 1)
  }
  out
}

build_hurdle_natural_residuals <- function(data, env_terms, k_space = 40) {
  natural_terms <- c("region", env_terms, "Bombus_W")
  presence <- crossfit_hurdle_prediction(
    data, "pigmented_mixture50", natural_terms, "binomial", TRUE, k_space
  )
  intensity <- crossfit_hurdle_prediction(
    data, "pigment_intensity_z", natural_terms, "gaussian", TRUE, k_space
  )
  data$natural_presence_probability <- NA_real_
  data$natural_presence_probability[presence$source_row] <- presence$prediction
  probability <- pmin(pmax(data$natural_presence_probability, 1e-8), 1 - 1e-8)
  y <- data$pigmented_mixture50
  data$presence_natural_deviance_residual <- sign(y - probability) * sqrt(
    -2 * (y * log(probability) + (1 - y) * log(1 - probability))
  )
  data$pigmented_presence_surprise <- ifelse(
    y == 1L, 1 - data$natural_presence_probability, NA_real_
  )
  data$natural_intensity_prediction <- NA_real_
  data$natural_intensity_prediction[intensity$source_row] <- intensity$prediction
  data$intensity_natural_residual <-
    data$pigment_intensity_z - data$natural_intensity_prediction

  data$presence_residual_percentile <- NA_real_
  data$intensity_residual_percentile <- NA_real_
  for (region_name in levels(data$region)) {
    idx <- data$region == region_name
    data$presence_residual_percentile[idx] <- rank01_complete(
      data$presence_natural_deviance_residual[idx]
    )
    data$intensity_residual_percentile[idx] <- rank01_complete(
      data$intensity_natural_residual[idx]
    )
  }
  presence$log$outcome <- "pigmentation_presence"
  intensity$log$outcome <- "pigmented_intensity"
  list(
    data = data,
    log = rbind(presence$log, intensity$log),
    natural_terms = natural_terms
  )
}

fit_hurdle_residual_tail <- function(data, k_space = 40,
                                     thresholds = c(0.80, 0.90, 0.95)) {
  require_packages("mgcv")
  data$z_H_R <- data$z_H * data$z_R
  data$DOY_z <- safe_z(data$DOY)
  data$DOY_z2 <- safe_z(data$DOY_z^2)
  data$DOY_z3 <- safe_z(data$DOY_z^3)
  data$year_z <- safe_z(data$year)
  outcomes <- list(
    pigmentation_presence = "presence_residual_percentile",
    pigmented_intensity = "intensity_residual_percentile"
  )
  variants <- list(
    primary = list(extra = character(), subset = rep(TRUE, nrow(data))),
    date_adjusted = list(
      extra = c("DOY_z", "DOY_z2", "DOY_z3", "year_z"),
      subset = rep(TRUE, nrow(data))
    ),
    access_adjusted = list(
      extra = if (is_available_axis(data$z_A)) "z_A" else character(),
      subset = rep(TRUE, nrow(data))
    ),
    boundary_excluded = list(
      extra = character(),
      subset = if ("R_primary_mesh_boundary" %in% names(data)) {
        !data$R_primary_mesh_boundary
      } else rep(TRUE, nrow(data))
    )
  )
  coefficients <- heldout <- logs <- enrichment <- candidates <- list()
  row_i <- 0L
  for (outcome_name in names(outcomes)) {
    percentile <- outcomes[[outcome_name]]
    for (region_name in levels(data$region)) {
      region_rows <- data$region == region_name & is.finite(data[[percentile]])
      for (threshold in thresholds) {
        flag_name <- paste0("tail_flag_", gsub("\\.", "", format(threshold)))
        data[[flag_name]] <- as.integer(data[[percentile]] >= threshold)
        for (variant_name in names(variants)) {
          variant <- variants[[variant_name]]
          d <- droplevels(data[
            region_rows & variant$subset, , drop = FALSE
          ])
          base_terms <- variant$extra
          full_terms <- c(base_terms, "z_H", "z_R", "z_H_R")
          required <- c(flag_name, full_terms, "x_km", "y_km", "spatial_fold")
          d <- d[stats::complete.cases(d[required]), , drop = FALSE]
          if (nrow(d) < 100L || min(table(d[[flag_name]])) < 6L) next
          k_use <- max(10L, min(as.integer(k_space), floor(nrow(d) / 20)))
          formula <- gam_formula_v3(flag_name, full_terms, TRUE, k_use)
          fit_result <- capture_warnings_v4(mgcv::gam(
            formula, data = d, family = stats::binomial(), method = "REML"
          ))
          tab <- summary(fit_result$value)$p.table
          wanted <- intersect(c("z_H", "z_R", "z_H_R"), rownames(tab))
          row_i <- row_i + 1L
          coefficients[[row_i]] <- data.frame(
            outcome = outcome_name, region = region_name, threshold = threshold,
            variant = variant_name, term = wanted,
            estimate = tab[wanted, 1], se = tab[wanted, 2],
            lower_95 = tab[wanted, 1] - 1.96 * tab[wanted, 2],
            upper_95 = tab[wanted, 1] + 1.96 * tab[wanted, 2],
            statistic = tab[wanted, 3], p_value = tab[wanted, 4],
            n = nrow(d), events = sum(d[[flag_name]]),
            model_warnings = collapse_warnings_v4(fit_result$warnings),
            stringsAsFactors = FALSE
          )
          cv <- crossfit_hurdle_increment(
            d, flag_name, base_terms, c("z_H", "z_R", "z_H_R"),
            "binomial", TRUE, k_space
          )
          cv$metrics$outcome <- outcome_name
          cv$metrics$region <- region_name
          cv$metrics$threshold <- threshold
          cv$metrics$variant <- variant_name
          heldout[[length(heldout) + 1L]] <- cv$metrics
          cv$log$outcome <- outcome_name
          cv$log$region <- region_name
          cv$log$threshold <- threshold
          cv$log$variant <- variant_name
          logs[[length(logs) + 1L]] <- cv$log
        }

        d_enrich <- data[region_rows, , drop = FALSE]
        upper <- d_enrich[[percentile]] >= threshold
        early <- d_enrich$early_score >= 0.9
        keep <- !is.na(upper) & !is.na(early)
        table_2x2 <- table(
          factor(upper[keep], levels = c(FALSE, TRUE)),
          factor(early[keep], levels = c(FALSE, TRUE))
        )
        fisher <- if (all(dim(table_2x2) == c(2L, 2L))) {
          stats::fisher.test(table_2x2)
        } else NULL
        enrichment[[length(enrichment) + 1L]] <- data.frame(
          outcome = outcome_name, region = region_name, threshold = threshold,
          n = sum(keep), tail_n = sum(upper[keep]),
          early_tail_n = sum(upper[keep] & early[keep]),
          early_tail_fraction = mean(early[keep][upper[keep]]),
          early_nontail_fraction = mean(early[keep][!upper[keep]]),
          fisher_odds_ratio = if (is.null(fisher)) NA_real_ else unname(fisher$estimate),
          fisher_p_value = if (is.null(fisher)) NA_real_ else fisher$p.value,
          stringsAsFactors = FALSE
        )
      }
      candidate_rows <- data[region_rows, , drop = FALSE]
      candidate_rows$candidate_outcome <- outcome_name
      candidate_rows$candidate_percentile <- candidate_rows[[percentile]]
      candidates[[length(candidates) + 1L]] <- candidate_rows
    }
  }
  candidate_data <- do.call(rbind, candidates)
  candidate_data <- candidate_data[order(
    candidate_data$candidate_outcome, candidate_data$region,
    -candidate_data$candidate_percentile
  ), ]
  candidate_columns <- intersect(c(
    "candidate_outcome", "candidate_percentile", "observation_id", "sample_id",
    "longitude", "latitude", "region", "colour_a", "pigmentation_class",
    "natural_presence_probability", "presence_natural_deviance_residual",
    "natural_intensity_prediction", "intensity_natural_residual",
    "z_H", "z_R", "z_H_R", "early_score", "z_A", "DOY", "year",
    "spatial_fold"
  ), names(candidate_data))
  list(
    coefficients = do.call(rbind, coefficients),
    heldout = do.call(rbind, heldout),
    log = do.call(rbind, logs),
    early_enrichment = do.call(rbind, enrichment),
    candidates = candidate_data[candidate_columns]
  )
}

fit_hurdle_evidence_ladder <- function(data, response, predictor, env_terms,
                                       family = c("gaussian", "binomial"),
                                       k_space = 40) {
  require_packages("mgcv")
  family <- match.arg(family)
  family_object <- if (family == "binomial") stats::binomial() else stats::gaussian()
  rungs <- list(
    landscape = list(base = character(), spatial = FALSE),
    environment_adjusted = list(base = env_terms, spatial = FALSE),
    space_adjusted = list(base = "region", spatial = TRUE),
    environment_space_adjusted = list(base = c("region", env_terms), spatial = TRUE)
  )
  coefficient_rows <- metric_rows <- log_rows <- list()
  for (i in seq_along(rungs)) {
    rung <- names(rungs)[i]
    spec <- rungs[[i]]
    base_terms <- spec$base
    required <- unique(c(
      response, base_terms, predictor, "spatial_fold",
      if (spec$spatial) c("x_km", "y_km")
    ))
    d <- data[stats::complete.cases(data[required]), , drop = FALSE]
    if ("region" %in% base_terms && nlevels(droplevels(d$region)) < 2L) {
      base_terms <- setdiff(base_terms, "region")
    }
    k_use <- max(10L, min(as.integer(k_space), floor(nrow(d) / 20)))
    formula <- gam_formula_v3(
      response, c(base_terms, predictor), spec$spatial, k_use
    )
    fit_result <- capture_warnings_v4(mgcv::gam(
      formula, data = d, family = family_object, method = "REML"
    ))
    fit <- fit_result$value
    tab <- summary(fit)$p.table
    coefficient_rows[[i]] <- data.frame(
      outcome = response, family = family, predictor = predictor, rung = rung,
      n = nrow(d), events = if (family == "binomial") sum(d[[response]]) else NA_real_,
      estimate = tab[predictor, 1], se = tab[predictor, 2],
      lower_95 = tab[predictor, 1] - 1.96 * tab[predictor, 2],
      upper_95 = tab[predictor, 1] + 1.96 * tab[predictor, 2],
      statistic = tab[predictor, 3], p_value = tab[predictor, 4],
      formula = paste(deparse(formula), collapse = " "),
      model_warnings = collapse_warnings_v4(fit_result$warnings),
      stringsAsFactors = FALSE
    )
    cv <- crossfit_hurdle_increment(
      data, response, base_terms, predictor, family, spec$spatial, k_space
    )
    cv$metrics$outcome <- response
    cv$metrics$predictor <- predictor
    cv$metrics$rung <- rung
    metric_rows[[i]] <- cv$metrics
    cv$log$outcome <- response
    cv$log$predictor <- predictor
    cv$log$rung <- rung
    log_rows[[i]] <- cv$log
  }
  list(
    coefficients = do.call(rbind, coefficient_rows),
    heldout = do.call(rbind, metric_rows),
    log = do.call(rbind, log_rows)
  )
}

fit_hurdle_interface <- function(data, env_terms, k_space = 40) {
  require_packages("mgcv")
  data$z_H_R <- data$z_H * data$z_R
  data$DOY_z <- safe_z(data$DOY)
  data$DOY_z2 <- safe_z(data$DOY_z^2)
  data$DOY_z3 <- safe_z(data$DOY_z^3)
  data$year_z <- safe_z(data$year)
  outcomes <- list(
    pigmentation_presence = list(response = "pigmented_mixture50", family = "binomial"),
    pigmented_intensity = list(response = "pigment_intensity_z", family = "gaussian")
  )
  variants <- list(
    primary = list(extra = character(), subset = rep(TRUE, nrow(data))),
    date_adjusted = list(
      extra = c("DOY_z", "DOY_z2", "DOY_z3", "year_z"),
      subset = rep(TRUE, nrow(data))
    ),
    access_adjusted = list(
      extra = if (is_available_axis(data$z_A)) "z_A" else character(),
      subset = rep(TRUE, nrow(data))
    ),
    boundary_excluded = list(
      extra = character(),
      subset = if ("R_primary_mesh_boundary" %in% names(data)) {
        !data$R_primary_mesh_boundary
      } else rep(TRUE, nrow(data))
    )
  )
  coefficients <- heldout <- logs <- list()
  z <- 0L
  natural <- c(env_terms, "Bombus_W")
  for (outcome_name in names(outcomes)) {
    spec <- outcomes[[outcome_name]]
    for (region_name in levels(data$region)) {
      for (variant_name in names(variants)) {
        variant <- variants[[variant_name]]
        d <- droplevels(data[
          data$region == region_name & variant$subset, , drop = FALSE
        ])
        base_terms <- c(natural, variant$extra)
        full_terms <- c(base_terms, "z_H", "z_R", "z_H_R")
        required <- c(spec$response, full_terms, "x_km", "y_km", "spatial_fold")
        d <- d[stats::complete.cases(d[required]), , drop = FALSE]
        if (nrow(d) < 100L || (spec$family == "binomial" &&
            min(table(d[[spec$response]])) < 15L)) next
        k_use <- max(10L, min(as.integer(k_space), floor(nrow(d) / 20)))
        family_object <- if (spec$family == "binomial") {
          stats::binomial()
        } else stats::gaussian()
        formula <- gam_formula_v3(spec$response, full_terms, TRUE, k_use)
        fit_result <- capture_warnings_v4(mgcv::gam(
          formula, data = d, family = family_object, method = "REML"
        ))
        fit <- fit_result$value
        tab <- summary(fit)$p.table
        wanted <- intersect(c("z_H", "z_R", "z_H_R"), rownames(tab))
        z <- z + 1L
        coefficients[[z]] <- data.frame(
          outcome = outcome_name, response = spec$response, family = spec$family,
          region = region_name, variant = variant_name,
          term = wanted, estimate = tab[wanted, 1], se = tab[wanted, 2],
          lower_95 = tab[wanted, 1] - 1.96 * tab[wanted, 2],
          upper_95 = tab[wanted, 1] + 1.96 * tab[wanted, 2],
          statistic = tab[wanted, 3], p_value = tab[wanted, 4],
          n = nrow(d), events = if (spec$family == "binomial") {
            sum(d[[spec$response]])
          } else NA_real_,
          model_warnings = collapse_warnings_v4(fit_result$warnings),
          stringsAsFactors = FALSE
        )
        full_cv <- crossfit_hurdle_increment(
          d, spec$response, base_terms, c("z_H", "z_R", "z_H_R"),
          spec$family, TRUE, k_space
        )
        interaction_cv <- crossfit_hurdle_increment(
          d, spec$response, c(base_terms, "z_H", "z_R"), "z_H_R",
          spec$family, TRUE, k_space
        )
        full_cv$metrics$outcome <- outcome_name
        full_cv$metrics$region <- region_name
        full_cv$metrics$variant <- variant_name
        full_cv$metrics$increment <- "H_R_block"
        interaction_cv$metrics$outcome <- outcome_name
        interaction_cv$metrics$region <- region_name
        interaction_cv$metrics$variant <- variant_name
        interaction_cv$metrics$increment <- "H_by_R_only"
        heldout[[length(heldout) + 1L]] <- full_cv$metrics
        heldout[[length(heldout) + 1L]] <- interaction_cv$metrics
        full_cv$log$outcome <- outcome_name
        full_cv$log$region <- region_name
        full_cv$log$variant <- variant_name
        full_cv$log$increment <- "H_R_block"
        interaction_cv$log$outcome <- outcome_name
        interaction_cv$log$region <- region_name
        interaction_cv$log$variant <- variant_name
        interaction_cv$log$increment <- "H_by_R_only"
        logs[[length(logs) + 1L]] <- full_cv$log
        logs[[length(logs) + 1L]] <- interaction_cv$log
      }
    }
  }
  list(
    coefficients = do.call(rbind, coefficients),
    heldout = do.call(rbind, heldout),
    log = do.call(rbind, logs)
  )
}

fit_hurdle_rule_sensitivity <- function(data, env_terms, k_space = 40) {
  rules <- list(
    mixture_primary = list(
      presence = "pigmented_mixture50", intensity = "pigment_intensity_z"
    ),
    mixture_high_confidence = list(
      presence = "pigmented_high_confidence",
      intensity = "pigment_intensity_high_confidence_z"
    ),
    a_greater_than_zero = list(
      presence = "pigmented_zero_rule", intensity = "pigment_intensity_zero_z"
    ),
    joint_Lab_mixture = list(
      presence = "pigmented_joint_lab50", intensity = "pigment_intensity_joint_lab_z"
    )
  )
  proxies <- intersect(c("Bombus_W", "Bombus_A"), names(data))
  fits <- list()
  for (rule_name in names(rules)) {
    for (role in names(rules[[rule_name]])) {
      response <- rules[[rule_name]][[role]]
      family <- if (role == "presence") "binomial" else "gaussian"
      for (proxy in proxies) {
        d <- data
        if (proxy == "Bombus_A") {
          d <- droplevels(
            d[d$region == "East" & is.finite(d$Bombus_A), , drop = FALSE]
          )
        }
        fit <- fit_hurdle_evidence_ladder(
          d, response, proxy, env_terms, family, k_space
        )
        fit$coefficients$classification_rule <- rule_name
        fit$coefficients$outcome_role <- role
        fit$heldout$classification_rule <- rule_name
        fit$heldout$outcome_role <- role
        fit$log$classification_rule <- rule_name
        fit$log$outcome_role <- role
        fits[[paste(rule_name, role, proxy, sep = "__")]] <- fit
      }
    }
  }
  bind_part <- function(part) do.call(rbind, lapply(fits, `[[`, part))
  list(
    coefficients = bind_part("coefficients"),
    heldout = bind_part("heldout"),
    log = bind_part("log")
  )
}

fit_primary_hurdle_bombus <- function(data, env_terms, k_space = 40) {
  bombus_proxies <- c(
    "Bombus_W", "Bombus_A", "bee_beaticola_ns", "bee_consobrinus_ns",
    "bee_honshuensis_ns", "Bombus_W_occ_100km",
    "Bombus_A_occ_density_100km", "Bombus_total_occ_density_100km"
  )
  bombus_proxies <- bombus_proxies[bombus_proxies %in% names(data)]
  presence <- intensity <- list()
  for (proxy in bombus_proxies) {
    proxy_data <- data
    if (proxy == "Bombus_A") {
      proxy_data <- droplevels(
        data[data$region == "East" & is.finite(data$Bombus_A), , drop = FALSE]
      )
    }
    presence[[proxy]] <- fit_hurdle_evidence_ladder(
      proxy_data, "pigmented_mixture50", proxy, env_terms, "binomial", k_space
    )
    intensity[[proxy]] <- fit_hurdle_evidence_ladder(
      proxy_data, "pigment_intensity_z", proxy, env_terms, "gaussian", k_space
    )
  }
  list(presence = presence, intensity = intensity)
}

fit_hurdle_collinearity_audit <- function(data, env_terms) {
  data$z_H_R <- data$z_H * data$z_R
  pigmented <- is.finite(data$pigment_intensity_z)
  alpine <- data$region == "East" & is.finite(data$Bombus_A)
  west <- data$region == "West"
  east <- data$region == "East"
  designs <- list(
    presence_national_W = list(
      subset = rep(TRUE, nrow(data)), terms = c(env_terms, "Bombus_W")
    ),
    presence_alpine_W_A = list(
      subset = alpine, terms = c(env_terms, "Bombus_W", "Bombus_A")
    ),
    intensity_national_W = list(
      subset = pigmented, terms = c(env_terms, "Bombus_W")
    ),
    intensity_alpine_W_A = list(
      subset = pigmented & alpine,
      terms = c(env_terms, "Bombus_W", "Bombus_A")
    ),
    presence_horticulture_West = list(
      subset = west,
      terms = c(env_terms, "Bombus_W", "z_H", "z_R", "z_H_R")
    ),
    presence_horticulture_East = list(
      subset = east,
      terms = c(env_terms, "Bombus_W", "z_H", "z_R", "z_H_R")
    ),
    intensity_horticulture_West = list(
      subset = pigmented & west,
      terms = c(env_terms, "Bombus_W", "z_H", "z_R", "z_H_R")
    ),
    intensity_horticulture_East = list(
      subset = pigmented & east,
      terms = c(env_terms, "Bombus_W", "z_H", "z_R", "z_H_R")
    )
  )
  audits <- lapply(names(designs), function(name) {
    spec <- designs[[name]]
    numeric_collinearity_audit(
      data[spec$subset, , drop = FALSE], spec$terms, design = name
    )
  })
  bind <- function(part) {
    rows <- lapply(audits, `[[`, part)
    rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
    if (length(rows)) do.call(rbind, rows) else data.frame()
  }
  list(
    vif = bind("vif"), correlations = bind("correlations"),
    condition = bind("condition")
  )
}

fit_hurdle_inla_model_set <- function(data, response, env_terms,
                                      family = c("gaussian", "binomial"),
                                      outcome_label = response) {
  require_packages("INLA")
  family <- match.arg(family)
  fit_shared_set <- function(d, terms_list, comparison_set) {
    required <- unique(c(response, unlist(terms_list), "x_km", "y_km"))
    d <- d[stats::complete.cases(d[required]), , drop = FALSE]
    coords <- as.matrix(d[c("x_km", "y_km")])
    mesh <- INLA::inla.mesh.2d(loc = coords, max.edge = c(20, 100), cutoff = 5)
    spde <- INLA::inla.spde2.pcmatern(
      mesh, alpha = 2, prior.range = c(100, 0.05), prior.sigma = c(1, 0.05)
    )
    out <- lapply(names(terms_list), function(name) {
      message("[hurdle INLA] fitting ", outcome_label, " / ", name)
      result <- fit_one_inla_spde(
        d, response, terms_list[[name]], paste0(outcome_label, "_", name),
        mesh, spde, family
      )
      result$metrics$comparison_set <- comparison_set
      result
    })
    names(out) <- names(terms_list)
    out
  }
  national_terms <- list(
    N_environment_space = c("region", env_terms),
    N_W_space = c("region", env_terms, "Bombus_W")
  )
  fits <- fit_shared_set(data, national_terms, paste0(outcome_label, "_national"))
  alpine <- data[is.finite(data$Bombus_A), , drop = FALSE]
  region_n <- table(alpine$region)
  if (nrow(alpine) >= 100L && any(region_n < 20L)) {
    alpine <- alpine[alpine$region == names(which.max(region_n)), , drop = FALSE]
  }
  alpine$region <- droplevels(alpine$region)
  alpine_base <- if (nlevels(alpine$region) > 1L) c("region", env_terms) else env_terms
  alpine_terms <- list(
    A_environment_space = alpine_base,
    A_W_space = c(alpine_base, "Bombus_W"),
    A_montane_space = c(alpine_base, "Bombus_A"),
    A_W_montane_space = c(alpine_base, "Bombus_W", "Bombus_A")
  )
  if (nrow(alpine) >= 100L) {
    fits <- c(
      fits,
      fit_shared_set(alpine, alpine_terms, paste0(outcome_label, "_alpine_support"))
    )
  }
  metrics <- do.call(rbind, lapply(fits, `[[`, "metrics"))
  metrics$delta_WAIC_within_set <- ave(
    metrics$WAIC, metrics$comparison_set, FUN = function(x) x - x[1]
  )
  list(
    metrics = metrics,
    fixed = do.call(rbind, lapply(fits, `[[`, "fixed")),
    hyper = do.call(rbind, lapply(fits, `[[`, "hyper")),
    fits = fits
  )
}

run_pigmentation_hurdle_v4 <- function(analysis_csv, output_dir,
                                       run_inla = TRUE, k_space = 40,
                                       confidence_cut = 0.8) {
  require_packages(c("mclust", "mgcv", "jsonlite"))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  data <- utils::read.csv(
    analysis_csv, check.names = FALSE, stringsAsFactors = FALSE
  )
  required <- c(
    "observation_id", "colour_a", "colour_L", "colour_C", "region",
    "spatial_fold", "x_km", "y_km", "Bombus_W", "Bombus_A", "z_H", "z_R"
  )
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Missing hurdle inputs: ", paste(missing, collapse = ", "))
  data$region <- factor(data$region, levels = c("West", "East"))
  environment_terms <- grep("^env_", names(data), value = TRUE)

  measurement <- fit_pigmentation_measurement(
    data$colour_a, data$colour_L, data$colour_C,
    confidence_cut = confidence_cut
  )
  data <- cbind(data, measurement$observations)
  data$R_primary_mesh_boundary <- primary_mesh_boundary_flag(
    data$longitude, data$latitude
  )
  write_csv_safe(measurement$summary, file.path(output_dir, "pigmentation_measurement_summary.csv"))
  write_csv_safe(measurement$components, file.path(output_dir, "pigmentation_mixture_components.csv"))
  write_csv_safe(
    measurement$joint_components,
    file.path(output_dir, "pigmentation_joint_lab_components.csv")
  )
  measurement_columns <- c(
    "observation_id", "colour_a", "colour_L", "colour_b", "colour_C",
    names(measurement$observations), "region", "spatial_fold"
  )
  write_csv_safe(
    data[measurement_columns],
    file.path(output_dir, "pigmentation_measurement_observations.csv")
  )
  sensitivity <- do.call(rbind, lapply(levels(data$region), function(region_name) {
    idx <- data$region == region_name
    high_confidence_idx <- idx & !is.na(data$pigmented_high_confidence)
    data.frame(
      region = region_name,
      rule = c(
        "a_mixture_0.5", "a_mixture_high_confidence",
        "a_greater_than_zero", "joint_Lab_mixture_0.5"
      ),
      n = c(sum(idx), sum(high_confidence_idx), sum(idx), sum(idx)),
      pigmented_n = c(
        sum(data$pigmented_mixture50[idx]),
        sum(data$pigmented_high_confidence[high_confidence_idx]),
        sum(data$pigmented_zero_rule[idx]),
        sum(data$pigmented_joint_lab50[idx], na.rm = TRUE)
      ),
      pigmented_fraction = c(
        mean(data$pigmented_mixture50[idx]),
        mean(data$pigmented_high_confidence[high_confidence_idx]),
        mean(data$pigmented_zero_rule[idx]),
        mean(data$pigmented_joint_lab50[idx], na.rm = TRUE)
      ),
      stringsAsFactors = FALSE
    )
  }))
  write_csv_safe(sensitivity, file.path(output_dir, "pigmentation_classification_sensitivity.csv"))

  bombus <- fit_primary_hurdle_bombus(data, environment_terms, k_space)
  presence_fits <- bombus$presence
  intensity_fits <- bombus$intensity
  bind_part <- function(fits, part) do.call(rbind, lapply(fits, `[[`, part))
  write_csv_safe(
    bind_part(presence_fits, "coefficients"),
    file.path(output_dir, "pigmentation_presence_bombus_coefficients.csv")
  )
  write_csv_safe(
    bind_part(presence_fits, "heldout"),
    file.path(output_dir, "pigmentation_presence_bombus_heldout.csv")
  )
  write_csv_safe(
    bind_part(presence_fits, "log"),
    file.path(output_dir, "pigmentation_presence_bombus_crossfit_log.csv")
  )
  write_csv_safe(
    bind_part(intensity_fits, "coefficients"),
    file.path(output_dir, "pigmented_intensity_bombus_coefficients.csv")
  )
  write_csv_safe(
    bind_part(intensity_fits, "heldout"),
    file.path(output_dir, "pigmented_intensity_bombus_heldout.csv")
  )
  write_csv_safe(
    bind_part(intensity_fits, "log"),
    file.path(output_dir, "pigmented_intensity_bombus_crossfit_log.csv")
  )

  rule_sensitivity <- fit_hurdle_rule_sensitivity(
    data, environment_terms, k_space
  )
  write_csv_safe(
    rule_sensitivity$coefficients,
    file.path(output_dir, "pigmentation_rule_sensitivity_coefficients.csv")
  )
  write_csv_safe(
    rule_sensitivity$heldout,
    file.path(output_dir, "pigmentation_rule_sensitivity_heldout.csv")
  )
  write_csv_safe(
    rule_sensitivity$log,
    file.path(output_dir, "pigmentation_rule_sensitivity_crossfit_log.csv")
  )

  collinearity <- fit_hurdle_collinearity_audit(data, environment_terms)
  write_csv_safe(
    collinearity$vif,
    file.path(output_dir, "pigmentation_hurdle_collinearity_vif.csv")
  )
  write_csv_safe(
    collinearity$correlations,
    file.path(output_dir, "pigmentation_hurdle_collinearity_correlations.csv")
  )
  write_csv_safe(
    collinearity$condition,
    file.path(output_dir, "pigmentation_hurdle_collinearity_condition.csv")
  )

  interface <- fit_hurdle_interface(data, environment_terms, k_space)
  write_csv_safe(
    interface$coefficients,
    file.path(output_dir, "pigmentation_hurdle_HR_coefficients.csv")
  )
  write_csv_safe(
    interface$heldout,
    file.path(output_dir, "pigmentation_hurdle_HR_heldout.csv")
  )
  write_csv_safe(
    interface$log,
    file.path(output_dir, "pigmentation_hurdle_HR_crossfit_log.csv")
  )

  natural_residuals <- build_hurdle_natural_residuals(
    data, environment_terms, k_space
  )
  data <- natural_residuals$data
  write_csv_safe(
    natural_residuals$log,
    file.path(output_dir, "pigmentation_natural_residual_crossfit_log.csv")
  )
  residual_tail <- fit_hurdle_residual_tail(data, k_space)
  write_csv_safe(
    residual_tail$coefficients,
    file.path(output_dir, "pigmentation_residual_tail_HR_coefficients.csv")
  )
  write_csv_safe(
    residual_tail$heldout,
    file.path(output_dir, "pigmentation_residual_tail_HR_heldout.csv")
  )
  write_csv_safe(
    residual_tail$log,
    file.path(output_dir, "pigmentation_residual_tail_HR_crossfit_log.csv")
  )
  write_csv_safe(
    residual_tail$early_enrichment,
    file.path(output_dir, "pigmentation_residual_tail_early_enrichment.csv")
  )
  write_csv_safe(
    residual_tail$candidates,
    file.path(output_dir, "pigmentation_residual_tail_candidates.csv")
  )

  inla <- NULL
  if (run_inla) {
    presence_inla <- fit_hurdle_inla_model_set(
      data, "pigmented_mixture50", environment_terms, "binomial", "presence"
    )
    intensity_inla <- fit_hurdle_inla_model_set(
      data, "pigment_intensity_z", environment_terms, "gaussian", "intensity"
    )
    inla <- list(presence = presence_inla, intensity = intensity_inla)
    write_csv_safe(
      rbind(presence_inla$metrics, intensity_inla$metrics),
      file.path(output_dir, "pigmentation_hurdle_inla_model_comparison.csv")
    )
    write_csv_safe(
      rbind(presence_inla$fixed, intensity_inla$fixed),
      file.path(output_dir, "pigmentation_hurdle_inla_fixed_effects.csv")
    )
    write_csv_safe(
      rbind(presence_inla$hyper, intensity_inla$hyper),
      file.path(output_dir, "pigmentation_hurdle_inla_hyperparameters.csv")
    )
  }

  write_csv_safe(data, file.path(output_dir, "analysis_data_pigmentation_hurdle.csv"))
  run_summary <- list(
    pipeline = "pigmentation_hurdle_v4",
    analysis_data = file.path(basename(dirname(analysis_csv)), basename(analysis_csv)),
    analysis_data_md5 = unname(tools::md5sum(analysis_csv)),
    n = nrow(data),
    n_white = sum(data$pigmented_mixture50 == 0L),
    n_pigmented = sum(data$pigmented_mixture50 == 1L),
    n_ambiguous = sum(is.na(data$pigmented_high_confidence)),
    decision_boundary_a = measurement$summary$decision_boundary_a,
    response_roles = list(
      all_flower_colour_a = "descriptive visible-colour geography only",
      pigmentation_presence = "primary Bombus pigmentation-expression outcome",
      pigmented_intensity = "colour intensity conditional on pigmented flowers; primary horticultural outcome"
    ),
    white_flower_rule = paste(
      "white-flower a* variation is measurement variation, not anthocyanin amount"
    ),
    inla_run = run_inla,
    claim_ceiling = paste(
      "optical pigmentation class and conditional visible intensity;",
      "not chemical anthocyanin concentration"
    )
  )
  jsonlite::write_json(
    run_summary, file.path(output_dir, "run_summary.json"),
    pretty = TRUE, auto_unbox = TRUE, na = "null"
  )
  list(
    data = data, measurement = measurement, rule_sensitivity = rule_sensitivity,
    collinearity = collinearity, interface = interface,
    natural_residuals = natural_residuals, residual_tail = residual_tail,
    inla = inla
  )
}
