v22_analysis_spec_version <- "v22.1_count_conditioned_departure_placement_null"

v22_require_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

v22_candidate_feature_statistic <- function(
    candidate, features, feature_names, statistic = c("mean", "median"),
    analysis_support = NULL) {
  statistic <- match.arg(statistic)
  if (is.vector(candidate)) candidate <- matrix(candidate, ncol = 1L)
  candidate <- as.matrix(candidate)
  feature_matrix <- as.matrix(features[, feature_names, drop = FALSE])
  storage.mode(feature_matrix) <- "double"
  if (nrow(candidate) != nrow(feature_matrix)) {
    stop("candidate and features must have the same number of rows.",
         call. = FALSE)
  }
  if (is.null(analysis_support)) {
    analysis_support <- stats::complete.cases(feature_matrix)
  }
  analysis_support <- as.logical(analysis_support)
  if (length(analysis_support) != nrow(candidate)) {
    stop("analysis_support must have one value per cell.", call. = FALSE)
  }

  value <- matrix(
    NA_real_, nrow = ncol(candidate), ncol = length(feature_names),
    dimnames = list(NULL, feature_names)
  )
  n_candidates <- integer(ncol(candidate))
  candidate_indices <- vector("list", ncol(candidate))
  for (draw in seq_len(ncol(candidate))) {
    index <- which(candidate[, draw] & analysis_support)
    n_candidates[draw] <- length(index)
    candidate_indices[[draw]] <- index
    if (!length(index)) next
    block <- feature_matrix[index, , drop = FALSE]
    value[draw, ] <- if (statistic == "mean") {
      colMeans(block, na.rm = TRUE)
    } else {
      apply(block, 2, stats::median, na.rm = TRUE)
    }
  }
  list(
    statistic = value,
    n_candidates = n_candidates,
    candidate_indices = candidate_indices,
    analysis_support = analysis_support,
    aggregation = statistic
  )
}

v22_count_conditioned_draws <- function(
    simulated_counts, observed_count, minimum_exact_draws = 200L,
    fallback_draws = 1000L) {
  simulated_counts <- as.integer(simulated_counts)
  observed_count <- as.integer(observed_count)
  exact <- which(simulated_counts == observed_count)
  if (length(exact) >= minimum_exact_draws) {
    selected <- exact
    mode <- "exact_candidate_count"
    maximum_absolute_count_difference <- 0L
  } else {
    usable <- which(is.finite(simulated_counts) & simulated_counts > 0L)
    ordering <- order(
      abs(simulated_counts[usable] - observed_count),
      usable
    )
    selected <- usable[head(ordering, min(fallback_draws, length(ordering)))]
    mode <- "nearest_candidate_count_fallback"
    maximum_absolute_count_difference <- if (length(selected)) {
      max(abs(simulated_counts[selected] - observed_count))
    } else NA_integer_
  }
  list(
    selected_draws = selected,
    exact_draws = exact,
    observed_count = observed_count,
    n_selected_draws = length(selected),
    n_exact_draws = length(exact),
    mode = mode,
    minimum_exact_draws = as.integer(minimum_exact_draws),
    fallback_draws = as.integer(fallback_draws),
    maximum_absolute_count_difference = maximum_absolute_count_difference
  )
}

v22_empirical_comparison <- function(
    observed, simulated, direction = c("greater", "less", "two_sided")) {
  direction <- match.arg(direction)
  simulated <- as.numeric(simulated)
  simulated <- simulated[is.finite(simulated)]
  if (!is.finite(observed) || !length(simulated)) {
    return(c(
      null_mean = NA_real_, null_sd = NA_real_, lower_95 = NA_real_,
      upper_95 = NA_real_, p_value = NA_real_, two_sided_p = NA_real_,
      percentile = NA_real_, monte_carlo_se = NA_real_, n_null = 0
    ))
  }
  p_upper <- (1 + sum(simulated >= observed)) / (length(simulated) + 1)
  p_lower <- (1 + sum(simulated <= observed)) / (length(simulated) + 1)
  p_two <- min(1, 2 * min(p_upper, p_lower))
  p <- switch(
    direction,
    greater = p_upper,
    less = p_lower,
    two_sided = p_two
  )
  c(
    null_mean = mean(simulated),
    null_sd = stats::sd(simulated),
    lower_95 = unname(stats::quantile(simulated, 0.025)),
    upper_95 = unname(stats::quantile(simulated, 0.975)),
    p_value = p,
    two_sided_p = p_two,
    percentile = mean(simulated <= observed),
    monte_carlo_se = sqrt(p * (1 - p) / (length(simulated) + 1)),
    n_null = length(simulated)
  )
}

v22_placement_summary <- function(
    observed, simulated, definitions, selected_draws,
    conditioning, aggregation = "mean") {
  v22_require_columns(
    definitions,
    c("feature", "role", "hypothesis_direction", "family"),
    "definitions"
  )
  feature_names <- as.character(definitions$feature)
  simulated <- as.matrix(simulated[, feature_names, drop = FALSE])
  selected_draws <- as.integer(selected_draws)
  selected_draws <- selected_draws[
    selected_draws >= 1L & selected_draws <= nrow(simulated)
  ]
  if (!length(selected_draws)) {
    stop("No null maps are available for placement inference.", call. = FALSE)
  }
  null <- simulated[selected_draws, , drop = FALSE]
  observed <- as.numeric(observed[feature_names])
  names(observed) <- feature_names

  center <- colMeans(null, na.rm = TRUE)
  spread <- apply(null, 2, stats::sd, na.rm = TRUE)
  spread[!is.finite(spread) | spread <= 1e-12] <- 1
  null_z <- sweep(sweep(null, 2, center, "-"), 2, spread, "/")
  observed_z <- (observed - center) / spread

  rows <- lapply(seq_len(nrow(definitions)), function(index) {
    feature <- feature_names[index]
    comparison <- v22_empirical_comparison(
      observed[index], null[, feature],
      as.character(definitions$hypothesis_direction[index])
    )
    data.frame(
      conditioning = conditioning,
      aggregation = aggregation,
      feature = feature,
      role = as.character(definitions$role[index]),
      family = as.character(definitions$family[index]),
      hypothesis_direction =
        as.character(definitions$hypothesis_direction[index]),
      observed_candidate_exposure = observed[index],
      observed_standardized_departure = observed_z[index],
      t(comparison),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$maxT_FWER_p <- NA_real_
  out$family_omnibus_p <- NA_real_

  for (family in unique(as.character(definitions$family))) {
    index <- which(as.character(definitions$family) == family)
    transformed_null <- matrix(
      NA_real_, nrow = nrow(null_z), ncol = length(index)
    )
    transformed_observed <- numeric(length(index))
    for (column in seq_along(index)) {
      original <- index[column]
      direction <- as.character(
        definitions$hypothesis_direction[original]
      )
      if (direction == "less") {
        transformed_null[, column] <- -null_z[, original]
        transformed_observed[column] <- -observed_z[original]
      } else if (direction == "two_sided") {
        transformed_null[, column] <- abs(null_z[, original])
        transformed_observed[column] <- abs(observed_z[original])
      } else {
        transformed_null[, column] <- null_z[, original]
        transformed_observed[column] <- observed_z[original]
      }
    }
    finite_max <- function(value) {
      value <- value[is.finite(value)]
      if (length(value)) max(value) else NA_real_
    }
    maximum_null <- apply(transformed_null, 1, finite_max)
    for (column in seq_along(index)) {
      out$maxT_FWER_p[index[column]] <-
        (1 + sum(maximum_null >= transformed_observed[column], na.rm = TRUE)) /
        (sum(is.finite(maximum_null)) + 1)
    }
    observed_maximum <- max(transformed_observed, na.rm = TRUE)
    omnibus <-
      (1 + sum(maximum_null >= observed_maximum, na.rm = TRUE)) /
      (sum(is.finite(maximum_null)) + 1)
    out$family_omnibus_p[index] <- omnibus
  }
  out
}

v22_count_exposure_diagnostic <- function(
    simulated_counts, simulated_statistics, feature_names) {
  simulated_statistics <- as.matrix(
    simulated_statistics[, feature_names, drop = FALSE]
  )
  rows <- lapply(feature_names, function(feature) {
    keep <- is.finite(simulated_counts) &
      is.finite(simulated_statistics[, feature])
    data.frame(
      feature = feature,
      n_maps = sum(keep),
      pearson_count_exposure_correlation = if (sum(keep) >= 3L) {
        stats::cor(
          simulated_counts[keep], simulated_statistics[keep, feature],
          method = "pearson"
        )
      } else NA_real_,
      spearman_count_exposure_correlation = if (sum(keep) >= 3L) {
        stats::cor(
          simulated_counts[keep], simulated_statistics[keep, feature],
          method = "spearman"
        )
      } else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
