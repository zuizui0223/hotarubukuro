v23_analysis_spec_version <- "v23.0_continuous_colour_isolation_human_context"

v23_require_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

v23_distance_matrix <- function(cells) {
  v23_require_columns(cells, c("x_km", "y_km"), "cells")
  coordinates <- as.matrix(cells[, c("x_km", "y_km"), drop = FALSE])
  storage.mode(coordinates) <- "double"
  distance <- as.matrix(stats::dist(coordinates))
  diag(distance) <- Inf
  distance
}

v23_neighbour_order <- function(distance) {
  distance <- as.matrix(distance)
  if (nrow(distance) != ncol(distance)) {
    stop("distance must be square.", call. = FALSE)
  }
  n <- nrow(distance)
  ordered <- t(vapply(
    seq_len(n),
    function(index) order(distance[index, ], method = "radix"),
    integer(n)
  ))
  ordered[, seq_len(max(n - 1L, 1L)), drop = FALSE]
}

v23_nearest_same_matrix <- function(state, distance, neighbour_order = NULL) {
  if (is.vector(state)) state <- matrix(as.logical(state), ncol = 1L)
  state <- as.matrix(state)
  storage.mode(state) <- "logical"
  distance <- as.matrix(distance)
  if (nrow(state) != nrow(distance) || nrow(distance) != ncol(distance)) {
    stop("state rows and the square distance matrix must align.", call. = FALSE)
  }
  if (is.null(neighbour_order)) {
    neighbour_order <- v23_neighbour_order(distance)
  }
  n <- nrow(state)
  n_draws <- ncol(state)
  out <- matrix(NA_real_, nrow = n, ncol = n_draws)
  unresolved <- matrix(TRUE, nrow = n, ncol = n_draws)
  for (rank_index in seq_len(ncol(neighbour_order))) {
    neighbour_index <- neighbour_order[, rank_index]
    neighbour_state <- state[neighbour_index, , drop = FALSE]
    matched <- unresolved & (neighbour_state == state)
    if (any(matched)) {
      distance_vector <- distance[cbind(seq_len(n), neighbour_index)]
      distance_block <- matrix(
        distance_vector, nrow = n, ncol = n_draws
      )
      out[matched] <- distance_block[matched]
      unresolved[matched] <- FALSE
    }
    if (!any(unresolved)) break
  }
  out
}

v23_any_colour_distances <- function(distance) {
  distance <- as.matrix(distance)
  sorted_distance <- t(apply(distance, 1L, sort, na.last = TRUE))
  list(
    any_colour_nn_km = sorted_distance[, 1L],
    any_colour_5nn_km = sorted_distance[, min(5L, ncol(sorted_distance))],
    any_colour_10nn_km = sorted_distance[, min(10L, ncol(sorted_distance))]
  )
}

v23_isolation_metrics <- function(
    state, distance, neighbour_order = NULL,
    any_colour_distances = NULL) {
  if (is.vector(state)) state <- matrix(as.logical(state), ncol = 1L)
  state <- as.matrix(state)
  same <- v23_nearest_same_matrix(state, distance, neighbour_order)
  if (is.null(any_colour_distances)) {
    any_colour_distances <- v23_any_colour_distances(distance)
  }
  any_nearest <- any_colour_distances$any_colour_nn_km
  any_fifth <- any_colour_distances$any_colour_5nn_km
  list(
    same_colour_nn_km = same,
    any_colour_nn_km = any_nearest,
    any_colour_5nn_km = any_fifth,
    any_colour_10nn_km = any_colour_distances$any_colour_10nn_km,
    relative_isolation_nn = log(sweep(same, 1L, any_nearest, "/")),
    relative_isolation_5nn = log(sweep(same, 1L, any_fifth, "/"))
  )
}

v23_spearman_rho <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 3L || length(unique(x[keep])) < 2L ||
      length(unique(y[keep])) < 2L) {
    return(NA_real_)
  }
  suppressWarnings(stats::cor(x[keep], y[keep], method = "spearman"))
}

v23_safe_spearman <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 3L || length(unique(x[keep])) < 2L ||
      length(unique(y[keep])) < 2L) {
    return(c(n = sum(keep), rho = NA_real_, asymptotic_p = NA_real_))
  }
  test <- suppressWarnings(stats::cor.test(
    x[keep], y[keep], method = "spearman", exact = FALSE
  ))
  c(n = sum(keep), rho = unname(test$estimate), asymptotic_p = test$p.value)
}

v23_permute_within_strata <- function(value, strata) {
  out <- value
  groups <- split(seq_along(value), as.character(strata), drop = TRUE)
  for (indices in groups) {
    if (length(indices) > 1L) out[indices] <- sample(value[indices])
  }
  out
}

v23_feature_permutation_test <- function(
    isolation, feature, strata, permutations = 2000L,
    seed = 20260725L) {
  observed <- v23_safe_spearman(isolation, feature)[["rho"]]
  set.seed(seed)
  null <- vapply(seq_len(permutations), function(iteration) {
    permuted <- v23_permute_within_strata(feature, strata)
    v23_safe_spearman(isolation, permuted)[["rho"]]
  }, numeric(1))
  null <- null[is.finite(null)]
  p <- if (is.finite(observed) && length(null)) {
    (1 + sum(abs(null) >= abs(observed))) / (length(null) + 1)
  } else {
    NA_real_
  }
  list(observed = observed, null = null, two_sided_p = p)
}

v23_shuffle_state_within_fold <- function(state, fold) {
  out <- as.logical(state)
  groups <- split(seq_along(out), as.character(fold), drop = TRUE)
  for (indices in groups) {
    out[indices] <- sample(out[indices], length(indices), replace = FALSE)
  }
  out
}

v23_colour_rho_difference <- function(isolation, state, feature) {
  pigmented <- v23_safe_spearman(isolation[state], feature[state])[["rho"]]
  white <- v23_safe_spearman(isolation[!state], feature[!state])[["rho"]]
  c(pigmented_rho = pigmented, white_rho = white,
    rho_difference = pigmented - white)
}

v23_fold_label_null <- function(
    state, feature, distance, neighbour_order, fold,
    permutations = 2000L, seed = 20260725L,
    metric = c("raw", "relative_nn")) {
  metric <- match.arg(metric)
  any_colour_distances <- v23_any_colour_distances(distance)
  observed_metrics <- v23_isolation_metrics(
    state, distance, neighbour_order, any_colour_distances
  )
  observed_isolation <- if (metric == "raw") {
    observed_metrics$same_colour_nn_km[, 1L]
  } else {
    observed_metrics$relative_isolation_nn[, 1L]
  }
  observed <- v23_colour_rho_difference(
    observed_isolation, state, feature
  )
  set.seed(seed)
  null <- matrix(
    NA_real_, nrow = permutations, ncol = 4L,
    dimnames = list(NULL, c(
      "pigmented_count", "pigmented_rho", "white_rho", "rho_difference"
    ))
  )
  for (iteration in seq_len(permutations)) {
    permuted_state <- v23_shuffle_state_within_fold(state, fold)
    permuted_metrics <- v23_isolation_metrics(
      permuted_state, distance, neighbour_order, any_colour_distances
    )
    permuted_isolation <- if (metric == "raw") {
      permuted_metrics$same_colour_nn_km[, 1L]
    } else {
      permuted_metrics$relative_isolation_nn[, 1L]
    }
    contrast <- v23_colour_rho_difference(
      permuted_isolation, permuted_state, feature
    )
    null[iteration, ] <- c(
      sum(permuted_state), contrast[["pigmented_rho"]],
      contrast[["white_rho"]], contrast[["rho_difference"]]
    )
  }
  valid <- is.finite(null[, "rho_difference"])
  p <- if (is.finite(observed[["rho_difference"]]) && any(valid)) {
    (1 + sum(
      null[valid, "rho_difference"] >= observed[["rho_difference"]]
    )) / (sum(valid) + 1)
  } else {
    NA_real_
  }
  list(observed = observed, null = null, upper_p = p)
}

v23_top_fraction_summary <- function(
    isolation, state, features, feature_names, fraction = 0.10) {
  rows <- list()
  for (colour in c("pigmented", "white")) {
    selected_colour <- if (colour == "pigmented") state else !state
    indices <- which(selected_colour & is.finite(isolation))
    ordering <- indices[order(
      -isolation[indices], as.character(features$exact_site_id[indices])
    )]
    n_top <- max(1L, ceiling(fraction * length(ordering)))
    top <- head(ordering, n_top)
    remainder <- setdiff(indices, top)
    for (feature in feature_names) {
      top_value <- as.numeric(features[[feature]][top])
      rest_value <- as.numeric(features[[feature]][remainder])
      rows[[length(rows) + 1L]] <- data.frame(
        colour = colour,
        feature = feature,
        fraction = fraction,
        n_total = length(indices),
        n_top = length(top),
        minimum_top_isolation_km = min(isolation[top]),
        top_mean = mean(top_value[is.finite(top_value)]),
        remainder_mean = mean(rest_value[is.finite(rest_value)]),
        top_minus_remainder =
          mean(top_value[is.finite(top_value)]) -
          mean(rest_value[is.finite(rest_value)]),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

v23_leave_one_fold_out <- function(
    isolation, state, features, feature_names, fold) {
  rows <- list()
  for (excluded in sort(unique(fold))) {
    keep <- fold != excluded
    for (feature in feature_names) {
      contrast <- v23_colour_rho_difference(
        isolation[keep], state[keep], as.numeric(features[[feature]][keep])
      )
      rows[[length(rows) + 1L]] <- data.frame(
        excluded_fold = excluded,
        feature = feature,
        n_retained = sum(keep),
        pigmented_rho = contrast[["pigmented_rho"]],
        white_rho = contrast[["white_rho"]],
        rho_difference = contrast[["rho_difference"]],
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

v23_null_comparison <- function(observed, simulated,
                                alternative = c("greater", "less")) {
  alternative <- match.arg(alternative)
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
  p <- if (alternative == "greater") p_upper else p_lower
  c(
    null_mean = mean(simulated),
    null_sd = stats::sd(simulated),
    lower_95 = unname(stats::quantile(simulated, 0.025)),
    upper_95 = unname(stats::quantile(simulated, 0.975)),
    empirical_p = p,
    empirical_two_sided_p = min(1, 2 * min(p_upper, p_lower)),
    percentile = mean(simulated <= observed),
    monte_carlo_se = sqrt(p * (1 - p) / (length(simulated) + 1))
  )
}

v23_maxT_upper_p <- function(observed, null_matrix) {
  observed <- as.numeric(observed)
  null_matrix <- as.matrix(null_matrix)
  keep <- is.finite(observed) &
    colSums(is.finite(null_matrix)) >= max(20L, floor(0.95 * nrow(null_matrix)))
  observed <- observed[keep]
  null_matrix <- null_matrix[, keep, drop = FALSE]
  if (!length(observed) || !nrow(null_matrix)) return(NA_real_)
  complete <- stats::complete.cases(null_matrix)
  null_matrix <- null_matrix[complete, , drop = FALSE]
  if (!nrow(null_matrix)) return(NA_real_)
  center <- colMeans(null_matrix)
  spread <- apply(null_matrix, 2L, stats::sd)
  spread[!is.finite(spread) | spread <= 1e-12] <- NA_real_
  valid <- is.finite(spread)
  if (!any(valid)) return(NA_real_)
  observed_z <- (observed[valid] - center[valid]) / spread[valid]
  null_z <- sweep(
    sweep(null_matrix[, valid, drop = FALSE], 2L, center[valid], "-"),
    2L, spread[valid], "/"
  )
  observed_max <- max(observed_z)
  null_max <- apply(null_z, 1L, max)
  (1 + sum(null_max >= observed_max)) / (length(null_max) + 1)
}
