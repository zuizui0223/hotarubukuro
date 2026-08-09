v17_analysis_spec_version <- "v17.1_local_pair_predictive_turnover"

v17_require_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

v17_scale_vector <- function(x) {
  x <- as.numeric(x)
  value <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (!any(keep)) return(value)
  center <- mean(x[keep])
  spread <- stats::sd(x[keep])
  if (!is.finite(spread) || spread <= 1e-12) {
    value[keep] <- 0
  } else {
    value[keep] <- (x[keep] - center) / spread
  }
  value
}

v17_align_result <- function(result, cells, label = "result") {
  index <- match(cells$exact_site_id, result$cell_id)
  if (anyNA(index) || anyDuplicated(index)) {
    stop(label, " cannot be aligned one-to-one with cells.", call. = FALSE)
  }
  list(
    observed = as.numeric(result$observed[index]),
    trials = as.numeric(result$trials[index]),
    draws = result$draws[index, , drop = FALSE],
    latent_mean = as.numeric(result$latent_mean[index])
  )
}

v17_fingerprint_matrix <- function(cells) {
  columns <- c(
    "bombus_total_habitat_support",
    "bombus_composition_pc1",
    "bombus_composition_pc2"
  )
  v17_require_columns(cells, columns, "cells")
  matrix <- as.matrix(cells[, columns, drop = FALSE])
  storage.mode(matrix) <- "double"
  matrix <- apply(matrix, 2, v17_scale_vector)
  if (!is.matrix(matrix)) matrix <- matrix(matrix, ncol = length(columns))
  colnames(matrix) <- columns
  matrix
}

v17_hellinger_matrix <- function(cells) {
  rank_columns <- paste0(
    c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis"),
    "_within_species_rank"
  )
  v17_require_columns(cells, rank_columns, "cells")
  availability <- as.matrix(cells[, rank_columns, drop = FALSE])
  storage.mode(availability) <- "double"
  total <- rowSums(availability)
  relative <- availability / pmax(total, 1e-12)
  relative[!is.finite(relative)] <- NA_real_
  sqrt(relative)
}

v17_environment_matrix <- function(cells) {
  columns <- c(
    "broad50km_pc1", "broad50km_pc2",
    "within50km_pc1", "within50km_pc2"
  )
  v17_require_columns(cells, columns, "cells")
  matrix <- as.matrix(cells[, columns, drop = FALSE])
  storage.mode(matrix) <- "double"
  matrix <- apply(matrix, 2, v17_scale_vector)
  if (!is.matrix(matrix)) matrix <- matrix(matrix, ncol = length(columns))
  colnames(matrix) <- columns
  matrix
}

v17_pair_distance <- function(matrix, i, j) {
  sqrt(rowMeans((matrix[i, , drop = FALSE] - matrix[j, , drop = FALSE])^2))
}

v17_pair_graph <- function(cells, radius_km = 25, k = 5L,
                           same_fold_only = TRUE,
                           common_support_only = TRUE) {
  required <- c(
    "exact_site_id", "x_km", "y_km", "spatial_fold",
    "bombus_fingerprint_common_support"
  )
  v17_require_columns(cells, required, "cells")
  if (!is.finite(radius_km) || radius_km <= 0) {
    stop("radius_km must be positive.", call. = FALSE)
  }
  k <- as.integer(k)
  if (!is.finite(k) || k < 1L) stop("k must be positive.", call. = FALSE)

  coordinates <- as.matrix(cells[, c("x_km", "y_km"), drop = FALSE])
  storage.mode(coordinates) <- "double"
  dx <- outer(coordinates[, 1], coordinates[, 1], "-")
  dy <- outer(coordinates[, 2], coordinates[, 2], "-")
  distance <- sqrt(dx^2 + dy^2)
  diag(distance) <- Inf
  eligible_node <- is.finite(coordinates[, 1]) & is.finite(coordinates[, 2])
  if (common_support_only) {
    eligible_node <- eligible_node &
      as.logical(cells$bombus_fingerprint_common_support)
  }

  directed <- vector("list", nrow(cells))
  for (i in which(eligible_node)) {
    eligible <- eligible_node & is.finite(distance[i, ]) &
      distance[i, ] <= radius_km
    if (same_fold_only) {
      eligible <- eligible &
        as.integer(cells$spatial_fold) == as.integer(cells$spatial_fold[i])
    }
    candidates <- which(eligible)
    if (!length(candidates)) next
    candidates <- candidates[order(distance[i, candidates], candidates)]
    candidates <- head(candidates, k)
    directed[[i]] <- data.frame(
      i = i, j = candidates,
      geographic_distance_km = distance[i, candidates],
      stringsAsFactors = FALSE
    )
  }
  directed <- directed[vapply(
    directed,
    function(value) if (is.null(value)) 0L else nrow(value),
    integer(1)
  ) > 0L]
  if (!length(directed)) return(data.frame())
  edges <- do.call(rbind, directed)
  edges$i_ordered <- pmin(edges$i, edges$j)
  edges$j_ordered <- pmax(edges$i, edges$j)
  edges$key <- paste(edges$i_ordered, edges$j_ordered, sep = "::")
  edges <- edges[order(edges$geographic_distance_km, edges$key), , drop = FALSE]
  edges <- edges[!duplicated(edges$key), , drop = FALSE]
  edges$i <- edges$i_ordered
  edges$j <- edges$j_ordered
  edges <- edges[order(edges$i, edges$j), , drop = FALSE]
  rownames(edges) <- NULL
  edges$edge_id <- paste0(
    "r", radius_km, "__",
    cells$exact_site_id[edges$i], "__", cells$exact_site_id[edges$j]
  )
  edges$radius_km <- radius_km
  edges$site_i <- as.character(cells$exact_site_id[edges$i])
  edges$site_j <- as.character(cells$exact_site_id[edges$j])
  edges$fold_i <- as.integer(cells$spatial_fold[edges$i])
  edges$fold_j <- as.integer(cells$spatial_fold[edges$j])
  edges$both_common_support <- as.logical(
    cells$bombus_fingerprint_common_support[edges$i] &
      cells$bombus_fingerprint_common_support[edges$j]
  )
  edges[, c(
    "edge_id", "radius_km", "i", "j", "site_i", "site_j",
    "fold_i", "fold_j", "geographic_distance_km",
    "both_common_support"
  ), drop = FALSE]
}

v17_add_pair_features <- function(edges, cells, presence, intensity) {
  if (!nrow(edges)) return(edges)
  env <- v17_environment_matrix(cells)
  fingerprint <- v17_fingerprint_matrix(cells)
  hellinger <- v17_hellinger_matrix(cells)
  i <- edges$i
  j <- edges$j

  edges$environmental_distance <- v17_pair_distance(env, i, j)
  edges$fingerprint_turnover <- v17_pair_distance(fingerprint, i, j)
  edges$composition_hellinger_turnover <-
    sqrt(rowSums((hellinger[i, , drop = FALSE] -
                    hellinger[j, , drop = FALSE])^2)) / sqrt(2)
  total <- as.numeric(cells$bombus_total_habitat_support)
  edges$total_support_difference <- abs(total[i] - total[j])
  edges$mean_total_support <- rowMeans(cbind(total[i], total[j]))

  observed_share <- as.numeric(cells$n_pigmented) /
    pmax(as.numeric(cells$n_observations), 1)
  edges$observed_presence_transition <- abs(
    observed_share[i] - observed_share[j]
  )
  p <- presence$latent_mean
  edges$natural_presence_difference <- abs(p[i] - p[j])
  midpoint <- rowMeans(cbind(p[i], p[j]))
  edges$natural_presence_uncertainty <- 4 * midpoint * (1 - midpoint)
  ni <- pmax(as.numeric(cells$n_observations[i]), 1)
  nj <- pmax(as.numeric(cells$n_observations[j]), 1)
  edges$log_presence_pair_effort <- log1p(2 / (1 / ni + 1 / nj))

  intensity_observed <- as.numeric(cells$conditional_intensity_median)
  intensity_support <- is.finite(intensity_observed[i]) &
    is.finite(intensity_observed[j])
  edges$conditional_intensity_pair <- intensity_support
  edges$observed_intensity_transition <- NA_real_
  edges$observed_intensity_transition[intensity_support] <- abs(
    intensity_observed[i[intensity_support]] -
      intensity_observed[j[intensity_support]]
  )
  mu <- intensity$latent_mean
  edges$natural_intensity_difference <- NA_real_
  edges$natural_intensity_difference[intensity_support] <- abs(
    mu[i[intensity_support]] - mu[j[intensity_support]]
  )
  npi <- pmax(as.numeric(cells$n_pigmented[i]), 1)
  npj <- pmax(as.numeric(cells$n_pigmented[j]), 1)
  edges$log_intensity_pair_effort <- NA_real_
  edges$log_intensity_pair_effort[intensity_support] <-
    log1p(2 / (1 / npi[intensity_support] + 1 / npj[intensity_support]))
  edges
}

v17_transition_draws <- function(edges, cells, result,
                                 response = c("presence", "intensity")) {
  response <- match.arg(response)
  i <- edges$i
  j <- edges$j
  if (response == "presence") {
    trials <- pmax(as.numeric(cells$n_observations), 1)
    values <- sweep(result$draws, 1, trials, "/")
  } else {
    values <- result$draws
  }
  abs(values[i, , drop = FALSE] - values[j, , drop = FALSE])
}

v17_fit_partial_statistic <- function(y, baseline, predictor) {
  baseline <- as.matrix(baseline)
  keep <- is.finite(y) & is.finite(predictor) &
    apply(baseline, 1, function(x) all(is.finite(x)))
  y <- as.numeric(y[keep])
  predictor <- as.numeric(predictor[keep])
  baseline <- baseline[keep, , drop = FALSE]
  if (length(y) < ncol(baseline) + 8L || stats::sd(y) <= 1e-12 ||
      stats::sd(predictor) <= 1e-12) {
    return(c(n = length(y), beta = NA_real_, delta_r2 = NA_real_))
  }
  y <- v17_scale_vector(y)
  baseline <- apply(baseline, 2, v17_scale_vector)
  if (!is.matrix(baseline)) baseline <- matrix(baseline, ncol = 1L)
  predictor <- v17_scale_vector(predictor)
  base_x <- cbind(intercept = 1, baseline)
  full_x <- cbind(base_x, predictor = predictor)
  base_fit <- stats::lm.fit(base_x, y)
  full_fit <- stats::lm.fit(full_x, y)
  if (anyNA(full_fit$coefficients)) {
    return(c(n = length(y), beta = NA_real_, delta_r2 = NA_real_))
  }
  tss <- sum((y - mean(y))^2)
  base_r2 <- 1 - sum(base_fit$residuals^2) / tss
  full_r2 <- 1 - sum(full_fit$residuals^2) / tss
  c(
    n = length(y),
    beta = unname(tail(full_fit$coefficients, 1)),
    delta_r2 = full_r2 - base_r2
  )
}

v17_null_comparison <- function(observed, simulated) {
  simulated <- simulated[is.finite(simulated)]
  if (!is.finite(observed) || !length(simulated)) {
    return(c(
      null_mean = NA_real_, null_sd = NA_real_, null_lower_95 = NA_real_,
      null_upper_95 = NA_real_, empirical_p = NA_real_,
      empirical_two_sided_p = NA_real_, percentile = NA_real_,
      monte_carlo_se = NA_real_
    ))
  }
  p_upper <- (1 + sum(simulated >= observed)) / (length(simulated) + 1)
  p_lower <- (1 + sum(simulated <= observed)) / (length(simulated) + 1)
  p_two <- min(1, 2 * min(p_upper, p_lower))
  c(
    null_mean = mean(simulated),
    null_sd = stats::sd(simulated),
    null_lower_95 = unname(stats::quantile(simulated, 0.025)),
    null_upper_95 = unname(stats::quantile(simulated, 0.975)),
    empirical_p = p_upper,
    empirical_two_sided_p = p_two,
    percentile = mean(simulated <= observed),
    monte_carlo_se = sqrt(p_upper * (1 - p_upper) /
                            (length(simulated) + 1))
  )
}

v17_pair_predictive_test <- function(edges, cells, result,
                                     response = c("presence", "intensity"),
                                     predictors = c(
                                       fingerprint_turnover = "primary_turnover",
                                       composition_hellinger_turnover =
                                         "composition_sensitivity",
                                       total_support_difference =
                                         "support_change_sensitivity",
                                       mean_total_support = "support_level_context"
                                     )) {
  response <- match.arg(response)
  if (response == "presence") {
    support <- rep(TRUE, nrow(edges))
    observed <- edges$observed_presence_transition
    baseline_columns <- c(
      "geographic_distance_km", "environmental_distance",
      "natural_presence_difference", "natural_presence_uncertainty",
      "log_presence_pair_effort"
    )
  } else {
    support <- as.logical(edges$conditional_intensity_pair)
    observed <- edges$observed_intensity_transition
    baseline_columns <- c(
      "geographic_distance_km", "environmental_distance",
      "natural_intensity_difference", "log_intensity_pair_effort"
    )
  }
  d <- edges[support, , drop = FALSE]
  observed <- observed[support]
  simulated <- v17_transition_draws(d, cells, result, response)
  baseline <- d[, baseline_columns, drop = FALSE]
  baseline$geographic_distance_km <-
    log1p(baseline$geographic_distance_km)

  summaries <- list()
  null_rows <- list()
  for (predictor in names(predictors)) {
    observed_stat <- v17_fit_partial_statistic(
      observed, baseline, d[[predictor]]
    )
    null_stat <- t(vapply(seq_len(ncol(simulated)), function(draw) {
      v17_fit_partial_statistic(
        simulated[, draw], baseline, d[[predictor]]
      )[c("beta", "delta_r2")]
    }, numeric(2)))
    beta_comparison <- v17_null_comparison(
      observed_stat[["beta"]], null_stat[, "beta"]
    )
    r2_comparison <- v17_null_comparison(
      observed_stat[["delta_r2"]], null_stat[, "delta_r2"]
    )
    summaries[[predictor]] <- data.frame(
      response = response,
      radius_km = unique(d$radius_km),
      predictor = predictor,
      predictor_role = unname(predictors[[predictor]]),
      n_edges = as.integer(observed_stat[["n"]]),
      n_nodes = length(unique(c(d$site_i, d$site_j))),
      n_spatial_folds = length(unique(d$fold_i)),
      observed_partial_beta = observed_stat[["beta"]],
      beta_null_mean = beta_comparison[["null_mean"]],
      beta_null_sd = beta_comparison[["null_sd"]],
      beta_null_lower_95 = beta_comparison[["null_lower_95"]],
      beta_null_upper_95 = beta_comparison[["null_upper_95"]],
      beta_empirical_p = beta_comparison[["empirical_p"]],
      beta_two_sided_p = beta_comparison[["empirical_two_sided_p"]],
      beta_percentile = beta_comparison[["percentile"]],
      beta_monte_carlo_se = beta_comparison[["monte_carlo_se"]],
      observed_delta_r2 = observed_stat[["delta_r2"]],
      delta_r2_null_mean = r2_comparison[["null_mean"]],
      delta_r2_empirical_p = r2_comparison[["empirical_p"]],
      stringsAsFactors = FALSE
    )
    null_rows[[predictor]] <- data.frame(
      response = response,
      radius_km = unique(d$radius_km),
      predictor = predictor,
      draw = seq_len(nrow(null_stat)),
      partial_beta = null_stat[, "beta"],
      delta_r2 = null_stat[, "delta_r2"],
      stringsAsFactors = FALSE
    )
  }
  list(
    summary = do.call(rbind, summaries),
    null = do.call(rbind, null_rows)
  )
}

v17_adjust_multiplicity <- function(summary) {
  summary$BH_q_primary_25km <- NA_real_
  primary <- summary$predictor_role == "primary_turnover" &
    summary$radius_km == 25 & is.finite(summary$beta_empirical_p)
  summary$BH_q_primary_25km[primary] <-
    stats::p.adjust(summary$beta_empirical_p[primary], method = "BH")
  summary$BH_q_all_turnover_scales <- NA_real_
  turnover <- summary$predictor_role == "primary_turnover" &
    is.finite(summary$beta_empirical_p)
  summary$BH_q_all_turnover_scales[turnover] <-
    stats::p.adjust(summary$beta_empirical_p[turnover], method = "BH")
  summary$BH_q_all_diagnostics <- stats::p.adjust(
    summary$beta_empirical_p, method = "BH"
  )
  summary
}

v17_simulation_stability <- function(null) {
  group <- interaction(
    null$response, null$radius_km, null$predictor, drop = TRUE
  )
  rows <- lapply(split(null, group), function(d) {
    first <- d$partial_beta[d$draw <= floor(max(d$draw) / 2)]
    second <- d$partial_beta[d$draw > floor(max(d$draw) / 2)]
    data.frame(
      response = d$response[1],
      radius_km = d$radius_km[1],
      predictor = d$predictor[1],
      first_half_mean = mean(first, na.rm = TRUE),
      second_half_mean = mean(second, na.rm = TRUE),
      absolute_mean_difference = abs(
        mean(first, na.rm = TRUE) - mean(second, na.rm = TRUE)
      ),
      first_half_sd = stats::sd(first, na.rm = TRUE),
      second_half_sd = stats::sd(second, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
