v18_analysis_spec_version <- "v18.1_predictive_tail_rematched_extremes"

v18_require_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

v18_align_result <- function(result, cells, label = "result") {
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

v18_rank01 <- function(x) {
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

v18_predictive_tail_q <- function(observed, draws,
                                  direction = c("upper", "lower")) {
  direction <- match.arg(direction)
  observed <- as.numeric(observed)
  q <- rep(NA_real_, length(observed))
  keep <- is.finite(observed)
  if (!any(keep)) return(q)
  comparison <- if (direction == "upper") {
    draws[keep, , drop = FALSE] >= observed[keep]
  } else {
    draws[keep, , drop = FALSE] <= observed[keep]
  }
  q[keep] <- (1 + rowSums(comparison, na.rm = TRUE)) /
    (ncol(draws) + 1)
  q
}

v18_simulation_tail_q <- function(draws,
                                  direction = c("upper", "lower")) {
  direction <- match.arg(direction)
  ranked <- t(apply(draws, 1, function(value) {
    if (direction == "upper") {
      rank(-value, ties.method = "max") / length(value)
    } else {
      rank(value, ties.method = "max") / length(value)
    }
  }))
  if (!is.matrix(ranked)) ranked <- matrix(ranked, nrow = nrow(draws))
  ranked
}

v18_tail_depth <- function(q) {
  -log10(pmax(q, .Machine$double.eps))
}

v18_z_matrix <- function(values) {
  center <- rowMeans(values)
  spread <- apply(values, 1, stats::sd)
  spread[!is.finite(spread) | spread <= 1e-12] <- NA_real_
  sweep(sweep(values, 1, center, "-"), 1, spread, "/")
}

v18_top_indices <- function(q, z, eligible, fraction, ids) {
  candidates <- which(
    as.logical(eligible) & is.finite(q) & is.finite(z)
  )
  if (!length(candidates)) return(integer())
  candidates <- candidates[order(
    q[candidates], -z[candidates], as.character(ids[candidates])
  )]
  head(candidates, max(1L, ceiling(fraction * length(candidates))))
}

v18_scale_vector <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (!any(keep)) return(out)
  spread <- stats::sd(x[keep])
  if (!is.finite(spread) || spread <= 1e-12) {
    out[keep] <- 0
  } else {
    out[keep] <- (x[keep] - mean(x[keep])) / spread
  }
  out
}

v18_environment_matrix <- function(cells) {
  terms <- c(
    "broad50km_pc1", "broad50km_pc2",
    "within50km_pc1", "within50km_pc2"
  )
  v18_require_columns(cells, terms, "cells")
  matrix <- as.matrix(cells[, terms, drop = FALSE])
  storage.mode(matrix) <- "double"
  matrix <- apply(matrix, 2, v18_scale_vector)
  if (!is.matrix(matrix)) matrix <- matrix(matrix, ncol = length(terms))
  matrix
}

v18_match_options <- function(cells, natural_probability,
                              maximum_distance_km = 200,
                              maximum_environment_distance = 2.5,
                              maximum_logit_difference = 1.5,
                              maximum_effort_difference = 1.5) {
  required <- c(
    "x_km", "y_km", "spatial_fold", "n_observations", "exact_site_id"
  )
  v18_require_columns(cells, required, "cells")
  env <- v18_environment_matrix(cells)
  coordinates <- as.matrix(cells[, c("x_km", "y_km"), drop = FALSE])
  dx <- outer(coordinates[, 1], coordinates[, 1], "-")
  dy <- outer(coordinates[, 2], coordinates[, 2], "-")
  geographic <- sqrt(dx^2 + dy^2)
  diag(geographic) <- Inf
  probability <- pmin(pmax(as.numeric(natural_probability), 1e-6), 1 - 1e-6)
  logit <- stats::qlogis(probability)
  effort <- log1p(pmax(as.numeric(cells$n_observations), 0))
  options <- vector("list", nrow(cells))
  detail_rows <- vector("list", nrow(cells))

  for (i in seq_len(nrow(cells))) {
    environment_distance <- sqrt(rowMeans(
      (env - matrix(env[i, ], nrow(env), ncol(env), byrow = TRUE))^2
    ))
    logit_difference <- abs(logit - logit[i])
    effort_difference <- abs(effort - effort[i])
    eligible <- seq_len(nrow(cells)) != i &
      as.integer(cells$spatial_fold) == as.integer(cells$spatial_fold[i]) &
      geographic[i, ] <= maximum_distance_km &
      environment_distance <= maximum_environment_distance &
      logit_difference <= maximum_logit_difference &
      effort_difference <= maximum_effort_difference
    candidates <- which(eligible)
    if (!length(candidates)) {
      options[[i]] <- integer()
      next
    }
    cost <- geographic[i, candidates] / maximum_distance_km +
      environment_distance[candidates] / maximum_environment_distance +
      logit_difference[candidates] / maximum_logit_difference +
      effort_difference[candidates] / maximum_effort_difference
    order_index <- order(cost, candidates)
    candidates <- candidates[order_index]
    options[[i]] <- candidates
    detail_rows[[i]] <- data.frame(
      case_index = i,
      control_index = candidates,
      matching_cost = cost[order_index],
      geographic_distance_km = geographic[i, candidates],
      environmental_distance = environment_distance[candidates],
      natural_logit_difference = logit_difference[candidates],
      effort_difference = effort_difference[candidates],
      stringsAsFactors = FALSE
    )
  }
  list(
    options = options,
    details = detail_rows,
    settings = data.frame(
      maximum_distance_km = maximum_distance_km,
      maximum_environment_distance = maximum_environment_distance,
      maximum_logit_difference = maximum_logit_difference,
      maximum_effort_difference = maximum_effort_difference,
      stringsAsFactors = FALSE
    )
  )
}

v18_match_cases <- function(cases, controls, match_options, q, z, ids) {
  cases <- unique(as.integer(cases))
  controls <- unique(as.integer(controls))
  if (!length(cases) || !length(controls)) return(data.frame())
  cases <- cases[order(q[cases], -z[cases], as.character(ids[cases]))]
  controls <- setdiff(controls, cases)
  used <- rep(FALSE, length(ids))
  rows <- list()
  for (case in cases) {
    candidates <- match_options$options[[case]]
    candidates <- candidates[
      candidates %in% controls & !used[candidates]
    ]
    if (!length(candidates)) next
    control <- candidates[1]
    used[control] <- TRUE
    detail <- match_options$details[[case]]
    detail <- detail[match(control, detail$control_index), , drop = FALSE]
    rows[[length(rows) + 1L]] <- data.frame(
      case_index = case,
      control_index = control,
      case_id = as.character(ids[case]),
      control_id = as.character(ids[control]),
      detail[, setdiff(names(detail), c("case_index", "control_index")),
             drop = FALSE],
      stringsAsFactors = FALSE
    )
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

v18_profile <- function(cells, presence, phenology, intensity) {
  observed_share <- as.numeric(cells$n_pigmented) /
    pmax(as.numeric(cells$n_observations), 1)
  predicted_share_draws <- sweep(
    presence$draws, 1, pmax(as.numeric(cells$n_observations), 1), "/"
  )
  color_q <- v18_predictive_tail_q(
    as.numeric(cells$n_pigmented), presence$draws, "upper"
  )
  color_sd <- apply(predicted_share_draws, 1, stats::sd)
  color_z <- (observed_share - rowMeans(predicted_share_draws)) /
    pmax(color_sd, 1e-12)
  early_q <- v18_predictive_tail_q(
    as.numeric(cells$median_DOY), phenology$draws, "lower"
  )
  dark_q <- v18_predictive_tail_q(
    as.numeric(cells$conditional_intensity_median),
    intensity$draws, "upper"
  )
  population_rank <- v18_rank01(cells$log_population_sum_25km)
  early_depth <- v18_tail_depth(early_q)
  dark_depth <- v18_tail_depth(dark_q)
  convergence_count <- as.integer(population_rank >= 0.90) +
    as.integer(early_q <= 0.10) + as.integer(dark_q <= 0.10)
  convergence_count[!is.finite(dark_q)] <- NA_integer_
  data.frame(
    exact_site_id = as.character(cells$exact_site_id),
    observed_pigment_share = observed_share,
    natural_pigment_probability = presence$latent_mean,
    unexpected_pigmented_q = color_q,
    unexpected_pigmented_tail_depth = v18_tail_depth(color_q),
    unexpected_pigmented_z = color_z,
    early_predictive_q = early_q,
    early_tail_depth = early_depth,
    dark_predictive_q = dark_q,
    dark_tail_depth = dark_depth,
    population_rank = population_rank,
    high_population = population_rank >= 0.90,
    early_tail_10 = early_q <= 0.10,
    dark_tail_10 = dark_q <= 0.10,
    convergence_count = convergence_count,
    two_plus_facets = convergence_count >= 2,
    stringsAsFactors = FALSE
  )
}

v18_pair_metric <- function(pairs, values) {
  if (!nrow(pairs)) return(NA_real_)
  difference <- values[pairs$case_index] - values[pairs$control_index]
  difference <- difference[is.finite(difference)]
  if (length(difference)) mean(difference) else NA_real_
}

v18_null_comparison <- function(observed, simulated,
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

v18_extreme_envelope <- function(profile, q_sim, z_sim, simulated_eligible,
                                 observed_eligible, fractions, ids) {
  rows <- list()
  observed_depth <- profile$unexpected_pigmented_tail_depth
  simulated_depth <- v18_tail_depth(q_sim)
  observed_max <- max(observed_depth[observed_eligible], na.rm = TRUE)
  simulated_max <- vapply(seq_len(ncol(q_sim)), function(draw) {
    max(simulated_depth[simulated_eligible[, draw], draw], na.rm = TRUE)
  }, numeric(1))
  comparison <- v18_null_comparison(observed_max, simulated_max, "greater")
  rows[[1]] <- data.frame(
    tier = "global_max", metric = "maximum_tail_depth",
    observed_value = observed_max, n_observed_selected = 1L,
    n_null_draws = ncol(q_sim), t(comparison), stringsAsFactors = FALSE
  )
  observed_q05 <- sum(
    observed_eligible & profile$unexpected_pigmented_q <= 0.05,
    na.rm = TRUE
  )
  simulated_q05 <- colSums(simulated_eligible & q_sim <= 0.05, na.rm = TRUE)
  comparison <- v18_null_comparison(observed_q05, simulated_q05, "greater")
  rows[[2]] <- data.frame(
    tier = "absolute_q05", metric = "candidate_count",
    observed_value = observed_q05, n_observed_selected = observed_q05,
    n_null_draws = ncol(q_sim), t(comparison), stringsAsFactors = FALSE
  )
  for (fraction in fractions) {
    tier <- paste0("top_", sprintf("%04g", 100 * fraction))
    observed_cases <- v18_top_indices(
      profile$unexpected_pigmented_q,
      profile$unexpected_pigmented_z,
      observed_eligible, fraction, ids
    )
    observed_value <- mean(observed_depth[observed_cases])
    simulated_value <- vapply(seq_len(ncol(q_sim)), function(draw) {
      cases <- v18_top_indices(
        q_sim[, draw], z_sim[, draw], simulated_eligible[, draw],
        fraction, ids
      )
      mean(simulated_depth[cbind(cases, rep(draw, length(cases)))])
    }, numeric(1))
    comparison <- v18_null_comparison(
      observed_value, simulated_value, "greater"
    )
    rows[[length(rows) + 1L]] <- data.frame(
      tier = tier, metric = "mean_selected_tail_depth",
      observed_value = observed_value,
      n_observed_selected = length(observed_cases),
      n_null_draws = ncol(q_sim), t(comparison),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

v18_matched_extreme_analysis <- function(
    cells, profile, q_sim, z_sim, simulated_counts, match_options,
    fractions = c(0.01, 0.025, 0.05, 0.10),
    control_min_q = 0.25) {
  ids <- as.character(cells$exact_site_id)
  observed_pigmented <- as.numeric(cells$n_pigmented) > 0
  simulated_pigmented <- simulated_counts > 0
  conditional_support <- is.finite(profile$dark_predictive_q)
  metric_definitions <- list(
    population_rank = list(
      support = rep(TRUE, nrow(cells)),
      values = profile$population_rank,
      role = "human_context"
    ),
    early_tail_depth = list(
      support = is.finite(profile$early_predictive_q),
      values = profile$early_tail_depth,
      role = "phenology_tail"
    ),
    dark_tail_depth = list(
      support = conditional_support,
      values = profile$dark_tail_depth,
      role = "intensity_tail"
    ),
    convergence_count = list(
      support = conditional_support & is.finite(profile$convergence_count),
      values = profile$convergence_count,
      role = "multi_facet_convergence"
    ),
    two_plus_facets = list(
      support = conditional_support & is.finite(profile$convergence_count),
      values = as.numeric(profile$two_plus_facets),
      role = "multi_facet_binary"
    )
  )
  summaries <- list()
  null_rows <- list()
  observed_pair_rows <- list()
  for (fraction in fractions) {
    tier <- paste0("top_", sprintf("%04g", 100 * fraction))
    for (metric in names(metric_definitions)) {
      definition <- metric_definitions[[metric]]
      support <- as.logical(definition$support)
      observed_eligible <- observed_pigmented & support
      observed_cases <- v18_top_indices(
        profile$unexpected_pigmented_q,
        profile$unexpected_pigmented_z,
        observed_eligible, fraction, ids
      )
      observed_controls <- which(
        observed_eligible &
          profile$unexpected_pigmented_q >= control_min_q
      )
      observed_pairs <- v18_match_cases(
        observed_cases, observed_controls, match_options,
        profile$unexpected_pigmented_q,
        profile$unexpected_pigmented_z, ids
      )
      observed_value <- v18_pair_metric(
        observed_pairs, definition$values
      )
      if (nrow(observed_pairs)) {
        observed_pairs$tier <- tier
        observed_pairs$metric <- metric
        observed_pairs$case_color_q <-
          profile$unexpected_pigmented_q[observed_pairs$case_index]
        observed_pairs$control_color_q <-
          profile$unexpected_pigmented_q[observed_pairs$control_index]
        observed_pairs$case_value <-
          definition$values[observed_pairs$case_index]
        observed_pairs$control_value <-
          definition$values[observed_pairs$control_index]
        observed_pairs$difference <-
          observed_pairs$case_value - observed_pairs$control_value
        observed_pair_rows[[length(observed_pair_rows) + 1L]] <-
          observed_pairs
      }

      simulated_value <- rep(NA_real_, ncol(q_sim))
      simulated_matched <- integer(ncol(q_sim))
      simulated_requested <- integer(ncol(q_sim))
      for (draw in seq_len(ncol(q_sim))) {
        eligible <- simulated_pigmented[, draw] & support
        cases <- v18_top_indices(
          q_sim[, draw], z_sim[, draw], eligible, fraction, ids
        )
        controls <- which(
          eligible & q_sim[, draw] >= control_min_q
        )
        pairs <- v18_match_cases(
          cases, controls, match_options,
          q_sim[, draw], z_sim[, draw], ids
        )
        simulated_value[draw] <- v18_pair_metric(
          pairs, definition$values
        )
        simulated_matched[draw] <- nrow(pairs)
        simulated_requested[draw] <- length(cases)
      }
      comparison <- v18_null_comparison(
        observed_value, simulated_value, "greater"
      )
      summaries[[length(summaries) + 1L]] <- data.frame(
        tier = tier, fraction = fraction, metric = metric,
        metric_role = definition$role,
        n_observed_cases_requested = length(observed_cases),
        n_observed_pairs = nrow(observed_pairs),
        observed_match_fraction = if (length(observed_cases)) {
          nrow(observed_pairs) / length(observed_cases)
        } else NA_real_,
        observed_value = observed_value,
        null_mean_matched_pairs = mean(simulated_matched),
        null_mean_match_fraction = mean(
          simulated_matched / pmax(simulated_requested, 1)
        ),
        n_null_draws = ncol(q_sim), t(comparison),
        stringsAsFactors = FALSE
      )
      null_rows[[length(null_rows) + 1L]] <- data.frame(
        tier = tier, fraction = fraction, metric = metric,
        draw = seq_len(ncol(q_sim)),
        matched_pairs = simulated_matched,
        requested_cases = simulated_requested,
        matched_contrast = simulated_value,
        stringsAsFactors = FALSE
      )
    }
  }
  summary <- do.call(rbind, summaries)
  summary$BH_q_primary_top5 <- NA_real_
  primary <- summary$fraction == 0.05 &
    summary$metric %in% c(
      "population_rank", "early_tail_depth", "dark_tail_depth",
      "convergence_count"
    )
  summary$BH_q_primary_top5[primary] <- stats::p.adjust(
    summary$empirical_p[primary], method = "BH"
  )
  summary$BH_q_all_diagnostics <- stats::p.adjust(
    summary$empirical_p, method = "BH"
  )
  list(
    summary = summary,
    null = do.call(rbind, null_rows),
    observed_pairs = if (length(observed_pair_rows)) {
      do.call(rbind, observed_pair_rows)
    } else data.frame()
  )
}

v18_simulation_stability <- function(null) {
  keys <- interaction(null$tier, null$metric, drop = TRUE)
  do.call(rbind, lapply(split(null, keys), function(d) {
    midpoint <- floor(max(d$draw) / 2)
    first <- d$matched_contrast[d$draw <= midpoint]
    second <- d$matched_contrast[d$draw > midpoint]
    data.frame(
      tier = d$tier[1], metric = d$metric[1],
      first_half_mean = mean(first, na.rm = TRUE),
      second_half_mean = mean(second, na.rm = TRUE),
      absolute_mean_difference = abs(
        mean(first, na.rm = TRUE) - mean(second, na.rm = TRUE)
      ),
      first_half_n_finite = sum(is.finite(first)),
      second_half_n_finite = sum(is.finite(second)),
      stringsAsFactors = FALSE
    )
  }))
}
