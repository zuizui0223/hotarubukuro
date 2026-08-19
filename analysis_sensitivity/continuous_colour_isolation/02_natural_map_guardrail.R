aligned <- v18_align_result(model, cells, "final8 natural maps")
if (ncol(aligned$draws) < n_draws) {
  stop(
    "Final-eight checkpoint has only ", ncol(aligned$draws),
    " draws; requested ", n_draws, call. = FALSE
  )
}
simulated_counts <- aligned$draws[, seq_len(n_draws), drop = FALSE]

natural_features <- c(
  population_features, "did_proximity_rank", "mountainness_rank",
  "observation_effort_rank", "independent_site_support_rank"
)
n_features <- length(natural_features)
raw_pigmented <- matrix(
  NA_real_, nrow = n_draws, ncol = n_features,
  dimnames = list(NULL, natural_features)
)
raw_white <- raw_pigmented
raw_difference <- raw_pigmented
relative_pigmented <- raw_pigmented
relative_white <- raw_pigmented
relative_difference <- raw_pigmented
pigmented_count <- integer(n_draws)

chunks <- split(seq_len(n_draws), ceiling(seq_len(n_draws) / chunk_size))
for (chunk_index in seq_along(chunks)) {
  draw_indices <- chunks[[chunk_index]]
  state_chunk <- simulated_counts[, draw_indices, drop = FALSE] > 0
  same_chunk <- v23_nearest_same_matrix(
    state_chunk, distance, neighbour_order
  )
  relative_chunk <- log(sweep(
    same_chunk, 1L, observed_metrics$any_colour_nn_km, "/"
  ))
  for (local_index in seq_along(draw_indices)) {
    draw <- draw_indices[local_index]
    simulated_state <- state_chunk[, local_index]
    pigmented_count[draw] <- sum(simulated_state)
    raw_value <- same_chunk[, local_index]
    relative_value <- relative_chunk[, local_index]
    for (feature_index in seq_along(natural_features)) {
      feature <- natural_features[feature_index]
      feature_value <- as.numeric(features[[feature]])
      raw_pigmented[draw, feature_index] <- v23_spearman_rho(
        raw_value[simulated_state], feature_value[simulated_state]
      )
      raw_white[draw, feature_index] <- v23_spearman_rho(
        raw_value[!simulated_state], feature_value[!simulated_state]
      )
      raw_difference[draw, feature_index] <-
        raw_pigmented[draw, feature_index] - raw_white[draw, feature_index]
      relative_pigmented[draw, feature_index] <- v23_spearman_rho(
        relative_value[simulated_state], feature_value[simulated_state]
      )
      relative_white[draw, feature_index] <- v23_spearman_rho(
        relative_value[!simulated_state], feature_value[!simulated_state]
      )
      relative_difference[draw, feature_index] <-
        relative_pigmented[draw, feature_index] -
        relative_white[draw, feature_index]
    }
  }
  message(
    "[continuous isolation] natural-map chunk ", chunk_index,
    "/", length(chunks), " complete"
  )
}

exact_count_draws <- which(pigmented_count == observed_pigmented_count)
if (length(exact_count_draws) >= 200L) {
  count_conditioning_mode <- "exact_observed_pigmented_count"
  conditioned_draws <- exact_count_draws
} else {
  count_conditioning_mode <- "nearest_pigmented_count_up_to_1000"
  conditioned_draws <- head(order(
    abs(pigmented_count - observed_pigmented_count), seq_len(n_draws)
  ), min(1000L, n_draws))
}
selection <- data.frame(
  observed_pigmented_count = observed_pigmented_count,
  n_natural_maps = n_draws,
  n_exact_count_maps = length(exact_count_draws),
  count_conditioning_mode = count_conditioning_mode,
  n_conditioned_maps = length(conditioned_draws),
  minimum_absolute_count_difference = min(
    abs(pigmented_count[conditioned_draws] - observed_pigmented_count)
  ),
  maximum_absolute_count_difference = max(
    abs(pigmented_count[conditioned_draws] - observed_pigmented_count)
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  selection,
  file.path(output_dir, "continuous_isolation_count_conditioning.csv"),
  row.names = FALSE
)

observed_for <- function(metric_name, feature) {
  observed_contrasts[
    observed_contrasts$metric == metric_name &
      observed_contrasts$feature == feature,
    , drop = FALSE
  ]
}

summary_rows <- list()
append_summary <- function(
    metric_name, feature, component, observed, simulated,
    mode, draw_index) {
  comparison <- v23_null_comparison(
    observed, simulated[draw_index], "greater"
  )
  summary_rows[[length(summary_rows) + 1L]] <<- data.frame(
    metric = metric_name,
    feature = feature,
    component = component,
    null_mode = mode,
    observed_value = observed,
    n_null_maps = sum(is.finite(simulated[draw_index])),
    t(comparison),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

all_draws <- seq_len(n_draws)
for (feature_index in seq_along(natural_features)) {
  feature <- natural_features[feature_index]
  raw_observed <- observed_for("raw_same_colour_nn", feature)
  relative_observed <- observed_for("relative_same_to_any_nn", feature)
  for (mode in c("all_nondegenerate_maps", count_conditioning_mode)) {
    selected_draws <- if (mode == "all_nondegenerate_maps") {
      all_draws
    } else {
      conditioned_draws
    }
    append_summary(
      "raw_same_colour_nn", feature, "pigmented_rho",
      raw_observed$pigmented_rho,
      raw_pigmented[, feature_index], mode, selected_draws
    )
    append_summary(
      "raw_same_colour_nn", feature, "white_rho",
      raw_observed$white_rho,
      raw_white[, feature_index], mode, selected_draws
    )
    append_summary(
      "raw_same_colour_nn", feature, "rho_difference",
      raw_observed$rho_difference,
      raw_difference[, feature_index], mode, selected_draws
    )
    append_summary(
      "relative_same_to_any_nn", feature, "pigmented_rho",
      relative_observed$pigmented_rho,
      relative_pigmented[, feature_index], mode, selected_draws
    )
    append_summary(
      "relative_same_to_any_nn", feature, "white_rho",
      relative_observed$white_rho,
      relative_white[, feature_index], mode, selected_draws
    )
    append_summary(
      "relative_same_to_any_nn", feature, "rho_difference",
      relative_observed$rho_difference,
      relative_difference[, feature_index], mode, selected_draws
    )
  }
}
natural_summary <- do.call(rbind, summary_rows)
utils::write.csv(
  natural_summary,
  file.path(output_dir, "continuous_isolation_natural_null_summary.csv"),
  row.names = FALSE
)

population_indices <- match(population_features, natural_features)
observed_population_raw <- vapply(population_features, function(feature) {
  observed_for("raw_same_colour_nn", feature)$rho_difference
}, numeric(1))
observed_population_relative <- vapply(population_features, function(feature) {
  observed_for("relative_same_to_any_nn", feature)$rho_difference
}, numeric(1))
maxT <- data.frame(
  metric = c("raw_same_colour_nn", "relative_same_to_any_nn"),
  all_maps_population_scale_maxT_p = c(
    v23_maxT_upper_p(
      observed_population_raw,
      raw_difference[, population_indices, drop = FALSE]
    ),
    v23_maxT_upper_p(
      observed_population_relative,
      relative_difference[, population_indices, drop = FALSE]
    )
  ),
  conditioned_population_scale_maxT_p = c(
    v23_maxT_upper_p(
      observed_population_raw,
      raw_difference[conditioned_draws, population_indices, drop = FALSE]
    ),
    v23_maxT_upper_p(
      observed_population_relative,
      relative_difference[conditioned_draws, population_indices, drop = FALSE]
    )
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  maxT,
  file.path(output_dir, "continuous_isolation_population_maxT.csv"),
  row.names = FALSE
)

per_draw <- data.frame(
  draw = seq_len(n_draws),
  pigmented_count = pigmented_count,
  raw_difference_population_focal =
    raw_difference[, "local_population_rank"],
  raw_difference_population_5km =
    raw_difference[, "population_5km_rank"],
  raw_difference_population_10km =
    raw_difference[, "population_10km_rank"],
  raw_difference_population_25km =
    raw_difference[, "population_25km_rank"],
  raw_difference_population_50km =
    raw_difference[, "population_50km_rank"],
  relative_difference_population_5km =
    relative_difference[, "population_5km_rank"],
  selected_for_count_conditioned = seq_len(n_draws) %in% conditioned_draws,
  stringsAsFactors = FALSE
)
utils::write.csv(
  per_draw,
  file.path(output_dir, "continuous_isolation_natural_null_draws.csv"),
  row.names = FALSE
)

primary_raw <- natural_summary[
  natural_summary$metric == "raw_same_colour_nn" &
    natural_summary$feature == "population_5km_rank" &
    natural_summary$component == "rho_difference" &
    natural_summary$null_mode == "all_nondegenerate_maps",
  , drop = FALSE
]
primary_relative <- natural_summary[
  natural_summary$metric == "relative_same_to_any_nn" &
    natural_summary$feature == "population_5km_rank" &
    natural_summary$component == "rho_difference" &
    natural_summary$null_mode == "all_nondegenerate_maps",
  , drop = FALSE
]
raw_population_profile <- observed_contrasts[
  observed_contrasts$metric == "raw_same_colour_nn" &
    observed_contrasts$feature %in% population_features,
  , drop = FALSE
]
raw_population_profile$radius_km <- c(0, 5, 10, 25, 50)[
  match(raw_population_profile$feature, population_features)
]
raw_population_profile <- raw_population_profile[
  order(raw_population_profile$radius_km), , drop = FALSE
]
null_interval_rows <- natural_summary[
  natural_summary$metric == "raw_same_colour_nn" &
    natural_summary$component == "rho_difference" &
    natural_summary$null_mode == "all_nondegenerate_maps" &
    natural_summary$feature %in% population_features,
  , drop = FALSE
]
null_interval_rows$radius_km <- c(0, 5, 10, 25, 50)[
  match(null_interval_rows$feature, population_features)
]
null_interval_rows <- null_interval_rows[
  order(null_interval_rows$radius_km), , drop = FALSE
]
profile <- merge(
  raw_population_profile,
  null_interval_rows[, c(
    "feature", "null_mean", "lower_95", "upper_95",
    "empirical_p", "percentile"
  )],
  by = "feature", all.x = TRUE, sort = FALSE
)
profile$radius_km <- c(0, 5, 10, 25, 50)[
  match(profile$feature, population_features)
]
profile <- profile[order(profile$radius_km), , drop = FALSE]
utils::write.csv(
  profile,
  file.path(output_dir, "continuous_isolation_population_profile.csv"),
  row.names = FALSE
)
