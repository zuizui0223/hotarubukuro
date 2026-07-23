v20_analysis_spec_version <- "v20.2_environment_similar_white_neighbourhood"

v20_require_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

v20_neighbour_graph <- function(
    cells, radius_km = 10, environment_caliper = 1,
    minimum_neighbours = 3L, same_fold_only = TRUE) {
  required <- c(
    "exact_site_id", "x_km", "y_km", "spatial_fold",
    "n_independent_sites"
  )
  v20_require_columns(cells, required, "cells")
  if (!exists("v18_environment_matrix", mode = "function")) {
    stop("Source extreme_human_diagnostic_v11.R first.", call. = FALSE)
  }
  coordinates <- as.matrix(cells[, c("x_km", "y_km"), drop = FALSE])
  geographic <- as.matrix(stats::dist(coordinates))
  environment <- v18_environment_matrix(cells)
  environmental <- as.matrix(stats::dist(environment)) /
    sqrt(ncol(environment))
  neighbours <- vector("list", nrow(cells))
  weights <- vector("list", nrow(cells))
  rows <- vector("list", nrow(cells))
  for (index in seq_len(nrow(cells))) {
    eligible <- seq_len(nrow(cells)) != index &
      geographic[index, ] <= radius_km &
      environmental[index, ] <= environment_caliper
    if (same_fold_only) {
      eligible <- eligible &
        as.integer(cells$spatial_fold) ==
        as.integer(cells$spatial_fold[index])
    }
    adjacent <- which(eligible)
    if (length(adjacent)) {
      adjacent <- adjacent[order(
        geographic[index, adjacent],
        as.character(cells$exact_site_id[adjacent])
      )]
      weight <- 1 / pmax(geographic[index, adjacent], 0.5)
      weight <- weight / sum(weight)
    } else {
      weight <- numeric()
    }
    neighbours[[index]] <- adjacent
    weights[[index]] <- weight
    rows[[index]] <- data.frame(
      exact_site_id = as.character(cells$exact_site_id[index]),
      n_neighbours = length(adjacent),
      n_neighbour_independent_sites = sum(
        as.numeric(cells$n_independent_sites[adjacent]), na.rm = TRUE
      ),
      mean_neighbour_distance_km = if (length(adjacent)) {
        mean(geographic[index, adjacent])
      } else NA_real_,
      maximum_neighbour_distance_km = if (length(adjacent)) {
        max(geographic[index, adjacent])
      } else NA_real_,
      mean_environmental_distance = if (length(adjacent)) {
        mean(environmental[index, adjacent])
      } else NA_real_,
      maximum_environmental_distance = if (length(adjacent)) {
        max(environmental[index, adjacent])
      } else NA_real_,
      supported = length(adjacent) >= minimum_neighbours,
      stringsAsFactors = FALSE
    )
  }
  list(
    neighbours = neighbours, weights = weights,
    geographic_distance = geographic,
    environmental_distance = environmental,
    support = do.call(rbind, rows),
    settings = data.frame(
      radius_km = radius_km,
      environment_caliper = environment_caliper,
      minimum_neighbours = as.integer(minimum_neighbours),
      same_fold_only = same_fold_only,
      stringsAsFactors = FALSE
    )
  )
}

v20_local_profile <- function(counts, graph,
                              maximum_neighbour_pigment_share = 0) {
  if (is.vector(counts)) counts <- matrix(counts, ncol = 1L)
  counts <- as.matrix(counts)
  present <- counts > 0
  neighbour_share <- matrix(
    NA_real_, nrow = nrow(counts), ncol = ncol(counts)
  )
  weighted_share <- matrix(
    NA_real_, nrow = nrow(counts), ncol = ncol(counts)
  )
  for (index in seq_len(nrow(counts))) {
    adjacent <- graph$neighbours[[index]]
    if (!length(adjacent)) next
    neighbour_share[index, ] <- colMeans(
      present[adjacent, , drop = FALSE]
    )
    weighted_share[index, ] <- as.numeric(
      graph$weights[[index]] %*%
        present[adjacent, , drop = FALSE]
    )
  }
  supported <- as.logical(graph$support$supported)
  candidate <- present &
    matrix(supported, nrow(counts), ncol(counts)) &
    neighbour_share <= maximum_neighbour_pigment_share
  list(
    present = present,
    neighbour_pigment_share = neighbour_share,
    weighted_neighbour_pigment_share = weighted_share,
    local_isolation_score = 1 - weighted_share,
    candidate = candidate,
    supported = supported
  )
}

v20_configuration_table <- function() {
  data.frame(
    configuration = c(
      "primary_10km_env1_all_white",
      "fold_restricted_10km_env1_all_white",
      "scale_5km_env1_all_white",
      "scale_25km_env1_white90",
      "environment_10km_env075_all_white",
      "environment_10km_env15_all_white"
    ),
    radius_km = c(10, 10, 5, 25, 10, 10),
    environment_caliper = c(1, 1, 1, 1, 0.75, 1.5),
    minimum_neighbours = c(3L, 3L, 3L, 5L, 3L, 3L),
    maximum_neighbour_pigment_share = c(0, 0, 0, 0.1, 0, 0),
    same_fold_only = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    role = c("primary", rep("sensitivity", 5)),
    stringsAsFactors = FALSE
  )
}

v20_metric_rows <- function(observed_profile, simulated_profile,
                            observed_q, simulated_q, configuration) {
  observed_supported_present <- observed_profile$present[, 1L] &
    observed_profile$supported
  simulated_supported_present <- simulated_profile$present &
    matrix(
      simulated_profile$supported,
      nrow(simulated_profile$present),
      ncol(simulated_profile$present)
    )
  observed_candidate <- observed_profile$candidate[, 1L]
  simulated_candidate <- simulated_profile$candidate
  observed_values <- c(
    candidate_count = sum(observed_candidate),
    candidate_fraction = sum(observed_candidate) /
      max(sum(observed_supported_present), 1),
    joint_candidate_q10_count = sum(
      observed_candidate & observed_q <= 0.10, na.rm = TRUE
    ),
    joint_candidate_q05_count = sum(
      observed_candidate & observed_q <= 0.05, na.rm = TRUE
    )
  )
  simulated_values <- cbind(
    candidate_count = colSums(simulated_candidate),
    candidate_fraction = colSums(simulated_candidate) /
      pmax(colSums(simulated_supported_present), 1),
    joint_candidate_q10_count = colSums(
      simulated_candidate & simulated_q <= 0.10
    ),
    joint_candidate_q05_count = colSums(
      simulated_candidate & simulated_q <= 0.05
    )
  )
  rows <- lapply(names(observed_values), function(metric) {
    comparison <- v18_null_comparison(
      observed_values[[metric]], simulated_values[, metric], "greater"
    )
    data.frame(
      configuration = configuration,
      metric = metric,
      observed_value = observed_values[[metric]],
      n_null_draws = nrow(simulated_values),
      t(comparison),
      stringsAsFactors = FALSE
    )
  })
  list(
    summary = do.call(rbind, rows),
    null = data.frame(
      configuration = configuration,
      draw = seq_len(nrow(simulated_values)),
      simulated_values,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  )
}

v20_filter_match_options <- function(match_options, graph,
                                     maximum_log_support_difference = 0.75) {
  filtered <- match_options
  support <- log1p(as.numeric(graph$support$n_neighbours))
  site_support <- log1p(as.numeric(
    graph$support$n_neighbour_independent_sites
  ))
  for (index in seq_along(filtered$options)) {
    candidates <- filtered$options[[index]]
    keep <- abs(support[candidates] - support[index]) <=
      maximum_log_support_difference &
      abs(site_support[candidates] - site_support[index]) <=
      maximum_log_support_difference
    candidates <- candidates[keep]
    filtered$options[[index]] <- candidates
    detail <- filtered$details[[index]]
    if (!is.null(detail) && nrow(detail)) {
      detail <- detail[
        detail$control_index %in% candidates, , drop = FALSE
      ]
    }
    # Preserve the one-list-entry-per-cell invariant. Assigning NULL with
    # `[[<-` would delete the entry and shift all later cell indices.
    filtered$details[index] <- list(detail)
  }
  filtered$settings$maximum_log_neighbour_support_difference <-
    maximum_log_support_difference
  filtered
}

v20_candidate_table <- function(
    cells, features, graph, profile, observed_q, observed_z) {
  index <- which(profile$candidate[, 1L])
  if (!length(index)) return(data.frame())
  out <- data.frame(
    candidate_index = index,
    exact_site_id = as.character(cells$exact_site_id[index]),
    longitude = as.numeric(cells$longitude[index]),
    latitude = as.numeric(cells$latitude[index]),
    n_observations = as.integer(cells$n_observations[index]),
    n_pigmented = as.integer(cells$n_pigmented[index]),
    observed_pigment_share = as.numeric(cells$n_pigmented[index]) /
      pmax(as.numeric(cells$n_observations[index]), 1),
    unexpected_pigmented_q = observed_q[index],
    unexpected_pigmented_z = observed_z[index],
    n_neighbours = graph$support$n_neighbours[index],
    n_neighbour_independent_sites =
      graph$support$n_neighbour_independent_sites[index],
    neighbour_pigment_share =
      profile$neighbour_pigment_share[index, 1L],
    local_isolation_score =
      profile$local_isolation_score[index, 1L],
    mean_neighbour_distance_km =
      graph$support$mean_neighbour_distance_km[index],
    mean_environmental_distance =
      graph$support$mean_environmental_distance[index],
    features[index, setdiff(
      names(features), c("exact_site_id", "spatial_fold")
    ), drop = FALSE],
    stringsAsFactors = FALSE
  )
  out <- out[order(
    out$unexpected_pigmented_q,
    -out$unexpected_pigmented_z,
    out$exact_site_id
  ), , drop = FALSE]
  out$candidate_rank <- seq_len(nrow(out))
  out
}
