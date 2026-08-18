v22_analysis_spec_version <- "v22.0_placement_null_human_context"

v22_require_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

v22_final8_environment_terms <- function() {
  c(
    "env_Temperature_PC1", "env_precip_PC1",
    "env_TemperatureSeasonality", "env_PrecipSeasonality",
    "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS"
  )
}

v22_scale_matrix <- function(data, terms) {
  v22_require_columns(data, terms, "cells")
  value <- as.matrix(data[, terms, drop = FALSE])
  storage.mode(value) <- "double"
  value <- apply(value, 2, function(x) {
    spread <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(spread) || spread <= 1e-12) {
      rep(0, length(x))
    } else {
      (x - mean(x, na.rm = TRUE)) / spread
    }
  })
  if (!is.matrix(value)) value <- matrix(value, ncol = length(terms))
  value
}

v22_final8_neighbour_graph <- function(
    cells, radius_km = 10, environment_caliper = 1,
    minimum_neighbours = 3L) {
  required <- c(
    "exact_site_id", "x_km", "y_km", "spatial_fold",
    "n_independent_sites", v22_final8_environment_terms()
  )
  v22_require_columns(cells, required, "cells")
  coordinates <- as.matrix(cells[, c("x_km", "y_km"), drop = FALSE])
  geographic <- as.matrix(stats::dist(coordinates))
  environment_matrix <- v22_scale_matrix(
    cells, v22_final8_environment_terms()
  )
  environmental <- as.matrix(stats::dist(environment_matrix)) /
    sqrt(ncol(environment_matrix))
  neighbours <- vector("list", nrow(cells))
  weights <- vector("list", nrow(cells))
  rows <- vector("list", nrow(cells))
  for (index in seq_len(nrow(cells))) {
    adjacent <- which(
      seq_len(nrow(cells)) != index &
        geographic[index, ] <= radius_km &
        environmental[index, ] <= environment_caliper
    )
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
    neighbours = neighbours,
    weights = weights,
    geographic_distance = geographic,
    environmental_distance = environmental,
    support = do.call(rbind, rows),
    settings = data.frame(
      representation = "current_broad_final8_rms_caliper1_primary",
      radius_km = radius_km,
      environment_caliper = environment_caliper,
      minimum_neighbours = as.integer(minimum_neighbours),
      normalization = paste(
        "RMS Euclidean distance after response-blind standardization of",
        "the eight current Broad abiotic axes"
      ),
      stringsAsFactors = FALSE
    )
  )
}

v22_directional_profile <- function(
    counts, graph,
    direction = c("pigmented_among_white", "white_among_pigmented")) {
  direction <- match.arg(direction)
  profile <- v20_local_profile(
    counts, graph, maximum_neighbour_pigment_share = 0
  )
  support_matrix <- matrix(
    as.logical(profile$supported),
    nrow(profile$present), ncol(profile$present)
  )
  if (direction == "pigmented_among_white") {
    candidate <- profile$candidate
    focal_eligible <- profile$present & support_matrix
  } else {
    all_neighbours_pigmented <-
      is.finite(profile$neighbour_pigment_share) &
      profile$neighbour_pigment_share >= 1 - 1e-12
    candidate <- !profile$present & support_matrix & all_neighbours_pigmented
    focal_eligible <- !profile$present & support_matrix
  }
  profile$candidate <- candidate
  profile$focal_eligible <- focal_eligible
  profile$direction <- direction
  profile
}

v22_candidate_exposure <- function(candidate, exposure) {
  if (is.vector(candidate)) candidate <- matrix(candidate, ncol = 1L)
  candidate <- as.matrix(candidate)
  storage.mode(candidate) <- "logical"
  exposure <- as.numeric(exposure)
  if (length(exposure) != nrow(candidate)) {
    stop("Exposure length must equal the number of candidate rows.",
         call. = FALSE)
  }
  finite <- is.finite(exposure)
  finite_matrix <- matrix(finite, nrow(candidate), ncol(candidate))
  requested <- colSums(candidate)
  usable <- colSums(candidate & finite_matrix)
  weighted_sum <- as.numeric(crossprod(
    ifelse(finite, exposure, 0), candidate * 1
  ))
  value <- weighted_sum / usable
  value[usable == 0L] <- NA_real_
  data.frame(
    candidate_count = as.integer(requested),
    usable_candidate_count = as.integer(usable),
    mean_exposure_rank = value,
    stringsAsFactors = FALSE
  )
}

v22_placement_null <- function(
    observed_candidate, simulated_candidate, exposure,
    radius_km, direction, feature = "population_exposure_rank") {
  observed <- v22_candidate_exposure(observed_candidate, exposure)
  simulated <- v22_candidate_exposure(simulated_candidate, exposure)
  comparison <- v18_null_comparison(
    observed$mean_exposure_rank[1L],
    simulated$mean_exposure_rank,
    "greater"
  )
  summary <- data.frame(
    spec_version = v22_analysis_spec_version,
    direction = direction,
    feature = feature,
    radius_km = as.numeric(radius_km),
    primary_radius = as.numeric(radius_km) == 5,
    observed_candidate_count = observed$candidate_count[1L],
    observed_usable_candidate_count = observed$usable_candidate_count[1L],
    missing_exposure_cells = sum(!is.finite(exposure)),
    n_null_draws_total = nrow(simulated),
    n_null_draws_finite = sum(is.finite(simulated$mean_exposure_rank)),
    n_zero_candidate_draws = sum(simulated$candidate_count == 0L),
    n_no_usable_exposure_draws = sum(
      !is.finite(simulated$mean_exposure_rank)
    ),
    observed_value = observed$mean_exposure_rank[1L],
    t(comparison),
    stringsAsFactors = FALSE
  )
  summary$observed_minus_null_mean <-
    summary$observed_value - summary$null_mean
  null <- data.frame(
    spec_version = v22_analysis_spec_version,
    direction = direction,
    feature = feature,
    radius_km = as.numeric(radius_km),
    draw = seq_len(nrow(simulated)),
    simulated,
    stringsAsFactors = FALSE
  )
  list(summary = summary, null = null)
}

v22_add_scale_maxT <- function(summary, null) {
  directions <- unique(as.character(summary$direction))
  output <- vector("list", length(directions))
  for (direction_index in seq_along(directions)) {
    direction <- directions[direction_index]
    current_summary <- summary[
      summary$direction == direction, , drop = FALSE
    ]
    current_summary <- current_summary[order(current_summary$radius_km), ]
    radii <- current_summary$radius_km
    draw_ids <- sort(unique(null$draw[null$direction == direction]))
    null_matrix <- matrix(
      NA_real_, nrow = length(draw_ids), ncol = length(radii),
      dimnames = list(draw_ids, as.character(radii))
    )
    for (radius_index in seq_along(radii)) {
      block <- null[
        null$direction == direction &
          null$radius_km == radii[radius_index], , drop = FALSE
      ]
      block <- block[match(draw_ids, block$draw), , drop = FALSE]
      null_matrix[, radius_index] <- block$mean_exposure_rank
    }
    center <- colMeans(null_matrix, na.rm = TRUE)
    spread <- apply(null_matrix, 2, stats::sd, na.rm = TRUE)
    spread[!is.finite(spread) | spread <= 1e-12] <- 1
    null_z <- sweep(
      sweep(null_matrix, 2, center, "-"), 2, spread, "/"
    )
    finite_max <- function(value) {
      value <- value[is.finite(value)]
      if (length(value)) max(value) else NA_real_
    }
    maximum_z <- apply(null_z, 1, finite_max)
    observed_z <- (
      current_summary$observed_value - center
    ) / spread
    current_summary$observed_standardized_departure <- observed_z
    current_summary$maxT_FWER_p <- vapply(
      seq_along(observed_z), function(index) {
        available <- maximum_z[is.finite(maximum_z)]
        (1 + sum(available >= observed_z[index])) /
          (length(available) + 1)
      }, numeric(1)
    )
    output[[direction_index]] <- current_summary
  }
  do.call(rbind, output)
}

v22_scale_profile <- function(summary, tolerance = 1e-12) {
  directions <- unique(as.character(summary$direction))
  rows <- lapply(directions, function(direction) {
    current <- summary[summary$direction == direction, , drop = FALSE]
    current <- current[order(current$radius_km), , drop = FALSE]
    effect <- current$observed_minus_null_mean
    change <- diff(effect)
    monotone_decay <- length(change) > 0L &&
      all(change <= tolerance) && any(change < -tolerance)
    flat_or_increasing <- length(change) > 0L &&
      all(change >= -tolerance)
    profile_shape <- if (monotone_decay) {
      "monotone_decay"
    } else if (flat_or_increasing) {
      "flat_or_increasing"
    } else {
      "non_monotone"
    }
    data.frame(
      direction = direction,
      radii_km = paste(current$radius_km, collapse = ","),
      effect_profile = paste(
        formatC(effect, digits = 8, format = "f"), collapse = ","
      ),
      shortest_radius_has_largest_effect =
        effect[1L] >= max(effect, na.rm = TRUE) - tolerance,
      monotone_nonincreasing = all(change <= tolerance),
      monotone_non_decreasing = all(change >= -tolerance),
      profile_shape = profile_shape,
      spearman_radius_effect = suppressWarnings(stats::cor(
        current$radius_km, effect, method = "spearman"
      )),
      effect_2km_minus_25km =
        effect[match(2, current$radius_km)] -
        effect[match(25, current$radius_km)],
      primary_5km_effect = effect[match(5, current$radius_km)],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

v22_population_exposure <- function(
    worldpop, cells, radii_km = c(2, 5, 10, 25)) {
  if (!exists("multiscale_point_context", mode = "function")) {
    stop("Source R/spatial_context.R first.", call. = FALSE)
  }
  rank_function <- if (exists("v19_rank01", mode = "function")) {
    v19_rank01
  } else {
    v18_rank01
  }
  population_sum <- multiscale_point_context(
    worldpop, cells, radii_km, summary_function = "sum"
  )
  out <- data.frame(
    exact_site_id = as.character(cells$exact_site_id),
    stringsAsFactors = FALSE
  )
  for (radius in radii_km) {
    value <- as.numeric(population_sum[, as.character(radius)])
    out[[paste0("population_sum_", radius, "km")]] <- value
    out[[paste0("log_population_sum_", radius, "km")]] <-
      log1p(pmax(value, 0))
    out[[paste0("population_", radius, "km_rank")]] <-
      rank_function(out[[paste0("log_population_sum_", radius, "km")]])
  }
  out
}

v22_profile_figure <- function(summary, output_prefix) {
  draw_figure <- function() {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    graphics::par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
    directions <- c(
      "pigmented_among_white", "white_among_pigmented"
    )
    titles <- c(
      "Pigmented among white", "White among pigmented"
    )
    for (index in seq_along(directions)) {
      block <- summary[
        summary$direction == directions[index], , drop = FALSE
      ]
      block <- block[order(block$radius_km), , drop = FALSE]
      limits <- range(
        block$lower_95, block$upper_95, block$observed_value,
        finite = TRUE
      )
      padding <- max(diff(limits) * 0.08, 0.01)
      graphics::plot(
        block$radius_km, block$observed_value,
        type = "b", pch = 19, lwd = 2,
        ylim = limits + c(-padding, padding),
        xlab = "Population-exposure radius (km)",
        ylab = "Mean exposure rank", main = titles[index]
      )
      graphics::arrows(
        block$radius_km, block$lower_95,
        block$radius_km, block$upper_95,
        angle = 90, code = 3, length = 0.05
      )
      graphics::lines(
        block$radius_km, block$null_mean,
        type = "b", pch = 1, lty = 2
      )
      graphics::legend(
        "topright", legend = c("Observed", "Natural-placement null"),
        pch = c(19, 1), lty = c(1, 2), bty = "n", cex = 0.8
      )
    }
  }
  grDevices::png(
    paste0(output_prefix, ".png"),
    width = 2200, height = 1000, res = 180
  )
  draw_figure()
  grDevices::dev.off()
  grDevices::pdf(
    paste0(output_prefix, ".pdf"), width = 11, height = 5
  )
  draw_figure()
  grDevices::dev.off()
  invisible(c(
    paste0(output_prefix, ".png"), paste0(output_prefix, ".pdf")
  ))
}
