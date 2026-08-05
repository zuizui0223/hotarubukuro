transition_required_columns <- function() {
  c(
    "exact_site_id", "longitude", "latitude", "x_km", "y_km",
    "pigmented_mixture50", "pigmented_high_confidence", "colour_a", "DOY",
    "bee_ardens", "bee_diversus", "bee_beaticola", "bee_consobrinus",
    "bee_honshuensis"
  )
}

transition_assert_columns <- function(data, columns = transition_required_columns()) {
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns)) {
    stop(
      "Transition-zone analysis is missing columns: ",
      paste(missing_columns, collapse = ", "), call. = FALSE
    )
  }
  invisible(TRUE)
}

transition_finite_mean <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

transition_first_text <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x)) x[1] else NA_character_
}

transition_rank01 <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (sum(keep) == 1L) {
    out[keep] <- 0.5
  } else if (sum(keep) > 1L) {
    out[keep] <- (rank(x[keep], ties.method = "average") - 1) / (sum(keep) - 1)
  }
  out
}

transition_z <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (!sum(keep)) return(out)
  centre <- mean(x[keep])
  spread <- stats::sd(x[keep])
  out[keep] <- if (is.finite(spread) && spread > 0) {
    (x[keep] - centre) / spread
  } else {
    0
  }
  out
}

transition_site_table <- function(data) {
  transition_assert_columns(data)
  groups <- split(seq_len(nrow(data)), as.character(data$exact_site_id))
  numeric_columns <- intersect(
    c(
      "longitude", "latitude", "x_km", "y_km", "elevation", "DOY", "year",
      "Temperature_PC1", "precip_PC1", "TemperatureSeasonality",
      "PrecipSeasonality", "topo_PC1", "soil_PC1", "soil_PC2", "RSDS",
      "env_Temperature_PC1", "env_precip_PC1",
      "env_TemperatureSeasonality", "env_PrecipSeasonality",
      "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS",
      "bee_ardens", "bee_diversus", "bee_beaticola", "bee_consobrinus",
      "bee_honshuensis", "Bombus_W", "Bombus_A", "human_population",
      "human_forest_edge", "road_access", "z_H", "z_R", "z_A",
      "spatial_fold", "block_x", "block_y",
      "natural_presence_probability", "presence_natural_deviance_residual",
      "natural_intensity_prediction", "intensity_natural_residual",
      "early_score", "colour_a", "pigment_intensity_z"
    ),
    names(data)
  )
  occurrence_columns <- grep(
    "^occ_(ardens|diversus|beaticola|consobrinus|honshuensis)_(50|100)km$",
    names(data), value = TRUE
  )
  numeric_columns <- unique(c(numeric_columns, occurrence_columns))

  rows <- lapply(groups, function(index) {
    block <- data[index, , drop = FALSE]
    pigment <- as.integer(block$pigmented_mixture50)
    n_pigmented <- sum(pigment == 1L, na.rm = TRUE)
    n_white <- sum(pigment == 0L, na.rm = TRUE)
    site_class <- if (n_pigmented > 0L && n_white > 0L) {
      "mixed"
    } else if (n_pigmented > 0L) {
      "pigmented"
    } else {
      "white"
    }
    high_confidence <- as.integer(block$pigmented_high_confidence)
    row <- data.frame(
      exact_site_id = transition_first_text(block$exact_site_id),
      n_observations = nrow(block),
      n_dates = length(unique(as.character(block$date))),
      n_years = if ("year" %in% names(block)) {
        length(unique(as.numeric(block$year)[is.finite(as.numeric(block$year))]))
      } else {
        NA_integer_
      },
      n_pigmented = n_pigmented,
      n_white = n_white,
      pigment_share = n_pigmented / max(1, n_pigmented + n_white),
      site_class = site_class,
      all_classifications_high_confidence = all(!is.na(high_confidence)),
      stringsAsFactors = FALSE
    )
    for (column in numeric_columns) {
      row[[column]] <- transition_finite_mean(block[[column]])
    }
    row
  })
  sites <- do.call(rbind, rows)
  rownames(sites) <- NULL
  sites
}

transition_population_table <- function(data, cell_km = 1, offset_km = 0) {
  if (!is.finite(cell_km) || cell_km <= 0) {
    stop("Population cell size must be positive.", call. = FALSE)
  }
  transition_assert_columns(data)
  original_site <- as.character(data$exact_site_id)
  cell_x <- floor((as.numeric(data$x_km) - offset_km) / cell_km)
  cell_y <- floor((as.numeric(data$y_km) - offset_km) / cell_km)
  cell_id <- paste0(
    "cell-", format(cell_km, trim = TRUE, scientific = FALSE), "km-",
    cell_x, "_", cell_y
  )
  population_data <- data
  population_data$exact_site_id <- cell_id
  populations <- transition_site_table(population_data)
  exact_site_counts <- vapply(
    split(original_site, cell_id), function(x) length(unique(x)), integer(1)
  )
  populations$n_exact_sites <- unname(exact_site_counts[populations$exact_site_id])
  populations$observed_x_km <- populations$x_km
  populations$observed_y_km <- populations$y_km
  population_cell_x <- floor((populations$observed_x_km - offset_km) / cell_km)
  population_cell_y <- floor((populations$observed_y_km - offset_km) / cell_km)
  populations$x_km <- (population_cell_x + 0.5) * cell_km + offset_km
  populations$y_km <- (population_cell_y + 0.5) * cell_km + offset_km
  populations$spatial_unit_km <- cell_km
  populations$grid_offset_km <- offset_km
  populations
}

transition_population_unit_sensitivity <- function(data,
                                                   cell_sizes_km = c(0.5, 1, 2, 5),
                                                   matching_radius_km = 25,
                                                   environment_caliper = 0.75) {
  rows <- lapply(cell_sizes_km, function(cell_km) {
    populations <- transition_population_table(data, cell_km)
    pairs <- transition_matched_pairs(
      populations, matching_radius_km, environment_caliper
    )
    data.frame(
      cell_km = cell_km,
      n_population_cells = nrow(populations),
      n_mixed_cells = sum(populations$site_class == "mixed"),
      n_white_cells = sum(populations$site_class == "white"),
      n_pigmented_cells = sum(populations$site_class == "pigmented"),
      n_independent_matched_pairs = nrow(pairs),
      median_pair_distance_km = if (nrow(pairs)) {
        stats::median(pairs$geographic_distance_km)
      } else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

transition_distance_matrix <- function(sites) {
  coordinates <- as.matrix(sites[, c("x_km", "y_km")])
  if (any(!is.finite(coordinates))) {
    stop("All site coordinates must be finite.", call. = FALSE)
  }
  as.matrix(stats::dist(coordinates))
}

transition_environment_matrix <- function(sites) {
  columns <- intersect(
    c(
      "elevation", "DOY", "Temperature_PC1", "precip_PC1",
      "TemperatureSeasonality", "PrecipSeasonality", "topo_PC1",
      "soil_PC1", "soil_PC2", "RSDS"
    ),
    names(sites)
  )
  matrix <- as.matrix(sites[, columns, drop = FALSE])
  complete_columns <- apply(matrix, 2, function(x) all(is.finite(x)))
  matrix <- matrix[, complete_columns, drop = FALSE]
  if (!ncol(matrix)) {
    stop("No complete environmental matching columns were available.", call. = FALSE)
  }
  matrix <- apply(matrix, 2, transition_z)
  if (is.null(dim(matrix))) matrix <- matrix(matrix, ncol = 1L)
  colnames(matrix) <- columns[complete_columns]
  matrix
}

transition_bombus_axes <- function(sites) {
  species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
  columns <- paste0("bee_", species)
  suitability <- as.matrix(sites[, columns, drop = FALSE])
  normalized <- apply(suitability, 2, transition_rank01)
  if (is.null(dim(normalized))) normalized <- matrix(normalized, ncol = 1L)
  colnames(normalized) <- paste0(species, "_rank01")
  normalized[!is.finite(normalized)] <- 0

  total <- rowSums(normalized)
  composition <- normalized / pmax(total, 1e-8)
  composition[total <= 0, ] <- 1 / ncol(composition)
  hellinger <- sqrt(composition)
  availability <- rowMeans(normalized)
  alpine_share <- rowSums(normalized[, 3:5, drop = FALSE]) / pmax(total, 1e-8)
  alpine_share[total <= 0] <- NA_real_

  list(
    normalized = normalized,
    hellinger = hellinger,
    availability = availability,
    alpine_share = alpine_share,
    interpretation = paste(
      "Each ENMeval suitability is converted to its nationwide within-species rank",
      "before species are combined; availability is relative suitability, not abundance or visitation"
    )
  )
}

transition_bombus_occurrence_axes <- function(sites, radius = c("50km", "100km")) {
  radius <- match.arg(radius)
  species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
  columns <- paste0("occ_", species, "_", radius)
  if (!all(columns %in% names(sites))) return(NULL)
  occurrence <- as.matrix(sites[, columns, drop = FALSE])
  normalized <- apply(occurrence, 2, transition_rank01)
  if (is.null(dim(normalized))) normalized <- matrix(normalized, ncol = 1L)
  colnames(normalized) <- paste0(species, "_occ_rank01_", radius)
  normalized[!is.finite(normalized)] <- 0
  total <- rowSums(normalized)
  composition <- normalized / pmax(total, 1e-8)
  composition[total <= 0, ] <- 1 / ncol(composition)
  list(
    normalized = normalized,
    hellinger = sqrt(composition),
    availability = rowMeans(normalized),
    alpine_share = rowSums(normalized[, 3:5, drop = FALSE]) / pmax(total, 1e-8),
    interpretation = paste(
      "response-blind target-group occurrence kernel ranks for the five study species;",
      "a sampling-sensitive secondary proxy, not abundance, visitation, or selection pressure"
    )
  )
}

transition_pair_distance <- function(matrix, i, j) {
  sqrt(rowSums((matrix[i, , drop = FALSE] - matrix[j, , drop = FALSE])^2))
}

transition_local_neighbourhoods <- function(sites, radii_km = c(10, 25, 50, 100)) {
  geographic <- transition_distance_matrix(sites)
  environmental <- transition_environment_matrix(sites)
  bombus <- transition_bombus_axes(sites)
  homogeneous <- sites$site_class %in% c("white", "pigmented")
  pigment <- ifelse(sites$site_class == "pigmented", 1, 0)

  output <- lapply(radii_km, function(radius_km) {
    rows <- lapply(seq_len(nrow(sites)), function(i) {
      neighbours <- which(
        homogeneous & seq_len(nrow(sites)) != i & geographic[i, ] <= radius_km
      )
      if (!length(neighbours)) {
        return(data.frame(
          exact_site_id = sites$exact_site_id[i], radius_km = radius_km,
          n_neighbours = 0L, neighbour_pigment_share = NA_real_,
          flower_transition_score = NA_real_, mean_environment_turnover = NA_real_,
          mean_bombus_turnover = NA_real_, mean_bombus_availability = NA_real_,
          focal_against_background = FALSE, stringsAsFactors = FALSE
        ))
      }
      p <- mean(pigment[neighbours])
      bombus_turnover <- transition_pair_distance(
        bombus$hellinger, rep(i, length(neighbours)), neighbours
      ) / sqrt(2)
      environment_turnover <- transition_pair_distance(
        environmental, rep(i, length(neighbours)), neighbours
      ) / sqrt(ncol(environmental))
      against_background <- homogeneous[i] && length(neighbours) >= 5L && (
        (pigment[i] == 1L && p <= 0.1) || (pigment[i] == 0L && p >= 0.9)
      )
      data.frame(
        exact_site_id = sites$exact_site_id[i], radius_km = radius_km,
        n_neighbours = length(neighbours), neighbour_pigment_share = p,
        flower_transition_score = 4 * p * (1 - p),
        mean_environment_turnover = mean(environment_turnover),
        mean_bombus_turnover = mean(bombus_turnover),
        mean_bombus_availability = mean(bombus$availability[neighbours]),
        focal_against_background = against_background,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  })
  result <- do.call(rbind, output)
  rownames(result) <- NULL
  result
}

transition_nearest_opposite <- function(sites, radii_km = c(10, 25, 50, 100)) {
  geographic <- transition_distance_matrix(sites)
  homogeneous <- sites$site_class %in% c("white", "pigmented")
  class <- sites$site_class
  nearest <- rep(NA_real_, nrow(sites))
  for (i in which(homogeneous)) {
    candidates <- which(homogeneous & class != class[i])
    if (length(candidates)) nearest[i] <- min(geographic[i, candidates])
  }
  site_output <- data.frame(
    exact_site_id = sites$exact_site_id,
    site_class = class,
    nearest_opposite_km = nearest,
    stringsAsFactors = FALSE
  )
  summary <- do.call(rbind, lapply(radii_km, function(radius_km) {
    data.frame(
      radius_km = radius_km,
      homogeneous_sites = sum(homogeneous),
      sites_with_opposite = sum(nearest <= radius_km, na.rm = TRUE),
      pigmented_sites_with_white = sum(
        class == "pigmented" & nearest <= radius_km, na.rm = TRUE
      ),
      white_sites_with_pigmented = sum(
        class == "white" & nearest <= radius_km, na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  }))
  list(sites = site_output, summary = summary)
}

transition_local_edges <- function(sites, k = 5L, maximum_km = 100) {
  geographic <- transition_distance_matrix(sites)
  environmental <- transition_environment_matrix(sites)
  bombus <- transition_bombus_axes(sites)
  homogeneous <- which(sites$site_class %in% c("white", "pigmented"))
  edge_keys <- character()
  for (i in homogeneous) {
    candidates <- setdiff(homogeneous, i)
    candidates <- candidates[geographic[i, candidates] <= maximum_km]
    if (!length(candidates)) next
    candidates <- candidates[order(geographic[i, candidates])]
    candidates <- head(candidates, k)
    edge_keys <- c(
      edge_keys,
      vapply(candidates, function(j) paste(sort(c(i, j)), collapse = "-"), character(1))
    )
  }
  edge_keys <- unique(edge_keys)
  if (!length(edge_keys)) return(data.frame())
  indices <- do.call(rbind, strsplit(edge_keys, "-", fixed = TRUE))
  i <- as.integer(indices[, 1])
  j <- as.integer(indices[, 2])
  bombus_turnover <- transition_pair_distance(bombus$hellinger, i, j) / sqrt(2)
  environment_turnover <- transition_pair_distance(environmental, i, j) /
    sqrt(ncol(environmental))
  output <- data.frame(
    edge_id = paste0("edge-", seq_along(i)),
    site_i = sites$exact_site_id[i],
    site_j = sites$exact_site_id[j],
    class_i = sites$site_class[i],
    class_j = sites$site_class[j],
    flower_discordant = as.integer(sites$site_class[i] != sites$site_class[j]),
    geographic_distance_km = geographic[cbind(i, j)],
    environmental_distance = environment_turnover,
    bombus_composition_turnover = bombus_turnover,
    bombus_availability_mean = rowMeans(cbind(
      bombus$availability[i], bombus$availability[j]
    )),
    bombus_availability_difference = abs(
      bombus$availability[i] - bombus$availability[j]
    ),
    alpine_share_difference = abs(bombus$alpine_share[i] - bombus$alpine_share[j]),
    midpoint_x_km = rowMeans(cbind(sites$x_km[i], sites$x_km[j])),
    midpoint_y_km = rowMeans(cbind(sites$y_km[i], sites$y_km[j])),
    stringsAsFactors = FALSE
  )
  for (radius in c("50km", "100km")) {
    occurrence <- transition_bombus_occurrence_axes(sites, radius)
    if (is.null(occurrence)) next
    output[[paste0("occurrence_composition_turnover_", radius)]] <-
      transition_pair_distance(occurrence$hellinger, i, j) / sqrt(2)
    output[[paste0("occurrence_availability_mean_", radius)]] <- rowMeans(cbind(
      occurrence$availability[i], occurrence$availability[j]
    ))
    output[[paste0("occurrence_availability_difference_", radius)]] <- abs(
      occurrence$availability[i] - occurrence$availability[j]
    )
  }
  output
}

transition_hotspot_pairs <- function(sites, edges, maximum_km = 25) {
  output <- edges[
    edges$flower_discordant == 1L & edges$geographic_distance_km <= maximum_km,
    , drop = FALSE
  ]
  if (!nrow(output)) return(output)
  site_index <- setNames(seq_len(nrow(sites)), sites$exact_site_id)
  i <- unname(site_index[output$site_i])
  j <- unname(site_index[output$site_j])
  output$both_classifications_high_confidence <-
    sites$all_classifications_high_confidence[i] &
    sites$all_classifications_high_confidence[j]
  output$binary_transition_gradient_per_km <-
    1 / pmax(output$geographic_distance_km, 0.1)
  output$longitude_i <- sites$longitude[i]
  output$latitude_i <- sites$latitude[i]
  output$longitude_j <- sites$longitude[j]
  output$latitude_j <- sites$latitude[j]
  output$colour_a_i <- sites$colour_a[i]
  output$colour_a_j <- sites$colour_a[j]
  output$natural_presence_probability_i <- sites$natural_presence_probability[i]
  output$natural_presence_probability_j <- sites$natural_presence_probability[j]
  ordering <- order(
    -as.integer(output$both_classifications_high_confidence),
    output$geographic_distance_km,
    output$environmental_distance
  )
  output <- output[ordering, , drop = FALSE]
  output$hotspot_rank <- seq_len(nrow(output))
  used <- character()
  output$independent_pair <- FALSE
  for (row in seq_len(nrow(output))) {
    endpoints <- c(output$site_i[row], output$site_j[row])
    if (any(endpoints %in% used)) next
    output$independent_pair[row] <- TRUE
    used <- c(used, endpoints)
  }
  rownames(output) <- NULL
  output
}

transition_matched_pairs <- function(sites, maximum_km = 25,
                                     environment_caliper = 0.75) {
  geographic <- transition_distance_matrix(sites)
  environmental <- transition_environment_matrix(sites)
  bombus <- transition_bombus_axes(sites)
  pigmented <- which(sites$site_class == "pigmented")
  white <- which(sites$site_class == "white")
  environmental_distance <- as.matrix(stats::dist(environmental)) /
    sqrt(ncol(environmental))
  candidate_grid <- which(
    geographic[pigmented, white, drop = FALSE] <= maximum_km &
      environmental_distance[pigmented, white, drop = FALSE] <= environment_caliper,
    arr.ind = TRUE
  )
  if (!nrow(candidate_grid)) return(data.frame())
  candidate_i <- pigmented[candidate_grid[, 1]]
  candidate_j <- white[candidate_grid[, 2]]
  candidate_geo <- geographic[cbind(candidate_i, candidate_j)]
  candidate_env <- environmental_distance[cbind(candidate_i, candidate_j)]
  score <- candidate_geo / maximum_km + candidate_env / environment_caliper
  order_index <- order(score, candidate_geo, candidate_env)

  used_i <- rep(FALSE, nrow(sites))
  used_j <- rep(FALSE, nrow(sites))
  selected <- integer()
  for (candidate in order_index) {
    i <- candidate_i[candidate]
    j <- candidate_j[candidate]
    if (used_i[i] || used_j[j]) next
    selected <- c(selected, candidate)
    used_i[i] <- TRUE
    used_j[j] <- TRUE
  }
  i <- candidate_i[selected]
  j <- candidate_j[selected]
  if (!length(i)) return(data.frame())
  output <- data.frame(
    pair_id = paste0("pair-", seq_along(i)),
    pigmented_site = sites$exact_site_id[i],
    white_site = sites$exact_site_id[j],
    maximum_km = maximum_km,
    environment_caliper = environment_caliper,
    geographic_distance_km = geographic[cbind(i, j)],
    environmental_distance = environmental_distance[cbind(i, j)],
    bombus_composition_turnover = transition_pair_distance(
      bombus$hellinger, i, j
    ) / sqrt(2),
    delta_bombus_availability = bombus$availability[i] - bombus$availability[j],
    delta_alpine_share = bombus$alpine_share[i] - bombus$alpine_share[j],
    stringsAsFactors = FALSE
  )
  for (column in colnames(bombus$normalized)) {
    output[[paste0("delta_", column)]] <-
      bombus$normalized[i, column] - bombus$normalized[j, column]
  }
  for (column in colnames(environmental)) {
    output[[paste0("delta_env_", column)]] <-
      environmental[i, column] - environmental[j, column]
  }
  for (column in intersect(c("Bombus_W", "Bombus_A"), names(sites))) {
    output[[paste0("delta_", column)]] <- sites[[column]][i] - sites[[column]][j]
  }
  output
}

transition_matching_sensitivity <- function(sites, radii_km = c(10, 25, 50),
                                            calipers = c(0.5, 0.75, 1)) {
  rows <- list()
  index <- 0L
  for (radius_km in radii_km) {
    for (caliper in calipers) {
      pairs <- transition_matched_pairs(sites, radius_km, caliper)
      index <- index + 1L
      environment_columns <- grep("^delta_env_", names(pairs), value = TRUE)
      mean_abs_environment_difference <- if (nrow(pairs) && length(environment_columns)) {
        mean(abs(as.matrix(pairs[, environment_columns, drop = FALSE])))
      } else {
        NA_real_
      }
      rows[[index]] <- data.frame(
        maximum_km = radius_km,
        environment_caliper = caliper,
        n_independent_pairs = nrow(pairs),
        median_geographic_distance_km = if (nrow(pairs)) {
          stats::median(pairs$geographic_distance_km)
        } else NA_real_,
        median_environmental_distance = if (nrow(pairs)) {
          stats::median(pairs$environmental_distance)
        } else NA_real_,
        mean_absolute_environment_z_difference = mean_abs_environment_difference,
        mean_delta_bombus_availability = if (nrow(pairs)) {
          mean(pairs$delta_bombus_availability)
        } else NA_real_,
        mean_delta_alpine_share = if (nrow(pairs)) {
          mean(pairs$delta_alpine_share, na.rm = TRUE)
        } else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

transition_spatial_block_summary <- function(sites, neighbourhoods, edges,
                                             radius_km = 25,
                                             block_size_km = 100) {
  local <- neighbourhoods[neighbourhoods$radius_km == radius_km, , drop = FALSE]
  site_local <- merge(sites, local, by = "exact_site_id", all.x = TRUE, sort = FALSE)
  site_local$transition_block <- paste(
    floor(site_local$x_km / block_size_km),
    floor(site_local$y_km / block_size_km), sep = "_"
  )
  site_groups <- split(seq_len(nrow(site_local)), site_local$transition_block)
  site_summary <- do.call(rbind, lapply(site_groups, function(index) {
    block <- site_local[index, , drop = FALSE]
    data.frame(
      transition_block = block$transition_block[1],
      n_sites = nrow(block),
      n_mixed_analysis_units = sum(block$site_class == "mixed"),
      n_eligible_neighbourhoods = sum(block$n_neighbours >= 5L, na.rm = TRUE),
      n_transition_neighbourhoods = sum(
        block$n_neighbours >= 5L & block$flower_transition_score >= 0.5,
        na.rm = TRUE
      ),
      n_against_background = sum(block$focal_against_background, na.rm = TRUE),
      mean_bombus_turnover = mean(block$mean_bombus_turnover, na.rm = TRUE),
      mean_environment_turnover = mean(block$mean_environment_turnover, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  edge_block <- paste(
    floor(edges$midpoint_x_km / block_size_km),
    floor(edges$midpoint_y_km / block_size_km), sep = "_"
  )
  eligible_edges <- edges$geographic_distance_km <= radius_km
  edge_counts <- aggregate(
    cbind(
      n_local_edges = as.integer(eligible_edges),
      n_discordant_edges = as.integer(eligible_edges & edges$flower_discordant == 1L)
    ),
    by = list(transition_block = edge_block), sum
  )
  merge(site_summary, edge_counts, by = "transition_block", all.x = TRUE, sort = FALSE)
}

transition_binary_auc <- function(observed, predicted) {
  keep <- is.finite(observed) & is.finite(predicted)
  observed <- as.integer(observed[keep])
  predicted <- predicted[keep]
  positives <- sum(observed == 1L)
  negatives <- sum(observed == 0L)
  if (!positives || !negatives) return(NA_real_)
  ranks <- rank(predicted, ties.method = "average")
  (sum(ranks[observed == 1L]) - positives * (positives + 1) / 2) /
    (positives * negatives)
}

transition_background_formula <- function(sites, k_space = 40L) {
  terms <- intersect(
    c(
      "elevation", "DOY", "Temperature_PC1", "precip_PC1",
      "TemperatureSeasonality", "PrecipSeasonality", "topo_PC1",
      "soil_PC1", "soil_PC2", "RSDS"
    ),
    names(sites)
  )
  k_use <- max(10L, min(as.integer(k_space), floor(nrow(sites) / 20)))
  stats::as.formula(paste(
    "pigment_share ~", paste(terms, collapse = " + "),
    "+ s(x_km, y_km, k =", k_use, ")"
  ), env = asNamespace("mgcv"))
}

transition_fit_background_probability <- function(train_sites, predict_sites,
                                                  k_space = 40L) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required for transition background models.", call. = FALSE)
  }
  formula <- transition_background_formula(train_sites, k_space)
  train_sites$.transition_weights <- train_sites$n_observations
  warnings <- character()
  fit <- withCallingHandlers(
    mgcv::gam(
      formula, data = train_sites, weights = .transition_weights,
      family = stats::binomial(), method = "REML"
    ),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  probability <- as.numeric(stats::predict(fit, newdata = predict_sites, type = "response"))
  list(
    probability = pmin(pmax(probability, 1e-8), 1 - 1e-8),
    formula = paste(deparse(formula), collapse = " "),
    warnings = paste(unique(warnings), collapse = " | ")
  )
}

transition_edge_spatial_cv <- function(sites, edges, maximum_km = 25) {
  site_fold <- setNames(as.integer(round(sites$spatial_fold)), sites$exact_site_id)
  data <- edges[edges$geographic_distance_km <= maximum_km, , drop = FALSE]
  data$fold_i <- unname(site_fold[data$site_i])
  data$fold_j <- unname(site_fold[data$site_j])
  folds <- sort(unique(c(data$fold_i, data$fold_j)))
  folds <- folds[is.finite(folds)]
  base_formula <- stats::as.formula(paste(
    "flower_discordant ~ log1p(geographic_distance_km) +",
    "environmental_distance + expected_background_discordance"
  ))
  full_formula <- stats::update.formula(
    base_formula,
    paste(
      ". ~ . + bombus_composition_turnover + bombus_availability_mean +",
      "bombus_availability_difference"
    )
  )
  composition_formula <- stats::update.formula(
    base_formula, ". ~ . + bombus_composition_turnover"
  )
  availability_formula <- stats::update.formula(
    base_formula,
    ". ~ . + bombus_availability_mean + bombus_availability_difference"
  )
  availability_level_formula <- stats::update.formula(
    base_formula, ". ~ . + bombus_availability_mean"
  )
  availability_gradient_formula <- stats::update.formula(
    base_formula, ". ~ . + bombus_availability_difference"
  )
  rows <- list()
  index <- 0L
  for (fold in folds) {
    train <- data[data$fold_i != fold & data$fold_j != fold, , drop = FALSE]
    test <- data[data$fold_i == fold & data$fold_j == fold, , drop = FALSE]
    if (nrow(train) < 50L || nrow(test) < 10L ||
        length(unique(train$flower_discordant)) < 2L) next
    background <- transition_fit_background_probability(
      sites[sites$spatial_fold != fold, , drop = FALSE], sites
    )
    site_probability <- setNames(background$probability, sites$exact_site_id)
    add_background <- function(edge_data) {
      probability_i <- unname(site_probability[edge_data$site_i])
      probability_j <- unname(site_probability[edge_data$site_j])
      edge_data$expected_background_discordance <-
        probability_i * (1 - probability_j) +
        (1 - probability_i) * probability_j
      edge_data
    }
    train <- add_background(train)
    test <- add_background(test)
    formulas <- list(
      base = base_formula,
      enmeval_composition = composition_formula,
      enmeval_availability_level = availability_level_formula,
      enmeval_availability_gradient = availability_gradient_formula,
      enmeval_availability = availability_formula,
      enmeval_full = full_formula
    )
    if (all(c(
      "occurrence_composition_turnover_50km",
      "occurrence_availability_mean_50km",
      "occurrence_availability_difference_50km"
    ) %in% names(train))) {
      formulas$occurrence_full_50km <- stats::update.formula(
        base_formula,
        paste(
          ". ~ . + occurrence_composition_turnover_50km +",
          "occurrence_availability_mean_50km +",
          "occurrence_availability_difference_50km"
        )
      )
    }
    if (all(c(
      "occurrence_composition_turnover_100km",
      "occurrence_availability_mean_100km",
      "occurrence_availability_difference_100km"
    ) %in% names(train))) {
      formulas$occurrence_full_100km <- stats::update.formula(
        base_formula,
        paste(
          ". ~ . + occurrence_composition_turnover_100km +",
          "occurrence_availability_mean_100km +",
          "occurrence_availability_difference_100km"
        )
      )
    }
    fits <- lapply(formulas, function(formula) {
      stats::glm(formula, data = train, family = stats::binomial())
    })
    for (model_name in names(fits)) {
      probability <- as.numeric(stats::predict(
        fits[[model_name]], newdata = test, type = "response"
      ))
      probability <- pmin(pmax(probability, 1e-8), 1 - 1e-8)
      observed <- test$flower_discordant
      index <- index + 1L
      rows[[index]] <- data.frame(
        maximum_km = maximum_km,
        heldout_spatial_fold = fold,
        model = model_name,
        n_train_edges = nrow(train),
        n_test_edges = nrow(test),
        heldout_log_loss = -mean(
          observed * log(probability) + (1 - observed) * log(1 - probability)
        ),
        heldout_brier = mean((observed - probability)^2),
        heldout_auc = transition_binary_auc(observed, probability),
        strict_node_disjoint_split = TRUE,
        background_formula = background$formula,
        background_warnings = background$warnings,
        background_excludes_bombus_region_human = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

transition_edge_cv_summary <- function(cv) {
  if (!nrow(cv)) return(data.frame())
  base <- cv[cv$model == "base", , drop = FALSE]
  model_names <- setdiff(unique(cv$model), "base")
  rows <- lapply(model_names, function(model_name) {
    added <- cv[cv$model == model_name, , drop = FALSE]
    paired <- merge(
      base, added, by = "heldout_spatial_fold", suffixes = c("_base", "_added")
    )
    weights <- paired$n_test_edges_base
    weighted <- function(x) sum(x * weights) / sum(weights)
    data.frame(
      model = model_name,
      n_folds = nrow(paired),
      n_heldout_edges = sum(weights),
      base_log_loss = weighted(paired$heldout_log_loss_base),
      added_log_loss = weighted(paired$heldout_log_loss_added),
      delta_log_loss_base_minus_added = weighted(
        paired$heldout_log_loss_base - paired$heldout_log_loss_added
      ),
      base_brier = weighted(paired$heldout_brier_base),
      added_brier = weighted(paired$heldout_brier_added),
      delta_brier_base_minus_added = weighted(
        paired$heldout_brier_base - paired$heldout_brier_added
      ),
      base_auc = weighted(paired$heldout_auc_base),
      added_auc = weighted(paired$heldout_auc_added),
      delta_auc_added_minus_base = weighted(
        paired$heldout_auc_added - paired$heldout_auc_base
      ),
      interpretation = paste(
        "positive log-loss and Brier deltas or positive AUC delta favour added terms;",
        "test edges share no endpoint site with training edges in the held-out fold"
      ),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

transition_enclave_descriptive <- function(candidates) {
  groups <- c("pigmented_in_white_background", "white_in_pigmented_background")
  variables <- intersect(
    c(
      "DOY", "early_score", "z_H", "z_R", "z_A", "road_access",
      "human_population", "colour_a", "intensity_natural_residual"
    ),
    names(candidates)
  )
  thresholds <- lapply(variables, function(column) {
    stats::quantile(candidates[[column]], 0.9, na.rm = TRUE, names = FALSE)
  })
  names(thresholds) <- variables
  rows <- list()
  index <- 0L
  for (group in groups) {
    block <- candidates[candidates$candidate_direction == group, , drop = FALSE]
    for (column in variables) {
      values <- as.numeric(block[[column]])
      index <- index + 1L
      rows[[index]] <- data.frame(
        candidate_direction = group,
        variable = column,
        n_finite = sum(is.finite(values)),
        mean = mean(values, na.rm = TRUE),
        median = stats::median(values, na.rm = TRUE),
        n_in_nationwide_top_decile = sum(values >= thresholds[[column]], na.rm = TRUE),
        comparison_role = ifelse(
          group == "white_in_pigmented_background",
          "symmetric negative-control direction", "direction predicted by horticultural hypothesis"
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

transition_candidate_table <- function(sites, neighbourhoods, radius_km = 25) {
  local <- neighbourhoods[neighbourhoods$radius_km == radius_km, , drop = FALSE]
  merged <- merge(sites, local, by = "exact_site_id", all.x = TRUE, sort = FALSE)
  homogeneous <- merged$site_class %in% c("white", "pigmented")
  unexpected_pigmented <- homogeneous & merged$site_class == "pigmented" &
    merged$n_neighbours >= 5L & merged$neighbour_pigment_share <= 0.1
  unexpected_white <- homogeneous & merged$site_class == "white" &
    merged$n_neighbours >= 5L & merged$neighbour_pigment_share >= 0.9
  merged$candidate_direction <- ifelse(
    unexpected_pigmented, "pigmented_in_white_background",
    ifelse(unexpected_white, "white_in_pigmented_background", "not_enclave")
  )
  merged$horticultural_candidate_only <- unexpected_pigmented &
    merged$all_classifications_high_confidence
  merged$interpretation <- ifelse(
    merged$horticultural_candidate_only,
    paste(
      "candidate only: provenance requires independent evidence; evaluate",
      "early flowering, intensity, human access, replication, and image metadata"
    ),
    "not classified as a horticultural candidate"
  )
  merged
}

transition_feasibility_summary <- function(sites, neighbourhoods, edges, candidates) {
  radii <- sort(unique(neighbourhoods$radius_km))
  local_rows <- do.call(rbind, lapply(radii, function(radius_km) {
    block <- neighbourhoods[
      neighbourhoods$radius_km == radius_km & neighbourhoods$n_neighbours >= 5L,
      , drop = FALSE
    ]
    data.frame(
      metric = c(
        "eligible_local_neighbourhoods", "flower_transition_score_ge_0.5",
        "focal_against_90pct_background"
      ),
      radius_km = radius_km,
      value = c(
        nrow(block), sum(block$flower_transition_score >= 0.5, na.rm = TRUE),
        sum(block$focal_against_background, na.rm = TRUE)
      ),
      stringsAsFactors = FALSE
    )
  }))
  edge_rows <- do.call(rbind, lapply(radii, function(radius_km) {
    block <- edges[edges$geographic_distance_km <= radius_km, , drop = FALSE]
    data.frame(
      metric = c("local_edges", "discordant_flower_edges"),
      radius_km = radius_km,
      value = c(nrow(block), sum(block$flower_discordant == 1L)),
      stringsAsFactors = FALSE
    )
  }))
  global_rows <- data.frame(
    metric = c(
      "observations", "analysis_spatial_units", "mixed_analysis_units",
      "homogeneous_white_sites", "homogeneous_pigmented_sites",
      "pigmented_in_white_background_candidates",
      "white_in_pigmented_background_controls"
    ),
    radius_km = c(NA, NA, NA, NA, NA, 25, 25),
    value = c(
      sum(sites$n_observations), nrow(sites), sum(sites$site_class == "mixed"),
      sum(sites$site_class == "white"), sum(sites$site_class == "pigmented"),
      sum(candidates$candidate_direction == "pigmented_in_white_background"),
      sum(candidates$candidate_direction == "white_in_pigmented_background")
    ),
    stringsAsFactors = FALSE
  )
  rbind(global_rows, local_rows, edge_rows)
}
