v21_analysis_spec_version <- "v21.2_within_white_neighbourhood_human_contrast"

v21_require_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

v21_landuse_registry <- function() {
  data.frame(
    code = c("0100", "0200", "0700", "0901", "0902", "1000", "1600"),
    feature = c(
      "paddy_fraction", "other_agriculture_fraction",
      "built_up_fraction", "road_land_fraction", "railway_fraction",
      "other_artificial_fraction", "golf_course_fraction"
    ),
    interpretation = c(
      "rice paddy", "other agricultural land", "built-up land",
      "road land", "railway land", "other artificial land",
      "golf course"
    ),
    stringsAsFactors = FALSE
  )
}

v21_aggregate_landuse_classes_dbf <- function(data) {
  v21_require_columns(data, c("L03b_001", "L03b_002"), "MLIT DBF")
  mesh10 <- as.character(data$L03b_001)
  code <- as.character(data$L03b_002)
  if (any(nchar(mesh10) != 10L | grepl("[^0-9]", mesh10))) {
    stop("L03b_001 must contain ten-digit 100 m mesh codes.",
         call. = FALSE)
  }
  if (anyDuplicated(mesh10)) {
    stop("MLIT DBF contains duplicated 100 m mesh codes.", call. = FALSE)
  }
  mesh8 <- substr(mesh10, 1L, 8L)
  keys <- sort(unique(mesh8))
  sum_by_mesh <- function(value) {
    aggregated <- rowsum(value, mesh8, reorder = TRUE)
    as.numeric(aggregated[keys, 1L])
  }
  registry <- v21_landuse_registry()
  out <- data.frame(
    mesh_1km = keys,
    represented_fraction = sum_by_mesh(rep.int(1, length(mesh8))) / 100,
    stringsAsFactors = FALSE
  )
  for (index in seq_len(nrow(registry))) {
    out[[registry$feature[index]]] <-
      sum_by_mesh(as.numeric(code == registry$code[index])) / 100
  }
  classified <- code %in% registry$code
  out$other_land_fraction <-
    sum_by_mesh(as.numeric(!classified)) / 100
  if (exists("decode_tertiary_mesh_centres", mode = "function")) {
    centres <- decode_tertiary_mesh_centres(out$mesh_1km)
    out$longitude <- centres$longitude
    out$latitude <- centres$latitude
  }
  out
}

v21_read_mlit_archive <- function(primary_mesh, cache_dir) {
  if (!requireNamespace("foreign", quietly = TRUE)) {
    stop("Package 'foreign' is required.", call. = FALSE)
  }
  archive <- file.path(
    cache_dir,
    sprintf("L03-b-21_%s-jgd2011_GML.zip", primary_mesh)
  )
  if (!file.exists(archive)) {
    stop("Missing cached MLIT archive: ", archive, call. = FALSE)
  }
  listing <- utils::unzip(archive, list = TRUE)
  dbf_name <- listing$Name[
    grepl("\\.dbf$", listing$Name, ignore.case = TRUE)
  ]
  if (length(dbf_name) != 1L) {
    stop("Expected exactly one DBF in ", archive, call. = FALSE)
  }
  extract_dir <- tempfile(paste0("mlit_classes_", primary_mesh, "_"))
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(
    archive, files = dbf_name, exdir = extract_dir, junkpaths = TRUE
  )
  foreign::read.dbf(
    file.path(extract_dir, basename(dbf_name)), as.is = TRUE
  )
}

v21_process_mlit_classes <- function(
    primary_meshes, cache_dir, processed_dir) {
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
  rows <- vector("list", length(primary_meshes))
  for (index in seq_along(primary_meshes)) {
    primary_mesh <- as.character(primary_meshes[index])
    processed_path <- file.path(
      processed_dir, paste0(primary_mesh, "_classes_1km.rds")
    )
    if (file.exists(processed_path)) {
      block <- readRDS(processed_path)
    } else {
      block <- v21_aggregate_landuse_classes_dbf(
        v21_read_mlit_archive(primary_mesh, cache_dir)
      )
      saveRDS(block, processed_path, compress = "xz")
    }
    block$primary_mesh <- primary_mesh
    rows[[index]] <- block
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  if (anyDuplicated(out$mesh_1km)) {
    stop("Class-specific MLIT output has duplicated 1-km mesh keys.",
         call. = FALSE)
  }
  out
}

v21_lookup_landuse_cells <- function(
    observations, class_cells, reference_raster) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required.", call. = FALSE)
  }
  v21_require_columns(
    observations,
    c("exact_site_id", "longitude", "latitude", "x_km", "y_km"),
    "observations"
  )
  registry <- v21_landuse_registry()
  feature_names <- c(registry$feature, "other_land_fraction")
  v21_require_columns(
    class_cells, c("longitude", "latitude", feature_names),
    "class cells"
  )
  raster <- if (inherits(reference_raster, "SpatRaster")) {
    reference_raster
  } else {
    terra::rast(reference_raster)
  }
  coordinates <- as.matrix(observations[, c("longitude", "latitude")])
  raster_cell <- terra::cellFromXY(raster, coordinates)
  raster_center <- terra::xyFromCell(raster, raster_cell)
  observation_key <- paste(
    round(raster_center[, 1L], 7), round(raster_center[, 2L], 7)
  )
  lookup_key <- paste(
    round(as.numeric(class_cells$longitude), 7),
    round(as.numeric(class_cells$latitude), 7)
  )
  lookup_index <- match(observation_key, lookup_key)
  linked <- data.frame(
    source_exact_site_id = as.character(observations$exact_site_id),
    exact_site_id = paste0(
      "cell-1km-", floor(as.numeric(observations$x_km)), "_",
      floor(as.numeric(observations$y_km))
    ),
    lookup_index = lookup_index,
    stringsAsFactors = FALSE
  )
  linked <- cbind(
    linked,
    class_cells[lookup_index, feature_names, drop = FALSE]
  )

  site_groups <- split(
    seq_len(nrow(linked)),
    paste(
      linked$exact_site_id, linked$source_exact_site_id, sep = "::"
    )
  )
  site_rows <- lapply(site_groups, function(indices) {
    block <- linked[indices, , drop = FALSE]
    values <- vapply(feature_names, function(feature) {
      value <- as.numeric(block[[feature]])
      if (any(is.finite(value))) {
        stats::median(value[is.finite(value)])
      } else {
        NA_real_
      }
    }, numeric(1))
    data.frame(
      exact_site_id = block$exact_site_id[1L],
      source_exact_site_id = block$source_exact_site_id[1L],
      as.list(values), check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  sites <- do.call(rbind, site_rows)
  cell_groups <- split(seq_len(nrow(sites)), sites$exact_site_id)
  cell_rows <- lapply(cell_groups, function(indices) {
    block <- sites[indices, , drop = FALSE]
    values <- vapply(feature_names, function(feature) {
      value <- as.numeric(block[[feature]])
      if (any(is.finite(value))) mean(value[is.finite(value)]) else NA_real_
    }, numeric(1))
    data.frame(
      exact_site_id = block$exact_site_id[1L],
      n_landuse_sites = nrow(block),
      as.list(values), check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  cells <- do.call(rbind, cell_rows)
  rownames(cells) <- NULL
  list(
    cells = cells,
    sites = sites,
    linked = linked,
    audit = data.frame(
      metric = c(
        "n_observations", "n_observations_linked",
        "n_independent_sites", "n_cells", "n_cells_complete_classes"
      ),
      value = c(
        nrow(linked), sum(!is.na(linked$lookup_index)),
        nrow(sites), nrow(cells),
        sum(stats::complete.cases(cells[, feature_names, drop = FALSE]))
      ),
      stringsAsFactors = FALSE
    )
  )
}

v21_add_human_features <- function(features, class_features) {
  registry <- v21_landuse_registry()
  required <- c(
    "exact_site_id", "local_population_rank", "regional_population_rank",
    "forest_human_edge_rank", "forest_cover_rank",
    "road_proximity_rank", "mountainness_rank", "remote_mountain_score"
  )
  v21_require_columns(features, required, "features")
  v21_require_columns(
    class_features, c("exact_site_id", registry$feature),
    "class features"
  )
  index <- match(features$exact_site_id, class_features$exact_site_id)
  out <- features
  for (feature in registry$feature) {
    out[[feature]] <- as.numeric(class_features[[feature]][index])
    out[[paste0(feature, "_rank")]] <- v19_rank01(out[[feature]])
  }
  out$agriculture_fraction <-
    out$paddy_fraction + out$other_agriculture_fraction
  out$transport_land_fraction <-
    out$road_land_fraction + out$railway_fraction
  out$artificial_land_fraction <-
    out$built_up_fraction + out$road_land_fraction +
    out$railway_fraction + out$other_artificial_fraction +
    out$golf_course_fraction
  for (feature in c(
    "agriculture_fraction", "transport_land_fraction",
    "artificial_land_fraction"
  )) {
    out[[paste0(feature, "_rank")]] <- v19_rank01(out[[feature]])
  }
  out$settlement_density_score <- rowMeans(out[, c(
    "local_population_rank", "regional_population_rank",
    "built_up_fraction_rank"
  )])
  out$transport_access_score <- rowMeans(out[, c(
    "road_proximity_rank", "road_land_fraction_rank",
    "railway_fraction_rank"
  )])
  out$cultivation_interface_score <- rowMeans(out[, c(
    "agriculture_fraction_rank", "forest_human_edge_rank",
    "road_proximity_rank"
  )])
  out$artificial_land_score <- rowMeans(out[, c(
    "built_up_fraction_rank", "road_land_fraction_rank",
    "railway_fraction_rank", "other_artificial_fraction_rank",
    "golf_course_fraction_rank"
  )])
  out$human_activity_consensus_score <- rowMeans(out[, c(
    "settlement_density_score", "transport_access_score",
    "cultivation_interface_score", "artificial_land_score"
  )])
  out
}

v21_feature_definitions <- function() {
  data.frame(
    feature = c(
      "local_population_rank", "regional_population_rank",
      "built_up_fraction_rank", "agriculture_fraction_rank",
      "road_proximity_rank", "forest_human_edge_rank",
      "settlement_density_score", "transport_access_score",
      "cultivation_interface_score", "artificial_land_score",
      "human_activity_consensus_score",
      "forest_cover_rank", "mountainness_rank", "remote_mountain_score"
    ),
    role = c(
      "population", "population", "settlement", "cultivation",
      "access", "human_natural_interface",
      "settlement_composite", "transport_composite",
      "cultivation_composite", "artificial_land_composite",
      "human_consensus",
      "natural_alternative", "natural_alternative",
      "natural_alternative_composite"
    ),
    hypothesis_direction = c(
      rep("greater", 11L), "two_sided", "two_sided", "less"
    ),
    stringsAsFactors = FALSE
  )
}

v21_core_features <- function() {
  c(
    "local_population_rank", "regional_population_rank",
    "built_up_fraction_rank", "agriculture_fraction_rank",
    "road_proximity_rank", "forest_human_edge_rank",
    "forest_cover_rank", "mountainness_rank"
  )
}

v21_local_contrasts <- function(
    present, candidate, graph, features, feature_names,
    analysis_support = NULL) {
  if (is.vector(present)) present <- matrix(present, ncol = 1L)
  if (is.vector(candidate)) candidate <- matrix(candidate, ncol = 1L)
  present <- as.matrix(present)
  candidate <- as.matrix(candidate)
  if (!identical(dim(present), dim(candidate))) {
    stop("present and candidate matrices must have identical dimensions.",
         call. = FALSE)
  }
  feature_matrix <- as.matrix(features[, feature_names, drop = FALSE])
  storage.mode(feature_matrix) <- "double"
  if (is.null(analysis_support)) {
    analysis_support <- stats::complete.cases(feature_matrix)
  }
  analysis_support <- as.logical(analysis_support)
  contrast <- matrix(
    NA_real_, nrow = ncol(present), ncol = length(feature_names),
    dimnames = list(NULL, feature_names)
  )
  n_requested <- integer(ncol(present))
  n_usable <- integer(ncol(present))
  details <- vector("list", ncol(present))
  for (draw in seq_len(ncol(present))) {
    cases <- which(candidate[, draw] & analysis_support)
    n_requested[draw] <- length(cases)
    rows <- list()
    for (case in cases) {
      white_neighbours <- graph$neighbours[[case]]
      white_neighbours <- white_neighbours[
        !present[white_neighbours, draw] &
          analysis_support[white_neighbours]
      ]
      if (!length(white_neighbours)) next
      difference <- feature_matrix[case, ] -
        colMeans(
          feature_matrix[white_neighbours, , drop = FALSE],
          na.rm = TRUE
        )
      rows[[length(rows) + 1L]] <- data.frame(
        case_index = case,
        n_white_neighbours = length(white_neighbours),
        as.list(difference), check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
    if (length(rows)) {
      block <- do.call(rbind, rows)
      contrast[draw, ] <- colMeans(
        block[, feature_names, drop = FALSE], na.rm = TRUE
      )
      n_usable[draw] <- nrow(block)
      details[[draw]] <- block
    }
  }
  list(
    contrast = contrast,
    n_requested = n_requested,
    n_usable = n_usable,
    details = details
  )
}

v21_static_local_spike <- function(
    graph, value, analysis_support = NULL) {
  value <- as.numeric(value)
  if (is.null(analysis_support)) analysis_support <- is.finite(value)
  analysis_support <- as.logical(analysis_support) & is.finite(value)
  out <- rep(NA_real_, length(value))
  for (index in seq_along(value)) {
    if (!analysis_support[index]) next
    neighbours <- graph$neighbours[[index]]
    neighbours <- neighbours[analysis_support[neighbours]]
    if (!length(neighbours)) next
    out[index] <- value[index] - mean(value[neighbours])
  }
  out
}

v21_convergence_summary <- function(
    observed_candidate, simulated_candidate,
    observed_q, simulated_q, spike, spike_feature,
    supported, spike_quantile = 0.90) {
  spike <- as.numeric(spike)
  eligible_spike <- is.finite(spike) & as.logical(supported)
  threshold <- unname(stats::quantile(
    spike[eligible_spike], spike_quantile, na.rm = TRUE
  ))
  high_spike <- eligible_spike & spike >= threshold
  observed_candidate <- as.logical(observed_candidate)
  simulated_candidate <- as.matrix(simulated_candidate)
  simulated_high <- matrix(
    high_spike, nrow(simulated_candidate), ncol(simulated_candidate)
  )
  observed <- c(
    candidate_human_spike_count =
      sum(observed_candidate & high_spike, na.rm = TRUE),
    candidate_q10_human_spike_count =
      sum(observed_candidate & high_spike & observed_q <= 0.10,
          na.rm = TRUE),
    candidate_q05_human_spike_count =
      sum(observed_candidate & high_spike & observed_q <= 0.05,
          na.rm = TRUE)
  )
  simulated <- cbind(
    candidate_human_spike_count =
      colSums(simulated_candidate & simulated_high),
    candidate_q10_human_spike_count =
      colSums(
        simulated_candidate & simulated_high & simulated_q <= 0.10
      ),
    candidate_q05_human_spike_count =
      colSums(
        simulated_candidate & simulated_high & simulated_q <= 0.05
      )
  )
  rows <- lapply(names(observed), function(metric) {
    comparison <- v18_null_comparison(
      observed[[metric]], simulated[, metric], "greater"
    )
    data.frame(
      spike_feature = spike_feature,
      spike_quantile = spike_quantile,
      spike_threshold = threshold,
      metric = metric,
      observed_value = observed[[metric]],
      n_null_draws = nrow(simulated),
      t(comparison),
      stringsAsFactors = FALSE
    )
  })
  list(
    summary = do.call(rbind, rows),
    null = data.frame(
      spike_feature = spike_feature,
      draw = seq_len(nrow(simulated)),
      simulated,
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    spike = spike,
    high_spike = high_spike,
    threshold = threshold
  )
}

v21_contrast_summary <- function(
    observed, simulated, definitions, configuration) {
  simulated <- as.matrix(
    simulated[, definitions$feature, drop = FALSE]
  )
  center <- colMeans(simulated, na.rm = TRUE)
  spread <- apply(simulated, 2, stats::sd, na.rm = TRUE)
  spread[!is.finite(spread) | spread <= 1e-12] <- 1
  null_z <- sweep(sweep(simulated, 2, center, "-"), 2, spread, "/")
  observed_z <- (
    observed[definitions$feature] - center
  ) / spread
  sign <- ifelse(definitions$hypothesis_direction == "less", -1, 1)
  finite_max <- function(value) {
    value <- value[is.finite(value)]
    if (length(value)) max(value) else NA_real_
  }
  maximum_directional <- apply(
    sweep(null_z, 2, sign, "*"), 1, finite_max
  )
  maximum_absolute <- apply(abs(null_z), 1, finite_max)
  rows <- lapply(seq_len(nrow(definitions)), function(index) {
    feature <- definitions$feature[index]
    null <- simulated[, feature]
    null <- null[is.finite(null)]
    value <- observed[[feature]]
    upper <- (1 + sum(null >= value)) / (length(null) + 1)
    lower <- (1 + sum(null <= value)) / (length(null) + 1)
    direction <- definitions$hypothesis_direction[index]
    p <- if (direction == "less") {
      lower
    } else if (direction == "two_sided") {
      min(1, 2 * min(upper, lower))
    } else {
      upper
    }
    fwer <- if (direction == "two_sided") {
      (1 + sum(maximum_absolute >= abs(observed_z[index]),
               na.rm = TRUE)) /
        (sum(is.finite(maximum_absolute)) + 1)
    } else {
      (1 + sum(maximum_directional >= observed_z[index] * sign[index],
               na.rm = TRUE)) /
        (sum(is.finite(maximum_directional)) + 1)
    }
    data.frame(
      configuration = configuration,
      feature = feature,
      role = definitions$role[index],
      hypothesis_direction = direction,
      observed_focal_minus_white_neighbour = value,
      null_mean = mean(null),
      null_sd = stats::sd(null),
      lower_95 = unname(stats::quantile(null, 0.025)),
      upper_95 = unname(stats::quantile(null, 0.975)),
      directional_or_two_sided_p = p,
      two_sided_p = min(1, 2 * min(upper, lower)),
      maxT_FWER_p = fwer,
      observed_standardized_departure = observed_z[index],
      n_null_draws = length(null),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
