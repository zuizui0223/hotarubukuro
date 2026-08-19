# Minimal builders for the frozen 1-km analysis-cell table used by the paper.
#
# This file intentionally contains only the transformations that feed the final
# Broad, local-Bombus and continuous-isolation analyses.  Earlier hotspot,
# candidate-ranking and horticulture scoring code is not part of this contract.

ac_require_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

ac_finite_mean <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

ac_first_text <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x)) x[[1L]] else NA_character_
}

ac_rank01 <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (sum(keep) == 1L) {
    out[keep] <- 1
  } else if (sum(keep) > 1L) {
    out[keep] <- (rank(x[keep], ties.method = "average") - 1) / (sum(keep) - 1)
  }
  out
}

ac_positive_rank <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (any(keep)) out[keep] <- rank(x[keep], ties.method = "average") / sum(keep)
  out
}

ac_site_table <- function(data) {
  required <- c(
    "exact_site_id", "longitude", "latitude", "x_km", "y_km",
    "pigmented_mixture50", "pigmented_high_confidence", "colour_a", "DOY",
    "bee_ardens", "bee_diversus", "bee_beaticola", "bee_consobrinus",
    "bee_honshuensis"
  )
  ac_require_columns(data, required, "observation table")
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
      "bee_honshuensis", "human_population", "spatial_fold", "block_x",
      "block_y", "natural_presence_probability", "early_score", "colour_a",
      "pigment_intensity_z"
    ),
    names(data)
  )
  rows <- lapply(groups, function(index) {
    block <- data[index, , drop = FALSE]
    pigment <- as.integer(block$pigmented_mixture50)
    n_pigmented <- sum(pigment == 1L, na.rm = TRUE)
    n_white <- sum(pigment == 0L, na.rm = TRUE)
    row <- data.frame(
      exact_site_id = ac_first_text(block$exact_site_id),
      n_observations = nrow(block),
      n_pigmented = n_pigmented,
      n_white = n_white,
      pigment_share = n_pigmented / max(1, n_pigmented + n_white),
      site_class = if (n_pigmented > 0L && n_white > 0L) {
        "mixed"
      } else if (n_pigmented > 0L) {
        "pigmented"
      } else {
        "white"
      },
      stringsAsFactors = FALSE
    )
    for (column in numeric_columns) row[[column]] <- ac_finite_mean(block[[column]])
    row
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

ac_population_table <- function(data, cell_km = 1) {
  if (!is.finite(cell_km) || cell_km <= 0) stop("cell_km must be positive", call. = FALSE)
  original_site <- as.character(data$exact_site_id)
  cell_x <- floor(as.numeric(data$x_km) / cell_km)
  cell_y <- floor(as.numeric(data$y_km) / cell_km)
  cell_id <- paste0("cell-", format(cell_km, trim = TRUE), "km-", cell_x, "_", cell_y)
  tmp <- data
  tmp$exact_site_id <- cell_id
  cells <- ac_site_table(tmp)
  exact_site_counts <- vapply(split(original_site, cell_id), function(x) {
    length(unique(x))
  }, integer(1))
  cells$n_exact_sites <- unname(exact_site_counts[cells$exact_site_id])
  cells$observed_x_km <- cells$x_km
  cells$observed_y_km <- cells$y_km
  cells$x_km <- (floor(cells$observed_x_km / cell_km) + 0.5) * cell_km
  cells$y_km <- (floor(cells$observed_y_km / cell_km) + 0.5) * cell_km
  cells$spatial_unit_km <- cell_km
  cells
}

ac_cell_phenotypes <- function(observations, cells) {
  required <- c(
    "exact_site_id", "x_km", "y_km", "pigmented_mixture50",
    "pigment_intensity_z", "DOY", "year"
  )
  ac_require_columns(observations, required, "phenotype observations")
  observations$cell_id <- paste0(
    "cell-1km-", floor(observations$x_km), "_", floor(observations$y_km)
  )
  site_groups <- split(
    seq_len(nrow(observations)),
    paste(observations$cell_id, observations$exact_site_id, sep = "::")
  )
  site_rows <- lapply(site_groups, function(index) {
    block <- observations[index, , drop = FALSE]
    pigmented <- block$pigmented_mixture50 == 1L
    data.frame(
      cell_id = block$cell_id[[1L]],
      source_exact_site_id = as.character(block$exact_site_id[[1L]]),
      site_intensity = if (any(pigmented)) {
        stats::median(block$pigment_intensity_z[pigmented], na.rm = TRUE)
      } else NA_real_,
      site_DOY = stats::median(block$DOY, na.rm = TRUE),
      site_year = stats::median(block$year, na.rm = TRUE),
      n_images = nrow(block),
      stringsAsFactors = FALSE
    )
  })
  site_data <- do.call(rbind, site_rows)
  groups <- split(seq_len(nrow(site_data)), site_data$cell_id)
  rows <- lapply(groups, function(index) {
    block <- site_data[index, , drop = FALSE]
    intensity <- block$site_intensity[is.finite(block$site_intensity)]
    data.frame(
      exact_site_id = block$cell_id[[1L]],
      n_independent_sites = nrow(block),
      n_images = sum(block$n_images),
      n_pigmented_sites = length(intensity),
      conditional_intensity_median = if (length(intensity)) {
        stats::median(intensity)
      } else NA_real_,
      median_DOY = stats::median(block$site_DOY, na.rm = TRUE),
      median_year = stats::median(block$site_year, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  phenotype <- do.call(rbind, rows)
  merged <- merge(cells, phenotype, by = "exact_site_id", all.x = TRUE, sort = FALSE)
  list(cells = merged, exact_sites = site_data)
}

ac_add_bombus_rank_support <- function(cells) {
  species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
  source_columns <- paste0("bee_", species)
  ac_require_columns(cells, source_columns, "analysis cells")
  suitability <- as.matrix(cells[, source_columns, drop = FALSE])
  storage.mode(suitability) <- "double"
  common <- apply(suitability, 1L, function(x) all(is.finite(x)))
  ranks <- apply(suitability, 2L, ac_positive_rank)
  if (is.null(dim(ranks))) ranks <- matrix(ranks, ncol = length(species))
  colnames(ranks) <- paste0(species, "_within_species_rank")
  out <- cbind(cells, as.data.frame(ranks))
  out$bombus_fingerprint_common_support <- common
  out
}

ac_environment_sources <- function(cache_root) {
  wanted <- c(
    elevation = "elevation_Japan_crop.tif",
    temperature = "bio10_Japan_crop_30s.tif",
    precipitation = "bio12_Japan_crop_30s.tif",
    radiation = "RSDS_Japan_crop_30s.tif"
  )
  vapply(wanted, function(filename) {
    hits <- list.files(
      cache_root, pattern = paste0("^", gsub("\\.", "\\\\.", filename), "$"),
      recursive = TRUE, full.names = TRUE
    )
    if (length(hits) != 1L) {
      stop("Expected one raster named ", filename, "; found ", length(hits), call. = FALSE)
    }
    hits[[1L]]
  }, character(1))
}

ac_point_context <- function(raster, sites, radii_km, summary_function = c("mean", "sum")) {
  summary_function <- match.arg(summary_function)
  raster_values <- as.matrix(raster, wide = TRUE)
  x_centres <- terra::xFromCol(raster, seq_len(terra::ncol(raster)))
  y_centres <- terra::yFromRow(raster, seq_len(terra::nrow(raster)))
  result <- matrix(
    NA_real_, nrow = nrow(sites), ncol = length(radii_km),
    dimnames = list(NULL, as.character(radii_km))
  )
  maximum_radius <- max(radii_km)
  for (index in seq_len(nrow(sites))) {
    longitude <- sites$longitude[index]
    latitude <- sites$latitude[index]
    longitude_km <- 111.32 * cos(latitude * pi / 180)
    rows <- which(abs(y_centres - latitude) * 110.57 <= maximum_radius)
    columns <- which(abs(x_centres - longitude) * longitude_km <= maximum_radius)
    if (!length(rows) || !length(columns)) next
    y_distance <- (y_centres[rows] - latitude) * 110.57
    x_distance <- (x_centres[columns] - longitude) * longitude_km
    distance <- sqrt(outer(y_distance^2, x_distance^2, "+"))
    values <- raster_values[rows, columns, drop = FALSE]
    for (radius_index in seq_along(radii_km)) {
      selected <- values[distance <= radii_km[radius_index] & is.finite(values)]
      if (!length(selected)) next
      result[index, radius_index] <- if (summary_function == "mean") {
        mean(selected)
      } else {
        sum(selected)
      }
    }
  }
  result
}

ac_oriented_pca <- function(data, columns, prefix, n_components = 2L) {
  matrix <- as.matrix(data[, columns, drop = FALSE])
  storage.mode(matrix) <- "double"
  complete <- stats::complete.cases(matrix)
  fit <- stats::prcomp(matrix[complete, , drop = FALSE], center = TRUE, scale. = TRUE)
  n_components <- min(n_components, ncol(fit$x))
  scores <- fit$x[, seq_len(n_components), drop = FALSE]
  rotation <- fit$rotation[, seq_len(n_components), drop = FALSE]
  for (component in seq_len(n_components)) {
    anchor <- which.max(abs(rotation[, component]))
    if (rotation[anchor, component] < 0) {
      scores[, component] <- -scores[, component]
      rotation[, component] <- -rotation[, component]
    }
  }
  score_data <- data.frame(matrix(NA_real_, nrow(data), n_components))
  names(score_data) <- paste0(prefix, "_pc", seq_len(n_components))
  score_data[complete, ] <- scores
  loadings <- do.call(rbind, lapply(seq_len(n_components), function(component) {
    data.frame(
      axis = prefix, component = paste0("PC", component),
      variable = rownames(rotation), loading = as.numeric(rotation[, component]),
      stringsAsFactors = FALSE
    )
  }))
  list(scores = score_data, loadings = loadings)
}

ac_add_local_environment_context <- function(cells, cache_root, radius_km = 50, aggregate_factor = 6L) {
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required", call. = FALSE)
  paths <- ac_environment_sources(cache_root)
  points <- terra::vect(cells, geom = c("longitude", "latitude"), crs = "EPSG:4326")
  context <- cells[, c("exact_site_id", "longitude", "latitude"), drop = FALSE]
  provenance <- list()
  for (variable in names(paths)) {
    raster <- terra::rast(paths[[variable]])
    fine <- terra::extract(raster, points, method = "bilinear")
    context[[paste0(variable, "_cell")]] <- as.numeric(fine[[ncol(fine)]])
    coarse <- terra::aggregate(raster, fact = aggregate_factor, fun = mean, na.rm = TRUE)
    broad <- ac_point_context(coarse, cells, radius_km, "mean")[, 1L]
    context[[paste0(variable, "_broad_", radius_km, "km")]] <- broad
    context[[paste0(variable, "_within_", radius_km, "km")]] <-
      context[[paste0(variable, "_cell")]] - broad
    provenance[[length(provenance) + 1L]] <- data.frame(
      variable = variable,
      raster_path = normalizePath(paths[[variable]], winslash = "/"),
      raster_md5 = unname(tools::md5sum(paths[[variable]])),
      stringsAsFactors = FALSE
    )
  }
  vars <- c("elevation", "temperature", "precipitation", "radiation")
  broad_cols <- paste0(vars, "_broad_", radius_km, "km")
  within_cols <- paste0(vars, "_within_", radius_km, "km")
  broad_pca <- ac_oriented_pca(context, broad_cols, paste0("broad", radius_km, "km"))
  within_pca <- ac_oriented_pca(context, within_cols, paste0("within", radius_km, "km"))
  context <- cbind(context, broad_pca$scores, within_pca$scores)
  merged <- merge(
    cells,
    context[, setdiff(names(context), c("longitude", "latitude")), drop = FALSE],
    by = "exact_site_id", all.x = TRUE, sort = FALSE
  )
  list(
    cells = merged,
    context = context,
    provenance = do.call(rbind, provenance),
    loadings = rbind(broad_pca$loadings, within_pca$loadings)
  )
}
