v22_analysis_spec_version <- "v22.0_did_local_human_context"

v22_did_source_url <- paste0(
  "https://nlftp.mlit.go.jp/ksj/gml/data/A16/A16-15/",
  "A16-15_GML.zip"
)

v22_ensure_did_shapefile <- function(
    cache_dir,
    source_url = v22_did_source_url) {
  archive <- file.path(cache_dir, "A16-15_GML.zip")
  shapefile <- file.path(
    cache_dir, "A16-15_GML", "A16-15_GML", "A16-15_00_DID.shp"
  )
  if (file.exists(shapefile)) return(shapefile)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(archive)) {
    partial <- paste0(archive, ".part")
    on.exit(unlink(partial, force = TRUE), add = TRUE)
    utils::download.file(
      source_url, partial, mode = "wb", quiet = FALSE
    )
    if (!file.rename(partial, archive)) {
      stop("Could not finalize the DID archive download.", call. = FALSE)
    }
  }
  utils::unzip(
    archive,
    exdir = file.path(cache_dir, "A16-15_GML")
  )
  if (!file.exists(shapefile)) {
    stop("DID shapefile is absent after extraction: ", shapefile,
         call. = FALSE)
  }
  shapefile
}

v22_build_did_distance_raster <- function(
    did_shapefile, template_raster) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required.", call. = FALSE)
  }
  template <- if (inherits(template_raster, "SpatRaster")) {
    template_raster
  } else {
    terra::rast(template_raster)
  }
  did <- terra::vect(did_shapefile)
  if (!nzchar(terra::crs(did))) {
    terra::crs(did) <- "EPSG:6668"
  }
  if (!terra::same.crs(did, template)) {
    did <- terra::project(did, terra::crs(template))
  }
  did_presence <- terra::rasterize(
    did, template[[1L]], field = 1, background = NA,
    touches = TRUE
  )
  names(did_presence) <- "did_presence"
  did_distance <- terra::distance(did_presence)
  names(did_distance) <- "did_distance_m"
  list(presence = did_presence, distance = did_distance)
}

v22_extract_did_context <- function(
    cells, did_distance, population_5km_rank) {
  v21_require_columns(
    cells, c("exact_site_id", "longitude", "latitude"), "cells"
  )
  if (length(population_5km_rank) != nrow(cells)) {
    stop("population_5km_rank must align one-to-one with cells.",
         call. = FALSE)
  }
  points <- terra::vect(
    cells, geom = c("longitude", "latitude"),
    crs = "EPSG:6668"
  )
  distance_m <- as.numeric(
    terra::extract(did_distance, points, ID = FALSE)[, 1L]
  )
  distance_km <- distance_m / 1000
  distance_rank <- v19_rank01(log1p(distance_km))
  proximity_rank <- v19_rank01(-log1p(distance_km))
  population_5km_rank <- as.numeric(population_5km_rank)
  out <- data.frame(
    exact_site_id = as.character(cells$exact_site_id),
    longitude = as.numeric(cells$longitude),
    latitude = as.numeric(cells$latitude),
    did_distance_km = distance_km,
    did_distance_rank = distance_rank,
    did_proximity_rank = proximity_rank,
    did_within_5km = as.numeric(distance_km <= 5),
    did_within_10km = as.numeric(distance_km <= 10),
    population_5km_rank = population_5km_rank,
    stringsAsFactors = FALSE
  )
  out$did_aligned_population_score <- rowMeans(out[, c(
    "population_5km_rank", "did_proximity_rank"
  )])
  out$populated_beyond_did_score <-
    out$population_5km_rank * out$did_distance_rank
  out$human_context_class <- v22_human_context_class(
    out$did_distance_km, out$population_5km_rank
  )
  out
}

v22_human_context_class <- function(
    did_distance_km, population_5km_rank) {
  distance <- as.numeric(did_distance_km)
  population <- as.numeric(population_5km_rank)
  out <- rep("intermediate_context", length(distance))
  out[distance <= 5 & population >= 0.75] <-
    "did_proximate_high_population"
  out[distance > 10 & population >= 0.75] <-
    "populated_beyond_did"
  out[distance > 10 & population <= 0.25] <-
    "remote_low_population"
  factor(
    out,
    levels = c(
      "did_proximate_high_population", "populated_beyond_did",
      "remote_low_population", "intermediate_context"
    )
  )
}

v22_feature_definitions <- function() {
  data.frame(
    feature = c(
      "did_proximity_rank", "did_within_5km", "did_within_10km",
      "did_aligned_population_score", "populated_beyond_did_score"
    ),
    role = c(
      "dense_settlement_proximity", "within_5km_did",
      "within_10km_did", "population_did_alignment",
      "population_outside_did"
    ),
    hypothesis_direction = rep("greater", 5L),
    stringsAsFactors = FALSE
  )
}

v22_context_composition <- function(
    observed_candidate, simulated_candidate, context_class) {
  observed_candidate <- as.logical(observed_candidate)
  simulated_candidate <- as.matrix(simulated_candidate)
  classes <- levels(factor(context_class))
  if (is.null(classes) || !length(classes)) {
    classes <- sort(unique(as.character(context_class)))
  }
  observed_total <- sum(observed_candidate)
  simulated_total <- colSums(simulated_candidate)
  observed_count <- vapply(classes, function(class) {
    sum(observed_candidate & as.character(context_class) == class)
  }, numeric(1))
  simulated_count <- sapply(classes, function(class) {
    colSums(
      simulated_candidate &
        matrix(
          as.character(context_class) == class,
          nrow(simulated_candidate), ncol(simulated_candidate)
        )
    )
  })
  if (is.vector(simulated_count)) {
    simulated_count <- matrix(
      simulated_count, ncol = length(classes),
      dimnames = list(NULL, classes)
    )
  }
  colnames(simulated_count) <- classes
  observed_fraction <- observed_count / observed_total
  simulated_fraction <- sweep(
    simulated_count, 1, pmax(simulated_total, 1), "/"
  )
  center <- colMeans(simulated_fraction, na.rm = TRUE)
  spread <- apply(simulated_fraction, 2, stats::sd, na.rm = TRUE)
  spread[!is.finite(spread) | spread <= 1e-12] <- 1
  null_z <- sweep(
    sweep(simulated_fraction, 2, center, "-"), 2, spread, "/"
  )
  observed_z <- (observed_fraction - center) / spread
  null_max <- apply(abs(null_z), 1, max, na.rm = TRUE)
  summary <- do.call(rbind, lapply(seq_along(classes), function(index) {
    null <- simulated_fraction[, index]
    upper <- (1 + sum(null >= observed_fraction[index])) /
      (length(null) + 1)
    lower <- (1 + sum(null <= observed_fraction[index])) /
      (length(null) + 1)
    data.frame(
      human_context_class = classes[index],
      observed_candidate_count = observed_count[index],
      observed_candidate_fraction = observed_fraction[index],
      null_mean_count = mean(simulated_count[, index]),
      null_mean_fraction = center[index],
      lower_95_fraction = unname(stats::quantile(null, 0.025)),
      upper_95_fraction = unname(stats::quantile(null, 0.975)),
      two_sided_p = min(1, 2 * min(upper, lower)),
      maxT_FWER_p =
        (1 + sum(null_max >= abs(observed_z[index]))) /
        (length(null_max) + 1),
      n_natural_maps = nrow(simulated_fraction),
      stringsAsFactors = FALSE
    )
  }))
  null <- data.frame(
    draw = seq_len(nrow(simulated_fraction)),
    total_candidates = simulated_total,
    simulated_count,
    setNames(
      as.data.frame(simulated_fraction),
      paste0(classes, "_fraction")
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  list(summary = summary, null = null)
}

v22_apply_convergence_fwer <- function(summary, null) {
  keys <- paste(summary$spike_feature, summary$metric, sep = "::")
  null_matrix <- sapply(seq_len(nrow(summary)), function(index) {
    block <- null[
      null$spike_feature == summary$spike_feature[index],
      , drop = FALSE
    ]
    block[[summary$metric[index]]]
  })
  colnames(null_matrix) <- keys
  center <- colMeans(null_matrix)
  spread <- apply(null_matrix, 2, stats::sd)
  spread[!is.finite(spread) | spread <= 1e-12] <- 1
  null_z <- sweep(sweep(null_matrix, 2, center, "-"), 2, spread, "/")
  observed_z <- (summary$observed_value - center) / spread
  null_max <- apply(null_z, 1, max)
  summary$BH_q <- stats::p.adjust(summary$empirical_p, method = "BH")
  summary$maxT_FWER_p <- vapply(seq_len(nrow(summary)), function(index) {
    (1 + sum(null_max >= observed_z[index])) /
      (length(null_max) + 1)
  }, numeric(1))
  summary
}
