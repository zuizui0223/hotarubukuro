# Validation and record-safe extraction for the prepared public predictor stack.

is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\])", path)
}

resolve_public_path <- function(path, project_root = getwd()) {
  path <- path.expand(as.character(path)[1L])
  if (!is_absolute_path(path)) path <- file.path(project_root, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

read_public_raster_manifest <- function(path, project_root = getwd()) {
  path <- resolve_public_path(path, project_root)
  if (!file.exists(path)) {
    stop("Public raster manifest not found: ", path, call. = FALSE)
  }
  manifest <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  required <- c(
    "source_id", "provider", "dataset_version", "source_url", "source_page",
    "license", "cache_path", "cache_sha256", "cache_bytes", "native_crs",
    "native_res_x", "native_res_y", "native_extent", "provider_native_resolution",
    "processed_path", "processed_sha256", "target_crs", "target_res_x", "target_res_y",
    "target_extent", "output_value_semantics", "unit",
    "processed_min", "processed_max", "valid_cells", "missing_cells",
    "source_value_semantics", "resample_method", "scale_factor",
    "processing_fingerprint",
    "processing_algorithm_version", "pipeline_version",
    "pipeline_config_sha256", "raster_registry_sha256",
    "raster_processing_code_sha256", "raster_preparation_script_sha256",
    "terra_version", "gdal_version",
    "proj_version", "geos_version"
  )
  missing <- setdiff(required, names(manifest))
  if (length(missing)) {
    stop(
      "Public raster manifest is missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(manifest) || anyNA(manifest$source_id) ||
      any(!nzchar(manifest$source_id)) || anyDuplicated(manifest$source_id)) {
    stop("Public raster source_id values must be non-empty and unique.", call. = FALSE)
  }
  hash_fields <- c(
    "cache_sha256", "processed_sha256", "pipeline_config_sha256",
    "raster_registry_sha256", "raster_processing_code_sha256",
    "raster_preparation_script_sha256"
  )
  invalid_hash <- vapply(hash_fields, function(field) {
    any(is.na(manifest[[field]]) | !grepl("^[0-9a-fA-F]{64}$", manifest[[field]]))
  }, logical(1))
  if (any(invalid_hash)) {
    stop(
      "Public raster manifest contains invalid SHA-256 fields: ",
      paste(hash_fields[invalid_hash], collapse = ", "),
      call. = FALSE
    )
  }
  version_fields <- c("processing_algorithm_version", "pipeline_version")
  invalid_version <- vapply(version_fields, function(field) {
    value <- suppressWarnings(as.numeric(manifest[[field]]))
    any(!is.finite(value) | value < 1 | value != floor(value)) ||
      length(unique(value)) != 1L
  }, logical(1))
  provenance_text_fields <- c(
    "terra_version", "gdal_version", "proj_version", "geos_version"
  )
  invalid_text <- vapply(provenance_text_fields, function(field) {
    value <- as.character(manifest[[field]])
    any(is.na(value) | !nzchar(value)) || length(unique(value)) != 1L
  }, logical(1))
  if (any(invalid_version) || any(invalid_text)) {
    stop("Public raster manifest has incomplete or inconsistent software provenance.", call. = FALSE)
  }
  required_text <- c(
    "provider", "dataset_version", "source_url", "source_page", "license",
    "cache_path", "native_crs", "native_extent", "provider_native_resolution",
    "processed_path", "target_crs", "target_extent", "source_value_semantics",
    "output_value_semantics", "resample_method", "unit"
  )
  invalid_required_text <- vapply(required_text, function(field) {
    value <- as.character(manifest[[field]])
    any(is.na(value) | !nzchar(value))
  }, logical(1))
  if (any(invalid_required_text) ||
      any(!grepl("^https://", manifest$source_url)) ||
      any(!grepl("^https://", manifest$source_page)) ||
      any(!grepl("^[0-9a-fA-F]{64}$", manifest$processing_fingerprint)) ||
      any(!is.finite(as.numeric(manifest$cache_bytes)) |
          as.numeric(manifest$cache_bytes) <= 0) ||
      any(!is.finite(as.numeric(manifest$native_res_x)) |
          as.numeric(manifest$native_res_x) <= 0) ||
      any(!is.finite(as.numeric(manifest$native_res_y)) |
          as.numeric(manifest$native_res_y) <= 0) ||
      any(!is.finite(as.numeric(manifest$scale_factor)) |
          as.numeric(manifest$scale_factor) <= 0)) {
    stop("Public raster manifest has incomplete source or processing metadata.", call. = FALSE)
  }
  manifest$resolved_path <- vapply(
    manifest$processed_path,
    resolve_public_path,
    character(1),
    project_root = project_root
  )
  manifest
}

validate_public_raster_collection <- function(
    manifest_path,
    project_root = getwd(),
    expected_source_ids = NULL,
    expected_extent = NULL,
    registry = NULL,
    verify_hashes = TRUE,
    resolution_tolerance = 1e-12) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required for public raster validation.", call. = FALSE)
  }
  if (verify_hashes && !requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required for public raster hash validation.", call. = FALSE)
  }
  manifest <- read_public_raster_manifest(manifest_path, project_root)
  if (anyDuplicated(normalizePath(
    manifest$resolved_path,
    winslash = "/",
    mustWork = FALSE
  ))) {
    stop("Prepared public raster paths must be unique.", call. = FALSE)
  }
  if (!is.null(expected_source_ids)) {
    expected_source_ids <- unique(as.character(expected_source_ids))
    missing <- setdiff(expected_source_ids, manifest$source_id)
    extra <- setdiff(manifest$source_id, expected_source_ids)
    if (length(missing) || length(extra)) {
      stop(
        "Public raster manifest/source registry mismatch. Missing: ",
        paste(missing, collapse = ", "),
        "; extra: ", paste(extra, collapse = ", "),
        call. = FALSE
      )
    }
    manifest <- manifest[match(expected_source_ids, manifest$source_id), , drop = FALSE]
  }
  missing_files <- manifest$resolved_path[!file.exists(manifest$resolved_path)]
  if (length(missing_files)) {
    stop("Prepared public rasters are missing: ", paste(missing_files, collapse = ", "), call. = FALSE)
  }
  if (verify_hashes) {
    observed_hash <- vapply(
      manifest$resolved_path,
      function(path) unname(digest::digest(file = path, algo = "sha256")),
      character(1)
    )
    bad <- tolower(observed_hash) != tolower(manifest$processed_sha256)
    if (any(bad)) {
      stop(
        "Prepared public raster hash mismatch: ",
        paste(manifest$source_id[bad], collapse = ", "),
        call. = FALSE
      )
    }
  }

  rasters <- lapply(manifest$resolved_path, terra::rast)
  layer_counts <- vapply(rasters, terra::nlyr, numeric(1))
  if (any(layer_counts != 1L)) {
    stop(
      "Prepared public predictors must each contain one layer: ",
      paste(manifest$source_id[layer_counts != 1L], collapse = ", "),
      call. = FALSE
    )
  }
  reference <- rasters[[1L]]
  geometry_ok <- vapply(
    rasters,
    function(raster) isTRUE(terra::compareGeom(
      reference, raster,
      stopOnError = FALSE,
      crs = TRUE,
      ext = TRUE,
      rowcol = TRUE,
      res = TRUE
    )),
    logical(1)
  )
  if (any(!geometry_ok)) {
    stop(
      "Prepared public rasters are not on one aligned grid: ",
      paste(manifest$source_id[!geometry_ok], collapse = ", "),
      call. = FALSE
    )
  }
  resolution <- terra::res(reference)
  if (any(abs(resolution - (30 / 3600)) > resolution_tolerance)) {
    stop("Prepared public raster grid is not 30 arc-seconds.", call. = FALSE)
  }
  if (!isTRUE(terra::same.crs(reference, "EPSG:4326"))) {
    stop("Prepared public raster grid is not EPSG:4326/WGS84.", call. = FALSE)
  }
  actual_extent <- as.vector(terra::ext(reference))
  if (!is.null(expected_extent)) {
    expected_extent <- as.numeric(expected_extent)
    if (length(expected_extent) != 4L ||
        any(abs(actual_extent - expected_extent) > resolution_tolerance)) {
      stop("Prepared public raster extent differs from the configured grid.", call. = FALSE)
    }
  }
  parse_extent <- function(value) {
    as.numeric(strsplit(as.character(value), ";", fixed = TRUE)[[1L]])
  }
  native_extent_ok <- vapply(manifest$native_extent, function(value) {
    extent <- parse_extent(value)
    length(extent) == 4L && all(is.finite(extent)) &&
      extent[[1L]] < extent[[2L]] && extent[[3L]] < extent[[4L]]
  }, logical(1))
  if (any(!native_extent_ok)) {
    stop("Public raster manifest contains an invalid native extent.", call. = FALSE)
  }
  manifest_extent_ok <- vapply(seq_len(nrow(manifest)), function(index) {
    declared <- parse_extent(manifest$target_extent[[index]])
    length(declared) == 4L &&
      all(is.finite(declared)) &&
      all(abs(declared - as.vector(terra::ext(rasters[[index]]))) <= resolution_tolerance)
  }, logical(1))
  manifest_resolution_ok <- vapply(seq_len(nrow(manifest)), function(index) {
    declared <- as.numeric(manifest[index, c("target_res_x", "target_res_y")])
    length(declared) == 2L && all(is.finite(declared)) &&
      all(abs(declared - terra::res(rasters[[index]])) <= resolution_tolerance)
  }, logical(1))
  manifest_crs_ok <- vapply(seq_len(nrow(manifest)), function(index) {
    !is.na(manifest$target_crs[[index]]) &&
      isTRUE(terra::same.crs(rasters[[index]], manifest$target_crs[[index]]))
  }, logical(1))
  if (any(!manifest_extent_ok | !manifest_resolution_ok | !manifest_crs_ok)) {
    bad <- !manifest_extent_ok | !manifest_resolution_ok | !manifest_crs_ok
    stop(
      "Public raster manifest metadata differs from actual files: ",
      paste(manifest$source_id[bad], collapse = ", "),
      call. = FALSE
    )
  }
  valid_cells <- vapply(rasters, function(raster) {
    as.numeric(terra::global(!is.na(raster), "sum", na.rm = TRUE)[[1L]])
  }, numeric(1))
  if (any(!is.finite(valid_cells) | valid_cells <= 0)) {
    stop("Prepared public predictors cannot be entirely missing.", call. = FALSE)
  }
  observed_ranges <- t(vapply(rasters, function(raster) {
    as.numeric(terra::minmax(raster))
  }, numeric(2)))
  declared_ranges <- as.matrix(data.frame(
    min = as.numeric(manifest$processed_min),
    max = as.numeric(manifest$processed_max)
  ))
  range_tolerance <- pmax(1e-10, abs(observed_ranges) * 1e-12)
  count_metadata_ok <-
    is.finite(as.numeric(manifest$valid_cells)) &
    is.finite(as.numeric(manifest$missing_cells)) &
    as.numeric(manifest$valid_cells) == valid_cells &
    as.numeric(manifest$missing_cells) == terra::ncell(reference) - valid_cells
  range_metadata_ok <-
    is.finite(declared_ranges[, 1L]) & is.finite(declared_ranges[, 2L]) &
    apply(abs(observed_ranges - declared_ranges) <= range_tolerance, 1L, all)
  if (any(!count_metadata_ok | !range_metadata_ok)) {
    bad <- !count_metadata_ok | !range_metadata_ok
    stop(
      "Public raster manifest value metadata differs from actual files: ",
      paste(manifest$source_id[bad], collapse = ", "),
      call. = FALSE
    )
  }
  # Deliberately broad physical-domain guards catch scale/unit mistakes without
  # treating ecological extremes as errors.
  physical_bounds <- data.frame(
    source_id = c(
      "chelsa_bio10", "chelsa_bio12", "chelsa_cmimean", "chelsa_vpdmean",
      "soilgrids_bdod_0_5cm", "soilgrids_cfvo_0_5cm",
      "soilgrids_sand_0_5cm", "soilgrids_silt_0_5cm",
      "soilgrids_nitrogen_0_5cm", "soilgrids_ocd_0_5cm",
      "soilgrids_soc_0_5cm", "soilgrids_phh2o_0_5cm",
      "worldclim_elevation_30s"
    ),
    lower = c(-80, 0, -1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, -500),
    upper = c(70, 20000, 1000, 20000, 3, 100, 100, 100, 100, 500, 1000, 14, 9000),
    stringsAsFactors = FALSE
  )
  bound_index <- match(manifest$source_id, physical_bounds$source_id)
  checked <- !is.na(bound_index)
  bounds_ok <- rep(TRUE, nrow(manifest))
  bounds_ok[checked] <-
    observed_ranges[checked, 1L] >= physical_bounds$lower[bound_index[checked]] &
    observed_ranges[checked, 2L] <= physical_bounds$upper[bound_index[checked]]
  if (any(!bounds_ok)) {
    stop(
      "Prepared public predictor values violate broad physical bounds: ",
      paste(manifest$source_id[!bounds_ok], collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(registry)) {
    validate_raster_sources(registry)
    active <- registry[registry$enabled, , drop = FALSE]
    active <- active[match(manifest$source_id, active$source_id), , drop = FALSE]
    metadata_ok <-
      manifest$provider == active$provider &
      manifest$dataset_version == active$dataset_version &
      manifest$source_url == active$url &
      manifest$source_page == active$source_page &
      manifest$license == active$license &
      manifest$resample_method == active$resample_method &
      as.numeric(manifest$scale_factor) == as.numeric(active$scale_factor) &
      manifest$source_value_semantics == active$value_semantics &
      manifest$unit == active$unit &
      manifest$output_value_semantics == vapply(
        seq_len(nrow(active)),
        function(index) output_semantics(active[index, , drop = FALSE]),
        character(1)
      )
    native_crs_ok <- vapply(seq_len(nrow(active)), function(index) {
      isTRUE(terra::same.crs(
        manifest$native_crs[[index]],
        active$native_crs[[index]]
      ))
    }, logical(1))
    non_wcs <- active$access != "wcs"
    native_resolution_ok <- rep(TRUE, nrow(active))
    native_resolution_ok[non_wcs] <-
      abs(as.numeric(manifest$native_res_x[non_wcs]) -
            as.numeric(active$native_resolution_arcsec[non_wcs]) / 3600) <= 1e-7 &
      abs(as.numeric(manifest$native_res_y[non_wcs]) -
            as.numeric(active$native_resolution_arcsec[non_wcs]) / 3600) <= 1e-7
    metadata_ok <- metadata_ok & native_crs_ok & native_resolution_ok
    if (anyNA(metadata_ok) || any(!metadata_ok)) {
      stop(
        "Public raster manifest metadata differs from the source registry: ",
        paste(manifest$source_id[is.na(metadata_ok) | !metadata_ok], collapse = ", "),
        call. = FALSE
      )
    }
    pinned <- !is.na(active$expected_sha256) &
      grepl("^[0-9a-fA-F]{64}$", active$expected_sha256)
    if (any(!pinned)) {
      stop(
        "Enabled public raster sources are not content-pinned: ",
        paste(active$source_id[!pinned], collapse = ", "),
        call. = FALSE
      )
    }
    cache_hash_ok <-
      tolower(manifest$cache_sha256) == tolower(active$expected_sha256)
    if (anyNA(cache_hash_ok) || any(!cache_hash_ok)) {
      stop(
        "Public raster cache hashes differ from the source registry: ",
        paste(manifest$source_id[is.na(cache_hash_ok) | !cache_hash_ok], collapse = ", "),
        call. = FALSE
      )
    }
  }
  stack <- terra::rast(rasters)
  names(stack) <- manifest$source_id

  soil_ids <- grep("^soilgrids_", names(stack), value = TRUE)
  if (length(soil_ids)) {
    required_mask_fields <- c(
      "validity_mask_rule", "validity_mask_sha256", "missing_cells"
    )
    if (length(setdiff(required_mask_fields, names(manifest)))) {
      stop("SoilGrids manifest rows lack native-mask provenance.", call. = FALSE)
    }
    soil_manifest <- manifest[match(soil_ids, manifest$source_id), , drop = FALSE]
    if (any(
      soil_manifest$validity_mask_rule !=
        "soilgrids_bdod_positive_before_reprojection" |
      !grepl("^[0-9a-f]{64}$", soil_manifest$validity_mask_sha256)
    )) {
      stop("SoilGrids rows do not use the reviewed common validity mask.", call. = FALSE)
    }
    unique_mask_hashes <- unique(tolower(soil_manifest$validity_mask_sha256))
    bdod_cache_hash <- tolower(
      soil_manifest$cache_sha256[
        soil_manifest$source_id == "soilgrids_bdod_0_5cm"
      ]
    )
    if (length(unique_mask_hashes) != 1L || length(bdod_cache_hash) != 1L ||
        !identical(unique_mask_hashes, bdod_cache_hash)) {
      stop("SoilGrids native-mask hashes are not tied to the bulk-density cache.", call. = FALSE)
    }
    reference_mask <- is.na(stack[[soil_ids[[1L]]]])
    mask_difference <- vapply(soil_ids[-1L], function(id) {
      as.numeric(terra::global(
        reference_mask != is.na(stack[[id]]),
        "sum",
        na.rm = TRUE
      )[[1L]])
    }, numeric(1))
    if (length(mask_difference) && any(mask_difference != 0)) {
      stop("Prepared SoilGrids layers do not share one missing-data mask.", call. = FALSE)
    }
    if ("soilgrids_bdod_0_5cm" %in% soil_ids &&
        terra::minmax(stack[["soilgrids_bdod_0_5cm"]])[[1L]] <= 0) {
      stop("Prepared SoilGrids bulk density contains invalid zero values.", call. = FALSE)
    }
    if ("soilgrids_phh2o_0_5cm" %in% soil_ids) {
      ph_range <- terra::minmax(stack[["soilgrids_phh2o_0_5cm"]])
      if (ph_range[[1L]] <= 0 || ph_range[[2L]] > 14) {
        stop("Prepared SoilGrids pH is outside (0, 14].", call. = FALSE)
      }
    }
  }

  worldpop_ids <- c("worldpop_2020_count", "worldpop_2020_density")
  if (all(worldpop_ids %in% names(stack))) {
    count <- stack[[worldpop_ids[[1L]]]]
    density <- stack[[worldpop_ids[[2L]]]]
    mask_difference <- terra::global(
      is.na(count) != is.na(density),
      "sum",
      na.rm = TRUE
    )[[1L]]
    if (mask_difference != 0 ||
        terra::minmax(count)[[1L]] < 0 ||
        terra::minmax(density)[[1L]] < 0) {
      stop("Prepared WorldPop count/density masks or ranges are inconsistent.", call. = FALSE)
    }
    reconstructed <- density * terra::cellSize(count, unit = "km")
    denominator <- terra::ifel(abs(count) > 1, abs(count), 1)
    relative_difference <- abs(reconstructed - count) / denominator
    max_relative_difference <- terra::global(
      relative_difference,
      "max",
      na.rm = TRUE
    )[[1L]]
    # FLT4 GeoTIFF output cannot retain exact decimal equality for large urban
    # cells. The relative tolerance is well below the precision relevant to the
    # source population estimates while still detecting semantic/resampling
    # mistakes.
    if (!is.finite(max_relative_difference) || max_relative_difference > 1e-6) {
      stop("Prepared WorldPop density does not reconstruct population count.", call. = FALSE)
    }
  }
  list(manifest = manifest, stack = stack, geometry_ok = geometry_ok)
}

extract_public_predictors_by_record_id <- function(
    records,
    manifest_path,
    project_root = getwd(),
    expected_source_ids = NULL,
    expected_extent = NULL,
    registry = NULL,
    id_col = "analysis_id",
    longitude_col = "longitude",
    latitude_col = "latitude",
    verify_hashes = TRUE) {
  validation <- validate_public_raster_collection(
    manifest_path = manifest_path,
    project_root = project_root,
    expected_source_ids = expected_source_ids,
    expected_extent = expected_extent,
    registry = registry,
    verify_hashes = verify_hashes
  )
  extracted <- extract_raster_values(
    records,
    validation$stack,
    id_col = id_col,
    longitude_col = longitude_col,
    latitude_col = latitude_col
  )
  list(values = extracted, validation = validation)
}
