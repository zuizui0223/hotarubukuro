# Reproducible acquisition and preparation of public raster predictors.
#
# The registry in config/raster_sources.csv is declarative. These helpers keep
# downloads in data/cache and create derived 30 arc-second rasters separately;
# source archives and the published SDM rasters are never overwritten.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

require_namespace <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package '", package, "' is required.", call. = FALSE)
  }
}

as_flag <- function(x, name = deparse(substitute(x))) {
  if (is.logical(x)) return(x)
  value <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(value))
  out[value %in% c("true", "t", "yes", "y", "1")] <- TRUE
  out[value %in% c("false", "f", "no", "n", "0")] <- FALSE
  if (anyNA(out)) {
    stop("Invalid boolean value in ", name, ": ",
         paste(unique(value[is.na(out)]), collapse = ", "), call. = FALSE)
  }
  out
}

read_raster_sources <- function(path = "config/raster_sources.csv") {
  if (!file.exists(path)) stop("Raster source registry not found: ", path, call. = FALSE)
  sources <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  sources$enabled <- as_flag(sources$enabled, "enabled")
  numeric_columns <- intersect(
    c("scale_factor", "native_resolution_arcsec", "wcs_width", "wcs_height"),
    names(sources)
  )
  sources[numeric_columns] <- lapply(sources[numeric_columns], as.numeric)
  validate_raster_sources(sources)
  sources
}

validate_raster_sources <- function(sources) {
  required <- c(
    "source_id", "provider", "dataset_version", "enabled", "access", "url",
    "archive_member", "cache_name", "output_name", "data_class",
    "value_semantics", "resample_method", "postprocess", "scale_factor",
    "unit", "native_resolution_arcsec", "native_crs", "expected_sha256",
    "license", "source_page", "coverage_id", "wcs_width", "wcs_height"
  )
  missing <- setdiff(required, names(sources))
  if (length(missing)) {
    stop("Raster registry is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(sources)) stop("Raster registry is empty.", call. = FALSE)
  if (anyNA(sources$source_id) || any(!nzchar(sources$source_id))) {
    stop("Every raster source needs a source_id.", call. = FALSE)
  }
  if (anyDuplicated(sources$source_id)) stop("source_id values must be unique.", call. = FALSE)
  if (anyDuplicated(sources$output_name[sources$enabled])) {
    stop("Enabled output_name values must be unique.", call. = FALSE)
  }

  allowed_access <- c("direct", "zip", "cog", "vrt", "wcs", "catalog")
  allowed_classes <- c("continuous", "categorical", "count")
  allowed_methods <- c("bilinear", "near", "average", "mode", "sum")
  allowed_postprocess <- c("none", "count_to_density_km2")
  checks <- list(
    access = sources$access %in% allowed_access,
    data_class = sources$data_class %in% allowed_classes,
    resample_method = sources$resample_method %in% allowed_methods,
    postprocess = sources$postprocess %in% allowed_postprocess
  )
  for (field in names(checks)) {
    if (any(!checks[[field]])) {
      stop("Invalid ", field, " for: ",
           paste(sources$source_id[!checks[[field]]], collapse = ", "), call. = FALSE)
    }
  }

  active <- sources$enabled
  if (any(active & (!grepl("^https://", sources$url) | is.na(sources$url)))) {
    stop("Enabled sources must use an explicit HTTPS URL.", call. = FALSE)
  }
  if (any(active & sources$access == "catalog")) {
    stop("Catalog-only rows must remain disabled until a downloadable asset is pinned.", call. = FALSE)
  }
  if (any(active & sources$access == "zip" &
          (is.na(sources$archive_member) | !nzchar(sources$archive_member)))) {
    stop("Enabled zip sources require archive_member.", call. = FALSE)
  }
  wcs <- active & sources$access == "wcs"
  if (any(wcs & (
      is.na(sources$coverage_id) | !nzchar(sources$coverage_id) |
      !is.finite(sources$wcs_width) | sources$wcs_width < 1 |
      !is.finite(sources$wcs_height) | sources$wcs_height < 1 |
      is.na(sources$native_crs) | !nzchar(sources$native_crs)
  ))) {
    stop("Enabled WCS sources require coverage_id, dimensions and native_crs.", call. = FALSE)
  }
  if (any(active & (is.na(sources$cache_name) | !nzchar(sources$cache_name)))) {
    stop("Enabled sources require cache_name.", call. = FALSE)
  }
  duplicated_cache_names <- unique(sources$cache_name[
    active & (duplicated(sources$cache_name) | duplicated(sources$cache_name, fromLast = TRUE))
  ])
  acquisition_identity_fields <- c(
    "access", "url", "archive_member", "coverage_id", "wcs_width",
    "wcs_height", "native_crs", "expected_sha256"
  )
  for (cache_name in duplicated_cache_names) {
    rows <- sources[active & sources$cache_name == cache_name, , drop = FALSE]
    identity <- vapply(acquisition_identity_fields, function(field) {
      value <- as.character(rows[[field]])
      value[is.na(value)] <- "<NA>"
      length(unique(value)) == 1L
    }, logical(1))
    if (any(!identity)) {
      stop(
        "Enabled sources sharing cache_name must have identical acquisition identity: ",
        cache_name, " differs in ",
        paste(acquisition_identity_fields[!identity], collapse = ", "),
        call. = FALSE
      )
    }
  }
  if (any(active & (is.na(sources$output_name) | !grepl("\\.tif$", sources$output_name)))) {
    stop("Enabled output_name values must end in .tif.", call. = FALSE)
  }
  if (any(active & (!is.finite(sources$scale_factor) | sources$scale_factor <= 0))) {
    stop("Enabled scale_factor values must be finite and positive.", call. = FALSE)
  }
  if (any(active & sources$data_class == "continuous" &
          !sources$resample_method %in% c("bilinear", "average"))) {
    stop("Continuous rasters require bilinear or average resampling.", call. = FALSE)
  }
  if (any(active & sources$data_class == "categorical" &
          !sources$resample_method %in% c("near", "mode"))) {
    stop("Categorical rasters require near or mode resampling.", call. = FALSE)
  }
  if (any(active & sources$data_class == "count" & sources$resample_method != "sum")) {
    stop("Extensive count rasters must use sum resampling.", call. = FALSE)
  }
  if (any(active & sources$postprocess == "count_to_density_km2" &
          sources$data_class != "count")) {
    stop("count_to_density_km2 is only valid for count rasters.", call. = FALSE)
  }
  invisible(sources)
}

read_pipeline_config <- function(path = "config/pipeline.yml") {
  require_namespace("yaml")
  if (!file.exists(path)) stop("Pipeline configuration not found: ", path, call. = FALSE)
  config <- yaml::read_yaml(path)
  required <- c("version", "paths", "grid", "download", "processing")
  missing <- setdiff(required, names(config))
  if (length(missing)) stop("Pipeline config is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(pipeline_bbox(config))
  if (!identical(as.character(config$grid$crs), "EPSG:4326")) {
    stop("The canonical public-raster grid must use EPSG:4326.", call. = FALSE)
  }
  if (!isTRUE(all.equal(as.numeric(config$grid$resolution_arcsec), 30))) {
    stop("The canonical grid resolution must be 30 arc-seconds.", call. = FALSE)
  }
  config
}

pipeline_bbox <- function(config) {
  extent <- config$grid$extent
  bbox <- c(
    xmin = as.numeric(extent$xmin), xmax = as.numeric(extent$xmax),
    ymin = as.numeric(extent$ymin), ymax = as.numeric(extent$ymax)
  )
  if (any(!is.finite(bbox)) || bbox[["xmin"]] >= bbox[["xmax"]] ||
      bbox[["ymin"]] >= bbox[["ymax"]] || bbox[["xmin"]] < -180 ||
      bbox[["xmax"]] > 180 || bbox[["ymin"]] < -90 || bbox[["ymax"]] > 90) {
    stop("Invalid EPSG:4326 grid extent in pipeline configuration.", call. = FALSE)
  }
  bbox
}

canonical_grid <- function(bbox, resolution_arcsec = 30, crs = "EPSG:4326",
                           origin = c(-180, -90)) {
  require_namespace("terra")
  bbox <- as.numeric(bbox)
  if (length(bbox) != 4L || any(!is.finite(bbox))) {
    stop("bbox must contain xmin, xmax, ymin, ymax.", call. = FALSE)
  }
  names(bbox) <- c("xmin", "xmax", "ymin", "ymax")
  resolution <- as.numeric(resolution_arcsec) / 3600
  if (!is.finite(resolution) || resolution <= 0) stop("Resolution must be positive.", call. = FALSE)
  snap_down <- function(value, anchor) anchor + floor((value - anchor) / resolution) * resolution
  snap_up <- function(value, anchor) anchor + ceiling((value - anchor) / resolution) * resolution
  snapped <- c(
    xmin = snap_down(bbox[["xmin"]], origin[[1]]),
    xmax = snap_up(bbox[["xmax"]], origin[[1]]),
    ymin = snap_down(bbox[["ymin"]], origin[[2]]),
    ymax = snap_up(bbox[["ymax"]], origin[[2]])
  )
  terra::rast(
    xmin = snapped[["xmin"]], xmax = snapped[["xmax"]],
    ymin = snapped[["ymin"]], ymax = snapped[["ymax"]],
    resolution = resolution, crs = crs
  )
}

sha256_file <- function(path) {
  require_namespace("digest")
  if (!file.exists(path)) stop("Cannot hash missing file: ", path, call. = FALSE)
  unname(digest::digest(file = path, algo = "sha256"))
}

verify_expected_hash <- function(path, expected_sha256 = NA_character_) {
  actual <- sha256_file(path)
  if (!is.na(expected_sha256) && nzchar(expected_sha256) &&
      !identical(tolower(actual), tolower(expected_sha256))) {
    stop("SHA-256 mismatch for ", path, ": expected ", expected_sha256,
         ", got ", actual, call. = FALSE)
  }
  actual
}

gdal_vsicurl <- function(url) {
  # Directory scans are unnecessary for explicit COG/VRT assets and can be
  # extremely expensive. GDAL-level retries cover transient range-request
  # failures while the final local cache is still promoted atomically.
  paste0("/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=", url)
}

download_with_retries <- function(url, destination, retries = 3L,
                                  timeout_seconds = 1800, quiet = FALSE) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(destination, ".part")
  curl <- Sys.which("curl")
  if (!nzchar(curl)) {
    stop(
      "A curl executable is required for resumable public-data downloads.",
      call. = FALSE
    )
  }
  status <- NA_integer_
  for (attempt in seq_len(as.integer(retries))) {
    bytes_before <- if (file.exists(temporary)) unname(file.info(temporary)$size) else 0
    if (!quiet) {
      message(
        "Download attempt ", attempt, "/", as.integer(retries),
        " (resuming at ", bytes_before, " bytes): ", url
      )
    }
    arguments <- c(
      "--fail", "--location", "--continue-at", "-",
      "--connect-timeout", "60",
      "--max-time", as.character(as.integer(timeout_seconds)),
      if (quiet) c("--silent", "--show-error") else "--progress-bar",
      "--output", shQuote(temporary),
      shQuote(url)
    )
    status <- suppressWarnings(system2(curl, arguments))
    if (identical(status, 0L) && file.exists(temporary) &&
        file.info(temporary)$size > 0) break
    bytes_after <- if (file.exists(temporary)) unname(file.info(temporary)$size) else 0
    if (!quiet) {
      message(
        "Download attempt ", attempt, " ended with curl status ", status,
        " at ", bytes_after, " bytes; the partial file is retained."
      )
    }
    if (attempt < retries) Sys.sleep(min(2 ^ (attempt - 1L), 8))
  }
  if (!identical(status, 0L) || !file.exists(temporary) ||
      file.info(temporary)$size <= 0) {
    bytes <- if (file.exists(temporary)) unname(file.info(temporary)$size) else 0
    stop(
      "Download failed for ", url, " with curl status ", status,
      " after retaining ", bytes, " resumable bytes at ", temporary, ".",
      call. = FALSE
    )
  }
  if (!file.rename(temporary, destination)) {
    if (!file.copy(temporary, destination, overwrite = TRUE)) {
      stop("Could not move download into cache: ", destination, call. = FALSE)
    }
    unlink(temporary)
  }
  destination
}

extract_zip_member <- function(archive, member, destination) {
  listing <- utils::unzip(archive, list = TRUE)$Name
  matches <- listing[listing == member]
  if (length(matches) != 1L) {
    stop("Archive member not found exactly once: ", member, call. = FALSE)
  }
  if (grepl("(^|/)\\.\\.(/|$)", member) || grepl("^/", member)) {
    stop("Unsafe archive member path: ", member, call. = FALSE)
  }
  extraction_dir <- tempfile("raster-unzip-")
  dir.create(extraction_dir)
  on.exit(unlink(extraction_dir, recursive = TRUE), add = TRUE)
  utils::unzip(archive, files = member, exdir = extraction_dir, junkpaths = FALSE)
  extracted <- file.path(extraction_dir, member)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(extracted, destination, overwrite = TRUE)) {
    stop("Could not copy extracted raster into cache: ", destination, call. = FALSE)
  }
  destination
}

cache_remote_subset <- function(url, destination, bbox) {
  require_namespace("terra")
  bbox <- stats::setNames(as.numeric(bbox), c("xmin", "xmax", "ymin", "ymax"))
  source <- terra::rast(gdal_vsicurl(url))
  if (terra::nlyr(source) != 1L) stop("Expected one raster layer at ", url, call. = FALSE)
  roi <- terra::as.polygons(
    terra::ext(bbox[["xmin"]], bbox[["xmax"]], bbox[["ymin"]], bbox[["ymax"]]),
    crs = "EPSG:4326"
  )
  if (!terra::same.crs(roi, source)) roi <- terra::project(roi, terra::crs(source))
  subset <- terra::crop(source, terra::ext(roi), snap = "out")
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(destination, ".part.tif")
  on.exit(unlink(temporary), add = TRUE)
  terra::writeRaster(
    subset, temporary, overwrite = TRUE,
    gdal = c("TILED=YES", "COMPRESS=DEFLATE", "PREDICTOR=2")
  )
  if (!file.rename(temporary, destination)) {
    if (!file.copy(temporary, destination, overwrite = TRUE)) {
      stop("Could not move remote subset into cache: ", destination, call. = FALSE)
    }
  }
  destination
}

soilgrids_wcs_request <- function(source, bbox) {
  if (nrow(source) != 1L || source$access[[1L]] != "wcs") {
    stop("soilgrids_wcs_request expects one WCS registry row.", call. = FALSE)
  }
  require_namespace("terra")
  bbox <- stats::setNames(as.numeric(bbox), c("xmin", "xmax", "ymin", "ymax"))
  roi <- terra::as.polygons(
    terra::ext(bbox[["xmin"]], bbox[["xmax"]], bbox[["ymin"]], bbox[["ymax"]]),
    crs = "EPSG:4326"
  )
  roi <- terra::project(roi, source$native_crs[[1L]])
  projected <- as.vector(terra::ext(roi))
  paste0(
    source$url[[1L]],
    "&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage",
    "&COVERAGEID=", utils::URLencode(source$coverage_id[[1L]], reserved = TRUE),
    "&FORMAT=GEOTIFF_INT16",
    "&SUBSET=x(", format(projected[[1L]], scientific = FALSE, digits = 17L),
    ",", format(projected[[2L]], scientific = FALSE, digits = 17L), ")",
    "&SUBSET=y(", format(projected[[3L]], scientific = FALSE, digits = 17L),
    ",", format(projected[[4L]], scientific = FALSE, digits = 17L), ")",
    "&SCALESIZE=x(", as.integer(source$wcs_width[[1L]]),
    "),y(", as.integer(source$wcs_height[[1L]]), ")",
    "&INTERPOLATION=NEAREST"
  )
}

cache_wcs_subset <- function(source, destination, bbox, retries = 3L,
                             timeout_seconds = 1800, quiet = FALSE) {
  require_namespace("terra")
  request <- soilgrids_wcs_request(source, bbox)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(destination, ".part.tif")
  on.exit(unlink(temporary), add = TRUE)
  old_environment <- Sys.getenv(
    c("GDAL_HTTP_TIMEOUT", "GDAL_HTTP_MAX_RETRY"),
    unset = NA_character_
  )
  on.exit({
    for (name in names(old_environment)) {
      if (is.na(old_environment[[name]])) {
        Sys.unsetenv(name)
      } else {
        do.call(Sys.setenv, setNames(list(old_environment[[name]]), name))
      }
    }
  }, add = TRUE)
  Sys.setenv(
    GDAL_HTTP_TIMEOUT = as.character(as.integer(timeout_seconds)),
    GDAL_HTTP_MAX_RETRY = as.character(as.integer(retries))
  )
  last_error <- NULL
  for (attempt in seq_len(as.integer(retries))) {
    unlink(temporary)
    last_error <- tryCatch({
      raster <- terra::rast(request)
      if (terra::nlyr(raster) != 1L) {
        stop("Expected one WCS raster layer for ", source$source_id[[1L]])
      }
      if (!nzchar(terra::crs(raster))) {
        terra::crs(raster) <- source$native_crs[[1L]]
      }
      if (!terra::same.crs(raster, source$native_crs[[1L]])) {
        stop("WCS response CRS differs from the declared native CRS.")
      }
      terra::writeRaster(
        raster,
        temporary,
        overwrite = TRUE,
        gdal = c("TILED=YES", "COMPRESS=DEFLATE", "PREDICTOR=2")
      )
      NULL
    }, error = identity)
    if (is.null(last_error) && file.exists(temporary) &&
        file.info(temporary)$size > 0) break
    if (!quiet) message("WCS attempt ", attempt, " failed for ", source$source_id[[1L]])
    if (attempt < retries) Sys.sleep(min(2 ^ (attempt - 1L), 8))
  }
  if (!is.null(last_error) || !file.exists(temporary) ||
      file.info(temporary)$size <= 0) {
    detail <- if (inherits(last_error, "condition")) {
      conditionMessage(last_error)
    } else {
      "empty or missing response"
    }
    stop("WCS download failed for ", source$source_id[[1L]], ": ", detail, call. = FALSE)
  }
  move_file_atomic(temporary, destination, "WCS cache raster")
  destination
}

source_cache_path <- function(source, cache_dir) {
  file.path(cache_dir, as.character(source$cache_name[[1]]))
}

materialize_source <- function(source, cache_dir, bbox, force = FALSE,
                               retries = 3L, timeout_seconds = 1800,
                               quiet = FALSE, allow_download = TRUE) {
  if (nrow(source) != 1L) stop("materialize_source expects one registry row.", call. = FALSE)
  destination <- source_cache_path(source, cache_dir)
  if (file.exists(destination) && file.info(destination)$size > 0 && !force) {
    verify_expected_hash(destination, source$expected_sha256[[1]])
    return(normalizePath(destination, winslash = "/", mustWork = TRUE))
  }
  if (!isTRUE(allow_download)) {
    stop(
      "Offline mode cannot use missing, empty, or force-invalidated cache: ",
      destination,
      call. = FALSE
    )
  }
  if (!isTRUE(source$enabled[[1]])) stop("Source is disabled: ", source$source_id[[1]], call. = FALSE)
  access <- source$access[[1]]
  if (access == "direct") {
    download_with_retries(source$url[[1]], destination, retries, timeout_seconds, quiet)
  } else if (access == "zip") {
    archive <- file.path(cache_dir, "archives", paste0(source$source_id[[1]], ".zip"))
    if (!file.exists(archive) || force) {
      download_with_retries(source$url[[1]], archive, retries, timeout_seconds, quiet)
    }
    extract_zip_member(archive, source$archive_member[[1]], destination)
  } else if (access %in% c("cog", "vrt")) {
    cache_remote_subset(source$url[[1]], destination, bbox)
  } else if (access == "wcs") {
    cache_wcs_subset(
      source, destination, bbox,
      retries = retries,
      timeout_seconds = timeout_seconds,
      quiet = quiet
    )
  } else {
    stop("Source has no pinned downloadable asset: ", source$source_id[[1]], call. = FALSE)
  }
  verify_expected_hash(destination, source$expected_sha256[[1]])
  normalizePath(destination, winslash = "/", mustWork = TRUE)
}

select_sources <- function(sources, only = NULL, include_disabled = FALSE) {
  selected <- if (include_disabled) sources else sources[sources$enabled, , drop = FALSE]
  if (!is.null(only) && length(only)) {
    unknown <- setdiff(only, sources$source_id)
    if (length(unknown)) stop("Unknown source_id: ", paste(unknown, collapse = ", "), call. = FALSE)
    selected <- selected[selected$source_id %in% only, , drop = FALSE]
  }
  selected
}

align_public_raster <- function(source, template, method, scale_factor = 1,
                                postprocess = "none", output_name = NULL,
                                validity_mask = NULL) {
  require_namespace("terra")
  if (is.character(source)) source <- terra::rast(source)
  if (!inherits(source, "SpatRaster") || terra::nlyr(source) != 1L) {
    stop("source must be a one-layer SpatRaster or raster path.", call. = FALSE)
  }
  if (!inherits(template, "SpatRaster") || terra::nlyr(template) != 1L) {
    stop("template must be a one-layer SpatRaster.", call. = FALSE)
  }
  if (!is.null(validity_mask)) {
    if (is.character(validity_mask)) validity_mask <- terra::rast(validity_mask)
    if (!inherits(validity_mask, "SpatRaster") || terra::nlyr(validity_mask) != 1L) {
      stop("validity_mask must be a one-layer SpatRaster or raster path.", call. = FALSE)
    }
    if (!isTRUE(terra::compareGeom(
      source, validity_mask, stopOnError = FALSE,
      crs = TRUE, ext = TRUE, rowcol = TRUE, res = TRUE
    ))) {
      stop("validity_mask geometry differs from the source raster.", call. = FALSE)
    }
    # SoilGrids WCS encodes ocean/non-soil cells as numeric zero without a
    # NoData flag. Bulk density cannot be zero in valid soil and therefore
    # provides one common land/soil mask without deleting valid zeros from
    # sand or coarse-fragment layers.
    source <- terra::ifel(validity_mask > 0, source, NA)
  }
  allowed <- c("bilinear", "near", "average", "mode", "sum")
  if (!method %in% allowed) stop("Unsupported raster method: ", method, call. = FALSE)
  aligned <- terra::project(source, template, method = method)
  if (!isTRUE(all.equal(as.numeric(scale_factor), 1))) aligned <- aligned * as.numeric(scale_factor)
  if (postprocess == "count_to_density_km2") {
    area_km2 <- terra::cellSize(template, unit = "km")
    aligned <- aligned / area_km2
  } else if (postprocess != "none") {
    stop("Unknown postprocess: ", postprocess, call. = FALSE)
  }
  if (!is.null(output_name)) names(aligned) <- output_name
  aligned
}

raster_metadata <- function(path) {
  require_namespace("terra")
  raster <- terra::rast(path)
  description <- terra::crs(raster, describe = TRUE)
  crs_code <- if (
    nrow(description) && !is.na(description$authority[[1]]) &&
      !is.na(description$code[[1]])
  ) {
    paste0(description$authority[[1]], ":", description$code[[1]])
  } else {
    terra::crs(raster, proj = TRUE)
  }
  extent <- as.vector(terra::ext(raster))
  range_stats <- terra::global(raster, c("min", "max"), na.rm = TRUE)
  valid_cells <- terra::global(!is.na(raster), "sum", na.rm = TRUE)[[1]]
  zero_cells <- terra::global(raster == 0, "sum", na.rm = TRUE)[[1]]
  data.frame(
    crs = crs_code,
    res_x = terra::res(raster)[[1]], res_y = terra::res(raster)[[2]],
    xmin = extent[[1]], xmax = extent[[2]], ymin = extent[[3]], ymax = extent[[4]],
    ncol = terra::ncol(raster), nrow = terra::nrow(raster),
    datatype = terra::datatype(raster), nodata = as.character(terra::NAflag(raster)),
    valid_cells = as.numeric(valid_cells),
    missing_cells = terra::ncell(raster) - as.numeric(valid_cells),
    zero_cells = as.numeric(zero_cells),
    zero_fraction_valid = as.numeric(zero_cells) / as.numeric(valid_cells),
    min = as.numeric(range_stats$min[[1]]),
    max = as.numeric(range_stats$max[[1]]),
    stringsAsFactors = FALSE
  )
}

output_semantics <- function(source) {
  if (source$postprocess[[1]] == "count_to_density_km2") {
    "population_density_people_per_km2"
  } else {
    source$value_semantics[[1]]
  }
}

# A processed raster is reusable only when both the cached source and every
# setting that determines its values are unchanged.  Keep this deliberately
# separate from the raster checksum: the latter describes an output, whereas
# this fingerprint answers whether that output is still the requested one.
format_fingerprint_value <- function(value) {
  if (!length(value) || is.na(value[[1]])) return("<NA>")
  if (is.numeric(value)) {
    return(format(value[[1]], digits = 17L, scientific = FALSE, trim = TRUE))
  }
  as.character(value[[1]])
}

processing_fingerprint <- function(source, cache_path, template,
                                   validity_mask_path = NULL,
                                   processing_code_sha256 = NA_character_,
                                   preparation_script_sha256 = NA_character_) {
  require_namespace("terra")
  if (nrow(source) != 1L) stop("processing_fingerprint expects one registry row.", call. = FALSE)
  if (!file.exists(cache_path)) stop("Cannot fingerprint missing cache raster: ", cache_path, call. = FALSE)

  extent <- as.vector(terra::ext(template))
  grid_settings <- c(
    template_crs = terra::crs(template, proj = TRUE),
    template_ncol = terra::ncol(template),
    template_nrow = terra::nrow(template),
    template_xmin = extent[[1]], template_xmax = extent[[2]],
    template_ymin = extent[[3]], template_ymax = extent[[4]],
    template_res_x = terra::res(template)[[1]],
    template_res_y = terra::res(template)[[2]]
  )
  source_fields <- c(
    "source_id", "provider", "dataset_version", "url", "archive_member",
    "data_class", "value_semantics", "resample_method", "postprocess",
    "scale_factor", "unit", "native_resolution_arcsec", "native_crs",
    "coverage_id", "wcs_width", "wcs_height"
  )
  source_settings <- vapply(source_fields, function(field) {
    paste0(field, "=", format_fingerprint_value(source[[field]]))
  }, character(1))
  grid_settings <- vapply(names(grid_settings), function(field) {
    paste0(field, "=", format_fingerprint_value(grid_settings[[field]]))
  }, character(1))
  validity_settings <- if (is.null(validity_mask_path)) {
    c("validity_mask_rule=<none>", "validity_mask_sha256=<none>")
  } else {
    if (!file.exists(validity_mask_path)) {
      stop("Cannot fingerprint missing validity mask: ", validity_mask_path, call. = FALSE)
    }
    c(
      "validity_mask_rule=soilgrids_bdod_positive_before_reprojection",
      paste0("validity_mask_sha256=", sha256_file(validity_mask_path))
    )
  }
  spatial_libraries <- terra::gdal(lib = "all")
  implementation_settings <- c(
    paste0("processing_code_sha256=", format_fingerprint_value(processing_code_sha256)),
    paste0("preparation_script_sha256=", format_fingerprint_value(preparation_script_sha256)),
    paste0("terra_version=", as.character(utils::packageVersion("terra"))),
    paste0("gdal_version=", unname(spatial_libraries[["gdal"]])),
    paste0("proj_version=", unname(spatial_libraries[["proj"]])),
    paste0("geos_version=", unname(spatial_libraries[["geos"]]))
  )
  digest::digest(
    paste(c("processing_algorithm_version=3", paste0("cache_sha256=", sha256_file(cache_path)),
            validity_settings, implementation_settings, source_settings,
            grid_settings), collapse = "\n"),
    algo = "sha256", serialize = FALSE
  )
}

processing_fingerprint_path <- function(output_path) {
  paste0(output_path, ".fingerprint")
}

processed_checksum_path <- function(output_path) {
  paste0(output_path, ".sha256")
}

read_processing_fingerprint <- function(output_path) {
  sidecar <- processing_fingerprint_path(output_path)
  if (!file.exists(sidecar)) return(NA_character_)
  value <- trimws(readLines(sidecar, warn = FALSE, encoding = "UTF-8"))
  value <- value[nzchar(value)]
  if (length(value) != 1L || !grepl("^[0-9a-f]{64}$", value, ignore.case = TRUE)) return(NA_character_)
  tolower(value)
}

processed_raster_status <- function(output_path, expected_fingerprint) {
  if (!file.exists(output_path) || file.info(output_path)$size <= 0) {
    return(list(stale = TRUE, reason = "processed raster is missing or empty"))
  }
  observed <- read_processing_fingerprint(output_path)
  if (is.na(observed)) {
    return(list(stale = TRUE, reason = "processed raster fingerprint is missing or invalid"))
  }
  if (!identical(observed, tolower(expected_fingerprint))) {
    return(list(stale = TRUE, reason = "processed raster settings fingerprint differs"))
  }
  checksum_path <- processed_checksum_path(output_path)
  if (!file.exists(checksum_path)) {
    return(list(stale = TRUE, reason = "processed raster checksum sidecar is missing"))
  }
  expected_checksum <- trimws(readLines(
    checksum_path, warn = FALSE, encoding = "UTF-8"
  ))
  if (length(expected_checksum) != 1L ||
      !grepl("^[0-9a-f]{64}$", expected_checksum, ignore.case = TRUE) ||
      !identical(tolower(expected_checksum), sha256_file(output_path))) {
    return(list(stale = TRUE, reason = "processed raster checksum differs"))
  }
  list(stale = FALSE, reason = "settings fingerprint matches")
}

move_file_atomic <- function(temporary, destination, label = "file") {
  # POSIX rename replaces the destination atomically.  The copy fallback is
  # only for filesystems where rename across a mount is unavailable.
  if (file.rename(temporary, destination)) return(invisible(destination))
  if (!file.copy(temporary, destination, overwrite = TRUE)) {
    stop("Could not move ", label, " into place: ", destination, call. = FALSE)
  }
  unlink(temporary)
  invisible(destination)
}

write_text_atomic <- function(text, destination) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(destination), "-"),
                        tmpdir = dirname(destination), fileext = ".part")
  on.exit(unlink(temporary), add = TRUE)
  writeLines(text, temporary, useBytes = TRUE)
  move_file_atomic(temporary, destination, "fingerprint")
}

write_raster_atomic <- function(raster, destination, ...) {
  require_namespace("terra")
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", tools::file_path_sans_ext(basename(destination)), "-"),
                        tmpdir = dirname(destination), fileext = ".part.tif")
  on.exit(unlink(temporary), add = TRUE)
  terra::writeRaster(raster, temporary, overwrite = TRUE, ...)
  move_file_atomic(temporary, destination, "processed raster")
}

prepare_source <- function(source, cache_dir, processed_dir, template, bbox,
                           force_download = FALSE, force_process = FALSE,
                           retries = 3L, timeout_seconds = 1800,
                           quiet = FALSE, validity_mask_path = NULL,
                           processing_code_sha256 = NA_character_,
                           preparation_script_sha256 = NA_character_,
                           allow_download = TRUE) {
  cache_path <- materialize_source(
    source, cache_dir, bbox, force_download, retries, timeout_seconds, quiet,
    allow_download
  )
  output_path <- file.path(processed_dir, source$output_name[[1]])
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  expected_fingerprint <- processing_fingerprint(
    source, cache_path, template, validity_mask_path,
    processing_code_sha256, preparation_script_sha256
  )
  existing <- processed_raster_status(output_path, expected_fingerprint)
  if (isTRUE(force_process) || existing$stale) {
    if (!isTRUE(force_process) && file.exists(output_path)) {
      message("Rebuilding stale processed raster ", source$output_name[[1]], ": ", existing$reason)
    }
    aligned <- align_public_raster(
      cache_path, template,
      method = source$resample_method[[1]],
      scale_factor = source$scale_factor[[1]],
      postprocess = source$postprocess[[1]],
      output_name = tools::file_path_sans_ext(source$output_name[[1]]),
      validity_mask = validity_mask_path
    )
    write_raster_atomic(
      aligned, output_path,
      gdal = c("TILED=YES", "COMPRESS=DEFLATE", "PREDICTOR=2")
    )
    # Write this only after the raster is in place.  An interrupted run then
    # leaves an intentionally stale raster, which is rebuilt safely next run.
    write_text_atomic(expected_fingerprint, processing_fingerprint_path(output_path))
    write_text_atomic(sha256_file(output_path), processed_checksum_path(output_path))
  }
  cache_meta <- raster_metadata(cache_path)
  output_meta <- raster_metadata(output_path)
  spatial_libraries <- terra::gdal(lib = "all")
  data.frame(
    source_id = source$source_id[[1]], provider = source$provider[[1]],
    dataset_version = source$dataset_version[[1]], source_url = source$url[[1]],
    source_page = source$source_page[[1]], license = source$license[[1]],
    coverage_id = source$coverage_id[[1]],
    wcs_width = source$wcs_width[[1]], wcs_height = source$wcs_height[[1]],
    wcs_request_url = if (source$access[[1]] == "wcs") {
      soilgrids_wcs_request(source, bbox)
    } else {
      NA_character_
    },
    retrieved_at_utc = format(file.info(cache_path)$mtime, tz = "UTC", usetz = TRUE),
    cache_path = cache_path, cache_sha256 = sha256_file(cache_path),
    cache_bytes = unname(file.info(cache_path)$size),
    provider_native_resolution = if (source$access[[1L]] == "wcs") {
      "250_m"
    } else if (is.finite(source$native_resolution_arcsec[[1L]])) {
      paste0(source$native_resolution_arcsec[[1L]], "_arcsec")
    } else {
      NA_character_
    },
    native_crs = cache_meta$crs, native_res_x = cache_meta$res_x,
    native_res_y = cache_meta$res_y,
    native_extent = paste(cache_meta[c("xmin", "xmax", "ymin", "ymax")], collapse = ";"),
    validity_mask_rule = if (is.null(validity_mask_path)) {
      NA_character_
    } else {
      "soilgrids_bdod_positive_before_reprojection"
    },
    validity_mask_path = if (is.null(validity_mask_path)) NA_character_ else validity_mask_path,
    validity_mask_sha256 = if (is.null(validity_mask_path)) {
      NA_character_
    } else {
      sha256_file(validity_mask_path)
    },
    target_crs = output_meta$crs, target_res_x = output_meta$res_x,
    target_res_y = output_meta$res_y,
    target_extent = paste(output_meta[c("xmin", "xmax", "ymin", "ymax")], collapse = ";"),
    resample_method = source$resample_method[[1]],
    processing_fingerprint = expected_fingerprint,
    source_value_semantics = source$value_semantics[[1]],
    output_value_semantics = output_semantics(source), scale_factor = source$scale_factor[[1]],
    unit = source$unit[[1]], processed_path = normalizePath(output_path, winslash = "/"),
    processed_sha256 = sha256_file(output_path),
    processed_bytes = unname(file.info(output_path)$size),
    processed_min = output_meta$min, processed_max = output_meta$max,
    valid_cells = output_meta$valid_cells,
    missing_cells = output_meta$missing_cells,
    zero_cells = output_meta$zero_cells,
    zero_fraction_valid = output_meta$zero_fraction_valid,
    processing_algorithm_version = 3L,
    terra_version = as.character(utils::packageVersion("terra")),
    gdal_version = unname(spatial_libraries[["gdal"]]),
    proj_version = unname(spatial_libraries[["proj"]]),
    geos_version = unname(spatial_libraries[["geos"]]),
    stringsAsFactors = FALSE
  )
}

write_csv_atomic <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".part")
  on.exit(unlink(temporary), add = TRUE)
  utils::write.csv(data, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    if (!file.copy(temporary, path, overwrite = TRUE)) stop("Could not write manifest: ", path, call. = FALSE)
  }
  invisible(path)
}

bind_manifest_row <- function(current, row) {
  if (is.null(current) || !nrow(current)) return(row)
  columns <- union(names(current), names(row))
  for (name in setdiff(columns, names(current))) {
    current[[name]] <- rep(NA, nrow(current))
  }
  for (name in setdiff(columns, names(row))) {
    row[[name]] <- rep(NA, nrow(row))
  }
  rbind(current[columns], row[columns])
}

parse_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  result <- list()
  positional <- character()
  i <- 1L
  while (i <= length(args)) {
    token <- args[[i]]
    if (!startsWith(token, "--")) {
      positional <- c(positional, token)
      i <- i + 1L
      next
    }
    token <- substring(token, 3L)
    if (grepl("=", token, fixed = TRUE)) {
      pieces <- strsplit(token, "=", fixed = TRUE)[[1]]
      key <- pieces[[1]]
      value <- paste(pieces[-1L], collapse = "=")
    } else {
      key <- token
      if (i < length(args) && !startsWith(args[[i + 1L]], "--")) {
        value <- args[[i + 1L]]
        i <- i + 1L
      } else {
        value <- TRUE
      }
    }
    key <- gsub("-", "_", key, fixed = TRUE)
    result[[key]] <- value
    i <- i + 1L
  }
  result$positional <- positional
  result
}

split_source_ids <- function(value) {
  if (is.null(value) || identical(value, FALSE) || !nzchar(value)) return(NULL)
  unique(trimws(strsplit(as.character(value), ",", fixed = TRUE)[[1]]))
}

is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\])", path)
}

resolve_repo_path <- function(root, path) {
  if (is_absolute_path(path)) path else file.path(root, path)
}

repo_relative_path <- function(path, root) {
  absolute <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- paste0(sub("/$", "", normalizePath(root, winslash = "/", mustWork = TRUE)), "/")
  if (startsWith(absolute, root)) substring(absolute, nchar(root) + 1L) else absolute
}

find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "config", "pipeline.yml")) &&
        file.exists(file.path(current, "R", "raster_sources.R"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  stop("Could not locate repository root from ", start, call. = FALSE)
}
