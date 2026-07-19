# Validation and mathematically explicit summaries for the five published SDMs.
# The rasters are immutable legacy outputs: this module validates and extracts
# them, but it does not fit or rebuild any species-distribution model.

expected_bombus_species <- function() {
  c("ardens", "beaticola", "consobrinus", "diversus", "honshuensis")
}

sdm_sha256 <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required for SDM hash validation.", call. = FALSE)
  }
  unname(digest::digest(file = path, algo = "sha256"))
}

read_sdm_manifest <- function(path = "sdm/manifest.csv") {
  if (!file.exists(path)) stop("SDM manifest not found: ", path, call. = FALSE)
  manifest <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  validate_sdm_manifest(manifest)
  manifest
}

validate_sdm_manifest <- function(manifest) {
  required <- c(
    "species", "file", "sha256", "crs", "res_x", "res_y", "xmin", "xmax",
    "ymin", "ymax", "ncol", "nrow", "nodata", "datatype", "valid_cells",
    "total_cells", "min", "max", "value_semantics", "model_output",
    "provenance_status"
  )
  missing <- setdiff(required, names(manifest))
  if (length(missing)) stop("SDM manifest is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  expected <- expected_bombus_species()
  if (!setequal(manifest$species, expected) || nrow(manifest) != length(expected)) {
    stop("SDM manifest must contain exactly the five expected Bombus species.", call. = FALSE)
  }
  if (anyDuplicated(manifest$species) || anyDuplicated(manifest$file) || anyDuplicated(manifest$sha256)) {
    stop("SDM manifest species, file and sha256 values must be unique.", call. = FALSE)
  }
  if (any(!grepl("^[0-9a-f]{64}$", manifest$sha256))) {
    stop("Invalid SHA-256 value in SDM manifest.", call. = FALSE)
  }
  if (any(manifest$value_semantics != "continuous_habitat_suitability")) {
    stop("Published SDMs must be labelled continuous_habitat_suitability.", call. = FALSE)
  }
  if (any(manifest$model_output != "maxnet_cloglog")) {
    stop("Published SDMs must be labelled maxnet_cloglog.", call. = FALSE)
  }
  allowed_provenance <- c("reproducible", "legacy_unverifiable")
  if (anyNA(manifest$provenance_status) ||
      any(!manifest$provenance_status %in% allowed_provenance)) {
    stop(
      "SDM provenance_status must be one of: ",
      paste(allowed_provenance, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(manifest)
}

sdm_models_reproducible <- function(manifest) {
  validate_sdm_manifest(manifest)
  all(manifest$provenance_status == "reproducible")
}

load_sdm_stack <- function(sdm_dir = "sdm", manifest = NULL) {
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.", call. = FALSE)
  if (is.null(manifest)) manifest <- read_sdm_manifest(file.path(sdm_dir, "manifest.csv"))
  validate_sdm_manifest(manifest)
  manifest <- manifest[match(expected_bombus_species(), manifest$species), , drop = FALSE]
  paths <- file.path(sdm_dir, manifest$file)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) stop("Missing SDM raster(s): ", paste(missing, collapse = ", "), call. = FALSE)
  stack <- terra::rast(paths)
  names(stack) <- manifest$species
  stack
}

inspect_sdm_raster <- function(path, species = tools::file_path_sans_ext(basename(path))) {
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.", call. = FALSE)
  raster <- terra::rast(path)
  if (terra::nlyr(raster) != 1L) stop("SDM must have one layer: ", path, call. = FALSE)
  description <- terra::crs(raster, describe = TRUE)
  crs_code <- if (nrow(description)) paste0(description$authority[[1]], ":", description$code[[1]]) else NA_character_
  extent <- as.vector(terra::ext(raster))
  values <- terra::values(raster, mat = FALSE)
  finite <- is.finite(values)
  data.frame(
    species = species, file = basename(path), sha256 = sdm_sha256(path),
    crs = crs_code, res_x = terra::res(raster)[[1]], res_y = terra::res(raster)[[2]],
    xmin = extent[[1]], xmax = extent[[2]], ymin = extent[[3]], ymax = extent[[4]],
    ncol = terra::ncol(raster), nrow = terra::nrow(raster),
    nodata = as.character(terra::NAflag(raster)), datatype = terra::datatype(raster),
    valid_cells = sum(finite), total_cells = length(values),
    min = if (any(finite)) min(values[finite]) else NA_real_,
    max = if (any(finite)) max(values[finite]) else NA_real_,
    stringsAsFactors = FALSE
  )
}

validate_sdm_collection <- function(sdm_dir = "sdm", manifest_path = file.path(sdm_dir, "manifest.csv"),
                                    tolerance = 1e-10) {
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.", call. = FALSE)
  manifest <- read_sdm_manifest(manifest_path)
  manifest <- manifest[match(expected_bombus_species(), manifest$species), , drop = FALSE]
  expected_files <- manifest$file
  actual_files <- basename(list.files(sdm_dir, pattern = "\\.tif$", full.names = TRUE))
  issues <- character()
  missing <- setdiff(expected_files, actual_files)
  extra <- setdiff(actual_files, expected_files)
  if (length(missing)) issues <- c(issues, paste0("missing files: ", paste(missing, collapse = ", ")))
  if (length(extra)) issues <- c(issues, paste0("unmanifested files: ", paste(extra, collapse = ", ")))
  if (length(missing)) {
    return(list(
      ok = FALSE,
      structural_ok = FALSE,
      model_reproducible = FALSE,
      provenance_status = unique(manifest$provenance_status),
      issues = issues,
      report = data.frame(),
      geometry_aligned = FALSE,
      nodata_masks_identical = FALSE,
      hashes_unique = FALSE,
      correlations = NULL
    ))
  }

  paths <- file.path(sdm_dir, expected_files)
  observed <- do.call(rbind, Map(inspect_sdm_raster, paths, manifest$species))
  rownames(observed) <- NULL
  checks <- data.frame(species = manifest$species, stringsAsFactors = FALSE)
  checks$hash_ok <- tolower(observed$sha256) == tolower(manifest$sha256)
  checks$crs_ok <- observed$crs == manifest$crs
  exact_fields <- c("ncol", "nrow", "valid_cells", "total_cells", "datatype", "nodata")
  checks$metadata_ok <- TRUE
  for (field in exact_fields) {
    checks$metadata_ok <- checks$metadata_ok & as.character(observed[[field]]) == as.character(manifest[[field]])
  }
  numeric_fields <- c("res_x", "res_y", "xmin", "xmax", "ymin", "ymax", "min", "max")
  for (field in numeric_fields) {
    checks$metadata_ok <- checks$metadata_ok &
      abs(as.numeric(observed[[field]]) - as.numeric(manifest[[field]])) <= tolerance
  }

  rasters <- lapply(paths, terra::rast)
  geometry_aligned <- all(vapply(rasters[-1L], function(raster) {
    isTRUE(terra::compareGeom(rasters[[1]], raster, stopOnError = FALSE,
                              crs = TRUE, ext = TRUE, rowcol = TRUE, res = TRUE))
  }, logical(1)))
  stack <- terra::rast(paths)
  values <- terra::values(stack, mat = TRUE)
  masks <- lapply(seq_len(ncol(values)), function(index) is.na(values[, index]))
  nodata_masks_identical <- all(vapply(masks[-1L], identical, logical(1), y = masks[[1]]))
  hashes_unique <- length(unique(observed$sha256)) == nrow(observed)
  finite_rows <- stats::complete.cases(values)
  correlations <- if (sum(finite_rows) > 1L) stats::cor(values[finite_rows, , drop = FALSE]) else NULL
  if (!all(checks$hash_ok)) issues <- c(issues, paste0("hash mismatch: ", paste(checks$species[!checks$hash_ok], collapse = ", ")))
  if (!all(checks$crs_ok)) issues <- c(issues, paste0("CRS mismatch: ", paste(checks$species[!checks$crs_ok], collapse = ", ")))
  if (!all(checks$metadata_ok)) issues <- c(issues, paste0("metadata mismatch: ", paste(checks$species[!checks$metadata_ok], collapse = ", ")))
  if (!geometry_aligned) issues <- c(issues, "SDM grids are not aligned")
  if (!nodata_masks_identical) issues <- c(issues, "SDM nodata masks differ")
  if (!hashes_unique) issues <- c(issues, "duplicate SDM file hashes")
  if (any(observed$min < 0 | observed$max > 1)) issues <- c(issues, "SDM values outside [0, 1]")

  structural_ok <- !length(issues)
  list(
    ok = structural_ok,
    structural_ok = structural_ok,
    model_reproducible = structural_ok && sdm_models_reproducible(manifest),
    provenance_status = unique(manifest$provenance_status),
    issues = issues,
    report = cbind(observed, checks[-1L]),
    geometry_aligned = geometry_aligned,
    nodata_masks_identical = nodata_masks_identical,
    hashes_unique = hashes_unique,
    correlations = correlations
  )
}

print_sdm_validation <- function(validation) {
  print(validation$report, row.names = FALSE)
  cat("structural_ok:", validation$structural_ok, "\n")
  cat("model_reproducible:", validation$model_reproducible, "\n")
  cat("provenance_status:", paste(validation$provenance_status, collapse = ", "), "\n")
  cat("geometry_aligned:", validation$geometry_aligned, "\n")
  cat("nodata_masks_identical:", validation$nodata_masks_identical, "\n")
  cat("hashes_unique:", validation$hashes_unique, "\n")
  if (length(validation$issues)) {
    cat("issues:\n", paste0("- ", validation$issues, collapse = "\n"), "\n", sep = "")
  } else {
    cat("SDM structural validation passed. Published rasters were not modified.\n")
    if (!isTRUE(validation$model_reproducible)) {
      cat(
        "SDM model-generation reproducibility is unavailable: the published ",
        "rasters are legacy_unverifiable.\n",
        sep = ""
      )
    }
  }
  invisible(validation)
}

validate_suitability_matrix <- function(x, expected_species = expected_bombus_species()) {
  x <- as.data.frame(x, check.names = FALSE)
  missing <- setdiff(expected_species, names(x))
  extra <- setdiff(names(x), expected_species)
  if (length(missing) || length(extra)) {
    stop("Suitability data must contain exactly: ", paste(expected_species, collapse = ", "), call. = FALSE)
  }
  x <- x[, expected_species, drop = FALSE]
  if (!all(vapply(x, is.numeric, logical(1)))) stop("Suitability columns must be numeric.", call. = FALSE)
  raw_values <- unlist(x, use.names = FALSE)
  if (any(!is.na(raw_values) & !is.finite(raw_values))) {
    stop("Suitability values must be finite or NA.", call. = FALSE)
  }
  finite <- raw_values[is.finite(raw_values)]
  if (length(finite) && any(finite < 0 | finite > 1)) stop("Suitability values must lie in [0, 1].", call. = FALSE)
  x
}

summed_suitability <- function(x, expected_species = expected_bombus_species()) {
  x <- validate_suitability_matrix(x, expected_species)
  complete <- stats::complete.cases(x)
  result <- rep(NA_real_, nrow(x))
  result[complete] <- rowSums(x[complete, , drop = FALSE])
  result
}

species_count <- function(x, thresholds, expected_species = expected_bombus_species()) {
  x <- validate_suitability_matrix(x, expected_species)
  if (is.null(names(thresholds)) || !setequal(names(thresholds), expected_species)) {
    stop("thresholds must be named for all five species.", call. = FALSE)
  }
  thresholds <- as.numeric(thresholds[expected_species])
  if (any(!is.finite(thresholds) | thresholds < 0 | thresholds > 1)) {
    stop("Species thresholds must be finite values in [0, 1].", call. = FALSE)
  }
  complete <- stats::complete.cases(x)
  result <- rep(NA_integer_, nrow(x))
  if (any(complete)) {
    binary <- sweep(as.matrix(x[complete, , drop = FALSE]), 2L, thresholds, FUN = ">=")
    result[complete] <- as.integer(rowSums(binary))
  }
  result
}

extract_sdm_by_record_id <- function(records, sdm_dir = "sdm", manifest = NULL,
                                     id_col = "record_id", longitude_col = "longitude",
                                     latitude_col = "latitude") {
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.", call. = FALSE)
  required <- c(id_col, longitude_col, latitude_col)
  missing <- setdiff(required, names(records))
  if (length(missing)) stop("Missing record columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (anyNA(records[[id_col]]) || anyDuplicated(records[[id_col]])) {
    stop("record_id values must be non-missing and unique.", call. = FALSE)
  }
  coordinates <- records[, c(longitude_col, latitude_col), drop = FALSE]
  if (any(!stats::complete.cases(coordinates))) stop("Coordinates must be complete before raster extraction.", call. = FALSE)
  stack <- load_sdm_stack(sdm_dir, manifest)
  points <- terra::vect(coordinates, geom = c(longitude_col, latitude_col), crs = "EPSG:4326")
  extracted <- as.data.frame(terra::extract(stack, points, ID = FALSE))
  if (nrow(extracted) != nrow(records)) stop("Raster extraction changed row count.", call. = FALSE)
  output <- data.frame(extracted, check.names = FALSE)
  output[[id_col]] <- records[[id_col]]
  output <- output[c(id_col, setdiff(names(output), id_col))]
  rownames(output) <- NULL
  output
}
