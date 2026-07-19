source(file.path(test_project_root, "R", "raster_sources.R"))
source(file.path(test_project_root, "R", "public_predictors.R"))

testthat::test_that("public predictor manifest validates geometry, hashes, coverage and IDs", {
  directory <- tempfile("public-predictors-")
  dir.create(directory)
  template <- terra::rast(
    nrows = 2, ncols = 2,
    xmin = 137, xmax = 137 + 2 / 120,
    ymin = 35, ymax = 35 + 2 / 120,
    crs = "EPSG:4326"
  )
  first <- template
  second <- template
  terra::values(first) <- c(1, 2, 3, 4)
  terra::values(second) <- c(10, 20, 30, 40)
  paths <- file.path(directory, c("first.tif", "second.tif"))
  terra::writeRaster(first, paths[[1]], overwrite = TRUE)
  terra::writeRaster(second, paths[[2]], overwrite = TRUE)

  manifest <- data.frame(
    source_id = c("predictor_first", "predictor_second"),
    provider = "synthetic",
    dataset_version = "test",
    source_url = "https://example.test/source.tif",
    source_page = "https://example.test/dataset",
    license = "test-license",
    cache_path = paths,
    processed_path = paths,
    cache_sha256 = vapply(paths, sha256_file, character(1)),
    cache_bytes = unname(file.info(paths)$size),
    native_crs = "EPSG:4326",
    native_res_x = 1 / 120,
    native_res_y = 1 / 120,
    native_extent = "137;137.016666666667;35;35.016666666667",
    provider_native_resolution = "30_arcsec",
    processed_sha256 = vapply(paths, sha256_file, character(1)),
    target_crs = "EPSG:4326",
    target_res_x = 1 / 120,
    target_res_y = 1 / 120,
    target_extent = "137;137.016666666667;35;35.016666666667",
    output_value_semantics = c("first", "second"),
    source_value_semantics = c("first", "second"),
    resample_method = "bilinear",
    scale_factor = 1,
    unit = "test_unit",
    processed_min = c(1, 10),
    processed_max = c(4, 40),
    valid_cells = 4,
    missing_cells = 0,
    processing_fingerprint = paste(rep("f", 64), collapse = ""),
    processing_algorithm_version = 3L,
    pipeline_version = 3L,
    pipeline_config_sha256 = paste(rep("a", 64), collapse = ""),
    raster_registry_sha256 = paste(rep("b", 64), collapse = ""),
    raster_processing_code_sha256 = paste(rep("c", 64), collapse = ""),
    raster_preparation_script_sha256 = paste(rep("d", 64), collapse = ""),
    terra_version = as.character(utils::packageVersion("terra")),
    gdal_version = terra::gdal(lib = "all")[["gdal"]],
    proj_version = terra::gdal(lib = "all")[["proj"]],
    geos_version = terra::gdal(lib = "all")[["geos"]],
    stringsAsFactors = FALSE
  )
  manifest_path <- file.path(directory, "manifest.csv")
  utils::write.csv(manifest, manifest_path, row.names = FALSE)

  records <- data.frame(
    analysis_id = c("bottom-left", "top-right"),
    longitude = c(137 + 1 / 240, 137 + 3 / 240),
    latitude = c(35 + 1 / 240, 35 + 3 / 240)
  )
  result <- extract_public_predictors_by_record_id(
    records,
    manifest_path,
    expected_source_ids = manifest$source_id
  )
  testthat::expect_identical(result$values$analysis_id, records$analysis_id)
  testthat::expect_identical(
    names(result$values),
    c("analysis_id", "predictor_first", "predictor_second")
  )
  testthat::expect_equal(result$values$predictor_first, c(3, 2))
  testthat::expect_equal(result$values$predictor_second, c(30, 20))

  testthat::expect_error(
    validate_public_raster_collection(
      manifest_path,
      expected_source_ids = c(manifest$source_id, "missing")
    ),
    "registry mismatch"
  )
  corrupted <- manifest
  corrupted$processed_sha256[[1]] <- paste(rep("0", 64), collapse = "")
  utils::write.csv(corrupted, manifest_path, row.names = FALSE)
  testthat::expect_error(
    validate_public_raster_collection(manifest_path),
    "hash mismatch"
  )
})
