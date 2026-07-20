locate_test_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")
  hit <- candidates[file.exists(file.path(candidates, "R", "raster_sources.R"))]
  if (!length(hit)) stop("Repository root not found from test working directory.")
  normalizePath(hit[[1]], winslash = "/")
}

raster_test_root <- locate_test_root()
source(file.path(raster_test_root, "R", "raster_sources.R"))

testthat::test_that("expanded public registry is internally consistent", {
  registry <- read_raster_sources(file.path(raster_test_root, "config", "raster_sources.csv"))
  enabled <- registry[registry$enabled, , drop = FALSE]

  testthat::expect_true(all(grepl("^https://", enabled$url)))
  testthat::expect_equal(nrow(enabled), 21L)
  testthat::expect_false(anyDuplicated(enabled$source_id))
  testthat::expect_false(anyDuplicated(enabled$output_name))

  pinned <- !is.na(enabled$expected_sha256) & nzchar(enabled$expected_sha256)
  testthat::expect_true(all(grepl("^[0-9a-f]{64}$", enabled$expected_sha256[pinned])))
  testthat::expect_true(any(!pinned))

  chelsa <- registry[registry$provider == "CHELSA", , drop = FALSE]
  testthat::expect_equal(nrow(chelsa), 10L)
  testthat::expect_true(all(chelsa$enabled))
  testthat::expect_true(all(chelsa$dataset_version == "2.1"))
  testthat::expect_true(all(chelsa$resample_method == "bilinear"))
  testthat::expect_equal(
    chelsa$unit[chelsa$source_id == "chelsa_rsdsmean"],
    "W_m-2"
  )
  testthat::expect_true(chelsa$enabled[chelsa$source_id == "chelsa_swb"])
  testthat::expect_true(chelsa$enabled[chelsa$source_id == "chelsa_rsdsmean"])

  soil <- registry[registry$provider == "ISRIC SoilGrids", , drop = FALSE]
  testthat::expect_equal(nrow(soil), 8L)
  testthat::expect_true(all(soil$enabled))
  testthat::expect_true(all(soil$access == "wcs"))
  testthat::expect_true(all(soil$resample_method == "average"))
  testthat::expect_true(all(soil$native_crs == "ESRI:54052"))
  testthat::expect_true(all(soil$wcs_width == 11000L))
  testthat::expect_true(all(soil$wcs_height == 5400L))

  worldpop <- registry[registry$provider == "WorldPop" & registry$enabled, , drop = FALSE]
  testthat::expect_equal(nrow(worldpop), 2L)
  testthat::expect_true(all(worldpop$data_class == "count"))
  testthat::expect_true(all(worldpop$resample_method == "sum"))
  testthat::expect_true(any(worldpop$postprocess == "count_to_density_km2"))

  worldcover <- registry[registry$source_id == "worldcover_2021", , drop = FALSE]
  testthat::expect_false(worldcover$enabled)
})

testthat::test_that("shared cache names require identical acquisition identity", {
  registry <- read_raster_sources(file.path(raster_test_root, "config", "raster_sources.csv"))
  altered <- registry
  altered$url[altered$source_id == "worldpop_2020_density"] <-
    "https://example.test/different-worldpop.tif"
  testthat::expect_error(
    validate_raster_sources(altered),
    "identical acquisition identity"
  )
})

testthat::test_that("offline materialization never falls through to a download", {
  registry <- read_raster_sources(file.path(raster_test_root, "config", "raster_sources.csv"))
  source_row <- registry[registry$source_id == "worldpop_2020_count", , drop = FALSE]
  source_row$expected_sha256 <- NA_character_
  source_row$cache_name <- "empty-cache.tif"
  cache_dir <- tempfile("offline-cache-")
  dir.create(cache_dir)
  file.create(file.path(cache_dir, source_row$cache_name))
  testthat::expect_error(
    materialize_source(
      source_row,
      cache_dir,
      c(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
      allow_download = FALSE
    ),
    "Offline mode"
  )
})

testthat::test_that("canonical grid is exact 30 arc-seconds", {
  pipeline <- read_pipeline_config(file.path(raster_test_root, "config", "pipeline.yml"))
  configured <- canonical_grid(
    pipeline_bbox(pipeline), pipeline$grid$resolution_arcsec, pipeline$grid$crs,
    c(pipeline$grid$origin_x, pipeline$grid$origin_y)
  )
  testthat::expect_equal(terra::res(configured), rep(1 / 120, 2), tolerance = 1e-14)
  testthat::expect_equal(
    unname(as.vector(terra::ext(configured))),
    c(128, 143, 30, 42),
    tolerance = 1e-12
  )
})

testthat::test_that("type-safe raster methods remain enforced", {
  continuous <- terra::rast(
    xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    ncol = 2, nrow = 2, crs = "EPSG:4326",
    vals = c(0, 10, 20, 30)
  )
  fine <- terra::rast(
    xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    ncol = 4, nrow = 4, crs = "EPSG:4326"
  )
  interpolated <- align_public_raster(continuous, fine, method = "bilinear")
  testthat::expect_true(terra::compareGeom(interpolated, fine, stopOnError = FALSE))

  categorical <- continuous
  terra::values(categorical) <- c(10, 20, 30, 40)
  nearest <- align_public_raster(categorical, fine, method = "near")
  testthat::expect_true(
    all(stats::na.omit(terra::values(nearest)) %in% c(10, 20, 30, 40))
  )
})

testthat::test_that("prepared outputs retain observed source and processed hashes", {
  registry <- read_raster_sources(file.path(raster_test_root, "config", "raster_sources.csv"))
  source_row <- registry[registry$source_id == "worldpop_2020_count", , drop = FALSE]
  source_row$expected_sha256 <- NA_character_
  source_row$cache_name <- "synthetic_worldpop.tif"
  source_row$output_name <- "synthetic_worldpop_30s.tif"

  cache_dir <- tempfile("raster-cache-")
  processed_dir <- tempfile("raster-processed-")
  dir.create(cache_dir)
  dir.create(processed_dir)

  source <- terra::rast(
    xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    ncol = 4, nrow = 4, crs = "EPSG:4326", vals = rep(1, 16)
  )
  terra::writeRaster(source, file.path(cache_dir, source_row$cache_name))
  template <- terra::rast(
    xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    ncol = 2, nrow = 2, crs = "EPSG:4326"
  )
  manifest <- prepare_source(
    source_row, cache_dir, processed_dir, template,
    bbox = c(xmin = 0, xmax = 2, ymin = 0, ymax = 2)
  )

  testthat::expect_match(manifest$cache_sha256, "^[0-9a-f]{64}$")
  testthat::expect_match(manifest$processed_sha256, "^[0-9a-f]{64}$")
  testthat::expect_match(manifest$processing_fingerprint, "^[0-9a-f]{64}$")
  testthat::expect_equal(manifest$resample_method, "sum")
})
