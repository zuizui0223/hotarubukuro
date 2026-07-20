locate_test_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")
  hit <- candidates[file.exists(file.path(candidates, "R", "raster_sources.R"))]
  if (!length(hit)) stop("Repository root not found from test working directory.")
  normalizePath(hit[[1]], winslash = "/")
}

raster_test_root <- locate_test_root()
source(file.path(raster_test_root, "R", "raster_sources.R"))
registry_path <- file.path(raster_test_root, "config", "raster_sources.csv")

expected_enabled_sources <- c(
  "chelsa_bio05", "chelsa_bio10", "chelsa_gdd5",
  "chelsa_bio12", "chelsa_bio14", "chelsa_bio15",
  "chelsa_cmimean", "chelsa_vpdmean", "chelsa_swb", "chelsa_rsdsmean",
  "soilgrids_bdod_0_5cm", "soilgrids_cfvo_0_5cm",
  "soilgrids_sand_0_5cm", "soilgrids_silt_0_5cm",
  "soilgrids_nitrogen_0_5cm", "soilgrids_ocd_0_5cm",
  "soilgrids_soc_0_5cm", "soilgrids_phh2o_0_5cm",
  "worldclim_elevation_30s",
  "worldpop_2020_count", "worldpop_2020_density"
)

testthat::test_that("expanded public registry contains the intended 21 predictors", {
  registry <- read_raster_sources(registry_path)
  enabled <- registry[registry$enabled, , drop = FALSE]

  testthat::expect_setequal(enabled$source_id, expected_enabled_sources)
  testthat::expect_length(enabled$source_id, length(expected_enabled_sources))
  testthat::expect_true(all(grepl("^https://", enabled$url)))
  testthat::expect_true(!anyDuplicated(enabled$source_id))
  testthat::expect_true(!anyDuplicated(enabled$output_name))

  pinned <- !is.na(enabled$expected_sha256) & nzchar(enabled$expected_sha256)
  testthat::expect_true(all(grepl("^[0-9a-f]{64}$", enabled$expected_sha256[pinned])))

  rsds <- enabled[enabled$source_id == "chelsa_rsdsmean", , drop = FALSE]
  swb <- enabled[enabled$source_id == "chelsa_swb", , drop = FALSE]
  testthat::expect_equal(rsds$unit[[1]], "W_m-2")
  testthat::expect_equal(swb$unit[[1]], "kg_m-2")

  soil <- enabled[enabled$provider == "ISRIC SoilGrids", , drop = FALSE]
  testthat::expect_length(soil$source_id, 8L)
  testthat::expect_true(all(soil$access == "wcs"))
  testthat::expect_true(all(soil$resample_method == "average"))
  testthat::expect_true(all(soil$native_crs == "ESRI:54052"))

  worldpop <- enabled[enabled$provider == "WorldPop", , drop = FALSE]
  testthat::expect_length(worldpop$source_id, 2L)
  testthat::expect_true(all(worldpop$data_class == "count"))
  testthat::expect_true(all(worldpop$resample_method == "sum"))

  worldcover <- registry[registry$source_id == "worldcover_2021", , drop = FALSE]
  testthat::expect_identical(worldcover$enabled[[1]], FALSE)
})

testthat::test_that("shared cache names require identical acquisition identity", {
  registry <- read_raster_sources(registry_path)
  altered <- registry
  altered$url[altered$source_id == "worldpop_2020_density"] <-
    "https://example.test/different-worldpop.tif"
  testthat::expect_error(
    validate_raster_sources(altered),
    "identical acquisition identity"
  )
})

testthat::test_that("offline materialization never downloads an empty cache", {
  registry <- read_raster_sources(registry_path)
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

testthat::test_that("canonical grid is anchored at 30 arc-seconds", {
  pipeline <- read_pipeline_config(file.path(raster_test_root, "config", "pipeline.yml"))
  configured <- canonical_grid(
    pipeline_bbox(pipeline),
    pipeline$grid$resolution_arcsec,
    pipeline$grid$crs,
    c(pipeline$grid$origin_x, pipeline$grid$origin_y)
  )

  testthat::expect_equal(terra::res(configured), c(1 / 120, 1 / 120), tolerance = 1e-14)
  testthat::expect_equal(
    unname(as.vector(terra::ext(configured))),
    c(128, 143, 30, 42),
    tolerance = 1e-12
  )
})

testthat::test_that("raster alignment retains type-safe methods", {
  continuous <- terra::rast(
    xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    ncol = 2, nrow = 2, crs = "EPSG:4326",
    vals = c(0, 10, 20, 30)
  )
  target <- terra::rast(
    xmin = 0, xmax = 2, ymin = 0, ymax = 2,
    ncol = 4, nrow = 4, crs = "EPSG:4326"
  )

  interpolated <- align_public_raster(continuous, target, method = "bilinear")
  testthat::expect_true(terra::compareGeom(interpolated, target, stopOnError = FALSE))

  categorical <- continuous
  terra::values(categorical) <- c(10, 20, 30, 40)
  nearest <- align_public_raster(categorical, target, method = "near")
  values <- stats::na.omit(as.vector(terra::values(nearest)))
  testthat::expect_true(all(values %in% c(10, 20, 30, 40)))
})
