locate_test_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")
  hit <- candidates[file.exists(file.path(candidates, "R", "raster_sources.R"))]
  if (!length(hit)) stop("Repository root not found from test working directory.")
  normalizePath(hit[[1]], winslash = "/")
}

raster_test_root <- locate_test_root()
source(file.path(raster_test_root, "R", "raster_sources.R"))

testthat::test_that("public source registry pins official products and semantics", {
  registry <- read_raster_sources(file.path(raster_test_root, "config", "raster_sources.csv"))
  testthat::expect_true(all(grepl("^https://", registry$url)))

  chelsa <- registry[registry$provider == "CHELSA", , drop = FALSE]
  testthat::expect_true(nrow(chelsa) >= 10L)
  ordinary_chelsa <- chelsa[chelsa$source_id != "chelsa_rsdsmean", , drop = FALSE]
  testthat::expect_true(all(grepl(
    "^https://os\\.unil\\.cloud\\.switch\\.ch/chelsa02/",
    ordinary_chelsa$url
  )))
  testthat::expect_true(all(chelsa$dataset_version == "2.1"))
  testthat::expect_true(all(chelsa$resample_method == "bilinear"))
  cmi <- chelsa[chelsa$source_id == "chelsa_cmimean", , drop = FALSE]
  testthat::expect_equal(cmi$unit, "kg_m-2_month-1")
  rsds <- chelsa[chelsa$source_id == "chelsa_rsdsmean", , drop = FALSE]
  testthat::expect_false(rsds$enabled)
  testthat::expect_equal(rsds$unit, "unresolved_provider_metadata_conflict")
  testthat::expect_match(
    rsds$url,
    "^https://os\\.zhdk\\.cloud\\.switch\\.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/"
  )
  swb <- chelsa[chelsa$source_id == "chelsa_swb", , drop = FALSE]
  testthat::expect_false(swb$enabled)

  soil <- registry[registry$provider == "ISRIC SoilGrids", , drop = FALSE]
  testthat::expect_setequal(
    sub("soilgrids_([^_]+).*", "\\1", soil$source_id),
    c("bdod", "cfvo", "sand", "silt", "nitrogen", "ocd", "soc", "phh2o")
  )
  testthat::expect_true(all(soil$access == "vrt"))
  testthat::expect_true(all(grepl("^https://files\\.isric\\.org/soilgrids/latest/data/", soil$url)))
  testthat::expect_true(all(soil$resample_method == "average"))

  worldpop <- registry[registry$provider == "WorldPop" & registry$enabled, , drop = FALSE]
  testthat::expect_equal(nrow(worldpop), 2L)
  testthat::expect_true(all(worldpop$data_class == "count"))
  testthat::expect_true(all(worldpop$resample_method == "sum"))
  testthat::expect_true(any(worldpop$postprocess == "count_to_density_km2"))

  elevation <- registry[registry$source_id == "worldclim_elevation_30s", , drop = FALSE]
  testthat::expect_equal(elevation$access, "zip")
  testthat::expect_equal(elevation$archive_member, "wc2.1_30s_elev.tif")
  worldcover <- registry[registry$source_id == "worldcover_2021", , drop = FALSE]
  testthat::expect_false(worldcover$enabled)
  testthat::expect_equal(worldcover$resample_method, "near")
})

testthat::test_that("canonical grid is globally anchored at exact 30 arc-seconds", {
  grid <- canonical_grid(c(0.001, 0.021, 0.001, 0.021), resolution_arcsec = 30)
  testthat::expect_equal(terra::res(grid), rep(1 / 120, 2), tolerance = 1e-14)
  testthat::expect_equal(unname(as.vector(terra::ext(grid))), c(0, 0.025, 0, 0.025), tolerance = 1e-12)
  testthat::expect_equal(terra::origin(grid), c(0, 0), tolerance = 1e-12)

  pipeline <- read_pipeline_config(file.path(raster_test_root, "config", "pipeline.yml"))
  configured <- canonical_grid(
    pipeline_bbox(pipeline), pipeline$grid$resolution_arcsec, pipeline$grid$crs,
    c(pipeline$grid$origin_x, pipeline$grid$origin_y)
  )
  testthat::expect_equal(terra::res(configured), rep(1 / 120, 2), tolerance = 1e-14)
  testthat::expect_equal(unname(as.vector(terra::ext(configured))), c(128, 143, 30, 42), tolerance = 1e-12)
})

testthat::test_that("continuous and categorical rasters use type-safe methods", {
  continuous <- terra::rast(xmin = 0, xmax = 2, ymin = 0, ymax = 2, ncol = 2, nrow = 2,
                            crs = "EPSG:4326", vals = c(0, 10, 20, 30))
  fine <- terra::rast(xmin = 0, xmax = 2, ymin = 0, ymax = 2, ncol = 4, nrow = 4,
                      crs = "EPSG:4326")
  interpolated <- align_public_raster(continuous, fine, method = "bilinear")
  testthat::expect_true(terra::compareGeom(interpolated, fine, stopOnError = FALSE))
  testthat::expect_true(all(terra::values(interpolated) >= 0 & terra::values(interpolated) <= 30))

  categorical <- continuous
  terra::values(categorical) <- c(10, 20, 30, 40)
  nearest <- align_public_raster(categorical, fine, method = "near")
  testthat::expect_true(all(stats::na.omit(terra::values(nearest)) %in% c(10, 20, 30, 40)))
})

testthat::test_that("WorldPop count aggregation preserves totals before density conversion", {
  source <- terra::rast(xmin = 0, xmax = 2, ymin = 0, ymax = 2, ncol = 4, nrow = 4,
                        crs = "EPSG:4326", vals = rep(1, 16))
  target <- terra::rast(xmin = 0, xmax = 2, ymin = 0, ymax = 2, ncol = 2, nrow = 2,
                        crs = "EPSG:4326")
  counts <- align_public_raster(source, target, method = "sum")
  testthat::expect_equal(as.numeric(terra::global(counts, "sum", na.rm = TRUE)[1, 1]), 16,
                         tolerance = 1e-8)
  testthat::expect_equal(as.vector(terra::values(counts)), rep(4, 4), tolerance = 1e-8)

  density <- align_public_raster(source, target, method = "sum",
                                 postprocess = "count_to_density_km2")
  reconstructed <- density * terra::cellSize(target, unit = "km")
  testthat::expect_equal(as.numeric(terra::global(reconstructed, "sum", na.rm = TRUE)[1, 1]), 16,
                         tolerance = 1e-6)
})

testthat::test_that("SoilGrids integer encodings have explicit conversion factors", {
  registry <- read_raster_sources(file.path(raster_test_root, "config", "raster_sources.csv"))
  scale <- setNames(registry$scale_factor, registry$source_id)
  testthat::expect_equal(scale[["soilgrids_bdod_0_5cm"]], 0.01)
  testthat::expect_equal(scale[["soilgrids_phh2o_0_5cm"]], 0.1)
  testthat::expect_equal(scale[["soilgrids_sand_0_5cm"]], 0.1)
})

testthat::test_that("prepared raster manifest records source and output hashes", {
  registry <- read_raster_sources(file.path(raster_test_root, "config", "raster_sources.csv"))
  source_row <- registry[registry$source_id == "worldpop_2020_count", , drop = FALSE]
  source_row$cache_name <- "synthetic_worldpop.tif"
  source_row$output_name <- "synthetic_worldpop_30s.tif"
  cache_dir <- tempfile("raster-cache-")
  processed_dir <- tempfile("raster-processed-")
  dir.create(cache_dir)
  dir.create(processed_dir)

  source <- terra::rast(xmin = 0, xmax = 2, ymin = 0, ymax = 2, ncol = 4, nrow = 4,
                        crs = "EPSG:4326", vals = rep(1, 16))
  terra::writeRaster(source, file.path(cache_dir, source_row$cache_name))
  template <- terra::rast(xmin = 0, xmax = 2, ymin = 0, ymax = 2, ncol = 2, nrow = 2,
                          crs = "EPSG:4326")
  manifest <- prepare_source(
    source_row, cache_dir, processed_dir, template,
    bbox = c(xmin = 0, xmax = 2, ymin = 0, ymax = 2)
  )
  testthat::expect_match(manifest$cache_sha256, "^[0-9a-f]{64}$")
  testthat::expect_match(manifest$processed_sha256, "^[0-9a-f]{64}$")
  testthat::expect_equal(manifest$resample_method, "sum")
  testthat::expect_match(manifest$processing_fingerprint, "^[0-9a-f]{64}$")
  testthat::expect_equal(manifest$output_value_semantics, "population_count_people_per_cell")
  output <- terra::rast(manifest$processed_path)
  testthat::expect_true(terra::compareGeom(output, template, stopOnError = FALSE))
  testthat::expect_equal(as.numeric(terra::global(output, "sum", na.rm = TRUE)[1, 1]), 16,
                         tolerance = 1e-6)
})

testthat::test_that("processed settings fingerprints detect stale rasters and rebuild safely", {
  registry <- read_raster_sources(file.path(raster_test_root, "config", "raster_sources.csv"))
  source_row <- registry[registry$source_id == "worldpop_2020_count", , drop = FALSE]
  source_row$cache_name <- "stale-source.tif"
  source_row$output_name <- "stale-output.tif"
  cache_dir <- tempfile("raster-cache-")
  processed_dir <- tempfile("raster-processed-")
  dir.create(cache_dir)
  dir.create(processed_dir)
  source <- terra::rast(xmin = 0, xmax = 2, ymin = 0, ymax = 2, ncol = 4, nrow = 4,
                        crs = "EPSG:4326", vals = rep(1, 16))
  terra::writeRaster(source, file.path(cache_dir, source_row$cache_name))
  template <- terra::rast(xmin = 0, xmax = 2, ymin = 0, ymax = 2, ncol = 2, nrow = 2,
                          crs = "EPSG:4326")

  first <- prepare_source(source_row, cache_dir, processed_dir, template,
                          bbox = c(xmin = 0, xmax = 2, ymin = 0, ymax = 2))
  testthat::expect_false(processed_raster_status(
    first$processed_path, first$processing_fingerprint
  )$stale)
  testthat::expect_true(file.exists(processing_fingerprint_path(first$processed_path)))

  changed <- source_row
  changed$scale_factor <- 2
  expected <- processing_fingerprint(changed, file.path(cache_dir, changed$cache_name), template)
  testthat::expect_true(processed_raster_status(first$processed_path, expected)$stale)
  rebuilt <- prepare_source(changed, cache_dir, processed_dir, template,
                            bbox = c(xmin = 0, xmax = 2, ymin = 0, ymax = 2))
  testthat::expect_false(identical(first$processing_fingerprint, rebuilt$processing_fingerprint))
  testthat::expect_equal(as.vector(terra::values(terra::rast(rebuilt$processed_path))), rep(8, 4))
})

testthat::test_that("atomic raster writes retain the prior complete raster on failure", {
  destination <- tempfile("atomic-raster-", fileext = ".tif")
  old <- terra::rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1, ncol = 1, nrow = 1,
                     crs = "EPSG:4326", vals = 7)
  terra::writeRaster(old, destination)
  testthat::expect_error(write_raster_atomic("not-a-raster", destination))
  testthat::expect_equal(as.numeric(terra::values(terra::rast(destination))), 7)
})
