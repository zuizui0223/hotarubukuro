source(testthat::test_path("..", "..", "R", "human_raster_features.R"))

testthat::test_that("Japanese primary mesh codes are decoded from coordinates", {
  testthat::expect_identical(
    primary_mesh_code(c(139.75, 135.5), c(35.5, 34.7)),
    c("5339", "5235")
  )
})

testthat::test_that("tertiary mesh centres follow the JIS grid", {
  centre <- decode_tertiary_mesh_centres("53395000")
  testthat::expect_equal(centre$longitude, 139 + 1 / 160)
  testthat::expect_equal(centre$latitude, 53 / 1.5 + 5 / 12 + 1 / 240)
})

testthat::test_that("forest-human adjacency is counted once across a 100 m edge", {
  synthetic <- data.frame(
    L03b_001 = c("5339000000", "5339000001", "5339000010", "5339000011"),
    L03b_002 = c("0500", "0700", "0500", "0700"),
    stringsAsFactors = FALSE
  )
  result <- aggregate_mlit_landuse_dbf(synthetic)
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$human_forest_edge_km_per_nominal_km2, 0.2)
  testthat::expect_equal(result$forest_fraction, 0.02)
  testthat::expect_equal(result$human_managed_fraction, 0.02)
})

testthat::test_that("non-managed classes do not create an interface", {
  synthetic <- data.frame(
    L03b_001 = c("5339000000", "5339000001"),
    L03b_002 = c("0500", "0600"),
    stringsAsFactors = FALSE
  )
  result <- aggregate_mlit_landuse_dbf(synthetic)
  testthat::expect_equal(result$human_forest_edge_km_per_nominal_km2, 0)
})

testthat::test_that("major-road distance is independent of the flower response", {
  synthetic <- data.frame(
    L03b_001 = c("5339000000", "5339000001", "5339000010"),
    L03b_002 = c("0500", "0901", "0700"),
    stringsAsFactors = FALSE
  )
  result <- aggregate_mlit_landuse_dbf(synthetic)
  testthat::expect_true(is.finite(result$major_road_distance_km))
  testthat::expect_equal(result$major_road_fraction, 0.01)
})
