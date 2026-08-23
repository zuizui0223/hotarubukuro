source("R/flowering_phenology.R")
source("R/flowering_phenology_elevation.R")

testthat::test_that("cell-year-colour aggregation weights exact sites before images", {
  x <- data.frame(
    exact_site_id = c("a", "a", "b", "w"), x_km = c(0.2, 0.2, 0.3, 0.4),
    y_km = c(0.2, 0.2, 0.3, 0.4), year = c(2023, 2023, 2023, 2023),
    DOY = c(150, 151, 160, 170), pigmented_mixture50 = c(1, 1, 1, 0),
    stringsAsFactors = FALSE)
  out <- fp_build_cell_year_colour(x)
  pig <- out[out$colour == "pigmented", ]
  testthat::expect_equal(nrow(pig), 1L)
  testthat::expect_equal(pig$n_sites, 2L)
  testthat::expect_equal(pig$n_images, 3L)
  testthat::expect_equal(pig$median_doy, median(c(150.5, 160)))
})

testthat::test_that("same-year mutual nearest pairing is one-to-one", {
  x <- data.frame(
    cell_id = c("p1", "p2", "w1", "w2"), year = rep(2023, 4),
    colour = c("pigmented", "pigmented", "white", "white"),
    median_doy = c(150, 160, 155, 170), x_km = c(0, 10, 1, 11),
    y_km = c(0, 0, 0, 0), n_sites = rep(1L, 4), stringsAsFactors = FALSE)
  pairs <- fp_mutual_nearest_pairs(x, max_distance_km = 5)
  testthat::expect_equal(nrow(pairs), 2L)
  testthat::expect_equal(sort(pairs$distance_km), c(1, 1))
  testthat::expect_equal(length(unique(pairs$pigmented_cell_id)), 2L)
  testthat::expect_equal(length(unique(pairs$white_cell_id)), 2L)
  testthat::expect_equal(pairs$early_days[pairs$pigmented_cell_id == "p1"], 5)
})

testthat::test_that("repeated years collapse before inference", {
  x <- data.frame(
    pair_id = c("a", "b"), year = c(2023L, 2024L), pigmented_cell_id = c("p", "p"),
    white_cell_id = c("w", "w"), pigmented_doy = c(150, 152), white_doy = c(160, 158),
    delta_doy = c(-10, -6), early_days = c(10, 6), distance_km = c(2, 2),
    pigmented_n_sites = 1L, white_n_sites = 1L, stringsAsFactors = FALSE)
  out <- fp_collapse_geometric_pairs(x)
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$delta_doy, -8)
  testthat::expect_equal(out$early_days, 8)
  testthat::expect_equal(out$n_years, 2L)
})

testthat::test_that("human join preserves phenology and pre-existing context", {
  pairs <- data.frame(
    geometric_pair_id = "p::w", pigmented_cell_id = "p", white_cell_id = "w",
    delta_doy = -7, early_days = 7, distance_km = 1, n_years = 1L, years = "2023",
    stringsAsFactors = FALSE)
  isolation <- data.frame(
    exact_site_id = c("p", "w"), colour = c("pigmented", "white"), spatial_fold = c(2, 2),
    same_colour_nn_km = c(12, 4), relative_isolation_nn = c(1.5, 0.2),
    local_population_rank = c(0.8, 0.3), population_5km_rank = c(0.9, 0.2),
    population_10km_rank = c(0.8, 0.2), population_25km_rank = c(0.7, 0.2),
    population_50km_rank = c(0.6, 0.2), stringsAsFactors = FALSE)
  out <- fp_human_pair_table(pairs, isolation)
  testthat::expect_equal(out$early_days, 7)
  testthat::expect_equal(out$relative_isolation_nn, 1.5)
  testthat::expect_equal(out$population_5km_rank, 0.9)
})

testthat::test_that("elevation joins by colour cell and year without changing pair timing", {
  pairs <- data.frame(
    pair_id = c("2023::p::w", "2023::p2::w2"), year = c(2023L, 2023L),
    pigmented_cell_id = c("p", "p2"), white_cell_id = c("w", "w2"),
    pigmented_doy = c(150, 160), white_doy = c(156, 165),
    delta_doy = c(-6, -5), early_days = c(6, 5), distance_km = c(0, 2),
    pigmented_n_sites = 1L, white_n_sites = 1L, stringsAsFactors = FALSE)
  elev <- data.frame(
    cell_id = c("p", "w", "p2", "w2"), year = rep(2023L, 4),
    colour = c("pigmented", "white", "pigmented", "white"),
    elevation_m = c(300, 320, 800, 600), n_elevation_sites = 1L,
    stringsAsFactors = FALSE)
  out <- fp_add_pair_elevation(pairs, elev)
  testthat::expect_equal(out$delta_doy, pairs$delta_doy)
  testthat::expect_equal(out$elevation_diff_m, c(-20, 200))
  testthat::expect_equal(out$abs_elevation_diff_m, c(20, 200))
})

testthat::test_that("elevation guardrail applies frozen same-cell and threshold scopes", {
  pairs <- data.frame(
    pair_id = paste0("x", 1:4), year = rep(2023L, 4),
    pigmented_cell_id = paste0("p", 1:4), white_cell_id = paste0("w", 1:4),
    pigmented_doy = c(150, 151, 152, 153), white_doy = c(160, 160, 160, 160),
    delta_doy = c(-10, -9, -8, -7), early_days = c(10, 9, 8, 7),
    distance_km = c(0, 1, 2, 3), pigmented_n_sites = 1L, white_n_sites = 1L,
    pigmented_elevation_m = c(100, 200, 300, 400),
    white_elevation_m = c(100, 250, 450, 700),
    elevation_diff_m = c(0, -50, -150, -300),
    abs_elevation_diff_m = c(0, 50, 150, 300), stringsAsFactors = FALSE)
  out <- fp_elevation_guardrails(pairs, permutations = 199L, seed = 2L)
  same <- out$summary[out$summary$scope == "same_1km_cell", ]
  within100 <- out$summary[out$summary$scope == "abs_elevation_diff_le_100m", ]
  within250 <- out$summary[out$summary$scope == "abs_elevation_diff_le_250m", ]
  testthat::expect_equal(same$n_unique_geometric_pairs, 1L)
  testthat::expect_equal(within100$n_unique_geometric_pairs, 2L)
  testthat::expect_equal(within250$n_unique_geometric_pairs, 3L)
  testthat::expect_equal(out$correlation$n_unique_geometric_pairs, 4L)
})
