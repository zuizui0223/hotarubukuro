source(testthat::test_path("..", "..", "scripts", "transition_zone_design_v5.R"))
source(testthat::test_path("..", "..", "scripts", "community_threshold_v6.R"))

community_test_sites <- function() {
  n <- 12
  data.frame(
    exact_site_id = paste0("c", seq_len(n)),
    date = "2024-06-01", year = 2024,
    longitude = 135 + seq_len(n) / 100,
    latitude = 35, x_km = seq_len(n), y_km = 0,
    elevation = seq_len(n), DOY = 180 + seq_len(n),
    Temperature_PC1 = seq(-1, 1, length.out = n),
    precip_PC1 = seq(1, -1, length.out = n),
    TemperatureSeasonality = sin(seq_len(n)),
    PrecipSeasonality = cos(seq_len(n)),
    topo_PC1 = seq(-0.5, 0.5, length.out = n),
    soil_PC1 = seq(-1, 1, length.out = n)^2,
    soil_PC2 = seq(1, -1, length.out = n)^2,
    RSDS = seq(-0.2, 0.2, length.out = n),
    pigmented_mixture50 = c(rep(0L, 6), rep(1L, 6)),
    pigmented_high_confidence = c(rep(0L, 6), rep(1L, 6)),
    pigment_intensity_z = c(rep(NA_real_, 6), seq(-1, 1, length.out = 6)),
    colour_a = c(rep(-2, 6), seq(10, 30, length.out = 6)),
    bee_ardens = seq(0.1, 0.9, length.out = n),
    bee_diversus = seq(0.2, 0.8, length.out = n),
    bee_beaticola = seq(0.05, 0.95, length.out = n),
    bee_consobrinus = seq(0.1, 0.7, length.out = n),
    bee_honshuensis = seq(0.15, 0.85, length.out = n),
    spatial_fold = rep(1:3, length.out = n),
    z_H = seq(-1, 1, length.out = n),
    z_R = seq(1, -1, length.out = n),
    z_A = 0,
    stringsAsFactors = FALSE
  )
}

testthat::test_that("community axes retain availability, diversity, and composition", {
  sites <- transition_site_table(community_test_sites())
  result <- community_add_axes(sites, structural_zero = TRUE)
  testthat::expect_true(all(result$data$bombus_availability > 0))
  testthat::expect_true(all(result$data$bombus_effective_richness >= 1))
  testthat::expect_true(all(result$data$bombus_effective_richness <= 5 + 1e-8))
  testthat::expect_true(all(result$data$bombus_alpine_share >= 0 &
                              result$data$bombus_alpine_share <= 1))
})

testthat::test_that("projection-support gaps are explicit and sensitivity-ready", {
  data <- community_test_sites()
  data$bee_beaticola[1:3] <- NA_real_
  sites <- transition_site_table(data)
  result <- community_add_axes(sites, structural_zero = TRUE)
  testthat::expect_false(all(result$data$bombus_common_support))
  testthat::expect_equal(result$data$bombus_coverage_n[1], 4)
  testthat::expect_equal(
    unname(result$axes$normalized[1, "beaticola_availability_rank"]), 0
  )
})

testthat::test_that("change points are selected from the training distribution", {
  x <- seq(0, 1, length.out = 101)
  points <- community_threshold_candidates(x)
  testthat::expect_true(min(points) >= 0.15)
  testthat::expect_true(max(points) <= 0.85)
  testthat::expect_gt(length(points), 5)
})

testthat::test_that("horticultural candidates do not depend on facet values", {
  sites <- transition_site_table(community_test_sites())
  sites <- community_add_axes(sites, TRUE)$data
  neighbourhoods <- transition_local_neighbourhoods(sites, radii_km = 25)
  background <- rep(0.5, nrow(sites))
  first <- community_candidate_table(
    sites, neighbourhoods, background, rep(0, nrow(sites)), rep(0, nrow(sites)), 25
  )
  sites$z_H <- rev(sites$z_H)
  sites$z_R <- rev(sites$z_R)
  second <- community_candidate_table(
    sites, neighbourhoods, background, rep(10, nrow(sites)), rep(-10, nrow(sites)), 25
  )
  testthat::expect_identical(first$exact_site_id, second$exact_site_id)
})

testthat::test_that("facet evidence is summarized separately with multiplicity control", {
  pairs <- data.frame(
    candidate_block = rep(c("a", "b", "c"), each = 3),
    delta_z_H = seq(-1, 1, length.out = 9),
    delta_z_R = rep(0.5, 9),
    delta_early_natural_score_v13 = seq(1, -1, length.out = 9)
  )
  summary <- community_facet_summary(pairs, repetitions = 100)
  testthat::expect_equal(nrow(summary), 3)
  testthat::expect_true(all(summary$simultaneous_requirement == FALSE))
  testthat::expect_true(all(summary$BH_q >= 0 & summary$BH_q <= 1))
})
