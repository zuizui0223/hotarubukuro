source(testthat::test_path("..", "..", "scripts", "transition_zone_design_v5.R"))

transition_test_data <- function() {
  data.frame(
    exact_site_id = paste0("s", 1:8),
    date = rep("2024-06-01", 8),
    longitude = 135 + 0:7 / 100,
    latitude = rep(35, 8),
    x_km = 0:7,
    y_km = rep(0, 8),
    elevation = 1:8,
    DOY = 150 + 1:8,
    Temperature_PC1 = seq(-1, 1, length.out = 8),
    pigmented_mixture50 = c(0, 0, 0, 0, 0, 0, 0, 1),
    pigmented_high_confidence = c(0, 0, 0, 0, 0, 0, 0, 1),
    colour_a = c(rep(-2, 7), 30),
    bee_ardens = seq(0.1, 0.8, length.out = 8),
    bee_diversus = seq(0.2, 0.9, length.out = 8),
    bee_beaticola = rev(seq(0.1, 0.8, length.out = 8)),
    bee_consobrinus = rev(seq(0.2, 0.9, length.out = 8)),
    bee_honshuensis = rev(seq(0.15, 0.85, length.out = 8)),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("site aggregation preserves the two-part flower response", {
  data <- transition_test_data()
  duplicated <- rbind(data, transform(data[1, ], pigmented_mixture50 = 1L))
  sites <- transition_site_table(duplicated)
  mixed <- sites[sites$exact_site_id == "s1", ]
  testthat::expect_identical(mixed$site_class, "mixed")
  testthat::expect_equal(mixed$n_white, 1)
  testthat::expect_equal(mixed$n_pigmented, 1)
})

testthat::test_that("population cells absorb coordinate jitter without replication", {
  data <- transition_test_data()
  jittered <- data[1, ]
  jittered$exact_site_id <- "s1-jitter"
  jittered$x_km <- data$x_km[1] + 0.01
  jittered$pigmented_mixture50 <- 1L
  jittered$pigmented_high_confidence <- 1L
  populations <- transition_population_table(rbind(data, jittered), cell_km = 1)
  first <- populations[grepl("-0_0$", populations$exact_site_id), ]
  testthat::expect_equal(nrow(first), 1)
  testthat::expect_identical(first$site_class, "mixed")
  testthat::expect_equal(first$n_exact_sites, 2)
})

testthat::test_that("local enclave detection is symmetric by construction", {
  sites <- transition_site_table(transition_test_data())
  neighbourhoods <- transition_local_neighbourhoods(sites, radii_km = 10)
  candidates <- transition_candidate_table(sites, neighbourhoods, radius_km = 10)
  testthat::expect_identical(
    candidates$candidate_direction[candidates$exact_site_id == "s8"],
    "pigmented_in_white_background"
  )

  reversed <- transition_test_data()
  reversed$pigmented_mixture50 <- 1L - reversed$pigmented_mixture50
  reversed$pigmented_high_confidence <- 1L - reversed$pigmented_high_confidence
  sites_reversed <- transition_site_table(reversed)
  neighbours_reversed <- transition_local_neighbourhoods(sites_reversed, radii_km = 10)
  candidates_reversed <- transition_candidate_table(
    sites_reversed, neighbours_reversed, radius_km = 10
  )
  testthat::expect_identical(
    candidates_reversed$candidate_direction[
      candidates_reversed$exact_site_id == "s8"
    ],
    "white_in_pigmented_background"
  )
})

testthat::test_that("Bombus axes normalize within species before composition", {
  sites <- transition_site_table(transition_test_data())
  axes <- transition_bombus_axes(sites)
  testthat::expect_equal(dim(axes$normalized), c(8, 5))
  testthat::expect_true(all(axes$normalized >= 0 & axes$normalized <= 1))
  testthat::expect_true(all(axes$availability >= 0 & axes$availability <= 1))
})

testthat::test_that("short edge table includes flower and Bombus turnover", {
  sites <- transition_site_table(transition_test_data())
  edges <- transition_local_edges(sites, k = 2, maximum_km = 3)
  testthat::expect_gt(nrow(edges), 0)
  testthat::expect_true(all(edges$geographic_distance_km <= 3))
  testthat::expect_true(any(edges$flower_discordant == 1L))
  testthat::expect_true(all(is.finite(edges$bombus_composition_turnover)))
})

testthat::test_that("hotspot pairs are flower-defined and can be made independent", {
  sites <- transition_site_table(transition_test_data())
  edges <- transition_local_edges(sites, k = 3, maximum_km = 10)
  hotspots <- transition_hotspot_pairs(sites, edges, maximum_km = 10)
  testthat::expect_true(all(hotspots$flower_discordant == 1L))
  testthat::expect_true(all(hotspots$binary_transition_gradient_per_km > 0))
  selected <- hotspots[hotspots$independent_pair, ]
  testthat::expect_equal(
    anyDuplicated(c(selected$site_i, selected$site_j)), 0
  )
})

testthat::test_that("environment-matched pairs do not reuse sites", {
  sites <- transition_site_table(transition_test_data())
  pairs <- transition_matched_pairs(sites, maximum_km = 10, environment_caliper = 5)
  testthat::expect_gt(nrow(pairs), 0)
  testthat::expect_equal(anyDuplicated(pairs$pigmented_site), 0)
  testthat::expect_equal(anyDuplicated(pairs$white_site), 0)
  testthat::expect_true(all(pairs$geographic_distance_km <= 10))
})

testthat::test_that("spatial-block summary retains disjunct transition replication", {
  sites <- transition_site_table(transition_test_data())
  neighbourhoods <- transition_local_neighbourhoods(sites, radii_km = 10)
  edges <- transition_local_edges(sites, k = 2, maximum_km = 10)
  blocks <- transition_spatial_block_summary(
    sites, neighbourhoods, edges, radius_km = 10, block_size_km = 3
  )
  testthat::expect_gt(nrow(blocks), 1)
  testthat::expect_equal(sum(blocks$n_sites), nrow(sites))
})

testthat::test_that("edge CV uses strict site-disjoint spatial folds", {
  set.seed(491)
  n <- 180
  data <- data.frame(
    exact_site_id = paste0("cv", seq_len(n)),
    date = "2024-06-01", longitude = 135 + runif(n), latitude = 35 + runif(n),
    x_km = runif(n, 0, 150), y_km = runif(n, 0, 150), elevation = rnorm(n),
    DOY = rnorm(n, 180, 10), Temperature_PC1 = rnorm(n),
    pigmented_mixture50 = rbinom(n, 1, 0.5),
    colour_a = rnorm(n), bee_ardens = runif(n), bee_diversus = runif(n),
    bee_beaticola = runif(n), bee_consobrinus = runif(n),
    bee_honshuensis = runif(n), spatial_fold = sample(1:5, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  data$pigmented_high_confidence <- data$pigmented_mixture50
  sites <- transition_site_table(data)
  edges <- transition_local_edges(sites, k = 5, maximum_km = 100)
  cv <- transition_edge_spatial_cv(sites, edges, maximum_km = 100)
  testthat::expect_true(all(cv$strict_node_disjoint_split))
  testthat::expect_true(all(cv$background_excludes_bombus_region_human))
  testthat::expect_false(any(grepl(
    "Bombus|region|z_H|z_R|z_A", cv$background_formula
  )))
  testthat::expect_true(all(c(
    "base", "enmeval_composition", "enmeval_availability_level",
    "enmeval_availability_gradient", "enmeval_availability", "enmeval_full"
  ) %in% cv$model))
  summary <- transition_edge_cv_summary(cv)
  testthat::expect_true(all(
    summary$n_folds == length(unique(cv$heldout_spatial_fold))
  ))
})

testthat::test_that("enclave report keeps the reciprocal direction as control", {
  sites <- transition_site_table(transition_test_data())
  neighbourhoods <- transition_local_neighbourhoods(sites, radii_km = 10)
  candidates <- transition_candidate_table(sites, neighbourhoods, radius_km = 10)
  reversed <- transition_test_data()
  reversed$exact_site_id <- paste0(reversed$exact_site_id, "r")
  reversed$x_km <- reversed$x_km + 20
  reversed$pigmented_mixture50 <- 1L - reversed$pigmented_mixture50
  reversed$pigmented_high_confidence <- 1L - reversed$pigmented_high_confidence
  sites_reversed <- transition_site_table(reversed)
  neighbours_reversed <- transition_local_neighbourhoods(sites_reversed, radii_km = 10)
  candidates_reversed <- transition_candidate_table(
    sites_reversed, neighbours_reversed, radius_km = 10
  )
  report <- transition_enclave_descriptive(rbind(candidates, candidates_reversed))
  testthat::expect_true(all(c(
    "pigmented_in_white_background", "white_in_pigmented_background"
  ) %in% report$candidate_direction))
})
