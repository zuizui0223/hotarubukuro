source(testthat::test_path("../../R/local_bombus_turnover.R"))

v17_test_cells <- function() {
  data.frame(
    exact_site_id = paste0("cell-", 1:8),
    x_km = c(0, 1, 2, 3, 20, 21, 22, 23),
    y_km = 0,
    spatial_fold = rep(c(1, 2), each = 4),
    bombus_fingerprint_common_support = TRUE,
    bombus_total_habitat_support = seq(0.2, 0.9, length.out = 8),
    bombus_composition_pc1 = seq(-1, 1, length.out = 8),
    bombus_composition_pc2 = rep(c(-0.5, 0.5), 4),
    ardens_within_species_rank = seq(0.1, 0.8, length.out = 8),
    diversus_within_species_rank = seq(0.8, 0.1, length.out = 8),
    beaticola_within_species_rank = 0.3,
    consobrinus_within_species_rank = 0.2,
    honshuensis_within_species_rank = 0.1,
    broad50km_pc1 = seq(-1, 1, length.out = 8),
    broad50km_pc2 = 0,
    within50km_pc1 = rep(c(-1, 1), 4),
    within50km_pc2 = seq(1, -1, length.out = 8),
    n_pigmented = c(0, 1, 1, 0, 1, 0, 1, 0),
    n_observations = 1,
    conditional_intensity_median = c(NA, 1, 2, NA, 1.5, NA, 2.5, NA),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("pair graph is response blind, unique, and fold joint", {
  cells <- v17_test_cells()
  graph <- v17_pair_graph(cells, radius_km = 5, k = 2)
  changed <- cells
  changed$n_pigmented <- rev(changed$n_pigmented)
  graph_changed <- v17_pair_graph(changed, radius_km = 5, k = 2)
  testthat::expect_equal(
    graph[, c("site_i", "site_j")],
    graph_changed[, c("site_i", "site_j")]
  )
  testthat::expect_false(anyDuplicated(graph$edge_id) > 0)
  testthat::expect_true(all(graph$fold_i == graph$fold_j))
})

testthat::test_that("pair graph safely skips isolated cells", {
  cells <- v17_test_cells()
  cells$x_km[8] <- 1000
  graph <- v17_pair_graph(cells, radius_km = 5, k = 2)
  testthat::expect_false("cell-8" %in% c(graph$site_i, graph$site_j))
  testthat::expect_gt(nrow(graph), 0)
})

testthat::test_that("conditional intensity remains absent outside pigmented support", {
  cells <- v17_test_cells()
  graph <- v17_pair_graph(cells, radius_km = 5, k = 3)
  template <- list(
    latent_mean = seq(0.1, 0.8, length.out = 8),
    draws = matrix(seq(0.1, 0.8, length.out = 8), 8, 10)
  )
  featured <- v17_add_pair_features(graph, cells, template, template)
  testthat::expect_true(all(
    is.na(featured$observed_intensity_transition[
      !featured$conditional_intensity_pair
    ])
  ))
  testthat::expect_true(all(
    is.finite(featured$observed_intensity_transition[
      featured$conditional_intensity_pair
    ])
  ))
})

testthat::test_that("partial statistic recovers a positive added association", {
  set.seed(17)
  n <- 200
  baseline <- cbind(distance = stats::runif(n), environment = stats::rnorm(n))
  predictor <- stats::rnorm(n)
  y <- 0.3 * baseline[, 1] + 0.7 * predictor + stats::rnorm(n, sd = 0.2)
  statistic <- v17_fit_partial_statistic(y, baseline, predictor)
  testthat::expect_gt(statistic[["beta"]], 0.5)
  testthat::expect_gt(statistic[["delta_r2"]], 0.5)
})

testthat::test_that("multiplicity adjustment isolates the 25-km primary family", {
  summary <- expand.grid(
    response = c("presence", "intensity"),
    radius_km = c(10, 25, 50),
    predictor_role = c("primary_turnover", "support_level_context"),
    stringsAsFactors = FALSE
  )
  summary$beta_empirical_p <- seq(0.01, 0.12, length.out = nrow(summary))
  adjusted <- v17_adjust_multiplicity(summary)
  primary <- adjusted$predictor_role == "primary_turnover" &
    adjusted$radius_km == 25
  testthat::expect_equal(sum(is.finite(adjusted$BH_q_primary_25km)), 2)
  testthat::expect_true(all(is.finite(
    adjusted$BH_q_primary_25km[primary]
  )))
})
