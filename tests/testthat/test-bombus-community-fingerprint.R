source(testthat::test_path("..", "..", "R", "environment_spatial.R"))
source(testthat::test_path("..", "..", "R", "local_transition_pairs.R"))
source(testthat::test_path("..", "..", "R", "hotspot_candidates.R"))
source(testthat::test_path("..", "..", "R", "bombus_community_fingerprint.R"))

testthat::test_that("community fingerprint uses suitability weights on common support", {
  set.seed(7)
  sites <- data.frame(
    bee_ardens = runif(30), bee_diversus = runif(30),
    bee_beaticola = runif(30), bee_consobrinus = runif(30),
    bee_honshuensis = runif(30)
  )
  sites$bee_beaticola[1] <- NA_real_
  result <- fingerprint_add_axes(sites)
  testthat::expect_false(result$data$bombus_fingerprint_common_support[1])
  testthat::expect_true(all(is.finite(
    result$data$bombus_total_habitat_support[-1]
  )))
  relative <- result$data[-1, grep("_relative_weight$", names(result$data))]
  testthat::expect_equal(unname(rowSums(relative)), rep(1, 29), tolerance = 1e-8)
  testthat::expect_equal(sort(unique(result$loadings$component)), c("PC1", "PC2"))
})

testthat::test_that("natural-surprise ranks are nationwide and directional", {
  sites <- data.frame(
    exact_site_id = paste0("s", 1:8),
    site_class = rep(c("pigmented", "white"), each = 4),
    n_observations = 1, n_pigmented = c(rep(1, 4), rep(0, 4)),
    n_white = c(rep(0, 4), rep(1, 4)), pigment_share = c(rep(1, 4), rep(0, 4)),
    x_km = c(0, 30, 60, 90, 0, 30, 60, 90), y_km = 0,
    longitude = 135, latitude = 35, elevation = seq(100, 800, by = 100),
    block_x = 0:7, block_y = 0,
    spatial_fold = rep(1:4, 2),
    bee_ardens = seq(0.1, 0.8, by = 0.1),
    bee_diversus = seq(0.8, 0.1, by = -0.1),
    bee_beaticola = rep(0.4, 8),
    bee_consobrinus = rep(0.2, 8),
    bee_honshuensis = rep(0.6, 8)
  )
  probability <- c(0.9, 0.7, 0.3, 0.1, 0.1, 0.3, 0.7, 0.9)
  result <- fingerprint_anomaly_ranks(
    sites, probability, rep(0, 8), rep(0, 8)
  )
  pigmented <- result[result$direction == "unexpected_pigmented", ]
  white <- result[result$direction == "unexpected_white", ]
  testthat::expect_equal(pigmented$directional_rank[which.min(probability[1:4])], 1)
  testthat::expect_equal(white$directional_rank[which.max(probability[5:8])], 1)
  testthat::expect_false("region" %in% names(result))
})

testthat::test_that("primary formulas use a community block and no site access", {
  formula <- paste(deparse(fingerprint_formula(
    "pigment_share", "environment_space_fingerprint", "env_temp", 500
  )), collapse = " ")
  testthat::expect_match(formula, "bombus_total_habitat_support")
  testthat::expect_match(formula, "bombus_composition_pc1")
  testthat::expect_match(formula, "bombus_composition_pc2")
  testthat::expect_false(grepl("z_R|z_A|road|forest|region", formula))
})
