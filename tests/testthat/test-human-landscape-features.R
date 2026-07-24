source(testthat::test_path("../../R/human_landscape_features.R"))

testthat::test_that("rank transformation preserves ties and bounds", {
  value <- v19_rank01(c(1, 2, 2, 4, NA))
  testthat::expect_equal(value[2], value[3])
  testthat::expect_true(all(value[is.finite(value)] >= 0))
  testthat::expect_true(all(value[is.finite(value)] <= 1))
})

testthat::test_that("landscape composites follow their declared directions", {
  cells <- data.frame(
    exact_site_id = paste0("c", 1:4),
    human_population = c(1, 10, 100, 1000),
    log_population_sum_25km = 1:4,
    elevation_within_25km = 1:4,
    spatial_fold = 1:4
  )
  landscape <- data.frame(
    exact_site_id = cells$exact_site_id,
    forest_human_edge_1km = 1:4,
    forest_fraction_1km = c(1, 0.8, 0.4, 0.1),
    human_managed_fraction_1km = c(0, 0.1, 0.5, 0.9),
    major_road_distance_km = c(10, 5, 2, 1),
    primary_mesh_boundary = FALSE
  )
  out <- v19_landscape_features(cells, landscape)
  testthat::expect_gt(out$human_reach_score[4], out$human_reach_score[1])
  testthat::expect_gt(out$urban_mountain_score[4],
                      out$urban_mountain_score[1])
  testthat::expect_gt(out$remote_mountain_score[1],
                      out$remote_mountain_score[4])
})

testthat::test_that("pair contrasts are case minus control", {
  features <- data.frame(a = c(0.8, 0.3, 0.2, 0.1))
  pairs <- data.frame(case_index = c(1, 2), control_index = c(3, 4))
  contrast <- v19_pair_contrasts(pairs, features, "a")
  testthat::expect_equal(unname(contrast), 0.4)
})

testthat::test_that("AUC handles ordering and ties", {
  outcome <- factor(
    c("case", "case", "control", "control"),
    levels = c("control", "case")
  )
  testthat::expect_equal(v19_auc(outcome, c(0.9, 0.8, 0.2, 0.1)), 1)
  testthat::expect_equal(v19_auc(outcome, rep(0.5, 4)), 0.5)
})

testthat::test_that("regularized distance remains finite under collinearity", {
  matrix <- cbind(a = 1:20, b = 2 * (1:20))
  result <- v19_regularized_distance(matrix)
  testthat::expect_true(all(is.finite(result$distance)))
  testthat::expect_gt(result$ridge, 0)
})

testthat::test_that("spatial RF keeps paired folds together", {
  testthat::skip_if_not_installed("ranger")
  set.seed(9)
  pairs <- 30
  data <- data.frame(
    outcome = factor(
      c(rep("case", pairs), rep("control", pairs)),
      levels = c("control", "case")
    ),
    pair_id = rep(seq_len(pairs), 2),
    spatial_fold = rep(rep(1:5, length.out = pairs), 2),
    x = c(rnorm(pairs, 1), rnorm(pairs, -1)),
    y = rnorm(2 * pairs)
  )
  result <- v19_spatial_rf(
    data, c("x", "y"), seed = 1L, num_trees = 50L
  )
  testthat::expect_gt(result$metrics[["spatial_cv_auc"]], 0.8)
  testthat::expect_equal(nrow(result$predictions), 2 * pairs)
})
