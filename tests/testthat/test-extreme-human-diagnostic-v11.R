source(testthat::test_path("../../scripts/extreme_human_diagnostic_v11.R"))

testthat::test_that("predictive tails use the requested ecological direction", {
  draws <- rbind(
    c(0, 1, 2, 3),
    c(0, 1, 2, 3)
  )
  upper <- v18_predictive_tail_q(c(3, 0), draws, "upper")
  lower <- v18_predictive_tail_q(c(3, 0), draws, "lower")
  testthat::expect_lt(upper[1], upper[2])
  testthat::expect_gt(lower[1], lower[2])
})

testthat::test_that("tail depth preserves predictive matrix dimensions", {
  q <- matrix(seq(0.1, 0.9, length.out = 12), 3, 4)
  depth <- v18_tail_depth(q)
  testthat::expect_equal(dim(depth), dim(q))
  testthat::expect_equal(depth[1, 1], -log10(q[1, 1]))
})

testthat::test_that("matching options depend on natural context, not extremes", {
  cells <- data.frame(
    exact_site_id = paste0("c", 1:6),
    x_km = 0:5, y_km = 0,
    spatial_fold = c(1, 1, 1, 2, 2, 2),
    n_observations = 1,
    broad50km_pc1 = 0:5 / 5, broad50km_pc2 = 0,
    within50km_pc1 = 0, within50km_pc2 = 0,
    stringsAsFactors = FALSE
  )
  options <- v18_match_options(
    cells, rep(0.5, 6), maximum_distance_km = 10
  )
  first <- options$options
  cells$invented_extreme <- rev(seq_len(nrow(cells)))
  options_changed <- v18_match_options(
    cells, rep(0.5, 6), maximum_distance_km = 10
  )
  testthat::expect_equal(first, options_changed$options)
})

testthat::test_that("greedy matching does not reuse controls or cross folds", {
  cells <- data.frame(
    exact_site_id = paste0("c", 1:6),
    x_km = 0:5, y_km = 0,
    spatial_fold = c(1, 1, 1, 2, 2, 2),
    n_observations = 1,
    broad50km_pc1 = 0, broad50km_pc2 = 0,
    within50km_pc1 = 0, within50km_pc2 = 0,
    stringsAsFactors = FALSE
  )
  options <- v18_match_options(
    cells, rep(0.5, 6), maximum_distance_km = 10
  )
  q <- c(0.01, 0.02, 0.8, 0.01, 0.02, 0.8)
  z <- rep(1, 6)
  pairs <- v18_match_cases(
    c(1, 2, 4, 5), c(3, 6), options, q, z, cells$exact_site_id
  )
  testthat::expect_false(anyDuplicated(pairs$control_index) > 0)
  testthat::expect_true(all(
    cells$spatial_fold[pairs$case_index] ==
      cells$spatial_fold[pairs$control_index]
  ))
})

testthat::test_that("top selection is deterministic under ties", {
  q <- c(0.1, 0.1, 0.1, 0.5)
  z <- c(1, 2, 2, 0)
  ids <- c("b", "c", "a", "d")
  selected <- v18_top_indices(q, z, rep(TRUE, 4), 0.5, ids)
  testthat::expect_equal(selected, c(3, 2))
})
