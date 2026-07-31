source(file.path("R", "local_state_asymmetry.R"))

testthat::test_that("directional profiles detect both local event directions", {
  graph <- list(
    neighbours = list(
      c(2L, 3L),       # pigmented focal among white neighbours
      c(1L, 4L),       # white focal among pigmented neighbours
      c(1L, 5L),
      c(2L, 6L),
      c(3L, 6L),
      c(4L, 5L)
    ),
    supported = rep(TRUE, 6L)
  )
  counts <- c(1, 0, 0, 1, 1, 1)
  trials <- rep(1, 6L)
  profile <- v23_directional_profiles(counts, trials, graph)

  testthat::expect_true(profile$pigmented_in_white[1, 1])
  testthat::expect_true(profile$white_in_pigmented[2, 1])
  testthat::expect_equal(
    profile$summary$pigmented_in_white_count, 1
  )
  testthat::expect_equal(
    profile$summary$white_in_pigmented_count, 1
  )
  testthat::expect_equal(profile$summary$log_rate_ratio, 0)
})

testthat::test_that("opportunity normalization distinguishes count and rate asymmetry", {
  graph <- list(
    neighbours = list(
      c(2L, 3L), c(1L, 4L), c(1L, 5L), c(2L, 6L),
      c(3L, 7L), c(4L, 8L), c(5L, 8L), c(6L, 7L)
    ),
    supported = rep(TRUE, 8L)
  )
  counts <- c(1, 0, 0, 1, 1, 1, 1, 1)
  trials <- rep(1, 8L)
  profile <- v23_directional_profiles(counts, trials, graph)

  testthat::expect_equal(profile$summary$pigmented_in_white_count, 1)
  testthat::expect_equal(profile$summary$white_in_pigmented_count, 1)
  testthat::expect_true(
    profile$summary$pigmented_in_white_rate >
      profile$summary$white_in_pigmented_rate
  )
  testthat::expect_true(profile$summary$log_rate_ratio > 0)
})

testthat::test_that("state-rule sensitivities exclude mixed cells as intended", {
  counts <- matrix(c(0, 1, 2), ncol = 1L)
  trials <- c(2, 2, 2)

  legacy <- v23_classify_states(counts, trials)
  pure <- v23_classify_states(
    counts, trials, white_max_share = 0, pigmented_min_share = 1
  )
  threshold <- v23_classify_states(
    counts, trials, white_max_share = 0.10, pigmented_min_share = 0.90
  )

  testthat::expect_equal(as.vector(legacy$white), c(TRUE, FALSE, FALSE))
  testthat::expect_equal(as.vector(legacy$pigmented), c(FALSE, TRUE, TRUE))
  testthat::expect_equal(as.vector(pure$classified), c(TRUE, FALSE, TRUE))
  testthat::expect_equal(
    as.vector(threshold$classified), c(TRUE, FALSE, TRUE)
  )
})

testthat::test_that("asymmetry summary compares observed and replicated maps", {
  graph <- list(
    neighbours = list(c(2L, 3L), c(1L, 4L), c(1L, 4L), c(2L, 3L)),
    supported = rep(TRUE, 4L)
  )
  trials <- rep(1, 4L)
  observed <- v23_directional_profiles(c(1, 0, 0, 1), trials, graph)
  simulated <- v23_directional_profiles(
    cbind(
      c(1, 0, 0, 1),
      c(0, 1, 1, 0),
      c(1, 1, 0, 0)
    ),
    trials, graph
  )
  summary <- v23_asymmetry_summary(
    observed, simulated, "legacy_any_pigmented"
  )

  testthat::expect_equal(nrow(summary), 7L)
  testthat::expect_true(all(summary$n_natural_maps == 3L))
  testthat::expect_true("log_rate_ratio" %in% summary$metric)
  testthat::expect_true(all(summary$analysis_status == "post_hoc_diagnostic"))
})

testthat::test_that("invalid overlapping state definitions are rejected", {
  testthat::expect_error(
    v23_classify_states(
      counts = c(0, 1), trials = c(1, 1),
      white_max_share = 0.6, pigmented_min_share = 0.4
    ),
    "overlap"
  )
})
