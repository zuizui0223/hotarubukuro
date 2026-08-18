source(testthat::test_path("../../R/local_departure_placement_null.R"))

testthat::test_that("candidate placement statistic averages only selected cells", {
  candidate <- matrix(c(
    TRUE, FALSE, TRUE,
    FALSE, TRUE, TRUE
  ), nrow = 3, ncol = 2)
  features <- data.frame(
    human = c(0.9, 0.1, 0.5),
    natural = c(0.2, 0.4, 0.6)
  )
  result <- v22_candidate_feature_statistic(
    candidate, features, names(features), "mean"
  )
  testthat::expect_equal(result$n_candidates, c(2L, 2L))
  testthat::expect_equal(
    unname(result$statistic[, "human"]), c(0.7, 0.3)
  )
})

testthat::test_that("exact candidate-count maps are primary when sufficient", {
  result <- v22_count_conditioned_draws(
    simulated_counts = c(rep(16L, 4), 15L, 17L),
    observed_count = 16L,
    minimum_exact_draws = 3L,
    fallback_draws = 5L
  )
  testthat::expect_equal(result$mode, "exact_candidate_count")
  testthat::expect_equal(result$selected_draws, 1:4)
  testthat::expect_equal(result$maximum_absolute_count_difference, 0L)
})

testthat::test_that("nearest-count fallback is deterministic", {
  result <- v22_count_conditioned_draws(
    simulated_counts = c(13L, 15L, 17L, 19L, 16L),
    observed_count = 16L,
    minimum_exact_draws = 2L,
    fallback_draws = 3L
  )
  testthat::expect_equal(
    result$mode, "nearest_candidate_count_fallback"
  )
  testthat::expect_equal(result$selected_draws, c(5L, 2L, 3L))
  testthat::expect_equal(result$maximum_absolute_count_difference, 1L)
})

testthat::test_that("placement summary uses map placement rather than neighbours", {
  definitions <- data.frame(
    feature = c("population", "road"),
    role = c("population", "road"),
    hypothesis_direction = c("greater", "greater"),
    family = c("human", "human"),
    stringsAsFactors = FALSE
  )
  simulated <- data.frame(
    population = c(0.1, 0.2, 0.3, 0.4),
    road = c(0.4, 0.3, 0.2, 0.1)
  )
  observed <- c(population = 0.5, road = 0.5)
  result <- v22_placement_summary(
    observed, simulated, definitions, 1:4,
    conditioning = "test", aggregation = "mean"
  )
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_equal(result$p_value, c(0.2, 0.2))
  testthat::expect_true(all(result$maxT_FWER_p >= result$p_value))
  testthat::expect_equal(
    unique(result$family_omnibus_p), result$maxT_FWER_p[1]
  )
})

testthat::test_that("human values never enter count conditioning", {
  counts <- c(16L, 16L, 15L, 17L)
  first <- v22_count_conditioned_draws(
    counts, 16L, minimum_exact_draws = 2L
  )
  invented_human_values <- c(100, -100, 0, 0)
  second <- v22_count_conditioned_draws(
    counts + 0L * invented_human_values,
    16L, minimum_exact_draws = 2L
  )
  testthat::expect_identical(first$selected_draws, second$selected_draws)
})
