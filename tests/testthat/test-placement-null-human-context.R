source(testthat::test_path("../../R/candidate_null_tools.R"))
source(testthat::test_path("../../R/local_pigmented_isolates.R"))
source(testthat::test_path("../../R/local_human_context.R"))
source(testthat::test_path("../../R/placement_null_human_context.R"))

testthat::test_that("reversed detector finds white focal among pigmented neighbours", {
  graph <- list(
    neighbours = list(c(2L, 3L), c(1L), c(1L)),
    weights = list(c(0.5, 0.5), 1, 1),
    support = data.frame(supported = c(TRUE, FALSE, FALSE))
  )
  forward <- v22_directional_profile(
    c(0, 1, 1), graph, "pigmented_among_white"
  )
  reverse <- v22_directional_profile(
    c(0, 1, 1), graph, "white_among_pigmented"
  )
  testthat::expect_false(forward$candidate[1, 1])
  testthat::expect_true(reverse$candidate[1, 1])
  testthat::expect_equal(reverse$neighbour_pigment_share[1, 1], 1)
})

testthat::test_that("placement statistic retains selected cell identity", {
  observed <- c(TRUE, FALSE, FALSE)
  simulated <- matrix(c(
    FALSE, TRUE, FALSE,
    FALSE, FALSE, TRUE
  ), nrow = 3, ncol = 2)
  result <- v22_placement_null(
    observed, simulated, exposure = c(0.9, 0.1, 0.2),
    radius_km = 5,
    direction = "pigmented_among_white",
    feature = "population_5km_rank"
  )
  testthat::expect_equal(result$summary$observed_value, 0.9)
  testthat::expect_equal(result$summary$null_mean, 0.15)
  testthat::expect_equal(result$null$mean_exposure_rank, c(0.1, 0.2))
})

testthat::test_that("zero-candidate draws are predeclared degeneracies", {
  observed <- c(TRUE, FALSE)
  simulated <- matrix(c(
    FALSE, FALSE,
    TRUE, FALSE
  ), nrow = 2, ncol = 2)
  result <- v22_placement_null(
    observed, simulated, exposure = c(0.8, 0.2),
    radius_km = 5,
    direction = "pigmented_among_white"
  )
  testthat::expect_equal(result$summary$n_zero_candidate_draws, 1L)
  testthat::expect_equal(result$summary$n_null_draws_finite, 1L)
  testthat::expect_true(is.na(result$null$mean_exposure_rank[1]))
})

testthat::test_that("scale maxT treats radii as one family", {
  summaries <- do.call(rbind, lapply(c(2, 5), function(radius) {
    result <- v22_placement_null(
      observed_candidate = c(TRUE, FALSE, FALSE),
      simulated_candidate = matrix(c(
        FALSE, TRUE, FALSE,
        FALSE, FALSE, TRUE,
        TRUE, FALSE, FALSE
      ), nrow = 3, ncol = 3),
      exposure = if (radius == 2) c(0.9, 0.1, 0.2) else c(0.8, 0.2, 0.3),
      radius_km = radius,
      direction = "pigmented_among_white"
    )
    result$summary
  }))
  nulls <- do.call(rbind, lapply(c(2, 5), function(radius) {
    result <- v22_placement_null(
      observed_candidate = c(TRUE, FALSE, FALSE),
      simulated_candidate = matrix(c(
        FALSE, TRUE, FALSE,
        FALSE, FALSE, TRUE,
        TRUE, FALSE, FALSE
      ), nrow = 3, ncol = 3),
      exposure = if (radius == 2) c(0.9, 0.1, 0.2) else c(0.8, 0.2, 0.3),
      radius_km = radius,
      direction = "pigmented_among_white"
    )
    result$null
  }))
  result <- v22_add_scale_maxT(summaries, nulls)
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_true(all(is.finite(result$maxT_FWER_p)))
  testthat::expect_true(all(result$maxT_FWER_p >= result$empirical_p))
})

testthat::test_that("scale profile reports monotone decay without choosing a radius", {
  summary <- data.frame(
    direction = "pigmented_among_white",
    radius_km = c(2, 5, 10, 25),
    observed_minus_null_mean = c(0.12, 0.09, 0.04, 0.01)
  )
  profile <- v22_scale_profile(summary)
  testthat::expect_equal(profile$profile_shape, "monotone_decay")
  testthat::expect_true(profile$shortest_radius_has_largest_effect)
  testthat::expect_equal(profile$effect_2km_minus_25km, 0.11)
})
