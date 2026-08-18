source(testthat::test_path("../../R/continuous_colour_isolation.R"))

testthat::test_that("nearest same-colour distance is threshold-free", {
  cells <- data.frame(x_km = c(0, 1, 4, 10), y_km = 0)
  distance <- v23_distance_matrix(cells)
  state <- c(TRUE, TRUE, FALSE, FALSE)
  metrics <- v23_isolation_metrics(state, distance)
  testthat::expect_equal(
    as.numeric(metrics$same_colour_nn_km[, 1L]),
    c(1, 1, 6, 6)
  )
  testthat::expect_equal(metrics$any_colour_nn_km, c(1, 1, 3, 6))
})

testthat::test_that("relative isolation separates colour spacing from cell spacing", {
  cells <- data.frame(x_km = c(0, 1, 4, 10), y_km = 0)
  distance <- v23_distance_matrix(cells)
  metrics <- v23_isolation_metrics(c(TRUE, TRUE, FALSE, FALSE), distance)
  testthat::expect_equal(
    as.numeric(metrics$relative_isolation_nn[, 1L]),
    log(c(1, 1, 2, 1))
  )
})

testthat::test_that("state shuffling preserves fold-specific colour counts", {
  state <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)
  fold <- c(1, 1, 1, 2, 2, 2)
  set.seed(1)
  shuffled <- v23_shuffle_state_within_fold(state, fold)
  testthat::expect_equal(
    tapply(shuffled, fold, sum),
    tapply(state, fold, sum)
  )
})

testthat::test_that("colour rho difference uses the same continuous measure", {
  isolation <- c(1, 2, 3, 4, 5, 6)
  state <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  feature <- c(1, 2, 3, 3, 2, 1)
  contrast <- v23_colour_rho_difference(isolation, state, feature)
  testthat::expect_equal(contrast[["pigmented_rho"]], 1)
  testthat::expect_equal(contrast[["white_rho"]], -1)
  testthat::expect_equal(contrast[["rho_difference"]], 2)
})

testthat::test_that("nearest-same matrix handles several maps at once", {
  cells <- data.frame(x_km = c(0, 1, 4, 10), y_km = 0)
  distance <- v23_distance_matrix(cells)
  states <- cbind(
    c(TRUE, TRUE, FALSE, FALSE),
    c(TRUE, FALSE, TRUE, FALSE)
  )
  result <- v23_nearest_same_matrix(states, distance)
  testthat::expect_equal(result[, 1L], c(1, 1, 6, 6))
  testthat::expect_equal(result[, 2L], c(4, 9, 4, 9))
})
