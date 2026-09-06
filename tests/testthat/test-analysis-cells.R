source(testthat::test_path("..", "..", "R", "analysis_cells.R"))

testthat::test_that("rank helpers are deterministic", {
  testthat::expect_equal(ac_rank01(c(10, 20, 30)), c(0, 0.5, 1))
  testthat::expect_equal(ac_positive_rank(c(10, 20, 30)), c(1/3, 2/3, 1))
})

testthat::test_that("one-km cells preserve colour counts and independent-site support", {
  d <- data.frame(
    exact_site_id = c("a", "a", "b"),
    longitude = c(140, 140, 140.001), latitude = c(38, 38, 38.001),
    x_km = c(10.1, 10.1, 10.6), y_km = c(20.1, 20.1, 20.7),
    pigmented_mixture50 = c(1, 0, 1), pigmented_high_confidence = c(1, 0, 1),
    colour_a = c(1, -1, 2), DOY = c(150, 151, 152),
    bee_ardens = c(.1, .2, .3), bee_diversus = c(.2, .3, .4),
    bee_beaticola = c(.3, .4, .5), bee_consobrinus = c(.4, .5, .6),
    bee_honshuensis = c(.5, .6, .7), stringsAsFactors = FALSE
  )
  out <- ac_population_table(d, 1)
  testthat::expect_equal(nrow(out), 1)
  testthat::expect_equal(out$n_observations, 3)
  testthat::expect_equal(out$n_pigmented, 2)
  testthat::expect_equal(out$n_white, 1)
  testthat::expect_equal(out$n_exact_sites, 2)
})
