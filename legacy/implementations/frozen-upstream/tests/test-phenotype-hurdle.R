source(testthat::test_path("..", "..", "R", "environment_spatial.R"))
source(testthat::test_path("..", "..", "R", "natural_biotic_covariates.R"))
source(testthat::test_path("..", "..", "R", "phenotype_hurdle.R"))

testthat::test_that("pigmentation measurement separates presence from intensity", {
  set.seed(42)
  white <- rnorm(250, -2, 1.5)
  coloured <- rnorm(250, 22, 5)
  a <- c(white, coloured)
  L <- c(rnorm(250, 87, 3), rnorm(250, 65, 6))
  C <- c(abs(rnorm(250, 4, 1.5)), abs(rnorm(250, 32, 7)))
  result <- fit_pigmentation_measurement(a, L, C)

  testthat::expect_gt(result$summary$decision_boundary_a, 0)
  testthat::expect_lt(result$summary$decision_boundary_a, 15)
  testthat::expect_gt(result$summary$largest_component_mean_gap, 10)
  testthat::expect_gt(result$summary$n_white, 220)
  testthat::expect_gt(result$summary$n_pigmented, 220)
  testthat::expect_true(all(
    is.na(result$observations$pigment_intensity_z[
      result$observations$pigmented_mixture50 == 0L
    ])
  ))
  testthat::expect_true(all(
    result$observations$pigment_excess_a[
      result$observations$pigmented_mixture50 == 0L
    ] == 0
  ))
  testthat::expect_true(all(is.na(
    result$observations$pigment_intensity_high_confidence_z[
      result$observations$pigmented_high_confidence != 1L |
        is.na(result$observations$pigmented_high_confidence)
    ]
  )))
  testthat::expect_true(all(is.na(
    result$observations$pigment_intensity_joint_lab_z[
      result$observations$pigmented_joint_lab50 != 1L |
        is.na(result$observations$pigmented_joint_lab50)
    ]
  )))
})

testthat::test_that("model warnings are captured with their fitting context", {
  captured <- capture_warnings_v4({
    warning("synthetic model warning")
    17
  })
  testthat::expect_identical(captured$value, 17)
  testthat::expect_identical(
    collapse_warnings_v4(captured$warnings), "synthetic model warning"
  )
})

testthat::test_that("residual percentiles keep missing conditional intensities out", {
  x <- c(-1, 0, 2, NA_real_)
  ranked <- rank01_complete(x)
  testthat::expect_equal(ranked[1:3], c(0, 0.5, 1))
  testthat::expect_true(is.na(ranked[4]))
})

testthat::test_that("classification is response blind to ecological columns", {
  set.seed(7)
  a <- c(rnorm(100, -1, 1), rnorm(100, 18, 4))
  L <- c(rnorm(100, 88, 2), rnorm(100, 68, 5))
  C <- c(abs(rnorm(100, 3, 1)), abs(rnorm(100, 27, 5)))
  ecology_a <- data.frame(Bombus_W = rnorm(200), z_H = rnorm(200))
  ecology_b <- ecology_a[200:1, ]

  first <- fit_pigmentation_measurement(a, L, C)$observations
  second <- fit_pigmentation_measurement(a, L, C)$observations
  testthat::expect_identical(
    first$pigmented_mixture50, second$pigmented_mixture50
  )
  testthat::expect_false(identical(ecology_a, ecology_b))
})

testthat::test_that("zero clipping is retained only as a sensitivity rule", {
  set.seed(11)
  a <- c(rnorm(150, -1, 2), rnorm(150, 20, 4))
  result <- fit_pigmentation_measurement(a)$observations
  testthat::expect_identical(result$pigmented_zero_rule, as.integer(a > 0))
  testthat::expect_true(any(
    result$pigmented_zero_rule == 1L & result$pigmented_mixture50 == 0L
  ))
})

testthat::test_that("binary prediction metrics have the correct improvement direction", {
  y <- c(0, 0, 1, 1)
  weak <- rep(0.5, 4)
  strong <- c(0.1, 0.2, 0.8, 0.9)
  weak_metrics <- hurdle_prediction_metrics(y, weak, "binomial")
  strong_metrics <- hurdle_prediction_metrics(y, strong, "binomial")
  testthat::expect_lt(strong_metrics$log_loss, weak_metrics$log_loss)
  testthat::expect_lt(strong_metrics$brier, weak_metrics$brier)
  testthat::expect_gt(strong_metrics$auc, weak_metrics$auc)
})

testthat::test_that("INLA helper exposes binomial without changing Gaussian default", {
  testthat::expect_identical(formals(fit_one_inla_spde)$family, "gaussian")
  body_text <- paste(deparse(body(fit_one_inla_spde)), collapse = " ")
  testthat::expect_match(body_text, "family = family", fixed = TRUE)
})
