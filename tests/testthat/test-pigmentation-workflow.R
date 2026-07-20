source("R/pigmentation_workflow.R")

testthat::test_that("sRGB conversion returns finite CIELAB values", {
  x <- srgb_to_lab(c(255, 200, 120), c(255, 80, 100), c(255, 120, 180))
  testthat::expect_equal(names(x), c("L", "a", "b"))
  testthat::expect_true(all(is.finite(as.matrix(x))))
})

testthat::test_that("legacy CFA uses a, inverted L and chroma", {
  testthat::skip_if_not_installed("lavaan")
  set.seed(1)
  latent <- rnorm(300)
  d <- data.frame(
    L = 70 - 7 * latent + rnorm(300),
    a = 18 + 5 * latent + rnorm(300),
    b = 5 + 2 * latent + rnorm(300)
  )
  fit <- fit_legacy_pigment_cfa(d)
  testthat::expect_length(fit$scores, 300)
  testthat::expect_setequal(fit$loadings$rhs, c("a", "Lm", "C"))
  testthat::expect_gt(cor(fit$scores, latent), 0.8)
})

testthat::test_that("Bombus indices preserve distribution-type distinction", {
  d <- data.frame(ardens=.2, beaticola=.1, consobrinus=.3, diversus=.4, honshuensis=.2)
  x <- bombus_indices(d)
  testthat::expect_equal(x$Bombus_availability, 1.2)
  testthat::expect_equal(x$Bombus_widespread, .6)
  testthat::expect_equal(x$Bombus_montane, .6)
})

testthat::test_that("DOY appears only in anomaly formula", {
  natural <- integrated_model_terms()
  testthat::expect_false("DOY" %in% natural)
  f <- anthropogenic_qgam_formula(c("pop_density_log", "built_fraction_1000m"), TRUE)
  txt <- paste(deparse(f), collapse = " ")
  testthat::expect_match(txt, "DOY")
  testthat::expect_match(txt, "pop_density_log")
})
