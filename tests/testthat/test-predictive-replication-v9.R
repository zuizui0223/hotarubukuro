source(testthat::test_path(
  "..", "..", "scripts", "predictive_replication_v9.R"
))

testthat::test_that("SPDE field is constrained away from the intercept", {
  definition <- paste(deparse(body(v16_make_spde)), collapse = " ")
  testthat::expect_match(
    definition, "Matrix::colSums(training_projection)", fixed = TRUE
  )
  testthat::expect_match(definition, "extraconstr", fixed = TRUE)
  testthat::expect_identical(
    v16_analysis_spec_version, "v16.5_centered_observation_year"
  )
})

testthat::test_that("posterior APredictor rows are restored to stack order", {
  sample <- list(latent = matrix(
    1:3, ncol = 1,
    dimnames = list(
      c("APredictor:00003", "APredictor:00001", "APredictor:00002"), NULL
    )
  ))
  testthat::expect_identical(
    v16_predictor_row_order(sample, c(1L, 2L, 3L)), c(2L, 3L, 1L)
  )
})

testthat::test_that("data quality checks preserve raw-to-cell binomial counts", {
  observations <- data.frame(
    x_km = c(1.2, 1.3, 2.1), y_km = c(3.2, 3.3, 4.2),
    pigmented_mixture50 = c(1, 0, 1),
    pigment_intensity_z = c(0.5, NA, 1.2)
  )
  cells <- data.frame(
    exact_site_id = c("cell-1km-1_3", "cell-1km-2_4"),
    n_observations = c(2, 1), n_pigmented = c(1, 1), n_white = c(1, 0),
    pigment_share = c(0.5, 1), x_km = c(1.5, 2.5), y_km = c(3.5, 4.5),
    spatial_fold = c(1, 2), broad50km_pc1 = 0, broad50km_pc2 = 0,
    within50km_pc1 = 0, within50km_pc2 = 0,
    bombus_total_habitat_support = 0.5, bombus_composition_pc1 = 0,
    bombus_composition_pc2 = 0, bombus_fingerprint_common_support = TRUE,
    conditional_intensity_median = c(0.5, 1.2), median_DOY = 150,
    median_year = 2020, log_population_sum_25km = 5
  )
  quality <- v16_data_quality(observations, cells)
  value <- setNames(quality$value, quality$metric)
  testthat::expect_equal(value[["n_raw_observations"]], 3)
  testthat::expect_equal(value[["sum_cell_observations"]], 3)
  testthat::expect_equal(value[["n_cell_aggregation_mismatches"]], 0)
  testthat::expect_equal(value[["n_mixed_cells"]], 1)
})

testthat::test_that("predictive tails use counts and retain mixed cells", {
  result <- list(
    model = "test", cell_id = c("a", "b", "c"),
    draws = rbind(
      c(0, 0, 1, 0),
      c(0, 1, 1, 2),
      c(1, 1, 1, 1)
    )
  )
  cells <- data.frame(
    exact_site_id = c("a", "b", "c"),
    n_pigmented = c(1, 1, 0), n_observations = c(1, 2, 1)
  )
  scores <- v16_presence_scores(result, cells)
  testthat::expect_equal(scores$unexpected_pigmented_q[1], 2 / 5)
  testthat::expect_true(is.finite(scores$unexpected_pigmented_rank[2]))
  testthat::expect_true(is.finite(scores$unexpected_white_rank[2]))
  testthat::expect_true(is.na(scores$unexpected_pigmented_rank[3]))
})

testthat::test_that("presence calibration keeps cell trials as weights", {
  set.seed(7)
  probability <- seq(0.1, 0.9, length.out = 20)
  trials <- rep(10L, length(probability))
  observed <- stats::rbinom(length(probability), trials, probability)
  draws <- vapply(seq_len(200), function(index) {
    stats::rbinom(length(probability), trials, probability)
  }, integer(length(probability)))
  cells <- data.frame(
    exact_site_id = paste0("c", seq_along(probability)),
    n_pigmented = observed, n_observations = trials,
    spatial_fold = rep(1:2, length.out = length(probability))
  )
  result <- list(
    model = "calibration-test", cell_id = cells$exact_site_id,
    draws = draws, latent_mean = rowMeans(draws / trials)
  )
  table <- v16_presence_calibration(result, cells)
  performance <- v16_model_performance(
    result, cells, "n_pigmented", "binomial", "n_observations"
  )
  fold_performance <- v16_fold_performance(
    result, cells, "n_pigmented", "binomial", "n_observations"
  )
  testthat::expect_equal(sum(table$n_cells), nrow(cells))
  testthat::expect_equal(sum(table$n_observations), sum(trials))
  testthat::expect_true(is.finite(performance$calibration_slope))
  testthat::expect_true(is.finite(performance$Bernoulli_Brier_score))
  testthat::expect_identical(fold_performance$heldout_spatial_fold, 1:2)
})

testthat::test_that("replicate candidate tails reapply the same discrete rule", {
  draws <- rbind(c(0, 1, 1, 0), c(0, 0, 1, 2))
  upper <- v16_simulation_tail_q(draws, "pigmented")
  lower <- v16_simulation_tail_q(draws, "white")
  testthat::expect_equal(upper[1, ], c(1, 0.5, 0.5, 1))
  testthat::expect_equal(lower[1, ], c(0.5, 1, 1, 0.5))
  testthat::expect_true(all(upper > 0 & upper <= 1))
})

testthat::test_that("top candidate selection is outcome-only and deterministic", {
  selected <- v16_top_indices(
    q = c(0.2, 0.01, 0.01, 0.5),
    z = c(1, 2, 3, 0),
    eligible = c(TRUE, TRUE, TRUE, FALSE),
    fraction = 0.5, ids = c("a", "b", "c", "d")
  )
  testthat::expect_identical(selected, c(3L, 2L))
  body_text <- paste(deparse(body(v16_top_indices)), collapse = " ")
  testthat::expect_false(grepl("population|DOY|intensity|road|forest", body_text))
})

testthat::test_that("fold predictor basis orthogonalizes Bombus block", {
  set.seed(11)
  train <- data.frame(e1 = rnorm(80), e2 = rnorm(80))
  train$f1 <- 2 * train$e1 - train$e2 + rnorm(80, sd = 0.1)
  test <- data.frame(e1 = rnorm(20), e2 = rnorm(20))
  test$f1 <- 2 * test$e1 - test$e2 + rnorm(20, sd = 0.1)
  basis <- v16_fold_predictors(train, test, c("e1", "e2"), "f1")
  testthat::expect_equal(ncol(basis$train), 3)
  testthat::expect_lt(abs(stats::cor(basis$train[, 1], basis$train[, 3])), 1e-8)
  testthat::expect_lt(abs(stats::cor(basis$train[, 2], basis$train[, 3])), 1e-8)
  testthat::expect_lt(basis$maximum_VIF, 1.1)
})

testthat::test_that("neighbour isolation respects same-fold restriction", {
  cells <- data.frame(
    x_km = c(0, 1, 2), y_km = c(0, 0, 0), spatial_fold = c(1, 1, 2)
  )
  neighbours <- v16_neighbour_structure(
    cells, k = 5, maximum_km = 10, same_fold_only = TRUE
  )
  isolation <- v16_local_isolation(c(1, 0, 1), neighbours)[, 1]
  testthat::expect_equal(isolation[1:2], c(1, -1))
  testthat::expect_true(is.na(isolation[3]))
})

testthat::test_that("null comparison reports Monte Carlo uncertainty", {
  result <- v16_null_comparison(5, 1:10, "greater")
  testthat::expect_equal(unname(result[["empirical_p"]]), 7 / 11)
  testthat::expect_true(result[["monte_carlo_se"]] > 0)
})

testthat::test_that("candidate null reapplies tiers without held-out facet leakage", {
  set.seed(22)
  n <- 8L
  B <- 100L
  cells <- data.frame(
    exact_site_id = paste0("c", seq_len(n)),
    x_km = seq(0, 35, length.out = n), y_km = 0,
    spatial_fold = rep(1:2, length.out = n),
    n_observations = rep(2L, n),
    n_pigmented = c(2, 1, 0, 2, 0, 1, 0, 2),
    n_white = c(0, 1, 2, 0, 2, 1, 2, 0),
    conditional_intensity_median = c(1, 0.5, NA, 1.2, NA, 0.4, NA, 0.9),
    median_DOY = seq(140, 154, length.out = n),
    log_population_sum_25km = seq(2, 9, length.out = n),
    n_independent_sites = 1:8
  )
  presence <- list(
    model = "test", cell_id = cells$exact_site_id,
    draws = matrix(rbinom(n * B, 2, 0.5), n, B)
  )
  intensity <- list(
    cell_id = cells$exact_site_id,
    draws = matrix(rnorm(n * B), n, B), latent_mean = rep(0, n)
  )
  phenology <- list(
    cell_id = cells$exact_site_id,
    draws = matrix(rnorm(n * B, 147, 5), n, B),
    latent_mean = rep(147, n)
  )
  result <- v16_candidate_null(presence, cells, intensity, phenology)
  intensity_shifted_draws <- intensity
  intensity_shifted_draws$draws <- intensity$draws + 1000
  phenology_shifted_draws <- phenology
  phenology_shifted_draws$draws <- phenology$draws + 1000
  shifted <- v16_candidate_null(
    presence, cells, intensity_shifted_draws, phenology_shifted_draws
  )
  testthat::expect_true(nrow(result$summary) > 20)
  testthat::expect_true(all(result$summary$n_null_draws > 0))
  testthat::expect_true(all(result$scores$model == "test"))
  testthat::expect_equal(length(result$observed_early_surprise), n)
  testthat::expect_true(any(grepl("_tier_gradient$", result$summary$metric)))
  facet_rows <- result$summary$metric %in% c(
    "mean_early_phenology_surprise", "mean_intensity_surprise"
  )
  testthat::expect_equal(
    result$summary[facet_rows, ], shifted$summary[facet_rows, ]
  )
})
