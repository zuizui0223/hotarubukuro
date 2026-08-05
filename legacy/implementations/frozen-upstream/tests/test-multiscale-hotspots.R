source(testthat::test_path("..", "..", "R", "environment_spatial.R"))
source(testthat::test_path("..", "..", "R", "local_transition_pairs.R"))
source(testthat::test_path("..", "..", "R", "hotspot_candidates.R"))
source(testthat::test_path("..", "..", "R", "bombus_community_fingerprint.R"))
source(testthat::test_path("..", "..", "R", "multiscale_hotspots.R"))

testthat::test_that("cell phenotypes use exact sites and raw flower traits", {
  observations <- data.frame(
    exact_site_id = c("a", "a", "b", "c", "d"),
    x_km = c(0.1, 0.1, 0.2, 1.1, 1.2), y_km = 0.1,
    pigmented_mixture50 = c(0, 0, 1, 1, 1),
    pigment_intensity_z = c(NA, NA, 0.2, 0.1, 0.9),
    DOY = c(150, 151, 152, 153, 154), year = 2024
  )
  cells <- data.frame(
    exact_site_id = c("cell-1km-0_0", "cell-1km-1_0"),
    stringsAsFactors = FALSE
  )
  result <- multiscale_cell_phenotypes(observations, cells)$cells
  first <- result[result$exact_site_id == "cell-1km-0_0", ]
  second <- result[result$exact_site_id == "cell-1km-1_0", ]
  testthat::expect_equal(first$n_independent_sites, 2)
  testthat::expect_equal(first$mixed_hotspot_observed, 1)
  testthat::expect_equal(second$n_pigmented_sites, 2)
  testthat::expect_equal(second$conditional_intensity_pairwise_median, 0.8)
})

testthat::test_that("hotspot formulas are one-stage and keep scale explicit", {
  formula <- paste(deparse(multiscale_hotspot_formula(
    "mixed_hotspot_observed", "scale_environment_fingerprint", 50, 300,
    effort = "log_independent_sites"
  )), collapse = " ")
  testthat::expect_match(formula, "broad50km_pc1")
  testthat::expect_match(formula, "within50km_pc1")
  testthat::expect_match(formula, "bombus_total_habitat_support")
  testthat::expect_false(grepl(
    "residual|region|z_R|z_A|(^|[^A-Za-z])road([^A-Za-z]|$)|forest",
    formula, perl = TRUE
  ))
})

testthat::test_that("fingerprint orthogonalization is fold-trained and response-blind", {
  set.seed(12)
  train <- data.frame(
    broad50km_pc1 = rnorm(100), broad50km_pc2 = rnorm(100),
    within50km_pc1 = rnorm(100), within50km_pc2 = rnorm(100)
  )
  train$bombus_total_habitat_support <- train$broad50km_pc1 + rnorm(100, sd = 0.2)
  train$bombus_composition_pc1 <- -train$broad50km_pc1 +
    train$bombus_total_habitat_support + rnorm(100, sd = 0.2)
  train$bombus_composition_pc2 <- train$within50km_pc1 +
    train$bombus_composition_pc1 + rnorm(100, sd = 0.2)
  result <- multiscale_orthogonalize_fingerprint(train, train[1:10, ], 50)
  columns <- c(
    multiscale_environment_terms(50),
    multiscale_orthogonal_fingerprint_terms()
  )
  correlation <- cor(result$train[columns])
  cross_block <- correlation[
    multiscale_environment_terms(50),
    multiscale_orthogonal_fingerprint_terms(), drop = FALSE
  ]
  testthat::expect_lt(max(abs(cross_block)), 1e-10)
})

testthat::test_that("natural surprise ranks are nationwide and facet-blind", {
  cells <- data.frame(
    site_class = c("pigmented", "pigmented", "white", "white"),
    longitude = c(130, 140, 130, 140)
  )
  result <- multiscale_natural_ranks(cells, c(0.9, 0.1, 0.1, 0.9))
  testthat::expect_equal(result$natural_surprise_rank[c(2, 4)], c(1, 1))
  testthat::expect_false("region" %in% names(result))
  testthat::expect_false(any(grepl("population|DOY|intensity", c(
    "natural_presence_probability_v15", "natural_surprise",
    "natural_surprise_rank"
  ))))
})

testthat::test_that("candidate residuals are out-of-fold diagnostics only", {
  ranked <- data.frame(
    exact_site_id = c("a", "b"), site_class = c("pigmented", "white"),
    natural_surprise_direction = c("unexpected_pigmented", "unexpected_white"),
    conditional_intensity_median = c(1.2, NA), median_DOY = c(160, 170)
  )
  predictions <- data.frame(
    outcome = "intensity_median", radius_km = 50,
    model = "scale_environment_spatial_fingerprint",
    exact_site_id = "a", prediction = 0.7
  )
  result <- multiscale_candidate_diagnostics(
    ranked, predictions, phenology_prediction = c(165, 168),
    fingerprint_probability = c(0.2, NA)
  )
  testthat::expect_equal(result$natural_intensity_surprise_v15[1], 0.5)
  testthat::expect_equal(result$early_phenology_surprise_v15, c(5, -2))
  testthat::expect_true(is.na(result$fingerprint_adjusted_rank_v15[2]))
  result$natural_surprise_rank <- c(0.9, 0.9)
  result$log_population_sum_25km <- c(10, 11)
  result$n_independent_sites <- c(2, 1)
  result$n_years <- c(1, 1)
  evidence <- multiscale_horticulture_evidence_matrix(result)
  testthat::expect_equal(nrow(evidence), 1)
  testthat::expect_true(evidence$distribution_top20)
  testthat::expect_equal(evidence$n_available_positive_facets, 3)
})
