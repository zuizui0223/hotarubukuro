source(testthat::test_path("../../R/human_raster_features.R"))
source(testthat::test_path("../../R/candidate_null_tools.R"))
source(testthat::test_path("../../R/human_landscape_features.R"))
source(testthat::test_path("../../R/local_human_context.R"))
source(testthat::test_path("../../R/did_sensitivity.R"))

testthat::test_that("human context classes use response-blind thresholds", {
  out <- v22_human_context_class(
    did_distance_km = c(2, 15, 15, 7),
    population_5km_rank = c(0.9, 0.9, 0.1, 0.5)
  )
  testthat::expect_equal(
    as.character(out),
    c(
      "did_proximate_high_population", "populated_beyond_did",
      "remote_low_population", "intermediate_context"
    )
  )
})

testthat::test_that("DID feature directions are locked before analysis", {
  definitions <- v22_feature_definitions()
  testthat::expect_equal(nrow(definitions), 5L)
  testthat::expect_true(
    all(definitions$hypothesis_direction == "greater")
  )
  testthat::expect_true(
    all(c(
      "did_proximity_rank", "populated_beyond_did_score"
    ) %in% definitions$feature)
  )
})

testthat::test_that("context composition applies fixed classes to maps", {
  observed <- c(TRUE, TRUE, FALSE, FALSE)
  simulated <- matrix(c(
    TRUE, FALSE, TRUE, FALSE,
    FALSE, TRUE, FALSE, TRUE
  ), nrow = 4, ncol = 2)
  context <- factor(
    c("urban", "rural", "urban", "other"),
    levels = c("urban", "rural", "other")
  )
  out <- v22_context_composition(observed, simulated, context)
  urban <- out$summary[
    out$summary$human_context_class == "urban", , drop = FALSE
  ]
  testthat::expect_equal(urban$observed_candidate_count, 1)
  testthat::expect_equal(urban$observed_candidate_fraction, 0.5)
  testthat::expect_equal(nrow(out$null), 2L)
})

testthat::test_that("DID provenance URL is an official MLIT endpoint", {
  testthat::expect_match(v22_did_source_url, "^https://nlftp\\.mlit\\.go\\.jp/")
  testthat::expect_match(v22_did_source_url, "A16-15_GML\\.zip$")
})
