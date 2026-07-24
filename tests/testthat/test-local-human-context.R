source(testthat::test_path("../../R/human_raster_features.R"))
source(testthat::test_path("../../R/candidate_null_tools.R"))
source(testthat::test_path("../../R/human_landscape_features.R"))
source(testthat::test_path("../../R/local_human_context.R"))

testthat::test_that("MLIT classes are aggregated separately at 1-km grain", {
  raw <- data.frame(
    L03b_001 = c(
      "5339450011", "5339450012", "5339450013", "5339450111"
    ),
    L03b_002 = c("0700", "0100", "0901", "0500"),
    stringsAsFactors = FALSE
  )
  out <- v21_aggregate_landuse_classes_dbf(raw)
  testthat::expect_equal(nrow(out), 2L)
  first <- out[out$mesh_1km == "53394500", ]
  testthat::expect_equal(first$represented_fraction, 0.03)
  testthat::expect_equal(first$built_up_fraction, 0.01)
  testthat::expect_equal(first$paddy_fraction, 0.01)
  testthat::expect_equal(first$road_land_fraction, 0.01)
})

testthat::test_that("duplicated 100-m cells are rejected", {
  raw <- data.frame(
    L03b_001 = c("5339450011", "5339450011"),
    L03b_002 = c("0700", "0100"),
    stringsAsFactors = FALSE
  )
  testthat::expect_error(
    v21_aggregate_landuse_classes_dbf(raw), "duplicated"
  )
})

testthat::test_that("local contrast compares focal to its white neighbours", {
  graph <- list(neighbours = list(c(2L, 3L), 1L, 1L))
  features <- data.frame(
    human = c(0.9, 0.2, 0.4),
    natural = c(0.5, 0.6, 0.4)
  )
  result <- v21_local_contrasts(
    present = c(TRUE, FALSE, FALSE),
    candidate = c(TRUE, FALSE, FALSE),
    graph = graph,
    features = features,
    feature_names = names(features)
  )
  testthat::expect_equal(result$n_usable, 1L)
  testthat::expect_equal(unname(result$contrast[1, "human"]), 0.6)
  testthat::expect_equal(unname(result$contrast[1, "natural"]), 0)
})

testthat::test_that("local contrast is not altered by pigmented neighbours", {
  graph <- list(neighbours = list(c(2L, 3L), 1L, 1L))
  features <- data.frame(human = c(0.9, 0.8, 0.2))
  result <- v21_local_contrasts(
    present = c(TRUE, TRUE, FALSE),
    candidate = c(TRUE, FALSE, FALSE),
    graph = graph,
    features = features,
    feature_names = "human"
  )
  testthat::expect_equal(unname(result$contrast[1, "human"]), 0.7)
})

testthat::test_that("feature definitions preserve planned directions", {
  definitions <- v21_feature_definitions()
  testthat::expect_equal(nrow(definitions), 14L)
  testthat::expect_true(
    all(definitions$hypothesis_direction[1:11] == "greater")
  )
  testthat::expect_equal(
    definitions$hypothesis_direction[14], "less"
  )
})

testthat::test_that("static human spike is outcome blind", {
  graph <- list(neighbours = list(c(2L, 3L), c(1L, 3L), c(1L, 2L)))
  spike <- v21_static_local_spike(graph, c(0.9, 0.2, 0.4))
  testthat::expect_equal(unname(spike[1]), 0.6)
  testthat::expect_equal(unname(spike[2]), -0.45)
})

testthat::test_that("convergence applies the same fixed spike to null maps", {
  observed_candidate <- c(TRUE, FALSE, FALSE, FALSE)
  simulated_candidate <- matrix(c(
    TRUE, FALSE, FALSE, FALSE,
    FALSE, TRUE, FALSE, FALSE
  ), 4, 2)
  result <- v21_convergence_summary(
    observed_candidate = observed_candidate,
    simulated_candidate = simulated_candidate,
    observed_q = c(0.05, 1, 1, 1),
    simulated_q = matrix(c(
      0.05, 1, 1, 1,
      1, 0.05, 1, 1
    ), 4, 2),
    spike = c(1, 0, -1, -2),
    spike_feature = "test",
    supported = rep(TRUE, 4),
    spike_quantile = 0.75
  )
  q10 <- result$summary[
    result$summary$metric == "candidate_q10_human_spike_count", ]
  testthat::expect_equal(q10$observed_value, 1)
  testthat::expect_equal(
    result$null$candidate_q10_human_spike_count, c(1, 0)
  )
})
