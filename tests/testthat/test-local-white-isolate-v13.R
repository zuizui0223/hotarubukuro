source(testthat::test_path("../../scripts/extreme_human_diagnostic_v11.R"))
source(testthat::test_path("../../scripts/local_white_isolate_v13.R"))

testthat::test_that("local profile identifies a pigmented focal among white cells", {
  graph <- list(
    neighbours = list(c(2L, 3L), c(1L), c(1L)),
    weights = list(c(0.5, 0.5), 1, 1),
    support = data.frame(supported = c(TRUE, FALSE, FALSE))
  )
  profile <- v20_local_profile(c(1, 0, 0), graph, 0)
  testthat::expect_true(profile$candidate[1, 1])
  testthat::expect_equal(profile$neighbour_pigment_share[1, 1], 0)
  testthat::expect_equal(profile$local_isolation_score[1, 1], 1)
})

testthat::test_that("a pigmented neighbour prevents an all-white candidate", {
  graph <- list(
    neighbours = list(c(2L, 3L), c(1L), c(1L)),
    weights = list(c(0.5, 0.5), 1, 1),
    support = data.frame(supported = c(TRUE, FALSE, FALSE))
  )
  profile <- v20_local_profile(c(1, 1, 0), graph, 0)
  testthat::expect_false(profile$candidate[1, 1])
  testthat::expect_equal(profile$neighbour_pigment_share[1, 1], 0.5)
})

testthat::test_that("candidate fraction uses supported pigmented focal cells", {
  observed <- list(
    present = matrix(c(TRUE, TRUE, FALSE), ncol = 1),
    candidate = matrix(c(TRUE, FALSE, FALSE), ncol = 1),
    supported = c(TRUE, TRUE, TRUE)
  )
  simulated <- list(
    present = matrix(c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE), 3, 2),
    candidate = matrix(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), 3, 2),
    supported = c(TRUE, TRUE, TRUE)
  )
  result <- v20_metric_rows(
    observed, simulated,
    observed_q = c(0.1, 0.5, 1),
    simulated_q = matrix(c(0.1, 0.5, 1, 0.2, 1, 1), 3, 2),
    configuration = "test"
  )
  fraction <- result$summary[
    result$summary$metric == "candidate_fraction", "observed_value"
  ]
  testthat::expect_equal(fraction, 0.5)
})

testthat::test_that("support filtering is outcome blind", {
  cells <- data.frame(
    exact_site_id = paste0("c", 1:4),
    x_km = c(0, 1, 2, 20), y_km = 0,
    spatial_fold = 1,
    n_independent_sites = 1,
    broad50km_pc1 = 0, broad50km_pc2 = 0,
    within50km_pc1 = 0, within50km_pc2 = 0
  )
  graph <- v20_neighbour_graph(
    cells, radius_km = 5, environment_caliper = 1,
    minimum_neighbours = 2
  )
  testthat::expect_true(graph$support$supported[1])
  cells$invented_colour <- c(1, 0, 0, 1)
  changed <- v20_neighbour_graph(
    cells, radius_km = 5, environment_caliper = 1,
    minimum_neighbours = 2
  )
  testthat::expect_equal(graph$neighbours, changed$neighbours)
})

testthat::test_that("matching support filter preserves empty detail entries", {
  match_options <- list(
    options = list(integer(), 1L, integer()),
    details = list(
      NULL,
      data.frame(control_index = 1L),
      NULL
    ),
    settings = data.frame(dummy = 1)
  )
  graph <- list(support = data.frame(
    n_neighbours = c(3L, 3L, 3L),
    n_neighbour_independent_sites = c(3L, 3L, 3L)
  ))
  filtered <- v20_filter_match_options(match_options, graph)
  testthat::expect_length(filtered$details, 3L)
  testthat::expect_null(filtered$details[[1L]])
  testthat::expect_equal(filtered$details[[2L]]$control_index, 1L)
  testthat::expect_null(filtered$details[[3L]])
})
