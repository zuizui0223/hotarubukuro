source(file.path("..", "..", "R", "local_state_asymmetry.R"))

testthat::test_that("directional profiles detect both local event directions", {
  graph <- list(
    neighbours = list(
      c(2L, 3L), c(1L, 4L), c(1L, 4L), c(2L, 3L)
    ),
    supported = rep(TRUE, 4L)
  )
  counts <- c(1, 0, 0, 1)
  trials <- rep(1, 4L)
  profile <- v23_directional_profiles(counts, trials, graph)

  testthat::expect_true(profile$pigmented_in_white[1, 1])
  testthat::expect_true(profile$white_in_pigmented[2, 1])
  testthat::expect_equal(profile$summary$pigmented_in_white_count, 2)
  testthat::expect_equal(profile$summary$white_in_pigmented_count, 2)
  testthat::expect_equal(profile$summary$log_rate_ratio, 0)
})

testthat::test_that("opportunity normalization distinguishes count and rate asymmetry", {
  graph <- list(
    neighbours = list(
      c(2L, 3L), c(1L, 4L), c(1L, 2L), c(1L, 2L),
      c(1L, 2L), c(1L, 2L), c(1L, 2L), c(1L, 2L)
    ),
    supported = rep(TRUE, 8L)
  )
  counts <- c(1, 0, 0, 1, 0, 0, 0, 0)
  trials <- rep(1, 8L)
  profile <- v23_directional_profiles(counts, trials, graph)

  testthat::expect_equal(profile$summary$pigmented_in_white_count, 1)
  testthat::expect_equal(profile$summary$white_in_pigmented_count, 1)
  testthat::expect_true(
    profile$summary$pigmented_in_white_rate >
      profile$summary$white_in_pigmented_rate
  )
  testthat::expect_true(profile$summary$log_rate_ratio > 0)
})

testthat::test_that("equal raw counts with unequal opportunity give a non-zero log rate ratio", {
  # Two pigmented-in-white events out of two eligible pigmented focals, and two
  # white-in-pigmented events out of six eligible white focals. The raw counts
  # are identical; only the denominators differ. Cells 5-8 are white but sit
  # beside another white cell, so they supply opportunity without an event.
  neighbours <- list(
    c(3L, 4L), c(3L, 4L),
    c(1L, 2L), c(1L, 2L),
    c(1L, 3L), c(1L, 3L), c(1L, 3L), c(1L, 3L)
  )
  graph <- list(neighbours = neighbours, supported = rep(TRUE, 8L))
  counts <- c(1, 1, 0, 0, 0, 0, 0, 0)
  trials <- rep(1, 8L)
  profile <- v23_directional_profiles(counts, trials, graph, pseudocount = 0.5)

  testthat::expect_equal(profile$summary$pigmented_in_white_count, 2)
  testthat::expect_equal(profile$summary$white_in_pigmented_count, 2)
  testthat::expect_equal(profile$summary$eligible_pigmented_focals, 2)
  testthat::expect_equal(profile$summary$eligible_white_focals, 6)
  testthat::expect_equal(profile$summary$count_difference, 0)
  testthat::expect_equal(
    profile$summary$pigmented_in_white_rate, (2 + 0.5) / (2 + 1)
  )
  testthat::expect_equal(
    profile$summary$white_in_pigmented_rate, (2 + 0.5) / (6 + 1)
  )
  testthat::expect_equal(
    profile$summary$log_rate_ratio,
    log(((2 + 0.5) / (2 + 1)) / ((2 + 0.5) / (6 + 1)))
  )
  testthat::expect_true(profile$summary$log_rate_ratio > 0)
})

testthat::test_that("state-rule sensitivities exclude mixed cells as intended", {
  counts <- matrix(c(0, 1, 2), ncol = 1L)
  trials <- c(2, 2, 2)

  legacy <- v23_classify_states(counts, trials)
  pure <- v23_classify_states(
    counts, trials, white_max_share = 0, pigmented_min_share = 1
  )
  threshold <- v23_classify_states(
    counts, trials, white_max_share = 0.10, pigmented_min_share = 0.90
  )

  testthat::expect_equal(as.vector(legacy$white), c(TRUE, FALSE, FALSE))
  testthat::expect_equal(as.vector(legacy$pigmented), c(FALSE, TRUE, TRUE))
  testthat::expect_equal(as.vector(pure$classified), c(TRUE, FALSE, TRUE))
  testthat::expect_equal(
    as.vector(threshold$classified), c(TRUE, FALSE, TRUE)
  )
  # The half-pigmented cell is unclassified under both sensitivities but
  # pigmented under the legacy rule that the locked analysis uses.
  testthat::expect_false(pure$classified[2, 1])
  testthat::expect_false(threshold$classified[2, 1])
  testthat::expect_true(legacy$pigmented[2, 1])
})

testthat::test_that("the predeclared state rules never overlap", {
  rules <- v23_state_rule_table()
  testthat::expect_equal(
    sort(rules$state_rule),
    sort(c("legacy_any_pigmented", "pure_cells", "share_10_90"))
  )
  testthat::expect_true(all(rules$white_max_share < rules$pigmented_min_share))

  counts <- matrix(0:4, ncol = 1L)
  trials <- rep(4, 5L)
  for (index in seq_len(nrow(rules))) {
    states <- v23_classify_states(
      counts, trials,
      white_max_share = rules$white_max_share[[index]],
      pigmented_min_share = rules$pigmented_min_share[[index]]
    )
    testthat::expect_false(any(states$white & states$pigmented))
  }
})

testthat::test_that("overlapping state assignments are rejected", {
  testthat::expect_error(
    v23_classify_states(
      counts = c(0, 1, 2), trials = c(2, 2, 2),
      white_max_share = 0.6, pigmented_min_share = 0.4
    ),
    "overlap"
  )
})

testthat::test_that("counts outside the trial range are rejected", {
  testthat::expect_error(
    v23_classify_states(counts = c(0, 3), trials = c(2, 2)),
    "between zero and trials"
  )
  testthat::expect_error(
    v23_classify_states(counts = c(-1, 1), trials = c(2, 2)),
    "between zero and trials"
  )
})

testthat::test_that("neighbour handling requires every eligible neighbour to be in the opposite state", {
  # Cell 1 is pigmented with neighbours 2 (white) and 3 (pigmented), so it is
  # not a pigmented-in-white event. Cell 4 is pigmented with two white
  # neighbours, so it is.
  graph <- list(
    neighbours = list(c(2L, 3L), c(1L, 3L), c(1L, 2L), c(5L, 6L),
                      c(4L, 6L), c(4L, 5L)),
    supported = rep(TRUE, 6L)
  )
  counts <- c(1, 0, 1, 1, 0, 0)
  trials <- rep(1, 6L)
  profile <- v23_directional_profiles(counts, trials, graph)

  testthat::expect_false(profile$pigmented_in_white[1, 1])
  testthat::expect_true(profile$pigmented_in_white[4, 1])
  testthat::expect_equal(profile$summary$pigmented_in_white_count, 1)
})

testthat::test_that("unsupported focal cells contribute no events and no opportunity", {
  graph <- list(
    neighbours = list(c(2L, 3L), c(1L, 3L), c(1L, 2L), integer()),
    supported = c(TRUE, TRUE, TRUE, FALSE)
  )
  counts <- c(1, 0, 0, 1)
  trials <- rep(1, 4L)
  profile <- v23_directional_profiles(counts, trials, graph)

  testthat::expect_false(profile$pigmented_in_white[4, 1])
  testthat::expect_equal(profile$summary$eligible_pigmented_focals, 1)
  testthat::expect_equal(profile$summary$eligible_white_focals, 2)
})

testthat::test_that("graph support must align one to one with the cells", {
  graph <- list(
    neighbours = list(c(2L), c(1L)),
    supported = c(TRUE, TRUE, TRUE)
  )
  testthat::expect_error(
    v23_directional_profiles(c(1, 0), c(1, 1), graph),
    "one-to-one"
  )
  testthat::expect_error(
    v23_graph_support(list(neighbours = list(1L)), 2L),
    "one entry per cell"
  )
})

testthat::test_that("asymmetry summary compares observed and replicated maps", {
  graph <- list(
    neighbours = list(c(2L, 3L), c(1L, 4L), c(1L, 4L), c(2L, 3L)),
    supported = rep(TRUE, 4L)
  )
  trials <- rep(1, 4L)
  observed <- v23_directional_profiles(c(1, 0, 0, 1), trials, graph)
  simulated <- v23_directional_profiles(
    cbind(
      c(1, 0, 0, 1),
      c(0, 1, 1, 0),
      c(1, 1, 0, 0)
    ),
    trials, graph
  )
  summary <- v23_asymmetry_summary(
    observed, simulated, "legacy_any_pigmented"
  )

  testthat::expect_equal(nrow(summary), 7L)
  testthat::expect_true(all(summary$n_natural_maps == 3L))
  testthat::expect_true("log_rate_ratio" %in% summary$metric)
  testthat::expect_true(all(summary$analysis_status == "post_hoc_diagnostic"))
  testthat::expect_true(all(
    c("observed_value", "null_mean", "null_sd", "lower_95", "upper_95",
      "upper_p", "lower_p", "two_sided_p", "percentile") %in% names(summary)
  ))
})

testthat::test_that("Monte Carlo p-values use the finite-simulation correction", {
  comparison <- v23_null_comparison(observed = 2, simulated = c(0, 1, 2, 3))
  testthat::expect_equal(comparison[["upper_p"]], (1 + 2) / (4 + 1))
  testthat::expect_equal(comparison[["lower_p"]], (1 + 3) / (4 + 1))
  testthat::expect_equal(
    comparison[["two_sided_p"]], min(1, 2 * min((1 + 2) / 5, (1 + 3) / 5))
  )
  testthat::expect_equal(comparison[["percentile"]], mean(c(0, 1, 2, 3) <= 2))

  extreme <- v23_null_comparison(observed = 99, simulated = c(0, 1, 2, 3))
  testthat::expect_equal(extreme[["upper_p"]], 1 / 5)
  testthat::expect_true(extreme[["upper_p"]] > 0)
})

testthat::test_that("the summary is reproducible from the per-map null table", {
  graph <- list(
    neighbours = list(c(2L, 3L), c(1L, 4L), c(1L, 4L), c(2L, 3L)),
    supported = rep(TRUE, 4L)
  )
  trials <- rep(1, 4L)
  set.seed(20260725L)
  draws <- matrix(stats::rbinom(4L * 25L, size = 1L, prob = 0.5), nrow = 4L)
  observed <- v23_directional_profiles(c(1, 0, 0, 1), trials, graph)
  simulated <- v23_directional_profiles(draws, trials, graph)
  summary <- v23_asymmetry_summary(observed, simulated, "legacy_any_pigmented")

  for (metric in unique(summary$metric)) {
    row <- summary[summary$metric == metric, , drop = FALSE]
    recomputed <- v23_null_comparison(
      row$observed_value, simulated$summary[[metric]]
    )
    testthat::expect_equal(row$upper_p, unname(recomputed[["upper_p"]]))
    testthat::expect_equal(row$lower_p, unname(recomputed[["lower_p"]]))
    testthat::expect_equal(row$two_sided_p, unname(recomputed[["two_sided_p"]]))
    testthat::expect_equal(row$null_mean, unname(recomputed[["null_mean"]]))
  }
})

testthat::test_that("profiles are deterministic for a fixed input", {
  graph <- list(
    neighbours = list(c(2L, 3L), c(1L, 4L), c(1L, 4L), c(2L, 3L)),
    supported = rep(TRUE, 4L)
  )
  trials <- rep(1, 4L)
  make_draws <- function() {
    set.seed(4242L)
    matrix(stats::rbinom(4L * 50L, size = 1L, prob = 0.4), nrow = 4L)
  }
  first <- v23_directional_profiles(make_draws(), trials, graph)$summary
  second <- v23_directional_profiles(make_draws(), trials, graph)$summary
  testthat::expect_identical(first, second)
})

testthat::test_that("observed candidate extraction aligns with the profile rows", {
  graph <- list(
    neighbours = list(c(2L, 3L), c(1L, 4L), c(1L, 4L), c(2L, 3L)),
    supported = rep(TRUE, 4L)
  )
  profile <- v23_directional_profiles(c(1, 0, 0, 1), rep(1, 4L), graph)
  candidates <- v23_observed_candidates(
    profile, c("cell-a", "cell-b", "cell-c", "cell-d"), "legacy_any_pigmented"
  )
  testthat::expect_setequal(
    candidates$direction[candidates$exact_site_id == "cell-a"],
    "pigmented_in_white"
  )
  testthat::expect_error(
    v23_observed_candidates(profile, c("cell-a"), "legacy_any_pigmented"),
    "do not align"
  )
})
