source(testthat::test_path("../../R/pipeline_support.R"))
repo_root <- testthat::test_path("../..")
hb_load_modules("final_registry", root = repo_root)

# The locked result registry reads adopted stage outputs. Most of them are
# versioned under results/, but the stage-05 DID candidate detail table is not,
# so these two tests can only run in a checkout that also carries a completed
# pipeline run. Skipping with an explicit reason keeps the check active wherever
# the artifacts exist instead of quietly passing when they do not.
skip_without_locked_artifacts <- function() {
  required <- file.path(
    repo_root, "results", "ecological_v22_did_human_context",
    "did_candidate_details.csv"
  )
  testthat::skip_if_not(
    file.exists(required),
    paste(
      "Locked stage-05 artifact", required,
      "is not versioned; run the publication pipeline before these checks."
    )
  )
}

# Exact numerical assertions belong only to the published 1,923-observation
# analysis. The public reconstruction deliberately runs on a different analysis
# population and must be tested for structure, finiteness and claim ceilings,
# not forced to equal the published numbers. The pipeline writes this marker
# before any stage runs, so the test cannot infer the baseline from the result.
current_analysis_baseline <- function() {
  marker <- file.path(
    repo_root, "results", "final_analysis_pipeline", "run_mode.txt"
  )
  if (!file.exists(marker)) return("published")
  lines <- readLines(marker, warn = FALSE)
  value <- sub("^baseline=", "", grep("^baseline=", lines, value = TRUE))
  if (length(value) == 1L && value %in% c("published", "reconstruction")) {
    value
  } else {
    stop("run_mode.txt does not contain one valid baseline= entry.")
  }
}

result_value <- function(results, id, field) {
  row <- results[results$result_id == id, , drop = FALSE]
  testthat::expect_equal(nrow(row), 1L, info = paste("result_id", id))
  as.numeric(row[[field]][[1L]])
}

expect_probability <- function(value, label) {
  testthat::expect_true(
    length(value) == 1L && is.finite(value) && value >= 0 && value <= 1,
    info = label
  )
}

testthat::test_that("publication interface exposes ordered paper stages", {
  stages <- hb_publication_stage_registry()
  testthat::expect_identical(
    stages$stage_id,
    c(
      "01_phenotype", "02_natural_model", "03_local_bombus",
      "04_candidate_definition", "05_human_context", "06_final_lock"
    )
  )
  testthat::expect_equal(anyDuplicated(stages$stage_id), 0L)
})

testthat::test_that("central module and package registries are complete", {
  testthat::expect_true(all(file.exists(file.path(repo_root, hb_module_files))))
  testthat::expect_equal(anyDuplicated(unname(hb_module_files)), 0L)
  testthat::expect_true(all(lengths(hb_package_groups) > 0L))
  testthat::expect_false(file.exists(file.path(repo_root, "final.Rmd")))
})

testthat::test_that("final artifact registry separates inference roles", {
  registry <- final_required_artifacts()
  testthat::expect_true(
    all(c(
      "confirmatory_measurement", "planned_local_bombus_test",
      "exploratory_human_context"
    ) %in% registry$inference_role)
  )
  testthat::expect_equal(anyDuplicated(registry$artifact), 0L)
})

testthat::test_that("final exclusions block circular or causal claims", {
  exclusions <- final_exclusion_registry()
  testthat::expect_true(
    any(grepl("residual", exclusions$excluded_item))
  )
  testthat::expect_true(
    any(grepl("individual Bombus", exclusions$excluded_item))
  )
  testthat::expect_true(
    any(grepl("horticultural origin", exclusions$excluded_item))
  )
})

testthat::test_that("final result registry respects the declared baseline", {
  skip_without_locked_artifacts()
  results <- final_result_registry(repo_root)
  baseline <- current_analysis_baseline()

  required_ids <- c(
    "local_bombus_presence", "local_bombus_intensity",
    "local_isolate_count", "local_isolate_fraction",
    "local_population_5km", "joint_human_followup_count"
  )
  testthat::expect_true(all(required_ids %in% results$result_id))
  testthat::expect_equal(anyDuplicated(results$result_id), 0L)

  presence_q <- result_value(results, "local_bombus_presence", "corrected_p")
  intensity_q <- result_value(results, "local_bombus_intensity", "corrected_p")
  population_q <- result_value(results, "local_population_5km", "corrected_p")
  isolate_count_p <- result_value(results, "local_isolate_count", "raw_p")
  isolate_fraction_p <- result_value(results, "local_isolate_fraction", "raw_p")
  joint_count <- result_value(results, "joint_human_followup_count", "estimate")

  expect_probability(presence_q, "local Bombus presence corrected p")
  expect_probability(intensity_q, "local Bombus intensity corrected p")
  expect_probability(population_q, "local population corrected p")
  expect_probability(isolate_count_p, "local-isolate count p")
  expect_probability(isolate_fraction_p, "local-isolate fraction p")
  testthat::expect_true(
    is.finite(joint_count) && joint_count >= 0 && joint_count == round(joint_count)
  )

  if (identical(baseline, "published")) {
    testthat::expect_equal(presence_q, 0.027972027972028)
    testthat::expect_gt(population_q, 0.05)
    testthat::expect_equal(joint_count, 1)
  } else {
    # The reconstruction reports its own numerical result. Direction and claim
    # ceilings remain testable, but equality to the 1,923-observation result is
    # neither expected nor evidence of correctness.
    testthat::expect_gt(
      result_value(results, "local_bombus_presence", "estimate"), 0
    )
    testthat::expect_gt(
      result_value(results, "local_bombus_intensity", "estimate"), 0
    )
  }
})

testthat::test_that("claim registry preserves causal ceilings", {
  skip_without_locked_artifacts()
  results <- final_result_registry(repo_root)
  claims <- final_claim_registry(results)
  local <- claims[
    claims$claim_id == "C4_local_bombus_turnover", ,
    drop = FALSE
  ]
  human <- claims[
    claims$claim_id == "C6_local_human_context", ,
    drop = FALSE
  ]
  isolates <- claims[
    claims$claim_id == "C5_local_isolates", ,
    drop = FALSE
  ]
  testthat::expect_match(local$claim_ceiling, "not causal")
  testthat::expect_match(human$claim_ceiling, "not human-mediated origin")
  testthat::expect_match(isolates$claim_ceiling, "not introduction evidence")
  testthat::expect_true(nzchar(local$status))
  testthat::expect_true(nzchar(human$status))
  testthat::expect_true(nzchar(isolates$status))
})
