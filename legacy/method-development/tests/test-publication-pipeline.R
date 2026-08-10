source(testthat::test_path("../../R/pipeline_support.R"))
repo_root <- testthat::test_path("../..")
hb_load_modules("final_registry", root = repo_root)

skip_without_active_artifacts <- function() {
  required <- c(
    file.path(
      repo_root, "results", "ecological_v22_did_human_context",
      "did_candidate_details.csv"
    ),
    file.path(
      repo_root, "results", "ecological_v17_bombus_limitation_gate",
      "bombus_limitation_gate_summary.csv"
    )
  )
  testthat::skip_if_not(
    all(file.exists(required)),
    "Run the active 1,909 pipeline before numerical interface checks."
  )
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

testthat::test_that("publication interface exposes ordered active stages", {
  stages <- hb_publication_stage_registry()
  testthat::expect_identical(
    stages$stage_id,
    c(
      "01_phenotype", "02_natural_model", "03_local_bombus",
      "04_candidate_definition", "05_human_context", "06_final_lock"
    )
  )
  testthat::expect_equal(anyDuplicated(stages$stage_id), 0L)
  testthat::expect_identical(
    stages$manuscript_role[stages$stage_id == "03_local_bombus"],
    "local_pollinator_limitation_test"
  )
})

testthat::test_that("module and package registries are complete", {
  testthat::expect_true(all(file.exists(file.path(repo_root, hb_module_files))))
  testthat::expect_equal(anyDuplicated(unname(hb_module_files)), 0L)
  testthat::expect_true(all(lengths(hb_package_groups) > 0L))
  testthat::expect_true("bombus_limitation_gate" %in% names(hb_stage_modules))
})

testthat::test_that("result and exclusion registries preserve inference roles", {
  registry <- final_required_artifacts()
  testthat::expect_true(all(c(
    "confirmatory_measurement", "local_pollinator_limitation_test",
    "exploratory_human_context"
  ) %in% registry$inference_role))
  exclusions <- final_exclusion_registry()
  testthat::expect_true(any(grepl("horticultural origin", exclusions$excluded_item)))
  testthat::expect_true(any(grepl("community-turnover", exclusions$excluded_item)))
})

testthat::test_that("active result registry is finite and directionally coherent", {
  skip_without_active_artifacts()
  results <- final_result_registry(repo_root)
  required_ids <- c(
    "local_bombus_limitation_presence", "local_bombus_limitation_intensity",
    "local_isolate_count", "local_isolate_fraction",
    "local_population_5km", "joint_human_followup_count"
  )
  testthat::expect_true(all(required_ids %in% results$result_id))
  testthat::expect_equal(anyDuplicated(results$result_id), 0L)
  for (id in required_ids[1:5]) {
    expect_probability(result_value(results, id, "raw_p"), paste(id, "raw p"))
  }
  testthat::expect_gt(
    result_value(results, "local_bombus_limitation_presence", "estimate"), 0
  )
  testthat::expect_false(
    result_value(results, "local_bombus_limitation_intensity", "estimate") > 0 &&
      result_value(results, "local_bombus_limitation_intensity", "raw_p") < 0.05
  )
  joint <- result_value(results, "joint_human_followup_count", "estimate")
  testthat::expect_true(is.finite(joint) && joint >= 0 && joint == round(joint))
})

testthat::test_that("claim registry preserves limitation-gate causal ceilings", {
  skip_without_active_artifacts()
  claims <- final_claim_registry(final_result_registry(repo_root))
  ceiling <- claims$claim_ceiling[
    claims$claim_id == "C4_local_bombus_limitation"
  ]
  testthat::expect_match(ceiling, "not abundance")
  testthat::expect_match(ceiling, "after exploratory design development")
  testthat::expect_match(
    claims$claim_ceiling[claims$claim_id == "C6_local_human_context"],
    "not human-mediated origin"
  )
})

testthat::test_that("active Bombus stage exposes matching and SDM uncertainty ceiling", {
  script_path <- file.path(repo_root, "scripts", "run_bombus_limitation_gate.R")
  script <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
  testthat::expect_match(script, "primary_low_threshold", fixed = TRUE)
  testthat::expect_match(script, "after exploratory design development", fixed = TRUE)
  testthat::expect_match(script, "no second local SPDE fit", fixed = TRUE)
  testthat::expect_match(script, "not abundance", fixed = TRUE)

  metadata_path <- file.path(
    repo_root, "results", "ecological_v17_bombus_limitation_gate",
    "bombus_limitation_gate_metadata.csv"
  )
  if (file.exists(metadata_path)) {
    metadata <- utils::read.csv(
      metadata_path, check.names = FALSE, stringsAsFactors = FALSE
    )
    required_fields <- c(
      "primary_low_threshold", "available_threshold",
      "local_environment_model", "local_spatial_model",
      "bombus_surface_role", "bombus_uncertainty", "inference_role"
    )
    testthat::expect_true(all(required_fields %in% metadata$field))
  }
})
