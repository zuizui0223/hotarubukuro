source(testthat::test_path("../../R/pipeline_support.R"))
repo_root <- testthat::test_path("../..")
hb_load_modules("final_registry", root = repo_root)

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

testthat::test_that("final result registry has locked analysis tiers", {
  results <- final_result_registry(
    testthat::test_path("../..")
  )
  testthat::expect_equal(
    results$corrected_p[
      results$result_id == "local_bombus_presence"
    ],
    0.027972027972028
  )
  testthat::expect_gt(
    results$corrected_p[
      results$result_id == "local_population_5km"
    ],
    0.05
  )
  testthat::expect_equal(
    results$estimate[
      results$result_id == "joint_human_followup_count"
    ],
    1
  )
})

testthat::test_that("claim registry preserves causal ceilings", {
  results <- final_result_registry(
    testthat::test_path("../..")
  )
  claims <- final_claim_registry(results)
  local <- claims[
    claims$claim_id == "C4_local_bombus_turnover", ,
    drop = FALSE
  ]
  human <- claims[
    claims$claim_id == "C6_local_human_context", ,
    drop = FALSE
  ]
  testthat::expect_match(local$claim_ceiling, "not causal")
  testthat::expect_match(human$status, "suggestive")
})
