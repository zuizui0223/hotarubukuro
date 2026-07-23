source(testthat::test_path(
  "../../scripts/final_analysis_pipeline_v1.R"
))

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
