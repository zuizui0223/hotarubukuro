args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[1] else
  "results/ecological_v16_predictive_replication"

read_result <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing result: ", path, call. = FALSE)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

quality <- read_result("predictive_replication_data_quality.csv")
logs <- read_result("predictive_replication_model_log.csv")
performance <- read_result("predictive_replication_model_performance.csv")
scores <- read_result("predictive_replication_cell_candidate_scores.csv")
null <- read_result("predictive_replication_candidate_null_summary.csv")
stability <- read_result("predictive_replication_simulation_stability.csv")
metadata <- read_result("predictive_replication_metadata.csv")
manifest <- read_result("predictive_replication_draw_manifest.csv")
independent_validation <- read_result(
  "predictive_replication_independent_validation.csv"
)

metric_value <- function(name) {
  value <- quality$value[quality$metric == name]
  if (length(value)) as.numeric(value[1]) else NA_real_
}
metadata_value <- function(name) {
  value <- metadata$value[metadata$field == name]
  if (length(value)) value[1] else NA_character_
}

checks <- list()
add_check <- function(check, status, evidence, severity = "") {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check, status = status, severity = severity,
    evidence = evidence, stringsAsFactors = FALSE
  )
}

add_check(
  "cell_grain_integrity",
  if (metric_value("n_duplicate_cell_ids") == 0 &&
      metric_value("n_cell_aggregation_mismatches") == 0 &&
      metric_value("n_invalid_cell_counts") == 0) "PASS" else "FAIL",
  paste0(
    "duplicates=", metric_value("n_duplicate_cell_ids"),
    "; aggregation mismatches=", metric_value("n_cell_aggregation_mismatches"),
    "; invalid counts=", metric_value("n_invalid_cell_counts")
  ),
  "critical"
)

add_check(
  "hurdle_response_separation",
  if (metric_value("n_white_cells_with_intensity") == 0) "PASS" else "FAIL",
  paste0(
    "white cells with conditional intensity=",
    metric_value("n_white_cells_with_intensity"),
    "; intensity cells=", metric_value("n_cells_with_conditional_intensity")
  ),
  "critical"
)

expected_models <- c(
  "national_environment_spde_presence",
  "national_environment_spde_intensity"
)
complete_folds <- aggregate(
  heldout_spatial_fold ~ model, logs,
  function(x) length(unique(x))
)
complete <- all(expected_models %in% complete_folds$model) &&
  nrow(complete_folds) == length(expected_models) &&
  all(complete_folds$heldout_spatial_fold[
    match(expected_models, complete_folds$model)
  ] == 5L)
add_check(
  "crossfit_model_completion", if (complete) "PASS" else "FAIL",
  paste0(
    "models=", paste(sort(unique(logs$model)), collapse = ","),
    "; fold counts=",
    paste(complete_folds$model, complete_folds$heldout_spatial_fold,
          sep = ":", collapse = ", ")
  ),
  "critical"
)

add_check(
  "national_pollinator_exclusion",
  if (!any(grepl("bombus|fingerprint", logs$model, ignore.case = TRUE)) &&
      !any(grepl("bombus|fingerprint", logs$formula, ignore.case = TRUE)) &&
      !any(grepl("bombus|fingerprint", manifest$model, ignore.case = TRUE)) &&
      !file.exists(file.path(
        output_dir, "predictive_replication_bombus_paired_contrast.csv"
      ))) "PASS" else "FAIL",
  "Stage 02 contains only flower environment-plus-SPDE models; no national Bombus comparator is fitted or retained.",
  "critical"
)

add_check(
  "independent_recalculation",
  if (nrow(independent_validation) > 0L &&
      !any(independent_validation$status == "FAIL")) "PASS" else "FAIL",
  paste0(
    "checks=", nrow(independent_validation),
    "; failures=", sum(independent_validation$status == "FAIL")
  ),
  "critical"
)

projection_columns <- c(
  "posterior_vs_INLA_fitted_correlation",
  "posterior_eta_vs_INLA_linear_correlation",
  "spde_training_location_mean_zero_constraint"
)
projection_ok <- all(projection_columns %in% names(logs)) &&
  all(logs$posterior_vs_INLA_fitted_correlation >= 0.98) &&
  all(logs$posterior_eta_vs_INLA_linear_correlation >= 0.98) &&
  all(logs$spde_training_location_mean_zero_constraint)
add_check(
  "spde_projected_predictor_integrity",
  if (projection_ok) "PASS" else "FAIL",
  if (all(projection_columns %in% names(logs))) paste0(
    "minimum response-scale correlation=",
    round(min(logs$posterior_vs_INLA_fitted_correlation), 4),
    "; minimum linear-predictor correlation=",
    round(min(logs$posterior_eta_vs_INLA_linear_correlation), 4),
    "; training-location constraints=",
    sum(logs$spde_training_location_mean_zero_constraint), "/", nrow(logs)
  ) else "Required APredictor integrity columns are missing.",
  "critical"
)

draw_complete <- nrow(manifest) == length(expected_models) &&
  setequal(manifest$model, expected_models) &&
  length(unique(manifest$n_draws)) == 1L && manifest$n_draws[1] >= 1000L
add_check(
  "predictive_draw_completion", if (draw_complete) "PASS" else "FAIL",
  paste0(
    "models=", nrow(manifest), "; draws=",
    paste(unique(manifest$n_draws), collapse = ",")
  ),
  "critical"
)

formula_text <- paste(logs$formula, collapse = " ")
leakage_pattern <- "population|DOY|intensity|human|road|forest|region|East|West|Bombus|fingerprint"
add_check(
  "natural_model_leakage",
  if (!grepl(leakage_pattern, formula_text, ignore.case = TRUE)) "PASS" else "FAIL",
  "National natural-model formulas contain environment plus spatial field only; pollinator and human-context variables are excluded.",
  "critical"
)

maximum_vif <- max(logs$maximum_predictor_VIF, na.rm = TRUE)
maximum_correlation <- max(
  logs$maximum_absolute_predictor_correlation, na.rm = TRUE
)
collinearity_status <- if (
  maximum_vif <= 5 && maximum_correlation <= 0.8
) "PASS" else if (
  maximum_vif <= 10 && maximum_correlation <= 0.95
) "WARN" else "FAIL"
add_check(
  "predictor_collinearity",
  collinearity_status,
  paste0(
    "maximum fold-specific VIF=", round(maximum_vif, 3),
    "; maximum absolute correlation=", round(maximum_correlation, 3)
  ),
  if (collinearity_status == "FAIL") "critical" else "medium"
)

specification_ok <- identical(
  metadata_value("analysis_spec_version"),
  "v16.5_centered_observation_year"
) && all(manifest$checkpoint_analysis_spec_version %in% c(
  "v16.4_apredictor_projection", "v16.5_centered_observation_year"
)) && identical(
  metadata_value("analysis_version"), "v16_natural_reference_only"
)
add_check(
  "analysis_specification_version",
  if (isTRUE(specification_ok)) "PASS" else "FAIL",
  paste0(
    "pipeline=", metadata_value("analysis_version"), "/",
    metadata_value("analysis_spec_version"),
    "; checkpoint versions=",
    paste(unique(manifest$checkpoint_analysis_spec_version), collapse = ",")
  ),
  "critical"
)
add_check(
  "pollinator_scope_lock",
  if (grepl("local stage 03", metadata_value("pollinator_scope"), fixed = TRUE)) {
    "PASS"
  } else "FAIL",
  paste0("pollinator_scope=", metadata_value("pollinator_scope")),
  "critical"
)
add_check(
  "residual_role",
  if (identical(metadata_value("residual_as_primary_response"), "false")) {
    "PASS"
  } else "FAIL",
  paste0("residual_as_primary_response=", metadata_value("residual_as_primary_response")),
  "critical"
)

candidate_q_columns <- c("unexpected_pigmented_q", "unexpected_white_q")
q_ok <- all(vapply(scores[candidate_q_columns], function(x) {
  all(is.finite(x) & x > 0 & x <= 1)
}, logical(1))) &&
  all(scores$model == "national_environment_spde_presence")
add_check(
  "candidate_tail_probability_bounds", if (q_ok) "PASS" else "FAIL",
  paste0(
    "range=", paste(range(unlist(scores[candidate_q_columns])), collapse = " to "),
    "; score models=", paste(unique(scores$model), collapse = ",")
  ),
  "critical"
)

coverage_ok <- nrow(performance) == 2L &&
  setequal(performance$model, expected_models) &&
  all(is.finite(performance$coverage_95)) &&
  all(performance$coverage_95 >= 0 & performance$coverage_95 <= 1)
add_check(
  "predictive_performance_finite", if (coverage_ok) "PASS" else "FAIL",
  paste0(
    "models=", paste(performance$model, collapse = ","),
    "; primary metrics=", paste(round(performance$primary_value, 4), collapse = ","),
    "; coverage=", paste(round(performance$coverage_95, 3), collapse = ",")
  ),
  "critical"
)

presence_performance <- performance[
  performance$model == "national_environment_spde_presence", , drop = FALSE
]
prevalence_gap <- abs(
  presence_performance$observed_prevalence -
    presence_performance$predicted_prevalence
)
calibration_available <- nrow(presence_performance) == 1L &&
  all(is.finite(prevalence_gap)) &&
  all(is.finite(presence_performance$calibration_slope))
add_check(
  "presence_predictive_calibration",
  if (!calibration_available) "FAIL" else if (
    max(prevalence_gap) <= 0.05 &&
      all(presence_performance$calibration_slope >= 0.5) &&
      all(presence_performance$calibration_slope <= 1.5)
  ) "PASS" else "WARN",
  if (calibration_available) paste0(
    "prevalence gap=", round(max(prevalence_gap), 4),
    "; calibration slope=",
    round(presence_performance$calibration_slope, 3),
    "; AUC=", round(presence_performance$AUC, 3)
  ) else "Presence calibration fields are missing or non-finite.",
  if (calibration_available) "medium" else "critical"
)

runtime_log_paths <- Sys.glob(file.path(output_dir, "run_stderr*.log"))
stderr_text <- if (length(runtime_log_paths)) {
  runtime_lines <- unlist(lapply(runtime_log_paths, readLines, warn = FALSE))
  runtime_lines <- iconv(runtime_lines, from = "", to = "ASCII", sub = "")
  paste(runtime_lines[!is.na(runtime_lines)], collapse = "\n")
} else ""
count_fixed <- function(pattern) {
  lengths(regmatches(
    stderr_text, gregexpr(pattern, stderr_text, fixed = TRUE, useBytes = TRUE)
  ))
}
fatal_inla <- count_fixed("result collection failed")
numeric_inla <- count_fixed("gsl:") +
  count_fixed("GMRFLib_2order_approx: rescue NAN/INF")
add_check(
  "INLA_runtime_diagnostics",
  if (fatal_inla > 0) "FAIL" else if (numeric_inla > 0) "WARN" else "PASS",
  paste0(
    "result-collection failures=", fatal_inla,
    "; numerical warnings=", numeric_inla
  ),
  if (fatal_inla > 0) "critical" else "medium"
)

stability_ok <- nrow(stability) == 2L &&
  all(stability$spearman_rank_correlation >= 0.95) &&
  all(stability$top20_jaccard >= 0.75)
add_check(
  "simulation_half_stability",
  if (stability_ok) "PASS" else "WARN",
  paste0(
    "rows=", nrow(stability),
    "; minimum rank rho=", round(min(stability$spearman_rank_correlation), 3),
    "; minimum top20 Jaccard=", round(min(stability$top20_jaccard), 3),
    "; maximum median tail-q difference=",
    round(max(stability$median_absolute_tail_probability_difference), 4)
  ),
  "medium"
)

pigmented_null <- null[
  null$model == "national_environment_spde_presence" &
    null$direction == "pigmented",
]
significant_facets <- pigmented_null[
  pigmented_null$metric %in% c(
    "mean_population_context",
    "mean_intensity_surprise", "mean_local_colour_isolation",
    "isolated_candidate_fraction_25km"
  ) & pigmented_null$BH_q < 0.05,
]
add_check(
  "candidate_facet_convergence", "RESULT",
  paste0(
    "unexpected-pigmented facet rows with BH q<0.05=",
    nrow(significant_facets)
  ),
  ""
)

significant_gradients <- pigmented_null[
  grepl("_tier_gradient$", pigmented_null$metric) &
    pigmented_null$BH_q < 0.05,
]
add_check(
  "candidate_tier_gradient", "RESULT",
  paste0(
    "unexpected-pigmented 5%-minus-20% gradients with BH q<0.05=",
    nrow(significant_gradients)
  ),
  ""
)

white_control <- null[
  null$model == "national_environment_spde_presence" &
    null$direction == "white" & null$BH_q < 0.05,
]
add_check(
  "unexpected_white_directional_control", "RESULT",
  paste0(
    "unexpected-white directional-control rows with BH q<0.05=",
    nrow(white_control)
  ),
  ""
)

audit <- do.call(rbind, checks)
utils::write.csv(
  audit, file.path(output_dir, "predictive_replication_audit.csv"),
  row.names = FALSE
)

blocking <- audit$status == "FAIL"
warnings <- audit$status == "WARN"
overall <- if (any(blocking)) {
  "NEEDS_REVISION"
} else if (any(warnings)) {
  "PASS_WITH_WARNINGS"
} else {
  "PASS"
}

lines <- c(
  paste0("# v16 natural flower-colour audit: ", overall),
  "",
  paste0(
    "Cells: ", metric_value("n_cells_1km"),
    "; images: ", metric_value("n_raw_observations"),
    "; mixed cells: ", metric_value("n_mixed_cells"), "."
  ),
  "",
  "## Checks",
  "",
  vapply(seq_len(nrow(audit)), function(index) {
    paste0(
      "- **", audit$status[index], " - ", audit$check[index], "**: ",
      audit$evidence[index]
    )
  }, character(1)),
  "",
  "## Interpretation ceiling",
  "",
  "Stage 02 quantifies the nationwide flower-colour reference under measured environment plus continuous residual geography at observed sampling cells. It contains no predicted Bombus term or national pollinator comparison. Bombus enters the active analysis only in the local, environmentally matched stage-03 limitation gate.",
  "",
  "The simulations quantify how often the same candidate-extraction workflow and its spatial or held-out facets arise under the fitted natural model. They do not estimate the probability of horticultural origin. Unmeasured natural covariates, genetic structure, observation bias, and conditional independence between hurdle components remain alternative explanations."
)
writeLines(lines, file.path(output_dir, "AUDIT.md"))
cat(overall, "\n")
print(audit)
if (any(blocking)) quit(status = 1L)
