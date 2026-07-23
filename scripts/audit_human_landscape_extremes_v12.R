args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) {
  args[[1L]]
} else {
  "results/ecological_v19_human_landscape_extremes"
}

read_output <- function(name) {
  utils::read.csv(
    file.path(output_dir, name), check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

validation <- read_output("landscape_independent_validation.csv")
summary <- read_output("landscape_contrast_summary.csv")
global <- read_output("landscape_global_tests.csv")
rf <- read_output("landscape_rf_summary.csv")
profiles <- read_output("landscape_profile_counts.csv")
join <- read_output("landscape_join_audit.csv")
collinearity <- read_output("landscape_collinearity.csv")
metadata <- read_output("landscape_analysis_metadata.csv")
metadata_value <- setNames(metadata$value, metadata$field)

checks <- list()
add_check <- function(check, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check, status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = detail, stringsAsFactors = FALSE
  )
}

add_check(
  "independent_validation",
  all(validation$status == "PASS"),
  paste("checks=", nrow(validation))
)
add_check(
  "complete_public_landscape_join",
  join$value[join$metric == "n_analysis_cells_complete_features"] >=
    0.95 * join$value[join$metric == "n_analysis_cells"],
  paste(
    "complete cells=",
    join$value[join$metric == "n_analysis_cells_complete_features"]
  )
)
add_check(
  "response_blind_landscape_definition",
  metadata_value[["landscape_features_used_for_selection_or_matching"]] == "none",
  "landscape axes do not select cases or controls"
)
add_check(
  "no_residual_response",
  metadata_value[["residual_used_as_response"]] == "false",
  "case definition is posterior predictive tail"
)
add_check(
  "natural_map_replication",
  metadata_value[["n_natural_maps"]] == "1000" &&
    metadata_value[["n_rf_null_maps"]] == "250",
  paste(
    "contrast maps=", metadata_value[["n_natural_maps"]],
    "RF maps=", metadata_value[["n_rf_null_maps"]]
  )
)
top5_global <- global[global$tier == "top_0005", ]
add_check(
  "primary_global_landscape_test",
  nrow(top5_global) == 1L && top5_global$empirical_p > 0.05,
  paste(
    "Mahalanobis=", round(top5_global$observed_value, 3),
    "p=", round(top5_global$empirical_p, 4)
  )
)
top5_human <- summary[
  summary$tier == "top_0005" &
    summary$feature %in% c(
      "human_reach_score", "urban_mountain_score",
      "satoyama_interface_score"
    ),
]
add_check(
  "human_consistent_composites",
  nrow(top5_human) == 3L &&
    all(top5_human$directional_or_two_sided_p > 0.05) &&
    all(top5_human$maxT_FWER_p > 0.05),
  paste(
    top5_human$feature,
    "difference=", round(top5_human$observed_case_control_difference, 3),
    "p=", round(top5_human$directional_or_two_sided_p, 3),
    collapse = "; "
  )
)
top5_remote <- summary[
  summary$tier == "top_0005" &
    summary$feature == "remote_mountain_score",
]
add_check(
  "remote_mountain_is_alternative_not_human_support",
  nrow(top5_remote) == 1L &&
    top5_remote$observed_case_control_difference > 0 &&
    top5_remote$maxT_FWER_p > 0.05,
  paste(
    "difference=", round(top5_remote$observed_case_control_difference, 3),
    "human-direction p=", round(
      top5_remote$directional_or_two_sided_p, 3
    ),
    "FWER=", round(top5_remote$maxT_FWER_p, 3)
  )
)
auc <- rf[rf$metric == "spatial_cv_auc", ]
concordance <- rf[rf$metric == "paired_concordance", ]
add_check(
  "rf_does_not_discriminate",
  nrow(auc) == 1L && auc$empirical_p > 0.05 &&
    nrow(concordance) == 1L && concordance$empirical_p > 0.05,
  paste(
    "AUC=", round(auc$observed_value, 3),
    "p=", round(auc$empirical_p, 3),
    "paired=", round(concordance$observed_value, 3),
    "p=", round(concordance$empirical_p, 3)
  )
)
top5_profiles <- profiles[profiles$tier == "top_0005", ]
expected_pairs <- rf$n_observed_pairs[rf$metric == "spatial_cv_auc"]
add_check(
  "profile_counts_close",
  length(expected_pairs) == 1L &&
    sum(top5_profiles$case_count) == expected_pairs &&
    sum(top5_profiles$control_count) == expected_pairs,
  paste(
    "case=", sum(top5_profiles$case_count),
    "control=", sum(top5_profiles$control_count)
  )
)
maximum_correlation <- max(collinearity$absolute_spearman_rho)
add_check(
  "collinearity_is_disclosed",
  maximum_correlation > 0.8,
  paste(
    "maximum absolute Spearman rho=", round(maximum_correlation, 3),
    "; no independent RF importance interpretation"
  )
)
add_check(
  "claim_ceiling",
  grepl(
    "not horticultural origin probability",
    metadata_value[["claim_ceiling"]], fixed = TRUE
  ),
  metadata_value[["claim_ceiling"]]
)
add_check(
  "final_Rmd_hash",
  validation$status[validation$check == "final_Rmd_untouched"] == "PASS",
  validation$detail[validation$check == "final_Rmd_untouched"]
)

audit <- do.call(rbind, checks)
utils::write.csv(
  audit, file.path(output_dir, "landscape_analysis_audit.csv"),
  row.names = FALSE
)
lines <- c(
  paste0(
    "# v19 human-landscape extreme audit: ",
    if (all(audit$status == "PASS")) "PASS" else "FAIL"
  ),
  "",
  vapply(seq_len(nrow(audit)), function(index) {
    paste0(
      "- **", audit$status[index], " - ", audit$check[index],
      "**: ", audit$detail[index]
    )
  }, character(1))
)
writeLines(lines, file.path(output_dir, "AUDIT.md"), useBytes = TRUE)
if (any(audit$status != "PASS")) {
  print(audit[audit$status != "PASS", ])
  stop("v19 audit failed.", call. = FALSE)
}
cat("v19 audit passed: ", nrow(audit), " checks\n")
