args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) {
  args[[1L]]
} else {
  "results/ecological_v21_local_human_neighbourhood"
}
read_output <- function(name) {
  utils::read.csv(
    file.path(output_dir, name), check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
validation <- read_output("human_neighbourhood_independent_validation.csv")
summary <- read_output("human_neighbourhood_contrast_summary.csv")
global <- read_output("human_neighbourhood_global_summary.csv")
support <- read_output("human_neighbourhood_configuration_support.csv")
quality <- read_output("human_neighbourhood_quality_summary.csv")
convergence <- read_output("human_neighbourhood_convergence_summary.csv")
followup <- read_output("human_neighbourhood_followup_candidates.csv")
population <- read_output("human_neighbourhood_population_scale_summary.csv")
prevalence <- read_output("human_neighbourhood_landuse_prevalence.csv")
collinearity <- read_output("human_neighbourhood_collinearity.csv")
metadata <- read_output("human_neighbourhood_metadata.csv")
metadata_value <- setNames(metadata$value, metadata$field)

checks <- list()
add_check <- function(check, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check,
    status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = detail,
    stringsAsFactors = FALSE
  )
}
primary <- "primary_10km_env1_all_white"
primary_summary <- summary[
  summary$configuration == primary, , drop = FALSE
]
feature <- function(name, configuration = primary) {
  summary[
    summary$configuration == configuration &
      summary$feature == name, , drop = FALSE
  ]
}

add_check(
  "independent_validation",
  all(validation$status == "PASS"),
  paste("checks=", nrow(validation))
)
primary_support <- support[support$configuration == primary, ]
add_check(
  "all_primary_candidates_retained",
  primary_support$observed_requested_cases == 16L &&
    primary_support$observed_usable_cases == 16L,
  paste(
    "usable=", primary_support$observed_usable_cases,
    "of", primary_support$observed_requested_cases
  )
)
primary_global <- global[global$configuration == primary, ]
add_check(
  "primary_multivariate_human_null",
  primary_global$empirical_p > 0.05,
  paste(
    "Mahalanobis=", round(primary_global$observed_value, 3),
    "p=", round(primary_global$empirical_p, 4)
  )
)
settlement <- feature("settlement_density_score")
built <- feature("built_up_fraction_rank")
primary_population <- feature("local_population_rank")
add_check(
  "weak_settlement_direction_not_confirmatory",
  settlement$observed_focal_minus_white_neighbour > 0 &&
    settlement$directional_or_two_sided_p > 0.05 &&
    settlement$maxT_FWER_p > 0.05 &&
    built$maxT_FWER_p > 0.05 &&
    primary_population$maxT_FWER_p > 0.05,
  paste(
    "settlement difference=", round(
      settlement$observed_focal_minus_white_neighbour, 4
    ),
    "raw p=", round(settlement$directional_or_two_sided_p, 4),
    "FWER=", round(settlement$maxT_FWER_p, 4)
  )
)
consensus <- feature("human_activity_consensus_score")
add_check(
  "human_consensus_not_supported",
  consensus$directional_or_two_sided_p > 0.05 &&
    consensus$maxT_FWER_p > 0.05,
  paste(
    "difference=", round(
      consensus$observed_focal_minus_white_neighbour, 4
    ),
    "p=", round(consensus$directional_or_two_sided_p, 4),
    "FWER=", round(consensus$maxT_FWER_p, 4)
  )
)
population_5 <- population[
  population$feature == "population_5km_rank", ]
population_10 <- population[
  population$feature == "population_10km_rank", ]
population_25 <- population[
  population$feature == "population_25km_rank", ]
population_50 <- population[
  population$feature == "population_50km_rank", ]
add_check(
  "localized_population_pattern_is_suggestive_not_confirmatory",
  population_5$directional_or_two_sided_p < 0.05 &&
    population_10$directional_or_two_sided_p < 0.05 &&
    population_5$maxT_FWER_p > 0.05 &&
    population_10$maxT_FWER_p > 0.05 &&
    population_25$directional_or_two_sided_p > 0.05 &&
    population_50$directional_or_two_sided_p > 0.05,
  paste(
    "5km p=", round(population_5$directional_or_two_sided_p, 4),
    "FWER=", round(population_5$maxT_FWER_p, 4),
    "; 10km p=", round(population_10$directional_or_two_sided_p, 4),
    "; 25km p=", round(population_25$directional_or_two_sided_p, 4),
    "; 50km p=", round(population_50$directional_or_two_sided_p, 4)
  )
)
scale25_population <- feature(
  "local_population_rank", "scale_25km_env1_white90"
)
scale25_consensus <- feature(
  "human_activity_consensus_score", "scale_25km_env1_white90"
)
add_check(
  "scale25_population_signal_is_sensitivity_only",
  scale25_population$directional_or_two_sided_p < 0.05 &&
    scale25_population$maxT_FWER_p > 0.05 &&
    scale25_consensus$maxT_FWER_p > 0.05,
  paste(
    "population raw p=", round(
      scale25_population$directional_or_two_sided_p, 4
    ),
    "FWER=", round(scale25_population$maxT_FWER_p, 4),
    "consensus raw p=", round(
      scale25_consensus$directional_or_two_sided_p, 4
    )
  )
)
settlement_spike <- convergence[
  convergence$spike_feature == "settlement_density_score" &
    convergence$metric == "candidate_human_spike_count", ]
joint_consensus <- convergence[
  convergence$spike_feature == "human_activity_consensus_score" &
    convergence$metric == "candidate_q10_human_spike_count", ]
add_check(
  "exploratory_spike_signal_below_corrected_threshold",
  settlement_spike$empirical_p < 0.05 &&
    settlement_spike$BH_q > 0.05 &&
    settlement_spike$maxT_FWER_p > 0.05 &&
    joint_consensus$maxT_FWER_p > 0.05,
  paste(
    "settlement spike count=", settlement_spike$observed_value,
    "raw p=", round(settlement_spike$empirical_p, 4),
    "FWER=", round(settlement_spike$maxT_FWER_p, 4),
    "; q10 consensus count=", joint_consensus$observed_value,
    "FWER=", round(joint_consensus$maxT_FWER_p, 4)
  )
)
add_check(
  "one_convergent_followup_not_population_claim",
  sum(followup$joint_q10_consensus_spike %in% TRUE) == 1L,
  paste(
    "joint q10 and human-spike candidates=",
    sum(followup$joint_q10_consensus_spike %in% TRUE)
  )
)
add_check(
  "sampling_environment_controls_null",
  all(quality$maxT_FWER_p > 0.05),
  paste(
    "minimum quality-control FWER=",
    round(min(quality$maxT_FWER_p), 4)
  )
)
candidate_golf <- prevalence$primary_focal_nonzero_fraction[
  prevalence$feature == "golf_course_fraction"
]
candidate_road <- prevalence$primary_focal_nonzero_fraction[
  prevalence$feature == "road_land_fraction"
]
add_check(
  "no_direct_golf_or_road_land_convergence",
  candidate_golf == 0 && candidate_road == 0,
  paste(
    "golf nonzero fraction=", candidate_golf,
    "road-land nonzero fraction=", candidate_road
  )
)
add_check(
  "collinearity_disclosed",
  max(collinearity$absolute_spearman_rho, na.rm = TRUE) > 0.9,
  paste(
    "maximum absolute Spearman rho=",
    round(max(collinearity$absolute_spearman_rho, na.rm = TRUE), 3),
    "; composites are not independent effects"
  )
)
add_check(
  "claim_ceiling",
  grepl(
    "not horticultural origin",
    metadata_value[["causal_claim_ceiling"]], fixed = TRUE
  ),
  metadata_value[["causal_claim_ceiling"]]
)
add_check(
  "final_Rmd_hash",
  validation$status[
    validation$check == "final_Rmd_untouched"
  ] == "PASS",
  validation$detail[
    validation$check == "final_Rmd_untouched"
  ]
)

audit <- do.call(rbind, checks)
utils::write.csv(
  audit,
  file.path(output_dir, "human_neighbourhood_claim_audit.csv"),
  row.names = FALSE
)
lines <- c(
  paste0(
    "# v21 local human-neighbourhood claim audit: ",
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
  stop("v21 claim audit failed.", call. = FALSE)
}
cat("v21 claim audit passed: ", nrow(audit), " checks\n")
