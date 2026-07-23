args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) {
  args[1]
} else "results/ecological_v18_extreme_human_diagnostic"

source("scripts/extreme_human_diagnostic_v11.R")

read_output <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing output: ", path, call. = FALSE)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

profile <- read_output("extreme_cell_tail_profiles.csv")
envelope <- read_output("extreme_global_envelope.csv")
summary <- read_output("extreme_rematched_summary.csv")
null <- read_output("extreme_rematched_null.csv")
pairs <- read_output("extreme_observed_matched_pairs.csv")
stability <- read_output("extreme_simulation_stability.csv")
settings <- read_output("extreme_matching_settings.csv")
metadata <- read_output("extreme_analysis_metadata.csv")
validation <- read_output("extreme_independent_validation.csv")
cells <- utils::read.csv(
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv",
  check.names = FALSE, stringsAsFactors = FALSE
)

meta <- function(field) metadata$value[match(field, metadata$field)]
checks <- list()
add_check <- function(check, pass, evidence, severity = "critical") {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check, status = if (isTRUE(pass)) "PASS" else "FAIL",
    severity = severity, evidence = evidence,
    stringsAsFactors = FALSE
  )
}

add_check(
  "analysis_version",
  identical(meta("analysis_spec_version"), v18_analysis_spec_version),
  paste("version=", meta("analysis_spec_version"))
)
add_check(
  "cell_grain",
  nrow(profile) == nrow(cells) &&
    !anyDuplicated(profile$exact_site_id) &&
    setequal(profile$exact_site_id, cells$exact_site_id),
  paste("profile rows=", nrow(profile),
        "unique ids=", length(unique(profile$exact_site_id)))
)
q_columns <- c(
  "unexpected_pigmented_q", "early_predictive_q", "dark_predictive_q"
)
q_values <- unlist(profile[q_columns], use.names = FALSE)
q_values <- q_values[is.finite(q_values)]
add_check(
  "tail_probability_bounds",
  all(q_values > 0 & q_values <= 1),
  paste("range=", paste(range(q_values), collapse = " to "))
)
add_check(
  "extreme_selector_is_colour_only",
  grepl("pigmentation tail", meta("extreme_selector"), fixed = TRUE) &&
    grepl("never used", meta("population_role"), fixed = TRUE),
  paste(meta("extreme_selector"), ";", meta("population_role"))
)
add_check(
  "residual_not_used",
  identical(meta("residual_used_as_response"), "false") &&
    grepl("no phenology residual", meta("phenology_role"), fixed = TRUE) &&
    grepl("no intensity residual", meta("intensity_role"), fixed = TRUE),
  paste(
    "residual_used_as_response=", meta("residual_used_as_response"),
    ";", meta("phenology_role"), ";", meta("intensity_role")
  )
)
null_counts <- table(interaction(null$tier, null$metric, drop = TRUE))
add_check(
  "predictive_rematching_completion",
  all(null_counts == 1000L),
  paste("groups=", length(null_counts),
        "draw range=", paste(range(null$draw), collapse = " to "))
)
pair_groups <- split(pairs, interaction(pairs$tier, pairs$metric, drop = TRUE))
nonreuse <- all(vapply(pair_groups, function(d) {
  !anyDuplicated(d$case_id) && !anyDuplicated(d$control_id)
}, logical(1)))
add_check(
  "nonreused_matched_controls",
  nonreuse,
  paste("observed matched groups=", length(pair_groups))
)
cell_fold <- setNames(cells$spatial_fold, cells$exact_site_id)
same_fold <- cell_fold[pairs$case_id] == cell_fold[pairs$control_id]
add_check(
  "same_fold_matching",
  all(same_fold),
  paste("cross-fold observed pairs=", sum(!same_fold))
)
add_check(
  "matching_calipers",
  all(pairs$geographic_distance_km <= settings$maximum_distance_km[1]) &&
    all(pairs$environmental_distance <=
          settings$maximum_environment_distance[1]) &&
    all(pairs$natural_logit_difference <=
          settings$maximum_logit_difference[1]) &&
    all(pairs$effort_difference <= settings$maximum_effort_difference[1]),
  paste(
    "maximum observed geography=", round(max(pairs$geographic_distance_km), 3),
    "environment=", round(max(pairs$environmental_distance), 3),
    "logit=", round(max(pairs$natural_logit_difference), 3),
    "effort=", round(max(pairs$effort_difference), 3)
  )
)
add_check(
  "typical_control_tail",
  all(pairs$control_color_q >= 0.25),
  paste("minimum control q=", min(pairs$control_color_q))
)
primary <- summary[summary$fraction == 0.05, , drop = FALSE]
add_check(
  "primary_match_coverage",
  min(primary$observed_match_fraction) >= 0.80 &&
    min(primary$null_mean_match_fraction) >= 0.75,
  paste(
    "observed range=",
    paste(round(range(primary$observed_match_fraction), 3),
          collapse = " to "),
    "null mean range=",
    paste(round(range(primary$null_mean_match_fraction), 3),
          collapse = " to ")
  ),
  severity = "medium"
)
add_check(
  "global_extreme_envelope",
  any(envelope$metric == "maximum_tail_depth") &&
    any(envelope$metric == "candidate_count") &&
    all(is.finite(envelope$empirical_p)),
  paste(
    "observed maximum=",
    envelope$observed_value[envelope$metric == "maximum_tail_depth"],
    "q05 candidates=",
    envelope$observed_value[envelope$metric == "candidate_count"]
  )
)
add_check(
  "simulation_stability",
  max(stability$absolute_mean_difference, na.rm = TRUE) < 0.05 &&
    min(stability$first_half_n_finite) == 500L &&
    min(stability$second_half_n_finite) == 500L,
  paste(
    "maximum half-mean difference=",
    round(max(stability$absolute_mean_difference), 4)
  ),
  severity = "medium"
)
add_check(
  "independent_recalculation",
  nrow(validation) >= 15L && all(validation$status == "PASS"),
  paste("checks=", nrow(validation),
        "failures=", sum(validation$status != "PASS"))
)
add_check(
  "claim_ceiling",
  grepl("not horticultural origin", meta("horticultural_claim_ceiling"),
        fixed = TRUE),
  meta("horticultural_claim_ceiling")
)
current_md5 <- if (file.exists("final.Rmd")) {
  unname(tools::md5sum("final.Rmd"))
} else NA_character_
add_check(
  "final_Rmd_untouched",
  identical(current_md5, meta("final_Rmd_md5")),
  paste("current=", current_md5, "recorded=", meta("final_Rmd_md5"))
)

audit <- do.call(rbind, checks)
write.csv(
  audit, file.path(output_dir, "extreme_analysis_audit.csv"),
  row.names = FALSE
)
overall <- if (all(audit$status == "PASS")) "PASS" else "NEEDS_REVISION"
lines <- c(
  paste0("# v18 extreme diagnostic audit: ", overall),
  "",
  vapply(seq_len(nrow(audit)), function(i) {
    paste0(
      "- **", audit$status[i], " - ", audit$check[i], "**: ",
      audit$evidence[i]
    )
  }, character(1))
)
writeLines(lines, file.path(output_dir, "AUDIT.md"), useBytes = TRUE)
cat(overall, "\n")
print(audit)
if (!all(audit$status == "PASS")) quit(status = 1L)

