args <- commandArgs(trailingOnly = TRUE)
positional <- args[!startsWith(args, "--")]
output_dir <- if (length(positional)) {
  positional[[1L]]
} else {
  "results/ecological_v20_local_white_isolates"
}

# Two kinds of check live in this file, and they behave differently once the
# baseline is the public reconstruction rather than the published run.
#
# Code and metadata properties -- the claim ceiling, the independent validation
# having no failures -- are properties of the analysis, not of the data. They
# are enforced as PASS/FAIL in both modes and are never relaxed.
#
# Inferential claims -- "the local event is not excessive", "the human
# composites are not supported" -- are answers the analysis gives about a
# particular observation set. The reconstruction is a different observation set
# and may answer differently; that is information, not a defect. Under
# --baseline reconstruction they are reported as RESULT with their observed
# values, following the RESULT convention already used in
# validation/audit_natural_predictive_model.R. Under --baseline published they
# are enforced exactly as before.
#
# A RESULT is never counted as a pass and never gates the pipeline. It is the
# reconstruction reporting what it found.
baseline_argument <- grep("^--baseline=", args, value = TRUE)
baseline <- if (length(baseline_argument)) {
  sub("^--baseline=", "", baseline_argument[[1L]])
} else {
  "published"
}
if (!baseline %in% c("published", "reconstruction")) {
  stop(
    "--baseline must be 'published' or 'reconstruction'; got '", baseline, "'.",
    call. = FALSE
  )
}
read_output <- function(name) {
  utils::read.csv(
    file.path(output_dir, name), check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
validation <- read_output("local_isolate_independent_validation.csv")
summary <- read_output("local_isolate_natural_null_summary.csv")
candidates <- read_output("local_isolate_candidates.csv")
landscape <- read_output("local_isolate_landscape_summary.csv")
global <- read_output("local_isolate_landscape_global.csv")
auxiliary <- read_output("local_isolate_auxiliary_facets_summary.csv")
auxiliary_counts <- read_output("local_isolate_auxiliary_facets_counts.csv")
pairs <- read_output("local_isolate_observed_pairs.csv")
metadata <- read_output("local_isolate_metadata.csv")
metadata_value <- setNames(metadata$value, metadata$field)

metric <- function(configuration, name) {
  summary[
    summary$configuration == configuration & summary$metric == name,
    , drop = FALSE
  ]
}
checks <- list()
add_check <- function(check, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check,
    status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = detail,
    stringsAsFactors = FALSE
  )
}
add_claim <- function(check, passed, detail) {
  if (identical(baseline, "published")) {
    add_check(check, passed, detail)
  } else {
    checks[[length(checks) + 1L]] <<- data.frame(
      check = check,
      status = "RESULT",
      detail = paste0(
        detail, "; published-mode verdict would be ",
        if (isTRUE(passed)) "PASS" else "FAIL"
      ),
      stringsAsFactors = FALSE
    )
  }
}
add_check(
  "independent_validation",
  !any(validation$status == "FAIL"),
  paste(
    "checks=", nrow(validation),
    "passed=", sum(validation$status == "PASS"),
    "not applicable=", sum(validation$status == "not_applicable")
  )
)
primary_count <- metric(
  "primary_10km_env1_all_white", "candidate_count"
)
primary_fraction <- metric(
  "primary_10km_env1_all_white", "candidate_fraction"
)
add_claim(
  "primary_local_event_not_excessive",
  primary_count$empirical_p > 0.05 &&
    primary_fraction$empirical_p > 0.05,
  paste(
    "count=", primary_count$observed_value,
    "p=", round(primary_count$empirical_p, 4),
    "fraction p=", round(primary_fraction$empirical_p, 4)
  )
)
fold_count <- metric(
  "fold_restricted_10km_env1_all_white", "candidate_count"
)
add_claim(
  "fold_boundary_count_stability",
  fold_count$observed_value == primary_count$observed_value,
  paste(
    "all-neighbour=", primary_count$observed_value,
    "same-fold=", fold_count$observed_value
  )
)
scale_25 <- metric("scale_25km_env1_white90", "candidate_fraction")
add_claim(
  "scale_specific_25km_signal_disclosed",
  scale_25$empirical_p < 0.05,
  paste(
    "sensitivity-only fraction=", round(scale_25$observed_value, 4),
    "p=", round(scale_25$empirical_p, 4)
  )
)
joint_q10 <- metric(
  "primary_10km_env1_all_white", "joint_candidate_q10_count"
)
joint_q05 <- metric(
  "primary_10km_env1_all_white", "joint_candidate_q05_count"
)
add_claim(
  "no_joint_natural_tail_support",
  joint_q10$empirical_p > 0.05 &&
    joint_q05$observed_value == 0,
  paste(
    "q10 count=", joint_q10$observed_value,
    "p=", round(joint_q10$empirical_p, 4),
    "q05 count=", joint_q05$observed_value
  )
)
add_claim(
  "human_landscape_global_null",
  global$empirical_p > 0.05,
  paste(
    "Mahalanobis=", round(global$observed_value, 3),
    "p=", round(global$empirical_p, 4)
  )
)
human_features <- c(
  "human_reach_score", "urban_mountain_score",
  "satoyama_interface_score"
)
human <- landscape[landscape$feature %in% human_features, ]
add_claim(
  "human_composites_not_supported",
  nrow(human) == length(human_features) &&
    all(human$maxT_FWER_p > 0.05),
  paste(
    human$feature,
    "difference=", round(human$observed_case_control_difference, 3),
    "FWER=", round(human$maxT_FWER_p, 3),
    collapse = "; "
  )
)
add_claim(
  "dark_convergence_not_supported",
  all(auxiliary$maxT_FWER_p > 0.05) &&
    all(auxiliary_counts$case_count == 0),
  paste(
    auxiliary$feature,
    "difference=", round(auxiliary$observed_case_control_difference, 3),
    "p=", round(auxiliary$directional_or_two_sided_p, 3),
    collapse = "; "
  )
)
add_claim(
  "small_matched_sample_disclosed",
  nrow(pairs) < nrow(candidates),
  paste("matched=", nrow(pairs), "of candidates=", nrow(candidates))
)
add_check(
  "claim_ceiling",
  grepl(
    "not introduction or horticultural provenance evidence",
    metadata_value[["horticultural_claim_ceiling"]], fixed = TRUE
  ),
  metadata_value[["horticultural_claim_ceiling"]]
)
audit <- do.call(rbind, checks)
utils::write.csv(
  audit, file.path(output_dir, "local_isolate_analysis_audit.csv"),
  row.names = FALSE
)
lines <- c(
  paste0(
    "# v20 local white-isolate claim audit: ",
    if (any(audit$status == "FAIL")) "FAIL" else "PASS"
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
reported <- audit[audit$status == "RESULT", , drop = FALSE]
if (any(audit$status == "FAIL")) {
  print(audit[audit$status == "FAIL", ])
  stop("v20 audit failed.", call. = FALSE)
}
if (nrow(reported)) {
  cat(
    "v20 claims reported as RESULT under --baseline ", baseline,
    " (not passes):\n", sep = ""
  )
  print(reported)
}
cat(
  "v20 claim audit: ", sum(audit$status == "PASS"), " passed, ",
  nrow(reported), " reported as RESULT\n", sep = ""
)
