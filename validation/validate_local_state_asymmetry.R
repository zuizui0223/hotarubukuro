args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) {
  args[[1L]]
} else {
  "results/ecological_v23_local_state_asymmetry"
}
source("R/local_state_asymmetry.R")

read_output <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing output: ", path)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

rules <- read_output("local_state_asymmetry_rules.csv")
summary <- read_output("local_state_asymmetry_summary.csv")
null <- read_output("local_state_asymmetry_null.csv")
candidates <- read_output("local_state_asymmetry_candidates.csv")
metadata <- read_output("local_state_asymmetry_metadata.csv")
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

expected_rules <- v23_state_rule_table()$state_rule
expected_metrics <- c(
  "pigmented_in_white_count", "white_in_pigmented_count",
  "pigmented_in_white_rate", "white_in_pigmented_rate",
  "count_difference", "rate_difference", "log_rate_ratio"
)
expected_directions <- c("pigmented_in_white", "white_in_pigmented")

add_check(
  "state_rules_complete",
  identical(sort(rules$state_rule), sort(expected_rules)),
  paste("rules=", paste(rules$state_rule, collapse = ","))
)
add_check(
  "summary_grid_complete",
  nrow(summary) == length(expected_rules) * length(expected_metrics) &&
    all(table(summary$state_rule) == length(expected_metrics)) &&
    all(expected_metrics %in% summary$metric),
  paste("rows=", nrow(summary))
)
n_natural_maps <- unique(as.integer(summary$n_natural_maps))
add_check(
  "natural_maps_complete",
  length(n_natural_maps) == 1L && n_natural_maps > 0L &&
    all(table(null$state_rule) == n_natural_maps),
  paste(
    "natural maps=", paste(n_natural_maps, collapse = ","),
    "null rows=", nrow(null)
  )
)
add_check(
  "candidate_directions_valid",
  all(candidates$direction %in% expected_directions) &&
    all(c("pigmented_in_white_count", "white_in_pigmented_count") %in%
          summary$metric),
  paste(
    "observed directions with at least one candidate=",
    paste(sort(unique(candidates$direction)), collapse = ",")
  )
)
add_check(
  "candidate_ids_unique_within_direction",
  !anyDuplicated(paste(
    candidates$state_rule, candidates$direction,
    candidates$exact_site_id, sep = "::"
  )),
  paste("candidate rows=", nrow(candidates))
)

recomputed_rows <- list()
for (state_rule in expected_rules) {
  observed <- summary[
    summary$state_rule == state_rule, , drop = FALSE
  ]
  simulated <- null[null$state_rule == state_rule, , drop = FALSE]
  for (metric in expected_metrics) {
    row <- observed[observed$metric == metric, , drop = FALSE]
    comparison <- v23_null_comparison(
      row$observed_value, simulated[[metric]]
    )
    recomputed_rows[[length(recomputed_rows) + 1L]] <- data.frame(
      state_rule = state_rule,
      metric = metric,
      upper_p = comparison[["upper_p"]],
      lower_p = comparison[["lower_p"]],
      two_sided_p = comparison[["two_sided_p"]],
      null_mean = comparison[["null_mean"]],
      stringsAsFactors = FALSE
    )
  }
}
recomputed <- do.call(rbind, recomputed_rows)
index <- match(
  paste(summary$state_rule, summary$metric),
  paste(recomputed$state_rule, recomputed$metric)
)
tolerance <- 1e-10
add_check(
  "summary_statistics_reproducible",
  all(abs(summary$upper_p - recomputed$upper_p[index]) <= tolerance) &&
    all(abs(summary$lower_p - recomputed$lower_p[index]) <= tolerance) &&
    all(abs(summary$two_sided_p - recomputed$two_sided_p[index]) <= tolerance) &&
    all(abs(summary$null_mean - recomputed$null_mean[index]) <= tolerance),
  paste("comparisons=", nrow(summary))
)
add_check(
  "post_hoc_label",
  all(summary$analysis_status == "post_hoc_diagnostic") &&
    grepl("posthoc", metadata_value[["analysis_spec_version"]], fixed = TRUE) &&
    grepl("not pre-specified", metadata_value[["analysis_status"]], fixed = TRUE),
  metadata_value[["analysis_status"]]
)
add_check(
  "claim_ceiling",
  grepl("not a mechanism", metadata_value[["claim_ceiling"]], fixed = TRUE) &&
    grepl("evolutionary direction", metadata_value[["claim_ceiling"]], fixed = TRUE),
  metadata_value[["claim_ceiling"]]
)

validation <- do.call(rbind, checks)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  validation,
  file.path(output_dir, "local_state_asymmetry_validation.csv"),
  row.names = FALSE
)
lines <- c(
  paste0(
    "# v23 local colour-state asymmetry validation: ",
    if (all(validation$status == "PASS")) "PASS" else "FAIL"
  ),
  "",
  vapply(seq_len(nrow(validation)), function(index) {
    paste0(
      "- **", validation$status[index], " - ",
      validation$check[index], "**: ", validation$detail[index]
    )
  }, character(1))
)
writeLines(
  lines, file.path(output_dir, "VALIDATION.md"), useBytes = TRUE
)
if (any(validation$status != "PASS")) {
  print(validation[validation$status != "PASS", ])
  stop("v23 local state-asymmetry validation failed.")
}
cat("v23 local state-asymmetry validation passed: ",
    nrow(validation), " checks\n", sep = "")
