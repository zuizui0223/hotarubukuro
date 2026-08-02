# Independent validation of the v23 local colour-state asymmetry outputs.
#
# The checks here re-derive every reported number from the per-map null table
# rather than trusting the runner, confirm the locked configuration and the
# post hoc claim ceiling, and confirm the required output schema. A failure
# here fails the workflow; no check may be relaxed to obtain a green run.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) && !startsWith(args[[1L]], "--")) {
  args[[1L]]
} else {
  "results/ecological_v23_local_state_asymmetry"
}
expected_maps_argument <- args[startsWith(args, "--required-maps=")]
expected_maps <- if (length(expected_maps_argument)) {
  as.integer(sub("--required-maps=", "", expected_maps_argument[[1L]], fixed = TRUE))
} else {
  1000L
}

source("R/local_state_asymmetry.R")

read_output <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing output: ", path, call. = FALSE)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

rules <- read_output("local_state_asymmetry_rules.csv")
summary <- read_output("local_state_asymmetry_summary.csv")
null <- read_output("local_state_asymmetry_null.csv")
candidates <- read_output("local_state_asymmetry_candidates.csv")
metadata <- read_output("local_state_asymmetry_metadata.csv")
metadata_value <- stats::setNames(metadata$value, metadata$field)

checks <- list()
add_check <- function(check, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check,
    status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = as.character(detail),
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

# The three predeclared definitions must partition, never overlap: a cell can
# never be both white and pigmented under the same rule.
overlap_free <- all(rules$white_max_share < rules$pigmented_min_share)
add_check(
  "state_definitions_do_not_overlap",
  overlap_free,
  paste0(
    "white_max_share=", paste(rules$white_max_share, collapse = ","),
    "; pigmented_min_share=", paste(signif(rules$pigmented_min_share, 4), collapse = ",")
  )
)

add_check(
  "summary_grid_complete",
  nrow(summary) == length(expected_rules) * length(expected_metrics) &&
    all(table(summary$state_rule) == length(expected_metrics)) &&
    all(expected_metrics %in% summary$metric),
  paste("rows=", nrow(summary))
)

required_summary_columns <- c(
  "analysis_spec_version", "analysis_status", "state_rule", "metric",
  "observed_value", "null_mean", "null_sd", "lower_95", "upper_95",
  "upper_p", "lower_p", "two_sided_p", "percentile", "n_natural_maps"
)
add_check(
  "summary_schema",
  all(required_summary_columns %in% names(summary)),
  paste(
    "missing=",
    paste(setdiff(required_summary_columns, names(summary)), collapse = ",")
  )
)

required_null_columns <- c(
  "draw", "pigmented_in_white_count", "white_in_pigmented_count",
  "eligible_pigmented_focals", "eligible_white_focals",
  "pigmented_in_white_rate", "white_in_pigmented_rate",
  "count_difference", "rate_difference", "log_rate_ratio",
  "state_rule", "pseudocount"
)
add_check(
  "null_schema",
  all(required_null_columns %in% names(null)),
  paste(
    "missing=",
    paste(setdiff(required_null_columns, names(null)), collapse = ",")
  )
)

n_natural_maps <- unique(as.integer(summary$n_natural_maps))
add_check(
  "exactly_required_natural_maps",
  length(n_natural_maps) == 1L && n_natural_maps == expected_maps &&
    all(table(null$state_rule) == expected_maps),
  paste0(
    "required=", expected_maps, "; reported=",
    paste(n_natural_maps, collapse = ","), "; null rows per rule=",
    paste(unique(as.integer(table(null$state_rule))), collapse = ",")
  )
)

add_check(
  "null_draw_indices_complete",
  all(vapply(split(null$draw, null$state_rule), function(draws) {
    identical(sort(as.integer(draws)), seq_len(expected_maps))
  }, logical(1))),
  paste("state rules checked=", length(unique(null$state_rule)))
)

# Opportunity normalization: every reported rate must be the pseudocount-
# adjusted event count divided by the number of eligible focal cells in the
# corresponding colour state.
pseudocount <- unique(null$pseudocount)
rate_ok <- length(pseudocount) == 1L && is.finite(pseudocount) &&
  max(abs(
    null$pigmented_in_white_rate -
      (null$pigmented_in_white_count + pseudocount) /
        (null$eligible_pigmented_focals + 2 * pseudocount)
  )) <= 1e-12 &&
  max(abs(
    null$white_in_pigmented_rate -
      (null$white_in_pigmented_count + pseudocount) /
        (null$eligible_white_focals + 2 * pseudocount)
  )) <= 1e-12 &&
  max(abs(
    null$log_rate_ratio -
      log(null$pigmented_in_white_rate / null$white_in_pigmented_rate)
  )) <= 1e-12
add_check(
  "opportunity_normalization_consistent",
  rate_ok,
  paste0("pseudocount=", paste(pseudocount, collapse = ","))
)

add_check(
  "eligible_focals_positive",
  all(null$eligible_pigmented_focals >= 0) &&
    all(null$eligible_white_focals >= 0) &&
    all(null$pigmented_in_white_count <= null$eligible_pigmented_focals) &&
    all(null$white_in_pigmented_count <= null$eligible_white_focals),
  paste0(
    "max pigmented-in-white count=", max(null$pigmented_in_white_count),
    "; max white-in-pigmented count=", max(null$white_in_pigmented_count)
  )
)

add_check(
  "candidate_directions_valid",
  all(candidates$direction %in% expected_directions),
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

# The observed candidate list must reproduce the observed event counts.
candidate_counts_match <- all(vapply(expected_rules, function(state_rule) {
  observed_rows <- summary[summary$state_rule == state_rule, , drop = FALSE]
  all(vapply(expected_directions, function(direction) {
    metric <- paste0(direction, "_count")
    reported <- observed_rows$observed_value[observed_rows$metric == metric]
    listed <- sum(
      candidates$state_rule == state_rule & candidates$direction == direction
    )
    length(reported) == 1L &&
      isTRUE(all.equal(as.numeric(reported), as.numeric(listed)))
  }, logical(1)))
}, logical(1)))
add_check(
  "candidate_list_matches_observed_counts",
  candidate_counts_match,
  paste("rules checked=", length(expected_rules))
)

# Recompute every reported Monte Carlo statistic from the per-map null table.
recomputed_rows <- list()
for (state_rule in expected_rules) {
  observed <- summary[summary$state_rule == state_rule, , drop = FALSE]
  simulated <- null[null$state_rule == state_rule, , drop = FALSE]
  for (metric in expected_metrics) {
    row <- observed[observed$metric == metric, , drop = FALSE]
    comparison <- v23_null_comparison(row$observed_value, simulated[[metric]])
    recomputed_rows[[length(recomputed_rows) + 1L]] <- data.frame(
      state_rule = state_rule,
      metric = metric,
      null_mean = comparison[["null_mean"]],
      null_sd = comparison[["null_sd"]],
      lower_95 = comparison[["lower_95"]],
      upper_95 = comparison[["upper_95"]],
      upper_p = comparison[["upper_p"]],
      lower_p = comparison[["lower_p"]],
      two_sided_p = comparison[["two_sided_p"]],
      percentile = comparison[["percentile"]],
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
comparable <- c(
  "null_mean", "null_sd", "lower_95", "upper_95",
  "upper_p", "lower_p", "two_sided_p", "percentile"
)
deviations <- vapply(comparable, function(column) {
  max(abs(summary[[column]] - recomputed[[column]][index]), na.rm = TRUE)
}, numeric(1))
add_check(
  "summary_statistics_reproducible",
  all(deviations <= tolerance | !is.finite(deviations)),
  paste0("maximum deviation=", signif(max(deviations[is.finite(deviations)]), 3))
)

# The finite-simulation correction must be the (1 + as-extreme) / (maps + 1)
# form rather than a plain proportion.
finite_correction_ok <- all(vapply(seq_len(nrow(summary)), function(row) {
  simulated <- null[null$state_rule == summary$state_rule[[row]], , drop = FALSE]
  values <- simulated[[summary$metric[[row]]]]
  values <- values[is.finite(values)]
  if (!length(values) || !is.finite(summary$observed_value[[row]])) return(TRUE)
  expected_upper <- (1 + sum(values >= summary$observed_value[[row]])) /
    (length(values) + 1)
  abs(summary$upper_p[[row]] - expected_upper) <= tolerance
}, logical(1)))
add_check(
  "finite_simulation_correction",
  finite_correction_ok,
  "p = (1 + number at least as extreme) / (usable maps + 1)"
)

# Locked configuration provenance.
locked_fields <- c(
  "locked_configuration_id", "locked_radius_km", "locked_environment_caliper",
  "locked_minimum_neighbours", "locked_same_fold_only"
)
add_check(
  "locked_configuration_recorded",
  all(locked_fields %in% names(metadata_value)) &&
    all(nzchar(metadata_value[locked_fields])),
  paste0(
    "configuration=", metadata_value[["locked_configuration_id"]],
    "; radius_km=", metadata_value[["locked_radius_km"]],
    "; caliper=", metadata_value[["locked_environment_caliper"]],
    "; minimum_neighbours=", metadata_value[["locked_minimum_neighbours"]],
    "; same_fold_only=", metadata_value[["locked_same_fold_only"]]
  )
)

configurations_path <- "R/local_pigmented_isolates.R"
locked_matches_source <- FALSE
if (file.exists(configurations_path)) {
  source("R/candidate_null_tools.R")
  source(configurations_path)
  primary <- v20_configuration_table()
  primary <- primary[primary$role == "primary", , drop = FALSE]
  locked_matches_source <- nrow(primary) == 1L &&
    identical(as.character(primary$configuration), metadata_value[["locked_configuration_id"]]) &&
    identical(as.character(primary$radius_km), metadata_value[["locked_radius_km"]]) &&
    identical(as.character(primary$environment_caliper), metadata_value[["locked_environment_caliper"]]) &&
    identical(as.character(primary$minimum_neighbours), metadata_value[["locked_minimum_neighbours"]]) &&
    identical(as.character(primary$same_fold_only), metadata_value[["locked_same_fold_only"]])
}
add_check(
  "locked_configuration_matches_source",
  locked_matches_source,
  "metadata reproduces the primary row of v20_configuration_table()"
)

add_check(
  "input_checksums_recorded",
  all(c("cells_sha256", "presence_checkpoint_sha256") %in% names(metadata_value)) &&
    all(nchar(metadata_value[c("cells_sha256", "presence_checkpoint_sha256")]) == 64L),
  paste0(
    "cells=", substr(metadata_value[["cells_sha256"]], 1, 16),
    "...; checkpoint=", substr(metadata_value[["presence_checkpoint_sha256"]], 1, 16), "..."
  )
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
  paste0("Validated output directory: `", output_dir, "`"),
  paste0("Required natural maps: ", expected_maps),
  "",
  vapply(seq_len(nrow(validation)), function(index) {
    paste0(
      "- **", validation$status[index], " - ",
      validation$check[index], "**: ", validation$detail[index]
    )
  }, character(1)),
  "",
  "## Claim ceiling",
  "",
  "This is a post hoc symmetry diagnostic. An extreme value supports only the",
  "statement that local colour-state discordance was directionally asymmetric",
  "relative to the fitted natural environmental and spatial baseline. It does",
  "not establish a mutation rate, a white-to-pigmented evolutionary transition,",
  "developmental directionality, dispersal direction, gene flow, horticultural",
  "introduction, pollinator-mediated selection, or adaptive evolution."
)
writeLines(lines, file.path(output_dir, "VALIDATION.md"), useBytes = TRUE)
if (any(validation$status != "PASS")) {
  print(validation[validation$status != "PASS", ], row.names = FALSE)
  stop("v23 local state-asymmetry validation failed.", call. = FALSE)
}
cat("v23 local state-asymmetry validation passed: ",
    nrow(validation), " checks\n", sep = "")
