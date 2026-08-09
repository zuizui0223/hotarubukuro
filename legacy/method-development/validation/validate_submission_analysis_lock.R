args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")

output_dir <- hb_arg_value(
  args, "--output", "results/submission_analysis_lock"
)
evidence_path <- file.path(output_dir, "submission_isolate_evidence.csv")
claim_path <- file.path(output_dir, "submission_claim_registry.csv")
summary_path <- file.path(output_dir, "submission_inference_summary.csv")
manifest_path <- file.path(output_dir, "submission_analysis_manifest.csv")
required <- c(evidence_path, claim_path, summary_path, manifest_path)
if (!all(file.exists(required))) {
  stop(
    "Missing submission-lock outputs: ",
    paste(required[!file.exists(required)], collapse = ", "),
    call. = FALSE
  )
}

evidence <- hb_read_csv(evidence_path)
claims <- hb_read_csv(claim_path)
summary <- hb_read_csv(summary_path)
manifest <- hb_read_csv(manifest_path)
value <- setNames(as.character(summary$value), summary$field)

checks <- list()
add_check <- function(check, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check,
    status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

add_check(
  "evidence_rows",
  nrow(evidence) == 5L &&
    all(c(
      "crossfit_primary", "crossfit_same_fold", "joint_full_data_ppc"
    ) %in% evidence$reference) &&
    all(c("candidate_count", "candidate_fraction") %in% evidence$metric),
  paste("rows=", nrow(evidence))
)

primary_fraction <- evidence[
  evidence$reference == "crossfit_primary" &
    evidence$metric == "candidate_fraction", , drop = FALSE
]
same_fold_fraction <- evidence[
  evidence$reference == "crossfit_same_fold" &
    evidence$metric == "candidate_fraction", , drop = FALSE
]
joint_fraction <- evidence[
  evidence$reference == "joint_full_data_ppc" &
    evidence$metric == "candidate_fraction", , drop = FALSE
]
primary_count <- evidence[
  evidence$reference == "crossfit_primary" &
    evidence$metric == "candidate_count", , drop = FALSE
]
joint_count <- evidence[
  evidence$reference == "joint_full_data_ppc" &
    evidence$metric == "candidate_count", , drop = FALSE
]
selected <- list(
  primary_fraction, same_fold_fraction, joint_fraction,
  primary_count, joint_count
)
add_check(
  "one_to_one_selectors",
  all(vapply(selected, nrow, integer(1)) == 1L),
  "five submission evidence selectors"
)

finite_probabilities <- all(vapply(selected, function(x) {
  all(is.finite(x$tail_probability)) &&
    all(x$tail_probability >= 0 & x$tail_probability <= 1) &&
    all(x$mc_lower_95 <= x$tail_probability) &&
    all(x$mc_upper_95 >= x$tail_probability)
}, logical(1)))
add_check(
  "probability_bounds",
  finite_probabilities,
  paste(round(evidence$tail_probability, 6), collapse = ",")
)

expected_status <- if (
  primary_fraction$mc_upper_95 < 0.05 &&
  same_fold_fraction$mc_upper_95 < 0.05 &&
  joint_fraction$mc_upper_95 < 0.05
) {
  "robust_upper_tail_across_predictive_references"
} else if (
  primary_fraction$tail_probability <= 0.10 &&
  joint_fraction$tail_probability > 0.05
) {
  "crossfit_upper_tail_not_jointly_robust"
} else if (primary_fraction$tail_probability <= 0.10) {
  "threshold_adjacent_upper_tail"
} else {
  "compatible_with_natural_model"
}
add_check(
  "status_recalculation",
  identical(value[["submission_status"]], expected_status),
  paste(value[["submission_status"]], "expected", expected_status)
)

add_check(
  "candidate_identity_count",
  identical(as.integer(as.numeric(value[["candidate_count"]])), 18L) &&
    hb_close_enough(
      as.numeric(value[["candidate_fraction"]]),
      primary_fraction$observed_value
    ) &&
    hb_close_enough(
      primary_fraction$observed_value,
      joint_fraction$observed_value
    ),
  paste(
    "n=", value[["candidate_count"]],
    "fraction=", value[["candidate_fraction"]]
  )
)

c5 <- claims[claims$claim_id == "C5_local_isolates", , drop = FALSE]
c7 <- claims[claims$claim_id == "C7_horticultural_origin", , drop = FALSE]
add_check(
  "claim_ceiling",
  nrow(c5) == 1L && nrow(c7) == 1L &&
    identical(c5$status, expected_status) &&
    grepl("joint spatial posterior-predictive", c5$claim, fixed = TRUE) &&
    grepl("not evidence", c5$claim_ceiling, ignore.case = TRUE) &&
    identical(c7$status, "not_demonstrated"),
  if (nrow(c5)) c5$status else "missing C5"
)

add_check(
  "fold_boundary_audit_propagated",
  is.finite(as.numeric(value[["candidate_fold_boundary_crossings"]])) &&
    as.numeric(value[["candidate_fold_boundary_crossings"]]) > 0,
  value[["candidate_fold_boundary_crossings"]]
)

manifest_ok <- all(vapply(seq_len(nrow(manifest)), function(index) {
  path <- as.character(manifest$path[index])
  file.exists(path) &&
    identical(unname(rp_sha256(path)), as.character(manifest$sha256[index]))
}, logical(1)))
add_check(
  "manifest_hashes",
  manifest_ok,
  paste("files=", nrow(manifest))
)

validation <- do.call(rbind, checks)
rp_write_csv_atomic(
  validation,
  file.path(output_dir, "submission_analysis_validation.csv")
)
writeLines(
  c(
    "# Submission analysis-lock validation",
    "",
    paste(sum(validation$status == "PASS"), "of", nrow(validation), "checks passed."),
    "",
    paste0("- ", validation$check, ": ", validation$status, " (", validation$detail, ")")
  ),
  file.path(output_dir, "VALIDATION.md"),
  useBytes = TRUE
)
if (any(validation$status != "PASS")) {
  stop("Submission analysis lock validation failed.", call. = FALSE)
}
cat("Submission analysis lock validation passed: ", expected_status, "\n", sep = "")
