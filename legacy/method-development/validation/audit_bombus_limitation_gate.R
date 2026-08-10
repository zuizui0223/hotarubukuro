args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
output_dir <- hb_arg_value(
  args, "--output", "results/ecological_v17_bombus_limitation_gate"
)
read_output <- function(name) hb_read_csv(file.path(output_dir, name))
summary <- read_output("bombus_limitation_gate_summary.csv")
pairs <- read_output("bombus_limitation_gate_pairs.csv")
metadata <- read_output("bombus_limitation_gate_metadata.csv")
validation <- read_output("bombus_limitation_gate_independent_validation.csv")
interpretation <- read_output("interpretation_summary.csv")
meta <- setNames(metadata$value, metadata$field)

checks <- list()
add <- function(check, pass, evidence, severity = "critical") {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check,
    status = if (isTRUE(pass)) "PASS" else "FAIL",
    severity = severity, evidence = evidence,
    stringsAsFactors = FALSE
  )
}

add("analysis_version",
    identical(meta[["analysis_spec_version"]], "v17.2_bombus_limitation_gate"),
    meta[["analysis_spec_version"]])
add("primary_lower_third_gate",
    abs(as.numeric(meta[["primary_low_threshold"]]) - 0.33) < 1e-12,
    meta[["primary_low_threshold"]])
add("available_median_gate",
    abs(as.numeric(meta[["available_threshold"]]) - 0.50) < 1e-12,
    meta[["available_threshold"]])
add("fixed_threshold_grid",
    identical(meta[["sensitivity_low_thresholds"]], "0.1,0.2,0.25,0.33"),
    meta[["sensitivity_low_thresholds"]])
add("one_to_one_matching",
    identical(meta[["pair_reuse"]], "one-to-one"), meta[["pair_reuse"]])
add("flower_blind_orientation",
    grepl("flower colour is never used", meta[["pair_orientation"]], fixed = TRUE),
    meta[["pair_orientation"]])
add("no_second_local_environment_model",
    grepl("none", meta[["local_environment_model"]], fixed = TRUE),
    meta[["local_environment_model"]])
add("no_second_local_spatial_model",
    grepl("none", meta[["local_spatial_model"]], fixed = TRUE),
    meta[["local_spatial_model"]])
add("fixed_sdm_ceiling",
    grepl("not propagated", meta[["bombus_uncertainty"]], fixed = TRUE),
    meta[["bombus_uncertainty"]])
add("post_development_gate_history",
    any(grepl("after exploratory design development", interpretation$value, fixed = TRUE)),
    "gate selection history retained")
add("primary_presence_direction",
    with(summary[summary$is_primary_gate & summary$response == "pigmentation_share", , drop = FALSE],
         nrow(summary[summary$is_primary_gate & summary$response == "pigmentation_share", , drop = FALSE]) == 1L &&
           observed_directed_difference > 0),
    "available-minus-limited pigmentation is positive")
add("intensity_not_overclaimed",
    with(summary[summary$is_primary_gate & summary$response == "pigmented_only_intensity", , drop = FALSE],
         nrow(summary[summary$is_primary_gate & summary$response == "pigmented_only_intensity", , drop = FALSE]) == 1L &&
           !(is.finite(upper_tail_p) && upper_tail_p < 0.05)),
    "conditional intensity is not a supported gate response")
add("independent_validation",
    nrow(validation) >= 10L && !any(validation$status == "FAIL"),
    paste("checks=", nrow(validation), "fails=", sum(validation$status == "FAIL")))
add("pairs_span_multiple_folds",
    length(unique(pairs$fold_i[pairs$is_primary_gate])) >= 4L,
    paste("primary folds=", paste(sort(unique(pairs$fold_i[pairs$is_primary_gate])), collapse = ",")),
    severity = "medium")

audit <- do.call(rbind, checks)
write.csv(audit, file.path(output_dir, "bombus_limitation_gate_audit.csv"), row.names = FALSE)
overall <- if (any(audit$status == "FAIL")) "NEEDS_REVISION" else "PASS"
writeLines(
  c(
    paste0("# Bombus limitation-gate audit: ", overall), "",
    vapply(seq_len(nrow(audit)), function(i) paste0(
      "- **", audit$status[i], " - ", audit$check[i], "**: ", audit$evidence[i]
    ), character(1))
  ),
  file.path(output_dir, "AUDIT.md"), useBytes = TRUE
)
print(audit)
if (any(audit$status == "FAIL")) quit(status = 1L)
