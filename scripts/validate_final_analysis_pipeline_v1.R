args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default = NULL) {
  prefix <- paste0(name, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  sub(prefix, "", hit[1L], fixed = TRUE)
}
output_dir <- arg_value(
  "--output", "results/final_analysis_pipeline"
)
source("scripts/final_analysis_pipeline_v1.R")

read_output <- function(name) {
  utils::read.csv(
    file.path(output_dir, name), check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
close_enough <- function(x, y, tolerance = 1e-9) {
  length(x) == length(y) &&
    all((is.na(x) & is.na(y)) |
          (is.finite(x) & is.finite(y) & abs(x - y) <= tolerance))
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

required <- read_output("final_required_artifacts.csv")
results <- read_output("final_result_registry.csv")
claims <- read_output("final_claim_registry.csv")
exclusions <- read_output("final_exclusion_registry.csv")
checksums <- read_output("final_input_checksums.csv")
metadata <- read_output("final_pipeline_metadata.csv")
stages <- read_output("final_stage_manifest.csv")
metadata_value <- setNames(metadata$value, metadata$field)

add_check(
  "analysis_specification",
  identical(
    metadata_value[["analysis_spec_version"]],
    final_analysis_spec_version
  ),
  metadata_value[["analysis_spec_version"]]
)
add_check(
  "required_artifact_set",
  nrow(required) == nrow(final_required_artifacts()) &&
    all(required$exists) &&
    all(file.exists(required$artifact)),
  paste("artifacts=", nrow(required))
)
current_md5 <- unname(tools::md5sum(checksums$artifact))
add_check(
  "input_checksum_lock",
  identical(current_md5, checksums$md5),
  paste("locked files=", length(current_md5))
)
recomputed_results <- final_result_registry(".")
add_check(
  "result_registry_recalculation",
  identical(results$result_id, recomputed_results$result_id) &&
    close_enough(results$estimate, recomputed_results$estimate) &&
    close_enough(results$raw_p, recomputed_results$raw_p) &&
    close_enough(results$corrected_p, recomputed_results$corrected_p),
  paste("results=", nrow(results))
)
recomputed_claims <- final_claim_registry(recomputed_results)
add_check(
  "claim_registry_recalculation",
  identical(claims$claim_id, recomputed_claims$claim_id) &&
    identical(claims$status, recomputed_claims$status) &&
    identical(claims$claim, recomputed_claims$claim),
  paste("claims=", nrow(claims))
)
add_check(
  "analysis_tier_separation",
  all(c(
    "confirmatory", "planned_local_test",
    "exploratory_human_context",
    "exploratory_human_context_sensitivity"
  ) %in% results$tier),
  paste("tiers=", paste(unique(results$tier), collapse = ","))
)

v17 <- results[
  results$result_id %in%
    c("local_bombus_presence", "local_bombus_intensity"),
  , drop = FALSE
]
add_check(
  "v17_primary_local_pair_family",
  nrow(v17) == 2L &&
    all(v17$estimate > 0) &&
    all(v17$corrected_p < 0.05) &&
    all(
      v17$correction_family ==
        "two primary 25-km hurdle responses"
    ),
  paste(
    "corrected p=",
    paste(round(v17$corrected_p, 4), collapse = ",")
  )
)
national_bombus <- results[
  results$result_id == "national_bombus_auc_gain", , drop = FALSE
]
add_check(
  "national_and_local_bombus_not_conflated",
  nrow(national_bombus) == 1L &&
    abs(national_bombus$estimate) < 0.02 &&
    all(v17$estimate > national_bombus$estimate),
  paste("national mean AUC gain=", round(national_bombus$estimate, 4))
)
isolates <- results[
  results$result_id %in%
    c("local_isolate_count", "local_isolate_fraction"),
  , drop = FALSE
]
add_check(
  "local_isolates_are_candidate_definition",
  all(isolates$raw_p > 0.05) &&
    all(isolates$status == "compatible_with_natural_model"),
  paste("p=", paste(round(isolates$raw_p, 3), collapse = ","))
)
human <- results[
  results$result_id %in%
    c(
      "local_population_5km",
      "local_population_did_alignment",
      "did_proximate_candidate_fraction"
    ),
  , drop = FALSE
]
add_check(
  "human_context_exploratory_ceiling",
  all(human$raw_p < 0.10) &&
    all(human$corrected_p > 0.05) &&
    all(human$status == "suggestive_not_corrected"),
  paste(
    "corrected p=",
    paste(round(human$corrected_p, 3), collapse = ",")
  )
)
add_check(
  "single_joint_followup",
  results$estimate[
    results$result_id == "joint_human_followup_count"
  ] == 1,
  paste(
    "count=",
    results$estimate[
      results$result_id == "joint_human_followup_count"
    ]
  )
)

add_check(
  "residual_exclusion",
  identical(
    metadata_value[["residual_as_primary_response"]], "false"
  ) &&
    any(grepl("residual", exclusions$excluded_item)),
  metadata_value[["residual_as_primary_response"]]
)
add_check(
  "horticultural_claim_exclusion",
  identical(
    metadata_value[["horticultural_origin_claim"]],
    "not demonstrated"
  ) &&
    any(
      exclusions$excluded_item ==
        "horticultural origin or introgression claim"
    ),
  metadata_value[["horticultural_origin_claim"]]
)
add_check(
  "individual_species_effect_exclusion",
  any(
    exclusions$excluded_item ==
      "individual Bombus species coefficients"
  ) &&
    !any(grepl(
      "ardens|diversus|beaticola|consobrinus|honshuensis",
      claims$claim
    )),
  "community fingerprint retained; species coefficients excluded"
)
add_check(
  "pipeline_stage_completion",
  all(stages$status == "PASS"),
  paste(
    "stages=", nrow(stages),
    "failures=", sum(stages$status != "PASS")
  )
)
current_final_md5 <- unname(tools::md5sum("final.Rmd"))
add_check(
  "final_rmd_untouched",
  identical(
    current_final_md5, metadata_value[["final_Rmd_md5"]]
  ) &&
    identical(
      current_final_md5, "0173d76bef175c6319d43196b1406cc8"
    ),
  paste("md5=", current_final_md5)
)

validation <- do.call(rbind, checks)
utils::write.csv(
  validation,
  file.path(output_dir, "final_independent_validation.csv"),
  row.names = FALSE
)
writeLines(
  c(
    "# Final pipeline independent validation",
    "",
    paste(
      sum(validation$status == "PASS"), "of", nrow(validation),
      "checks passed."
    ),
    "",
    paste0(
      "- ", validation$check, ": ", validation$status,
      " (", validation$detail, ")"
    )
  ),
  file.path(output_dir, "VALIDATION.md"),
  useBytes = TRUE
)
if (any(validation$status != "PASS")) {
  print(validation[validation$status != "PASS", , drop = FALSE])
  stop("Final pipeline validation failed.", call. = FALSE)
}
cat("Final pipeline independent validation passed ",
    nrow(validation), " checks.\n")
