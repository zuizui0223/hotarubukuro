args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
hb_load_modules("final_registry")
output_dir <- hb_arg_value(args, "--output", "results/final_analysis_pipeline")
read_output <- function(name) hb_read_csv(file.path(output_dir, name))
read_baseline <- function() {
  explicit <- grep("^--baseline=", args, value = TRUE)
  if (length(explicit)) return(sub("^--baseline=", "", explicit[[1L]]))
  marker <- file.path(output_dir, "run_mode.txt")
  if (!file.exists(marker)) return("reconstruction")
  lines <- readLines(marker, warn = FALSE)
  value <- sub("^baseline=", "", grep("^baseline=", lines, value = TRUE))
  if (length(value) != 1L) stop("run_mode.txt must contain exactly one baseline= entry.", call. = FALSE)
  value
}
baseline_declared <- read_baseline()
baseline <- if (identical(baseline_declared, "analysis_1909")) "reconstruction" else baseline_declared
if (!baseline %in% c("published", "reconstruction")) stop("Unknown final-validation baseline: ", baseline_declared, call. = FALSE)

checks <- list()
add_check <- function(check, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check, status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = detail, stringsAsFactors = FALSE
  )
}
valid_probability <- function(x) length(x) > 0L && all(is.finite(x) & x >= 0 & x <= 1)
natural_status <- function(p) ifelse(p <= 0.05, "upper_tail_against_natural_model", "compatible_with_natural_model")
exploratory_status <- function(raw_p, corrected_p) {
  ifelse(corrected_p <= 0.05, "familywise_supported_direction",
         ifelse(raw_p < 0.10, "suggestive_not_corrected", "not_supported"))
}

required <- read_output("final_required_artifacts.csv")
results <- read_output("final_result_registry.csv")
claims <- read_output("final_claim_registry.csv")
exclusions <- read_output("final_exclusion_registry.csv")
checksums <- read_output("final_input_checksums.csv")
metadata <- read_output("final_pipeline_metadata.csv")
stages <- read_output("final_stage_manifest.csv")
stage_registry <- read_output("publication_stage_registry.csv")
metadata_value <- setNames(metadata$value, metadata$field)

add_check("declared_baseline", baseline %in% c("published", "reconstruction"), paste0(baseline_declared, " -> ", baseline))
add_check("analysis_specification", identical(metadata_value[["analysis_spec_version"]], final_analysis_spec_version), metadata_value[["analysis_spec_version"]])
add_check(
  "required_artifact_set",
  nrow(required) == nrow(final_required_artifacts()) && all(required$exists) && all(file.exists(required$artifact)),
  paste("artifacts=", nrow(required))
)
current_md5 <- unname(tools::md5sum(checksums$artifact))
add_check("input_checksum_lock", identical(current_md5, checksums$md5), paste("locked files=", length(current_md5)))
recomputed_results <- final_result_registry(".")
add_check(
  "result_registry_recalculation",
  identical(results$result_id, recomputed_results$result_id) &&
    hb_close_enough(results$estimate, recomputed_results$estimate) &&
    hb_close_enough(results$raw_p, recomputed_results$raw_p) &&
    hb_close_enough(results$corrected_p, recomputed_results$corrected_p) &&
    identical(results$status, recomputed_results$status),
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
    "confirmatory", "local_mechanistic_sensitivity",
    "exploratory_human_context", "exploratory_human_context_sensitivity"
  ) %in% results$tier),
  paste("tiers=", paste(unique(results$tier), collapse = ","))
)

gate <- results[
  results$result_id %in% c(
    "local_bombus_limitation_presence", "local_bombus_limitation_intensity"
  ), , drop = FALSE
]
gate_presence <- gate[gate$result_id == "local_bombus_limitation_presence", , drop = FALSE]
gate_intensity <- gate[gate$result_id == "local_bombus_limitation_intensity", , drop = FALSE]
local_bombus_ok <- nrow(gate) == 2L &&
  gate_presence$estimate > 0 &&
  valid_probability(gate$raw_p) && valid_probability(gate$corrected_p) &&
  identical(gate_presence$correction_family,
            "retained 0.10/0.20/0.25/0.33 gate grid across both response stages") &&
  identical(gate_presence$status,
            if (gate_presence$raw_p < 0.05 && gate_presence$corrected_p >= 0.05)
              "directional_exploratory_support_not_gridwise_significant"
            else gate_presence$status) &&
  !(gate_intensity$estimate > 0 && gate_intensity$raw_p < 0.05)
add_check(
  "local_bombus_limitation_family",
  local_bombus_ok,
  paste(
    "presence estimate=", round(gate_presence$estimate, 3),
    "raw p=", round(gate_presence$raw_p, 4),
    "grid q=", round(gate_presence$corrected_p, 4),
    "; intensity estimate=", round(gate_intensity$estimate, 3),
    "p=", round(gate_intensity$raw_p, 4)
  )
)
add_check(
  "local_bombus_claim_ceiling",
  any(grepl("after exploratory design development",
            claims$claim_ceiling[claims$claim_id == "C4_local_bombus_limitation"], fixed = TRUE)) &&
    any(grepl("not abundance",
              claims$claim_ceiling[claims$claim_id == "C4_local_bombus_limitation"], fixed = TRUE)),
  claims$claim_ceiling[claims$claim_id == "C4_local_bombus_limitation"]
)
national_bombus <- results[results$result_id == "national_bombus_auc_gain", , drop = FALSE]
add_check(
  "national_and_local_bombus_not_conflated",
  nrow(national_bombus) == 1L && is.finite(national_bombus$estimate) &&
    grepl("limitation", metadata_value[["local_mechanism_test"]], ignore.case = TRUE),
  paste("national mean AUC gain=", round(national_bombus$estimate, 4))
)

isolates <- results[results$result_id %in% c("local_isolate_count", "local_isolate_fraction"), , drop = FALSE]
expected_isolate_status <- natural_status(isolates$raw_p)
isolate_ok <- nrow(isolates) == 2L && valid_probability(isolates$raw_p) &&
  identical(as.character(isolates$status), as.character(expected_isolate_status))
add_check("local_isolates_are_candidate_definition", isolate_ok,
          paste("p=", paste(round(isolates$raw_p, 3), collapse = ","),
                "status=", paste(isolates$status, collapse = ",")))

human <- results[results$result_id %in% c(
  "local_population_5km", "local_population_did_alignment",
  "did_proximate_candidate_fraction"
), , drop = FALSE]
expected_human_status <- exploratory_status(human$raw_p, human$corrected_p)
human_ok <- nrow(human) == 3L && valid_probability(human$raw_p) &&
  valid_probability(human$corrected_p) &&
  identical(as.character(human$status), as.character(expected_human_status))
add_check("human_context_exploratory_ceiling", human_ok,
          paste("corrected p=", paste(round(human$corrected_p, 3), collapse = ",")))

joint_count <- results$estimate[results$result_id == "joint_human_followup_count"]
add_check("joint_followup_count",
          length(joint_count) == 1L && is.finite(joint_count) && joint_count >= 0 && joint_count == round(joint_count),
          paste("count=", joint_count))
add_check("residual_exclusion",
          identical(metadata_value[["residual_as_primary_response"]], "false") &&
            any(grepl("residual", exclusions$excluded_item)),
          metadata_value[["residual_as_primary_response"]])
add_check("horticultural_claim_exclusion",
          identical(metadata_value[["horticultural_origin_claim"]], "not demonstrated") &&
            any(exclusions$excluded_item == "horticultural origin or introgression claim"),
          metadata_value[["horticultural_origin_claim"]])
add_check("species_causal_effect_exclusion",
          any(grepl("individual Bombus species causal coefficients", exclusions$excluded_item, fixed = TRUE)) &&
            !any(grepl("ardens|diversus|beaticola|consobrinus|honshuensis", claims$claim)),
          "species-specific causal coefficients excluded")
add_check("turnover_not_active_claim",
          any(grepl("unsigned Bombus community-turnover", exclusions$excluded_item, fixed = TRUE)) &&
            !any(grepl("turnover", claims$claim[claims$claim_id == "C4_local_bombus_limitation"], ignore.case = TRUE)),
          "turnover retained only as sensitivity")
add_check("pipeline_stage_completion", all(stages$status == "PASS"),
          paste("stages=", nrow(stages), "failures=", sum(stages$status != "PASS")))
add_check(
  "publication_stage_registry",
  identical(stage_registry$stage_id, hb_publication_stage_registry()$stage_id) &&
    identical(stage_registry$manuscript_role, hb_publication_stage_registry()$manuscript_role),
  paste("paper stages=", nrow(stage_registry))
)
add_check(
  "repository_interface",
  all(file.exists(c("R/pipeline_support.R", "R/final_registry.R",
                    "scripts/run_publication_pipeline.R", "validation"))) && !file.exists("final.Rmd"),
  "R modules, scripts, and validation separated; legacy Rmd removed"
)

validation <- do.call(rbind, checks)
utils::write.csv(validation, file.path(output_dir, "final_independent_validation.csv"), row.names = FALSE)
writeLines(c(
  "# Final pipeline independent validation", "",
  paste(sum(validation$status == "PASS"), "of", nrow(validation),
        "checks passed under the", baseline_declared, "baseline."), "",
  paste0("- ", validation$check, ": ", validation$status, " (", validation$detail, ")")
), file.path(output_dir, "VALIDATION.md"), useBytes = TRUE)
if (any(validation$status != "PASS")) {
  print(validation[validation$status != "PASS", , drop = FALSE])
  stop("Final pipeline validation failed.", call. = FALSE)
}
cat("Final pipeline independent validation passed ", nrow(validation),
    " checks under the ", baseline_declared, " baseline.\n", sep = "")
