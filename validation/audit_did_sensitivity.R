args <- commandArgs(trailingOnly = TRUE)
positional <- args[!startsWith(args, "--")]
output_dir <- if (length(positional)) {
  positional[[1L]]
} else {
  "results/ecological_v22_did_human_context"
}
read_output <- function(name) {
  utils::read.csv(
    file.path(output_dir, name), check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
checks <- list()
add_check <- function(claim, passed, evidence) {
  checks[[length(checks) + 1L]] <<- data.frame(
    claim = claim,
    status = if (isTRUE(passed)) "PASS" else "FAIL",
    evidence = evidence,
    stringsAsFactors = FALSE
  )
}

# See validation/audit_phenotype.R. The assertions below that name a published
# value -- a candidate count, a specific cell -- are statements about the
# published run, not about the analysis being correct. Under
# --baseline reconstruction they are reported as not_applicable carrying the
# observed value; under --baseline published they are enforced unchanged.
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
add_not_applicable <- function(name, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    claim = name, status = "not_applicable",
    evidence = as.character(detail), stringsAsFactors = FALSE
  )
}
add_published_finding <- function(name, passed, detail) {
  if (identical(baseline, "published")) {
    add_check(name, passed, detail)
  } else {
    add_not_applicable(name, paste0(
      detail,
      ";reason=the reconstruction defines its own analysis population"
    ))
  }
}

# Inferential claims are reported as RESULT under --baseline reconstruction and
# enforced under --baseline published; see validation/audit_local_pigmented_isolates.R
# for the reasoning. Code and metadata properties stay PASS/FAIL in both modes.
add_claim <- function(name, passed, detail) {
  if (identical(baseline, "published")) {
    add_check(name, passed, detail)
  } else {
    checks[[length(checks) + 1L]] <<- data.frame(
      claim = name,
      status = "RESULT",
      evidence = paste0(
        detail, "; published-mode verdict would be ",
        if (isTRUE(passed)) "PASS" else "FAIL"
      ),
      stringsAsFactors = FALSE
    )
  }
}

summary <- read_output("did_contrast_summary.csv")
convergence <- read_output("did_convergence_summary.csv")
composition <- read_output("did_context_composition_summary.csv")
candidates <- read_output("did_candidate_details.csv")
context <- read_output("did_cell_context.csv")
metadata <- read_output("did_metadata.csv")
validation <- read_output("did_independent_validation.csv")
collinearity <- read_output("did_collinearity.csv")
metadata_value <- setNames(metadata$value, metadata$field)
quality <- utils::read.csv(
  paste0(
    "results/ecological_v21_local_human_neighbourhood/",
    "human_neighbourhood_quality_summary.csv"
  ),
  check.names = FALSE, stringsAsFactors = FALSE
)

aligned <- summary[
  summary$feature == "did_aligned_population_score", , drop = FALSE
]
proximity <- summary[
  summary$feature == "did_proximity_rank", , drop = FALSE
]
did_spike <- convergence[
  convergence$spike_feature == "did_proximity_rank" &
    convergence$metric == "candidate_human_spike_count",
  , drop = FALSE
]
urban_class <- composition[
  composition$human_context_class ==
    "did_proximate_high_population",
  , drop = FALSE
]
outside_class <- composition[
  composition$human_context_class == "populated_beyond_did",
  , drop = FALSE
]
joint <- candidates[candidates$joint_q10_did_proximity_spike, ]

add_published_finding(
  "Fixed local-isolate population",
  nrow(candidates) == 16L,
  paste("v20/v21 candidates retained:", nrow(candidates))
)
add_check(
  "DID and population were post-selection features",
  grepl(
    "do not select", metadata_value[["candidate_selector"]],
    fixed = TRUE
  ),
  metadata_value[["candidate_selector"]]
)
add_claim(
  "Population-DID alignment is directional but not corrected-significant",
  aligned$directional_or_two_sided_p < 0.05 &&
    aligned$maxT_FWER_p >= 0.05,
  sprintf(
    "raw p=%.4f; maxT FWER p=%.4f",
    aligned$directional_or_two_sided_p, aligned$maxT_FWER_p
  )
)
add_claim(
  "DID proximity alone is weaker",
  proximity$directional_or_two_sided_p >= 0.05 &&
    proximity$maxT_FWER_p >= 0.05,
  sprintf(
    "raw p=%.4f; maxT FWER p=%.4f",
    proximity$directional_or_two_sided_p,
    proximity$maxT_FWER_p
  )
)
aligned_input_rho <- max(abs(collinearity$spearman_rho[
  (collinearity$feature_1 == "did_aligned_population_score" &
     collinearity$feature_2 %in%
       c("did_proximity_rank", "population_5km_rank")) |
    (collinearity$feature_2 == "did_aligned_population_score" &
       collinearity$feature_1 %in%
         c("did_proximity_rank", "population_5km_rank"))
]))
add_claim(
  "Population-DID composite is not an independent effect",
  aligned_input_rho > 0.95,
  sprintf(
    "maximum Spearman rho with its two inputs = %.3f",
    aligned_input_rho
  )
)
add_claim(
  "DID-proximity local spikes exceed natural-map expectation only raw",
  did_spike$empirical_p < 0.05 &&
    did_spike$maxT_FWER_p >= 0.05,
  sprintf(
    "observed=%d; null mean=%.3f; raw p=%.4f; FWER p=%.4f",
    did_spike$observed_value, did_spike$null_mean,
    did_spike$empirical_p, did_spike$maxT_FWER_p
  )
)
add_claim(
  "Urban-context enrichment remains sensitivity-level",
  urban_class$observed_candidate_fraction >
    urban_class$null_mean_fraction &&
    urban_class$maxT_FWER_p >= 0.05,
  sprintf(
    "observed=%d/%d (%.3f); null fraction=%.3f; FWER p=%.4f",
    urban_class$observed_candidate_count, nrow(candidates),
    urban_class$observed_candidate_fraction,
    urban_class$null_mean_fraction,
    urban_class$maxT_FWER_p
  )
)
add_claim(
  "DID-outside populated class is too sparse for inference",
  sum(
    context$human_context_class == "populated_beyond_did"
  ) <= 1L &&
    outside_class$observed_candidate_count == 0L,
  paste(
    "all supported cells=",
    sum(context$human_context_class == "populated_beyond_did"),
    "; observed candidates=", outside_class$observed_candidate_count
  )
)
add_published_finding(
  "Only one q10 plus DID-spike follow-up candidate",
  nrow(joint) == 1L &&
    joint$exact_site_id == "cell-1km--108_-147",
  paste(
    "n=", nrow(joint),
    if (nrow(joint)) paste("cell=", joint$exact_site_id) else ""
  )
)
add_published_finding(
  # The early-flowering half of this claim has been withdrawn with the
  # phenology component; the dark-tail half is unchanged.
  "Convergent candidate lacks a dark predictive tail",
  nrow(joint) == 1L &&
    joint$dark_predictive_q > 0.10,
  if (nrow(joint)) {
    sprintf("dark q=%.3f", joint$dark_predictive_q)
  } else {
    "joint candidate absent"
  }
)
add_claim(
  "Sampling and environmental balance controls remain null",
  all(quality$maxT_FWER_p >= 0.05),
  paste(
    "minimum v21 quality-control FWER p=",
    round(min(quality$maxT_FWER_p), 4)
  )
)
add_check(
  "No causal horticultural claim",
  grepl(
    "not planting", metadata_value[["causal_claim_ceiling"]],
    fixed = TRUE
  ),
  metadata_value[["causal_claim_ceiling"]]
)
add_check(
  "Independent validation complete",
  !any(validation$status == "FAIL"),
  paste(sum(validation$status == "PASS"), "checks passed")
)
audit <- do.call(rbind, checks)
utils::write.csv(
  audit, file.path(output_dir, "did_claim_audit.csv"),
  row.names = FALSE
)

lines <- c(
  "# v22 claim audit",
  "",
  paste(
    sum(audit$status == "PASS"), "claim checks passed;",
    sum(audit$status %in% c("not_applicable", "RESULT")),
    "reported rather than enforced."
  ),
  "",
  sprintf(
    paste(
      "The fixed 16 local pigmented-isolate cells showed a directional",
      "population-DID alignment contrast (raw p = %.3f), but it did not",
      "pass within-family maxT correction (p = %.3f)."
    ),
    aligned$directional_or_two_sided_p, aligned$maxT_FWER_p
  ),
  "",
  sprintf(
    paste(
      "Nine of 16 candidates (56.3%%) were DID-proximate high-population",
      "cells versus a %.1f%% natural-map mean; corrected p = %.3f."
    ),
    100 * urban_class$null_mean_fraction, urban_class$maxT_FWER_p
  ),
  "",
  sprintf(
    paste(
      "The alignment score is a prespecified convergence summary, not an",
      "independent predictor: its maximum Spearman rho with its two inputs",
      "was %.3f."
    ),
    aligned_input_rho
  ),
  "",
  paste(
    "This is a convergent local settlement-context tendency, not evidence",
    "of planting, garden escape, horticultural provenance, or introgression.",
    "The one strongest convergent cell was not a dark predictive-tail",
    "observation."
  ),
  "",
  paste0(
    "- ", audit$claim, ": ", audit$status, " (", audit$evidence, ")"
  )
)
writeLines(
  lines, file.path(output_dir, "AUDIT.md"), useBytes = TRUE
)
skipped <- audit[
  audit$status %in% c("not_applicable", "RESULT"), , drop = FALSE
]
if (any(audit$status == "FAIL")) {
  print(audit[audit$status == "FAIL", , drop = FALSE])
  stop("v22 claim audit failed.", call. = FALSE)
}
if (nrow(skipped)) {
  cat(
    "v22 claims reported rather than enforced under --baseline ",
    baseline, ":\n", sep = ""
  )
  print(skipped)
}
cat("v22 claim audit passed ", nrow(audit), " checks.\n")
