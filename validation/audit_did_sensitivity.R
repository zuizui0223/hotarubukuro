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

baseline_argument <- grep("^--baseline=", args, value = TRUE)
baseline <- if (length(baseline_argument)) {
  sub("^--baseline=", "", baseline_argument[[1L]])
} else {
  "published"
}
if (!baseline %in% c("published", "reconstruction")) {
  stop("--baseline must be published or reconstruction.", call. = FALSE)
}

checks <- list()
add_check <- function(claim, passed, evidence) {
  checks[[length(checks) + 1L]] <<- data.frame(
    claim = claim,
    status = if (isTRUE(passed)) "PASS" else "FAIL",
    evidence = as.character(evidence),
    stringsAsFactors = FALSE
  )
}
add_report <- function(name, detail, published_pass = NA) {
  if (identical(baseline, "published")) {
    add_check(name, isTRUE(published_pass), detail)
  } else {
    checks[[length(checks) + 1L]] <<- data.frame(
      claim = name, status = "RESULT",
      evidence = paste0(
        detail,
        if (!is.na(published_pass)) paste0(
          "; published-mode verdict would be ",
          if (isTRUE(published_pass)) "PASS" else "FAIL"
        ) else ""
      ),
      stringsAsFactors = FALSE
    )
  }
}
add_published_finding <- function(name, passed, detail) {
  if (identical(baseline, "published")) {
    add_check(name, passed, detail)
  } else {
    checks[[length(checks) + 1L]] <<- data.frame(
      claim = name, status = "not_applicable",
      evidence = paste0(
        detail,
        "; reason=the reconstruction defines its own analysis population"
      ),
      stringsAsFactors = FALSE
    )
  }
}
one_row <- function(data, label) {
  if (nrow(data) != 1L) {
    stop("Expected one row for ", label, "; found ", nrow(data), call. = FALSE)
  }
  data
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

aligned <- one_row(
  summary[summary$feature == "did_aligned_population_score", , drop = FALSE],
  "DID-aligned population contrast"
)
proximity <- one_row(
  summary[summary$feature == "did_proximity_rank", , drop = FALSE],
  "DID proximity contrast"
)
did_spike <- one_row(
  convergence[
    convergence$spike_feature == "did_proximity_rank" &
      convergence$metric == "candidate_human_spike_count", , drop = FALSE
  ],
  "DID-proximity candidate-spike count"
)
urban_class <- one_row(
  composition[
    composition$human_context_class == "did_proximate_high_population",
    , drop = FALSE
  ],
  "DID-proximate high-population class"
)
outside_class <- one_row(
  composition[
    composition$human_context_class == "populated_beyond_did", , drop = FALSE
  ],
  "populated-beyond-DID class"
)
joint <- candidates[candidates$joint_q10_did_proximity_spike, , drop = FALSE]

add_published_finding(
  "Fixed local-isolate population",
  nrow(candidates) == 16L,
  paste("v20/v21 candidates retained:", nrow(candidates))
)
add_check(
  "DID and population were post-selection features",
  grepl("do not select", metadata_value[["candidate_selector"]], fixed = TRUE),
  metadata_value[["candidate_selector"]]
)
add_report(
  "Population-DID alignment result",
  sprintf(
    "estimate=%.4f; raw p=%.4f; maxT FWER p=%.4f",
    aligned$observed_focal_minus_white_neighbour,
    aligned$directional_or_two_sided_p, aligned$maxT_FWER_p
  ),
  aligned$directional_or_two_sided_p < 0.05 && aligned$maxT_FWER_p >= 0.05
)
add_report(
  "DID proximity result",
  sprintf(
    "estimate=%.4f; raw p=%.4f; maxT FWER p=%.4f",
    proximity$observed_focal_minus_white_neighbour,
    proximity$directional_or_two_sided_p, proximity$maxT_FWER_p
  ),
  proximity$directional_or_two_sided_p >= 0.05 &&
    proximity$maxT_FWER_p >= 0.05
)
aligned_input_rho <- max(abs(collinearity$spearman_rho[
  (collinearity$feature_1 == "did_aligned_population_score" &
     collinearity$feature_2 %in%
       c("did_proximity_rank", "population_5km_rank")) |
    (collinearity$feature_2 == "did_aligned_population_score" &
       collinearity$feature_1 %in%
         c("did_proximity_rank", "population_5km_rank"))
]))
add_report(
  "Population-DID composite dependence",
  sprintf("maximum Spearman rho with its two inputs=%.3f", aligned_input_rho),
  aligned_input_rho > 0.95
)
add_report(
  "DID-proximity local-spike result",
  sprintf(
    "observed=%d; null mean=%.3f; raw p=%.4f; FWER p=%.4f",
    did_spike$observed_value, did_spike$null_mean,
    did_spike$empirical_p, did_spike$maxT_FWER_p
  ),
  did_spike$empirical_p < 0.05 && did_spike$maxT_FWER_p >= 0.05
)
add_report(
  "DID-proximate high-population composition result",
  sprintf(
    "observed=%d/%d (%.3f); null fraction=%.3f; FWER p=%.4f",
    urban_class$observed_candidate_count, nrow(candidates),
    urban_class$observed_candidate_fraction,
    urban_class$null_mean_fraction, urban_class$maxT_FWER_p
  ),
  urban_class$observed_candidate_fraction > urban_class$null_mean_fraction &&
    urban_class$maxT_FWER_p >= 0.05
)
add_report(
  "DID-outside populated class support",
  paste(
    "all supported cells=",
    sum(context$human_context_class == "populated_beyond_did"),
    "; observed candidates=", outside_class$observed_candidate_count
  ),
  sum(context$human_context_class == "populated_beyond_did") <= 1L &&
    outside_class$observed_candidate_count == 0L
)
add_published_finding(
  "Published q10 plus DID-spike follow-up candidate",
  nrow(joint) == 1L && joint$exact_site_id == "cell-1km--108_-147",
  paste(
    "n=", nrow(joint),
    if (nrow(joint)) paste("cell=", paste(joint$exact_site_id, collapse = "|")) else ""
  )
)
add_published_finding(
  "Published convergent candidate lacks a dark predictive tail",
  nrow(joint) == 1L && joint$dark_predictive_q > 0.10,
  if (nrow(joint) == 1L) {
    sprintf("dark q=%.3f", joint$dark_predictive_q)
  } else {
    paste("joint candidates=", nrow(joint))
  }
)
add_report(
  "Sampling and environmental balance controls",
  paste(
    "minimum v21 quality-control FWER p=",
    round(min(quality$maxT_FWER_p), 4)
  ),
  all(quality$maxT_FWER_p >= 0.05)
)
add_check(
  "No causal horticultural claim",
  grepl("not planting", metadata_value[["causal_claim_ceiling"]], fixed = TRUE),
  metadata_value[["causal_claim_ceiling"]]
)
add_check(
  "Independent validation complete",
  !any(validation$status == "FAIL"),
  paste(
    sum(validation$status == "PASS"), "checks passed;",
    sum(validation$status == "not_applicable"), "not applicable"
  )
)

audit <- do.call(rbind, checks)
utils::write.csv(
  audit, file.path(output_dir, "did_claim_audit.csv"), row.names = FALSE
)

alignment_sentence <- sprintf(
  paste(
    "Among %d fixed candidates, the population-DID alignment contrast was",
    "%.4f (raw p %.3f; maxT p %.3f)."
  ),
  nrow(candidates), aligned$observed_focal_minus_white_neighbour,
  aligned$directional_or_two_sided_p, aligned$maxT_FWER_p
)
urban_sentence <- sprintf(
  paste(
    "%d of %d candidates (%.1f%%) were DID-proximate high-population",
    "cells, compared with a %.1f%% mean across natural-map candidate sets",
    "(maxT p %.3f)."
  ),
  urban_class$observed_candidate_count, nrow(candidates),
  100 * urban_class$observed_candidate_fraction,
  100 * urban_class$null_mean_fraction, urban_class$maxT_FWER_p
)
joint_sentence <- if (nrow(joint) == 1L) {
  sprintf(
    paste(
      "One q10 unexpected-pigmentation plus DID-spike follow-up cell was",
      "identified (%s); its dark predictive q was %.3f."
    ),
    joint$exact_site_id, joint$dark_predictive_q
  )
} else {
  sprintf(
    "%d q10 unexpected-pigmentation plus DID-spike follow-up cells were identified.",
    nrow(joint)
  )
}

writeLines(
  c(
    "# v22 claim audit",
    "",
    paste(
      sum(audit$status == "PASS"), "structural checks passed;",
      sum(audit$status %in% c("not_applicable", "RESULT")),
      "findings were reported rather than enforced."
    ),
    "",
    alignment_sentence,
    "",
    urban_sentence,
    "",
    sprintf(
      paste(
        "The alignment score is a convergence summary, not an independent",
        "predictor; its maximum Spearman rho with its inputs was %.3f."
      ),
      aligned_input_rho
    ),
    "",
    joint_sentence,
    "",
    paste(
      "These describe settlement context within the sampled route network,",
      "not planting, garden escape, horticultural provenance, or introgression."
    ),
    "",
    paste0(
      "- ", audit$claim, ": ", audit$status, " (", audit$evidence, ")"
    )
  ),
  file.path(output_dir, "AUDIT.md"), useBytes = TRUE
)

failed <- audit[audit$status == "FAIL", , drop = FALSE]
reported <- audit[
  audit$status %in% c("not_applicable", "RESULT"), , drop = FALSE
]
if (nrow(failed)) {
  print(failed)
  stop("v22 claim audit failed.", call. = FALSE)
}
if (nrow(reported)) {
  cat(
    "v22 findings reported rather than enforced under --baseline ",
    baseline, ":\n", sep = ""
  )
  print(reported)
}
cat(
  "v22 claim audit completed: ", sum(audit$status == "PASS"),
  " passed, ", nrow(reported), " reported.\n", sep = ""
)
