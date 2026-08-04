args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")

output_dir <- hb_arg_value(
  args,
  "--output", "results/final_analysis_pipeline"
)
read_output <- function(name) {
  hb_read_csv(file.path(output_dir, name))
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

results <- read_output("final_result_registry.csv")
claims <- read_output("final_claim_registry.csv")
exclusions <- read_output("final_exclusion_registry.csv")
metadata <- read_output("final_pipeline_metadata.csv")
validation <- read_output("final_independent_validation.csv")
metadata_value <- setNames(metadata$value, metadata$field)
v21_quality <- utils::read.csv(
  paste0(
    "results/ecological_v21_local_human_neighbourhood/",
    "human_neighbourhood_quality_summary.csv"
  ),
  check.names = FALSE, stringsAsFactors = FALSE
)
v22_candidates <- utils::read.csv(
  paste0(
    "results/ecological_v22_did_human_context/",
    "did_candidate_details.csv"
  ),
  check.names = FALSE, stringsAsFactors = FALSE
)

get_result <- function(id) {
  results[results$result_id == id, , drop = FALSE]
}
local_presence <- get_result("local_bombus_presence")
local_intensity <- get_result("local_bombus_intensity")
national_bombus <- get_result("national_bombus_auc_gain")
isolate_count <- get_result("local_isolate_count")
isolate_fraction <- get_result("local_isolate_fraction")
population <- get_result("local_population_5km")
did <- get_result("local_population_did_alignment")
urban <- get_result("did_proximate_candidate_fraction")
joint <- v22_candidates[
  v22_candidates$joint_q10_did_proximity_spike, , drop = FALSE
]

add_check(
  "Two-part phenotype is the only final flower-colour response",
  all(c(
    "phenotype_white_count", "phenotype_pigmented_count",
    "national_intensity_rmse"
  ) %in% results$result_id) &&
    any(grepl(
      "all-flower continuous", exclusions$excluded_item
    )),
  "pigmentation presence and pigmented-only intensity retained"
)
add_claim(
  "National Bombus gain remains small",
  abs(national_bombus$estimate) < 0.02,
  sprintf("mean AUC change=%.4f", national_bombus$estimate)
)
add_claim(
  "Local Bombus turnover is supported in both hurdle stages",
  local_presence$corrected_p < 0.05 &&
    local_intensity$corrected_p < 0.05,
  sprintf(
    "presence beta=%.3f q=%.3f; intensity beta=%.3f q=%.3f",
    local_presence$estimate, local_presence$corrected_p,
    local_intensity$estimate, local_intensity$corrected_p
  )
)
add_check(
  "Local Bombus result is association not causal selection",
  grepl(
    "not causal", claims$claim_ceiling[
      claims$claim_id == "C4_local_bombus_turnover"
    ],
    fixed = TRUE
  ),
  claims$claim_ceiling[
    claims$claim_id == "C4_local_bombus_turnover"
  ]
)
add_claim(
  "Primary local isolates are not excessive under the natural model",
  isolate_count$raw_p > 0.05 && isolate_fraction$raw_p > 0.05,
  sprintf(
    "count=%d p=%.3f; fraction=%.3f p=%.3f",
    isolate_count$estimate, isolate_count$raw_p,
    isolate_fraction$estimate, isolate_fraction$raw_p
  )
)
add_claim(
  "Human-context directions remain exploratory",
  all(c(
    population$corrected_p, did$corrected_p, urban$corrected_p
  ) > 0.05),
  sprintf(
    "5-km population p=%.3f; DID alignment p=%.3f; class p=%.3f",
    population$corrected_p, did$corrected_p, urban$corrected_p
  )
)
add_claim(
  "Sampling and environmental controls remain null",
  all(v21_quality$maxT_FWER_p >= 0.05),
  sprintf(
    "minimum quality-control FWER p=%.3f",
    min(v21_quality$maxT_FWER_p)
  )
)
add_published_finding(
  # The early-flowering half of this claim has been withdrawn with the
  # phenology component; the dark-tail half is unchanged.
  "Only one human-context follow-up cell and no dark-tail convergence",
  nrow(joint) == 1L &&
    joint$dark_predictive_q > 0.10,
  if (nrow(joint) == 1L) {
    sprintf(
      "%s; dark q=%.3f",
      joint$exact_site_id, joint$dark_predictive_q
    )
  } else {
    paste("joint cells=", nrow(joint))
  }
)
add_check(
  "Residual and superseded horticultural analyses are excluded",
  any(grepl("residual", exclusions$excluded_item)) &&
    any(grepl("random-forest", exclusions$excluded_item)) &&
    any(grepl("matched-landscape", exclusions$excluded_item)),
  "exclusion registry contains residual and superseded exploratory branches"
)
add_check(
  "Horticultural origin is not demonstrated",
  claims$status[
    claims$claim_id == "C7_horticultural_origin"
  ] == "not_demonstrated" &&
    metadata_value[["horticultural_origin_claim"]] ==
      "not demonstrated",
  "field history and genetic evidence remain required"
)
add_check(
  "Independent final validation complete",
  !any(validation$status == "FAIL"),
  paste(sum(validation$status == "PASS"), "checks passed")
)
add_check(
  "Legacy monolithic workflow removed",
  !file.exists("final.Rmd") &&
    any(
      exclusions$excluded_item ==
        "legacy monolithic R Markdown workflow"
    ),
  "stage-specific R modules are the only publication implementation"
)

audit <- do.call(rbind, checks)
utils::write.csv(
  audit, file.path(output_dir, "final_claim_audit.csv"),
  row.names = FALSE
)
# A not_applicable claim is its own state: it is never counted as a pass, and
# it never locks the pipeline. Under --baseline reconstruction the lock states
# that every applicable claim held, not that the published claims were
# reproduced.
overall <- if (any(audit$status == "FAIL")) {
  "NEEDS_REVISION"
} else if (any(audit$status %in% c("not_applicable", "RESULT"))) {
  "LOCKED_WITH_REPORTED_CLAIMS"
} else {
  "LOCKED"
}
writeLines(
  c(
    paste0("# Final analysis claim audit: ", overall),
    "",
    paste(
      "Confirmatory core: hierarchical pigmentation response and",
      "nationwide environment-plus-SPDE natural predictive replication."
    ),
    "",
    sprintf(
      paste(
        "Planned local mechanism: 25-km Bombus fingerprint turnover",
        "was associated with pigmentation-presence turnover",
        "(beta %.3f, q %.3f) and pigmented-only intensity turnover",
        "(beta %.3f, q %.3f)."
      ),
      local_presence$estimate, local_presence$corrected_p,
      local_intensity$estimate, local_intensity$corrected_p
    ),
    "",
    sprintf(
      paste(
        "Exploratory human context: 5-km population and DID alignment",
        "were directional but not family-wise significant",
        "(corrected p %.3f and %.3f)."
      ),
      population$corrected_p, did$corrected_p
    ),
    "",
    paste(
      "No causal pollinator effect, planting, horticultural origin,",
      "escape, introgression, or genetic pollution is inferred."
    ),
    "",
    paste0(
      "- ", audit$claim, ": ", audit$status,
      " (", audit$evidence, ")"
    )
  ),
  file.path(output_dir, "AUDIT.md"),
  useBytes = TRUE
)
if (any(audit$status == "FAIL")) {
  print(audit[audit$status == "FAIL", , drop = FALSE])
  stop("Final claim audit failed.", call. = FALSE)
}
skipped <- audit[
  audit$status %in% c("not_applicable", "RESULT"), , drop = FALSE
]
if (nrow(skipped)) {
  cat(
    "Final claims reported rather than enforced under --baseline ",
    baseline, " (never counted as passes):\n", sep = ""
  )
  print(skipped)
}
cat("Final analysis claim audit: ", overall, " (",
    sum(audit$status == "PASS"), " passed, ",
    nrow(skipped), " not applicable).\n", sep = "")
