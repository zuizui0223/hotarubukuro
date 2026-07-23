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
add_check(
  "National Bombus gain remains small",
  abs(national_bombus$estimate) < 0.02,
  sprintf("mean AUC change=%.4f", national_bombus$estimate)
)
add_check(
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
add_check(
  "Primary local isolates are not excessive under the natural model",
  isolate_count$raw_p > 0.05 && isolate_fraction$raw_p > 0.05,
  sprintf(
    "count=%d p=%.3f; fraction=%.3f p=%.3f",
    isolate_count$estimate, isolate_count$raw_p,
    isolate_fraction$estimate, isolate_fraction$raw_p
  )
)
add_check(
  "Human-context directions remain exploratory",
  all(c(
    population$corrected_p, did$corrected_p, urban$corrected_p
  ) > 0.05),
  sprintf(
    "5-km population p=%.3f; DID alignment p=%.3f; class p=%.3f",
    population$corrected_p, did$corrected_p, urban$corrected_p
  )
)
add_check(
  "Sampling and environmental controls remain null",
  all(v21_quality$maxT_FWER_p >= 0.05),
  sprintf(
    "minimum quality-control FWER p=%.3f",
    min(v21_quality$maxT_FWER_p)
  )
)
add_check(
  "Only one human-context follow-up cell and no early-dark convergence",
  nrow(joint) == 1L &&
    joint$early_predictive_q > 0.10 &&
    joint$dark_predictive_q > 0.10,
  if (nrow(joint) == 1L) {
    sprintf(
      "%s; early q=%.3f; dark q=%.3f",
      joint$exact_site_id, joint$early_predictive_q,
      joint$dark_predictive_q
    )
  } else {
    paste("joint cells=", nrow(joint))
  }
)
add_check(
  "Residual and superseded horticultural analyses are excluded",
  any(grepl("residual", exclusions$excluded_item)) &&
    any(grepl("v18-v19", exclusions$excluded_item)) &&
    any(grepl("v20 matched", exclusions$excluded_item)),
  "exclusion registry contains residual, v18-v19, and old v20 branches"
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
  all(validation$status == "PASS"),
  paste(sum(validation$status == "PASS"), "checks passed")
)
add_check(
  "final.Rmd unchanged",
  identical(
    unname(tools::md5sum("final.Rmd")),
    metadata_value[["final_Rmd_md5"]]
  ),
  paste("md5=", metadata_value[["final_Rmd_md5"]])
)

audit <- do.call(rbind, checks)
utils::write.csv(
  audit, file.path(output_dir, "final_claim_audit.csv"),
  row.names = FALSE
)
overall <- if (all(audit$status == "PASS")) {
  "LOCKED"
} else {
  "NEEDS_REVISION"
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
if (overall != "LOCKED") {
  print(audit[audit$status != "PASS", , drop = FALSE])
  stop("Final claim audit failed.", call. = FALSE)
}
cat("Final analysis claim audit: LOCKED (",
    nrow(audit), " checks).\n", sep = "")
