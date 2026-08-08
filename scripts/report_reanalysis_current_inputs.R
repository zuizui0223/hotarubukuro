args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
output_dir <- arg_value("--output", "results/reanalysis_current_inputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

read_csv <- function(path, required = TRUE) {
  if (!file.exists(path)) {
    if (required) stop("Missing reanalysis artifact: ", path, call. = FALSE)
    return(NULL)
  }
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

write_csv <- function(x, name) {
  utils::write.csv(x, file.path(output_dir, name), row.names = FALSE, na = "")
}

source_data <- read_csv("reanalysis_inputs/Data_S1_current_analysis.csv")
phenotype <- read_csv(
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
)
measurement <- read_csv(
  "results/ecological_v11_pigmentation_hurdle/pigmentation_measurement_summary.csv"
)
cells <- read_csv(
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
final_results <- read_csv(
  "results/final_analysis_pipeline/final_result_registry.csv"
)
final_claims <- read_csv(
  "results/final_analysis_pipeline/final_claim_registry.csv"
)
gate <- read_csv(
  "results/ecological_v17_bombus_limitation_gate/bombus_limitation_gate_summary.csv"
)
isolates <- read_csv(
  "results/ecological_v20_local_white_isolates/local_isolate_candidates.csv"
)
submission_null <- read_csv(
  "results/ecological_v25_submission_isolate_null/submission_isolate_natural_null_summary.csv"
)
joint <- read_csv(
  "results/ecological_v26_joint_submission_isolate_ppc/joint_isolate_natural_null_summary.csv",
  required = FALSE
)
joint_ids <- read_csv(
  "results/ecological_v26_joint_submission_isolate_ppc/joint_candidate_ids.csv",
  required = FALSE
)
boundary <- read_csv(
  "results/ecological_v26_joint_submission_isolate_ppc/crossfit_boundary_audit.csv",
  required = FALSE
)
sdm <- read_csv(
  "results/bombus_sdm_rebuild_A/ENMeval_selected_models.csv"
)
coverage <- read_csv(
  "results/bombus_sdm_rebuild_A/flower_prediction_coverage.csv"
)

row <- function(section, metric, value, detail = "") {
  data.frame(
    section = section, metric = metric, value = as.character(value),
    detail = as.character(detail), stringsAsFactors = FALSE
  )
}
rows <- list()
add <- function(...) rows[[length(rows) + 1L]] <<- row(...)

add("population", "source_rows", nrow(source_data), "current Data_S1-derived analysis copy")
if ("duplicate_image_sha256" %in% names(source_data)) {
  duplicate <- tolower(trimws(as.character(source_data$duplicate_image_sha256))) %in%
    c("1", "true", "t", "yes", "y")
  add("population", "canonical_extra_duplicate_rows", sum(duplicate),
      "stable source-row order; first exact image hash retained")
}
add("population", "phenotype_analysis_rows", nrow(phenotype),
    "fresh v11 analysis population after analysis-specific availability")
add("population", "cells_1km", nrow(cells), "fresh v15 1-km analysis cells")
if ("bombus_fingerprint_common_support" %in% names(cells)) {
  add("population", "bombus_common_support_cells",
      sum(cells$bombus_fingerprint_common_support %in% TRUE, na.rm = TRUE),
      "all five species available for the local Bombus fingerprint")
}
if (all(c("n_white", "n_pigmented") %in% names(measurement))) {
  add("phenotype", "white_observations", measurement$n_white[[1L]])
  add("phenotype", "pigmented_observations", measurement$n_pigmented[[1L]])
}

if (all(c("is_primary_gate", "response") %in% names(gate))) {
  primary_gate <- gate[gate$is_primary_gate %in% TRUE, , drop = FALSE]
  for (i in seq_len(nrow(primary_gate))) {
    g <- primary_gate[i, , drop = FALSE]
    detail <- paste(
      c(
        if ("observed_directed_difference" %in% names(g))
          paste0("difference=", signif(g$observed_directed_difference, 5)),
        if ("upper_tail_p" %in% names(g)) paste0("p=", signif(g$upper_tail_p, 5)),
        if ("BH_q_all_gate_tests" %in% names(g))
          paste0("BH_q=", signif(g$BH_q_all_gate_tests, 5)),
        if ("n_pairs" %in% names(g)) paste0("pairs=", g$n_pairs)
      ), collapse = "; "
    )
    add("bombus_gate", as.character(g$response),
        if ("observed_directed_difference" %in% names(g)) g$observed_directed_difference else NA,
        detail)
  }
}

add("local_isolates", "candidate_count", nrow(isolates),
    "same primary local-isolate definition; count is not fixed a priori")
primary_null <- submission_null[
  submission_null$configuration == "primary_10km_env1_all_white" &
    submission_null$metric %in% c("candidate_count", "candidate_fraction"),
  , drop = FALSE
]
for (i in seq_len(nrow(primary_null))) {
  x <- primary_null[i, , drop = FALSE]
  add("local_isolates", paste0(x$metric, "_natural_null"), x$observed_value,
      paste0("null_mean=", signif(x$null_mean, 5),
             ";p=", signif(x$empirical_p, 5),
             ";draws=", x$n_null_draws))
}

if (!is.null(joint)) {
  primary_joint <- joint[
    joint$configuration == "primary_10km_env1_all_white" &
      joint$metric %in% c("candidate_count", "candidate_fraction"),
    , drop = FALSE
  ]
  for (i in seq_len(nrow(primary_joint))) {
    x <- primary_joint[i, , drop = FALSE]
    add("joint_ppc", x$metric, x$observed_value,
        paste0("null_mean=", signif(x$null_mean, 5),
               ";p=", signif(x$empirical_p, 5),
               ";maps=", x$n_null_draws))
  }
}

# Dynamic identity check for the rerun. The old submission validation hard-codes
# 18, which is a historical result identity rather than an analysis invariant.
identity_ok <- NA
boundary_ok <- NA
if (!is.null(joint_ids)) {
  identity_ok <- identical(
    sort(as.character(joint_ids$exact_site_id)),
    sort(as.character(isolates$exact_site_id))
  )
  add("validation", "joint_candidate_identity", identity_ok,
      paste0("candidate_count=", nrow(isolates)))
}
if (!is.null(boundary)) {
  b <- setNames(as.numeric(boundary$value), boundary$metric)
  if ("observed_candidates" %in% names(b)) {
    boundary_ok <- isTRUE(all.equal(as.numeric(b[["observed_candidates"]]), nrow(isolates)))
    add("validation", "joint_boundary_candidate_count", boundary_ok,
        paste0("boundary=", b[["observed_candidates"]], ";candidates=", nrow(isolates)))
  }
}

if (all(c("species", "feature_class", "regularization_multiplier") %in% names(sdm))) {
  for (i in seq_len(nrow(sdm))) {
    x <- sdm[i, , drop = FALSE]
    detail <- paste0(
      "FC=", x$feature_class,
      ";RM=", x$regularization_multiplier,
      if ("AICc" %in% names(x)) paste0(";AICc=", signif(x$AICc, 7)) else ""
    )
    add("sdm", paste0("selected_", x$species), "fresh_seeded_ENMeval", detail)
  }
}
if ("all_five_finite" %in% names(coverage)) {
  finite <- tolower(trimws(as.character(coverage$all_five_finite))) %in%
    c("1", "true", "t", "yes", "y")
  add("sdm", "flower_rows_all_five_finite", sum(finite),
      paste0("of ", nrow(coverage), " source rows before downstream-specific filtering"))
}

overview <- do.call(rbind, rows)
write_csv(overview, "reanalysis_overview.csv")
write_csv(final_results, "final_result_registry.csv")
write_csv(final_claims, "final_claim_registry.csv")

# Collect machine-readable validation states without imposing historical result
# identities on the fresh run.
validation_files <- c(
  "results/ecological_v11_pigmentation_hurdle/validation_summary.csv",
  "results/ecological_v16_predictive_replication/natural_predictive_model_validation.csv",
  "results/ecological_v17_bombus_limitation_gate/bombus_limitation_gate_independent_validation.csv",
  "results/ecological_v19_human_landscape_extremes/VALIDATION.csv",
  "results/ecological_v20_local_white_isolates/VALIDATION.csv",
  "results/ecological_v21_local_human_neighbourhood/VALIDATION.csv",
  "results/ecological_v22_did_human_context/VALIDATION.csv",
  "results/ecological_v25_submission_isolate_null/submission_isolate_null_validation.csv",
  "results/final_analysis_pipeline/final_independent_validation.csv",
  "results/final_analysis_pipeline/final_claim_audit.csv"
)
validation_rows <- lapply(validation_files[file.exists(validation_files)], function(path) {
  x <- read_csv(path)
  status_col <- intersect(c("status", "result"), names(x))
  if (!length(status_col)) return(data.frame())
  data.frame(
    file = path,
    n_rows = nrow(x),
    n_fail = sum(toupper(as.character(x[[status_col[[1L]]]])) %in% c("FAIL", "FAILED"), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
})
validation_rows <- validation_rows[vapply(validation_rows, nrow, integer(1)) > 0L]
if (length(validation_rows)) {
  validation_summary <- do.call(rbind, validation_rows)
  write_csv(validation_summary, "validation_overview.csv")
}

markdown <- c(
  "# Fresh-input full reanalysis",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  "",
  paste0("- Source rows: ", nrow(source_data)),
  paste0("- Fresh phenotype analysis rows: ", nrow(phenotype)),
  paste0("- Fresh 1-km cells: ", nrow(cells)),
  paste0("- Primary local isolates: ", nrow(isolates)),
  if (!is.na(identity_ok)) paste0("- Joint PPC candidate identity: ", if (identity_ok) "PASS" else "FAIL") else NULL,
  if (!is.na(boundary_ok)) paste0("- Joint PPC boundary count: ", if (boundary_ok) "PASS" else "FAIL") else NULL,
  "",
  "The scientific stage order, response definitions, natural model, Bombus limitation gate,",
  "local-isolate definition, natural-null procedure, and human-context analyses are unchanged.",
  "Only the upstream flower/environment reconstruction and Bombus source build were replaced."
)
writeLines(markdown, file.path(output_dir, "README.md"), useBytes = TRUE)

if (isFALSE(identity_ok) || isFALSE(boundary_ok)) {
  stop("Dynamic joint-PPC identity validation failed.", call. = FALSE)
}
cat("Fresh-input reanalysis summary written to ", output_dir, "\n", sep = "")
