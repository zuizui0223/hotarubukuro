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
  if (!is.null(x)) {
    utils::write.csv(
      x, file.path(output_dir, name), row.names = FALSE, na = ""
    )
  }
}

source_data <- read_csv("reanalysis_inputs/Data_S1_current_analysis.csv")
phenotype <- read_csv(
  paste0(
    "results/ecological_v11_pigmentation_hurdle/",
    "analysis_data_pigmentation_hurdle.csv"
  )
)
measurement <- read_csv(
  paste0(
    "results/ecological_v11_pigmentation_hurdle/",
    "pigmentation_measurement_summary.csv"
  )
)
cells <- read_csv(
  paste0(
    "results/ecological_v15_multiscale_hotspots/",
    "multiscale_hotspot_cells_1km.csv"
  )
)
performance <- read_csv(
  paste0(
    "results/ecological_v16_predictive_replication/",
    "predictive_replication_model_performance.csv"
  )
)
isolates <- read_csv(
  paste0(
    "results/ecological_v20_local_white_isolates/",
    "local_isolate_candidates.csv"
  )
)
submission_null <- read_csv(
  paste0(
    "results/ecological_v25_submission_isolate_null/",
    "submission_isolate_natural_null_summary.csv"
  ),
  required = FALSE
)
joint <- read_csv(
  paste0(
    "results/ecological_v26_joint_submission_isolate_ppc/",
    "joint_isolate_natural_null_summary.csv"
  ),
  required = FALSE
)
joint_ids <- read_csv(
  paste0(
    "results/ecological_v26_joint_submission_isolate_ppc/",
    "joint_candidate_ids.csv"
  ),
  required = FALSE
)
boundary <- read_csv(
  paste0(
    "results/ecological_v26_joint_submission_isolate_ppc/",
    "crossfit_boundary_audit.csv"
  ),
  required = FALSE
)
human_population <- read_csv(
  paste0(
    "results/ecological_v21_local_human_neighbourhood/",
    "human_neighbourhood_population_scale_summary.csv"
  ),
  required = FALSE
)
did <- read_csv(
  "results/ecological_v22_did_human_context/did_contrast_summary.csv",
  required = FALSE
)
sdm <- read_csv("results/bombus_sdm_rebuild_A/ENMeval_selected_models.csv")
coverage <- read_csv("results/bombus_sdm_rebuild_A/flower_prediction_coverage.csv")

row <- function(section, metric, value, detail = "") {
  data.frame(
    section = section,
    metric = metric,
    value = as.character(value),
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}
rows <- list()
add <- function(...) rows[[length(rows) + 1L]] <<- row(...)

add(
  "population", "source_rows", nrow(source_data),
  "current Data_S1-derived analysis copy"
)
if ("duplicate_image_sha256" %in% names(source_data)) {
  duplicate <- tolower(trimws(as.character(
    source_data$duplicate_image_sha256
  ))) %in% c("1", "true", "t", "yes", "y")
  add(
    "population", "canonical_extra_duplicate_rows", sum(duplicate),
    "stable source-row order; first exact image hash retained"
  )
}
add(
  "population", "phenotype_analysis_rows", nrow(phenotype),
  "fresh v11 analysis population after analysis-specific availability"
)
add(
  "population", "cells_1km", nrow(cells),
  "fresh v15 1-km analysis cells"
)
if ("bombus_fingerprint_common_support" %in% names(cells)) {
  add(
    "population", "bombus_common_support_cells",
    sum(cells$bombus_fingerprint_common_support %in% TRUE, na.rm = TRUE),
    "all five species available for the supplementary community fingerprint"
  )
}
if (all(c("n_white", "n_pigmented") %in% names(measurement))) {
  add("phenotype", "white_observations", measurement$n_white[[1L]])
  add("phenotype", "pigmented_observations", measurement$n_pigmented[[1L]])
}

for (index in seq_len(nrow(performance))) {
  item <- performance[index, , drop = FALSE]
  detail <- paste0(
    "metric=", item$primary_metric,
    if ("AUC" %in% names(item) && is.finite(item$AUC)) {
      paste0(";AUC=", signif(item$AUC, 5))
    } else {
      ""
    },
    if ("RMSE" %in% names(item) && is.finite(item$RMSE)) {
      paste0(";RMSE=", signif(item$RMSE, 5))
    } else {
      ""
    }
  )
  add(
    "national_model", as.character(item$model), item$primary_value,
    detail
  )
}

add(
  "local_isolates", "candidate_count", nrow(isolates),
  "same primary local-isolate definition; count is not fixed a priori"
)
if (!is.null(submission_null)) {
  primary_null <- submission_null[
    submission_null$configuration == "primary_10km_env1_all_white" &
      submission_null$metric %in% c("candidate_count", "candidate_fraction"),
    , drop = FALSE
  ]
  for (index in seq_len(nrow(primary_null))) {
    item <- primary_null[index, , drop = FALSE]
    add(
      "local_isolates", paste0(item$metric, "_natural_null"),
      item$observed_value,
      paste0(
        "null_mean=", signif(item$null_mean, 5),
        ";p=", signif(item$empirical_p, 5),
        ";draws=", item$n_null_draws
      )
    )
  }
}

if (!is.null(joint)) {
  primary_joint <- joint[
    joint$configuration == "primary_10km_env1_all_white" &
      joint$metric %in% c("candidate_count", "candidate_fraction"),
    , drop = FALSE
  ]
  for (index in seq_len(nrow(primary_joint))) {
    item <- primary_joint[index, , drop = FALSE]
    add(
      "joint_ppc", item$metric, item$observed_value,
      paste0(
        "null_mean=", signif(item$null_mean, 5),
        ";p=", signif(item$empirical_p, 5),
        ";maps=", item$n_null_draws
      )
    )
  }
}

identity_ok <- NA
boundary_ok <- NA
if (!is.null(joint_ids)) {
  identity_ok <- identical(
    sort(as.character(joint_ids$exact_site_id)),
    sort(as.character(isolates$exact_site_id))
  )
  add(
    "validation", "joint_candidate_identity", identity_ok,
    paste0("candidate_count=", nrow(isolates))
  )
}
if (!is.null(boundary)) {
  boundary_values <- setNames(as.numeric(boundary$value), boundary$metric)
  if ("observed_candidates" %in% names(boundary_values)) {
    boundary_ok <- isTRUE(all.equal(
      as.numeric(boundary_values[["observed_candidates"]]),
      nrow(isolates)
    ))
    add(
      "validation", "joint_boundary_candidate_count", boundary_ok,
      paste0(
        "boundary=", boundary_values[["observed_candidates"]],
        ";candidates=", nrow(isolates)
      )
    )
  }
}

if (!is.null(human_population) && nrow(human_population)) {
  preferred <- human_population[
    human_population$feature == "population_5km_rank", , drop = FALSE
  ]
  if (nrow(preferred) == 1L) {
    add(
      "human_context", "population_5km_rank",
      preferred$observed_focal_minus_white_neighbour,
      paste0(
        "p=", preferred$directional_or_two_sided_p,
        ";maxT_FWER=", preferred$maxT_FWER_p
      )
    )
  }
}
if (!is.null(did) && nrow(did)) {
  preferred <- did[
    did$feature == "did_aligned_population_score", , drop = FALSE
  ]
  if (nrow(preferred) == 1L) {
    add(
      "human_context", "did_aligned_population_score",
      preferred$observed_focal_minus_white_neighbour,
      paste0(
        "p=", preferred$directional_or_two_sided_p,
        ";maxT_FWER=", preferred$maxT_FWER_p
      )
    )
  }
}

if (all(c(
  "species", "feature_class", "regularization_multiplier"
) %in% names(sdm))) {
  for (index in seq_len(nrow(sdm))) {
    item <- sdm[index, , drop = FALSE]
    add(
      "sdm", paste0("selected_", item$species), "fresh_seeded_ENMeval",
      paste0(
        "FC=", item$feature_class,
        ";RM=", item$regularization_multiplier,
        if ("AICc" %in% names(item)) {
          paste0(";AICc=", signif(item$AICc, 7))
        } else {
          ""
        }
      )
    )
  }
}
if ("all_five_finite" %in% names(coverage)) {
  finite <- tolower(trimws(as.character(coverage$all_five_finite))) %in%
    c("1", "true", "t", "yes", "y")
  add(
    "sdm", "flower_rows_all_five_finite", sum(finite),
    paste0(
      "of ", nrow(coverage),
      " source rows before downstream-specific filtering"
    )
  )
}

overview <- do.call(rbind, rows)
write_csv(overview, "reanalysis_overview.csv")
write_csv(performance, "natural_model_performance.csv")
write_csv(submission_null, "submission_isolate_natural_null_summary.csv")
write_csv(joint, "joint_isolate_natural_null_summary.csv")
write_csv(
  human_population, "human_neighbourhood_population_scale_summary.csv"
)
write_csv(did, "did_contrast_summary.csv")

# Current validators use their real output names. Historical generic
# VALIDATION.csv and fixed-result submission-lock paths are deliberately absent.
validation_files <- c(
  paste0(
    "results/ecological_v16_predictive_replication/",
    "predictive_replication_independent_validation.csv"
  ),
  paste0(
    "results/ecological_v16_predictive_replication/",
    "predictive_replication_audit.csv"
  ),
  paste0(
    "results/ecological_v19_human_landscape_extremes/",
    "landscape_independent_validation.csv"
  ),
  paste0(
    "results/ecological_v20_local_white_isolates/",
    "local_isolate_independent_validation.csv"
  ),
  paste0(
    "results/ecological_v20_local_white_isolates/",
    "local_isolate_analysis_audit.csv"
  ),
  "results/ecological_v24_candidate_doy_check/VALIDATION.csv",
  paste0(
    "results/ecological_v21_local_human_neighbourhood/",
    "human_neighbourhood_independent_validation.csv"
  ),
  paste0(
    "results/ecological_v21_local_human_neighbourhood/",
    "human_neighbourhood_claim_audit.csv"
  ),
  paste0(
    "results/ecological_v22_did_human_context/",
    "did_independent_validation.csv"
  ),
  paste0(
    "results/ecological_v22_did_human_context/",
    "did_claim_audit.csv"
  ),
  paste0(
    "results/ecological_v26_joint_submission_isolate_ppc/",
    "joint_submission_ppc_validation.csv"
  )
)
validation_rows <- lapply(
  validation_files[file.exists(validation_files)],
  function(path) {
    table <- read_csv(path)
    status_column <- intersect(c("status", "result"), names(table))
    if (!length(status_column)) return(data.frame())
    status <- toupper(as.character(table[[status_column[[1L]]]]))
    data.frame(
      file = path,
      n_rows = nrow(table),
      n_pass = sum(status == "PASS", na.rm = TRUE),
      n_fail = sum(status %in% c("FAIL", "FAILED"), na.rm = TRUE),
      n_warn = sum(status %in% c("WARN", "WARNING"), na.rm = TRUE),
      n_result = sum(status == "RESULT", na.rm = TRUE),
      n_not_applicable = sum(
        status %in% c("NOT_APPLICABLE", "NOT APPLICABLE"),
        na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  }
)
validation_rows <- validation_rows[
  vapply(validation_rows, nrow, integer(1)) > 0L
]
if (length(validation_rows)) {
  validation_overview <- do.call(rbind, validation_rows)
  write_csv(validation_overview, "validation_overview.csv")
  if (any(validation_overview$n_fail > 0L)) {
    stop("Current validation overview contains failed checks.", call. = FALSE)
  }
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
  if (!is.na(identity_ok)) {
    paste0(
      "- Joint PPC candidate identity: ",
      if (identity_ok) "PASS" else "FAIL"
    )
  } else {
    NULL
  },
  if (!is.na(boundary_ok)) {
    paste0(
      "- Joint PPC boundary count: ",
      if (boundary_ok) "PASS" else "FAIL"
    )
  } else {
    NULL
  },
  "",
  paste(
    "The report summarizes the fresh broad natural template and event-based",
    "anomaly/human-context stages."
  ),
  paste(
    "The focal Bombus availability test is intentionally run in a separate",
    "local-transition pipeline."
  )
)
writeLines(
  markdown, file.path(output_dir, "README.md"), useBytes = TRUE
)

if (isFALSE(identity_ok) || isFALSE(boundary_ok)) {
  stop("Dynamic joint-PPC identity validation failed.", call. = FALSE)
}
cat(
  "Fresh-input reanalysis summary written to ", output_dir, "\n",
  sep = ""
)
