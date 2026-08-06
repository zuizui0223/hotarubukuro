args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

output_dir <- arg_value(
  "--output", "results/ecological_v25_submission_isolate_null"
)
active_output_dir <- arg_value(
  "--active-output", "results/ecological_v20_local_white_isolates"
)
expected_draws <- as.integer(arg_value(
  "--draws", Sys.getenv("HOTARUBUKURO_SUBMISSION_DRAWS", "10000")
))

summary_path <- file.path(
  output_dir, "submission_isolate_natural_null_summary.csv"
)
null_path <- file.path(output_dir, "submission_isolate_natural_null.csv")
ids_path <- file.path(output_dir, "submission_candidate_ids.csv")
metadata_path <- file.path(output_dir, "submission_isolate_null_metadata.csv")
manifest_path <- file.path(output_dir, "submission_reference_manifest.csv")
active_summary_path <- file.path(
  active_output_dir, "local_isolate_natural_null_summary.csv"
)
active_null_path <- file.path(
  active_output_dir, "local_isolate_natural_null.csv"
)
candidate_path <- file.path(active_output_dir, "local_isolate_candidates.csv")

submission_summary <- hb_read_csv(summary_path)
submission_null <- hb_read_csv(null_path)
submission_ids <- hb_read_csv(ids_path)
metadata <- hb_read_csv(metadata_path)
manifest <- hb_read_csv(manifest_path)
active_summary <- hb_read_csv(active_summary_path)
active_null <- hb_read_csv(active_null_path)
candidates <- hb_read_csv(candidate_path)

checks <- list()
add_check <- function(check, status, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check,
    status = if (isTRUE(status)) "PASS" else "FAIL",
    detail = detail,
    stringsAsFactors = FALSE
  )
}

required_summary <- c(
  "configuration", "metric", "observed_value", "n_null_draws",
  "null_mean", "null_sd", "lower_95", "upper_95", "empirical_p",
  "empirical_two_sided_p", "percentile", "monte_carlo_se"
)
required_null <- c(
  "configuration", "draw", "candidate_count", "candidate_fraction",
  "joint_candidate_q10_count", "joint_candidate_q05_count"
)
add_check(
  "required_columns",
  all(required_summary %in% names(submission_summary)) &&
    all(required_null %in% names(submission_null)),
  paste("summary=", nrow(submission_summary), " null=", nrow(submission_null))
)

configuration_count <- length(unique(submission_summary$configuration))
metric_count <- length(unique(submission_summary$metric))
add_check(
  "submission_draw_count",
  nrow(submission_null) == configuration_count * expected_draws &&
    all(submission_summary$n_null_draws == expected_draws) &&
    all(table(submission_null$configuration) == expected_draws),
  paste(
    "configurations=", configuration_count,
    " metrics=", metric_count,
    " draws=", expected_draws
  )
)

candidate_match <- identical(
  sort(as.character(submission_ids$exact_site_id)),
  sort(as.character(candidates$exact_site_id))
)
add_check(
  "candidate_identity_lock", candidate_match,
  paste("candidates=", nrow(submission_ids))
)

active_match <- identical(submission_summary, active_summary) &&
  identical(submission_null, active_null)
add_check(
  "active_final_registry_source", active_match,
  "submission-scale summary and null replace only the v20 natural-null tables"
)

metadata_values <- stats::setNames(metadata$value, metadata$field)
checkpoint_path <- metadata_values[["presence_checkpoint"]]
checkpoint_ok <- file.exists(checkpoint_path) &&
  identical(rp_sha256(checkpoint_path), metadata_values[["presence_checkpoint_sha256"]])
add_check(
  "checkpoint_hash", checkpoint_ok,
  paste("checkpoint=", checkpoint_path)
)

thread_fields <- c(
  "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS", "GOTO_NUM_THREADS",
  "BLIS_NUM_THREADS", "INLA_NUM_THREADS"
)
threads_ok <- all(metadata_values[thread_fields] == "1") &&
  identical(metadata_values[["OMP_DYNAMIC"]], "FALSE")
add_check(
  "single_thread_lock", threads_ok,
  paste(paste0(thread_fields, "=", metadata_values[thread_fields]), collapse = "; ")
)

checkpoint <- if (checkpoint_ok) readRDS(checkpoint_path) else NULL
checkpoint_draws <- if (!is.null(checkpoint) && is.matrix(checkpoint$draws)) {
  ncol(checkpoint$draws)
} else {
  NA_integer_
}
add_check(
  "checkpoint_draw_count",
  identical(as.integer(checkpoint_draws), as.integer(expected_draws)),
  paste("checkpoint_draws=", checkpoint_draws)
)

recalculated <- list()
for (row in seq_len(nrow(submission_summary))) {
  item <- submission_summary[row, ]
  block <- submission_null[
    submission_null$configuration == item$configuration,
    , drop = FALSE
  ]
  values <- as.numeric(block[[item$metric]])
  observed <- as.numeric(item$observed_value)
  upper_p <- (1 + sum(values >= observed, na.rm = TRUE)) /
    (length(values) + 1)
  percentile <- mean(values <= observed, na.rm = TRUE)
  recalculated[[length(recalculated) + 1L]] <- data.frame(
    configuration = item$configuration,
    metric = item$metric,
    mean_ok = hb_close_enough(mean(values, na.rm = TRUE), item$null_mean, 1e-10),
    sd_ok = hb_close_enough(stats::sd(values, na.rm = TRUE), item$null_sd, 1e-10),
    upper_p_ok = hb_close_enough(upper_p, item$empirical_p, 1e-12),
    percentile_ok = hb_close_enough(percentile, item$percentile, 1e-12),
    stringsAsFactors = FALSE
  )
}
recalculated <- do.call(rbind, recalculated)
summary_ok <- all(
  recalculated$mean_ok & recalculated$sd_ok &
    recalculated$upper_p_ok & recalculated$percentile_ok
)
add_check(
  "summary_recalculation", summary_ok,
  paste("comparisons=", nrow(recalculated))
)

manifest_ok <- TRUE
manifest_error <- "verified"
tryCatch(
  rp_verify_manifest(manifest, root = ".", label = "submission reference"),
  error = function(error) {
    manifest_ok <<- FALSE
    manifest_error <<- conditionMessage(error)
  }
)
add_check("submission_manifest", manifest_ok, manifest_error)

primary <- submission_summary[
  submission_summary$configuration == "primary_10km_env1_all_white" &
    submission_summary$metric %in% c("candidate_count", "candidate_fraction"),
  , drop = FALSE
]
add_check(
  "primary_result_complete",
  nrow(primary) == 2L &&
    all(is.finite(primary$observed_value)) &&
    all(is.finite(primary$empirical_p)) &&
    all(primary$monte_carlo_se < 0.01),
  paste(
    paste0(primary$metric, " p=", format(primary$empirical_p, digits = 6)),
    collapse = "; "
  )
)

validation <- do.call(rbind, checks)
rp_write_csv_atomic(
  validation,
  file.path(output_dir, "submission_isolate_null_validation.csv")
)
writeLines(
  c(
    "# Submission isolate-null validation",
    "",
    paste0("- checks: ", sum(validation$status == "PASS"), "/", nrow(validation), " PASS"),
    paste0("- natural maps: ", expected_draws),
    paste0("- candidate IDs: ", nrow(submission_ids))
  ),
  file.path(output_dir, "VALIDATION.md")
)

if (any(validation$status != "PASS")) {
  print(validation[validation$status != "PASS", , drop = FALSE])
  stop("Submission isolate-null validation failed.", call. = FALSE)
}

cat("Submission isolate-null validation PASS.\n")
print(primary)
