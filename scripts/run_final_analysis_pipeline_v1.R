args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = NULL) {
  prefix <- paste0(name, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  sub(prefix, "", hit[1L], fixed = TRUE)
}
as_bool <- function(value) {
  tolower(as.character(value)) %in% c("1", "true", "yes", "y")
}

mode <- arg_value("--mode", "extensions")
if (!mode %in% c("verify", "extensions", "full")) {
  stop("--mode must be verify, extensions, or full.", call. = FALSE)
}
output_dir <- arg_value(
  "--output", "results/final_analysis_pipeline"
)
run_tests <- as_bool(arg_value("--tests", "true"))
source("scripts/final_analysis_pipeline_v1.R")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
log_dir <- file.path(output_dir, "logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
rscript <- file.path(R.home("bin"), "Rscript")
if (.Platform$file.sep == "\\") rscript <- paste0(rscript, ".exe")

stage_rows <- list()
write_manifest <- function() {
  if (!length(stage_rows)) return(invisible(NULL))
  utils::write.csv(
    do.call(rbind, stage_rows),
    file.path(output_dir, "final_stage_manifest.csv"),
    row.names = FALSE
  )
}
run_stage <- function(stage, script, arguments = character(),
                      role = "validation") {
  stdout_path <- file.path(log_dir, paste0(stage, "_stdout.log"))
  stderr_path <- file.path(log_dir, paste0(stage, "_stderr.log"))
  started <- Sys.time()
  message("[final] ", stage)
  status <- system2(
    rscript, c(script, arguments),
    stdout = stdout_path, stderr = stderr_path
  )
  ended <- Sys.time()
  stage_rows[[length(stage_rows) + 1L]] <<- data.frame(
    stage = stage,
    role = role,
    command = paste(c(script, arguments), collapse = " "),
    status = if (identical(status, 0L)) "PASS" else "FAIL",
    elapsed_seconds = as.numeric(
      difftime(ended, started, units = "secs")
    ),
    stdout_log = stdout_path,
    stderr_log = stderr_path,
    stringsAsFactors = FALSE
  )
  write_manifest()
  if (!identical(status, 0L)) {
    stop(
      "Final pipeline stage failed: ", stage,
      ". See ", stderr_path, call. = FALSE
    )
  }
  invisible(TRUE)
}

run_stage(
  "audit_v11_phenotype", "scripts/audit_pigmentation_v11_results.R",
  role = "frozen_upstream_audit"
)
run_stage(
  "audit_v15_cell_context", "scripts/audit_multiscale_hotspots_v8.R",
  role = "frozen_upstream_audit"
)

if (mode == "full") {
  run_stage(
    "run_v16_natural_replication",
    "scripts/run_predictive_replication_v9.R",
    role = "confirmatory_core"
  )
}
run_stage(
  "validate_v16_checkpoints",
  "scripts/validate_predictive_replication_v9.R",
  role = "confirmatory_core_validation"
)
run_stage(
  "audit_v16_natural_replication",
  "scripts/audit_predictive_replication_v9.R",
  role = "confirmatory_core_validation"
)

if (mode %in% c("extensions", "full")) {
  run_stage(
    "run_v17_local_bombus_pairs",
    "scripts/run_local_pair_predictive_v10.R",
    role = "planned_local_mechanism"
  )
}
run_stage(
  "validate_v17_local_bombus_pairs",
  "scripts/validate_local_pair_predictive_v10.R",
  role = "planned_local_mechanism_validation"
)
run_stage(
  "audit_v17_local_bombus_pairs",
  "scripts/audit_local_pair_predictive_v10.R",
  role = "planned_local_mechanism_validation"
)

if (mode == "full") {
  run_stage(
    "run_v19_feature_engineering",
    "scripts/run_human_landscape_extremes_v12.R",
    role = "feature_engineering_only"
  )
}
run_stage(
  "validate_v19_feature_engineering",
  "scripts/validate_human_landscape_extremes_v12.R",
  role = "feature_engineering_validation"
)
run_stage(
  "audit_v19_feature_engineering",
  "scripts/audit_human_landscape_extremes_v12.R",
  role = "feature_engineering_validation"
)

if (mode %in% c("extensions", "full")) {
  run_stage(
    "run_v20_local_isolates",
    "scripts/run_local_white_isolate_v13.R",
    role = "candidate_definition"
  )
}
run_stage(
  "validate_v20_local_isolates",
  "scripts/validate_local_white_isolate_v13.R",
  role = "candidate_definition_validation"
)
run_stage(
  "audit_v20_local_isolates",
  "scripts/audit_local_white_isolate_v13.R",
  role = "candidate_definition_validation"
)

if (mode %in% c("extensions", "full")) {
  run_stage(
    "run_v21_local_human_context",
    "scripts/run_local_human_neighbourhood_v14.R",
    role = "exploratory_human_context"
  )
}
run_stage(
  "validate_v21_local_human_context",
  "scripts/validate_local_human_neighbourhood_v14.R",
  role = "exploratory_human_context_validation"
)
run_stage(
  "audit_v21_local_human_context",
  "scripts/audit_local_human_neighbourhood_v14.R",
  role = "exploratory_human_context_validation"
)

if (mode %in% c("extensions", "full")) {
  run_stage(
    "run_v22_did_sensitivity",
    "scripts/run_did_human_context_v15.R",
    role = "exploratory_human_context_sensitivity"
  )
}
run_stage(
  "validate_v22_did_sensitivity",
  "scripts/validate_did_human_context_v15.R",
  role = "exploratory_human_context_validation"
)
run_stage(
  "audit_v22_did_sensitivity",
  "scripts/audit_did_human_context_v15.R",
  role = "exploratory_human_context_validation"
)

if (run_tests) {
  run_stage(
    "testthat_full_suite", "tests/testthat.R",
    role = "software_validation"
  )
}

final_write_lock(".", output_dir)
run_stage(
  "validate_final_lock",
  "scripts/validate_final_analysis_pipeline_v1.R",
  c(paste0("--output=", output_dir)),
  role = "final_lock_validation"
)
run_stage(
  "audit_final_claims",
  "scripts/audit_final_analysis_pipeline_v1.R",
  c(paste0("--output=", output_dir)),
  role = "final_claim_audit"
)

write_manifest()
cat(
  "Final analysis pipeline completed in mode '", mode,
  "': ", normalizePath(output_dir), "\n", sep = ""
)
