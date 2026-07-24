args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
hb_load_modules("final_registry")

mode <- hb_arg_value(args, "--mode", "extensions")
if (!mode %in% c("verify", "extensions", "full")) {
  stop("--mode must be verify, extensions, or full.", call. = FALSE)
}
output_dir <- hb_arg_value(
  args,
  "--output", "results/final_analysis_pipeline"
)
run_tests <- hb_as_bool(hb_arg_value(args, "--tests", "true"))

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
  "01_audit_phenotype", "validation/audit_phenotype.R",
  role = "frozen_upstream_audit"
)
run_stage(
  "02_audit_multiscale_context",
  "validation/audit_multiscale_hotspots.R",
  role = "frozen_upstream_audit"
)

if (mode == "full") {
  run_stage(
    "02_run_natural_predictive_model",
    "scripts/run_natural_predictive_model.R",
    role = "confirmatory_core"
  )
}
run_stage(
  "02_validate_natural_predictive_model",
  "validation/validate_natural_predictive_model.R",
  role = "confirmatory_core_validation"
)
run_stage(
  "02_audit_natural_predictive_model",
  "validation/audit_natural_predictive_model.R",
  role = "confirmatory_core_validation"
)

if (mode %in% c("extensions", "full")) {
  run_stage(
    "03_run_local_bombus_turnover",
    "scripts/run_local_bombus_turnover.R",
    role = "planned_local_mechanism"
  )
}
run_stage(
  "03_validate_local_bombus_turnover",
  "validation/validate_local_bombus_turnover.R",
  role = "planned_local_mechanism_validation"
)
run_stage(
  "03_audit_local_bombus_turnover",
  "validation/audit_local_bombus_turnover.R",
  role = "planned_local_mechanism_validation"
)

if (mode == "full") {
  run_stage(
    "04_run_human_landscape_features",
    "scripts/run_human_landscape_features.R",
    role = "feature_engineering_only"
  )
}
run_stage(
  "04_validate_human_landscape_features",
  "validation/validate_human_landscape_features.R",
  role = "feature_engineering_validation"
)
run_stage(
  "04_audit_human_landscape_features",
  "validation/audit_human_landscape_features.R",
  role = "feature_engineering_validation"
)

if (mode %in% c("extensions", "full")) {
  run_stage(
    "04_define_local_pigmented_isolates",
    "scripts/run_local_pigmented_isolates.R",
    role = "candidate_definition"
  )
}
run_stage(
  "04_validate_local_pigmented_isolates",
  "validation/validate_local_pigmented_isolates.R",
  role = "candidate_definition_validation"
)
run_stage(
  "04_audit_local_pigmented_isolates",
  "validation/audit_local_pigmented_isolates.R",
  role = "candidate_definition_validation"
)

if (mode %in% c("extensions", "full")) {
  run_stage(
    "05_run_local_human_context",
    "scripts/run_local_human_context.R",
    role = "exploratory_human_context"
  )
}
run_stage(
  "05_validate_local_human_context",
  "validation/validate_local_human_context.R",
  role = "exploratory_human_context_validation"
)
run_stage(
  "05_audit_local_human_context",
  "validation/audit_local_human_context.R",
  role = "exploratory_human_context_validation"
)

if (mode %in% c("extensions", "full")) {
  run_stage(
    "05_run_did_sensitivity",
    "scripts/run_did_sensitivity.R",
    role = "exploratory_human_context_sensitivity"
  )
}
run_stage(
  "05_validate_did_sensitivity",
  "validation/validate_did_sensitivity.R",
  role = "exploratory_human_context_validation"
)
run_stage(
  "05_audit_did_sensitivity",
  "validation/audit_did_sensitivity.R",
  role = "exploratory_human_context_validation"
)

if (run_tests) {
  run_stage(
    "06_test_publication_modules", "tests/testthat.R",
    role = "software_validation"
  )
}

final_write_lock(".", output_dir)
run_stage(
  "06_validate_publication_lock",
  "validation/validate_publication_pipeline.R",
  c(paste0("--output=", output_dir)),
  role = "final_lock_validation"
)
run_stage(
  "06_audit_publication_claims",
  "validation/audit_publication_claims.R",
  c(paste0("--output=", output_dir)),
  role = "final_claim_audit"
)

write_manifest()
cat(
  "Final analysis pipeline completed in mode '", mode,
  "': ", normalizePath(output_dir), "\n", sep = ""
)
