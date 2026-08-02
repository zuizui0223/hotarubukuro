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

# Which analysis the frozen upstream audits are auditing against. `published`
# is the locked behaviour and the default. `reconstruction` keeps every
# dataset-independent check and reports the handful of checks that assert the
# published run's own row counts and warning pattern as not applicable; see
# validation/audit_phenotype.R.
baseline <- hb_arg_value(args, "--baseline", "published")
if (!baseline %in% c("published", "reconstruction")) {
  stop("--baseline must be published or reconstruction.", call. = FALSE)
}
# The bidirectional local colour-state discordance diagnostic is off by default
# so the locked stage sequence is unchanged. It is an exploratory addition, not
# part of the published pipeline.
run_discordance <- hb_as_bool(hb_arg_value(args, "--discordance", "false"))

# Numerical stabilisation for the phenology component of stage 02 only. The
# default of 0 is the locked behaviour: no stabilisation, nothing changed. A
# positive value is passed straight through to control.inla(diagonal=) for that
# one component; see the comment in scripts/run_natural_predictive_model.R for
# why the reconstruction needs it and what it does not change.
phenology_diagonal <- hb_arg_value(args, "--phenology-diagonal", "0")

# Survey mode: record a stage failure and keep going, instead of stopping.
#
# Default false, so the locked behaviour — stop at the first failing stage — is
# unchanged, and a normal run still cannot proceed on a broken upstream.
#
# It exists because every stage after 02 consumes stage 02's output, so none of
# them has ever executed on the reconstruction. Discovering their failures one
# per run costs a full CI cycle each. With this on, the first run that clears
# stage 02 reports every remaining blocker at once.
#
# It never converts failure into success: each stage keeps its own PASS or FAIL
# in the manifest, the run still exits non-zero if anything failed, and the
# comparison's freshness gate still refuses a verdict on artifacts the run did
# not regenerate.
continue_on_failure <- hb_as_bool(
  hb_arg_value(args, "--continue-on-failure", "false")
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Stamp what this run was, before it runs. Every reader of these outputs — the
# comparison, the reproducibility report, a person opening the artifact — can
# then tell a canonical run from a survey run without inferring it from whether
# any stage happens to have failed. Written unconditionally so a canonical run
# in a reused workspace overwrites a previous survey run's stamp rather than
# inheriting it.
writeLines(
  c(
    if (continue_on_failure) "run_mode=survey" else "run_mode=canonical",
    paste0("continue_on_failure=", tolower(as.character(continue_on_failure))),
    paste0("mode=", mode),
    paste0("baseline=", baseline),
    paste0("phenology_inla_diagonal=", phenology_diagonal),
    paste0("started_utc=", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    if (continue_on_failure) {
      paste(
        "A survey run exists to enumerate downstream failures during",
        "development. Its outputs are diagnostic. They are not a reference",
        "reconstruction and no robustness verdict may be drawn from them."
      )
    }
  ),
  file.path(output_dir, "run_mode.txt")
)

log_dir <- file.path(output_dir, "logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
rscript <- file.path(R.home("bin"), "Rscript")
if (.Platform$file.sep == "\\") rscript <- paste0(rscript, ".exe")

stage_rows <- list()
failed_stages <- list()
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
    # Surface the stage's own output here. The logs are uploaded as artifacts,
    # but a failure has to be diagnosable from the workflow log alone, which is
    # the only channel available when the artifact cannot be downloaded.
    for (entry in list(
      list(label = "stdout", path = stdout_path, lines = 60L),
      list(label = "stderr", path = stderr_path, lines = 200L)
    )) {
      if (!file.exists(entry$path)) next
      text <- readLines(entry$path, warn = FALSE)
      if (!length(text)) next
      message(
        "----- ", stage, " ", entry$label, " (last ",
        min(length(text), entry$lines), " of ", length(text), " lines) -----"
      )
      message(paste(utils::tail(text, entry$lines), collapse = "\n"))
    }
    message("----- end of ", stage, " logs -----")
    failed_stages[[length(failed_stages) + 1L]] <<- list(
      stage = stage, stderr_path = stderr_path
    )
    if (!continue_on_failure) {
      stop(
        "Final pipeline stage failed: ", stage,
        ". See ", stderr_path, call. = FALSE
      )
    }
    message(
      "[final] --continue-on-failure is set; recording the failure and ",
      "continuing so that this run reports every remaining blocker."
    )
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

# Printed last so a survey run ends with the list of what to fix, rather than
# with whichever stage happened to run last.
report_failures <- function() {
  if (!length(failed_stages)) return(invisible(NULL))
  message("")
  message("===== stages that failed in this run =====")
  for (entry in failed_stages) {
    message("")
    message("--- ", entry$stage, " ---")
    # Not every recorded failure has a log: the publication lock fails in-process
    # rather than through run_stage, so its entry carries NA. file.exists(NA)
    # raises "invalid 'file' argument", which would take down the very summary a
    # survey run exists to produce — and the lock failing is the ordinary case in
    # a survey run, since it reads the outputs of stages that may not have run.
    if (!is.na(entry$stderr_path) && nzchar(entry$stderr_path) &&
        file.exists(entry$stderr_path)) {
      text <- readLines(entry$stderr_path, warn = FALSE)
      text <- text[nzchar(trimws(text))]
      if (length(text)) {
        message(paste(utils::tail(text, 15L), collapse = "\n"))
      }
    }
  }
  message("")
  message(
    "===== ", length(failed_stages), " stage(s) failed; the pipeline did not ",
    "complete ====="
  )
  invisible(NULL)
}

run_stage(
  "01_audit_phenotype", "validation/audit_phenotype.R",
  c(paste0("--baseline=", baseline)),
  role = "frozen_upstream_audit"
)
run_stage(
  "02_audit_multiscale_context",
  "validation/audit_multiscale_hotspots.R",
  c(paste0("--baseline=", baseline)),
  role = "frozen_upstream_audit"
)

if (mode == "full") {
  run_stage(
    "02_run_natural_predictive_model",
    "scripts/run_natural_predictive_model.R",
    c(paste0("--phenology-diagonal=", phenology_diagonal)),
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

# Exploratory addition, run after the established stages and before the lock so
# that it consumes the same cross-fitted natural presence checkpoint the locked
# analysis produced, and so that a failure here cannot alter any published
# quantity. It is skipped unless asked for.
if (run_discordance) {
  run_stage(
    "08_run_local_state_discordance",
    "scripts/run_local_state_asymmetry.R",
    role = "exploratory_discordance"
  )
  run_stage(
    "08_validate_local_state_discordance",
    "validation/validate_local_state_asymmetry.R",
    role = "exploratory_discordance_validation"
  )
}

if (run_tests) {
  run_stage(
    "06_test_publication_modules", "tests/testthat.R",
    role = "software_validation"
  )
}

# Not a run_stage, so in survey mode it needs its own guard: it reads the
# outputs of the stages above, and a survey run is precisely the case where some
# of those are absent.
if (continue_on_failure) {
  lock_result <- try(final_write_lock(".", output_dir), silent = TRUE)
  if (inherits(lock_result, "try-error")) {
    message(
      "[final] the publication lock could not be written: ",
      conditionMessage(attr(lock_result, "condition"))
    )
    failed_stages[[length(failed_stages) + 1L]] <- list(
      stage = "06_write_publication_lock", stderr_path = NA_character_
    )
  }
} else {
  final_write_lock(".", output_dir)
}
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

if (length(failed_stages)) {
  report_failures()
  # A survey run that reached the end is still a failed run. Exiting non-zero
  # keeps that fact in the driver's pipeline_status and in the workflow result.
  quit(save = "no", status = 1L)
}

cat(
  "Final analysis pipeline completed in mode '", mode,
  "': ", normalizePath(output_dir), "\n", sep = ""
)
