# Record what a run consumed and what it produced.
#
# Every workflow ends here so that the reproducibility/ directory always has the
# same shape: an input manifest, an output manifest, the resolved session, the
# dependency audit written by preflight, the pipeline DAG, and a human-readable
# summary. All manifests carry SHA-256 hashes.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
report_dir <- arg_value("--report-dir", "reproducibility")
workflow_label <- arg_value("--workflow", "canonical-analysis")
inputs_argument <- arg_value("--inputs", "")
outputs_argument <- arg_value("--outputs", "")
dag_source <- arg_value("--dag", "docs/pipeline-dag.md")

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

expand_paths <- function(argument) {
  if (!nzchar(argument)) return(character())
  entries <- trimws(strsplit(argument, ",", fixed = TRUE)[[1L]])
  entries <- entries[nzchar(entries)]
  unlist(lapply(entries, function(entry) {
    if (dir.exists(entry)) {
      list.files(entry, recursive = TRUE, full.names = TRUE, all.files = FALSE)
    } else {
      entry
    }
  }), use.names = FALSE)
}

input_paths <- expand_paths(inputs_argument)
output_paths <- expand_paths(outputs_argument)

input_manifest <- rp_manifest_rows(input_paths, role = "input")
output_manifest <- rp_manifest_rows(output_paths, role = "output")

# Hashing a file does not make it this run's work. A run that stopped early
# still finds the repository's committed results on disk, and listing them as
# outputs would overstate what the run produced, so each row records whether the
# file was written after the run began.
run_started <- suppressWarnings(
  as.numeric(Sys.getenv("HOTARUBUKURO_RUN_STARTED", ""))
)
if (nrow(output_manifest)) {
  modified <- as.numeric(file.info(output_manifest$path)$mtime)
  output_manifest$produced_in_run <- if (isTRUE(is.finite(run_started))) {
    modified >= run_started
  } else {
    NA
  }
}
produced_count <- if (nrow(output_manifest) &&
                     !all(is.na(output_manifest$produced_in_run))) {
  sum(output_manifest$produced_in_run, na.rm = TRUE)
} else {
  NA_integer_
}

if (nrow(input_manifest)) {
  rp_write_manifest(input_manifest, file.path(report_dir, "input_manifest.csv"))
}
rp_write_manifest(output_manifest, file.path(report_dir, "output_manifest.csv"))
rp_write_session_record(report_dir)
rp_write_csv_atomic(
  rp_environment_record(), file.path(report_dir, "package_versions.csv")
)

# The three records that make a completed run auditable on its own terms: the
# machine and toolchain it ran on, every seed the analysis code sets, and the
# checkpoints that make it restartable.
rp_write_lines_atomic(
  rp_workflow_environment_lines(),
  file.path(report_dir, "workflow_environment.txt")
)

seed_registry <- rp_seed_registry()
stage_registry_path <- file.path(report_dir, "pipeline_stage_registry.csv")
if (file.exists(stage_registry_path)) {
  stages <- utils::read.csv(
    stage_registry_path, check.names = FALSE, stringsAsFactors = FALSE,
    colClasses = "character"
  )
  owner <- stats::setNames(stages$stage_id, stages$generating_script)
  seed_registry$stage_id <- unname(owner[seed_registry$script])
  declared <- stages[!stages$seed %in% c("", "NA"), c("stage_id", "seed")]
  seed_registry$declared_stage_seed <- unname(
    stats::setNames(declared$seed, declared$stage_id)[seed_registry$stage_id]
  )
  disagreements <- with(seed_registry, which(
    kind == "seed_argument_default" & !is.na(declared_stage_seed) &
      value != declared_stage_seed
  ))
  if (length(disagreements)) {
    stop(
      "The stage registry and the scripts disagree about seeds:\n  ",
      paste(with(seed_registry[disagreements, ], sprintf(
        "%s:%d declares %s but stage %s declares %s",
        script, line, value, stage_id, declared_stage_seed
      )), collapse = "\n  "),
      call. = FALSE
    )
  }
} else {
  seed_registry$stage_id <- NA_character_
  seed_registry$declared_stage_seed <- NA_character_
}
rp_write_csv_atomic(seed_registry, file.path(report_dir, "seed_registry.csv"))

checkpoints <- rp_checkpoint_manifest("results")
rp_write_csv_atomic(
  checkpoints, file.path(report_dir, "checkpoint_manifest.csv")
)

if (file.exists(dag_source)) {
  file.copy(dag_source, file.path(report_dir, "pipeline_dag.md"), overwrite = TRUE)
}

missing_inputs <- setdiff(input_paths, input_manifest$path)
commit <- rp_git_commit()
run_url <- {
  server <- Sys.getenv("GITHUB_SERVER_URL", "https://github.com")
  repository <- Sys.getenv("GITHUB_REPOSITORY", "")
  run_id <- Sys.getenv("GITHUB_RUN_ID", "")
  if (nzchar(repository) && nzchar(run_id)) {
    paste0(server, "/", repository, "/actions/runs/", run_id)
  } else {
    NA_character_
  }
}

summary_lines <- c(
  paste0("# Reproduction summary: ", workflow_label),
  "",
  paste0("- commit: `", commit, "`"),
  paste0("- workflow run: ", if (is.na(run_url)) "local" else run_url),
  paste0("- generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("- R: ", paste(R.version$major, R.version$minor, sep = "."),
         "; CRAN snapshot: ", rp_cran_repository()),
  paste0("- INLA: ", if (requireNamespace("INLA", quietly = TRUE)) {
    as.character(utils::packageVersion("INLA"))
  } else {
    "not installed"
  }),
  paste0("- inputs recorded: ", nrow(input_manifest)),
  paste0("- outputs recorded: ", nrow(output_manifest),
         if (is.na(produced_count)) {
           ""
         } else {
           paste0(" (", produced_count, " written by this run, ",
                  nrow(output_manifest) - produced_count,
                  " already present before it started)")
         }),
  paste0("- seeds recorded: ", nrow(seed_registry)),
  paste0("- checkpoints recorded: ", nrow(checkpoints)),
  "",
  "## Reproducibility class",
  "",
  "Two different things are called reproducibility here and they are kept apart.",
  "",
  "- *Bitwise*: identical bytes on a rerun. It holds for the derived tables that",
  "  are pure functions of the immutable inputs, and every one of them is hashed",
  "  in `output_manifest.csv`.",
  "- *Statistical*: the same conclusion within stated numerical tolerance. It is",
  "  what holds for the stages that draw from an INLA posterior. `INLA` is not",
  "  bit-deterministic across runs even under a fixed seed, so those quantities",
  "  are checked against `numerical_regression_report.csv` tolerances rather than",
  "  against a hash.",
  "",
  "`pipeline_stage_registry.csv` records which class each stage belongs to in its",
  "`deterministic` column.",
  ""
)

# Numerical stabilisation, read back from what the stage actually wrote rather
# than from what a driver requested. A solver setting that changes results has
# to be visible in the run's own record, not only in the code that set it.
stabilisation_rows <- local({
  path <- file.path(
    "results", "ecological_v16_predictive_replication",
    "predictive_replication_model_log.csv"
  )
  if (!file.exists(path)) return(NULL)
  logs <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!"inla_diagonal" %in% names(logs)) return(NULL)
  values <- stats::aggregate(inla_diagonal ~ model, logs, max)
  values[order(values$model), , drop = FALSE]
})
if (!is.null(stabilisation_rows)) {
  stabilised <- stabilisation_rows[stabilisation_rows$inla_diagonal > 0, ,
                                   drop = FALSE]
  summary_lines <- c(
    summary_lines,
    "## Numerical stabilisation",
    "",
    if (!nrow(stabilised)) {
      paste0(
        "No component used a precision-matrix diagonal. Every one of the ",
        nrow(stabilisation_rows), " cross-fitted components was fitted with ",
        "INLA's defaults."
      )
    } else {
      c(
        paste0(
          "The following components were fitted with `control.inla(diagonal=)`, ",
          "which adds a constant to the diagonal of the precision matrix so the ",
          "Cholesky factorisation inside `inla.qsample` stays defined. This is a ",
          "property of the solver, not of the scientific model: no formula, ",
          "prior, spatial fold, draw count, neighbourhood definition or ",
          "threshold differs from an unstabilised fit, and no other component is ",
          "affected."
        ),
        "",
        "| component | diagonal |",
        "|---|---|",
        paste0(
          "| `", stabilised$model, "` | ",
          format(stabilised$inla_diagonal, scientific = TRUE), " |"
        )
      )
    },
    ""
  )
}

if (length(missing_inputs)) {
  summary_lines <- c(
    summary_lines,
    "## Declared inputs that were not present",
    "",
    paste0("- `", missing_inputs, "`"),
    ""
  )
}

if (!is.na(produced_count) && produced_count < nrow(output_manifest)) {
  summary_lines <- c(
    summary_lines,
    "## Outputs this run did not produce",
    "",
    paste0(
      "This run wrote ", produced_count, " of the ", nrow(output_manifest),
      " declared output files. The rest were already on disk when it started — ",
      "committed results, or artifacts of an earlier run — and are hashed here ",
      "for completeness, not claimed as this run's work. The ",
      "`produced_in_run` column of `output_manifest.csv` marks each one."
    ),
    ""
  )
}

summary_lines <- c(
  summary_lines,
  "## Output hashes",
  "",
  "| path | bytes | sha256 | produced by this run |",
  "|---|---:|---|---|",
  if (nrow(output_manifest)) {
    paste0(
      "| `", output_manifest$path, "` | ", format(output_manifest$bytes, trim = TRUE),
      " | `", output_manifest$sha256, "` | ",
      ifelse(is.na(output_manifest$produced_in_run), "unknown",
             ifelse(output_manifest$produced_in_run, "yes", "no")),
      " |"
    )
  } else {
    "| _no outputs recorded_ | | | |"
  },
  "",
  "## Input hashes",
  "",
  "| path | bytes | sha256 |",
  "|---|---:|---|",
  if (nrow(input_manifest)) {
    paste0(
      "| `", input_manifest$path, "` | ", format(input_manifest$bytes, trim = TRUE),
      " | `", input_manifest$sha256, "` |"
    )
  } else {
    "| _no inputs recorded_ | | |"
  }
)

rp_write_lines_atomic(
  summary_lines, file.path(report_dir, "reproduction_summary.md")
)

cat("Reproducibility report written to ", normalizePath(report_dir), "\n", sep = "")
