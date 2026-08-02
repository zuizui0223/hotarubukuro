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
  paste0("- outputs recorded: ", nrow(output_manifest)),
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

if (length(missing_inputs)) {
  summary_lines <- c(
    summary_lines,
    "## Declared inputs that were not present",
    "",
    paste0("- `", missing_inputs, "`"),
    ""
  )
}

summary_lines <- c(
  summary_lines,
  "## Output hashes",
  "",
  "| path | bytes | sha256 |",
  "|---|---:|---|",
  if (nrow(output_manifest)) {
    paste0(
      "| `", output_manifest$path, "` | ", format(output_manifest$bytes, trim = TRUE),
      " | `", output_manifest$sha256, "` |"
    )
  } else {
    "| _no outputs recorded_ | | |"
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
