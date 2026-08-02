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
