#!/usr/bin/env Rscript

# Backward-compatible entry point for the main analysis preparation pipeline.
# The implementation lives in scripts/run_analysis.R and R/analysis_core.R so
# the R script and R Markdown report cannot silently diverge again.

file_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(file_argument)) {
  normalizePath(sub("^--file=", "", file_argument[1L]), winslash = "/", mustWork = TRUE)
} else {
  normalizePath("Code_S3.R", winslash = "/", mustWork = TRUE)
}
project_root <- dirname(script_path)

source(file.path(project_root, "scripts", "run_analysis.R"), local = TRUE)

if (sys.nframe() == 0L) {
  run_analysis_cli(commandArgs(trailingOnly = TRUE), project_root = project_root)
}
