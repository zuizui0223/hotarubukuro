#!/usr/bin/env Rscript

# Matrix-job entry point for the environmental interaction screen.
#
# The core screen supports both responses in one invocation. GitHub Actions
# runs one response per matrix job to keep 96 INLA fits within a practical
# wall-clock limit. The frozen-model lock table in the core script contains
# targets for both responses, so this wrapper narrows that table to the response
# requested by --outcome before evaluating the core. No model formula, data,
# result, threshold, or comparison rule is altered.

core_path <- "scripts/run_environment_interaction_inla_screen.R"
if (!file.exists(core_path)) stop("Core interaction screen not found: ", core_path, call. = FALSE)

code <- readLines(core_path, warn = FALSE, encoding = "UTF-8")
needle <- grep("^lock_rows <- lapply\\(seq_len\\(nrow\\(lock_targets\\)\\)", code)
if (length(needle) != 1L) {
  stop("Unable to locate the frozen-lock evaluation point in ", core_path, call. = FALSE)
}

filter_line <- paste0(
  "lock_targets <- lock_targets[lock_targets$outcome %in% names(outcomes), ",
  ", drop = FALSE]"
)
code <- append(code, filter_line, after = needle - 1L)

temporary <- tempfile("environment-interaction-matrix-", fileext = ".R")
on.exit(unlink(temporary), add = TRUE)
writeLines(code, temporary, useBytes = TRUE)

sys.source(temporary, envir = globalenv(), keep.source = FALSE)
