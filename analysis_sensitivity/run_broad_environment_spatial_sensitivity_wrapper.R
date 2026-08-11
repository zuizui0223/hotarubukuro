#!/usr/bin/env Rscript

# Runtime patch for the temporary Broad finalization sensitivity. The original
# experimental script initialized a one-row data.frame before assigning
# observation-length fixed-effect columns. This wrapper changes only that line
# and delegates all model logic and command-line arguments to the original file.

args <- commandArgs(trailingOnly = TRUE)
source_path <- "analysis_sensitivity/run_broad_environment_spatial_sensitivity.R"
if (!file.exists(source_path)) stop("Missing source script: ", source_path, call. = FALSE)
text <- readLines(source_path, warn = FALSE, encoding = "UTF-8")
old <- "  out <- data.frame(intercept = 1)"
new <- "  out <- data.frame(intercept = rep(1, nrow(d)))"
if (sum(text == old) != 1L) {
  stop("Expected exactly one fixed-effect initialization line; found ", sum(text == old), call. = FALSE)
}
text[text == old] <- new
tmp <- tempfile(fileext = ".R")
on.exit(unlink(tmp), add = TRUE)
writeLines(text, tmp, useBytes = TRUE)
status <- system2("Rscript", c(tmp, args))
if (!identical(status, 0L)) quit(status = status)
