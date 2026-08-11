#!/usr/bin/env Rscript

# Runtime patch for the temporary comprehensive Broad audit. The audit source
# initializes a one-row fixed-effect data.frame before assigning observation-
# length columns. This wrapper changes only that initialization and delegates
# every other model, fold, prior and decision rule to the original script.

args <- commandArgs(trailingOnly = TRUE)
source_path <- "scripts/run_broad_environment_spatial_audit.R"
if (!file.exists(source_path)) stop("Missing source script: ", source_path, call. = FALSE)
text <- readLines(source_path, warn = FALSE, encoding = "UTF-8")
old <- "    fixed <- data.frame(intercept = 1, stringsAsFactors = FALSE)"
new <- "    fixed <- data.frame(intercept = rep(1, nrow(data)), stringsAsFactors = FALSE)"
if (sum(text == old) != 1L) {
  stop("Expected exactly one fixed-effect initialization line; found ", sum(text == old), call. = FALSE)
}
text[text == old] <- new
tmp <- tempfile(fileext = ".R")
on.exit(unlink(tmp), add = TRUE)
writeLines(text, tmp, useBytes = TRUE)
status <- system2("Rscript", c(tmp, args))
if (!identical(status, 0L)) quit(status = status)
