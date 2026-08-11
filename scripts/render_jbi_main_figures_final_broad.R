#!/usr/bin/env Rscript

# Execute the existing journal-width renderer while routing its core Figure 2
# construction through the finalized Broad observation-level model adapter.

source_path <- "scripts/render_jbi_main_figures.R"
if (!file.exists(source_path)) stop("Missing current figure renderer: ", source_path, call. = FALSE)
text <- readLines(source_path, warn = FALSE, encoding = "UTF-8")
old <- 'source("scripts/build_jbi_figure_bundle.R")'
new <- 'source("scripts/build_jbi_figure_bundle_final_broad.R")'
hits <- which(text == old)
if (length(hits) != 1L) {
  stop("Expected one core-builder source line; found ", length(hits), call. = FALSE)
}
text[hits] <- new

tmp <- tempfile(fileext = ".R")
on.exit(unlink(tmp), add = TRUE)
writeLines(text, tmp, useBytes = TRUE)
sys.source(tmp, envir = .GlobalEnv, keep.source = FALSE)
