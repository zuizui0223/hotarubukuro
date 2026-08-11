#!/usr/bin/env Rscript

# Runtime correction wrapper for the temporary comprehensive Broad audit.
# It fixes three plumbing defects without changing any model, fold, prior,
# variable definition or decision rule:
#   1. the fixed-effect data frame must have one row per observation;
#   2. forest_fraction is keyed by the current 1-km cell id rather than the
#      observation-level exact_site_id; and
#   3. the repeated-observation IID effect must use the same 1-km analysis cell,
#      because only 38 exact observation sites repeat whereas 1,305 cells
#      contain the 1,922 observations used by the current paper.

args <- commandArgs(trailingOnly = TRUE)
source_path <- "scripts/run_broad_environment_spatial_audit.R"
if (!file.exists(source_path)) stop("Missing source script: ", source_path, call. = FALSE)
text <- paste(readLines(source_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

old_fixed <- "    fixed <- data.frame(intercept = 1, stringsAsFactors = FALSE)"
new_fixed <- "    fixed <- data.frame(intercept = rep(1, nrow(data)), stringsAsFactors = FALSE)"
if (length(gregexpr(old_fixed, text, fixed = TRUE)[[1]]) != 1L ||
    gregexpr(old_fixed, text, fixed = TRUE)[[1]][1] < 0L) {
  stop("Expected exactly one fixed-effect initialization", call. = FALSE)
}
text <- sub(old_fixed, new_fixed, text, fixed = TRUE)

old_join <- paste(
  "landscape <- landscape[!duplicated(landscape$exact_site_id), c(\"exact_site_id\", \"forest_fraction\")]",
  "raw$.input_order <- seq_len(nrow(raw))",
  "raw <- merge(raw, landscape, by = \"exact_site_id\", all.x = TRUE, sort = FALSE)",
  "raw <- raw[order(raw$.input_order), , drop = FALSE]",
  "raw$.input_order <- NULL",
  sep = "\n"
)
new_join <- paste(
  "landscape <- landscape[!duplicated(landscape$exact_site_id), c(\"exact_site_id\", \"forest_fraction\")]",
  "names(landscape)[names(landscape) == \"exact_site_id\"] <- \"cell_id_1km\"",
  "raw$cell_id_1km <- sprintf(\"cell-1km-%d_%d\", floor(as.numeric(raw$x_km)), floor(as.numeric(raw$y_km)))",
  "raw$.input_order <- seq_len(nrow(raw))",
  "raw <- merge(raw, landscape, by = \"cell_id_1km\", all.x = TRUE, sort = FALSE)",
  "raw <- raw[order(raw$.input_order), , drop = FALSE]",
  "raw$.input_order <- NULL",
  "if (any(!is.finite(raw$forest_fraction))) stop(\"Forest fraction did not map to every current 1-km observation cell.\", call. = FALSE)",
  sep = "\n"
)
if (!grepl(old_join, text, fixed = TRUE)) {
  stop("Expected forest-join block was not found", call. = FALSE)
}
text <- sub(old_join, new_join, text, fixed = TRUE)

old_iid <- "fixed$site_iid <- as.integer(factor(data$exact_site_id, levels = unique(data$exact_site_id)))"
new_iid <- "fixed$site_iid <- as.integer(factor(data$cell_id_1km, levels = unique(data$cell_id_1km)))"
if (!grepl(old_iid, text, fixed = TRUE)) {
  stop("Expected exact-site IID line was not found", call. = FALSE)
}
text <- sub(old_iid, new_iid, text, fixed = TRUE)

tmp <- tempfile(fileext = ".R")
on.exit(unlink(tmp), add = TRUE)
writeLines(strsplit(text, "\n", fixed = TRUE)[[1]], tmp, useBytes = TRUE)
status <- system2("Rscript", c(tmp, args))
if (!identical(status, 0L)) quit(status = status)
