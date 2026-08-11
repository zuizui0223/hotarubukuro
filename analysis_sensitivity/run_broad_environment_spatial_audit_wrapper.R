#!/usr/bin/env Rscript

# Runtime correction and scope-pruning wrapper for the temporary comprehensive
# Broad audit. It fixes three plumbing defects without changing any model,
# fold, prior, variable definition or decision rule:
#   1. the fixed-effect data frame must have one row per observation;
#   2. forest_fraction is keyed by the current 1-km cell id rather than the
#      observation-level exact_site_id; and
#   3. the repeated-observation IID effect must use the same 1-km analysis cell,
#      because only 38 exact observation sites repeat whereas 1,305 cells
#      contain the 1,922 observations used by the current paper.
#
# It also removes model variants already adjudicated in the successful exact
# VPD/SWB/interaction/barrier artifacts, leaving only the still-unresolved
# completeness guardrails: BIO6, BIO13, forest fraction, coast distance,
# DOY/year, image QC, their full extension and a 1-km-cell IID effect.

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

scope_anchor <- "outcomes_all <- list("
if (!grepl(scope_anchor, text, fixed = TRUE)) {
  stop("Expected outcome registry anchor was not found", call. = FALSE)
}
pruning <- paste(
  "state_specs <- state_specs[c(",
  "  'state_additive', 'state_add_bio6', 'state_add_bio13',",
  "  'state_add_forest', 'state_add_coast', 'state_observation_time',",
  "  'state_image_qc', 'state_full_extended'",
  ")]",
  "intensity_specs <- intensity_specs[c(",
  "  'intensity_additive', 'intensity_thermal_variability',",
  "  'intensity_thermal_add_bio6', 'intensity_thermal_add_bio13',",
  "  'intensity_thermal_add_forest', 'intensity_thermal_add_coast',",
  "  'intensity_thermal_observation_time', 'intensity_thermal_image_qc',",
  "  'intensity_thermal_full_extended'",
  ")]",
  "spatial_specs <- spatial_specs[spatial_specs$spatial_spec %in% c(",
  "  'stationary_region', 'stationary_region_site'",
  "), , drop = FALSE]",
  "",
  sep = "\n"
)
text <- sub(scope_anchor, paste0(pruning, scope_anchor), text, fixed = TRUE)

tmp <- tempfile(fileext = ".R")
on.exit(unlink(tmp), add = TRUE)
writeLines(strsplit(text, "\n", fixed = TRUE)[[1]], tmp, useBytes = TRUE)
status <- system2("Rscript", c(tmp, args))
if (!identical(status, 0L)) quit(status = status)
