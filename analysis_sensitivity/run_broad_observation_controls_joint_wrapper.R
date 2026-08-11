#!/usr/bin/env Rscript

# Execution-only final guardrail. Fit the ecological reference model against a
# simultaneous DOY/year + image-QC adjustment, without re-running the already
# adjudicated environmental and barrier grids.

args <- commandArgs(trailingOnly = TRUE)
source_path <- "scripts/run_broad_environment_spatial_audit.R"
if (!file.exists(source_path)) stop("Missing source script: ", source_path, call. = FALSE)
text <- paste(readLines(source_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

replace_once <- function(text, old, new, label) {
  if (!grepl(old, text, fixed = TRUE)) stop("Patch anchor not found: ", label, call. = FALSE)
  sub(old, new, text, fixed = TRUE)
}

text <- replace_once(
  text,
  "    fixed <- data.frame(intercept = 1, stringsAsFactors = FALSE)",
  "    fixed <- data.frame(intercept = rep(1, nrow(data)), stringsAsFactors = FALSE)",
  "fixed-effect rows"
)

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
text <- replace_once(text, old_join, new_join, "forest-cell join")

anchor <- "outcomes_all <- list("
pruning <- paste(
  "state_specs <- state_specs[c('state_additive', 'state_observation_controls')]",
  "intensity_specs <- intensity_specs[c('intensity_thermal_variability', 'intensity_thermal_observation_controls')]",
  "spatial_specs <- spatial_specs[spatial_specs$spatial_spec == 'stationary_region', , drop = FALSE]",
  "",
  sep = "\n"
)
text <- replace_once(text, anchor, paste0(pruning, anchor), "outcome registry")
text <- replace_once(
  text,
  "model_specs = intensity_specs, reference_model = \"intensity_additive\",",
  "model_specs = intensity_specs, reference_model = \"intensity_thermal_variability\",",
  "intensity reference"
)

tmp <- tempfile(fileext = ".R")
on.exit(unlink(tmp), add = TRUE)
writeLines(strsplit(text, "\n", fixed = TRUE)[[1]], tmp, useBytes = TRUE)
status <- system2("Rscript", c(tmp, args))
if (!identical(status, 0L)) quit(status = status)
