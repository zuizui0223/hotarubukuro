#!/usr/bin/env Rscript

# Adapter for the current JBI figure builder. Figure 2 has two deliberately
# distinct evidence layers: panels A/D use the finalized observation-level
# Broad models, whereas panels B/C retain the separately locked cross-fitted
# cell-level natural reference used by Main 3. This adapter changes only the
# coefficient/range inputs and adds the retained thermal interaction label.

source_path <- "scripts/build_jbi_figure_bundle.R"
if (!file.exists(source_path)) stop("Missing core figure builder: ", source_path, call. = FALSE)
text <- readLines(source_path, warn = FALSE, encoding = "UTF-8")

replace_once <- function(text, old, new) {
  hits <- which(grepl(old, text, fixed = TRUE))
  if (length(hits) != 1L) {
    stop("Expected exactly one builder match for: ", old, "; found ", length(hits), call. = FALSE)
  }
  text[hits] <- sub(old, new, text[hits], fixed = TRUE)
  text
}

text <- replace_once(
  text,
  "results/ecological_v11_pigmentation_hurdle/pigmentation_hurdle_inla_fixed_effects.csv",
  "reproducibility/broad_environment_spatial_final_fixed_effects_2026-08-11.csv"
)
text <- replace_once(
  text,
  "results/ecological_v11_pigmentation_hurdle/pigmentation_hurdle_inla_hyperparameters.csv",
  "reproducibility/broad_environment_spatial_final_hyperparameters_2026-08-11.csv"
)
text <- replace_once(
  text,
  "  env_RSDS = \"Solar radiation\"",
  paste0(
    "  env_RSDS = \"Solar radiation\",\n",
    "  env_Temperature_PC1_x_TemperatureSeasonality = \"Temperature × seasonality\""
  )
)

tmp <- tempfile(fileext = ".R")
on.exit(unlink(tmp), add = TRUE)
writeLines(text, tmp, useBytes = TRUE)
target <- parent.frame()
sys.source(tmp, envir = target, keep.source = FALSE)

# Preserve the exact panel-A and panel-D data actually used after the adapter.
# These files make the mixed evidence architecture explicit and independently
# auditable without changing the core builder's numerical lock for Main 2/3.
if (exists("fixed", envir = target, inherits = FALSE) && exists("data_dir", envir = target, inherits = FALSE)) {
  fixed_used <- get("fixed", envir = target, inherits = FALSE)
  data_dir_used <- get("data_dir", envir = target, inherits = FALSE)
  utils::write.csv(
    fixed_used,
    file.path(data_dir_used, "figure2_final_observation_fixed_effects.csv"),
    row.names = FALSE
  )
}
if (exists("range_data", envir = target, inherits = FALSE) && exists("data_dir", envir = target, inherits = FALSE)) {
  range_used <- get("range_data", envir = target, inherits = FALSE)
  data_dir_used <- get("data_dir", envir = target, inherits = FALSE)
  utils::write.csv(
    range_used,
    file.path(data_dir_used, "figure2_final_observation_spatial_ranges.csv"),
    row.names = FALSE
  )
}
