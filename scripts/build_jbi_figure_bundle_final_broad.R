#!/usr/bin/env Rscript

# Adapter for the current JBI figure builder. Figure 2 has two deliberately
# distinct evidence layers: panels A/D use the finalized observation-level
# Broad models, whereas panels B/C retain the separately locked cross-fitted
# cell-level natural reference used by Main 3. Figure 4D is replaced by the
# final 10,000-map human-context adjudication so that the figure shows the
# matching-sensitive 5-km settlement result rather than superseded family-wise
# maxT diagnostics.

source_path <- "scripts/build_jbi_figure_bundle.R"
if (!file.exists(source_path)) stop("Missing core figure builder: ", source_path, call. = FALSE)
text <- paste(readLines(source_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

replace_once <- function(text, old, new) {
  hits <- gregexpr(old, text, fixed = TRUE)[[1]]
  n_hits <- if (length(hits) == 1L && hits[[1]] < 0L) 0L else length(hits)
  if (n_hits != 1L) {
    stop("Expected exactly one builder match for: ", old, "; found ", n_hits, call. = FALSE)
  }
  sub(old, new, text, fixed = TRUE)
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

# Add the small, provenance-locked final human-context summary to the source
# manifest without requiring the 5.6-MB audit artifact at figure-render time.
text <- replace_once(
  text,
  paste0(
    "  \"results/ecological_v22_did_human_context/did_contrast_summary.csv\"\n",
    ")"
  ),
  paste0(
    "  \"results/ecological_v22_did_human_context/did_contrast_summary.csv\",\n",
    "  \"reproducibility/human_context_figure4_final_summary_2026-08-11.csv\"\n",
    ")"
  )
)
text <- replace_once(
  text,
  "did_summary <- hb_read_csv(required_inputs[[14]])",
  paste0(
    "did_summary <- hb_read_csv(required_inputs[[14]])\n",
    "human_final <- hb_read_csv(required_inputs[[15]])"
  )
)

start_marker <- "human <- rbind("
end_marker <- "  ggplot2::theme(plot.margin = ggplot2::margin(7, 35, 7, 7))"
start <- regexpr(start_marker, text, fixed = TRUE)[[1]]
end_start <- regexpr(end_marker, text, fixed = TRUE)[[1]]
if (start < 0L || end_start < start) stop("Could not locate Figure 4D replacement block.", call. = FALSE)
end <- end_start + nchar(end_marker) - 1L
new_human_panel <- paste0(
  "human_final$observed <- read_num(human_final$observed_contrast)\n",
  "human_final$lower <- read_num(human_final$lower_95)\n",
  "human_final$upper <- read_num(human_final$upper_95)\n",
  "human_final$maxT <- read_num(human_final$global_maxT_FWER_p)\n",
  "human_final$natural_model_label <- factor(\n",
  "  ifelse(human_final$natural_model == 'current_four_pc', 'Current multiscale natural model', 'Direct final-eight-axis natural model'),\n",
  "  levels = c('Direct final-eight-axis natural model', 'Current multiscale natural model')\n",
  ")\n",
  "human_final$matching_label <- factor(\n",
  "  ifelse(human_final$environment_matching == 'current_four_pc', 'Predeclared four-PC matching', 'Support-matched eight-axis matching'),\n",
  "  levels = c('Predeclared four-PC matching', 'Support-matched eight-axis matching')\n",
  ")\n",
  "human_final$scenario_label <- factor(\n",
  "  paste(human_final$matching_label, human_final$natural_model_label, sep = '\\n'),\n",
  "  levels = rev(c(\n",
  "    'Predeclared four-PC matching\\nCurrent multiscale natural model',\n",
  "    'Predeclared four-PC matching\\nDirect final-eight-axis natural model',\n",
  "    'Support-matched eight-axis matching\\nCurrent multiscale natural model',\n",
  "    'Support-matched eight-axis matching\\nDirect final-eight-axis natural model'\n",
  "  ))\n",
  ")\n",
  "human_final$maxT_label <- sprintf('global maxT P=%.3f', human_final$maxT)\n",
  "fig4d <- ggplot2::ggplot(\n",
  "  human_final, ggplot2::aes(x = observed, y = scenario_label, colour = matching_label, shape = natural_model_label)\n",
  ") +\n",
  "  ggplot2::geom_vline(xintercept = 0, linetype = 'dashed', colour = mid_grey) +\n",
  "  ggplot2::geom_errorbar(\n",
  "    ggplot2::aes(xmin = lower, xmax = upper), width = 0, linewidth = 0.55, orientation = 'y'\n",
  "  ) +\n",
  "  ggplot2::geom_point(size = 2.0) +\n",
  "  ggplot2::geom_text(\n",
  "    ggplot2::aes(label = maxT_label), hjust = -0.03, size = 1.8, colour = mid_grey\n",
  "  ) +\n",
  "  ggplot2::scale_colour_manual(values = c(\n",
  "    'Predeclared four-PC matching' = white_like,\n",
  "    'Support-matched eight-axis matching' = pigmented\n",
  "  )) +\n",
  "  ggplot2::scale_shape_manual(values = c(\n",
  "    'Current multiscale natural model' = 16,\n",
  "    'Direct final-eight-axis natural model' = 17\n",
  "  )) +\n",
  "  ggplot2::coord_cartesian(\n",
  "    xlim = c(min(human_final$lower) - 0.01, max(human_final$observed) + 0.18), clip = 'off'\n",
  "  ) +\n",
  "  ggplot2::labs(\n",
  "    title = 'Settlement signal depends on natural matching',\n",
  "    subtitle = '5-km population contrast; 10,000 maps; global maxT across 11 mechanisms',\n",
  "    x = 'Candidate minus white-neighbour population rank', y = NULL, colour = NULL, shape = NULL\n",
  "  ) +\n",
  "  theme_publication(base_size = 7.0) +\n",
  "  ggplot2::theme(\n",
  "    legend.position = 'none',\n",
  "    axis.text.y = ggplot2::element_text(size = 5.9),\n",
  "    plot.margin = ggplot2::margin(7, 46, 7, 7)\n",
  "  )"
)
text <- paste0(substr(text, 1L, start - 1L), new_human_panel, substr(text, end + 1L, nchar(text)))

# Replace the superseded human numerical lock with the final global-family
# quantities used in Figure 4D.
old_metrics <- paste0(
  "    \"candidate_fraction\", \"candidate_fraction_p\", \"population_5km_maxT_p\",\n",
  "    \"population_did_alignment_maxT_p\""
)
new_metrics <- paste0(
  "    \"candidate_fraction\", \"candidate_fraction_p\",\n",
  "    \"population_5km_primary_global_maxT_p\",\n",
  "    \"population_5km_primary_final8model_global_maxT_p\",\n",
  "    \"population_5km_final8match_currentmodel_global_maxT_p\",\n",
  "    \"population_5km_final8match_final8model_global_maxT_p\""
)
text <- replace_once(text, old_metrics, new_metrics)
old_values <- paste0(
  "    read_num(primary_isolate$observed_value[primary_isolate$metric == \"candidate_fraction\"]),\n",
  "    read_num(primary_isolate$empirical_p[primary_isolate$metric == \"candidate_fraction\"]),\n",
  "    read_num(population_summary$maxT_FWER_p[population_summary$feature == \"population_5km_rank\"]),\n",
  "    read_num(did_summary$maxT_FWER_p[did_summary$feature == \"did_aligned_population_score\"])"
)
new_values <- paste0(
  "    read_num(primary_isolate$observed_value[primary_isolate$metric == \"candidate_fraction\"]),\n",
  "    read_num(primary_isolate$empirical_p[primary_isolate$metric == \"candidate_fraction\"]),\n",
  "    read_num(human_final$global_maxT_FWER_p[human_final$configuration == \"current_model_current_graph\"]),\n",
  "    read_num(human_final$global_maxT_FWER_p[human_final$configuration == \"final8_model_current_graph\"]),\n",
  "    read_num(human_final$global_maxT_FWER_p[human_final$configuration == \"current_model_final8_support_matched\"]),\n",
  "    read_num(human_final$global_maxT_FWER_p[human_final$configuration == \"final8_model_final8_support_matched\"])"
)
text <- replace_once(text, old_values, new_values)

# Make the panel-source description explicit.
text <- replace_once(
  text,
  '"current event-null, candidate and post-selection human-context outputs"',
  '"current event-null/candidates plus final 10,000-map global human-context adjudication"'
)

tmp <- tempfile(fileext = ".R")
on.exit(unlink(tmp), add = TRUE)
writeLines(strsplit(text, "\n", fixed = TRUE)[[1]], tmp, useBytes = TRUE)
target <- parent.frame()
sys.source(tmp, envir = target, keep.source = FALSE)

# Preserve the exact final Broad and human data actually used after the adapter.
if (exists("fixed", envir = target, inherits = FALSE) && exists("data_dir", envir = target, inherits = FALSE)) {
  fixed_used <- get("fixed", envir = target, inherits = FALSE)
  data_dir_used <- get("data_dir", envir = target, inherits = FALSE)
  utils::write.csv(fixed_used, file.path(data_dir_used, "figure2_final_observation_fixed_effects.csv"), row.names = FALSE)
}
if (exists("range_data", envir = target, inherits = FALSE) && exists("data_dir", envir = target, inherits = FALSE)) {
  range_used <- get("range_data", envir = target, inherits = FALSE)
  data_dir_used <- get("data_dir", envir = target, inherits = FALSE)
  utils::write.csv(range_used, file.path(data_dir_used, "figure2_final_observation_spatial_ranges.csv"), row.names = FALSE)
}
if (exists("human_final", envir = target, inherits = FALSE) && exists("data_dir", envir = target, inherits = FALSE)) {
  human_used <- get("human_final", envir = target, inherits = FALSE)
  data_dir_used <- get("data_dir", envir = target, inherits = FALSE)
  utils::write.csv(human_used, file.path(data_dir_used, "figure4_final_human_context_scenarios.csv"), row.names = FALSE)
}
