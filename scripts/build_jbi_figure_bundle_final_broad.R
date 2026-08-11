#!/usr/bin/env Rscript

# Thin adapter around the current JBI figure builder.
#
# Figure 2 intentionally combines two evidence layers:
#   A/D = finalized observation-level Broad INLA-SPDE coefficients/ranges;
#   B/C = separately locked cross-fitted cell-level natural reference.
#
# Figure 4 A-C retain the predeclared event and natural-null display. Panel D is
# overwritten after the core builder finishes so it uses the final 10,000-map,
# one-global-maxT human-context adjudication rather than superseded family-wise
# population/DID summaries. Doing this post-build avoids brittle replacement of
# the core Figure 4 code and keeps the reusable core builder unchanged.

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

# Observation-level Figure 2 locks. Their schema and model labels deliberately
# match the core builder, so only paths and one additional term label change.
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
writeLines(strsplit(text, "\n", fixed = TRUE)[[1]], tmp, useBytes = TRUE)
sys.source(tmp, envir = .GlobalEnv, keep.source = FALSE)

# Preserve the exact finalized observation-level data used in Figure 2.
utils::write.csv(
  fixed,
  file.path(data_dir, "figure2_final_observation_fixed_effects.csv"),
  row.names = FALSE
)
utils::write.csv(
  range_data,
  file.path(data_dir, "figure2_final_observation_spatial_ranges.csv"),
  row.names = FALSE
)

# Final human-context evidence is a compact, provenance-locked summary of the
# successful 10,000-map high-rep audit. Human variables never enter candidate
# selection; this panel only visualizes how the post-selection 5-km population
# contrast changes across two defensible natural models and two response-blind
# environmental matching definitions.
human_path <- "reproducibility/human_context_figure4_final_summary_2026-08-11.csv"
if (!file.exists(human_path)) stop("Missing final human-context Figure 4 lock: ", human_path, call. = FALSE)
human_final <- hb_read_csv(human_path)
required_human <- c(
  "configuration", "natural_model", "environment_matching", "observed_contrast",
  "null_mean", "lower_95", "upper_95", "directional_p", "global_maxT_FWER_p",
  "n_null_draws", "n_candidates"
)
missing_human <- setdiff(required_human, names(human_final))
if (length(missing_human)) stop("Final human-context summary lacks: ", paste(missing_human, collapse = ", "), call. = FALSE)
if (nrow(human_final) != 4L || any(as.integer(human_final$n_null_draws) != 10000L)) {
  stop("Final human-context Figure 4 lock must contain four 10,000-map scenarios.", call. = FALSE)
}

human_final$observed <- read_num(human_final$observed_contrast)
human_final$lower <- read_num(human_final$lower_95)
human_final$upper <- read_num(human_final$upper_95)
human_final$maxT <- read_num(human_final$global_maxT_FWER_p)
human_final$natural_model_label <- factor(
  ifelse(
    human_final$natural_model == "current_four_pc",
    "Current multiscale natural model",
    "Direct final-eight-axis natural model"
  ),
  levels = c("Direct final-eight-axis natural model", "Current multiscale natural model")
)
human_final$matching_label <- factor(
  ifelse(
    human_final$environment_matching == "current_four_pc",
    "Predeclared four-PC matching",
    "Support-matched eight-axis matching"
  ),
  levels = c("Predeclared four-PC matching", "Support-matched eight-axis matching")
)
scenario_levels <- c(
  "Predeclared four-PC matching\nCurrent multiscale natural model",
  "Predeclared four-PC matching\nDirect final-eight-axis natural model",
  "Support-matched eight-axis matching\nCurrent multiscale natural model",
  "Support-matched eight-axis matching\nDirect final-eight-axis natural model"
)
human_final$scenario_label <- factor(
  paste(human_final$matching_label, human_final$natural_model_label, sep = "\n"),
  levels = rev(scenario_levels)
)
human_final$maxT_label <- sprintf("global maxT P=%.3f", human_final$maxT)

fig4d <- ggplot2::ggplot(
  human_final,
  ggplot2::aes(
    x = observed, y = scenario_label,
    colour = matching_label, shape = natural_model_label
  )
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = mid_grey) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower, xmax = upper),
    width = 0, linewidth = 0.55, orientation = "y"
  ) +
  ggplot2::geom_point(size = 2.0) +
  ggplot2::geom_text(
    ggplot2::aes(label = maxT_label),
    hjust = -0.03, size = 1.8, colour = mid_grey
  ) +
  ggplot2::scale_colour_manual(values = c(
    "Predeclared four-PC matching" = white_like,
    "Support-matched eight-axis matching" = pigmented
  )) +
  ggplot2::scale_shape_manual(values = c(
    "Current multiscale natural model" = 16,
    "Direct final-eight-axis natural model" = 17
  )) +
  ggplot2::coord_cartesian(
    xlim = c(min(human_final$lower) - 0.01, max(human_final$observed) + 0.18),
    clip = "off"
  ) +
  ggplot2::labs(
    title = "Settlement signal depends on natural matching",
    subtitle = "5-km population contrast; 10,000 maps; one global maxT family",
    x = "Candidate minus white-neighbour population rank",
    y = NULL, colour = NULL, shape = NULL
  ) +
  theme_publication(base_size = 7.0) +
  ggplot2::theme(
    legend.position = "none",
    axis.text.y = ggplot2::element_text(size = 5.9),
    plot.margin = ggplot2::margin(7, 46, 7, 7)
  )

figure_4 <- (tag_panel(fig4a, "A") | tag_panel(fig4b, "B")) /
  (tag_panel(fig4c, "C") | tag_panel(fig4d, "D"))
new_figure4 <- save_figure(
  figure_4, "Figure_4_calibrated_local_departures", 4, 7.2, 7.35
)
new_figure4$narrative_job <- "calibrated ecological departures and provenance follow-up"
figure_manifest <- rbind(
  figure_manifest[as.integer(figure_manifest$figure) != 4L, , drop = FALSE],
  new_figure4
)
format_order <- match(as.character(figure_manifest$format), c("PNG", "PDF"))
figure_manifest <- figure_manifest[order(as.integer(figure_manifest$figure), format_order), , drop = FALSE]
utils::write.csv(
  figure_manifest, file.path(output_dir, "figure_manifest.csv"), row.names = FALSE
)
utils::write.csv(
  human_final,
  file.path(data_dir, "figure4_final_human_context_scenarios.csv"),
  row.names = FALSE
)

# Replace superseded population/DID family-wise locks with the final single
# global-family adjudication. The core event count/fraction lock is unchanged.
lock_path <- file.path(output_dir, "figure_numerical_lock.csv")
figure_lock <- hb_read_csv(lock_path)
figure_lock <- figure_lock[
  !figure_lock$metric %in% c("population_5km_maxT_p", "population_did_alignment_maxT_p"),
  , drop = FALSE
]
final_human_lock <- data.frame(
  metric = c(
    "population_5km_primary_global_maxT_p",
    "population_5km_primary_final8model_global_maxT_p",
    "population_5km_final8match_currentmodel_global_maxT_p",
    "population_5km_final8match_final8model_global_maxT_p"
  ),
  value = c(
    human_final$maxT[human_final$configuration == "current_model_current_graph"],
    human_final$maxT[human_final$configuration == "final8_model_current_graph"],
    human_final$maxT[human_final$configuration == "current_model_final8_support_matched"],
    human_final$maxT[human_final$configuration == "final8_model_final8_support_matched"]
  ),
  stringsAsFactors = FALSE
)
if (any(lengths(lapply(final_human_lock$value, length)) != 1L) || any(!is.finite(final_human_lock$value))) {
  stop("Could not construct the four final human-context numerical locks.", call. = FALSE)
}
figure_lock <- rbind(figure_lock, final_human_lock)
utils::write.csv(figure_lock, lock_path, row.names = FALSE)

# Append the human lock to the source manifest after the core builder and update
# Figure 4's panel-source description.
source_manifest_path <- file.path(output_dir, "figure_source_manifest.csv")
source_manifest <- hb_read_csv(source_manifest_path)
if (!human_path %in% source_manifest$path) {
  source_manifest <- rbind(
    source_manifest,
    data.frame(
      path = human_path,
      size_bytes = as.numeric(file.info(human_path)$size),
      sha256 = sha256_file(human_path),
      stringsAsFactors = FALSE
    )
  )
}
utils::write.csv(source_manifest, source_manifest_path, row.names = FALSE)

panel_path <- file.path(data_dir, "panel_source_index.csv")
panel_source <- hb_read_csv(panel_path)
panel_source$generated_from[as.integer(panel_source$figure) == 4L] <-
  "current event-null/candidates plus final 10,000-map global human-context adjudication"
utils::write.csv(panel_source, panel_path, row.names = FALSE)

cat("Final Broad + human-context figure adapter complete.\n")
