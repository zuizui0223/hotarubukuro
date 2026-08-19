# Figure 4: continuous same-colour isolation and its natural-map guardrails.
# This file is sourced after the common figure builder has defined map/theme
# helpers and after add_100km_scale() has been defined by the journal renderer.

continuous_root <- "results/continuous_colour_isolation_human_context"
continuous_cell_path <- file.path(
  continuous_root, "continuous_isolation_cell_metrics.csv"
)
continuous_summary_path <- file.path(
  continuous_root, "continuous_isolation_natural_null_summary.csv"
)
continuous_maxT_path <- file.path(
  continuous_root, "continuous_isolation_population_maxT.csv"
)
continuous_profile_path <- file.path(
  continuous_root, "continuous_isolation_population_profile.csv"
)
continuous_validation_path <- file.path(
  continuous_root, "continuous_isolation_validation.json"
)
continuous_paths <- c(
  continuous_cell_path, continuous_summary_path, continuous_maxT_path,
  continuous_profile_path, continuous_validation_path
)
missing_continuous <- continuous_paths[
  !file.exists(continuous_paths) | file.info(continuous_paths)$size <= 0
]
if (length(missing_continuous)) {
  stop(
    "Missing continuous-isolation Figure 4 input(s): ",
    paste(missing_continuous, collapse = ", "),
    call. = FALSE
  )
}

continuous_cells <- hb_read_csv(continuous_cell_path)
continuous_summary <- hb_read_csv(continuous_summary_path)
continuous_maxT <- hb_read_csv(continuous_maxT_path)
continuous_profile <- hb_read_csv(continuous_profile_path)

continuous_cells$longitude <- read_num(continuous_cells$longitude)
continuous_cells$latitude <- read_num(continuous_cells$latitude)
continuous_cells$same_colour_nn_km <- read_num(
  continuous_cells$same_colour_nn_km
)
continuous_cells$relative_isolation_nn <- read_num(
  continuous_cells$relative_isolation_nn
)
continuous_cells$population_5km_rank <- read_num(
  continuous_cells$population_5km_rank
)
continuous_cells$colour <- factor(
  as.character(continuous_cells$colour),
  levels = c("white", "pigmented"),
  labels = c("White", "Pigmented")
)
continuous_cells$is_pigmented <- continuous_cells$colour == "Pigmented"

within_colour_rank01 <- function(value, group) {
  out <- rep(NA_real_, length(value))
  for (level in unique(group)) {
    index <- which(group == level & is.finite(value))
    if (!length(index)) next
    if (length(index) == 1L) {
      out[index] <- 0.5
    } else {
      out[index] <- (rank(value[index], ties.method = "average") - 1) /
        (length(index) - 1)
    }
  }
  out
}
continuous_cells$relative_isolation_rank <- within_colour_rank01(
  continuous_cells$relative_isolation_nn,
  continuous_cells$colour
)

rho_label <- function(metric, colour, feature = "population_5km_rank") {
  hit <- continuous_summary$metric == metric &
    continuous_summary$feature == feature &
    continuous_summary$component == paste0(tolower(colour), "_rho") &
    continuous_summary$null_mode == "all_nondegenerate_maps"
  if (sum(hit) != 1L) {
    stop(
      "Expected one continuous summary row for ", metric, " / ", colour,
      call. = FALSE
    )
  }
  sprintf("%s  ρ = %.3f", colour, read_num(continuous_summary$observed_value[hit]))
}

# (a) National geometry. White cells establish the sampled frame; pigmented
# cells are sized by same-colour isolation and filled by local population rank.
fig4a <- base_japan_map() +
  ggplot2::geom_point(
    data = continuous_cells[!continuous_cells$is_pigmented, , drop = FALSE],
    ggplot2::aes(x = longitude, y = latitude),
    shape = 21, fill = "#D8DEE3", colour = paper,
    stroke = 0.15, size = 0.85, alpha = 0.66
  ) +
  ggplot2::geom_point(
    data = continuous_cells[continuous_cells$is_pigmented, , drop = FALSE],
    ggplot2::aes(
      x = longitude, y = latitude,
      size = same_colour_nn_km,
      fill = population_5km_rank
    ),
    shape = 21, colour = ink, stroke = 0.18, alpha = 0.86
  ) +
  ggplot2::scale_size_continuous(
    name = "Nearest pigmented\ndistance (km)",
    range = c(0.75, 4.1),
    breaks = c(2, 10, 25, 50)
  ) +
  ggplot2::scale_fill_gradient(
    name = "5-km population\nexposure rank",
    low = "#F2E8ED", high = pigmented,
    limits = c(0, 1), oob = scales::squish
  ) +
  ggplot2::labs(
    title = "Pigmented isolation across the sampled geography",
    subtitle = "674 pigmented cells; white cells show the complete flower-cell frame"
  ) +
  ggplot2::theme(
    legend.box = "vertical",
    legend.key.height = grid::unit(3.0, "mm"),
    plot.title = ggplot2::element_text(size = 8.1)
  )
fig4a <- add_100km_scale(fig4a)

# (b) Rank-space display of the sampling-density-corrected relationship. Rank
# y-values correspond directly to the Spearman estimand reported in the text.
fig4b <- ggplot2::ggplot(
  continuous_cells,
  ggplot2::aes(
    x = population_5km_rank,
    y = relative_isolation_rank,
    colour = colour
  )
) +
  ggplot2::geom_point(size = 0.85, alpha = 0.32) +
  ggplot2::geom_smooth(
    method = "lm", formula = y ~ x,
    se = FALSE, linewidth = 0.78
  ) +
  ggplot2::scale_colour_manual(
    values = c("White" = white_like, "Pigmented" = pigmented)
  ) +
  ggplot2::annotate(
    "text", x = 0.03, y = 0.96,
    label = rho_label("relative_same_to_any_nn", "Pigmented"),
    hjust = 0, vjust = 1, colour = pigmented, size = 2.45
  ) +
  ggplot2::annotate(
    "text", x = 0.03, y = 0.88,
    label = rho_label("relative_same_to_any_nn", "White"),
    hjust = 0, vjust = 1, colour = white_like, size = 2.45
  ) +
  ggplot2::labs(
    title = "Population exposure rises with pigmented isolation",
    subtitle = "Isolation scaled by nearest spacing of all flower cells",
    x = "Population exposure within 5 km (rank)",
    y = "Relative same-colour isolation (within-colour rank)",
    colour = "Flower-colour state"
  ) +
  theme_publication(base_size = 7.4) +
  ggplot2::theme(legend.position = "none")

# (c) Natural-map guardrails at the primary 5-km scale. The focal pigmented
# relationship and the colour difference are deliberately shown separately.
guardrail_keep <- continuous_summary$feature == "population_5km_rank" &
  continuous_summary$null_mode == "all_nondegenerate_maps" &
  continuous_summary$metric %in% c(
    "raw_same_colour_nn", "relative_same_to_any_nn"
  ) &
  continuous_summary$component %in% c("pigmented_rho", "rho_difference")
guardrail <- continuous_summary[guardrail_keep, , drop = FALSE]
guardrail$observed_value <- read_num(guardrail$observed_value)
guardrail$null_mean <- read_num(guardrail$null_mean)
guardrail$lower_95 <- read_num(guardrail$lower_95)
guardrail$upper_95 <- read_num(guardrail$upper_95)
guardrail$empirical_p <- read_num(guardrail$empirical_p)
guardrail$metric_label <- factor(
  guardrail$metric,
  levels = c("raw_same_colour_nn", "relative_same_to_any_nn"),
  labels = c("Raw isolation", "Relative isolation")
)
guardrail$component_label <- factor(
  guardrail$component,
  levels = c("pigmented_rho", "rho_difference"),
  labels = c("Pigmented ρ", "Pigmented − white ρ")
)
guardrail$display_label <- sprintf("P = %.4f", guardrail$empirical_p)
fig4c <- ggplot2::ggplot(
  guardrail,
  ggplot2::aes(x = component_label, y = observed_value)
) +
  ggplot2::geom_linerange(
    ggplot2::aes(ymin = lower_95, ymax = upper_95),
    linewidth = 0.75, colour = "#9AA5AE"
  ) +
  ggplot2::geom_point(
    ggplot2::aes(y = null_mean),
    shape = 21, fill = paper, colour = ink,
    size = 2.25, stroke = 0.45
  ) +
  ggplot2::geom_point(
    shape = 21, fill = pigmented, colour = ink,
    size = 2.65, stroke = 0.40
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = display_label),
    nudge_y = 0.035, size = 2.10, colour = mid_grey
  ) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = mid_grey) +
  ggplot2::facet_wrap(~metric_label, ncol = 2) +
  ggplot2::labs(
    title = "Natural geography explains the white contrast, not the pigmented excess",
    subtitle = "Filled: observed; open: natural mean; bars: natural 95% interval",
    x = NULL,
    y = "5-km isolation–population correlation"
  ) +
  theme_publication(base_size = 7.2) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 18, hjust = 1),
    legend.position = "none"
  )

# (d) Observed pigmented rho against the natural interval across population
# radii. The profile is shown for both raw and density-corrected isolation.
population_features <- c(
  "local_population_rank", "population_5km_rank",
  "population_10km_rank", "population_25km_rank",
  "population_50km_rank"
)
population_radius <- c(0, 5, 10, 25, 50)
profile_keep <- continuous_summary$feature %in% population_features &
  continuous_summary$null_mode == "all_nondegenerate_maps" &
  continuous_summary$component == "pigmented_rho" &
  continuous_summary$metric %in% c(
    "raw_same_colour_nn", "relative_same_to_any_nn"
  )
profile4 <- continuous_summary[profile_keep, , drop = FALSE]
profile4$radius_km <- population_radius[
  match(profile4$feature, population_features)
]
profile4$observed_value <- read_num(profile4$observed_value)
profile4$null_mean <- read_num(profile4$null_mean)
profile4$lower_95 <- read_num(profile4$lower_95)
profile4$upper_95 <- read_num(profile4$upper_95)
profile4$metric_label <- factor(
  profile4$metric,
  levels = c("raw_same_colour_nn", "relative_same_to_any_nn"),
  labels = c("Raw isolation", "Relative isolation")
)
fig4d <- ggplot2::ggplot(
  profile4,
  ggplot2::aes(x = radius_km, y = observed_value)
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower_95, ymax = upper_95),
    fill = light_grey, alpha = 0.80
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = null_mean),
    colour = "#8A969F", linewidth = 0.58, linetype = "dashed"
  ) +
  ggplot2::geom_line(colour = pigmented, linewidth = 0.82) +
  ggplot2::geom_point(
    shape = 21, fill = pigmented, colour = ink,
    size = 2.1, stroke = 0.35
  ) +
  ggplot2::facet_wrap(~metric_label, ncol = 2) +
  ggplot2::scale_x_continuous(breaks = population_radius) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dotted", colour = mid_grey) +
  ggplot2::labs(
    title = "Pigmented association exceeds natural expectation across scales",
    subtitle = "Observed line versus natural mean and 95% interval",
    x = "Population-exposure radius (km; 0 = focal cell)",
    y = "Pigmented Spearman ρ"
  ) +
  theme_publication(base_size = 7.2)

figure_4 <- (tag_panel(fig4a, "a") | tag_panel(fig4b, "b")) /
  (tag_panel(fig4c, "c") | tag_panel(fig4d, "d"))

# Write exact panel data and extend the source/numerical manifests. The old
# event-family panel data remain in the bundle as supplementary provenance.
utils::write.csv(
  continuous_cells,
  file.path(data_dir, "figure4_continuous_cell_metrics.csv"),
  row.names = FALSE
)
utils::write.csv(
  guardrail,
  file.path(data_dir, "figure4_continuous_natural_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  profile4,
  file.path(data_dir, "figure4_continuous_population_profile.csv"),
  row.names = FALSE
)

source_manifest_path <- file.path(output_dir, "figure_source_manifest.csv")
source_manifest_current <- hb_read_csv(source_manifest_path)
source_manifest_add <- data.frame(
  path = continuous_paths,
  size_bytes = as.numeric(file.info(continuous_paths)$size),
  sha256 = vapply(continuous_paths, sha256_file, character(1)),
  stringsAsFactors = FALSE
)
source_manifest_current <- source_manifest_current[
  !source_manifest_current$path %in% continuous_paths,
  , drop = FALSE
]
utils::write.csv(
  rbind(source_manifest_current, source_manifest_add),
  source_manifest_path,
  row.names = FALSE
)

lookup_continuous <- function(metric, component, field) {
  hit <- continuous_summary$metric == metric &
    continuous_summary$feature == "population_5km_rank" &
    continuous_summary$component == component &
    continuous_summary$null_mode == "all_nondegenerate_maps"
  if (sum(hit) != 1L) {
    stop("Missing continuous Figure 4 lock row: ", metric, " / ", component, call. = FALSE)
  }
  read_num(continuous_summary[[field]][hit])
}
figure_lock_path <- file.path(output_dir, "figure_numerical_lock.csv")
figure_lock <- hb_read_csv(figure_lock_path)
continuous_lock <- data.frame(
  metric = c(
    "continuous_pigmented_cells",
    "continuous_white_cells",
    "continuous_pigmented_median_isolation_km",
    "continuous_raw_population_5km_pigmented_rho",
    "continuous_raw_population_5km_natural_mean",
    "continuous_raw_population_5km_upper_p",
    "continuous_relative_population_5km_pigmented_rho",
    "continuous_relative_population_5km_natural_mean",
    "continuous_relative_population_5km_upper_p",
    "continuous_raw_population_5km_direct_difference",
    "continuous_relative_population_5km_direct_difference"
  ),
  value = c(
    sum(continuous_cells$is_pigmented),
    sum(!continuous_cells$is_pigmented),
    stats::median(
      continuous_cells$same_colour_nn_km[continuous_cells$is_pigmented],
      na.rm = TRUE
    ),
    lookup_continuous("raw_same_colour_nn", "pigmented_rho", "observed_value"),
    lookup_continuous("raw_same_colour_nn", "pigmented_rho", "null_mean"),
    lookup_continuous("raw_same_colour_nn", "pigmented_rho", "empirical_p"),
    lookup_continuous("relative_same_to_any_nn", "pigmented_rho", "observed_value"),
    lookup_continuous("relative_same_to_any_nn", "pigmented_rho", "null_mean"),
    lookup_continuous("relative_same_to_any_nn", "pigmented_rho", "empirical_p"),
    lookup_continuous("raw_same_colour_nn", "rho_difference", "observed_value"),
    lookup_continuous("relative_same_to_any_nn", "rho_difference", "observed_value")
  ),
  stringsAsFactors = FALSE
)
figure_lock <- figure_lock[!figure_lock$metric %in% continuous_lock$metric, , drop = FALSE]
utils::write.csv(
  rbind(figure_lock, continuous_lock),
  figure_lock_path,
  row.names = FALSE
)
