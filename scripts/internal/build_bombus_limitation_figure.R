# Figure 3 replacement: national Bombus sensitivity and local limitation gate.
# This script runs after build_publication_figures_core.R and writes a new stem;
# the manuscript figure map points only to this limitation-gate figure.

source("R/pipeline_support.R")
hb_require_stage_packages("publication_figures")
output_dir <- file.path("manuscript", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

folds <- hb_read_csv(
  "results/ecological_v16_predictive_replication/predictive_replication_bombus_paired_contrast.csv"
)
gate <- hb_read_csv(
  "results/ecological_v17_bombus_limitation_gate/bombus_limitation_gate_summary.csv"
)
pairs <- hb_read_csv(
  "results/ecological_v17_bombus_limitation_gate/bombus_limitation_gate_pairs.csv"
)
null <- hb_read_csv(
  "results/ecological_v17_bombus_limitation_gate/bombus_limitation_gate_null.csv"
)
cells <- hb_read_csv(
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)

primary_pairs <- pairs[pairs$is_primary_gate, , drop = FALSE]
share <- cells$n_pigmented / pmax(cells$n_observations, 1)
paired <- data.frame(
  pair = seq_len(nrow(primary_pairs)),
  limited = share[primary_pairs$low_i],
  available = share[primary_pairs$high_i]
)
paired_long <- rbind(
  data.frame(pair = paired$pair, state = "Bombus-limited", share = paired$limited),
  data.frame(pair = paired$pair, state = "Bombus-available", share = paired$available)
)
paired_long$state <- factor(
  paired_long$state, levels = c("Bombus-limited", "Bombus-available")
)

p1 <- ggplot2::ggplot(folds, ggplot2::aes(x = factor(fold), y = AUC_improvement)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::labs(
    title = "National Bombus fingerprint adds little discrimination",
    x = "Held-out spatial fold", y = "AUC change"
  ) + ggplot2::theme_minimal(base_size = 9)

p2 <- ggplot2::ggplot(paired_long, ggplot2::aes(x = state, y = share, group = pair)) +
  ggplot2::geom_line(alpha = 0.35) +
  ggplot2::geom_point(size = 1.7) +
  ggplot2::stat_summary(
    ggplot2::aes(group = 1), fun = mean, geom = "line", linewidth = 1.1
  ) +
  ggplot2::stat_summary(fun = mean, geom = "point", size = 3) +
  ggplot2::labs(
    title = "Pigmentation increases across the lower-third limitation gate",
    subtitle = "25 km, one-to-one, environmentally matched pairs",
    x = NULL, y = "Pigmented share"
  ) + ggplot2::theme_minimal(base_size = 9)

presence <- gate[gate$response == "pigmentation_share", , drop = FALSE]
presence$threshold_label <- paste0("≤", presence$low_threshold)
p3 <- ggplot2::ggplot(
  presence,
  ggplot2::aes(x = factor(threshold_label, levels = threshold_label),
               y = observed_directed_difference)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_point(
    ggplot2::aes(y = natural_null_mean), shape = 1, size = 2.5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0("p=", sprintf("%.3f", upper_tail_p))),
    nudge_y = 0.035, size = 2.7
  ) +
  ggplot2::labs(
    title = "Direction is positive across the retained gate grid",
    subtitle = "Filled = observed; open = natural-map mean",
    x = "All-species low-support threshold", y = "Available − limited pigmentation"
  ) + ggplot2::theme_minimal(base_size = 9)

primary_null <- null[
  abs(null$low_threshold - 0.33) < 1e-12 &
    null$response == "pigmentation_share", , drop = FALSE
]
primary <- gate[gate$is_primary_gate & gate$response == "pigmentation_share", , drop = FALSE]
p4 <- ggplot2::ggplot(primary_null, ggplot2::aes(x = statistic)) +
  ggplot2::geom_histogram(bins = 35, boundary = 0) +
  ggplot2::geom_vline(
    xintercept = primary$observed_directed_difference,
    linewidth = 0.8, linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Observed contrast against 1,000 natural maps",
    subtitle = sprintf(
      "Observed = %.3f; upper-tail p = %.3f; across-grid BH q = %.3f",
      primary$observed_directed_difference,
      primary$upper_tail_p, primary$BH_q_all_gate_tests
    ),
    x = "Available − limited pigmentation contrast", y = "Predictive maps"
  ) + ggplot2::theme_minimal(base_size = 9)

figure_3 <- (p1 + p2) / (p3 + p4) + patchwork::plot_annotation(tag_levels = "a")
for (ext in c("png", "pdf")) {
  path <- file.path(output_dir, paste0("figure_3_bombus_limitation.", ext))
  if (ext == "png") {
    ggplot2::ggsave(path, figure_3, width = 7.2, height = 7.0, units = "in", dpi = 600)
  } else {
    ggplot2::ggsave(
      path, figure_3, width = 7.2, height = 7.0, units = "in",
      device = grDevices::cairo_pdf
    )
  }
}
