#!/usr/bin/env Rscript

# Render the current JBI four-figure bundle with a journal-width layout pass.
# The core builder owns data assembly, numerical locks and panel construction;
# this file shortens display labels, prevents clipping and rewrites the final
# PNG/PDF files plus their manifest without changing any plotted value.

source("scripts/build_jbi_figure_bundle.R")

# Figure 1A: keep every measurement step visible at two-column width.
workflow_nodes <- data.frame(
  x = 1:5,
  y = 1,
  label = c(
    "Hiking\nrecord",
    "GPS-linked\nphoto",
    "Taxon/flower\nROI check",
    "sRGB →\nCIELAB",
    "State +\nintensity"
  ),
  stringsAsFactors = FALSE
)
workflow_edges <- data.frame(
  x = 1:4 + 0.30,
  xend = 2:5 - 0.30,
  y = 1,
  yend = 1
)
fig1a <- ggplot2::ggplot() +
  ggplot2::geom_segment(
    data = workflow_edges,
    ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
    linewidth = 0.55,
    colour = mid_grey,
    arrow = grid::arrow(length = grid::unit(2.2, "mm"), type = "closed")
  ) +
  ggplot2::geom_label(
    data = workflow_nodes,
    ggplot2::aes(x = x, y = y, label = label),
    size = 2.30,
    label.size = 0.28,
    label.padding = grid::unit(1.25, "mm"),
    fill = paper,
    colour = ink
  ) +
  ggplot2::annotate(
    "text",
    x = 3,
    y = 0.55,
    label = "Repurposed observation stream → auditable quantitative trait",
    colour = mid_grey,
    size = 2.45
  ) +
  ggplot2::coord_cartesian(
    xlim = c(0.50, 5.50),
    ylim = c(0.35, 1.35),
    clip = "off"
  ) +
  ggplot2::labs(title = "From hiking record to quantitative phenotype") +
  ggplot2::theme_void(base_family = "sans") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold", colour = ink, size = 8.8,
      margin = ggplot2::margin(b = 4)
    ),
    plot.margin = ggplot2::margin(8, 8, 8, 8)
  )
figure_1 <- (tag_panel(fig1a, "A") / tag_panel(fig1b, "B")) |
  (tag_panel(fig1c, "C") / tag_panel(fig1d, "D"))
figure_1 <- figure_1 + patchwork::plot_layout(widths = c(1.05, 1))

# Figure 2: shorten titles and place colour-bar titles above the bars.
fig2a <- fig2a +
  ggplot2::labs(title = "Environmental effects conditional on space") +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 8.2))
fig2b <- fig2b +
  ggplot2::guides(
    colour = ggplot2::guide_colourbar(
      title = "Predicted probability",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(34, "mm")
    )
  )
fig2c <- fig2c +
  ggplot2::guides(
    colour = ggplot2::guide_colourbar(
      title = "Predicted intensity",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(34, "mm")
    )
  )
performance_plot <- performance_plot +
  ggplot2::labs(title = "Blocked prediction") +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 7.8))
fig2d <- range_plot / performance_plot +
  patchwork::plot_layout(heights = c(1, 1.2))
figure_2 <- (tag_panel(fig2a, "A") | tag_panel(fig2b, "B")) /
  (tag_panel(fig2c, "C") |
     patchwork::wrap_elements(full = fig2d) +
       ggplot2::labs(tag = "D") +
       ggplot2::theme(
         plot.tag = ggplot2::element_text(face = "bold", size = 11, colour = ink),
         plot.tag.position = c(0, 1)
       ))
figure_2 <- figure_2 + patchwork::plot_layout(widths = c(1.15, 1))

# Figure 3: make the local-design message readable without losing limitations.
fig3b <- fig3b +
  ggplot2::labs(title = "Selected boundaries are environmentally local")
fig3c <- fig3c +
  ggplot2::labs(title = "Focal-guild contrast across pairs")
sensitivity$display_p <- sprintf("%.3f", sensitivity$signflip_one_sided_p)
sensitivity$label_x <- sensitivity$radius_km + ifelse(
  sensitivity$radius_km == 5,
  1.15,
  ifelse(sensitivity$radius_km == 25, -1.15, 0)
)
fig3d <- ggplot2::ggplot(
  sensitivity,
  ggplot2::aes(
    x = radius_km,
    y = mean_signed_bombus_difference,
    group = 1
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = mid_grey
  ) +
  ggplot2::geom_line(colour = "#7A8690", linewidth = 0.52) +
  ggplot2::geom_point(
    ggplot2::aes(fill = radius_km == 5),
    shape = 21,
    colour = ink,
    size = 2.05,
    stroke = 0.35
  ) +
  ggplot2::geom_text(
    ggplot2::aes(x = label_x, label = display_p),
    nudge_y = 0.006,
    size = 2.05,
    colour = mid_grey
  ) +
  ggplot2::facet_wrap(~exposure_label, ncol = 2) +
  ggplot2::scale_fill_manual(
    values = c("TRUE" = pigmented, "FALSE" = paper),
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(5, 10, 25),
    expand = ggplot2::expansion(mult = c(0.05, 0.05))
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0.12, 0.24))
  ) +
  ggplot2::labs(
    title = "Scale and exposure claim ceiling",
    subtitle = "Point labels are one-sided sign-flip P values",
    x = "Radius (km)",
    y = "Mean signed contrast"
  ) +
  theme_publication(base_size = 7.2)
figure_3 <- (tag_panel(fig3a, "A") | tag_panel(fig3b, "B")) /
  (tag_panel(fig3c, "C") | tag_panel(fig3d, "D"))

# Figure 4: keep the event calibration and familywise result visually clean.
fig4c <- fig4c +
  ggplot2::labs(title = "Events in repeated natural maps")
fig4d <- ggplot2::ggplot(
  human,
  ggplot2::aes(x = observed, y = feature_label, colour = family)
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = mid_grey
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower, xmax = upper),
    width = 0,
    linewidth = 0.55,
    orientation = "y"
  ) +
  ggplot2::geom_point(size = 1.9) +
  ggplot2::scale_colour_manual(
    values = c("Population" = white_like, "DID context" = pigmented)
  ) +
  ggplot2::coord_cartesian(
    xlim = c(min(human$lower) - 0.01, max(human$upper) + 0.02)
  ) +
  ggplot2::labs(
    title = "Post-selection human context",
    subtitle = "Natural-map 95% intervals; no familywise P < .05",
    x = "Candidate minus white-neighbour rank",
    y = NULL,
    colour = NULL
  ) +
  theme_publication(base_size = 7.2)
figure_4 <- (tag_panel(fig4a, "A") | tag_panel(fig4b, "B")) /
  (tag_panel(fig4c, "C") | tag_panel(fig4d, "D"))

# Overwrite the core previews with the final journal-width files and refresh
# their SHA-256 manifest. Numerical and source locks written by the core builder
# remain unchanged and are independently checked afterwards.
figure_rows <- list(
  save_figure(
    figure_1,
    "Figure_1_measurement_two_part_phenotype",
    1,
    7.4,
    7.25
  ),
  save_figure(
    figure_2,
    "Figure_2_broad_environment_spatial_template",
    2,
    7.4,
    7.35
  ),
  save_figure(
    figure_3,
    "Figure_3_local_focal_bombus_boundaries",
    3,
    7.4,
    7.25
  ),
  save_figure(
    figure_4,
    "Figure_4_calibrated_local_departures",
    4,
    7.4,
    7.35
  )
)
figure_manifest <- do.call(rbind, figure_rows)
figure_manifest$narrative_job <- rep(
  c(
    "measurement and two-part trait representation",
    "broad environment plus unresolved space",
    "scale shift to local focal-pollinator boundaries",
    "calibrated ecological departures and provenance follow-up"
  ),
  each = 2
)
utils::write.csv(
  figure_manifest,
  file.path(output_dir, "figure_manifest.csv"),
  row.names = FALSE
)

message("Applied the current JBI journal-width layout pass.")
print(figure_manifest[, c("figure", "format", "path", "size_bytes", "sha256")])
