#!/usr/bin/env Rscript

# Render the current JBI four-figure bundle with a journal-width layout pass.
# The core/final-Broad adapter owns data assembly and numerical locks; this file
# only improves final display while preserving the current scientific panels.

source("scripts/build_jbi_figure_bundle.R")

# JBI requires bar scales on maps. All current Main maps use WGS84
# longitude/latitude; this helper draws a 100-km bar at 32.15°N using the
# local longitude-to-distance conversion at the bar latitude.
add_100km_scale <- function(plot, lon = 130.05, lat = 32.15) {
  km <- 100
  delta_lon <- km / (111.32 * cos(lat * pi / 180))
  tick <- 0.09
  plot +
    ggplot2::annotate(
      "segment", x = lon, xend = lon + delta_lon, y = lat, yend = lat,
      linewidth = 0.62, colour = ink
    ) +
    ggplot2::annotate(
      "segment", x = lon, xend = lon, y = lat - tick, yend = lat + tick,
      linewidth = 0.62, colour = ink
    ) +
    ggplot2::annotate(
      "segment", x = lon + delta_lon, xend = lon + delta_lon,
      y = lat - tick, yend = lat + tick,
      linewidth = 0.62, colour = ink
    ) +
    ggplot2::annotate(
      "text", x = lon + delta_lon / 2, y = lat + 0.22,
      label = "100 km", size = 2.2, colour = ink
    )
}

# Apply the same explicit map scale to every national map panel constructed by
# the current builder/adapter. Non-map panels are unchanged.
fig1b <- add_100km_scale(fig1b)
fig2b <- add_100km_scale(fig2b)
fig2c <- add_100km_scale(fig2c)
fig3a <- add_100km_scale(fig3a)
fig4b <- add_100km_scale(fig4b)

# Figure 1a: keep every measurement step visible at two-column width.
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
    label = "Repurposed image stream → two ecological colour layers",
    colour = mid_grey,
    size = 2.45
  ) +
  ggplot2::coord_cartesian(
    xlim = c(0.50, 5.50),
    ylim = c(0.35, 1.35),
    clip = "off"
  ) +
  ggplot2::labs(title = "From hiking record to pigmentation state and intensity") +
  ggplot2::theme_void(base_family = "sans") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold", colour = ink, size = 8.8,
      margin = ggplot2::margin(b = 4)
    ),
    plot.margin = ggplot2::margin(8, 8, 8, 8)
  )
figure_1 <- (tag_panel(fig1a, "a") / tag_panel(fig1b, "b")) |
  (tag_panel(fig1c, "c") / tag_panel(fig1d, "d"))
figure_1 <- figure_1 + patchwork::plot_layout(widths = c(1.05, 1))

# Figure 2: shorten titles and place colour-bar titles above the bars.
fig2a <- fig2a +
  ggplot2::labs(title = "Broad environmental associations conditional on space") +
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
  ggplot2::labs(title = "Transfer to withheld geography") +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 7.8))
fig2d <- range_plot / performance_plot +
  patchwork::plot_layout(heights = c(1, 1.2))
figure_2 <- (tag_panel(fig2a, "a") | tag_panel(fig2b, "b")) /
  (tag_panel(fig2c, "c") |
     patchwork::wrap_elements(full = fig2d) +
       ggplot2::labs(tag = "d") +
       ggplot2::theme(
         plot.tag = ggplot2::element_text(face = "bold", size = 11, colour = ink),
         plot.tag.position = c(0, 1)
       ))
figure_2 <- figure_2 + patchwork::plot_layout(widths = c(1.15, 1))

# Figure 3: make the local-maintenance message readable without losing limitations.
fig3b <- fig3b +
  ggplot2::labs(title = "Fixed boundaries are environmentally local")
fig3c <- fig3c +
  ggplot2::labs(title = "Weak focal-guild contrast at state boundaries")
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
    title = "Local correspondence attenuates across scales",
    subtitle = "Point labels are one-sided sign-flip P values",
    x = "Radius (km)",
    y = "Mean signed contrast"
  ) +
  theme_publication(base_size = 7.2)
figure_3 <- (tag_panel(fig3a, "a") | tag_panel(fig3b, "b")) /
  (tag_panel(fig3c, "c") | tag_panel(fig3d, "d"))

# Figure 4a must reflect the current final-eight-axis event definition. Rebuild
# the conceptual panel explicitly so stale development labels cannot leak into
# the final manuscript figure.
angles <- seq(0, 2 * pi, length.out = 9)[-9]
neighbour_nodes_current <- data.frame(
  x = 1.25 * cos(angles), y = 1.25 * sin(angles)
)
circle_current <- data.frame(
  x = 1.55 * cos(seq(0, 2 * pi, length.out = 240)),
  y = 1.55 * sin(seq(0, 2 * pi, length.out = 240))
)
fig4a <- ggplot2::ggplot() +
  ggplot2::geom_path(
    data = circle_current, ggplot2::aes(x = x, y = y),
    linetype = "dashed", colour = mid_grey
  ) +
  ggplot2::geom_point(
    data = neighbour_nodes_current, ggplot2::aes(x = x, y = y),
    shape = 21, fill = white_like, colour = paper, stroke = 0.35, size = 4.0
  ) +
  ggplot2::geom_point(
    ggplot2::aes(x = 0, y = 0), shape = 21,
    fill = pigmented, colour = ink, stroke = 0.55, size = 5.2
  ) +
  ggplot2::annotate(
    "text", x = 0, y = -0.35,
    label = "Pigmented focal cell", size = 2.55, colour = ink
  ) +
  ggplot2::annotate(
    "text", x = 0, y = 1.85,
    label = "Within 10 km and eight-axis environmental RMS distance ≤ 1",
    size = 2.35, colour = mid_grey
  ) +
  ggplot2::annotate(
    "text", x = 0, y = -1.90,
    label = "Event is defined before population, land use, roads or DID are read",
    size = 2.35, colour = mid_grey
  ) +
  ggplot2::coord_equal(
    xlim = c(-2.15, 2.15), ylim = c(-2.10, 2.10), clip = "off"
  ) +
  ggplot2::labs(title = "Local colour departure as an ecological event") +
  ggplot2::theme_void(base_family = "sans") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", colour = ink, size = 8.8),
    plot.margin = ggplot2::margin(8, 8, 8, 8)
  )

# Figure 4b-c already come from the final-Broad/current-human adapter. Preserve
# the adapter's current human feature family in 4d rather than reconstructing
# a development-only composite panel.
fig4c <- fig4c +
  ggplot2::labs(title = "Departure frequency in repeated natural maps")
fig4d <- fig4d +
  ggplot2::labs(
    title = "Possible contemporary human overlay",
    subtitle = "10,000-map family; no global maxT P < .05"
  ) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 8.2))
figure_4 <- (tag_panel(fig4a, "a") | tag_panel(fig4b, "b")) /
  (tag_panel(fig4c, "c") | tag_panel(fig4d, "d"))

# Overwrite the adapter previews with the final journal-width files and refresh
# their SHA-256 manifest. Numerical and source locks written by the adapter
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
    "reveal pigmentation state and intensity as distinct colour layers",
    "locate broad environmental and unresolved geographic structure",
    "test weak local pollinator-maintenance correspondence at state boundaries",
    "calibrate natural departures before contemporary provenance follow-up"
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