#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
hb_require_stage_packages("publication_figures")
hb_require_packages(c("digest", "ragg"))

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

arg_value <- function(flag, default = NULL) hb_arg_value(args, flag, default)
output_dir <- arg_value("--output", "results/jbi_figure_bundle")
main_dir <- file.path(output_dir, "main_figures")
data_dir <- file.path(output_dir, "figure_data")
dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

ink <- "#25313B"
mid_grey <- "#6B7782"
light_grey <- "#DCE3E8"
paper <- "#FFFFFF"
map_land <- "#F1F0EB"
map_edge <- "#88949E"
white_like <- "#4E79A7"
pigmented <- "#A83270"
intensity_low <- "#D8E6F2"
intensity_high <- "#7A174D"
positive <- "#7A174D"
negative <- "#4E79A7"
accent_gold <- "#B07C17"

read_num <- function(x) suppressWarnings(as.numeric(x))
sha256_file <- function(path) unname(digest::digest(file = path, algo = "sha256"))

required_inputs <- c(
  "Data_S1.csv",
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv",
  "results/ecological_v11_pigmentation_hurdle/pigmentation_mixture_components.csv",
  "results/ecological_v11_pigmentation_hurdle/pigmentation_hurdle_inla_fixed_effects.csv",
  "results/ecological_v11_pigmentation_hurdle/pigmentation_hurdle_inla_hyperparameters.csv",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv",
  "results/ecological_v16_predictive_replication/predictive_replication_model_performance.csv",
  "results/ecological_v18_bombus_local_sharp_transition/sharp_transition_summary.csv",
  "results/ecological_v18_bombus_local_sharp_transition/sharp_transition_nonoverlapping_pairs.csv",
  "results/ecological_v20_local_white_isolates/local_isolate_candidates.csv",
  "results/ecological_v20_local_white_isolates/local_isolate_natural_null.csv",
  "results/ecological_v20_local_white_isolates/local_isolate_natural_null_summary.csv",
  "results/ecological_v21_local_human_neighbourhood/human_neighbourhood_population_scale_summary.csv",
  "results/ecological_v22_did_human_context/did_contrast_summary.csv"
)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs)) {
  stop("Missing figure inputs: ", paste(missing_inputs, collapse = ", "), call. = FALSE)
}

source_manifest <- data.frame(
  path = required_inputs,
  size_bytes = as.numeric(file.info(required_inputs)$size),
  sha256 = vapply(required_inputs, sha256_file, character(1)),
  stringsAsFactors = FALSE
)
utils::write.csv(
  source_manifest, file.path(output_dir, "figure_source_manifest.csv"),
  row.names = FALSE
)

theme_publication <- function(base_size = 8.5) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", colour = ink, size = base_size + 0.7,
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = mid_grey, size = base_size - 0.4,
        margin = ggplot2::margin(b = 4)
      ),
      axis.title = ggplot2::element_text(colour = ink),
      axis.text = ggplot2::element_text(colour = ink),
      strip.text = ggplot2::element_text(face = "bold", colour = ink),
      strip.background = ggplot2::element_rect(fill = "#F4F7F9", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E8EDF1", linewidth = 0.25),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      legend.key.height = grid::unit(3.2, "mm"),
      plot.margin = ggplot2::margin(7, 7, 7, 7)
    )
}

theme_map <- function(base_size = 8) {
  theme_publication(base_size) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = base_size - 1.1, colour = "#64717D"),
      panel.grid.major = ggplot2::element_line(colour = "#DDE5EA", linewidth = 0.22),
      panel.background = ggplot2::element_rect(fill = "#F8FAFB", colour = NA),
      panel.border = ggplot2::element_rect(colour = "#C5CED5", fill = NA, linewidth = 0.3)
    )
}

japan <- rnaturalearth::ne_countries(
  scale = "medium", country = "Japan", returnclass = "sf"
)

base_japan_map <- function() {
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = japan, fill = map_land, colour = map_edge, linewidth = 0.34
    ) +
    ggplot2::coord_sf(
      xlim = c(129.0, 142.7), ylim = c(31.5, 41.0),
      expand = FALSE, datum = sf::st_crs(4326)
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(130, 134, 138, 142),
      labels = function(x) paste0(x, "°E")
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(32, 36, 40),
      labels = function(x) paste0(x, "°N")
    ) +
    theme_map()
}

tag_panel <- function(plot, label) {
  plot +
    ggplot2::labs(tag = label) +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(face = "bold", size = 11, colour = ink),
      plot.tag.position = c(0, 1)
    )
}

save_figure <- function(plot, stem, figure_number, width = 7.2, height = 7.2) {
  png_path <- file.path(main_dir, paste0(stem, ".png"))
  pdf_path <- file.path(main_dir, paste0(stem, ".pdf"))
  ggplot2::ggsave(
    png_path, plot = plot, device = ragg::agg_png,
    width = width, height = height, units = "in", dpi = 600,
    bg = paper
  )
  ggplot2::ggsave(
    pdf_path, plot = plot, device = grDevices::cairo_pdf,
    width = width, height = height, units = "in", bg = paper
  )
  rbind(
    data.frame(
      figure = figure_number, format = "PNG", path = png_path,
      width_in = width, height_in = height,
      size_bytes = as.numeric(file.info(png_path)$size),
      sha256 = sha256_file(png_path), stringsAsFactors = FALSE
    ),
    data.frame(
      figure = figure_number, format = "PDF", path = pdf_path,
      width_in = width, height_in = height,
      size_bytes = as.numeric(file.info(pdf_path)$size),
      sha256 = sha256_file(pdf_path), stringsAsFactors = FALSE
    )
  )
}

analysis <- hb_read_csv(required_inputs[[2]])
raw_colour <- hb_read_csv(required_inputs[[1]])
components <- hb_read_csv(required_inputs[[3]])
fixed <- hb_read_csv(required_inputs[[4]])
hyper <- hb_read_csv(required_inputs[[5]])
cells <- hb_read_csv(required_inputs[[6]])
performance <- hb_read_csv(required_inputs[[7]])
sharp_summary <- hb_read_csv(required_inputs[[8]])
sharp_pairs <- hb_read_csv(required_inputs[[9]])
candidates <- hb_read_csv(required_inputs[[10]])
isolate_null <- hb_read_csv(required_inputs[[11]])
isolate_summary <- hb_read_csv(required_inputs[[12]])
population_summary <- hb_read_csv(required_inputs[[13]])
did_summary <- hb_read_csv(required_inputs[[14]])

analysis$colour_a <- read_num(analysis$colour_a)
analysis$longitude <- read_num(analysis$longitude)
analysis$latitude <- read_num(analysis$latitude)
analysis$pigment_intensity_z <- read_num(analysis$pigment_intensity_z)
analysis$pigmented_mixture50 <- as.integer(analysis$pigmented_mixture50)
analysis$pigmentation_class <- factor(
  ifelse(analysis$pigmented_mixture50 == 1L, "Pigmented", "White-like"),
  levels = c("White-like", "Pigmented")
)
threshold <- unique(read_num(analysis$pigment_boundary_a))
threshold <- threshold[is.finite(threshold)]
if (length(threshold) != 1L) stop("Expected one pigmentation boundary.", call. = FALSE)

rgb_columns <- if (all(c("R", "G", "B") %in% names(raw_colour))) {
  c("R", "G", "B")
} else if (all(c("median_R", "median_G", "median_B") %in% names(raw_colour))) {
  c("median_R", "median_G", "median_B")
} else character()
analysis$observed_rgb <- ifelse(
  analysis$pigmented_mixture50 == 1L, pigmented, white_like
)
if (length(rgb_columns) == 3L && "observation_id" %in% names(raw_colour)) {
  rgb_index <- match(analysis$observation_id, raw_colour$observation_id)
  if (!anyNA(rgb_index)) {
    rgb <- as.data.frame(lapply(raw_colour[rgb_index, rgb_columns], read_num))
    valid_rgb <- stats::complete.cases(rgb) &
      apply(rgb, 1, function(x) all(x >= 0 & x <= 255))
    analysis$observed_rgb[valid_rgb] <- grDevices::rgb(
      rgb[[1]][valid_rgb], rgb[[2]][valid_rgb], rgb[[3]][valid_rgb],
      maxColorValue = 255
    )
  }
}

cells$longitude <- read_num(cells$longitude)
cells$latitude <- read_num(cells$latitude)
cells$natural_presence_probability <- read_num(cells$natural_presence_probability)
cells$natural_intensity_prediction <- read_num(cells$natural_intensity_prediction)
cells$conditional_intensity_median <- read_num(cells$conditional_intensity_median)
cells$pigment_share <- read_num(cells$pigment_share)
cells$site_label <- factor(
  cells$site_class,
  levels = c("white", "mixed", "pigmented"),
  labels = c("White-only", "Mixed", "Pigmented-only")
)

# Figure 1: measurement pipeline and two-part phenotype.
workflow_nodes <- data.frame(
  x = 1:5, y = 1,
  label = c(
    "Hiking\nactivity", "GPS-linked\nphotograph",
    "Author taxon,\nflower & ROI check", "sRGB →\nCIELAB",
    "Pigmentation\nstate + intensity"
  ),
  stringsAsFactors = FALSE
)
workflow_edges <- data.frame(x = 1:4 + 0.32, xend = 2:5 - 0.32, y = 1, yend = 1)
fig1a <- ggplot2::ggplot() +
  ggplot2::geom_segment(
    data = workflow_edges,
    ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
    linewidth = 0.55, colour = mid_grey,
    arrow = grid::arrow(length = grid::unit(2.4, "mm"), type = "closed")
  ) +
  ggplot2::geom_label(
    data = workflow_nodes, ggplot2::aes(x = x, y = y, label = label),
    size = 2.5, label.size = 0.28, label.padding = grid::unit(1.5, "mm"),
    fill = paper, colour = ink
  ) +
  ggplot2::annotate(
    "text", x = 3, y = 0.55,
    label = "Alternative observation stream → auditable quantitative trait",
    colour = mid_grey, size = 2.6
  ) +
  ggplot2::coord_cartesian(xlim = c(0.55, 5.45), ylim = c(0.35, 1.35), clip = "off") +
  ggplot2::labs(title = "From recreation record to flower-colour phenotype") +
  ggplot2::theme_void(base_family = "sans") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", colour = ink, size = 9.2),
    plot.margin = ggplot2::margin(8, 8, 8, 8)
  )

map_colour <- analysis[order(analysis$colour_a), , drop = FALSE]
fig1b <- base_japan_map() +
  ggplot2::geom_point(
    data = map_colour,
    ggplot2::aes(x = longitude, y = latitude, fill = observed_rgb),
    shape = 21, colour = "#59656F", stroke = 0.10,
    size = 1.05, alpha = 0.92
  ) +
  ggplot2::scale_fill_identity() +
  ggplot2::labs(
    title = "Final georeferenced observations",
    subtitle = paste0("n = ", format(nrow(analysis), big.mark = ","), "; extracted visible colour")
  )

components$mean_a <- read_num(components$mean_a)
components$sd_a <- read_num(components$sd_a)
components$proportion <- read_num(components$proportion)
x_grid <- seq(min(analysis$colour_a), max(analysis$colour_a), length.out = 600)
binwidth <- diff(range(analysis$colour_a)) / 45
component_curves <- do.call(rbind, lapply(seq_len(nrow(components)), function(i) {
  data.frame(
    component = paste0("Component ", components$component[[i]]),
    x = x_grid,
    count = stats::dnorm(
      x_grid, mean = components$mean_a[[i]], sd = components$sd_a[[i]]
    ) * components$proportion[[i]] * nrow(analysis) * binwidth,
    regime = components$measurement_regime[[i]],
    stringsAsFactors = FALSE
  )
}))
fig1c <- ggplot2::ggplot(
  analysis, ggplot2::aes(x = colour_a, fill = pigmentation_class)
) +
  ggplot2::geom_histogram(
    bins = 45, position = "identity", alpha = 0.76,
    colour = paper, linewidth = 0.12
  ) +
  ggplot2::geom_line(
    data = component_curves,
    ggplot2::aes(x = x, y = count, group = component, colour = regime),
    inherit.aes = FALSE, linewidth = 0.55, alpha = 0.85
  ) +
  ggplot2::geom_vline(
    xintercept = threshold, linetype = "dashed", colour = ink, linewidth = 0.65
  ) +
  ggplot2::annotate(
    "text", x = threshold, y = Inf,
    label = sprintf("  boundary a* = %.2f", threshold),
    hjust = 0, vjust = 1.5, size = 2.45, colour = ink
  ) +
  ggplot2::scale_fill_manual(values = c("White-like" = white_like, "Pigmented" = pigmented)) +
  ggplot2::scale_colour_manual(
    values = c("white_colour_noise" = white_like, "pigmented" = pigmented),
    guide = "none"
  ) +
  ggplot2::labs(
    title = "Response-blind a* mixture",
    x = "CIELAB a* from display-referred sRGB", y = "Photographs",
    fill = "State"
  ) +
  theme_publication()

pigmented_only <- analysis[
  analysis$pigmented_mixture50 == 1L & is.finite(analysis$pigment_intensity_z),
  , drop = FALSE
]
fig1d <- ggplot2::ggplot(
  pigmented_only, ggplot2::aes(x = pigment_intensity_z)
) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = after_stat(density)), bins = 34,
    fill = "#E7B7CE", colour = paper, linewidth = 0.15
  ) +
  ggplot2::geom_density(colour = intensity_high, linewidth = 0.75) +
  ggplot2::geom_rug(colour = intensity_high, alpha = 0.25, sides = "b") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = mid_grey) +
  ggplot2::labs(
    title = "Intensity is conditional on pigmentation",
    subtitle = paste0("n = ", nrow(pigmented_only), "; white-like observations excluded"),
    x = "Conditional visible intensity (z score)", y = "Density"
  ) +
  theme_publication()

figure_1 <- (tag_panel(fig1a, "A") / tag_panel(fig1b, "B")) |
  (tag_panel(fig1c, "C") / tag_panel(fig1d, "D"))
figure_1 <- figure_1 + patchwork::plot_layout(widths = c(1.05, 1))

# Figure 2: broad environmental and spatial natural template.
fixed <- fixed[
  fixed$model %in% c("presence_N_environment_space", "intensity_N_environment_space") &
    grepl("^env_", fixed$term),
  , drop = FALSE
]
fixed$mean <- read_num(fixed$mean)
fixed$lower <- read_num(fixed[["0.025quant"]])
fixed$upper <- read_num(fixed[["0.975quant"]])
term_labels <- c(
  env_Temperature_PC1 = "Temperature PC1",
  env_precip_PC1 = "Precipitation PC1",
  env_TemperatureSeasonality = "Temperature seasonality",
  env_PrecipSeasonality = "Precipitation seasonality",
  env_topo_PC1 = "Topography PC1",
  env_soil_PC1 = "Soil PC1",
  env_soil_PC2 = "Soil PC2",
  env_RSDS = "Solar radiation"
)
fixed$term_label <- factor(
  unname(term_labels[fixed$term]), levels = rev(unname(term_labels))
)
fixed$response <- factor(
  ifelse(grepl("^presence", fixed$model), "Pigmentation state", "Pigmented-only intensity"),
  levels = c("Pigmentation state", "Pigmented-only intensity")
)
fig2a <- ggplot2::ggplot(
  fixed, ggplot2::aes(x = mean, y = term_label, colour = response)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = mid_grey) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower, xmax = upper),
    width = 0, linewidth = 0.58, orientation = "y"
  ) +
  ggplot2::geom_point(size = 1.9) +
  ggplot2::facet_wrap(~response, ncol = 2) +
  ggplot2::scale_colour_manual(
    values = c("Pigmentation state" = white_like, "Pigmented-only intensity" = pigmented),
    guide = "none"
  ) +
  ggplot2::labs(
    title = "Environment conditional on continuous space",
    x = "Posterior mean (95% credible interval)", y = NULL
  ) +
  theme_publication(base_size = 7.8)

presence_cells <- cells[is.finite(cells$natural_presence_probability), , drop = FALSE]
fig2b <- base_japan_map() +
  ggplot2::geom_point(
    data = presence_cells,
    ggplot2::aes(x = longitude, y = latitude, colour = natural_presence_probability),
    size = 1.20, alpha = 0.92
  ) +
  ggplot2::scale_colour_gradient(
    low = "#D9E9F5", high = "#183F67", limits = c(0, 1),
    oob = scales::squish, name = "Predicted\nprobability"
  ) +
  ggplot2::labs(title = "Cross-fitted pigmentation geography")

intensity_cells <- cells[is.finite(cells$natural_intensity_prediction), , drop = FALSE]
intensity_limit <- max(abs(stats::quantile(
  intensity_cells$natural_intensity_prediction, c(0.02, 0.98), na.rm = TRUE
)))
fig2c <- base_japan_map() +
  ggplot2::geom_point(
    data = intensity_cells,
    ggplot2::aes(x = longitude, y = latitude, colour = natural_intensity_prediction),
    size = 1.20, alpha = 0.92
  ) +
  ggplot2::scale_colour_gradient2(
    low = white_like, mid = "#F4F4F0", high = pigmented, midpoint = 0,
    limits = c(-intensity_limit, intensity_limit), oob = scales::squish,
    name = "Predicted\nintensity"
  ) +
  ggplot2::labs(title = "Cross-fitted conditional intensity")

range_data <- hyper[
  hyper$model %in% c("presence_N_environment_space", "intensity_N_environment_space") &
    hyper$hyperparameter == "Range for spatial",
  , drop = FALSE
]
range_data$mean <- read_num(range_data$mean)
range_data$lower <- read_num(range_data[["0.025quant"]])
range_data$upper <- read_num(range_data[["0.975quant"]])
range_data$response <- factor(
  ifelse(grepl("^presence", range_data$model), "Pigmentation state", "Pigmented-only intensity"),
  levels = c("Pigmentation state", "Pigmented-only intensity")
)
range_plot <- ggplot2::ggplot(
  range_data, ggplot2::aes(x = mean, y = response, colour = response)
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower, xmax = upper),
    width = 0, linewidth = 0.65, orientation = "y"
  ) +
  ggplot2::geom_point(size = 2.1) +
  ggplot2::scale_colour_manual(
    values = c("Pigmentation state" = white_like, "Pigmented-only intensity" = pigmented),
    guide = "none"
  ) +
  ggplot2::labs(title = "Residual spatial range", x = "Range (km; 95% CrI)", y = NULL) +
  theme_publication(base_size = 7.6)

presence_perf <- performance[performance$model == "national_environment_spde_presence", , drop = FALSE]
intensity_perf <- performance[performance$model == "national_environment_spde_intensity", , drop = FALSE]
performance_cards <- data.frame(
  x = c(1, 2, 1, 2), y = c(2, 2, 1, 1),
  label = c(
    sprintf("Presence AUC\n%.3f", read_num(presence_perf$AUC)),
    sprintf("Brier score\n%.3f", read_num(presence_perf$Bernoulli_Brier_score)),
    sprintf("Intensity RMSE\n%.3f", read_num(intensity_perf$RMSE)),
    sprintf("95%% coverage\n%.3f", read_num(intensity_perf$coverage_95))
  ),
  stringsAsFactors = FALSE
)
performance_plot <- ggplot2::ggplot(performance_cards, ggplot2::aes(x = x, y = y, label = label)) +
  ggplot2::geom_label(
    fill = paper, colour = ink, label.size = 0.28,
    label.padding = grid::unit(1.4, "mm"), size = 2.5
  ) +
  ggplot2::coord_cartesian(xlim = c(0.5, 2.5), ylim = c(0.55, 2.45)) +
  ggplot2::labs(title = "Blocked predictive performance") +
  ggplot2::theme_void(base_family = "sans") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", colour = ink, size = 8.3),
    plot.margin = ggplot2::margin(5, 5, 5, 5)
  )
fig2d <- range_plot / performance_plot + patchwork::plot_layout(heights = c(1, 1.2))

figure_2 <- (tag_panel(fig2a, "A") | tag_panel(fig2b, "B")) /
  (tag_panel(fig2c, "C") | patchwork::wrap_elements(full = fig2d) +
     ggplot2::labs(tag = "D") +
     ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", size = 11)))
figure_2 <- figure_2 + patchwork::plot_layout(widths = c(1.15, 1))

# Figure 3: scale shift to sharp local focal-pollinator boundaries.
focal_pairs <- sharp_pairs[
  read_num(sharp_pairs$radius_km) == 5 &
    abs(read_num(sharp_pairs$transition_threshold) - 1) < 1e-10,
  , drop = FALSE
]
focal_pairs$delta_effective_occmax <- read_num(focal_pairs$delta_effective_occmax)
focal_pairs$white_longitude <- read_num(focal_pairs$white_longitude)
focal_pairs$white_latitude <- read_num(focal_pairs$white_latitude)
focal_pairs$pigmented_longitude <- read_num(focal_pairs$pigmented_longitude)
focal_pairs$pigmented_latitude <- read_num(focal_pairs$pigmented_latitude)

fig3a <- base_japan_map() +
  ggplot2::geom_point(
    data = cells, ggplot2::aes(x = longitude, y = latitude),
    colour = light_grey, size = 0.60, alpha = 0.55
  ) +
  ggplot2::geom_segment(
    data = focal_pairs,
    ggplot2::aes(
      x = white_longitude, y = white_latitude,
      xend = pigmented_longitude, yend = pigmented_latitude
    ),
    colour = "#68747E", linewidth = 0.35, alpha = 0.65
  ) +
  ggplot2::geom_point(
    data = focal_pairs,
    ggplot2::aes(x = white_longitude, y = white_latitude),
    shape = 21, fill = white_like, colour = paper, stroke = 0.1, size = 1.15
  ) +
  ggplot2::geom_point(
    data = focal_pairs,
    ggplot2::aes(x = pigmented_longitude, y = pigmented_latitude),
    shape = 21, fill = pigmented, colour = paper, stroke = 0.1, size = 1.15
  ) +
  ggplot2::labs(
    title = "Bombus-blind 5-km pure transitions",
    subtitle = paste0("n = ", nrow(focal_pairs), " non-overlapping white → pigmented pairs")
  )

primary_scale <- sharp_summary[
  sharp_summary$exposure == "effective_occmax" &
    abs(read_num(sharp_summary$transition_threshold) - 1) < 1e-10,
  , drop = FALSE
]
primary_scale$radius_km <- read_num(primary_scale$radius_km)
selected_environment <- data.frame(
  radius_km = primary_scale$radius_km,
  set = "Selected pure transitions",
  distance = read_num(primary_scale$median_environmental_distance_selected),
  stringsAsFactors = FALSE
)
all_environment <- data.frame(
  radius_km = primary_scale$radius_km,
  set = "All eligible local edges",
  distance = read_num(primary_scale$median_environmental_distance_all_local_edges),
  stringsAsFactors = FALSE
)
environment_plot_data <- rbind(selected_environment, all_environment)
fig3b <- ggplot2::ggplot(
  environment_plot_data,
  ggplot2::aes(x = radius_km, y = distance, colour = set, group = set)
) +
  ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::geom_point(size = 2.1) +
  ggplot2::scale_x_continuous(breaks = c(5, 10, 25)) +
  ggplot2::scale_colour_manual(
    values = c("Selected pure transitions" = pigmented, "All eligible local edges" = mid_grey)
  ) +
  ggplot2::labs(
    title = "Selected boundaries are locally environment-similar",
    x = "Maximum pair radius (km)",
    y = "Median four-PC environmental distance", colour = NULL
  ) +
  theme_publication()

focal_mean <- mean(focal_pairs$delta_effective_occmax)
focal_median <- stats::median(focal_pairs$delta_effective_occmax)
focal_positive <- mean(focal_pairs$delta_effective_occmax > 0)
focal_pairs$direction <- factor(
  ifelse(focal_pairs$delta_effective_occmax > 0, "Pigmented side higher", "White side equal/higher"),
  levels = c("White side equal/higher", "Pigmented side higher")
)
fig3c <- ggplot2::ggplot(
  focal_pairs, ggplot2::aes(x = 1, y = delta_effective_occmax, colour = direction)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = mid_grey) +
  ggplot2::geom_jitter(width = 0.14, height = 0, size = 1.45, alpha = 0.78) +
  ggplot2::geom_hline(yintercept = focal_mean, colour = pigmented, linewidth = 0.72) +
  ggplot2::geom_hline(yintercept = focal_median, colour = ink, linewidth = 0.55, linetype = "dotdash") +
  ggplot2::annotate(
    "text", x = 1.32, y = focal_mean,
    label = sprintf("mean = %+.3f", focal_mean), hjust = 0, vjust = -0.6,
    colour = pigmented, size = 2.45
  ) +
  ggplot2::annotate(
    "text", x = 1.32, y = focal_median,
    label = sprintf("median = %+.3f", focal_median), hjust = 0, vjust = 1.4,
    colour = ink, size = 2.45
  ) +
  ggplot2::annotate(
    "text", x = 1.32, y = min(focal_pairs$delta_effective_occmax),
    label = sprintf("%.1f%% positive pairs", 100 * focal_positive),
    hjust = 0, vjust = -0.3, colour = mid_grey, size = 2.45
  ) +
  ggplot2::scale_colour_manual(
    values = c("White side equal/higher" = negative, "Pigmented side higher" = positive)
  ) +
  ggplot2::coord_cartesian(xlim = c(0.70, 1.72)) +
  ggplot2::scale_x_continuous(breaks = NULL) +
  ggplot2::labs(
    title = "Occurrence-referenced focal-guild contrast",
    subtitle = "Mean is positive but the pairwise pattern is not pervasive",
    x = NULL, y = "Pigmented minus white support", colour = NULL
  ) +
  theme_publication()

exposure_labels <- c(
  effective_occmax = "Occurrence-referenced\nB. ardens + B. diversus",
  effective_rawmax = "Raw cloglog\nB. ardens + B. diversus",
  all5_occmax = "Occurrence-referenced\nmaximum across five species",
  montane_occmax = "Occurrence-referenced\nthree montane species"
)
sensitivity <- sharp_summary[
  sharp_summary$exposure %in% names(exposure_labels) &
    abs(read_num(sharp_summary$transition_threshold) - 1) < 1e-10,
  , drop = FALSE
]
sensitivity$radius_km <- read_num(sensitivity$radius_km)
sensitivity$mean_signed_bombus_difference <- read_num(sensitivity$mean_signed_bombus_difference)
sensitivity$signflip_one_sided_p <- read_num(sensitivity$signflip_one_sided_p)
sensitivity$exposure_label <- factor(
  unname(exposure_labels[sensitivity$exposure]),
  levels = unname(exposure_labels)
)
sensitivity$p_label <- sprintf("P=%.3f", sensitivity$signflip_one_sided_p)
fig3d <- ggplot2::ggplot(
  sensitivity,
  ggplot2::aes(x = radius_km, y = mean_signed_bombus_difference, group = 1)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = mid_grey) +
  ggplot2::geom_line(colour = "#7A8690", linewidth = 0.52) +
  ggplot2::geom_point(
    ggplot2::aes(fill = radius_km == 5), shape = 21,
    colour = ink, size = 2.05, stroke = 0.35
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = p_label), nudge_y = 0.006,
    size = 2.05, colour = mid_grey, check_overlap = TRUE
  ) +
  ggplot2::facet_wrap(~exposure_label, ncol = 2) +
  ggplot2::scale_fill_manual(values = c("TRUE" = pigmented, "FALSE" = paper), guide = "none") +
  ggplot2::scale_x_continuous(breaks = c(5, 10, 25)) +
  ggplot2::labs(
    title = "Scale and exposure claim ceiling",
    x = "Radius (km)", y = "Mean signed contrast"
  ) +
  theme_publication(base_size = 7.2)

figure_3 <- (tag_panel(fig3a, "A") | tag_panel(fig3b, "B")) /
  (tag_panel(fig3c, "C") | tag_panel(fig3d, "D"))

# Figure 4: event-based local departures and post-selection context.
angles <- seq(0, 2 * pi, length.out = 9)[-9]
neighbour_nodes <- data.frame(
  x = 1.25 * cos(angles), y = 1.25 * sin(angles), state = "White neighbours"
)
circle <- data.frame(
  x = 1.55 * cos(seq(0, 2 * pi, length.out = 240)),
  y = 1.55 * sin(seq(0, 2 * pi, length.out = 240))
)
fig4a <- ggplot2::ggplot() +
  ggplot2::geom_path(data = circle, ggplot2::aes(x = x, y = y), linetype = "dashed", colour = mid_grey) +
  ggplot2::geom_point(
    data = neighbour_nodes, ggplot2::aes(x = x, y = y),
    shape = 21, fill = white_like, colour = paper, stroke = 0.35, size = 4.0
  ) +
  ggplot2::geom_point(
    ggplot2::aes(x = 0, y = 0), shape = 21,
    fill = pigmented, colour = ink, stroke = 0.55, size = 5.2
  ) +
  ggplot2::annotate("text", x = 0, y = -0.35, label = "Pigmented focal cell", size = 2.55, colour = ink) +
  ggplot2::annotate(
    "text", x = 0, y = 1.85,
    label = "Within 10 km and four-PC environmental distance ≤ 1",
    size = 2.45, colour = mid_grey
  ) +
  ggplot2::annotate(
    "text", x = 0, y = -1.90,
    label = "Event is defined before population, land use, roads or DID are read",
    size = 2.35, colour = mid_grey
  ) +
  ggplot2::coord_equal(xlim = c(-2.15, 2.15), ylim = c(-2.10, 2.10), clip = "off") +
  ggplot2::labs(title = "Repeatable local ecological event") +
  ggplot2::theme_void(base_family = "sans") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", colour = ink, size = 9.2),
    plot.margin = ggplot2::margin(8, 8, 8, 8)
  )

candidates$longitude <- read_num(candidates$longitude)
candidates$latitude <- read_num(candidates$latitude)
fig4b <- base_japan_map() +
  ggplot2::geom_point(
    data = cells, ggplot2::aes(x = longitude, y = latitude, fill = site_label),
    shape = 21, colour = paper, stroke = 0.07, size = 0.92, alpha = 0.67
  ) +
  ggplot2::geom_point(
    data = candidates, ggplot2::aes(x = longitude, y = latitude),
    shape = 21, fill = paper, colour = ink, stroke = 0.72, size = 2.25
  ) +
  ggplot2::scale_fill_manual(
    values = c("White-only" = white_like, "Mixed" = light_grey, "Pigmented-only" = pigmented)
  ) +
  ggplot2::labs(
    title = "Seventeen field/provenance targets",
    subtitle = "Open circles mark fixed primary candidates", fill = "Cell sample"
  )

primary_configuration <- "primary_10km_env1_all_white"
null_primary <- isolate_null[
  isolate_null$configuration == primary_configuration,
  , drop = FALSE
]
summary_primary <- isolate_summary[
  isolate_summary$configuration == primary_configuration &
    isolate_summary$metric %in% c("candidate_count", "candidate_fraction"),
  , drop = FALSE
]
null_long <- rbind(
  data.frame(
    metric = "Candidate count", value = read_num(null_primary$candidate_count),
    observed = read_num(summary_primary$observed_value[summary_primary$metric == "candidate_count"]),
    stringsAsFactors = FALSE
  ),
  data.frame(
    metric = "Candidate fraction", value = read_num(null_primary$candidate_fraction),
    observed = read_num(summary_primary$observed_value[summary_primary$metric == "candidate_fraction"]),
    stringsAsFactors = FALSE
  )
)
fig4c <- ggplot2::ggplot(null_long, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(
    bins = 36, fill = light_grey, colour = paper, linewidth = 0.15
  ) +
  ggplot2::geom_vline(
    ggplot2::aes(xintercept = observed), colour = pigmented, linewidth = 0.78
  ) +
  ggplot2::facet_wrap(~metric, ncol = 1, scales = "free_x") +
  ggplot2::labs(
    title = "Observed events within repeated natural maps",
    subtitle = "Magenta lines are the observed count and fraction",
    x = "Replicated value", y = "Natural maps"
  ) +
  theme_publication(base_size = 7.6)

human <- rbind(
  data.frame(family = "Population", population_summary, check.names = FALSE),
  data.frame(family = "DID context", did_summary, check.names = FALSE)
)
human$observed <- read_num(human$observed_focal_minus_white_neighbour)
human$lower <- read_num(human$lower_95)
human$upper <- read_num(human$upper_95)
human$maxT <- read_num(human$maxT_FWER_p)
human_labels <- c(
  local_population_rank = "Focal-cell population",
  population_5km_rank = "Population within 5 km",
  population_10km_rank = "Population within 10 km",
  population_25km_rank = "Population within 25 km",
  population_50km_rank = "Population within 50 km",
  did_proximity_rank = "DID proximity",
  did_within_5km = "DID within 5 km",
  did_within_10km = "DID within 10 km",
  did_aligned_population_score = "Population–DID alignment",
  populated_beyond_did_score = "Population beyond DID"
)
human$feature_label <- factor(
  unname(human_labels[human$feature]), levels = rev(unname(human_labels))
)
human$maxT_label <- sprintf("maxT P=%.3f", human$maxT)
fig4d <- ggplot2::ggplot(
  human, ggplot2::aes(x = observed, y = feature_label, colour = family)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = mid_grey) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower, xmax = upper),
    width = 0, linewidth = 0.55, orientation = "y"
  ) +
  ggplot2::geom_point(size = 1.9) +
  ggplot2::geom_text(
    ggplot2::aes(label = maxT_label), hjust = -0.05,
    size = 1.85, colour = mid_grey, check_overlap = TRUE
  ) +
  ggplot2::scale_colour_manual(values = c("Population" = white_like, "DID context" = pigmented)) +
  ggplot2::coord_cartesian(xlim = c(min(human$lower) - 0.01, max(human$upper) + 0.12), clip = "off") +
  ggplot2::labs(
    title = "Post-selection human context",
    subtitle = "Natural-map 95% intervals; no familywise P < .05",
    x = "Candidate minus white-neighbour rank", y = NULL, colour = NULL
  ) +
  theme_publication(base_size = 7.2) +
  ggplot2::theme(plot.margin = ggplot2::margin(7, 35, 7, 7))

figure_4 <- (tag_panel(fig4a, "A") | tag_panel(fig4b, "B")) /
  (tag_panel(fig4c, "C") | tag_panel(fig4d, "D"))

figure_rows <- list(
  save_figure(figure_1, "Figure_1_measurement_two_part_phenotype", 1, 7.2, 7.25),
  save_figure(figure_2, "Figure_2_broad_environment_spatial_template", 2, 7.2, 7.35),
  save_figure(figure_3, "Figure_3_local_focal_bombus_boundaries", 3, 7.2, 7.25),
  save_figure(figure_4, "Figure_4_calibrated_local_departures", 4, 7.2, 7.35)
)
figure_manifest <- do.call(rbind, figure_rows)
figure_manifest$narrative_job <- rep(c(
  "measurement and two-part trait representation",
  "broad environment plus unresolved space",
  "scale shift to local focal-pollinator boundaries",
  "calibrated ecological departures and provenance follow-up"
), each = 2)
utils::write.csv(
  figure_manifest, file.path(output_dir, "figure_manifest.csv"), row.names = FALSE
)

primary_focal <- sharp_summary[sharp_summary$focal_strict_local_test %in% TRUE, , drop = FALSE]
primary_isolate <- isolate_summary[
  isolate_summary$configuration == primary_configuration &
    isolate_summary$metric %in% c("candidate_count", "candidate_fraction"),
  , drop = FALSE
]
figure_lock <- data.frame(
  metric = c(
    "phenotype_observations", "white_like_observations", "pigmented_observations",
    "cells_1km", "pigmentation_boundary_a", "focal_pairs_5km",
    "focal_mean_contrast", "focal_median_contrast", "focal_positive_fraction",
    "focal_signflip_p", "candidate_count", "candidate_count_p",
    "candidate_fraction", "candidate_fraction_p", "population_5km_maxT_p",
    "population_did_alignment_maxT_p"
  ),
  value = c(
    nrow(analysis), sum(analysis$pigmented_mixture50 == 0L),
    sum(analysis$pigmented_mixture50 == 1L), nrow(cells), threshold,
    read_num(primary_focal$n_nonoverlapping_pairs),
    read_num(primary_focal$mean_signed_bombus_difference),
    read_num(primary_focal$median_signed_bombus_difference),
    read_num(primary_focal$proportion_positive),
    read_num(primary_focal$signflip_one_sided_p),
    read_num(primary_isolate$observed_value[primary_isolate$metric == "candidate_count"]),
    read_num(primary_isolate$empirical_p[primary_isolate$metric == "candidate_count"]),
    read_num(primary_isolate$observed_value[primary_isolate$metric == "candidate_fraction"]),
    read_num(primary_isolate$empirical_p[primary_isolate$metric == "candidate_fraction"]),
    read_num(population_summary$maxT_FWER_p[population_summary$feature == "population_5km_rank"]),
    read_num(did_summary$maxT_FWER_p[did_summary$feature == "did_aligned_population_score"])
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  figure_lock, file.path(output_dir, "figure_numerical_lock.csv"), row.names = FALSE
)

utils::write.csv(
  data.frame(
    figure = 1:4,
    panels = c("A-D", "A-D", "A-D", "A-D"),
    generated_from = c(
      "phenotype observations and mixture outputs",
      "INLA-SPDE coefficients, cross-fitted cell predictions and performance",
      "current non-overlapping local-transition output",
      "current event-null, candidate and post-selection human-context outputs"
    ),
    stringsAsFactors = FALSE
  ),
  file.path(data_dir, "panel_source_index.csv"), row.names = FALSE
)

readme <- c(
  "# JBI main-figure bundle",
  "",
  "This directory is generated from checksum-locked current-paper artifacts.",
  "It contains four 600-dpi PNG review copies, four vector PDF copies,",
  "a per-file SHA-256 manifest, the input-source manifest and the numerical lock",
  "used to ensure that graphical polishing does not change the manuscript results.",
  "",
  "The figures preserve the current hierarchy:",
  "measurement -> broad template -> local focal-pollinator test -> calibrated departure.",
  "Five-species community turnover and montane guardrails remain Supporting Information."
)
writeLines(readme, file.path(output_dir, "README.md"), useBytes = TRUE)

message("Wrote JBI figure bundle to ", normalizePath(output_dir, winslash = "/", mustWork = TRUE))
print(figure_manifest[, c("figure", "format", "path", "size_bytes", "sha256")])
