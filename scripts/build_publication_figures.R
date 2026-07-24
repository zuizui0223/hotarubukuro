#!/usr/bin/env Rscript

# Build the four publication figures from the locked analysis artifacts.
# Each figure is exported as a high-resolution PNG for the review DOCX and as
# a vector PDF for journal submission.

source("R/pipeline_support.R")
hb_require_stage_packages("publication_figures")

output_dir <- file.path("manuscript", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

blue <- "#2B6CB0"
pink <- "#B83280"
ink <- "#263238"
mid_grey <- "#718096"
light_grey <- "#D9E2EC"
paper <- "#FFFFFF"

theme_publication <- function(base_size = 9) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "Arial") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", colour = ink, size = base_size + 1,
        margin = ggplot2::margin(b = 5)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = mid_grey, size = base_size - 0.5,
        margin = ggplot2::margin(b = 5)
      ),
      axis.title = ggplot2::element_text(colour = ink),
      axis.text = ggplot2::element_text(colour = ink),
      strip.text = ggplot2::element_text(face = "bold", colour = ink),
      strip.background = ggplot2::element_rect(
        fill = "#F4F7FA", colour = NA
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        colour = "#E8EDF2", linewidth = 0.25
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(7, 7, 7, 7)
    )
}

theme_map <- function() {
  theme_publication() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.key.height = grid::unit(3.2, "mm")
    )
}

tag_panel <- function(plot, label) {
  plot +
    ggplot2::labs(tag = label) +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(face = "bold", size = 11),
      plot.tag.position = c(0, 1)
    )
}

japan <- rnaturalearth::ne_countries(
  scale = "small", country = "Japan", returnclass = "sf"
)

base_japan_map <- function() {
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = japan, fill = "#F5F5F2", colour = "#AAB4BE",
      linewidth = 0.25
    ) +
    ggplot2::coord_sf(
      xlim = c(129.2, 142.4), ylim = c(32.0, 40.8),
      expand = FALSE
    ) +
    theme_map()
}

save_figure <- function(plot, stem, width = 7.2, height = 7.1) {
  png_path <- file.path(output_dir, paste0(stem, ".png"))
  pdf_path <- file.path(output_dir, paste0(stem, ".pdf"))
  ggplot2::ggsave(
    png_path, plot = plot, width = width, height = height,
    units = "in", dpi = 600, bg = paper
  )
  ggplot2::ggsave(
    pdf_path, plot = plot, width = width, height = height,
    units = "in", device = grDevices::cairo_pdf, bg = paper
  )
  message("Wrote ", normalizePath(png_path, winslash = "/", mustWork = TRUE))
  message("Wrote ", normalizePath(pdf_path, winslash = "/", mustWork = TRUE))
}

read_num <- function(x) suppressWarnings(as.numeric(x))

analysis <- hb_read_csv(
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
)
cells <- hb_read_csv(
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
components <- hb_read_csv(
  "results/ecological_v11_pigmentation_hurdle/pigmentation_mixture_components.csv"
)

analysis$colour_a <- read_num(analysis$colour_a)
analysis$longitude <- read_num(analysis$longitude)
analysis$latitude <- read_num(analysis$latitude)
analysis$pigmentation_class <- factor(
  analysis$pigmentation_class,
  levels = c("white", "pigmented"),
  labels = c("White-like", "Pigmented")
)
cells$longitude <- read_num(cells$longitude)
cells$latitude <- read_num(cells$latitude)
cells$conditional_intensity_median <- read_num(
  cells$conditional_intensity_median
)
threshold <- unique(read_num(analysis$pigment_boundary_a))
stopifnot(length(threshold) == 1L, is.finite(threshold))

# Figure 1: supervised workflow and two-stage response.
workflow <- data.frame(
  x = 1,
  y = 4:1,
  label = c(
    "YAMAP records\n1,965 photos",
    "Author review\nidentity + petal region",
    "Two-part phenotype\npresence + intensity",
    "Scale-aware models\nnational + local"
  ),
  fill = c(light_grey, light_grey, "#F7D6E7", "#D8E8F5")
)
fig1a <- ggplot2::ggplot(workflow, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_segment(
    data = data.frame(y = c(3.7, 2.7, 1.7), yend = c(3.3, 2.3, 1.3)),
    ggplot2::aes(x = 1, xend = 1, y = y, yend = yend),
    inherit.aes = FALSE, arrow = grid::arrow(length = grid::unit(2.2, "mm")),
    colour = mid_grey, linewidth = 0.6
  ) +
  ggplot2::geom_label(
    ggplot2::aes(label = label, fill = fill),
    linewidth = 0.25, label.padding = grid::unit(2.2, "mm"),
    size = 2.7, colour = ink
  ) +
  ggplot2::scale_fill_identity() +
  ggplot2::coord_cartesian(xlim = c(0.35, 1.65), ylim = c(0.55, 4.45)) +
  ggplot2::labs(title = "Supervised digital-phenotyping workflow") +
  ggplot2::theme_void(base_family = "Arial", base_size = 9) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold", colour = ink, size = 10, margin = ggplot2::margin(b = 5)
    ),
    plot.margin = ggplot2::margin(7, 7, 7, 7)
  )

fig1b <- ggplot2::ggplot(
  analysis,
  ggplot2::aes(x = colour_a, fill = pigmentation_class)
) +
  ggplot2::geom_histogram(
    bins = 45, position = "identity", alpha = 0.78,
    colour = paper, linewidth = 0.12
  ) +
  ggplot2::geom_vline(
    xintercept = threshold, linetype = "dashed", colour = ink,
    linewidth = 0.55
  ) +
  ggplot2::annotate(
    "text", x = threshold, y = Inf,
    label = sprintf("  boundary a* = %.2f", threshold),
    hjust = 0, vjust = 1.5, size = 2.5, colour = ink
  ) +
  ggplot2::scale_fill_manual(values = c("White-like" = blue, "Pigmented" = pink)) +
  ggplot2::labs(
    title = "Response-blind optical classification",
    x = "CIELAB a* (uncalibrated sRGB)", y = "Photographs", fill = "Stage 1"
  ) +
  theme_publication()

fig1c <- base_japan_map() +
  ggplot2::geom_point(
    data = analysis,
    ggplot2::aes(x = longitude, y = latitude, fill = pigmentation_class),
    shape = 21, colour = paper, stroke = 0.08, size = 1.05, alpha = 0.8
  ) +
  ggplot2::scale_fill_manual(
    values = c("White-like" = blue, "Pigmented" = pink)
  ) +
  ggplot2::labs(
    title = "Pigmentation presence",
    fill = "Stage 1"
  )

intensity_cells <- cells[is.finite(cells$conditional_intensity_median), ]
fig1d <- base_japan_map() +
  ggplot2::geom_point(
    data = intensity_cells,
    ggplot2::aes(
      x = longitude, y = latitude, colour = conditional_intensity_median
    ),
    size = 1.35, alpha = 0.9
  ) +
  ggplot2::scale_colour_gradient2(
    low = "#F2D7E5", mid = "#C35A91", high = "#701A4B",
    midpoint = 0, name = "Intensity\n(z score)"
  ) +
  ggplot2::labs(title = "Conditional intensity among pigmented flowers")

figure_1 <- (fig1a + fig1b) / (fig1c + fig1d) +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", size = 11))
save_figure(figure_1, "figure_1_two_part_phenotype")

# Figure 2: national environmental clines and residual spatial scale.
fixed <- hb_read_csv(
  "results/ecological_v11_pigmentation_hurdle/pigmentation_hurdle_inla_fixed_effects.csv"
)
fixed <- fixed[
  fixed$model %in% c(
    "presence_N_environment_space", "intensity_N_environment_space"
  ) & fixed$term != "Intercept",
]
fixed$mean <- read_num(fixed$mean)
fixed$lower <- read_num(fixed[["0.025quant"]])
fixed$upper <- read_num(fixed[["0.975quant"]])
term_labels <- c(
  env_Temperature_PC1 = "Temperature",
  env_precip_PC1 = "Precipitation",
  env_TemperatureSeasonality = "Temperature seasonality",
  env_PrecipSeasonality = "Precipitation seasonality",
  env_topo_PC1 = "Topography",
  env_soil_PC1 = "Soil PC1",
  env_soil_PC2 = "Soil PC2",
  env_RSDS = "Radiation"
)
fixed$term_label <- unname(term_labels[fixed$term])
fixed <- fixed[!is.na(fixed$term_label), ]
fixed$response <- ifelse(
  grepl("^presence", fixed$model), "Pigmentation presence",
  "Pigmented-only intensity"
)
fixed$term_label <- factor(
  fixed$term_label, levels = rev(unname(term_labels))
)

fig2a <- ggplot2::ggplot(
  fixed,
  ggplot2::aes(x = mean, y = term_label, colour = response)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = mid_grey) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower, xmax = upper),
    width = 0, linewidth = 0.65, orientation = "y"
  ) +
  ggplot2::geom_point(size = 2.1) +
  ggplot2::facet_wrap(~response, ncol = 2) +
  ggplot2::scale_colour_manual(
    values = c(
      "Pigmentation presence" = blue,
      "Pigmented-only intensity" = pink
    ),
    guide = "none"
  ) +
  ggplot2::labs(
    title = "Conditional environmental coefficients",
    x = "Posterior mean (95% credible interval)", y = NULL
  ) +
  theme_publication(base_size = 8.2)

hyper <- hb_read_csv(
  "results/ecological_v11_pigmentation_hurdle/pigmentation_hurdle_inla_hyperparameters.csv"
)
hyper <- hyper[
  hyper$model %in% c(
    "presence_N_environment_space", "intensity_N_environment_space"
  ) & hyper$hyperparameter == "Range for spatial",
]
hyper$mean <- read_num(hyper$mean)
hyper$lower <- read_num(hyper[["0.025quant"]])
hyper$upper <- read_num(hyper[["0.975quant"]])
hyper$response <- ifelse(
  grepl("^presence", hyper$model), "Pigmentation presence",
  "Pigmented-only intensity"
)
fig2b <- ggplot2::ggplot(
  hyper,
  ggplot2::aes(x = mean, y = response, colour = response)
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower, xmax = upper),
    width = 0, linewidth = 0.75, orientation = "y"
  ) +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::scale_colour_manual(
    values = c(
      "Pigmentation presence" = blue,
      "Pigmented-only intensity" = pink
    ),
    guide = "none"
  ) +
  ggplot2::labs(
    title = "Residual spatial range",
    x = "SPDE range (km; 95% credible interval)", y = NULL
  ) +
  theme_publication()

presence_checkpoint <- readRDS(
  "results/ecological_v16_predictive_replication/checkpoints/national_environment_spde_presence_draws1000.rds"
)
intensity_checkpoint <- readRDS(
  "results/ecological_v16_predictive_replication/checkpoints/national_environment_spde_intensity_draws1000.rds"
)
presence_map <- data.frame(
  exact_site_id = as.character(presence_checkpoint$cell_id),
  predicted = as.numeric(presence_checkpoint$latent_mean)
)
intensity_map <- data.frame(
  exact_site_id = as.character(intensity_checkpoint$cell_id),
  predicted = as.numeric(intensity_checkpoint$latent_mean)
)
map_base <- cells[, c("exact_site_id", "longitude", "latitude")]
map_base$exact_site_id <- as.character(map_base$exact_site_id)
presence_map <- merge(map_base, presence_map, by = "exact_site_id", all.y = TRUE)
intensity_map <- merge(map_base, intensity_map, by = "exact_site_id", all.y = TRUE)

fig2c <- base_japan_map() +
  ggplot2::geom_point(
    data = presence_map[is.finite(presence_map$predicted), ],
    ggplot2::aes(x = longitude, y = latitude, colour = predicted),
    size = 1.35
  ) +
  ggplot2::scale_colour_gradient(
    low = "#D7E8F5", high = "#174A7E",
    limits = c(0, 1), oob = scales::squish,
    name = NULL
  ) +
  ggplot2::labs(title = "Cross-fitted pigmentation probability")

intensity_limit <- max(abs(stats::quantile(
  intensity_map$predicted, c(0.02, 0.98), na.rm = TRUE
)))
fig2d <- base_japan_map() +
  ggplot2::geom_point(
    data = intensity_map[is.finite(intensity_map$predicted), ],
    ggplot2::aes(x = longitude, y = latitude, colour = predicted),
    size = 1.35
  ) +
  ggplot2::scale_colour_gradient2(
    low = blue, mid = "#F5F5F2", high = pink, midpoint = 0,
    limits = c(-intensity_limit, intensity_limit),
    oob = scales::squish, name = NULL
  ) +
  ggplot2::labs(title = "Cross-fitted conditional intensity")

figure_2 <- ((
  tag_panel(fig2a, "a") / tag_panel(fig2b, "b")
) | (
  tag_panel(fig2c, "c") / tag_panel(fig2d, "d")
)) +
  patchwork::plot_layout(widths = c(1.15, 1))
save_figure(figure_2, "figure_2_environment_space")

# Figure 3: incremental national information and local turnover tests.
folds <- hb_read_csv(
  "results/ecological_v16_predictive_replication/predictive_replication_model_fold_performance.csv"
)
folds <- folds[
  folds$model %in% c(
    "common_support_environment_spde_presence",
    "common_support_environment_spde_bombus_presence"
  ),
]
folds$AUC <- read_num(folds$AUC)
folds$heldout_spatial_fold <- as.factor(folds$heldout_spatial_fold)
folds$model_label <- ifelse(
  grepl("bombus", folds$model),
  "Environment + space + Bombus fingerprint",
  "Environment + space"
)
fig3a <- ggplot2::ggplot(
  folds,
  ggplot2::aes(
    x = model_label, y = AUC, group = heldout_spatial_fold
  )
) +
  ggplot2::geom_line(colour = "#AAB4BE", linewidth = 0.45) +
  ggplot2::geom_point(
    ggplot2::aes(colour = model_label), size = 2.1
  ) +
  ggplot2::stat_summary(
    ggplot2::aes(group = model_label),
    fun = mean, geom = "point", shape = 23, size = 3.2,
    fill = paper, colour = ink
  ) +
  ggplot2::scale_colour_manual(
    values = c(
      "Environment + space" = blue,
      "Environment + space + Bombus fingerprint" = pink
    ),
    guide = "none"
  ) +
  ggplot2::scale_x_discrete(
    labels = c("Environment\n+ space", "Add Bombus\nfingerprint")
  ) +
  ggplot2::labs(
    title = "Same-support held-out discrimination",
    subtitle = "Lines pair the same five spatial folds",
    x = NULL, y = "AUC"
  ) +
  theme_publication()

local_null <- hb_read_csv(
  "results/ecological_v17_local_pair_turnover/local_pair_predictive_null.csv"
)
local_summary <- hb_read_csv(
  "results/ecological_v17_local_pair_turnover/local_pair_predictive_summary.csv"
)
for (column in c("radius_km", "partial_beta")) {
  local_null[[column]] <- read_num(local_null[[column]])
}
for (column in c(
  "radius_km", "observed_partial_beta", "beta_null_lower_95",
  "beta_null_upper_95", "beta_empirical_p"
)) {
  local_summary[[column]] <- read_num(local_summary[[column]])
}
local_null <- local_null[
  local_null$predictor == "fingerprint_turnover",
]
local_summary <- local_summary[
  local_summary$predictor == "fingerprint_turnover",
]
local_summary$response_label <- ifelse(
  local_summary$response == "presence",
  "Pigmentation-share turnover", "Intensity turnover"
)

null_panel <- function(response_name, title) {
  null_subset <- local_null[
    local_null$response == response_name & local_null$radius_km == 25,
  ]
  observed <- local_summary$observed_partial_beta[
    local_summary$response == response_name &
      local_summary$radius_km == 25
  ]
  ggplot2::ggplot(null_subset, ggplot2::aes(x = partial_beta)) +
    ggplot2::geom_histogram(
      bins = 38, fill = light_grey, colour = paper, linewidth = 0.15
    ) +
    ggplot2::geom_vline(
      xintercept = observed, colour = pink, linewidth = 0.85
    ) +
    ggplot2::annotate(
      "text", x = observed, y = Inf, label = " observed",
      hjust = 0, vjust = 1.5, size = 2.5, colour = pink
    ) +
    ggplot2::labs(
      title = title, subtitle = "25-km natural-map reference",
      x = "Partial beta", y = "Replicated maps"
    ) +
    theme_publication()
}
fig3b <- null_panel("presence", "Pigmentation-share turnover")
fig3c <- null_panel("intensity", "Pigmented-only intensity turnover")

fig3d <- ggplot2::ggplot(
  local_summary,
  ggplot2::aes(
    x = radius_km, y = observed_partial_beta,
    colour = response_label, group = response_label
  )
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = mid_grey) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = beta_null_lower_95, ymax = beta_null_upper_95,
      fill = response_label
    ),
    alpha = 0.13, colour = NA
  ) +
  ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::geom_point(size = 2.1) +
  ggplot2::scale_colour_manual(
    values = c(
      "Pigmentation-share turnover" = blue,
      "Intensity turnover" = pink
    )
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Pigmentation-share turnover" = blue,
      "Intensity turnover" = pink
    ),
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(breaks = c(10, 25, 50)) +
  ggplot2::labs(
    title = "Pre-specified spatial-scale sensitivity",
    subtitle = "Bands are the 95% natural-map intervals",
    x = "Pair radius (km)", y = "Observed partial beta",
    colour = "Response"
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_publication()

figure_3 <- (fig3a + fig3b) / (fig3c + fig3d) +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", size = 11))
save_figure(figure_3, "figure_3_bombus_turnover")

# Figure 4: local isolates and human-context follow-up.
candidates <- hb_read_csv(
  "results/ecological_v20_local_white_isolates/local_isolate_candidates.csv"
)
candidates$longitude <- read_num(candidates$longitude)
candidates$latitude <- read_num(candidates$latitude)
candidates$candidate_rank <- read_num(candidates$candidate_rank)
candidates$early_predictive_q <- read_num(candidates$early_predictive_q)
candidates$dark_predictive_q <- read_num(candidates$dark_predictive_q)

cells$site_label <- factor(
  cells$site_class,
  levels = c("white", "mixed", "pigmented"),
  labels = c("White-only", "Mixed", "Pigmented-only")
)
fig4a <- base_japan_map() +
  ggplot2::geom_point(
    data = cells,
    ggplot2::aes(x = longitude, y = latitude, fill = site_label),
    shape = 21, colour = paper, stroke = 0.08, size = 1.1, alpha = 0.72
  ) +
  ggplot2::geom_point(
    data = candidates,
    ggplot2::aes(x = longitude, y = latitude),
    shape = 21, fill = paper, colour = ink, stroke = 0.65, size = 2.35
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "White-only" = blue, "Mixed" = light_grey,
      "Pigmented-only" = pink
    )
  ) +
  ggplot2::labs(
    title = "a   Local pigmented isolates (n = 16)",
    fill = "Cell class"
  )

isolate_null <- hb_read_csv(
  "results/ecological_v20_local_white_isolates/local_isolate_natural_null.csv"
)
isolate_null <- isolate_null[
  isolate_null$configuration == "primary_10km_env1_all_white",
]
isolate_long <- rbind(
  data.frame(
    metric = "Candidate count", value = read_num(isolate_null$candidate_count),
    observed = 16
  ),
  data.frame(
    metric = "Candidate fraction",
    value = read_num(isolate_null$candidate_fraction),
    observed = 0.0448
  )
)
fig4b <- ggplot2::ggplot(isolate_long, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(
    bins = 34, fill = light_grey, colour = paper, linewidth = 0.15
  ) +
  ggplot2::geom_vline(
    ggplot2::aes(xintercept = observed), colour = pink, linewidth = 0.85
  ) +
  ggplot2::facet_wrap(~metric, scales = "free_x", ncol = 1) +
  ggplot2::labs(
    title = "b   Natural-map extremeness",
    subtitle = "Observed values shown in magenta",
    x = "Replicated value", y = "Replicated maps"
  ) +
  theme_publication(base_size = 8.2)

population <- hb_read_csv(
  "results/ecological_v21_local_human_neighbourhood/human_neighbourhood_population_scale_summary.csv"
)
did <- hb_read_csv(
  "results/ecological_v22_did_human_context/did_contrast_summary.csv"
)
human <- rbind(
  data.frame(family = "Population", population, check.names = FALSE),
  data.frame(family = "DID context", did, check.names = FALSE)
)
human$observed <- read_num(human$observed_focal_minus_white_neighbour)
human$lower <- read_num(human$lower_95)
human$upper <- read_num(human$upper_95)
human$maxT <- read_num(human$maxT_FWER_p)
human_labels <- c(
  local_population_rank = "Local population",
  population_5km_rank = "Population 5 km",
  population_10km_rank = "Population 10 km",
  population_25km_rank = "Population 25 km",
  population_50km_rank = "Population 50 km",
  did_proximity_rank = "DID proximity",
  did_within_5km = "DID within 5 km",
  did_within_10km = "DID within 10 km",
  did_aligned_population_score = "Population-DID alignment",
  populated_beyond_did_score = "Population beyond DID"
)
human$feature_label <- unname(human_labels[human$feature])
human$feature_label <- factor(
  human$feature_label, levels = rev(unname(human_labels))
)
fig4c <- ggplot2::ggplot(
  human,
  ggplot2::aes(x = observed, y = feature_label, colour = family)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = mid_grey) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower, xmax = upper),
    width = 0, linewidth = 0.6, orientation = "y"
  ) +
  ggplot2::geom_point(size = 2.1) +
  ggplot2::scale_colour_manual(
    values = c("Population" = blue, "DID context" = pink),
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(-0.2, -0.1, 0, 0.1, 0.2)
  ) +
  ggplot2::labs(
    title = "c   Human-context contrasts",
    subtitle = "Null 95% intervals; all maxT p > .05",
    x = "Focal minus white-neighbour rank", y = NULL,
    colour = "Feature family"
  ) +
  theme_publication(base_size = 8)

tail_long <- rbind(
  data.frame(
    candidate_rank = candidates$candidate_rank,
    feature = "Early flowering", q = candidates$early_predictive_q
  ),
  data.frame(
    candidate_rank = candidates$candidate_rank,
    feature = "Dark intensity", q = candidates$dark_predictive_q
  )
)
fig4d <- ggplot2::ggplot(
  tail_long,
  ggplot2::aes(x = candidate_rank, y = q, colour = feature, shape = feature)
) +
  ggplot2::geom_hline(yintercept = 0.10, linetype = "dashed", colour = mid_grey) +
  ggplot2::geom_line(linewidth = 0.5) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::scale_colour_manual(
    values = c("Early flowering" = blue, "Dark intensity" = pink)
  ) +
  ggplot2::scale_shape_manual(values = c("Early flowering" = 16, "Dark intensity" = 17)) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE, title = NULL),
    shape = ggplot2::guide_legend(nrow = 2, byrow = TRUE, title = NULL)
  ) +
  ggplot2::scale_x_continuous(breaks = c(1, 4, 8, 12, 16)) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.2)
  ) +
  ggplot2::labs(
    title = "d   Early and dark tail checks",
    subtitle = "Dashed line: pre-specified q = .10 tail criterion",
    x = "Candidate rank", y = "Predictive q",
    colour = "Feature", shape = "Feature"
  ) +
  theme_publication()

figure_4 <- (fig4a + fig4b) / (fig4c + fig4d)
save_figure(figure_4, "figure_4_isolates_human_context")

manifest <- data.frame(
  figure = paste0("Figure ", 1:4),
  png = file.path(
    output_dir,
    c(
      "figure_1_two_part_phenotype.png",
      "figure_2_environment_space.png",
      "figure_3_bombus_turnover.png",
      "figure_4_isolates_human_context.png"
    )
  ),
  pdf = file.path(
    output_dir,
    c(
      "figure_1_two_part_phenotype.pdf",
      "figure_2_environment_space.pdf",
      "figure_3_bombus_turnover.pdf",
      "figure_4_isolates_human_context.pdf"
    )
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  manifest, file.path(output_dir, "figure_manifest.csv"), row.names = FALSE
)
