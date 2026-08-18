#!/usr/bin/env Rscript

# Final manuscript-facing adapter.
# Figure 2 uses the finalized response-specific Broad observation models.
# Figure 3B replaces the historical four-PC balance diagnostic with the
# final-eight-axis audit of the already frozen Bombus transition pairs.
# Figure 4 is rebuilt from the current-Broad 16-candidate / 10,000-map human
# artifact rather than the superseded four-PC primary.
# The one-shot profile carries only inputs used by the current four figures;
# superseded Figure 4 products are intentionally not part of that contract.

source_path <- "scripts/build_jbi_figure_bundle.R"
if (!file.exists(source_path)) stop("Missing core figure builder: ", source_path, call. = FALSE)
builder_lines <- readLines(source_path, warn = FALSE, encoding = "UTF-8")

replace_literal_once <- function(lines, old, new) {
  hits <- which(grepl(old, lines, fixed = TRUE))
  if (length(hits) != 1L) {
    stop("Expected exactly one builder match for: ", old, "; found ", length(hits), call. = FALSE)
  }
  lines[[hits]] <- sub(old, new, lines[[hits]], fixed = TRUE)
  lines
}

drop_exact_once <- function(lines, target) {
  hits <- which(lines == target)
  if (length(hits) != 1L) {
    stop("Expected exactly one obsolete builder line: ", target, "; found ", length(hits), call. = FALSE)
  }
  lines[-hits]
}

replace_exact_range_once <- function(lines, start_line, end_line, replacement) {
  starts <- which(lines == start_line)
  ends <- which(lines == end_line)
  if (length(starts) != 1L) {
    stop("Expected exactly one builder range start; found ", length(starts), call. = FALSE)
  }
  ends <- ends[ends >= starts[[1]]]
  if (length(ends) != 1L) {
    stop("Expected exactly one builder range end after start; found ", length(ends), call. = FALSE)
  }
  start <- starts[[1]]
  end <- ends[[1]]
  before <- if (start > 1L) lines[seq_len(start - 1L)] else character()
  after <- if (end < length(lines)) lines[(end + 1L):length(lines)] else character()
  c(before, replacement, after)
}

# Figure 1 and Figure 2 need two compact numerical inputs that are preserved
# verbatim from the accepted Broad evidence. Route those reads to permanent,
# Git-frozen CSVs rather than making the current renderer depend on old result trees.
builder_lines <- replace_literal_once(
  builder_lines,
  "results/ecological_v11_pigmentation_hurdle/pigmentation_mixture_components.csv",
  "reproducibility/figure_inputs/pigmentation_mixture_components.csv"
)
builder_lines <- replace_literal_once(
  builder_lines,
  "results/ecological_v16_predictive_replication/predictive_replication_model_performance.csv",
  "reproducibility/figure_inputs/predictive_replication_model_performance.csv"
)

# The final adapter rebuilds Figure 4 from current-Broad outputs below. Remove
# the superseded isolate / neighbourhood / DID products from the core preflight
# and read block so exact reproduction does not require unused historical files.
obsolete_required_lines <- c(
  '  "results/ecological_v20_local_white_isolates/local_isolate_candidates.csv",',
  '  "results/ecological_v20_local_white_isolates/local_isolate_natural_null.csv",',
  '  "results/ecological_v20_local_white_isolates/local_isolate_natural_null_summary.csv",',
  '  "results/ecological_v21_local_human_neighbourhood/human_neighbourhood_population_scale_summary.csv",',
  '  "results/ecological_v22_did_human_context/did_contrast_summary.csv"'
)
for (line in obsolete_required_lines) builder_lines <- drop_exact_once(builder_lines, line)
builder_lines <- replace_literal_once(
  builder_lines,
  '  "results/ecological_v18_bombus_local_sharp_transition/sharp_transition_nonoverlapping_pairs.csv",',
  '  "results/ecological_v18_bombus_local_sharp_transition/sharp_transition_nonoverlapping_pairs.csv"'
)

obsolete_read_lines <- c(
  "candidates <- hb_read_csv(required_inputs[[10]])",
  "isolate_null <- hb_read_csv(required_inputs[[11]])",
  "isolate_summary <- hb_read_csv(required_inputs[[12]])",
  "population_summary <- hb_read_csv(required_inputs[[13]])",
  "did_summary <- hb_read_csv(required_inputs[[14]])"
)
for (line in obsolete_read_lines) builder_lines <- drop_exact_once(builder_lines, line)

builder_lines <- replace_exact_range_once(
  builder_lines,
  "candidates$longitude <- read_num(candidates$longitude)",
  '  (tag_panel(fig4c, "C") | tag_panel(fig4d, "D"))',
  c(
    "# The current adapter below supplies all data-driven Figure 4 panels.",
    "# Retain only the provenance-blind conceptual event panel as a placeholder.",
    "figure_4 <- fig4a"
  )
)

lock_start <- "primary_focal <- sharp_summary[sharp_summary$focal_strict_local_test %in% TRUE, , drop = FALSE]"
lock_write <- '  figure_lock, file.path(output_dir, "figure_numerical_lock.csv"), row.names = FALSE'
start_hits <- which(builder_lines == lock_start)
write_hits <- which(builder_lines == lock_write)
if (length(start_hits) != 1L || length(write_hits) != 1L || write_hits[[1]] <= start_hits[[1]]) {
  stop(
    "Could not isolate the legacy Figure 4 numerical-lock block.",
    call. = FALSE
  )
}
lock_end <- write_hits[[1]] + 1L
if (lock_end > length(builder_lines) || builder_lines[[lock_end]] != ")") {
  stop("Legacy numerical-lock write call has an unexpected shape.", call. = FALSE)
}
stable_lock <- c(
  "primary_focal <- sharp_summary[sharp_summary$focal_strict_local_test %in% TRUE, , drop = FALSE]",
  "if (nrow(primary_focal) != 1L) stop(\"Expected one focal local-transition row.\", call. = FALSE)",
  "figure_lock <- data.frame(",
  "  metric = c(",
  "    \"phenotype_observations\", \"white_like_observations\", \"pigmented_observations\",",
  "    \"cells_1km\", \"pigmentation_boundary_a\", \"focal_pairs_5km\",",
  "    \"focal_mean_contrast\", \"focal_median_contrast\", \"focal_positive_fraction\",",
  "    \"focal_signflip_p\"",
  "  ),",
  "  value = c(",
  "    nrow(analysis), sum(analysis$pigmented_mixture50 == 0L),",
  "    sum(analysis$pigmented_mixture50 == 1L), nrow(cells), threshold,",
  "    read_num(primary_focal$n_nonoverlapping_pairs),",
  "    read_num(primary_focal$mean_signed_bombus_difference),",
  "    read_num(primary_focal$median_signed_bombus_difference),",
  "    read_num(primary_focal$proportion_positive),",
  "    read_num(primary_focal$signflip_one_sided_p)",
  "  ),",
  "  stringsAsFactors = FALSE",
  ")",
  "utils::write.csv(",
  "  figure_lock, file.path(output_dir, \"figure_numerical_lock.csv\"), row.names = FALSE",
  ")"
)
before_lock <- if (start_hits[[1]] > 1L) builder_lines[seq_len(start_hits[[1]] - 1L)] else character()
after_lock <- if (lock_end < length(builder_lines)) builder_lines[(lock_end + 1L):length(builder_lines)] else character()
builder_lines <- c(before_lock, stable_lock, after_lock)

text <- paste(builder_lines, collapse = "\n")
replace_once <- function(text, old, new) {
  hits <- gregexpr(old, text, fixed = TRUE)[[1]]
  n_hits <- if (length(hits) == 1L && hits[[1]] < 0L) 0L else length(hits)
  if (n_hits != 1L) stop("Expected exactly one builder match for: ", old, "; found ", n_hits, call. = FALSE)
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

tmp <- tempfile(fileext = ".R")
on.exit(unlink(tmp), add = TRUE)
writeLines(strsplit(text, "\n", fixed = TRUE)[[1]], tmp, useBytes = TRUE)
sys.source(tmp, envir = .GlobalEnv, keep.source = FALSE)

utils::write.csv(fixed, file.path(data_dir, "figure2_final_observation_fixed_effects.csv"), row.names = FALSE)
utils::write.csv(range_data, file.path(data_dir, "figure2_final_observation_spatial_ranges.csv"), row.names = FALSE)

bombus_audit_path <- "results/bombus_final8_environment_audit/bombus_pair_environment_distance_audit.csv"
human_root <- "results/human_context_current_broad"
human_membership_path <- file.path(human_root, "current_broad_primary_candidate_membership.csv")
human_null_path <- file.path(human_root, "current_broad_primary_candidate_null.csv")
human_maxT_path <- file.path(human_root, "current_broad_primary_human_maxT.csv")
human_decision_path <- file.path(human_root, "current_broad_primary_decision.csv")
for (path in c(bombus_audit_path, human_membership_path, human_null_path, human_maxT_path, human_decision_path)) {
  if (!file.exists(path) || file.info(path)$size <= 0) stop("Missing current figure evidence: ", path, call. = FALSE)
}

# ---- Figure 3B: finalized Broad environmental balance diagnostic ----
bombus_env <- hb_read_csv(bombus_audit_path)
bombus_env <- bombus_env[
  abs(read_num(bombus_env$transition_threshold) - 1) < 1e-12,
  , drop = FALSE
]
bombus_env$radius <- factor(
  paste0(as.integer(read_num(bombus_env$radius_km)), " km"),
  levels = c("5 km", "10 km", "25 km")
)
balance_long <- rbind(
  data.frame(radius = bombus_env$radius, set = "Selected sharp transitions", distance = read_num(bombus_env$median_selected_final8)),
  data.frame(radius = bombus_env$radius, set = "All local graph edges", distance = read_num(bombus_env$median_all_local_edges_final8))
)
fig3b <- ggplot2::ggplot(balance_long, ggplot2::aes(x = radius, y = distance, fill = set)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.62) +
  ggplot2::scale_fill_manual(values = c("Selected sharp transitions" = pigmented, "All local graph edges" = light_grey)) +
  ggplot2::labs(
    title = "Fixed transitions are locally environment-balanced",
    subtitle = "RMS distance across the finalized eight Broad abiotic axes",
    x = "Neighbour radius", y = "Median standardized environmental distance", fill = NULL
  ) +
  theme_publication(base_size = 7.0) +
  ggplot2::theme(legend.position = "bottom")

figure_3 <- (tag_panel(fig3a, "A") | tag_panel(fig3b, "B")) /
  (tag_panel(fig3c, "C") | tag_panel(fig3d, "D"))
new_figure3 <- save_figure(figure_3, "Figure_3_local_focal_pollinator_boundaries", 3, 7.2, 7.1)
new_figure3$narrative_job <- "scale shift to sharp local focal-pollinator boundaries"

# ---- Figure 4: current-Broad candidate and human inference ----
membership <- hb_read_csv(human_membership_path)
current_null <- hb_read_csv(human_null_path)
human_final <- hb_read_csv(human_maxT_path)
decision <- hb_read_csv(human_decision_path)
if (nrow(decision) != 1L || as.integer(decision$candidate_count[[1]]) != 16L) {
  stop("Current-Broad decision lock must contain exactly 16 candidates.", call. = FALSE)
}

candidate_ids <- unique(as.character(membership$exact_site_id))
if (length(candidate_ids) != 16L) stop("Expected 16 unique current-Broad candidate IDs.", call. = FALSE)
candidate_cells <- cells[cells$exact_site_id %in% candidate_ids, , drop = FALSE]
if (nrow(candidate_cells) != 16L) stop("Candidate IDs do not align to the current cell table.", call. = FALSE)

# Keep the core conceptual event diagram (A), but current data drive B-D.
fig4b <- base_japan_map() +
  ggplot2::geom_point(
    data = cells, ggplot2::aes(x = longitude, y = latitude),
    shape = 21, fill = light_grey, colour = paper, stroke = 0.06, size = 0.75, alpha = 0.55
  ) +
  ggplot2::geom_point(
    data = candidate_cells, ggplot2::aes(x = longitude, y = latitude),
    shape = 21, fill = pigmented, colour = ink, stroke = 0.30, size = 2.0
  ) +
  ggplot2::labs(
    title = "Current-Broad local departure targets",
    subtitle = "16 candidates defined without human variables"
  )

null_two <- current_null[current_null$metric %in% c("candidate_count", "candidate_fraction"), , drop = FALSE]
null_two$label <- factor(
  ifelse(null_two$metric == "candidate_count", "Candidate count", "Candidate fraction"),
  levels = c("Candidate fraction", "Candidate count")
)
null_two$observed <- read_num(null_two$observed_value)
null_two$mean <- read_num(null_two$null_mean)
null_two$lower <- read_num(null_two$lower_95)
null_two$upper <- read_num(null_two$upper_95)
null_two$p <- read_num(null_two$empirical_p)
fig4c <- ggplot2::ggplot(null_two, ggplot2::aes(y = label)) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower, xmax = upper), width = 0, orientation = "y",
    linewidth = 0.65, colour = mid_grey
  ) +
  ggplot2::geom_point(ggplot2::aes(x = mean), shape = 1, size = 2.2, colour = mid_grey) +
  ggplot2::geom_point(ggplot2::aes(x = observed), shape = 21, size = 2.5, fill = pigmented, colour = ink) +
  ggplot2::geom_text(
    ggplot2::aes(x = upper, label = sprintf("P=%.3f", p)),
    hjust = -0.15, size = 2.2, colour = mid_grey
  ) +
  ggplot2::facet_wrap(~label, scales = "free_x", ncol = 1) +
  ggplot2::labs(
    title = "Departure frequency is compatible with natural maps",
    subtitle = "Filled point = observed; open point = null mean; line = 95% null interval",
    x = NULL, y = NULL
  ) +
  theme_publication(base_size = 7.0) +
  ggplot2::theme(strip.text.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())

human_plot <- human_final[!human_final$role %in% c("natural_forest_alternative", "natural_mountain_alternative"), , drop = FALSE]
human_plot$observed <- read_num(human_plot$observed_focal_minus_white_neighbour)
human_plot$lower <- read_num(human_plot$lower_95)
human_plot$upper <- read_num(human_plot$upper_95)
human_plot$maxT <- read_num(human_plot$maxT_FWER_p)
feature_labels <- c(
  local_population_rank = "Population: focal cell",
  population_5km_rank = "Population: 5 km",
  population_10km_rank = "Population: 10 km",
  population_25km_rank = "Population: 25 km",
  population_50km_rank = "Population: 50 km",
  did_proximity_rank = "DID proximity",
  road_proximity_rank = "Road proximity",
  built_up_fraction_rank = "Built-up fraction",
  forest_human_edge_rank = "Forest–human edge"
)
human_plot$label <- unname(feature_labels[as.character(human_plot$feature)])
human_plot$label <- factor(human_plot$label, levels = rev(unname(feature_labels)))
fig4d <- ggplot2::ggplot(human_plot, ggplot2::aes(x = observed, y = label)) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = mid_grey) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower, xmax = upper), width = 0, orientation = "y",
    linewidth = 0.48, colour = light_grey
  ) +
  ggplot2::geom_point(
    ggplot2::aes(fill = feature == "population_5km_rank"),
    shape = 21, size = 2.1, colour = ink
  ) +
  ggplot2::scale_fill_manual(values = c("TRUE" = pigmented, "FALSE" = paper), guide = "none") +
  ggplot2::geom_text(
    data = human_plot[human_plot$feature == "population_5km_rank", , drop = FALSE],
    ggplot2::aes(label = sprintf("global maxT P=%.3f", maxT)),
    hjust = -0.05, vjust = -0.65, size = 2.1, colour = pigmented
  ) +
  ggplot2::labs(
    title = "Short-scale settlement exposure is the leading context",
    subtitle = "Current-Broad post-selection family; 10,000 maps",
    x = "Candidate minus local white-neighbour rank", y = NULL
  ) +
  theme_publication(base_size = 6.8) +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 5.6), plot.margin = ggplot2::margin(7, 30, 7, 7))

figure_4 <- (tag_panel(fig4a, "A") | tag_panel(fig4b, "B")) /
  (tag_panel(fig4c, "C") | tag_panel(fig4d, "D"))
new_figure4 <- save_figure(figure_4, "Figure_4_calibrated_local_departures", 4, 7.2, 7.35)
new_figure4$narrative_job <- "calibrated ecological departures and provenance follow-up"

# Replace Figures 3 and 4 in the manifest.
figure_manifest <- figure_manifest[!as.integer(figure_manifest$figure) %in% c(3L, 4L), , drop = FALSE]
figure_manifest <- rbind(figure_manifest, new_figure3, new_figure4)
format_order <- match(as.character(figure_manifest$format), c("PNG", "PDF"))
figure_manifest <- figure_manifest[order(as.integer(figure_manifest$figure), format_order), , drop = FALSE]
utils::write.csv(figure_manifest, file.path(output_dir, "figure_manifest.csv"), row.names = FALSE)

# Current human table retained as a reviewable figure-data lock.
utils::write.csv(human_final, file.path(data_dir, "figure4_final_human_context_scenarios.csv"), row.names = FALSE)
utils::write.csv(null_two, file.path(data_dir, "figure4_current_broad_candidate_null.csv"), row.names = FALSE)
utils::write.csv(bombus_env, file.path(data_dir, "figure3_final8_environment_balance.csv"), row.names = FALSE)

# Replace superseded Figure 4 numerical locks with current-Broad primary values.
lock_path <- file.path(output_dir, "figure_numerical_lock.csv")
figure_lock <- hb_read_csv(lock_path)
remove_metrics <- c(
  "candidate_count", "candidate_count_p", "candidate_fraction", "candidate_fraction_p",
  "population_5km_maxT_p", "population_did_alignment_maxT_p",
  "population_5km_primary_global_maxT_p", "population_5km_primary_final8model_global_maxT_p",
  "population_5km_final8match_currentmodel_global_maxT_p", "population_5km_final8match_final8model_global_maxT_p"
)
figure_lock <- figure_lock[!figure_lock$metric %in% remove_metrics, , drop = FALSE]
focal_env <- bombus_env[read_num(bombus_env$radius_km) == 5, , drop = FALSE]
current_lock <- data.frame(
  metric = c(
    "candidate_count", "candidate_count_p", "candidate_fraction", "candidate_fraction_p",
    "population_5km_current_broad_global_maxT_p",
    "bombus_final8_selected_env_distance", "bombus_final8_all_edge_env_distance"
  ),
  value = c(
    16,
    read_num(null_two$empirical_p[null_two$metric == "candidate_count"]),
    read_num(null_two$observed[null_two$metric == "candidate_fraction"]),
    read_num(null_two$p[null_two$metric == "candidate_fraction"]),
    read_num(human_final$maxT_FWER_p[human_final$feature == "population_5km_rank"]),
    read_num(focal_env$median_selected_final8),
    read_num(focal_env$median_all_local_edges_final8)
  ),
  stringsAsFactors = FALSE
)
figure_lock <- rbind(figure_lock, current_lock)
utils::write.csv(figure_lock, lock_path, row.names = FALSE)

# Add new locked sources to the source manifest.
source_manifest_path <- file.path(output_dir, "figure_source_manifest.csv")
source_manifest <- hb_read_csv(source_manifest_path)
new_sources <- c(bombus_audit_path, human_membership_path, human_null_path, human_maxT_path, human_decision_path)
for (path in new_sources) {
  if (!path %in% source_manifest$path) {
    source_manifest <- rbind(source_manifest, data.frame(
      path = path, size_bytes = as.numeric(file.info(path)$size), sha256 = sha256_file(path), stringsAsFactors = FALSE
    ))
  }
}
utils::write.csv(source_manifest, source_manifest_path, row.names = FALSE)

panel_path <- file.path(data_dir, "panel_source_index.csv")
panel_source <- hb_read_csv(panel_path)
panel_source$generated_from[as.integer(panel_source$figure) == 3L] <-
  "frozen local-transition evidence plus final-eight-axis Broad balance audit"
panel_source$generated_from[as.integer(panel_source$figure) == 4L] <-
  "current-Broad 16-candidate event replay and 10,000-map global human-context adjudication"
utils::write.csv(panel_source, panel_path, row.names = FALSE)

cat("Final integrated Broad + Bombus + human figure adapter complete.\n")
