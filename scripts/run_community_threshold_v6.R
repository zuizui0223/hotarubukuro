args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = "") {
  index <- match(flag, args)
  if (is.na(index) || index == length(args)) default else args[index + 1L]
}

as_bool <- function(value) tolower(value) %in% c("true", "1", "yes", "y")

input <- arg_value(
  "--input",
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
)
output_dir <- arg_value(
  "--output-dir", "results/ecological_v13_community_threshold"
)
run_inla <- as_bool(arg_value("--run-inla", "true"))
bootstrap_repetitions <- as.integer(arg_value("--bootstrap", "2000"))

source("scripts/ecological_analysis_v2.R")
source("scripts/transition_zone_design_v5.R")
source("scripts/community_threshold_v6.R")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
data <- utils::read.csv(input, check.names = FALSE, stringsAsFactors = FALSE)
sites <- transition_population_table(data, cell_km = 1)
axes_result <- community_add_axes(sites, structural_zero = TRUE)
sites <- axes_result$data

presence_cv_nationwide <- community_presence_threshold_cv(
  sites, support = "nationwide_zero"
)
presence_cv_common <- community_presence_threshold_cv(
  sites, support = "common_support"
)
presence_cv <- rbind(presence_cv_nationwide, presence_cv_common)
presence_summary <- community_presence_cv_summary(presence_cv)
presence_effects_nationwide <- community_fit_presence_effects(
  sites, support = "nationwide_zero"
)
presence_effects_common <- community_fit_presence_effects(
  sites, support = "common_support"
)
presence_coefficients <- rbind(
  presence_effects_nationwide$coefficients,
  presence_effects_common$coefficients
)
presence_curves <- rbind(
  presence_effects_nationwide$curves,
  presence_effects_common$curves
)

background <- community_crossfit_background(sites)
sites$natural_presence_probability_v13 <- background$probability
edges <- community_edge_table(
  sites, axes_result$axes, background$probability, k = 5L, maximum_km = 100
)
edge_cv <- rbind(
  community_edge_threshold_cv(sites, edges, maximum_km = 25,
                              common_support_only = FALSE),
  community_edge_threshold_cv(sites, edges, maximum_km = 25,
                              common_support_only = TRUE)
)
edge_summary <- community_edge_cv_summary(edge_cv)

intensity <- community_crossfit_gaussian(
  sites, "pigment_intensity_z", weight = "n_pigmented"
)
sites$year_z <- transition_z(sites$year)
phenology <- community_crossfit_gaussian(
  sites, "DOY", weight = "n_observations", extra_terms = "year_z"
)
neighbourhoods <- transition_local_neighbourhoods(sites)
candidates <- community_candidate_table(
  sites, neighbourhoods, background$probability,
  intensity$residual, phenology$residual, radius_km = 25
)
candidate_pairs <- community_match_horticultural_controls(
  sites, candidates, background$probability,
  intensity$residual, phenology$residual
)
facet_summary <- community_facet_summary(
  candidate_pairs, repetitions = bootstrap_repetitions
)
facet_rank_trends <- community_facet_rank_trends(
  candidate_pairs, repetitions = bootstrap_repetitions
)
horticulture_sensitivity <- community_horticulture_matching_sensitivity(
  sites, candidates, background$probability, intensity$residual,
  phenology$residual, repetitions = max(500L, bootstrap_repetitions %/% 2L)
)

env_terms <- community_environment_terms(data)
environment_vif <- compute_vif(data, env_terms)
spde <- if (run_inla) {
  community_fit_spde_foundation(data, env_terms)
} else {
  list(metrics = data.frame(), fixed = data.frame(), hyper = data.frame(),
       status = "skipped by --run-inla")
}

species_columns <- paste0("bee_", community_species())
support_rows <- do.call(rbind, lapply(species_columns, function(column) {
  data.frame(
    variable = column,
    n_observations = nrow(data),
    n_finite = sum(is.finite(data[[column]])),
    finite_fraction = mean(is.finite(data[[column]])),
    structural_zero_interpretation = if (all(is.finite(data[[column]]))) {
      "not needed"
    } else {
      "projection-support absence only; checked against five-species common support"
    },
    stringsAsFactors = FALSE
  )
}))

data_quality <- data.frame(
  metric = c(
    "observations", "population_cells_1km", "white_cells", "pigmented_cells",
    "mixed_cells", "five_species_common_support_cells",
    "five_species_common_support_fraction", "candidate_pigmented_enclaves_25km",
    "matched_candidate_control_pairs", "manual_colour_region_review"
  ),
  value = as.character(c(
    nrow(data), nrow(sites), sum(sites$site_class == "white"),
    sum(sites$site_class == "pigmented"), sum(sites$site_class == "mixed"),
    sum(sites$bombus_common_support), mean(sites$bombus_common_support),
    nrow(candidates), nrow(candidate_pairs), "author_confirmed"
  )),
  interpretation = c(
    "image observations after the response-blind pigmentation classification",
    "primary local population unit; coordinate jitter is absorbed",
    "binary absence regime; a* is not interpreted as anthocyanin amount",
    "binary presence regime",
    "both regimes observed within a 1-km cell",
    "all five ENMeval predictions finite",
    "common-support sensitivity denominator is all 1-km cells",
    "defined without human, phenology, or intensity variables",
    "one non-reused pigmented control per candidate when feasible",
    "author inspected the extracted flower subject and specified the petal region"
  ),
  stringsAsFactors = FALSE
)

axis_columns <- c(
  "bombus_availability", "bombus_effective_richness", "bombus_alpine_share",
  "bombus_composition_pc1"
)
axis_correlations <- as.data.frame(stats::cor(
  sites[, axis_columns], use = "pairwise.complete.obs"
))
axis_correlations$axis <- rownames(axis_correlations)
rownames(axis_correlations) <- NULL
axis_correlations <- axis_correlations[, c("axis", axis_columns)]

metadata <- data.frame(
  field = c(
    "presence_response", "conditional_intensity_response", "national_foundation",
    "pollinator_availability", "pollinator_richness", "pollinator_composition",
    "threshold_estimand", "contact_zone_estimand", "terrain_claim_ceiling",
    "horticulture_candidate_definition", "horticulture_facets",
    "causal_claim_ceiling", "spde_status"
  ),
  value = c(
    "response-blind white versus pigmented Gaussian-mixture class",
    "pigment intensity only among pigmented flowers; white a* excluded",
    "nationwide environment plus SPDE; no East-West fixed split",
    "mean within-species rank of five ENMeval suitability predictions",
    "Hill N1 effective species number from continuous suitability composition",
    "three alpine-species share; Hellinger turnover for local pairs",
    "training-fold-selected hinge change point evaluated only in held-out spatial folds",
    "excess local colour discordance associated with community change after distance, environment, and natural background",
    "endpoint elevation/topography difference is a proxy, not an intervening barrier surface",
    "pigmented cell with at least five neighbours and <=10% pigmented neighbours, ranked by independent natural surprise",
    "population, forest edge, road access, conditional darkness, early flowering, and replication are separate tests",
    "observational pattern concordance; not visitation, selection, introgression, or horticultural provenance",
    spde$status
  ),
  stringsAsFactors = FALSE
)

write.csv(sites, file.path(output_dir, "community_population_cells_1km.csv"),
          row.names = FALSE)
write.csv(axes_result$axes$loadings,
          file.path(output_dir, "community_composition_pc1_loadings.csv"),
          row.names = FALSE)
write.csv(support_rows, file.path(output_dir, "community_enmeval_support.csv"),
          row.names = FALSE)
write.csv(axis_correlations,
          file.path(output_dir, "community_axis_correlations.csv"), row.names = FALSE)
write.csv(environment_vif,
          file.path(output_dir, "community_environment_vif.csv"), row.names = FALSE)
write.csv(data_quality, file.path(output_dir, "community_data_quality.csv"),
          row.names = FALSE)
write.csv(presence_cv,
          file.path(output_dir, "community_presence_threshold_spatial_cv.csv"),
          row.names = FALSE)
write.csv(presence_summary,
          file.path(output_dir, "community_presence_threshold_cv_summary.csv"),
          row.names = FALSE)
write.csv(presence_coefficients,
          file.path(output_dir, "community_presence_effect_coefficients.csv"),
          row.names = FALSE)
write.csv(presence_curves,
          file.path(output_dir, "community_presence_partial_dependence.csv"),
          row.names = FALSE)
write.csv(background$log,
          file.path(output_dir, "community_natural_background_crossfit_log.csv"),
          row.names = FALSE)
write.csv(edges, file.path(output_dir, "community_local_edges.csv"), row.names = FALSE)
write.csv(edge_cv, file.path(output_dir, "community_edge_spatial_cv.csv"),
          row.names = FALSE)
write.csv(edge_summary,
          file.path(output_dir, "community_edge_spatial_cv_summary.csv"),
          row.names = FALSE)
write.csv(rbind(intensity$log, phenology$log),
          file.path(output_dir, "community_intensity_phenology_crossfit_log.csv"),
          row.names = FALSE)
write.csv(candidates,
          file.path(output_dir, "community_horticultural_candidates.csv"),
          row.names = FALSE)
write.csv(candidate_pairs,
          file.path(output_dir, "community_horticultural_matched_pairs.csv"),
          row.names = FALSE)
write.csv(facet_summary,
          file.path(output_dir, "community_horticultural_facet_summary.csv"),
          row.names = FALSE)
write.csv(facet_rank_trends,
          file.path(output_dir, "community_horticultural_facet_rank_trends.csv"),
          row.names = FALSE)
write.csv(horticulture_sensitivity,
          file.path(output_dir, "community_horticultural_matching_sensitivity.csv"),
          row.names = FALSE)
write.csv(spde$metrics,
          file.path(output_dir, "community_spde_model_comparison.csv"),
          row.names = FALSE)
write.csv(spde$fixed,
          file.path(output_dir, "community_spde_fixed_effects.csv"),
          row.names = FALSE)
write.csv(spde$hyper,
          file.path(output_dir, "community_spde_hyperparameters.csv"),
          row.names = FALSE)
write.csv(metadata, file.path(output_dir, "community_analysis_metadata.csv"),
          row.names = FALSE)

cat("v13 community-threshold analysis written to", normalizePath(output_dir), "\n")
print(data_quality)
print(presence_summary)
print(edge_summary)
print(facet_summary)
print(spde$metrics)
