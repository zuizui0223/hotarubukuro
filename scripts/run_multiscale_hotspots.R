args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(flag, default = "") hb_arg_value(args, flag, default)

input <- arg_value(
  "--input",
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
)
output_dir <- arg_value(
  "--output-dir", "results/ecological_v15_multiscale_hotspots"
)
environment_cache <- arg_value(
  "--environment-cache", Sys.getenv("HOTARUBUKURO_PUBLIC_CACHE")
)
worldpop_raster <- arg_value("--worldpop-raster", "")
bootstrap <- as.integer(arg_value("--bootstrap", "1000"))

hb_require_stage_packages("multiscale_hotspots")
hb_load_modules("multiscale_hotspots")

if (!dir.exists(environment_cache)) {
  stop("Environment cache not found: ", environment_cache, call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
observations <- utils::read.csv(input, check.names = FALSE, stringsAsFactors = FALSE)
cells <- transition_population_table(observations, cell_km = 1)
fingerprint <- fingerprint_add_axes(cells)
cells <- fingerprint$data
phenotypes <- multiscale_cell_phenotypes(observations, cells)
cells <- phenotypes$cells

raster_paths <- multiscale_environment_sources(environment_cache)
context_result <- multiscale_environment_context(cells, raster_paths)
environment_axes <- multiscale_add_environment_axes(context_result$context)
context <- environment_axes$data
cells <- merge(
  cells,
  context[setdiff(names(context), c("longitude", "latitude"))],
  by = "exact_site_id", all.x = TRUE, sort = FALSE
)

if (!nzchar(worldpop_raster)) {
  hits <- list.files(
    environment_cache, pattern = "^population_count_Japan_crop\\.tif$",
    recursive = TRUE, full.names = TRUE
  )
  if (length(hits) != 1L) {
    stop(
      "Expected one cropped WorldPop raster; found ", length(hits),
      call. = FALSE
    )
  }
  worldpop_raster <- hits[[1]]
}
population_result <- multiscale_population_context(cells, worldpop_raster)
population_columns <- setdiff(
  names(population_result$context), c("longitude", "latitude")
)
cells <- merge(
  cells, population_result$context[population_columns],
  by = "exact_site_id", all.x = TRUE, sort = FALSE
)

write.csv(
  cells, file.path(output_dir, "multiscale_hotspot_cells_1km.csv"),
  row.names = FALSE
)
write.csv(
  context, file.path(output_dir, "multiscale_environment_context.csv"),
  row.names = FALSE
)
write.csv(
  context_result$provenance,
  file.path(output_dir, "multiscale_environment_provenance.csv"),
  row.names = FALSE
)
write.csv(
  population_result$provenance,
  file.path(output_dir, "multiscale_population_provenance.csv"),
  row.names = FALSE
)
cat("v15 scale context prepared for ", nrow(cells), " cells\n", sep = "")

cv <- multiscale_hotspot_cv(cells)
cat("v15 spatial cross-validation complete\n")
cv_summary <- multiscale_hotspot_cv_summary(cv$predictions)
cv_contrasts <- multiscale_hotspot_contrasts(cv_summary)
fold_metrics <- multiscale_hotspot_fold_metrics(cv$predictions)
fold_contrasts <- multiscale_hotspot_fold_contrasts(fold_metrics)
collinearity <- multiscale_collinearity_audit(cells)

background <- community_crossfit_background(cells)
fingerprint_background <- multiscale_crossfit_presence_fingerprint(cells)
phenology_background <- multiscale_crossfit_phenology(cells)
ranked <- multiscale_natural_ranks(cells, background$probability)
ranked <- multiscale_candidate_diagnostics(
  ranked, cv$predictions, phenology_background$prediction,
  fingerprint_background$probability
)
facets <- multiscale_horticulture_facets(ranked, bootstrap)
evidence_matrix <- multiscale_horticulture_evidence_matrix(ranked)
evidence_summary <- as.data.frame(table(evidence_matrix$followup_tier))
names(evidence_summary) <- c("followup_tier", "n_cells")
rank_sensitivity <- do.call(rbind, lapply(
  c("unexpected_pigmented", "unexpected_white"), function(direction) {
    data <- ranked[
      ranked$natural_surprise_direction == direction &
        is.finite(ranked$natural_surprise_rank) &
        is.finite(ranked$fingerprint_adjusted_rank_v15), , drop = FALSE
    ]
    primary_top <- data$natural_surprise_rank >= 0.8
    fingerprint_top <- data$fingerprint_adjusted_rank_v15 >= 0.8
    intersection <- sum(primary_top & fingerprint_top)
    union <- sum(primary_top | fingerprint_top)
    data.frame(
      direction = direction, n_common_support = nrow(data),
      spearman_rank_correlation = stats::cor(
        data$natural_surprise_rank, data$fingerprint_adjusted_rank_v15,
        method = "spearman"
      ),
      top20_intersection = intersection, top20_union = union,
      top20_jaccard = intersection / union, stringsAsFactors = FALSE
    )
  }
))
cat("v15 nationwide candidate facets complete\n")

quality <- data.frame(
  metric = c(
    "observations", "population_cells_1km", "cells_two_or_more_sites",
    "observed_mixed_hotspots", "cells_two_or_more_pigmented_sites",
    "cells_three_or_more_pigmented_sites", "five_species_common_support",
    "five_species_support_pigmented", "five_species_support_white",
    "primary_environment_radius_km", "candidate_regional_split",
    "site_road_forest_primary", "residual_as_primary_response"
  ),
  value = as.character(c(
    nrow(observations), nrow(cells), sum(cells$n_independent_sites >= 2),
    sum(cells$mixed_hotspot_observed == 1 & cells$n_independent_sites >= 2),
    sum(cells$n_pigmented_sites >= 2), sum(cells$n_pigmented_sites >= 3),
    sum(cells$bombus_fingerprint_common_support),
    sum(cells$bombus_fingerprint_common_support & cells$site_class == "pigmented"),
    sum(cells$bombus_fingerprint_common_support & cells$site_class == "white"),
    50, "none", "false", "false"
  )),
  interpretation = c(
    "manually confirmed petal-region image observations",
    "primary areal unit", "eligible for binary within-cell heterogeneity",
    "raw white-pigmented coexistence; not a residual",
    "eligible for robust pairwise intensity dispersion",
    "strict intensity-dispersion subset",
    "all five ENMeval surfaces finite",
    "common-support class count; limits nationwide candidate ranking",
    "common-support class count; limits nationwide candidate ranking",
    "25 and 100 km are prespecified sensitivity scales",
    "candidate ranks are nationwide", "excluded because occurrences follow hiking trails",
    "natural predictions rank candidates only"
  ), stringsAsFactors = FALSE
)

metadata <- data.frame(
  field = c(
    "national_foundation", "scale_decomposition", "binary_hotspot_response",
    "intensity_hotspot_response", "intensity_level_response",
    "bombus_construct", "bombus_claim_ceiling", "local_spatial_control",
    "horticulture_candidate_definition", "candidate_bee_support", "residual_role"
  ),
  value = c(
    "v13 nationwide environment plus SPDE remains the broad-scale foundation",
    "response-blind 25, 50, and 100 km raster means plus cell-minus-mean deviations",
    "observed within-cell white-pigmented coexistence among independent coordinates",
    "median pairwise difference among pigmented exact-site medians",
    "median pigment intensity among pigmented exact sites",
    "total within-species rank support plus two Hellinger composition PCs tested as one block",
    "relative habitat support and predicted composition; not abundance, visitation, or selection",
    "100-km folds, with spatial-smooth sensitivity instead of assuming cell independence",
    "nationwide positive natural-surprise rank; no East-West split or human facet in ranking",
    "five-species natural prediction is a common-support sensitivity, not the national rank",
    "out-of-fold intensity and DOY differences are candidate diagnostics only; never primary responses"
  ), stringsAsFactors = FALSE
)

write.csv(cells, file.path(output_dir, "multiscale_hotspot_cells_1km.csv"), row.names = FALSE)
write.csv(phenotypes$exact_sites, file.path(output_dir, "multiscale_exact_site_traits.csv"), row.names = FALSE)
write.csv(context, file.path(output_dir, "multiscale_environment_context.csv"), row.names = FALSE)
write.csv(context_result$provenance, file.path(output_dir, "multiscale_environment_provenance.csv"), row.names = FALSE)
write.csv(population_result$provenance, file.path(output_dir, "multiscale_population_provenance.csv"), row.names = FALSE)
write.csv(environment_axes$loadings, file.path(output_dir, "multiscale_environment_pca_loadings.csv"), row.names = FALSE)
write.csv(fingerprint$loadings, file.path(output_dir, "multiscale_bombus_fingerprint_loadings.csv"), row.names = FALSE)
write.csv(fingerprint$variance, file.path(output_dir, "multiscale_bombus_fingerprint_variance.csv"), row.names = FALSE)
write.csv(cv$predictions, file.path(output_dir, "multiscale_hotspot_spatial_cv_predictions.csv"), row.names = FALSE)
write.csv(cv$log, file.path(output_dir, "multiscale_hotspot_spatial_cv_log.csv"), row.names = FALSE)
write.csv(cv_summary, file.path(output_dir, "multiscale_hotspot_spatial_cv_summary.csv"), row.names = FALSE)
write.csv(cv_contrasts, file.path(output_dir, "multiscale_hotspot_spatial_cv_contrasts.csv"), row.names = FALSE)
write.csv(fold_metrics, file.path(output_dir, "multiscale_hotspot_fold_metrics.csv"), row.names = FALSE)
write.csv(fold_contrasts, file.path(output_dir, "multiscale_hotspot_fold_contrasts.csv"), row.names = FALSE)
write.csv(collinearity, file.path(output_dir, "multiscale_predictor_collinearity.csv"), row.names = FALSE)
write.csv(ranked, file.path(output_dir, "multiscale_natural_surprise_ranks.csv"), row.names = FALSE)
write.csv(facets, file.path(output_dir, "multiscale_horticulture_tier_facets.csv"), row.names = FALSE)
write.csv(evidence_matrix, file.path(output_dir, "multiscale_horticulture_evidence_matrix.csv"), row.names = FALSE)
write.csv(evidence_summary, file.path(output_dir, "multiscale_horticulture_evidence_summary.csv"), row.names = FALSE)
write.csv(rank_sensitivity, file.path(output_dir, "multiscale_candidate_rank_sensitivity.csv"), row.names = FALSE)
write.csv(background$log, file.path(output_dir, "multiscale_natural_prediction_crossfit_log.csv"), row.names = FALSE)
write.csv(fingerprint_background$log, file.path(output_dir, "multiscale_fingerprint_presence_crossfit_log.csv"), row.names = FALSE)
write.csv(phenology_background$log, file.path(output_dir, "multiscale_phenology_crossfit_log.csv"), row.names = FALSE)
write.csv(quality, file.path(output_dir, "multiscale_data_quality.csv"), row.names = FALSE)
write.csv(metadata, file.path(output_dir, "multiscale_analysis_metadata.csv"), row.names = FALSE)

cat("v15 multiscale hotspot analysis written to ", normalizePath(output_dir), "\n", sep = "")
print(quality)
print(cv_contrasts)
print(facets)
