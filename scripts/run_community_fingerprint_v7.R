args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = "") {
  index <- match(flag, args)
  if (is.na(index) || index == length(args)) default else args[index + 1L]
}

input <- arg_value(
  "--input",
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
)
output_dir <- arg_value(
  "--output-dir", "results/ecological_v14_community_fingerprint"
)
worldpop <- arg_value("--worldpop", Sys.getenv("HOTARUBUKURO_WORLDPOP"))
bootstrap <- as.integer(arg_value("--bootstrap", "2000"))

source("scripts/ecological_analysis_v2.R")
source("scripts/transition_zone_design_v5.R")
source("scripts/community_threshold_v6.R")
source("scripts/community_fingerprint_v7.R")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
data <- utils::read.csv(input, check.names = FALSE, stringsAsFactors = FALSE)
sites <- transition_population_table(data, cell_km = 1)
fingerprint <- fingerprint_add_axes(sites)
sites <- fingerprint$data

presence_cv <- fingerprint_crossvalidate(sites, "presence")
intensity_cv <- fingerprint_crossvalidate(sites, "intensity")
predictions <- rbind(presence_cv$predictions, intensity_cv$predictions)
logs <- rbind(presence_cv$log, intensity_cv$log)
summary <- fingerprint_cv_summary(predictions)
contrasts <- fingerprint_cv_contrasts(summary)
curves <- fingerprint_fit_curves(sites)

background <- community_crossfit_background(sites)
intensity <- community_crossfit_gaussian(
  sites, "pigment_intensity_z", weight = "n_pigmented"
)
sites$year_z <- transition_z(sites$year)
phenology <- community_crossfit_gaussian(
  sites, "DOY", weight = "n_observations", extra_terms = "year_z"
)
anomalies <- fingerprint_anomaly_ranks(
  sites, background$probability, intensity$residual, phenology$residual
)

population_context <- data.frame()
facets <- data.frame()
if (nzchar(worldpop) && file.exists(worldpop)) {
  population_context <- fingerprint_extract_population_context(sites, worldpop)
  columns <- setdiff(names(population_context), c("longitude", "latitude"))
  anomalies <- merge(anomalies, population_context[columns],
                     by = "exact_site_id", all.x = TRUE, sort = FALSE)
  facets <- fingerprint_horticulture_facets(anomalies, bootstrap)
}

metadata <- data.frame(
  field = c(
    "bombus_construct", "species_weight", "abundance_claim",
    "primary_bombus_test", "intensity_definition", "cline_decomposition",
    "candidate_definition", "regional_split", "site_road_forest_primary",
    "human_context", "sampling_frame"
  ),
  value = c(
    "weighted five-species community suitability fingerprint",
    "within-species ENMeval rank; equal species prior, locally varying suitability weight",
    "not abundance, visitation, occupancy probability, or selection pressure",
    "one held-out block test of total support plus two Hellinger composition PCs",
    "z score of CIELAB a-star excess above the response-blind mixture boundary among pigmented flowers",
    "elevation-only total cline versus elevation beyond a spatial smooth; WAIC is not the sole criterion",
    "nationwide positive natural-surprise rank among pigmented cells",
    "none",
    "excluded because observations were selected along hiking trails",
    "WorldPop sums in 10, 25, and 50 km buffers; 25 km is primary",
    "trail-selected occurrences; observation-neighbour density is an ascertainment adjustment"
  ), stringsAsFactors = FALSE
)

write.csv(sites, file.path(output_dir, "fingerprint_population_cells_1km.csv"), row.names = FALSE)
write.csv(fingerprint$loadings, file.path(output_dir, "fingerprint_composition_loadings.csv"), row.names = FALSE)
write.csv(fingerprint$variance, file.path(output_dir, "fingerprint_composition_variance.csv"), row.names = FALSE)
write.csv(predictions, file.path(output_dir, "fingerprint_spatial_cv_predictions.csv"), row.names = FALSE)
write.csv(logs, file.path(output_dir, "fingerprint_spatial_cv_log.csv"), row.names = FALSE)
write.csv(summary, file.path(output_dir, "fingerprint_spatial_cv_summary.csv"), row.names = FALSE)
write.csv(contrasts, file.path(output_dir, "fingerprint_spatial_cv_contrasts.csv"), row.names = FALSE)
write.csv(curves$elevation, file.path(output_dir, "pigmented_intensity_elevation_clines.csv"), row.names = FALSE)
write.csv(curves$community, file.path(output_dir, "pigmented_intensity_community_curve.csv"), row.names = FALSE)
write.csv(anomalies, file.path(output_dir, "nationwide_natural_surprise_ranks.csv"), row.names = FALSE)
write.csv(population_context, file.path(output_dir, "worldpop_landscape_context.csv"), row.names = FALSE)
write.csv(facets, file.path(output_dir, "horticulture_rank_facets.csv"), row.names = FALSE)
write.csv(metadata, file.path(output_dir, "fingerprint_analysis_metadata.csv"), row.names = FALSE)

cat("v14 community-fingerprint analysis written to ", normalizePath(output_dir), "\n", sep = "")
print(contrasts)
print(facets)
