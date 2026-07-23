args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1L) args[1] else
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
output_dir <- if (length(args) >= 2L) args[2] else
  "results/ecological_v12_transition_zones"

source("scripts/transition_zone_design_v5.R")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
data <- read.csv(input, check.names = FALSE, stringsAsFactors = FALSE)
exact_sites <- transition_site_table(data)
sites <- transition_population_table(data, cell_km = 1)
population_unit_sensitivity <- transition_population_unit_sensitivity(data)
neighbourhoods <- transition_local_neighbourhoods(sites)
nearest <- transition_nearest_opposite(sites)
edges <- transition_local_edges(sites)
hotspot_pairs <- transition_hotspot_pairs(sites, edges, maximum_km = 25)
candidates <- transition_candidate_table(sites, neighbourhoods, radius_km = 25)
summary <- transition_feasibility_summary(sites, neighbourhoods, edges, candidates)
matched_pairs <- transition_matched_pairs(sites, maximum_km = 25, environment_caliper = 0.75)
matching_sensitivity <- transition_matching_sensitivity(sites)
block_summary <- transition_spatial_block_summary(sites, neighbourhoods, edges)
edge_cv <- transition_edge_spatial_cv(sites, edges, maximum_km = 25)
edge_cv_summary <- transition_edge_cv_summary(edge_cv)
enclave_descriptive <- transition_enclave_descriptive(candidates)

write.csv(
  exact_sites, file.path(output_dir, "transition_exact_coordinate_sites.csv"),
  row.names = FALSE
)
write.csv(sites, file.path(output_dir, "transition_population_cells_1km.csv"), row.names = FALSE)
write.csv(
  population_unit_sensitivity,
  file.path(output_dir, "transition_population_unit_sensitivity.csv"),
  row.names = FALSE
)
write.csv(
  neighbourhoods, file.path(output_dir, "transition_local_neighbourhoods.csv"),
  row.names = FALSE
)
write.csv(
  nearest$sites, file.path(output_dir, "transition_nearest_opposite_sites.csv"),
  row.names = FALSE
)
write.csv(
  nearest$summary, file.path(output_dir, "transition_nearest_opposite_summary.csv"),
  row.names = FALSE
)
write.csv(edges, file.path(output_dir, "transition_local_edges.csv"), row.names = FALSE)
write.csv(
  hotspot_pairs, file.path(output_dir, "transition_hotspot_pairs_25km.csv"),
  row.names = FALSE
)
write.csv(
  candidates, file.path(output_dir, "transition_enclave_candidates_25km.csv"),
  row.names = FALSE
)
write.csv(
  summary, file.path(output_dir, "transition_design_feasibility.csv"),
  row.names = FALSE
)
write.csv(
  matched_pairs, file.path(output_dir, "transition_matched_pairs_25km.csv"),
  row.names = FALSE
)
write.csv(
  matching_sensitivity, file.path(output_dir, "transition_matching_sensitivity.csv"),
  row.names = FALSE
)
write.csv(
  block_summary, file.path(output_dir, "transition_spatial_blocks_100km.csv"),
  row.names = FALSE
)
write.csv(
  edge_cv, file.path(output_dir, "transition_edge_spatial_cv_25km.csv"),
  row.names = FALSE
)
write.csv(
  edge_cv_summary,
  file.path(output_dir, "transition_edge_spatial_cv_summary_25km.csv"),
  row.names = FALSE
)
write.csv(
  enclave_descriptive,
  file.path(output_dir, "transition_enclave_symmetric_descriptive_25km.csv"),
  row.names = FALSE
)

metadata <- data.frame(
  item = c(
    "primary_geographic_scope", "east_west_role", "flower_response_1",
    "flower_response_2", "local_boundary_estimand", "bombus_composition",
    "bombus_availability", "primary_local_unit", "background_confounding_model",
    "horticultural_direction", "causal_limit"
  ),
  value = c(
    "one nationwide analysis",
    "descriptive or sensitivity analysis only",
    "white versus optically pigmented",
    "pigment intensity conditional on being pigmented",
    "short-edge or local-neighbourhood flower discordance conditional on geographic and environmental distance",
    "turnover of five within-species rank-normalized ENMeval suitability profiles",
    "mean within-species rank-normalized ENMeval suitability; not abundance or visitation",
    "1-km population cell; 0.5, 2, and 5 km are sensitivity analyses",
    "spatial-fold cross-fitted nationwide environment plus spatial smooth; excludes Bombus, East-West region, and human variables",
    "test pigmented-in-white-background against symmetric white-in-pigmented-background controls",
    "observational concordance and candidate detection; not direct selection or genetic introgression"
  ),
  stringsAsFactors = FALSE
)
write.csv(metadata, file.path(output_dir, "transition_design_metadata.csv"), row.names = FALSE)

cat("Transition-zone feasibility outputs written to", normalizePath(output_dir), "\n")
print(summary)
print(matching_sensitivity)
print(edge_cv_summary)
