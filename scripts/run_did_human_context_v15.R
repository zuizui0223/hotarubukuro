args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = NULL) {
  prefix <- paste0(name, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  sub(prefix, "", hit[1L], fixed = TRUE)
}

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
feature_path <- arg_value(
  "--features",
  paste0(
    "results/ecological_v21_local_human_neighbourhood/",
    "human_neighbourhood_cell_features.csv"
  )
)
checkpoint_root <- arg_value(
  "--checkpoint-root",
  "results/ecological_v16_predictive_replication/checkpoints"
)
template_path <- arg_value(
  "--template",
  paste0(
    "results/public_rasters/mlit_human_forest_edge_2021/",
    "mlit_human_forest_edge_1km.tif"
  )
)
did_cache <- arg_value(
  "--did-cache",
  file.path(
    Sys.getenv("USERPROFILE"), ".cache", "hotarubukuro",
    "mlit_did_2015"
  )
)
output_dir <- arg_value(
  "--output", "results/ecological_v22_did_human_context"
)
max_draws <- as.integer(arg_value("--max-draws", "1000"))

source("scripts/mlit_human_forest_edge.R")
source("scripts/extreme_human_diagnostic_v11.R")
source("scripts/human_landscape_extremes_v12.R")
source("scripts/local_white_isolate_v13.R")
source("scripts/local_human_neighbourhood_v14.R")
source("scripts/did_human_context_v15.R")

cells <- utils::read.csv(
  cells_path, check.names = FALSE, stringsAsFactors = FALSE
)
features <- utils::read.csv(
  feature_path, check.names = FALSE, stringsAsFactors = FALSE
)
feature_index <- match(cells$exact_site_id, features$exact_site_id)
if (anyNA(feature_index) || anyDuplicated(feature_index)) {
  stop("v21 features do not align one-to-one with cells.", call. = FALSE)
}
features <- features[feature_index, , drop = FALSE]

did_shapefile <- v22_ensure_did_shapefile(did_cache)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
did_presence_path <- file.path(
  output_dir, "mlit_did_presence_2015_1km.tif"
)
did_distance_path <- file.path(
  output_dir, "mlit_did_distance_2015_1km.tif"
)
if (file.exists(did_presence_path) && file.exists(did_distance_path)) {
  message("[v22] reusing cached DID rasters")
  did_rasters <- list(
    presence = terra::rast(did_presence_path),
    distance = terra::rast(did_distance_path)
  )
} else {
  message("[v22] rasterizing 2015 DID polygons to the study 1-km grid")
  did_rasters <- v22_build_did_distance_raster(
    did_shapefile, template_path
  )
  terra::writeRaster(
    did_rasters$presence, did_presence_path, overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
  )
  terra::writeRaster(
    did_rasters$distance, did_distance_path, overwrite = TRUE,
    wopt = list(
      datatype = "FLT8S",
      gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3")
    )
  )
  did_rasters <- list(
    presence = terra::rast(did_presence_path),
    distance = terra::rast(did_distance_path)
  )
}
did_context <- v22_extract_did_context(
  cells, did_rasters$distance, features$population_5km_rank
)

presence <- v18_align_result(
  readRDS(file.path(
    checkpoint_root,
    "national_environment_spde_presence_draws1000.rds"
  )),
  cells, "presence"
)
counts <- presence$draws
if (is.finite(max_draws) && max_draws > 0L &&
    ncol(counts) > max_draws) {
  counts <- counts[, seq_len(max_draws), drop = FALSE]
}
observed_q <- v18_predictive_tail_q(
  as.numeric(cells$n_pigmented), counts, "upper"
)
simulated_q <- v18_simulation_tail_q(counts, "upper")

primary_configuration <- v20_configuration_table()
primary_configuration <- primary_configuration[
  primary_configuration$role == "primary", , drop = FALSE
]
graph <- v20_neighbour_graph(
  cells,
  radius_km = primary_configuration$radius_km,
  environment_caliper = primary_configuration$environment_caliper,
  minimum_neighbours = primary_configuration$minimum_neighbours,
  same_fold_only = primary_configuration$same_fold_only
)
observed_profile <- v20_local_profile(
  cells$n_pigmented, graph,
  primary_configuration$maximum_neighbour_pigment_share
)
simulated_profile <- v20_local_profile(
  counts, graph,
  primary_configuration$maximum_neighbour_pigment_share
)

definitions <- v22_feature_definitions()
feature_names <- definitions$feature
analysis_support <- stats::complete.cases(
  did_context[, feature_names, drop = FALSE]
)
message("[v22] comparing DID context within the same white neighbourhoods")
observed <- v21_local_contrasts(
  observed_profile$present, observed_profile$candidate,
  graph, did_context, feature_names, analysis_support
)
simulated <- v21_local_contrasts(
  simulated_profile$present, simulated_profile$candidate,
  graph, did_context, feature_names, analysis_support
)
contrast_null <- data.frame(
  configuration = "primary_10km_env1_all_white",
  draw = seq_len(ncol(counts)),
  requested_cases = simulated$n_requested,
  usable_cases = simulated$n_usable,
  simulated$contrast,
  check.names = FALSE, stringsAsFactors = FALSE
)
contrast_summary <- v21_contrast_summary(
  observed$contrast[1L, ], contrast_null, definitions,
  "primary_10km_env1_all_white"
)

convergence_features <- c(
  "did_proximity_rank", "populated_beyond_did_score"
)
convergence_results <- lapply(convergence_features, function(feature) {
  spike <- v21_static_local_spike(
    graph, did_context[[feature]], analysis_support
  )
  v21_convergence_summary(
    observed_profile$candidate[, 1L],
    simulated_profile$candidate,
    observed_q, simulated_q, spike, feature,
    observed_profile$supported & analysis_support,
    spike_quantile = 0.90
  )
})
convergence_summary <- do.call(
  rbind, lapply(convergence_results, `[[`, "summary")
)
convergence_null <- do.call(
  rbind, lapply(convergence_results, `[[`, "null")
)
convergence_summary <- v22_apply_convergence_fwer(
  convergence_summary, convergence_null
)

composition <- v22_context_composition(
  observed_profile$candidate[, 1L],
  simulated_profile$candidate,
  did_context$human_context_class
)

candidate_index <- which(
  observed_profile$candidate[, 1L] & analysis_support
)
candidate_details <- did_context[candidate_index, , drop = FALSE]
candidate_details$unexpected_pigmented_q <- observed_q[candidate_index]
candidate_details$n_white_neighbours <- vapply(
  candidate_index, function(index) {
    neighbours <- graph$neighbours[[index]]
    sum(
      !observed_profile$present[neighbours, 1L] &
        analysis_support[neighbours]
    )
  }, numeric(1)
)
for (feature in feature_names) {
  candidate_details[[paste0(feature, "_white_mean")]] <- vapply(
    candidate_index, function(index) {
      neighbours <- graph$neighbours[[index]]
      neighbours <- neighbours[
        !observed_profile$present[neighbours, 1L] &
          analysis_support[neighbours]
      ]
      mean(did_context[[feature]][neighbours])
    }, numeric(1)
  )
  candidate_details[[paste0(feature, "_focal_minus_white")]] <-
    candidate_details[[feature]] -
    candidate_details[[paste0(feature, "_white_mean")]]
}
for (index in seq_along(convergence_features)) {
  feature <- convergence_features[index]
  convergence <- convergence_results[[index]]
  candidate_details[[paste0(feature, "_local_spike")]] <-
    convergence$spike[candidate_index]
  candidate_details[[paste0(feature, "_top10_spike")]] <-
    convergence$high_spike[candidate_index]
}
candidate_details$joint_q10_did_proximity_spike <- with(
  candidate_details,
  unexpected_pigmented_q <= 0.10 &
    did_proximity_rank_top10_spike
)
v21_followup_path <- paste0(
  "results/ecological_v21_local_human_neighbourhood/",
  "human_neighbourhood_followup_candidates.csv"
)
if (file.exists(v21_followup_path)) {
  followup <- utils::read.csv(
    v21_followup_path, check.names = FALSE,
    stringsAsFactors = FALSE
  )
  auxiliary <- intersect(
    c(
      "exact_site_id", "early_predictive_q", "dark_predictive_q",
      "settlement_density_score_local_spike",
      "human_activity_consensus_score_local_spike"
    ),
    names(followup)
  )
  candidate_details <- merge(
    candidate_details, followup[, auxiliary, drop = FALSE],
    by = "exact_site_id", all.x = TRUE, sort = FALSE
  )
}
candidate_details <- candidate_details[order(
  !candidate_details$joint_q10_did_proximity_spike,
  candidate_details$unexpected_pigmented_q,
  -candidate_details$did_proximity_rank,
  candidate_details$exact_site_id
), , drop = FALSE]
candidate_details$followup_rank <- seq_len(nrow(candidate_details))

collinearity <- v19_collinearity(
  did_context,
  c(
    feature_names, "population_5km_rank", "did_distance_rank"
  )
)

utils::write.csv(
  did_context,
  file.path(output_dir, "did_cell_context.csv"),
  row.names = FALSE
)
utils::write.csv(
  definitions,
  file.path(output_dir, "did_feature_definitions.csv"),
  row.names = FALSE
)
utils::write.csv(
  contrast_summary,
  file.path(output_dir, "did_contrast_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  contrast_null,
  file.path(output_dir, "did_contrast_null.csv"),
  row.names = FALSE
)
utils::write.csv(
  convergence_summary,
  file.path(output_dir, "did_convergence_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  convergence_null,
  file.path(output_dir, "did_convergence_null.csv"),
  row.names = FALSE
)
utils::write.csv(
  composition$summary,
  file.path(output_dir, "did_context_composition_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  composition$null,
  file.path(output_dir, "did_context_composition_null.csv"),
  row.names = FALSE
)
utils::write.csv(
  candidate_details,
  file.path(output_dir, "did_candidate_details.csv"),
  row.names = FALSE
)
utils::write.csv(
  collinearity,
  file.path(output_dir, "did_collinearity.csv"),
  row.names = FALSE
)

archive <- file.path(did_cache, "A16-15_GML.zip")
provenance <- data.frame(
  source_title = paste(
    "MLIT National Land Numerical Information",
    "Densely Inhabited District 2015"
  ),
  source_url = v22_did_source_url,
  archive_path = normalizePath(archive, winslash = "/"),
  archive_md5 = unname(tools::md5sum(archive)),
  source_crs = "JGD2011 EPSG:6668",
  distance_method = paste(
    "nearest 1-km template cell touched by a DID polygon;",
    "distance in metres from terra::distance"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  provenance, file.path(output_dir, "did_provenance.csv"),
  row.names = FALSE
)

metadata <- data.frame(
  field = c(
    "analysis_spec_version", "generated_at", "n_natural_maps",
    "candidate_selector", "did_feature_role", "did_year",
    "did_spatial_grain", "residual_used_as_response",
    "causal_claim_ceiling", "final_Rmd_md5"
  ),
  value = c(
    v22_analysis_spec_version, as.character(Sys.time()),
    as.character(ncol(counts)),
    paste(
      "primary local pigmented isolate selector fixed in v20/v21;",
      "DID and population do not select candidates or neighbours"
    ),
    paste(
      "post-selection human-landscape characterization and",
      "natural-map replicated sensitivity analysis"
    ),
    "2015", "1-km raster approximation", "false",
    paste(
      "association with dense-settlement context only;",
      "not planting, escape, horticultural origin, or introgression"
    ),
    if (file.exists("final.Rmd")) {
      unname(tools::md5sum("final.Rmd"))
    } else {
      NA_character_
    }
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  metadata, file.path(output_dir, "did_metadata.csv"),
  row.names = FALSE
)

readme <- c(
  "# v22 DID human-context sensitivity analysis",
  "",
  paste(
    "The fixed v20/v21 local pigmented-isolate candidates are characterized",
    "using distance to 2015 Densely Inhabited Districts (DID)."
  ),
  "",
  paste(
    "Each focal cell is compared with its own environment-similar observed",
    "white neighbours. The identical calculation is repeated for 1000",
    "natural predictive maps."
  ),
  "",
  paste(
    "DID describes dense urban settlement, not all rural settlements.",
    "Population outside DID is therefore an exploratory landscape class,",
    "not evidence of horticultural provenance."
  ),
  "",
  "`final.Rmd` is intentionally not modified."
)
writeLines(readme, file.path(output_dir, "README.md"), useBytes = TRUE)

cat("v22 DID human-context analysis written to ",
    normalizePath(output_dir), "\n")
print(contrast_summary)
print(convergence_summary)
print(composition$summary)
