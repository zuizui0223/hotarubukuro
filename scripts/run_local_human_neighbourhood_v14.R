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
observations_path <- arg_value(
  "--observations",
  paste0(
    "results/ecological_v11_pigmentation_hurdle/",
    "analysis_data_pigmentation_hurdle.csv"
  )
)
base_features_path <- arg_value(
  "--base-features",
  paste0(
    "results/ecological_v19_human_landscape_extremes/",
    "landscape_cell_features.csv"
  )
)
worldpop_raster <- arg_value("--worldpop-raster", "")
mlit_dir <- arg_value(
  "--mlit-dir", "results/public_rasters/mlit_human_forest_edge_2021"
)
mlit_cache <- arg_value(
  "--mlit-cache",
  file.path(
    Sys.getenv("USERPROFILE"), ".cache", "hotarubukuro",
    "mlit_l03_2021"
  )
)
checkpoint_root <- arg_value(
  "--checkpoint-root",
  "results/ecological_v16_predictive_replication/checkpoints"
)
output_dir <- arg_value(
  "--output",
  "results/ecological_v21_local_human_neighbourhood"
)
max_draws <- as.integer(arg_value("--max-draws", "1000"))

source("scripts/mlit_human_forest_edge.R")
source("scripts/extreme_human_diagnostic_v11.R")
source("scripts/human_landscape_extremes_v12.R")
source("scripts/local_white_isolate_v13.R")
source("scripts/local_human_neighbourhood_v14.R")
source("scripts/multiscale_hotspots_v8.R")

cells <- utils::read.csv(
  cells_path, check.names = FALSE, stringsAsFactors = FALSE
)
observations <- utils::read.csv(
  observations_path, check.names = FALSE, stringsAsFactors = FALSE
)
base_features <- utils::read.csv(
  base_features_path, check.names = FALSE, stringsAsFactors = FALSE
)
base_index <- match(cells$exact_site_id, base_features$exact_site_id)
if (anyNA(base_index) || anyDuplicated(base_index)) {
  stop("Base features do not align one-to-one with cells.", call. = FALSE)
}
base_features <- base_features[base_index, , drop = FALSE]
if (!nzchar(worldpop_raster)) {
  population_provenance <- utils::read.csv(
    paste0(
      "results/ecological_v15_multiscale_hotspots/",
      "multiscale_population_provenance.csv"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  worldpop_raster <- population_provenance$raster_path[1L]
}
if (!file.exists(worldpop_raster)) {
  stop("WorldPop raster is unavailable: ", worldpop_raster,
       call. = FALSE)
}

manifest <- utils::read.csv(
  file.path(mlit_dir, "download_manifest.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
message("[v21] separating seven MLIT human land-use classes")
class_cells <- v21_process_mlit_classes(
  manifest$primary_mesh, mlit_cache,
  file.path(output_dir, "processed_primary_meshes")
)
class_lookup <- v21_lookup_landuse_cells(
  observations, class_cells,
  file.path(mlit_dir, "mlit_human_forest_edge_1km.tif")
)
features <- v21_add_human_features(base_features, class_lookup$cells)
message("[v21] recomputing WorldPop sums at true 5/10/25/50-km radii")
worldpop <- terra::rast(worldpop_raster)
population_radii <- c(5L, 10L, 25L, 50L)
population_sum <- multiscale_point_context(
  worldpop, cells, population_radii, summary_function = "sum"
)
population_context <- cells[, c(
  "exact_site_id", "longitude", "latitude"
)]
for (radius in population_radii) {
  source_column <- paste0("population_sum_", radius, "km")
  population_context[[source_column]] <-
    population_sum[, as.character(radius)]
  population_context[[paste0("log_population_sum_", radius, "km")]] <-
    log1p(pmax(population_context[[source_column]], 0))
  output_column <- paste0("population_", radius, "km_rank")
  features[[output_column]] <- v19_rank01(
    population_context[[paste0("log_population_sum_", radius, "km")]]
  )
}
features$regional_population_rank <- features$population_25km_rank
features$settlement_density_score <- rowMeans(features[, c(
  "local_population_rank", "regional_population_rank",
  "built_up_fraction_rank"
)])
features$human_activity_consensus_score <- rowMeans(features[, c(
  "settlement_density_score", "transport_access_score",
  "cultivation_interface_score", "artificial_land_score"
)])
features$observation_effort_rank <-
  v19_rank01(log1p(as.numeric(cells$n_observations)))
features$independent_site_support_rank <-
  v19_rank01(log1p(as.numeric(cells$n_independent_sites)))

definitions <- v21_feature_definitions()
feature_names <- definitions$feature
core_features <- v21_core_features()
analysis_support <- stats::complete.cases(
  features[, feature_names, drop = FALSE]
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

configurations <- v20_configuration_table()
contrast_summaries <- list()
contrast_null_rows <- list()
global_rows <- list()
support_rows <- list()
observed_detail_rows <- list()

evaluate_configuration <- function(
    configuration, support_mask, output_label = NULL) {
  label <- if (is.null(output_label)) {
    configuration$configuration
  } else {
    output_label
  }
  graph <- v20_neighbour_graph(
    cells,
    radius_km = configuration$radius_km,
    environment_caliper = configuration$environment_caliper,
    minimum_neighbours = configuration$minimum_neighbours,
    same_fold_only = configuration$same_fold_only
  )
  observed_profile <- v20_local_profile(
    cells$n_pigmented, graph,
    configuration$maximum_neighbour_pigment_share
  )
  simulated_profile <- v20_local_profile(
    counts, graph,
    configuration$maximum_neighbour_pigment_share
  )
  observed <- v21_local_contrasts(
    observed_profile$present, observed_profile$candidate,
    graph, features, feature_names, support_mask
  )
  simulated <- v21_local_contrasts(
    simulated_profile$present, simulated_profile$candidate,
    graph, features, feature_names, support_mask
  )
  null <- data.frame(
    configuration = label,
    draw = seq_len(ncol(counts)),
    requested_cases = simulated$n_requested,
    usable_cases = simulated$n_usable,
    simulated$contrast,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  summary <- v21_contrast_summary(
    observed$contrast[1L, ], null, definitions, label
  )
  global <- v19_global_landscape_test(
    observed$contrast[1L, ], null, core_features, label
  )
  names(global)[names(global) == "tier"] <- "configuration"
  details <- observed$details[[1L]]
  if (!is.null(details) && nrow(details)) {
    details$configuration <- label
    details$exact_site_id <- cells$exact_site_id[details$case_index]
    details$longitude <- cells$longitude[details$case_index]
    details$latitude <- cells$latitude[details$case_index]
    details$primary_mesh_boundary <-
      features$primary_mesh_boundary[details$case_index]
    details$human_activity_consensus_focal <-
      features$human_activity_consensus_score[details$case_index]
    details$human_activity_consensus_white_mean <-
      details$human_activity_consensus_focal -
      details$human_activity_consensus_score
  }
  support <- data.frame(
    configuration = label,
    role = if (is.null(output_label)) {
      configuration$role
    } else {
      "quality_sensitivity"
    },
    observed_requested_cases = observed$n_requested,
    observed_usable_cases = observed$n_usable,
    null_mean_requested_cases = mean(simulated$n_requested),
    null_mean_usable_cases = mean(simulated$n_usable),
    n_natural_maps = ncol(counts),
    stringsAsFactors = FALSE
  )
  list(
    graph = graph, observed_profile = observed_profile,
    simulated_profile = simulated_profile,
    observed = observed, simulated = simulated,
    summary = summary, null = null, global = global,
    details = details, support = support
  )
}

message("[v21] comparing each focal isolate with its own white neighbours")
primary_result <- NULL
for (row in seq_len(nrow(configurations))) {
  configuration <- configurations[row, , drop = FALSE]
  result <- evaluate_configuration(configuration, analysis_support)
  contrast_summaries[[length(contrast_summaries) + 1L]] <- result$summary
  contrast_null_rows[[length(contrast_null_rows) + 1L]] <- result$null
  global_rows[[length(global_rows) + 1L]] <- result$global
  support_rows[[length(support_rows) + 1L]] <- result$support
  if (!is.null(result$details) && nrow(result$details)) {
    observed_detail_rows[[length(observed_detail_rows) + 1L]] <-
      result$details
  }
  if (configuration$role == "primary") primary_result <- result
}

primary_configuration <- configurations[
  configurations$role == "primary", , drop = FALSE
]
boundary_support <- analysis_support &
  !(features$primary_mesh_boundary %in% TRUE)
boundary_result <- evaluate_configuration(
  primary_configuration, boundary_support,
  "primary_10km_env1_all_white_exclude_mlit_boundary"
)
contrast_summaries[[length(contrast_summaries) + 1L]] <-
  boundary_result$summary
contrast_null_rows[[length(contrast_null_rows) + 1L]] <-
  boundary_result$null
global_rows[[length(global_rows) + 1L]] <- boundary_result$global
support_rows[[length(support_rows) + 1L]] <- boundary_result$support
if (!is.null(boundary_result$details) && nrow(boundary_result$details)) {
  observed_detail_rows[[length(observed_detail_rows) + 1L]] <-
    boundary_result$details
}

convergence_features <- c(
  "settlement_density_score", "human_activity_consensus_score"
)
convergence_results <- lapply(convergence_features, function(feature) {
  spike <- v21_static_local_spike(
    primary_result$graph, features[[feature]], analysis_support
  )
  v21_convergence_summary(
    primary_result$observed_profile$candidate[, 1L],
    primary_result$simulated_profile$candidate,
    observed_q, simulated_q, spike, feature,
    primary_result$observed_profile$supported & analysis_support,
    spike_quantile = 0.90
  )
})
convergence_summary <- do.call(
  rbind, lapply(convergence_results, `[[`, "summary")
)
convergence_null <- do.call(
  rbind, lapply(convergence_results, `[[`, "null")
)
convergence_summary$BH_q <- stats::p.adjust(
  convergence_summary$empirical_p, method = "BH"
)
convergence_keys <- paste(
  convergence_summary$spike_feature, convergence_summary$metric, sep = "::"
)
convergence_null_matrix <- sapply(
  seq_len(nrow(convergence_summary)), function(index) {
    block <- convergence_null[
      convergence_null$spike_feature ==
        convergence_summary$spike_feature[index], ,
      drop = FALSE
    ]
    block[[convergence_summary$metric[index]]]
  }
)
colnames(convergence_null_matrix) <- convergence_keys
convergence_center <- colMeans(convergence_null_matrix)
convergence_sd <- apply(convergence_null_matrix, 2, stats::sd)
convergence_sd[
  !is.finite(convergence_sd) | convergence_sd <= 1e-12
] <- 1
convergence_null_z <- sweep(
  sweep(convergence_null_matrix, 2, convergence_center, "-"),
  2, convergence_sd, "/"
)
convergence_observed_z <- (
  convergence_summary$observed_value - convergence_center
) / convergence_sd
convergence_max_z <- apply(convergence_null_z, 1, max)
convergence_summary$maxT_FWER_p <- vapply(
  seq_len(nrow(convergence_summary)), function(index) {
    (1 + sum(convergence_max_z >= convergence_observed_z[index])) /
      (length(convergence_max_z) + 1)
  }, numeric(1)
)

primary_candidate_index <- which(
  primary_result$observed_profile$candidate[, 1L] & analysis_support
)
followup_candidates <- data.frame(
  exact_site_id = cells$exact_site_id[primary_candidate_index],
  longitude = cells$longitude[primary_candidate_index],
  latitude = cells$latitude[primary_candidate_index],
  unexpected_pigmented_q = observed_q[primary_candidate_index],
  local_population_rank =
    features$local_population_rank[primary_candidate_index],
  built_up_fraction =
    features$built_up_fraction[primary_candidate_index],
  settlement_density_score =
    features$settlement_density_score[primary_candidate_index],
  human_activity_consensus_score =
    features$human_activity_consensus_score[primary_candidate_index],
  stringsAsFactors = FALSE
)
for (index in seq_along(convergence_features)) {
  feature <- convergence_features[index]
  result <- convergence_results[[index]]
  percentile <- v19_rank01(result$spike)
  followup_candidates[[paste0(feature, "_local_spike")]] <-
    result$spike[primary_candidate_index]
  followup_candidates[[paste0(feature, "_spike_percentile")]] <-
    percentile[primary_candidate_index]
  followup_candidates[[paste0(feature, "_top10_spike")]] <-
    result$high_spike[primary_candidate_index]
}
v20_candidates_path <- paste0(
  "results/ecological_v20_local_white_isolates/",
  "local_isolate_candidates.csv"
)
if (file.exists(v20_candidates_path)) {
  v20_candidates <- utils::read.csv(
    v20_candidates_path, check.names = FALSE,
    stringsAsFactors = FALSE
  )
  auxiliary_columns <- intersect(
    c(
      "exact_site_id", "early_predictive_q", "dark_predictive_q",
      "early_tail_10", "dark_tail_10"
    ),
    names(v20_candidates)
  )
  followup_candidates <- merge(
    followup_candidates,
    v20_candidates[, auxiliary_columns, drop = FALSE],
    by = "exact_site_id", all.x = TRUE, sort = FALSE
  )
}
followup_candidates$joint_q10_consensus_spike <- with(
  followup_candidates,
  unexpected_pigmented_q <= 0.10 &
    human_activity_consensus_score_top10_spike
)
followup_candidates <- followup_candidates[order(
  !followup_candidates$joint_q10_consensus_spike,
  followup_candidates$unexpected_pigmented_q,
  -followup_candidates$human_activity_consensus_score_spike_percentile,
  followup_candidates$exact_site_id
), , drop = FALSE]
followup_candidates$followup_rank <- seq_len(nrow(followup_candidates))

contrast_summary <- do.call(rbind, contrast_summaries)
contrast_null <- do.call(rbind, contrast_null_rows)
global_summary <- do.call(rbind, global_rows)
configuration_support <- do.call(rbind, support_rows)
observed_details <- do.call(rbind, observed_detail_rows)

rank_features <- unique(c(
  feature_names,
  paste0(v21_landuse_registry()$feature, "_rank"),
  "agriculture_fraction_rank", "transport_land_fraction_rank",
  "artificial_land_fraction_rank"
))
collinearity <- v19_collinearity(features, rank_features)

registry <- v21_landuse_registry()
primary_cases <- if (is.null(primary_result$observed$details[[1L]])) {
  integer()
} else {
  primary_result$observed$details[[1L]]$case_index
}
primary_white <- unique(unlist(lapply(primary_cases, function(case) {
  adjacent <- primary_result$graph$neighbours[[case]]
  adjacent[
    !primary_result$observed_profile$present[adjacent, 1L] &
      analysis_support[adjacent]
  ]
})))
landuse_prevalence <- do.call(rbind, lapply(
  registry$feature, function(feature) {
    data.frame(
      feature = feature,
      all_cell_nonzero_fraction =
        mean(features[[feature]] > 0, na.rm = TRUE),
      all_cell_mean = mean(features[[feature]], na.rm = TRUE),
      primary_focal_nonzero_fraction =
        mean(features[[feature]][primary_cases] > 0, na.rm = TRUE),
      primary_focal_mean =
        mean(features[[feature]][primary_cases], na.rm = TRUE),
      primary_white_nonzero_fraction =
        mean(features[[feature]][primary_white] > 0, na.rm = TRUE),
      primary_white_mean =
        mean(features[[feature]][primary_white], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
))

quality_features <- c(
  "observation_effort_rank", "independent_site_support_rank",
  "broad50km_pc1", "broad50km_pc2", "within50km_pc1", "within50km_pc2"
)
quality_data <- cbind(
  features[, c(
    "observation_effort_rank", "independent_site_support_rank"
  ), drop = FALSE],
  cells[, c(
    "broad50km_pc1", "broad50km_pc2",
    "within50km_pc1", "within50km_pc2"
  ), drop = FALSE]
)
quality_observed <- v21_local_contrasts(
  primary_result$observed_profile$present,
  primary_result$observed_profile$candidate,
  primary_result$graph, quality_data, quality_features,
  stats::complete.cases(quality_data)
)
quality_simulated <- v21_local_contrasts(
  primary_result$simulated_profile$present,
  primary_result$simulated_profile$candidate,
  primary_result$graph, quality_data, quality_features,
  stats::complete.cases(quality_data)
)
quality_definitions <- data.frame(
  feature = quality_features,
  role = c(
    "sampling_effort", "sampling_support",
    rep("environment_balance", 4L)
  ),
  hypothesis_direction = rep("two_sided", length(quality_features)),
  stringsAsFactors = FALSE
)
quality_null <- data.frame(
  configuration = "primary_10km_env1_all_white",
  draw = seq_len(ncol(counts)),
  requested_cases = quality_simulated$n_requested,
  usable_cases = quality_simulated$n_usable,
  quality_simulated$contrast,
  check.names = FALSE, stringsAsFactors = FALSE
)
quality_summary <- v21_contrast_summary(
  quality_observed$contrast[1L, ], quality_null,
  quality_definitions, "primary_10km_env1_all_white"
)

population_scale_features <- c(
  "local_population_rank", "population_5km_rank",
  "population_10km_rank",
  "population_25km_rank", "population_50km_rank"
)
population_scale_data <- features[
  , population_scale_features, drop = FALSE
]
population_observed <- v21_local_contrasts(
  primary_result$observed_profile$present,
  primary_result$observed_profile$candidate,
  primary_result$graph, population_scale_data,
  population_scale_features,
  stats::complete.cases(population_scale_data)
)
population_simulated <- v21_local_contrasts(
  primary_result$simulated_profile$present,
  primary_result$simulated_profile$candidate,
  primary_result$graph, population_scale_data,
  population_scale_features,
  stats::complete.cases(population_scale_data)
)
population_definitions <- data.frame(
  feature = population_scale_features,
  role = c(
    "cell_population", "population_5km", "population_10km",
    "population_25km", "population_50km"
  ),
  hypothesis_direction = rep("greater", 5L),
  stringsAsFactors = FALSE
)
population_null <- data.frame(
  configuration = "primary_10km_env1_all_white",
  draw = seq_len(ncol(counts)),
  requested_cases = population_simulated$n_requested,
  usable_cases = population_simulated$n_usable,
  population_simulated$contrast,
  check.names = FALSE, stringsAsFactors = FALSE
)
population_summary <- v21_contrast_summary(
  population_observed$contrast[1L, ], population_null,
  population_definitions, "primary_10km_env1_all_white"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  class_cells,
  file.path(output_dir, "mlit_landuse_class_cells_1km.csv"),
  row.names = FALSE
)
utils::write.csv(
  class_lookup$audit,
  file.path(output_dir, "human_neighbourhood_landuse_join_audit.csv"),
  row.names = FALSE
)
utils::write.csv(
  features,
  file.path(output_dir, "human_neighbourhood_cell_features.csv"),
  row.names = FALSE
)
utils::write.csv(
  definitions,
  file.path(output_dir, "human_neighbourhood_feature_definitions.csv"),
  row.names = FALSE
)
utils::write.csv(
  contrast_summary,
  file.path(output_dir, "human_neighbourhood_contrast_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  contrast_null,
  file.path(output_dir, "human_neighbourhood_contrast_null.csv"),
  row.names = FALSE
)
utils::write.csv(
  global_summary,
  file.path(output_dir, "human_neighbourhood_global_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  configuration_support,
  file.path(output_dir, "human_neighbourhood_configuration_support.csv"),
  row.names = FALSE
)
utils::write.csv(
  observed_details,
  file.path(output_dir, "human_neighbourhood_observed_details.csv"),
  row.names = FALSE
)
utils::write.csv(
  landuse_prevalence,
  file.path(output_dir, "human_neighbourhood_landuse_prevalence.csv"),
  row.names = FALSE
)
utils::write.csv(
  collinearity,
  file.path(output_dir, "human_neighbourhood_collinearity.csv"),
  row.names = FALSE
)
utils::write.csv(
  quality_summary,
  file.path(output_dir, "human_neighbourhood_quality_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  quality_null,
  file.path(output_dir, "human_neighbourhood_quality_null.csv"),
  row.names = FALSE
)
utils::write.csv(
  convergence_summary,
  file.path(output_dir, "human_neighbourhood_convergence_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  convergence_null,
  file.path(output_dir, "human_neighbourhood_convergence_null.csv"),
  row.names = FALSE
)
utils::write.csv(
  followup_candidates,
  file.path(output_dir, "human_neighbourhood_followup_candidates.csv"),
  row.names = FALSE
)
utils::write.csv(
  population_summary,
  file.path(output_dir, "human_neighbourhood_population_scale_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  population_null,
  file.path(output_dir, "human_neighbourhood_population_scale_null.csv"),
  row.names = FALSE
)
utils::write.csv(
  population_context,
  file.path(output_dir, "human_neighbourhood_worldpop_multiscale.csv"),
  row.names = FALSE
)
utils::write.csv(
  data.frame(
    source = normalizePath(worldpop_raster, winslash = "/"),
    md5 = unname(tools::md5sum(worldpop_raster)),
    radii_km = paste(population_radii, collapse = ","),
    method = paste(
      "sum of native WorldPop cells whose centres fall within",
      "site-latitude Euclidean radius"
    ),
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "human_neighbourhood_worldpop_provenance.csv"),
  row.names = FALSE
)

metadata <- data.frame(
  field = c(
    "analysis_spec_version", "generated_at", "n_natural_maps",
    "primary_estimand", "case_selector", "human_feature_role",
    "comparison_unit", "landuse_source", "landuse_class_lock",
    "worldpop_multiscale_source", "worldpop_multiscale_method",
    "residual_used_as_response", "causal_claim_ceiling",
    "final_Rmd_md5"
  ),
  value = c(
    v21_analysis_spec_version, as.character(Sys.time()),
    as.character(ncol(counts)),
    paste(
      "mean focal-minus-white-neighbour human-feature contrast among",
      "primary local pigmented isolates"
    ),
    paste(
      "flower colour, response-blind distance, environment caliper,",
      "and sampling support only"
    ),
    "human variables are held out until after candidate selection",
    paste(
      "each focal pigmented cell versus its own observed white cells",
      "within the same local environmental neighbourhood"
    ),
    "MLIT National Land Numerical Information L03-b 2021 100-m land use",
    paste(v21_landuse_registry()$code, collapse = ","),
    normalizePath(worldpop_raster, winslash = "/"),
    paste(
      "native-raster cell-centre sums at 5, 10, 25, and 50 km;",
      "legacy perfectly rank-correlated v14 radius columns are not used"
    ),
    "false",
    paste(
      "local human-placement association only; not horticultural origin,",
      "planting, escape, or genetic introgression"
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
  metadata,
  file.path(output_dir, "human_neighbourhood_metadata.csv"),
  row.names = FALSE
)

readme <- c(
  "# v21 within-white-neighbourhood human-activity analysis",
  "",
  paste(
    "Each pigmented focal cell is compared directly with its own observed",
    "white cells inside the response-blind distance and environmental graph.",
    "The identical contrast is recomputed in 1000 natural predictive maps."
  ),
  "",
  paste(
    "The MLIT managed-land aggregate is separated into paddy, other",
    "agriculture, built-up, road, railway, other artificial, and golf-course",
    "fractions. Human variables never select candidate cells or neighbours."
  ),
  "",
  paste(
    "This design tests a local human-placement signature. It does not identify",
    "horticultural provenance, garden escape, planting, or introgression."
  ),
  "",
  "`final.Rmd` is intentionally not modified."
)
writeLines(readme, file.path(output_dir, "README.md"), useBytes = TRUE)

cat("v21 local human-neighbourhood analysis written to ",
    normalizePath(output_dir), "\n")
print(contrast_summary[
  contrast_summary$configuration == "primary_10km_env1_all_white", ])
print(global_summary[
  global_summary$configuration == "primary_10km_env1_all_white", ])
print(configuration_support)
