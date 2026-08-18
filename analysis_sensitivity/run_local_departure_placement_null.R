#!/usr/bin/env Rscript

# Count-conditioned placement-null reanalysis of the 16 current-Broad local
# pigmented departures. Candidate detection is replayed unchanged on all 10,000
# natural posterior-predictive maps. Human variables are joined only after each
# map's candidate set has been fixed.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  if (hit[length(hit)] == length(args)) {
    stop("Missing value after ", flag, call. = FALSE)
  }
  args[hit[length(hit)] + 1L]
}

reference_root <- arg_value("--reference-root", "reference-artifact")
final8_root <- arg_value("--final8-root", "final8-artifact")
worldpop_raster <- arg_value("--worldpop-raster", "")
worldpop_provenance <- arg_value("--worldpop-provenance", "")
output_dir <- arg_value(
  "--output", "results/local_departure_placement_null"
)
n_draws <- as.integer(arg_value("--draws", "10000"))
seed <- as.integer(arg_value("--seed", "20260725"))
minimum_exact_draws <- as.integer(
  arg_value("--minimum-exact-draws", "200")
)
fallback_draws <- as.integer(arg_value("--fallback-draws", "1000"))
if (n_draws != 10000L) {
  stop("The preregistered placement-null replay requires 10,000 maps.",
       call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_packages <- c("jsonlite", "terra")
missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

source("R/candidate_null_tools.R")
source("R/local_pigmented_isolates.R")
source("R/local_human_context.R")
source("R/local_departure_placement_null.R")
source("R/spatial_context.R")

cells_path <- file.path(
  reference_root,
  "results/ecological_v15_multiscale_hotspots/",
  "multiscale_hotspot_cells_1km.csv"
)
human_path <- file.path(
  reference_root,
  "results/ecological_v21_local_human_neighbourhood/",
  "human_neighbourhood_cell_features.csv"
)
did_path <- file.path(
  reference_root,
  "results/ecological_v22_did_human_context/did_cell_context.csv"
)
final8_checkpoint <- file.path(final8_root, "final8_presence_draws10000.rds")
for (path in c(cells_path, human_path, did_path, final8_checkpoint)) {
  if (!file.exists(path) || file.info(path)$size <= 0) {
    stop("Missing input: ", path, call. = FALSE)
  }
}

cells <- utils::read.csv(
  cells_path, check.names = FALSE, stringsAsFactors = FALSE
)
human <- utils::read.csv(
  human_path, check.names = FALSE, stringsAsFactors = FALSE
)
did <- utils::read.csv(
  did_path, check.names = FALSE, stringsAsFactors = FALSE
)
final8_model <- readRDS(final8_checkpoint)

final8_terms <- c(
  "env_Temperature_PC1", "env_precip_PC1",
  "env_TemperatureSeasonality", "env_PrecipSeasonality",
  "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS"
)
v22_require_columns(cells, unique(c(
  "exact_site_id", "longitude", "latitude", "x_km", "y_km",
  "spatial_fold", "n_independent_sites", "n_observations",
  "n_pigmented", final8_terms
)), "cells")

scale_matrix <- function(data, terms) {
  matrix <- as.matrix(data[, terms, drop = FALSE])
  storage.mode(matrix) <- "double"
  matrix <- apply(matrix, 2, function(value) {
    spread <- stats::sd(value, na.rm = TRUE)
    if (!is.finite(spread) || spread <= 1e-12) {
      rep(0, length(value))
    } else {
      (value - mean(value, na.rm = TRUE)) / spread
    }
  })
  if (!is.matrix(matrix)) matrix <- matrix(matrix, ncol = length(terms))
  matrix
}

coordinates <- as.matrix(cells[, c("x_km", "y_km"), drop = FALSE])
geographic <- as.matrix(stats::dist(coordinates))
final8_matrix <- scale_matrix(cells, final8_terms)
final8_distance <- as.matrix(stats::dist(final8_matrix)) /
  sqrt(ncol(final8_matrix))

make_primary_graph <- function(environment_distance, caliper = 1) {
  neighbours <- vector("list", nrow(cells))
  weights <- vector("list", nrow(cells))
  rows <- vector("list", nrow(cells))
  for (index in seq_len(nrow(cells))) {
    adjacent <- which(
      seq_len(nrow(cells)) != index &
        geographic[index, ] <= 10 &
        environment_distance[index, ] <= caliper
    )
    if (length(adjacent)) {
      adjacent <- adjacent[order(
        geographic[index, adjacent],
        as.character(cells$exact_site_id[adjacent])
      )]
      weight <- 1 / pmax(geographic[index, adjacent], 0.5)
      weight <- weight / sum(weight)
    } else {
      weight <- numeric()
    }
    neighbours[[index]] <- adjacent
    weights[[index]] <- weight
    rows[[index]] <- data.frame(
      exact_site_id = as.character(cells$exact_site_id[index]),
      n_neighbours = length(adjacent),
      n_neighbour_independent_sites = sum(
        as.numeric(cells$n_independent_sites[adjacent]), na.rm = TRUE
      ),
      mean_neighbour_distance_km = if (length(adjacent)) {
        mean(geographic[index, adjacent])
      } else NA_real_,
      maximum_neighbour_distance_km = if (length(adjacent)) {
        max(geographic[index, adjacent])
      } else NA_real_,
      mean_environmental_distance = if (length(adjacent)) {
        mean(environment_distance[index, adjacent])
      } else NA_real_,
      maximum_environmental_distance = if (length(adjacent)) {
        max(environment_distance[index, adjacent])
      } else NA_real_,
      supported = length(adjacent) >= 3L,
      stringsAsFactors = FALSE
    )
  }
  list(
    neighbours = neighbours,
    weights = weights,
    geographic_distance = geographic,
    environmental_distance = environment_distance,
    support = do.call(rbind, rows),
    settings = data.frame(
      representation = "current_broad_final8_rms_caliper1_primary",
      radius_km = 10,
      environment_caliper = caliper,
      minimum_neighbours = 3L,
      normalization = paste(
        "RMS Euclidean distance after response-blind standardization"
      ),
      human_variables_used_in_selection = FALSE,
      stringsAsFactors = FALSE
    )
  )
}
primary_graph <- make_primary_graph(final8_distance, 1)

human_index <- match(cells$exact_site_id, human$exact_site_id)
did_index <- match(cells$exact_site_id, did$exact_site_id)
if (anyNA(human_index) || anyNA(did_index) ||
    anyDuplicated(human_index) || anyDuplicated(did_index)) {
  stop("Human feature tables do not align one-to-one with cells.",
       call. = FALSE)
}

population_2km_rank <- NULL
population_2km_sum <- NULL
if ("population_2km_rank" %in% names(human)) {
  population_2km_rank <- as.numeric(
    human$population_2km_rank[human_index]
  )
  if ("population_sum_2km" %in% names(human)) {
    population_2km_sum <- as.numeric(
      human$population_sum_2km[human_index]
    )
  }
} else {
  if (!nzchar(worldpop_raster) || !file.exists(worldpop_raster)) {
    stop(
      "The preregistered 2-km population exposure requires --worldpop-raster.",
      call. = FALSE
    )
  }
  message("[v22 placement null] deriving the preregistered 2-km WorldPop sum")
  worldpop <- terra::rast(worldpop_raster)
  context <- multiscale_point_context(
    worldpop, cells, radii_km = 2, summary_function = "sum"
  )
  population_2km_sum <- as.numeric(context[, 1L])
  population_2km_rank <- v18_rank01(
    log1p(pmax(population_2km_sum, 0))
  )
}

features <- data.frame(
  exact_site_id = as.character(cells$exact_site_id),
  population_2km_rank = population_2km_rank,
  population_5km_rank = as.numeric(
    human$population_5km_rank[human_index]
  ),
  population_10km_rank = as.numeric(
    human$population_10km_rank[human_index]
  ),
  population_25km_rank = as.numeric(
    human$population_25km_rank[human_index]
  ),
  population_50km_rank = as.numeric(
    human$population_50km_rank[human_index]
  ),
  did_proximity_rank = as.numeric(did$did_proximity_rank[did_index]),
  road_proximity_rank = as.numeric(
    human$road_proximity_rank[human_index]
  ),
  built_up_fraction_rank = as.numeric(
    human$built_up_fraction_rank[human_index]
  ),
  forest_human_edge_rank = as.numeric(
    human$forest_human_edge_rank[human_index]
  ),
  forest_cover_rank = as.numeric(
    human$forest_cover_rank[human_index]
  ),
  mountainness_rank = as.numeric(
    human$mountainness_rank[human_index]
  ),
  observation_effort_rank = as.numeric(
    human$observation_effort_rank[human_index]
  ),
  independent_site_support_rank = as.numeric(
    human$independent_site_support_rank[human_index]
  ),
  stringsAsFactors = FALSE
)
if (!is.null(population_2km_sum)) {
  features$population_sum_2km <- population_2km_sum
}

human_features <- c(
  "population_2km_rank", "population_5km_rank",
  "population_10km_rank", "population_25km_rank",
  "population_50km_rank", "did_proximity_rank",
  "road_proximity_rank", "built_up_fraction_rank",
  "forest_human_edge_rank"
)
population_features <- c(
  "population_2km_rank", "population_5km_rank",
  "population_10km_rank", "population_25km_rank",
  "population_50km_rank"
)
natural_features <- c("forest_cover_rank", "mountainness_rank")
observation_features <- c(
  "observation_effort_rank", "independent_site_support_rank"
)
all_features <- c(human_features, natural_features, observation_features)
analysis_support <- stats::complete.cases(
  features[, all_features, drop = FALSE]
)
if (!all(analysis_support)) {
  stop(
    "Placement-null feature registry is incomplete for ",
    sum(!analysis_support), " cells; no post-selection support filter is allowed.",
    call. = FALSE
  )
}

aligned <- v18_align_result(final8_model, cells, "final8 presence")
if (ncol(aligned$draws) != n_draws) {
  stop("Locked final8 checkpoint does not contain ", n_draws, " draws.",
       call. = FALSE)
}
if (!isTRUE(all.equal(
    as.numeric(aligned$observed), as.numeric(cells$n_pigmented),
    tolerance = 0
))) {
  stop("Final8 observed counts do not equal the frozen cell table.",
       call. = FALSE)
}

observed_q <- v18_predictive_tail_q(
  aligned$observed, aligned$draws, "upper"
)
simulated_q <- v18_simulation_tail_q(aligned$draws, "upper")
observed_profile <- v20_local_profile(
  matrix(aligned$observed, ncol = 1L), primary_graph
)
simulated_profile <- v20_local_profile(aligned$draws, primary_graph)

# Existing questions are retained as separate outputs.
event_result <- v20_metric_rows(
  observed_profile, simulated_profile,
  observed_q, simulated_q,
  "current_broad_final8_graph_caliper1"
)

old_definitions <- data.frame(
  feature = c(
    "population_5km_rank", "population_10km_rank",
    "population_25km_rank", "population_50km_rank",
    "did_proximity_rank", "road_proximity_rank",
    "built_up_fraction_rank", "forest_human_edge_rank",
    "forest_cover_rank", "mountainness_rank"
  ),
  role = c(
    "population_5km", "population_10km", "population_25km",
    "population_50km", "dense_settlement", "transport_access",
    "built_context", "managed_natural_interface",
    "natural_forest_alternative", "natural_mountain_alternative"
  ),
  hypothesis_direction = c(rep("greater", 8L), "two_sided", "two_sided"),
  stringsAsFactors = FALSE
)
old_observed <- v21_local_contrasts(
  observed_profile$present, observed_profile$candidate,
  primary_graph, features, old_definitions$feature
)
old_simulated <- v21_local_contrasts(
  simulated_profile$present, simulated_profile$candidate,
  primary_graph, features, old_definitions$feature
)
old_value <- as.numeric(old_observed$contrast[1L, ])
names(old_value) <- colnames(old_observed$contrast)
old_method_summary <- v21_contrast_summary(
  old_value, as.data.frame(old_simulated$contrast),
  old_definitions,
  "current_candidate_minus_own_white_neighbour"
)

# New question: where are departures placed relative to natural-map placement?
observed_mean <- v22_candidate_feature_statistic(
  observed_profile$candidate, features, all_features, "mean",
  analysis_support
)
simulated_mean <- v22_candidate_feature_statistic(
  simulated_profile$candidate, features, all_features, "mean",
  analysis_support
)
observed_median <- v22_candidate_feature_statistic(
  observed_profile$candidate, features, all_features, "median",
  analysis_support
)
simulated_median <- v22_candidate_feature_statistic(
  simulated_profile$candidate, features, all_features, "median",
  analysis_support
)

count_condition <- v22_count_conditioned_draws(
  simulated_mean$n_candidates,
  observed_mean$n_candidates[1L],
  minimum_exact_draws = minimum_exact_draws,
  fallback_draws = fallback_draws
)
if (!length(count_condition$selected_draws)) {
  stop("No natural maps survived count conditioning.", call. = FALSE)
}

make_definitions <- function(feature, family, role, direction) {
  data.frame(
    feature = feature,
    role = role,
    hypothesis_direction = direction,
    family = family,
    stringsAsFactors = FALSE
  )
}
global_definitions <- rbind(
  make_definitions(
    human_features, rep("global_human", length(human_features)),
    c(
      "population_2km_primary", "population_5km", "population_10km",
      "population_25km", "population_50km", "dense_settlement",
      "transport_access", "built_context", "managed_natural_interface"
    ),
    rep("greater", length(human_features))
  ),
  make_definitions(
    natural_features, rep("natural_alternative", length(natural_features)),
    c("natural_forest_alternative", "natural_mountain_alternative"),
    rep("two_sided", length(natural_features))
  ),
  make_definitions(
    observation_features,
    rep("observation_process", length(observation_features)),
    c("observation_effort_alternative", "independent_site_support_alternative"),
    rep("two_sided", length(observation_features))
  )
)
population_definitions <- make_definitions(
  population_features,
  rep("population_scale", length(population_features)),
  c(
    "population_2km_primary", "population_5km", "population_10km",
    "population_25km", "population_50km"
  ),
  rep("greater", length(population_features))
)

observed_mean_vector <- as.numeric(observed_mean$statistic[1L, ])
names(observed_mean_vector) <- colnames(observed_mean$statistic)
observed_median_vector <- as.numeric(observed_median$statistic[1L, ])
names(observed_median_vector) <- colnames(observed_median$statistic)

exact_global <- v22_placement_summary(
  observed_mean_vector, as.data.frame(simulated_mean$statistic),
  global_definitions, count_condition$selected_draws,
  count_condition$mode, "mean"
)
exact_global$analysis_family <- "global_registry"
exact_population <- v22_placement_summary(
  observed_mean_vector, as.data.frame(simulated_mean$statistic),
  population_definitions, count_condition$selected_draws,
  count_condition$mode, "mean"
)
exact_population$analysis_family <- "population_scale_family"
exact_mean_summary <- rbind(exact_global, exact_population)

nonempty_draws <- which(simulated_mean$n_candidates > 0L)
all_map_summary <- v22_placement_summary(
  observed_mean_vector, as.data.frame(simulated_mean$statistic),
  global_definitions, nonempty_draws,
  "all_nonempty_natural_maps", "mean"
)
all_map_summary$analysis_family <- "global_registry"

median_summary <- v22_placement_summary(
  observed_median_vector, as.data.frame(simulated_median$statistic),
  global_definitions, count_condition$selected_draws,
  count_condition$mode, "median"
)
median_summary$analysis_family <- "global_registry"

count_diagnostic <- v22_count_exposure_diagnostic(
  simulated_mean$n_candidates,
  simulated_mean$statistic,
  all_features
)
count_distribution <- as.data.frame(table(
  candidate_count = simulated_mean$n_candidates
), stringsAsFactors = FALSE)
count_distribution$candidate_count <- as.integer(
  as.character(count_distribution$candidate_count)
)
count_distribution$proportion <- count_distribution$Freq / n_draws
count_distribution$observed_count <- observed_mean$n_candidates[1L]
count_distribution$selected_for_primary <-
  count_distribution$candidate_count %in%
  unique(simulated_mean$n_candidates[count_condition$selected_draws])

observed_candidate_index <- which(observed_profile$candidate[, 1L])
candidate_membership <- cbind(
  data.frame(
    exact_site_id = as.character(cells$exact_site_id[observed_candidate_index]),
    longitude = as.numeric(cells$longitude[observed_candidate_index]),
    latitude = as.numeric(cells$latitude[observed_candidate_index]),
    unexpected_pigmented_q = observed_q[observed_candidate_index],
    n_observations = as.integer(cells$n_observations[observed_candidate_index]),
    n_pigmented = as.integer(cells$n_pigmented[observed_candidate_index]),
    stringsAsFactors = FALSE
  ),
  features[observed_candidate_index, all_features, drop = FALSE]
)

null_draws <- data.frame(
  draw = seq_len(n_draws),
  candidate_count = simulated_mean$n_candidates,
  exact_count_primary = seq_len(n_draws) %in% count_condition$selected_draws,
  simulated_mean$statistic,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

primary_2km <- exact_global[
  exact_global$feature == "population_2km_rank", , drop = FALSE
]
population_2km_family <- exact_population[
  exact_population$feature == "population_2km_rank", , drop = FALSE
]
best_human <- exact_global[
  exact_global$family == "global_human", , drop = FALSE
]
best_human <- best_human[
  which.min(best_human$maxT_FWER_p), , drop = FALSE
]
old_5km <- old_method_summary[
  old_method_summary$feature == "population_5km_rank", , drop = FALSE
]
count_row <- event_result$summary[
  event_result$summary$metric == "candidate_count", , drop = FALSE
]
fraction_row <- event_result$summary[
  event_result$summary$metric == "candidate_fraction", , drop = FALSE
]

primary_decision <- data.frame(
  specification = v22_analysis_spec_version,
  n_natural_maps = n_draws,
  observed_candidate_count = observed_mean$n_candidates[1L],
  null_mean_candidate_count = mean(simulated_mean$n_candidates),
  candidate_count_p = count_row$empirical_p[1L],
  candidate_fraction_p = fraction_row$empirical_p[1L],
  count_conditioning_mode = count_condition$mode,
  exact_count_null_maps = count_condition$n_exact_draws,
  primary_null_maps = count_condition$n_selected_draws,
  primary_feature = "population_2km_rank",
  primary_observed_mean_rank = primary_2km$observed_candidate_exposure[1L],
  primary_null_mean_rank = primary_2km$null_mean[1L],
  primary_directional_p = primary_2km$p_value[1L],
  primary_population_scale_maxT_FWER =
    population_2km_family$maxT_FWER_p[1L],
  primary_global_human_maxT_FWER = primary_2km$maxT_FWER_p[1L],
  global_human_omnibus_p = primary_2km$family_omnibus_p[1L],
  best_global_human_feature = best_human$feature[1L],
  best_global_human_observed_mean_rank =
    best_human$observed_candidate_exposure[1L],
  best_global_human_maxT_FWER = best_human$maxT_FWER_p[1L],
  current_method_5km_contrast =
    old_5km$observed_focal_minus_white_neighbour[1L],
  current_method_5km_directional_p =
    old_5km$directional_or_two_sided_p[1L],
  current_method_5km_global_FWER = old_5km$maxT_FWER_p[1L],
  human_variables_used_in_candidate_detection = FALSE,
  provenance_claim_supported = FALSE,
  stringsAsFactors = FALSE
)

worldpop_sha256 <- NA_character_
if (nzchar(worldpop_raster) && file.exists(worldpop_raster)) {
  sha_line <- tryCatch(
    system2("sha256sum", worldpop_raster, stdout = TRUE, stderr = TRUE),
    error = function(error) character()
  )
  if (length(sha_line)) {
    worldpop_sha256 <- strsplit(sha_line[1L], "[[:space:]]+")[[1L]][1L]
  }
}
provenance_record <- NULL
if (nzchar(worldpop_provenance) && file.exists(worldpop_provenance)) {
  provenance_record <- jsonlite::read_json(
    worldpop_provenance, simplifyVector = TRUE
  )
}

validation <- list(
  status = "PASS",
  specification = v22_analysis_spec_version,
  preregistered_question = paste(
    "Conditional on the unchanged natural event detector producing the",
    "observed number of local departures, are observed departures placed",
    "toward greater human exposure than natural posterior-predictive maps?"
  ),
  n_maps = n_draws,
  seed = seed,
  candidate_detector = list(
    natural_reference =
      "locked final-eight-axis cross-fitted pigmentation-state model",
    local_graph =
      "10-km current-Broad final-eight-axis standardized RMS caliper 1",
    minimum_neighbours = 3,
    white_neighbour_requirement = "all neighbours white",
    human_variables_used = FALSE
  ),
  conditioning = count_condition,
  observed_candidate_count = observed_mean$n_candidates[1L],
  primary = as.list(primary_decision[1L, ]),
  two_km_population = list(
    source_path = if (nzchar(worldpop_raster)) basename(worldpop_raster) else NA,
    sha256 = worldpop_sha256,
    historical_provenance = provenance_record
  ),
  inference_ceiling = paste(
    "Placement beyond the natural map can prioritize provenance fieldwork,",
    "but cannot identify planted, escaped, introgressed, plastic or",
    "human-caused flowers without field history and genetic evidence."
  )
)

utils::write.csv(
  primary_graph$settings,
  file.path(output_dir, "placement_null_graph_registry.csv"),
  row.names = FALSE
)
utils::write.csv(
  primary_graph$support,
  file.path(output_dir, "placement_null_graph_support.csv"),
  row.names = FALSE
)
utils::write.csv(
  event_result$summary,
  file.path(output_dir, "placement_null_candidate_frequency.csv"),
  row.names = FALSE
)
utils::write.csv(
  old_method_summary,
  file.path(output_dir, "current_white_neighbour_comparator.csv"),
  row.names = FALSE
)
utils::write.csv(
  exact_mean_summary,
  file.path(output_dir, "placement_null_exact_count_mean.csv"),
  row.names = FALSE
)
utils::write.csv(
  all_map_summary,
  file.path(output_dir, "placement_null_all_maps_mean_sensitivity.csv"),
  row.names = FALSE
)
utils::write.csv(
  median_summary,
  file.path(output_dir, "placement_null_exact_count_median_sensitivity.csv"),
  row.names = FALSE
)
utils::write.csv(
  count_diagnostic,
  file.path(output_dir, "placement_null_count_exposure_diagnostic.csv"),
  row.names = FALSE
)
utils::write.csv(
  count_distribution,
  file.path(output_dir, "placement_null_candidate_count_distribution.csv"),
  row.names = FALSE
)
utils::write.csv(
  candidate_membership,
  file.path(output_dir, "placement_null_observed_candidate_membership.csv"),
  row.names = FALSE
)
utils::write.csv(
  null_draws,
  file.path(output_dir, "placement_null_map_statistics.csv"),
  row.names = FALSE
)
utils::write.csv(
  primary_decision,
  file.path(output_dir, "placement_null_primary_decision.csv"),
  row.names = FALSE
)
utils::write.csv(
  global_definitions,
  file.path(output_dir, "placement_null_feature_registry.csv"),
  row.names = FALSE
)
jsonlite::write_json(
  validation,
  file.path(output_dir, "placement_null_validation.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)

result_lines <- c(
  "# Local-departure placement-null result",
  "",
  paste0("- Specification: `", v22_analysis_spec_version, "`"),
  paste0("- Natural posterior-predictive maps: ", n_draws),
  paste0("- Observed local departures: ", observed_mean$n_candidates[1L]),
  paste0(
    "- Candidate-count question: P = ",
    formatC(count_row$empirical_p[1L], digits = 5, format = "f"),
    "; candidate-fraction P = ",
    formatC(fraction_row$empirical_p[1L], digits = 5, format = "f")
  ),
  paste0(
    "- Placement conditioning: ", count_condition$mode,
    " (", count_condition$n_selected_draws, " maps; ",
    count_condition$n_exact_draws, " exact-count maps available)"
  ),
  paste0(
    "- Primary 2-km population mean rank: observed = ",
    formatC(primary_2km$observed_candidate_exposure[1L], digits = 5, format = "f"),
    ", null mean = ",
    formatC(primary_2km$null_mean[1L], digits = 5, format = "f"),
    ", directional P = ",
    formatC(primary_2km$p_value[1L], digits = 5, format = "f"),
    ", population-scale maxT FWER P = ",
    formatC(population_2km_family$maxT_FWER_p[1L], digits = 5, format = "f"),
    ", global-human maxT FWER P = ",
    formatC(primary_2km$maxT_FWER_p[1L], digits = 5, format = "f")
  ),
  paste0(
    "- Best global human feature: `", best_human$feature[1L],
    "`; maxT FWER P = ",
    formatC(best_human$maxT_FWER_p[1L], digits = 5, format = "f")
  ),
  paste0(
    "- Reproduced current 5-km candidate-minus-white contrast: ",
    formatC(old_5km$observed_focal_minus_white_neighbour[1L], digits = 5, format = "f"),
    "; directional P = ",
    formatC(old_5km$directional_or_two_sided_p[1L], digits = 5, format = "f"),
    "; global maxT FWER P = ",
    formatC(old_5km$maxT_FWER_p[1L], digits = 5, format = "f")
  ),
  "",
  "## Interpretation ceiling",
  "",
  paste(
    "This test separates candidate placement from candidate frequency and from",
    "candidate-versus-neighbour contrast. A supported placement departure would",
    "show that the observed events occupy more human-exposed cells than the same",
    "event detector occupies in natural maps. It would not establish human origin,",
    "horticultural escape, introgression or causal human modification."
  )
)
writeLines(result_lines, file.path(output_dir, "RESULT_SUMMARY.md"))

message(
  "[v22 placement null] PASS; candidates=",
  observed_mean$n_candidates[1L],
  "; exact-count maps=", count_condition$n_exact_draws,
  "; primary 2-km P=", signif(primary_2km$p_value[1L], 5),
  "; primary global FWER=", signif(primary_2km$maxT_FWER_p[1L], 5)
)
