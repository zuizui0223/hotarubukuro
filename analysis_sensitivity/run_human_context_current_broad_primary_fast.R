#!/usr/bin/env Rscript

# Fast, exact replay of the current-Broad-primary human-context analysis.
# Reuses the checksum-locked 10,000-draw final-eight-axis cross-fitted presence
# model from the prior adjudication artifact and rebuilds only the NEW primary
# environmental graph: final-eight-axis standardized RMS distance <= 1.
#
# This avoids refitting an unchanged INLA model. Human variables never enter
# candidate selection or graph construction.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  if (hit[length(hit)] == length(args)) stop("Missing value after ", flag, call. = FALSE)
  args[hit[length(hit)] + 1L]
}
reference_root <- arg_value("--reference-root", "reference-artifact")
final8_root <- arg_value("--final8-root", "final8-artifact")
output_dir <- arg_value("--output", "results/human_context_current_broad_primary")
n_draws <- as.integer(arg_value("--draws", "10000"))
seed <- as.integer(arg_value("--seed", "20260725"))
if (n_draws != 10000L) stop("Locked fast replay expects exactly 10000 maps", call. = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_packages <- c("jsonlite")
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "), call. = FALSE)

source("R/candidate_null_tools.R")
source("R/local_pigmented_isolates.R")
source("R/local_human_context.R")

cells_path <- file.path(reference_root, "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv")
human_path <- file.path(reference_root, "results/ecological_v21_local_human_neighbourhood/human_neighbourhood_cell_features.csv")
did_path <- file.path(reference_root, "results/ecological_v22_did_human_context/did_cell_context.csv")
final8_checkpoint <- file.path(final8_root, "final8_presence_draws10000.rds")
for (p in c(cells_path, human_path, did_path, final8_checkpoint)) {
  if (!file.exists(p) || file.info(p)$size <= 0) stop("Missing input: ", p, call. = FALSE)
}

cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
human <- utils::read.csv(human_path, check.names = FALSE, stringsAsFactors = FALSE)
did <- utils::read.csv(did_path, check.names = FALSE, stringsAsFactors = FALSE)
final8_model <- readRDS(final8_checkpoint)

final8 <- c(
  "env_Temperature_PC1", "env_precip_PC1", "env_TemperatureSeasonality",
  "env_PrecipSeasonality", "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS"
)
v20_require_columns(cells, unique(c(
  "exact_site_id", "x_km", "y_km", "spatial_fold", "n_independent_sites",
  "n_observations", "n_pigmented", final8
)), "cells")

scale_matrix <- function(data, terms) {
  X <- as.matrix(data[, terms, drop = FALSE])
  storage.mode(X) <- "double"
  X <- apply(X, 2, function(x) {
    sx <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(sx) || sx <= 1e-12) rep(0, length(x)) else (x - mean(x, na.rm = TRUE)) / sx
  })
  if (!is.matrix(X)) X <- matrix(X, ncol = length(terms))
  X
}

coordinates <- as.matrix(cells[, c("x_km", "y_km"), drop = FALSE])
geographic <- as.matrix(stats::dist(coordinates))
X <- scale_matrix(cells, final8)
final_env <- as.matrix(stats::dist(X)) / sqrt(ncol(X))

make_graph <- function(env, caliper = 1) {
  neighbours <- vector("list", nrow(cells))
  weights <- vector("list", nrow(cells))
  rows <- vector("list", nrow(cells))
  for (i in seq_len(nrow(cells))) {
    adjacent <- which(seq_len(nrow(cells)) != i & geographic[i, ] <= 10 & env[i, ] <= caliper)
    if (length(adjacent)) {
      adjacent <- adjacent[order(geographic[i, adjacent], as.character(cells$exact_site_id[adjacent]))]
      w <- 1 / pmax(geographic[i, adjacent], 0.5)
      w <- w / sum(w)
    } else {
      w <- numeric()
    }
    neighbours[[i]] <- adjacent
    weights[[i]] <- w
    rows[[i]] <- data.frame(
      exact_site_id = as.character(cells$exact_site_id[i]),
      n_neighbours = length(adjacent),
      n_neighbour_independent_sites = sum(as.numeric(cells$n_independent_sites[adjacent]), na.rm = TRUE),
      mean_neighbour_distance_km = if (length(adjacent)) mean(geographic[i, adjacent]) else NA_real_,
      maximum_neighbour_distance_km = if (length(adjacent)) max(geographic[i, adjacent]) else NA_real_,
      mean_environmental_distance = if (length(adjacent)) mean(env[i, adjacent]) else NA_real_,
      maximum_environmental_distance = if (length(adjacent)) max(env[i, adjacent]) else NA_real_,
      supported = length(adjacent) >= 3L,
      stringsAsFactors = FALSE
    )
  }
  list(
    neighbours = neighbours,
    weights = weights,
    geographic_distance = geographic,
    environmental_distance = env,
    support = do.call(rbind, rows),
    settings = data.frame(
      representation = "current_broad_final8_rms_caliper1_primary",
      radius_km = 10,
      environment_caliper = caliper,
      minimum_neighbours = 3L,
      normalization = "RMS Euclidean distance after response-blind standardization",
      stringsAsFactors = FALSE
    )
  )
}
primary_graph <- make_graph(final_env, 1)

hidx <- match(cells$exact_site_id, human$exact_site_id)
didx <- match(cells$exact_site_id, did$exact_site_id)
if (anyNA(hidx) || anyNA(didx)) stop("Human features do not align to cells", call. = FALSE)
features <- data.frame(
  exact_site_id = as.character(cells$exact_site_id),
  local_population_rank = as.numeric(human$local_population_rank[hidx]),
  population_5km_rank = as.numeric(human$population_5km_rank[hidx]),
  population_10km_rank = as.numeric(human$population_10km_rank[hidx]),
  population_25km_rank = as.numeric(human$population_25km_rank[hidx]),
  population_50km_rank = as.numeric(human$population_50km_rank[hidx]),
  did_proximity_rank = as.numeric(did$did_proximity_rank[didx]),
  road_proximity_rank = as.numeric(human$road_proximity_rank[hidx]),
  built_up_fraction_rank = as.numeric(human$built_up_fraction_rank[hidx]),
  forest_human_edge_rank = as.numeric(human$forest_human_edge_rank[hidx]),
  forest_cover_rank = as.numeric(human$forest_cover_rank[hidx]),
  mountainness_rank = as.numeric(human$mountainness_rank[hidx]),
  stringsAsFactors = FALSE
)
definitions <- data.frame(
  feature = c(
    "local_population_rank", "population_5km_rank", "population_10km_rank",
    "population_25km_rank", "population_50km_rank", "did_proximity_rank",
    "road_proximity_rank", "built_up_fraction_rank", "forest_human_edge_rank",
    "forest_cover_rank", "mountainness_rank"
  ),
  role = c(
    "population_focal", "population_5km", "population_10km", "population_25km",
    "population_50km", "dense_settlement", "transport_access", "built_context",
    "managed_natural_interface", "natural_forest_alternative", "natural_mountain_alternative"
  ),
  hypothesis_direction = c(rep("greater", 9), "two_sided", "two_sided"),
  stringsAsFactors = FALSE
)
effort <- data.frame(
  exact_site_id = as.character(cells$exact_site_id),
  observation_effort_rank = as.numeric(human$observation_effort_rank[hidx]),
  independent_site_support_rank = as.numeric(human$independent_site_support_rank[hidx]),
  stringsAsFactors = FALSE
)
effort_def <- data.frame(
  feature = c("observation_effort_rank", "independent_site_support_rank"),
  role = c("observation_effort_alternative", "independent_site_support_alternative"),
  hypothesis_direction = c("two_sided", "two_sided"),
  stringsAsFactors = FALSE
)

scenario <- "current_broad_primary_final8_model_final8_graph_caliper1"
aligned <- v18_align_result(final8_model, cells, scenario)
if (ncol(aligned$draws) != n_draws) {
  stop("Locked final8 checkpoint does not contain ", n_draws, " draws", call. = FALSE)
}
observed_q <- v18_predictive_tail_q(aligned$observed, aligned$draws, "upper")
simulated_q <- v18_simulation_tail_q(aligned$draws, "upper")
obs_profile <- v20_local_profile(matrix(aligned$observed, ncol = 1L), primary_graph)
sim_profile <- v20_local_profile(aligned$draws, primary_graph)
event <- v20_metric_rows(obs_profile, sim_profile, observed_q, simulated_q, scenario)$summary
event$scenario <- scenario

obs_h <- v21_local_contrasts(obs_profile$present, obs_profile$candidate, primary_graph, features, definitions$feature)
sim_h <- v21_local_contrasts(sim_profile$present, sim_profile$candidate, primary_graph, features, definitions$feature)
ov <- as.numeric(obs_h$contrast[1L, ])
names(ov) <- colnames(obs_h$contrast)
human_summary <- v21_contrast_summary(ov, as.data.frame(sim_h$contrast), definitions, scenario)
human_summary$observed_n_candidates <- sum(obs_profile$candidate[, 1L])

obs_e <- v21_local_contrasts(obs_profile$present, obs_profile$candidate, primary_graph, effort, effort_def$feature)
sim_e <- v21_local_contrasts(sim_profile$present, sim_profile$candidate, primary_graph, effort, effort_def$feature)
ev <- as.numeric(obs_e$contrast[1L, ])
names(ev) <- colnames(obs_e$contrast)
effort_summary <- v21_contrast_summary(ev, as.data.frame(sim_e$contrast), effort_def, scenario)

candidate_ids <- as.character(cells$exact_site_id[obs_profile$candidate[, 1L]])
candidates <- data.frame(scenario = scenario, exact_site_id = candidate_ids, stringsAsFactors = FALSE)

human_test_rows <- human_summary$feature %in% definitions$feature[1:9]
population_rows <- human_summary$feature %in% definitions$feature[1:5]
best_human <- human_summary[human_test_rows, , drop = FALSE]
best_human <- best_human[which.min(best_human$maxT_FWER_p), , drop = FALSE]
best_population <- human_summary[population_rows, , drop = FALSE]
best_population <- best_population[which.min(best_population$maxT_FWER_p), , drop = FALSE]

primary_decision <- data.frame(
  scenario = scenario,
  candidate_count = length(candidate_ids),
  supported_cells = sum(primary_graph$support$supported),
  mean_neighbours = mean(primary_graph$support$n_neighbours),
  best_population_scale = best_population$feature[1],
  best_population_contrast = best_population$observed_focal_minus_white_neighbour[1],
  best_population_directional_p = best_population$directional_or_two_sided_p[1],
  best_population_global_FWER = best_population$maxT_FWER_p[1],
  best_global_human_feature = best_human$feature[1],
  best_global_human_FWER = best_human$maxT_FWER_p[1],
  any_human_global_FWER_below_0_05 = any(human_summary$maxT_FWER_p[human_test_rows] < 0.05, na.rm = TRUE),
  stringsAsFactors = FALSE
)

utils::write.csv(primary_graph$settings, file.path(output_dir, "current_broad_primary_graph_registry.csv"), row.names = FALSE)
utils::write.csv(primary_graph$support, file.path(output_dir, "current_broad_primary_graph_support.csv"), row.names = FALSE)
utils::write.csv(event, file.path(output_dir, "current_broad_primary_candidate_null.csv"), row.names = FALSE)
utils::write.csv(human_summary, file.path(output_dir, "current_broad_primary_human_maxT.csv"), row.names = FALSE)
utils::write.csv(effort_summary, file.path(output_dir, "current_broad_primary_observation_bias.csv"), row.names = FALSE)
utils::write.csv(candidates, file.path(output_dir, "current_broad_primary_candidate_membership.csv"), row.names = FALSE)
utils::write.csv(primary_decision, file.path(output_dir, "current_broad_primary_decision.csv"), row.names = FALSE)
utils::write.csv(definitions, file.path(output_dir, "current_broad_primary_feature_registry.csv"), row.names = FALSE)

jsonlite::write_json(
  list(
    status = "PASS",
    n_maps = n_draws,
    seed = seed,
    final8_checkpoint = basename(final8_checkpoint),
    primary_definition = list(
      natural_reference = "locked final-eight-axis cross-fitted pigmentation-state model",
      environmental_matching = "eight current Broad abiotic axes, standardized RMS distance",
      radius_km = 10,
      rms_caliper = 1,
      minimum_neighbours = 3,
      human_variables_used_in_selection = FALSE,
      note = "East/West is a structural observation-level adjustment, not an environmental matching dimension."
    ),
    candidate_count = length(candidate_ids),
    supported_cells = sum(primary_graph$support$supported),
    mean_neighbours = mean(primary_graph$support$n_neighbours),
    best_population_scale = as.character(best_population$feature[1]),
    best_population_global_FWER = as.numeric(best_population$maxT_FWER_p[1]),
    best_global_human_feature = as.character(best_human$feature[1]),
    best_global_human_FWER = as.numeric(best_human$maxT_FWER_p[1])
  ),
  file.path(output_dir, "current_broad_primary_validation.json"),
  pretty = TRUE,
  auto_unbox = TRUE
)

message("[current broad primary fast] PASS; candidates=", length(candidate_ids),
        "; best human FWER=", signif(best_human$maxT_FWER_p[1], 5))
