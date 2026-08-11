#!/usr/bin/env Rscript

# Response-blind support calibration + global human-context multiplicity guardrail.
#
# The final eight-axis environmental distance has a different geometry from the
# current four-PC distance. Using the same RMS caliper (=1) therefore changes
# the number of admissible neighbours before flower colour or human variables
# are considered. This script calibrates the eight-axis caliper using ONLY the
# response-blind neighbour-support distribution of the current graph, then
# replays the natural-map and human-context tests. It also places all five
# population scales and the other primary mechanism proxies in one maxT family.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  if (hit[length(hit)] == length(args)) stop("Missing value after ", flag, call. = FALSE)
  args[hit[length(hit)] + 1L]
}
reference_root <- arg_value("--reference-root", "reference-artifact")
audit_root <- arg_value("--audit-root", "first-audit")
output_dir <- arg_value("--output", "results/human_context_support_calibrated_guardrail")
n_draws <- as.integer(arg_value("--draws", "1000"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required", call. = FALSE)
}
source("R/candidate_null_tools.R")
source("R/local_pigmented_isolates.R")
source("R/local_human_context.R")

cells <- utils::read.csv(
  file.path(reference_root, "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
human <- utils::read.csv(
  file.path(reference_root, "results/ecological_v21_local_human_neighbourhood/human_neighbourhood_cell_features.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
did <- utils::read.csv(
  file.path(reference_root, "results/ecological_v22_did_human_context/did_cell_context.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
baseline <- readRDS(file.path(
  reference_root, "results/ecological_v16_predictive_replication/checkpoints",
  paste0("national_environment_spde_presence_draws", n_draws, ".rds")
))
final8_path <- file.path(audit_root, paste0("final8_presence_draws", n_draws, ".rds"))
if (!file.exists(final8_path)) {
  hits <- list.files(audit_root, pattern = paste0("final8_presence_draws", n_draws, "\\.rds$"), recursive = TRUE, full.names = TRUE)
  if (length(hits) != 1L) stop("Could not locate final8 presence checkpoint", call. = FALSE)
  final8_path <- hits[[1L]]
}
final8_model <- readRDS(final8_path)

current4 <- c("broad50km_pc1", "broad50km_pc2", "within50km_pc1", "within50km_pc2")
final8_terms <- c(
  "env_Temperature_PC1", "env_precip_PC1", "env_TemperatureSeasonality",
  "env_PrecipSeasonality", "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS"
)

scale_matrix <- function(data, terms) {
  X <- as.matrix(data[, terms, drop = FALSE]); storage.mode(X) <- "double"
  X <- apply(X, 2, function(x) {
    sx <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(sx) || sx <= 1e-12) rep(0, length(x)) else (x - mean(x, na.rm = TRUE)) / sx
  })
  if (!is.matrix(X)) X <- matrix(X, ncol = length(terms))
  X
}
coordinates <- as.matrix(cells[, c("x_km", "y_km"), drop = FALSE])
geographic <- as.matrix(stats::dist(coordinates))

environment_distance <- function(terms) {
  X <- scale_matrix(cells, terms)
  as.matrix(stats::dist(X)) / sqrt(ncol(X))
}
current_environment <- environment_distance(current4)
final8_environment <- environment_distance(final8_terms)

support_vector <- function(environmental, caliper) {
  vapply(seq_len(nrow(cells)), function(i) {
    sum(
      seq_len(nrow(cells)) != i &
        geographic[i, ] <= 10 &
        environmental[i, ] <= caliper
    )
  }, integer(1))
}
current_support <- support_vector(current_environment, 1)
target_mean <- mean(current_support)
target_supported <- sum(current_support >= 3L)

caliper_grid <- seq(0.50, 1.20, by = 0.01)
calibration <- do.call(rbind, lapply(caliper_grid, function(caliper) {
  support <- support_vector(final8_environment, caliper)
  score <- abs(mean(support) - target_mean) / target_mean +
    abs(sum(support >= 3L) - target_supported) / nrow(cells)
  data.frame(
    caliper = caliper,
    mean_neighbours = mean(support),
    supported_cells = sum(support >= 3L),
    score = score,
    stringsAsFactors = FALSE
  )
}))
best_rows <- which(calibration$score == min(calibration$score))
if (length(best_rows) > 1L) {
  best_rows <- best_rows[which.min(abs(calibration$caliper[best_rows] - 1))]
}
best_caliper <- calibration$caliper[best_rows[1L]]
calibration$selected <- calibration$caliper == best_caliper
utils::write.csv(calibration, file.path(output_dir, "response_blind_caliper_calibration.csv"), row.names = FALSE)

make_graph <- function(environmental, caliper, label) {
  neighbours <- vector("list", nrow(cells)); weights <- vector("list", nrow(cells)); rows <- vector("list", nrow(cells))
  for (i in seq_len(nrow(cells))) {
    adjacent <- which(
      seq_len(nrow(cells)) != i & geographic[i, ] <= 10 & environmental[i, ] <= caliper
    )
    if (length(adjacent)) {
      adjacent <- adjacent[order(geographic[i, adjacent], as.character(cells$exact_site_id[adjacent]))]
      weight <- 1 / pmax(geographic[i, adjacent], 0.5)
      weight <- weight / sum(weight)
    } else weight <- numeric()
    neighbours[[i]] <- adjacent; weights[[i]] <- weight
    rows[[i]] <- data.frame(
      exact_site_id = as.character(cells$exact_site_id[i]),
      n_neighbours = length(adjacent),
      n_neighbour_independent_sites = sum(as.numeric(cells$n_independent_sites[adjacent]), na.rm = TRUE),
      mean_neighbour_distance_km = if (length(adjacent)) mean(geographic[i, adjacent]) else NA_real_,
      maximum_neighbour_distance_km = if (length(adjacent)) max(geographic[i, adjacent]) else NA_real_,
      mean_environmental_distance = if (length(adjacent)) mean(environmental[i, adjacent]) else NA_real_,
      maximum_environmental_distance = if (length(adjacent)) max(environmental[i, adjacent]) else NA_real_,
      supported = length(adjacent) >= 3L, stringsAsFactors = FALSE
    )
  }
  list(
    neighbours = neighbours, weights = weights,
    geographic_distance = geographic, environmental_distance = environmental,
    support = do.call(rbind, rows),
    settings = data.frame(
      representation = label, radius_km = 10,
      environment_caliper = caliper, minimum_neighbours = 3L,
      normalization = "RMS Euclidean distance after response-blind standardization",
      stringsAsFactors = FALSE
    )
  )
}
current_graph <- make_graph(current_environment, 1, "current_4pc_caliper1")
raw_final8_graph <- make_graph(final8_environment, 1, "final8_caliper1")
matched_final8_graph <- make_graph(final8_environment, best_caliper, "final8_support_calibrated")
utils::write.csv(
  rbind(current_graph$settings, raw_final8_graph$settings, matched_final8_graph$settings),
  file.path(output_dir, "graph_registry.csv"), row.names = FALSE
)

# Primary mechanism family. Population is explicitly a five-scale subfamily;
# no one scale is selected after seeing the response. DID, road, built-up and
# forest-human edge are separate exposure/context mechanisms. Forest cover and
# mountainness are two-sided natural alternatives.
hidx <- match(cells$exact_site_id, human$exact_site_id)
didx <- match(cells$exact_site_id, did$exact_site_id)
if (anyNA(hidx) || anyNA(didx)) stop("Human/DID features do not align to cells", call. = FALSE)
feature_table <- data.frame(
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
    "population_focal", "population_5km", "population_10km", "population_25km", "population_50km",
    "dense_settlement", "transport_access", "built_context", "managed_natural_interface",
    "natural_forest_alternative", "natural_mountain_alternative"
  ),
  hypothesis_direction = c(rep("greater", 9L), "two_sided", "two_sided"),
  stringsAsFactors = FALSE
)
utils::write.csv(definitions, file.path(output_dir, "global_mechanism_family_registry.csv"), row.names = FALSE)

effort_table <- data.frame(
  exact_site_id = as.character(cells$exact_site_id),
  observation_effort_rank = as.numeric(human$observation_effort_rank[hidx]),
  independent_site_support_rank = as.numeric(human$independent_site_support_rank[hidx]),
  stringsAsFactors = FALSE
)
effort_def <- data.frame(
  feature = c("observation_effort_rank", "independent_site_support_rank"),
  role = c("observation_effort_alternative", "independent_site_support_alternative"),
  hypothesis_direction = c("two_sided", "two_sided"), stringsAsFactors = FALSE
)

run_scenario <- function(model_result, graph, scenario) {
  aligned <- v18_align_result(model_result, cells, scenario)
  observed_q <- v18_predictive_tail_q(aligned$observed, aligned$draws, "upper")
  simulated_q <- v18_simulation_tail_q(aligned$draws, "upper")
  observed_profile <- v20_local_profile(matrix(aligned$observed, ncol = 1L), graph)
  simulated_profile <- v20_local_profile(aligned$draws, graph)
  event_summary <- v20_metric_rows(
    observed_profile, simulated_profile, observed_q, simulated_q, scenario
  )$summary
  event_summary$scenario <- scenario

  observed_human <- v21_local_contrasts(
    observed_profile$present, observed_profile$candidate,
    graph, feature_table, definitions$feature
  )
  simulated_human <- v21_local_contrasts(
    simulated_profile$present, simulated_profile$candidate,
    graph, feature_table, definitions$feature
  )
  observed_vector <- as.numeric(observed_human$contrast[1L, ])
  names(observed_vector) <- colnames(observed_human$contrast)
  human_summary <- v21_contrast_summary(
    observed_vector, as.data.frame(simulated_human$contrast), definitions, scenario
  )
  human_summary$observed_n_candidates <- sum(observed_profile$candidate[, 1L])
  human_summary$mean_null_n_candidates <- mean(simulated_human$n_requested)

  observed_effort <- v21_local_contrasts(
    observed_profile$present, observed_profile$candidate,
    graph, effort_table, effort_def$feature
  )
  simulated_effort <- v21_local_contrasts(
    simulated_profile$present, simulated_profile$candidate,
    graph, effort_table, effort_def$feature
  )
  effort_vector <- as.numeric(observed_effort$contrast[1L, ])
  names(effort_vector) <- colnames(observed_effort$contrast)
  effort_summary <- v21_contrast_summary(
    effort_vector, as.data.frame(simulated_effort$contrast), effort_def, scenario
  )

  candidate_ids <- as.character(cells$exact_site_id[observed_profile$candidate[, 1L]])
  list(
    event = event_summary, human = human_summary, effort = effort_summary,
    candidates = data.frame(scenario = scenario, exact_site_id = candidate_ids, stringsAsFactors = FALSE)
  )
}

scenarios <- list(
  current_model_current_graph = list(model = baseline, graph = current_graph),
  final8_model_current_graph = list(model = final8_model, graph = current_graph),
  current_model_final8_raw_graph = list(model = baseline, graph = raw_final8_graph),
  final8_model_final8_raw_graph = list(model = final8_model, graph = raw_final8_graph),
  current_model_final8_support_matched = list(model = baseline, graph = matched_final8_graph),
  final8_model_final8_support_matched = list(model = final8_model, graph = matched_final8_graph)
)
results <- lapply(names(scenarios), function(name) {
  message("[support-calibrated human audit] ", name)
  run_scenario(scenarios[[name]]$model, scenarios[[name]]$graph, name)
})
names(results) <- names(scenarios)

utils::write.csv(
  do.call(rbind, lapply(results, `[[`, "event")),
  file.path(output_dir, "candidate_null_support_calibrated.csv"), row.names = FALSE
)
global_summary <- do.call(rbind, lapply(results, `[[`, "human"))
utils::write.csv(
  global_summary,
  file.path(output_dir, "global_human_mechanism_maxT.csv"), row.names = FALSE
)
utils::write.csv(
  do.call(rbind, lapply(results, `[[`, "effort")),
  file.path(output_dir, "observation_bias_alternative.csv"), row.names = FALSE
)
utils::write.csv(
  do.call(rbind, lapply(results, `[[`, "candidates")),
  file.path(output_dir, "candidate_membership_support_calibrated.csv"), row.names = FALSE
)

scenario_decision <- do.call(rbind, lapply(split(global_summary, global_summary$configuration), function(block) {
  population <- block[block$feature %in% definitions$feature[1:5], , drop = FALSE]
  human_mechanisms <- block[block$feature %in% definitions$feature[1:9], , drop = FALSE]
  best_pop <- population[which.min(population$maxT_FWER_p), , drop = FALSE]
  best_global <- block[which.min(block$maxT_FWER_p), , drop = FALSE]
  data.frame(
    scenario = block$configuration[1L],
    candidate_count = unique(block$observed_n_candidates)[1L],
    best_population_scale = best_pop$feature,
    best_population_global_FWER = best_pop$maxT_FWER_p,
    best_global_feature = best_global$feature,
    best_global_FWER = best_global$maxT_FWER_p,
    any_anthropogenic_feature_global_FWER_below_0_05 = any(human_mechanisms$maxT_FWER_p < 0.05),
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(
  scenario_decision,
  file.path(output_dir, "global_human_decision_table.csv"), row.names = FALSE
)

jsonlite::write_json(
  list(
    status = "PASS",
    selected_final8_caliper = best_caliper,
    calibration_rule = paste(
      "Selected without colour or human data by minimizing relative difference in mean neighbour count",
      "plus absolute supported-cell-count difference divided by n cells, versus current 4-PC caliper=1 graph."
    ),
    current_mean_neighbours = target_mean,
    current_supported_cells = target_supported,
    matched_final8_mean_neighbours = calibration$mean_neighbours[best_rows[1L]],
    matched_final8_supported_cells = calibration$supported_cells[best_rows[1L]],
    global_family = as.character(definitions$feature),
    claim_ceiling = paste(
      "Human-context associations remain post-selection context/exposure signals and cannot establish",
      "planting, garden escape, cultivar ancestry or introgression."
    )
  ),
  file.path(output_dir, "validation.json"), pretty = TRUE, auto_unbox = TRUE
)
cat("Support-calibrated human-context guardrail complete; selected caliper=", best_caliper, "\n", sep = "")
