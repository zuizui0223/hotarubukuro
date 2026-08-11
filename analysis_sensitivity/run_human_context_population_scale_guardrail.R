#!/usr/bin/env Rscript

# Population-scale multiplicity guardrail for the final Broad-to-human audit.
# The 5-km population contrast must not be promoted after seeing its P value.
# Re-evaluate the pre-existing focal/5/10/25/50-km scale family under every
# environmental model/graph propagation scenario and retain maxT FWER.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  if (hit[length(hit)] == length(args)) stop("Missing value after ", flag, call. = FALSE)
  args[hit[length(hit)] + 1L]
}
reference_root <- arg_value("--reference-root", "reference-artifact")
audit_root <- arg_value("--audit-root", "first-audit")
output_dir <- arg_value("--output", "results/human_context_population_scale_guardrail")
n_draws <- as.integer(arg_value("--draws", "1000"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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
baseline <- readRDS(file.path(
  reference_root, "results/ecological_v16_predictive_replication/checkpoints",
  paste0("national_environment_spde_presence_draws", n_draws, ".rds")
))
final8_path <- file.path(audit_root, paste0("final8_presence_draws", n_draws, ".rds"))
if (!file.exists(final8_path)) {
  hits <- list.files(audit_root, pattern = paste0("final8_presence_draws", n_draws, "\\.rds$"), recursive = TRUE, full.names = TRUE)
  if (length(hits) != 1L) stop("Could not locate final8 presence checkpoint in ", audit_root, call. = FALSE)
  final8_path <- hits[[1L]]
}
final8 <- readRDS(final8_path)

current4 <- c("broad50km_pc1", "broad50km_pc2", "within50km_pc1", "within50km_pc2")
final8_terms <- c(
  "env_Temperature_PC1", "env_precip_PC1", "env_TemperatureSeasonality",
  "env_PrecipSeasonality", "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS"
)

scale_columns <- function(data, terms) {
  X <- as.matrix(data[, terms, drop = FALSE]); storage.mode(X) <- "double"
  X <- apply(X, 2, function(x) {
    sx <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(sx) || sx <= 1e-12) rep(0, length(x)) else (x - mean(x, na.rm = TRUE)) / sx
  })
  if (!is.matrix(X)) X <- matrix(X, ncol = length(terms))
  X
}

make_graph <- function(terms, label) {
  coordinates <- as.matrix(cells[, c("x_km", "y_km")])
  geographic <- as.matrix(stats::dist(coordinates))
  environment <- scale_columns(cells, terms)
  environmental <- as.matrix(stats::dist(environment)) / sqrt(ncol(environment))
  neighbours <- vector("list", nrow(cells)); weights <- vector("list", nrow(cells)); rows <- vector("list", nrow(cells))
  for (i in seq_len(nrow(cells))) {
    adjacent <- which(seq_len(nrow(cells)) != i & geographic[i, ] <= 10 & environmental[i, ] <= 1)
    if (length(adjacent)) {
      adjacent <- adjacent[order(geographic[i, adjacent], as.character(cells$exact_site_id[adjacent]))]
      weight <- 1 / pmax(geographic[i, adjacent], 0.5); weight <- weight / sum(weight)
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
    settings = data.frame(representation = label, stringsAsFactors = FALSE)
  )
}
current_graph <- make_graph(current4, "current_4pc")
final_graph <- make_graph(final8_terms, "final_broad_8axis")

idx <- match(cells$exact_site_id, human$exact_site_id)
if (anyNA(idx)) stop("Human feature table does not align to cells", call. = FALSE)
population_features <- data.frame(
  exact_site_id = as.character(cells$exact_site_id),
  local_population_rank = as.numeric(human$local_population_rank[idx]),
  population_5km_rank = as.numeric(human$population_5km_rank[idx]),
  population_10km_rank = as.numeric(human$population_10km_rank[idx]),
  population_25km_rank = as.numeric(human$population_25km_rank[idx]),
  population_50km_rank = as.numeric(human$population_50km_rank[idx]),
  stringsAsFactors = FALSE
)
definitions <- data.frame(
  feature = c(
    "local_population_rank", "population_5km_rank", "population_10km_rank",
    "population_25km_rank", "population_50km_rank"
  ),
  role = c("cell_population", "population_5km", "population_10km", "population_25km", "population_50km"),
  hypothesis_direction = rep("greater", 5L), stringsAsFactors = FALSE
)

run_one <- function(result, graph, scenario) {
  aligned <- v18_align_result(result, cells, scenario)
  observed_profile <- v20_local_profile(matrix(aligned$observed, ncol = 1L), graph)
  simulated_profile <- v20_local_profile(aligned$draws, graph)
  observed <- v21_local_contrasts(
    observed_profile$present, observed_profile$candidate,
    graph, population_features, definitions$feature
  )
  simulated <- v21_local_contrasts(
    simulated_profile$present, simulated_profile$candidate,
    graph, population_features, definitions$feature
  )
  observed_vector <- as.numeric(observed$contrast[1L, ]); names(observed_vector) <- colnames(observed$contrast)
  summary <- v21_contrast_summary(
    observed_vector, as.data.frame(simulated$contrast), definitions, scenario
  )
  summary$observed_n_candidates <- sum(observed_profile$candidate[, 1L])
  summary$mean_null_n_candidates <- mean(simulated$n_requested)
  summary
}

scenarios <- list(
  current_model_current_graph = list(result = baseline, graph = current_graph),
  current_model_final8_graph = list(result = baseline, graph = final_graph),
  final8_model_current_graph = list(result = final8, graph = current_graph),
  final8_model_final8_graph = list(result = final8, graph = final_graph)
)
rows <- lapply(names(scenarios), function(name) {
  run_one(scenarios[[name]]$result, scenarios[[name]]$graph, name)
})
summary <- do.call(rbind, rows)
utils::write.csv(summary, file.path(output_dir, "population_scale_propagation_summary.csv"), row.names = FALSE)
utils::write.csv(definitions, file.path(output_dir, "population_scale_registry.csv"), row.names = FALSE)

# Explicitly record the minimum scale-family corrected P in each scenario so a
# post-hoc single-scale result cannot be reported without its multiplicity guardrail.
minimum <- do.call(rbind, lapply(split(summary, summary$configuration), function(block) {
  best <- block[which.min(block$maxT_FWER_p), , drop = FALSE]
  data.frame(
    scenario = block$configuration[1L],
    best_scale = best$feature,
    best_directional_p = best$directional_or_two_sided_p,
    best_scale_family_maxT_FWER_p = best$maxT_FWER_p,
    any_scale_FWER_below_0_05 = any(block$maxT_FWER_p < 0.05),
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(minimum, file.path(output_dir, "population_scale_family_decision.csv"), row.names = FALSE)

jsonlite::write_json(
  list(
    status = "PASS",
    rule = "No population scale is promoted from a single-scale P value; maxT FWER is evaluated jointly over focal/5/10/25/50 km.",
    scenarios = as.character(minimum$scenario),
    minimum_FWER = min(minimum$best_scale_family_maxT_FWER_p)
  ),
  file.path(output_dir, "validation.json"), pretty = TRUE, auto_unbox = TRUE
)
cat("Population-scale guardrail complete\n")
