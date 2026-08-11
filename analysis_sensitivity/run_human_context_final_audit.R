#!/usr/bin/env Rscript

# Final downstream audit for the JBI Broad -> local-departure -> human-context chain.
#
# Questions:
# 1. Would a stricter VIF<5 rule change the final observation-level Broad model?
# 2. Does replacing the legacy four-PC cell-level environmental representation
#    by the final eight Broad environmental axes materially change the 17 local
#    departure identities or their natural-map calibration?
# 3. Do the post-selection human-context results survive that environmental
#    propagation, after reducing strongly redundant human composites to a small
#    mechanism-based feature family?
#
# This is a sensitivity audit. It does not silently overwrite the current
# manuscript reference or relabel candidates as anthropogenic.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  if (hit[length(hit)] == length(args)) stop("Missing value after ", flag, call. = FALSE)
  args[hit[length(hit)] + 1L]
}

reference_root <- arg_value("--reference-root", "reference-artifact")
output_dir <- arg_value("--output", "results/human_context_final_audit")
n_draws <- as.integer(arg_value("--draws", "1000"))
seed <- as.integer(arg_value("--seed", "20260811"))
if (!is.finite(n_draws) || n_draws < 500L) stop("--draws must be at least 500", call. = FALSE)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_packages <- c("INLA", "Matrix", "jsonlite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

source("R/natural_predictive_model.R")
source("R/candidate_null_tools.R")
source("R/local_pigmented_isolates.R")
source("R/local_human_context.R")

Sys.setenv(
  OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1",
  INLA_NUM_THREADS = "1", OMP_DYNAMIC = "FALSE"
)
set.seed(seed)

path_in_reference <- function(...) file.path(reference_root, ...)
obs_path <- path_in_reference(
  "results", "ecological_v11_pigmentation_hurdle",
  "analysis_data_pigmentation_hurdle.csv"
)
cells_path <- path_in_reference(
  "results", "ecological_v15_multiscale_hotspots",
  "multiscale_hotspot_cells_1km.csv"
)
baseline_checkpoint <- path_in_reference(
  "results", "ecological_v16_predictive_replication", "checkpoints",
  paste0("national_environment_spde_presence_draws", n_draws, ".rds")
)
human_path <- path_in_reference(
  "results", "ecological_v21_local_human_neighbourhood",
  "human_neighbourhood_cell_features.csv"
)
did_path <- path_in_reference(
  "results", "ecological_v22_did_human_context", "did_cell_context.csv"
)
for (path in c(obs_path, cells_path, baseline_checkpoint, human_path, did_path)) {
  if (!file.exists(path) || file.info(path)$size <= 0) stop("Missing input: ", path, call. = FALSE)
}

observations <- utils::read.csv(obs_path, check.names = FALSE, stringsAsFactors = FALSE)
cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
human <- utils::read.csv(human_path, check.names = FALSE, stringsAsFactors = FALSE)
did <- utils::read.csv(did_path, check.names = FALSE, stringsAsFactors = FALSE)
baseline <- readRDS(baseline_checkpoint)

current4 <- c(
  "broad50km_pc1", "broad50km_pc2",
  "within50km_pc1", "within50km_pc2"
)
final8 <- c(
  "env_Temperature_PC1", "env_precip_PC1",
  "env_TemperatureSeasonality", "env_PrecipSeasonality",
  "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS"
)
needed_cells <- unique(c(
  "exact_site_id", "x_km", "y_km", "spatial_fold", "n_independent_sites",
  "n_observations", "n_pigmented", current4, final8
))
v20_require_columns(cells, needed_cells, "cells")

safe_vif <- function(data, terms) {
  X <- as.data.frame(data[, terms, drop = FALSE])
  rows <- lapply(terms, function(term) {
    others <- setdiff(terms, term)
    if (!length(others)) return(data.frame(term = term, VIF = 1))
    cc <- stats::complete.cases(X[, c(term, others), drop = FALSE])
    fit <- stats::lm(stats::reformulate(others, response = term), data = X[cc, , drop = FALSE])
    data.frame(term = term, VIF = 1 / pmax(1 - summary(fit)$r.squared, 1e-12))
  })
  do.call(rbind, rows)
}

# VIF policy sensitivity on the exact observation-level final models.
observations$regionEast <- as.integer(as.character(observations$region) == "East")
observations$thermal_interaction <-
  as.numeric(observations$env_Temperature_PC1) *
  as.numeric(observations$env_TemperatureSeasonality)
state_terms <- c("regionEast", final8)
intensity_terms <- c(state_terms, "thermal_interaction")
state_vif <- safe_vif(observations, state_terms)
state_vif$response <- "pigmentation_state"
intensity_data <- observations[is.finite(observations$pigment_intensity_z), , drop = FALSE]
intensity_vif <- safe_vif(intensity_data, intensity_terms)
intensity_vif$response <- "conditional_intensity"
intensity_no_region_vif <- safe_vif(intensity_data, setdiff(intensity_terms, "regionEast"))
intensity_no_region_vif$response <- "conditional_intensity_no_region_guardrail"
vif <- rbind(
  state_vif[, c("response", "term", "VIF")],
  intensity_vif[, c("response", "term", "VIF")],
  intensity_no_region_vif[, c("response", "term", "VIF")]
)
utils::write.csv(vif, file.path(output_dir, "vif_threshold_audit.csv"), row.names = FALSE)

scale_columns <- function(data, terms) {
  out <- as.matrix(data[, terms, drop = FALSE])
  storage.mode(out) <- "double"
  out <- apply(out, 2, function(x) {
    sx <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(sx) || sx <= 1e-12) return(rep(0, length(x)))
    (x - mean(x, na.rm = TRUE)) / sx
  })
  if (!is.matrix(out)) out <- matrix(out, ncol = length(terms))
  colnames(out) <- terms
  out
}

make_graph <- function(cells, terms, label,
                       radius_km = 10, environment_caliper = 1,
                       minimum_neighbours = 3L) {
  coordinates <- as.matrix(cells[, c("x_km", "y_km"), drop = FALSE])
  geographic <- as.matrix(stats::dist(coordinates))
  environment <- scale_columns(cells, terms)
  environmental <- as.matrix(stats::dist(environment)) / sqrt(ncol(environment))
  neighbours <- vector("list", nrow(cells))
  weights <- vector("list", nrow(cells))
  support_rows <- vector("list", nrow(cells))
  for (index in seq_len(nrow(cells))) {
    adjacent <- which(
      seq_len(nrow(cells)) != index &
        geographic[index, ] <= radius_km &
        environmental[index, ] <= environment_caliper
    )
    if (length(adjacent)) {
      adjacent <- adjacent[order(
        geographic[index, adjacent], as.character(cells$exact_site_id[adjacent])
      )]
      weight <- 1 / pmax(geographic[index, adjacent], 0.5)
      weight <- weight / sum(weight)
    } else {
      weight <- numeric()
    }
    neighbours[[index]] <- adjacent
    weights[[index]] <- weight
    support_rows[[index]] <- data.frame(
      exact_site_id = as.character(cells$exact_site_id[index]),
      n_neighbours = length(adjacent),
      n_neighbour_independent_sites = sum(
        as.numeric(cells$n_independent_sites[adjacent]), na.rm = TRUE
      ),
      mean_neighbour_distance_km = if (length(adjacent)) mean(geographic[index, adjacent]) else NA_real_,
      maximum_neighbour_distance_km = if (length(adjacent)) max(geographic[index, adjacent]) else NA_real_,
      mean_environmental_distance = if (length(adjacent)) mean(environmental[index, adjacent]) else NA_real_,
      maximum_environmental_distance = if (length(adjacent)) max(environmental[index, adjacent]) else NA_real_,
      supported = length(adjacent) >= minimum_neighbours,
      stringsAsFactors = FALSE
    )
  }
  list(
    neighbours = neighbours, weights = weights,
    geographic_distance = geographic, environmental_distance = environmental,
    support = do.call(rbind, support_rows),
    settings = data.frame(
      representation = label,
      terms = paste(terms, collapse = ";"),
      radius_km = radius_km, environment_caliper = environment_caliper,
      minimum_neighbours = minimum_neighbours,
      normalization = "RMS Euclidean distance after column standardization",
      stringsAsFactors = FALSE
    )
  )
}

current_graph <- make_graph(cells, current4, "current_4pc")
final_graph <- make_graph(cells, final8, "final_broad_8axis")
utils::write.csv(
  rbind(current_graph$settings, final_graph$settings),
  file.path(output_dir, "environment_representation_registry.csv"), row.names = FALSE
)

# Confirm that the detector itself exactly recovers the manuscript's 17 cells.
observed_counts <- matrix(as.numeric(cells$n_pigmented), ncol = 1L)
current_observed_profile <- v20_local_profile(observed_counts, current_graph)
final_observed_profile <- v20_local_profile(observed_counts, final_graph)
current_ids <- as.character(cells$exact_site_id[current_observed_profile$candidate[, 1L]])
final_ids <- as.character(cells$exact_site_id[final_observed_profile$candidate[, 1L]])
overlap_ids <- intersect(current_ids, final_ids)
identity_summary <- data.frame(
  current_candidate_count = length(current_ids),
  final8_candidate_count = length(final_ids),
  overlap_count = length(overlap_ids),
  current_retained_fraction = length(overlap_ids) / max(length(current_ids), 1),
  Jaccard = length(overlap_ids) / max(length(union(current_ids, final_ids)), 1),
  stringsAsFactors = FALSE
)
utils::write.csv(identity_summary, file.path(output_dir, "candidate_identity_propagation.csv"), row.names = FALSE)
utils::write.csv(
  data.frame(
    exact_site_id = sort(union(current_ids, final_ids)),
    current_4pc_candidate = sort(union(current_ids, final_ids)) %in% current_ids,
    final_8axis_candidate = sort(union(current_ids, final_ids)) %in% final_ids,
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "candidate_identity_membership.csv"), row.names = FALSE
)

# Fit the final eight-axis cell-level natural reference using exactly the same
# five spatial folds, response, SPDE mesh/prior code and predictive machinery.
final8_result <- v16_crossfit_spde(
  cells, response = "n_pigmented", family = "binomial",
  environment_terms = final8, trials = "n_observations",
  model = "final8_environment_spde_presence_sensitivity",
  n_draws = n_draws, seed = seed
)
saveRDS(final8_result, file.path(output_dir, paste0("final8_presence_draws", n_draws, ".rds")))

baseline_perf <- v16_model_performance(
  baseline, cells, "n_pigmented", "binomial", "n_observations"
)
final8_perf <- v16_model_performance(
  final8_result, cells, "n_pigmented", "binomial", "n_observations"
)
predictive_performance <- rbind(baseline_perf, final8_perf)
utils::write.csv(
  predictive_performance,
  file.path(output_dir, "natural_reference_predictive_performance.csv"), row.names = FALSE
)
utils::write.csv(
  rbind(baseline$log, final8_result$log),
  file.path(output_dir, "natural_reference_fold_log.csv"), row.names = FALSE
)

# Merge the human feature table with DID context and keep one mechanism-based
# feature per construct. Composite scores remain diagnostics only.
feature_index <- match(cells$exact_site_id, human$exact_site_id)
did_index <- match(cells$exact_site_id, did$exact_site_id)
if (anyNA(feature_index) || anyNA(did_index)) {
  stop("Human/DID features do not align one-to-one with current cells", call. = FALSE)
}
reduced_features <- data.frame(
  exact_site_id = as.character(cells$exact_site_id),
  population_5km_rank = as.numeric(human$population_5km_rank[feature_index]),
  did_proximity_rank = as.numeric(did$did_proximity_rank[did_index]),
  road_proximity_rank = as.numeric(human$road_proximity_rank[feature_index]),
  built_up_fraction_rank = as.numeric(human$built_up_fraction_rank[feature_index]),
  forest_human_edge_rank = as.numeric(human$forest_human_edge_rank[feature_index]),
  forest_cover_rank = as.numeric(human$forest_cover_rank[feature_index]),
  mountainness_rank = as.numeric(human$mountainness_rank[feature_index]),
  stringsAsFactors = FALSE
)
reduced_definitions <- data.frame(
  feature = c(
    "population_5km_rank", "did_proximity_rank", "road_proximity_rank",
    "built_up_fraction_rank", "forest_human_edge_rank",
    "forest_cover_rank", "mountainness_rank"
  ),
  role = c(
    "anthropogenic_exposure", "dense_settlement_exposure", "transport_access",
    "built_disturbance_context", "managed_natural_interface",
    "natural_alternative", "natural_alternative"
  ),
  hypothesis_direction = c(
    rep("greater", 5L), "two_sided", "two_sided"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  reduced_definitions,
  file.path(output_dir, "reduced_human_hypothesis_registry.csv"), row.names = FALSE
)

# Quantify redundancy in the current full feature surface rather than treating
# correlated composites as independent ecological mechanisms.
numeric_human <- human[vapply(human, is.numeric, logical(1))]
correlation <- suppressWarnings(stats::cor(numeric_human, method = "spearman", use = "pairwise.complete.obs"))
upper <- which(upper.tri(correlation), arr.ind = TRUE)
collinearity <- data.frame(
  feature_1 = colnames(correlation)[upper[, 1]],
  feature_2 = colnames(correlation)[upper[, 2]],
  spearman_rho = correlation[upper],
  stringsAsFactors = FALSE
)
collinearity <- collinearity[order(-abs(collinearity$spearman_rho)), , drop = FALSE]
utils::write.csv(
  collinearity,
  file.path(output_dir, "human_feature_collinearity.csv"), row.names = FALSE
)
utils::write.csv(
  collinearity[abs(collinearity$spearman_rho) >= 0.85, , drop = FALSE],
  file.path(output_dir, "human_feature_high_redundancy.csv"), row.names = FALSE
)

scenario_rows <- list()
human_rows <- list()
scenario_candidates <- list()

run_scenario <- function(result, graph, scenario) {
  aligned <- v18_align_result(result, cells, scenario)
  if (any(abs(aligned$observed - as.numeric(cells$n_pigmented)) > 1e-8)) {
    stop("Natural-reference observed counts do not align in ", scenario, call. = FALSE)
  }
  observed_q <- v18_predictive_tail_q(aligned$observed, aligned$draws, "upper")
  simulated_q <- v18_simulation_tail_q(aligned$draws, "upper")
  observed_profile <- v20_local_profile(matrix(aligned$observed, ncol = 1L), graph)
  simulated_profile <- v20_local_profile(aligned$draws, graph)
  metric <- v20_metric_rows(
    observed_profile, simulated_profile, observed_q, simulated_q, scenario
  )$summary
  metric$scenario <- scenario

  ids <- as.character(cells$exact_site_id[observed_profile$candidate[, 1L]])
  candidate_table <- data.frame(
    scenario = scenario, exact_site_id = ids, stringsAsFactors = FALSE
  )

  observed_human <- v21_local_contrasts(
    observed_profile$present, observed_profile$candidate,
    graph, reduced_features, reduced_definitions$feature
  )
  simulated_human <- v21_local_contrasts(
    simulated_profile$present, simulated_profile$candidate,
    graph, reduced_features, reduced_definitions$feature
  )
  observed_vector <- as.numeric(observed_human$contrast[1L, ])
  names(observed_vector) <- colnames(observed_human$contrast)
  human_summary <- v21_contrast_summary(
    observed_vector,
    as.data.frame(simulated_human$contrast),
    reduced_definitions,
    scenario
  )
  human_summary$observed_n_candidates <- length(ids)
  human_summary$mean_null_n_candidates <- mean(simulated_human$n_requested)
  human_summary$mean_null_usable_candidates <- mean(simulated_human$n_usable)

  list(metric = metric, human = human_summary, candidates = candidate_table)
}

scenarios <- list(
  current_model_current_graph = list(result = baseline, graph = current_graph),
  current_model_final8_graph = list(result = baseline, graph = final_graph),
  final8_model_current_graph = list(result = final8_result, graph = current_graph),
  final8_model_final8_graph = list(result = final8_result, graph = final_graph)
)
for (scenario in names(scenarios)) {
  message("[human final audit] ", scenario)
  result <- run_scenario(
    scenarios[[scenario]]$result, scenarios[[scenario]]$graph, scenario
  )
  scenario_rows[[scenario]] <- result$metric
  human_rows[[scenario]] <- result$human
  scenario_candidates[[scenario]] <- result$candidates
}

utils::write.csv(
  do.call(rbind, scenario_rows),
  file.path(output_dir, "candidate_null_propagation_summary.csv"), row.names = FALSE
)
utils::write.csv(
  do.call(rbind, human_rows),
  file.path(output_dir, "reduced_human_context_propagation_summary.csv"), row.names = FALSE
)
utils::write.csv(
  do.call(rbind, scenario_candidates),
  file.path(output_dir, "scenario_candidate_membership.csv"), row.names = FALSE
)

# One compact machine-readable decision record. PASS means the audit completed;
# it is not a declaration of anthropogenic origin.
max_state_vif <- max(state_vif$VIF, na.rm = TRUE)
max_intensity_vif <- max(intensity_vif$VIF, na.rm = TRUE)
focal_int_vif <- intensity_vif$VIF[intensity_vif$term == "thermal_interaction"]
max_intensity_no_region_vif <- max(intensity_no_region_vif$VIF, na.rm = TRUE)
validation <- list(
  status = "PASS",
  n_observations = nrow(observations), n_cells = nrow(cells),
  baseline_candidate_count = length(current_ids),
  final8_matching_candidate_count = length(final_ids),
  candidate_overlap = length(overlap_ids),
  candidate_Jaccard = identity_summary$Jaccard,
  max_state_VIF = max_state_vif,
  max_intensity_VIF = max_intensity_vif,
  focal_thermal_interaction_VIF = focal_int_vif,
  max_intensity_no_region_VIF = max_intensity_no_region_vif,
  vif_policy_interpretation = paste(
    "VIF<5 preferred; 5-10 requires focal-term and coefficient/spatial stability review;",
    ">10 treated as non-identifiable for promotion unless explicitly justified"
  ),
  human_claim_ceiling = paste(
    "Human variables are post-selection exposure/context proxies and sampling-bias alternatives;",
    "they do not establish planting, escape, introgression or horticultural provenance."
  )
)
jsonlite::write_json(
  validation, file.path(output_dir, "validation.json"),
  pretty = TRUE, auto_unbox = TRUE
)

cat("Human-context final audit complete: ", output_dir, "\n", sep = "")
