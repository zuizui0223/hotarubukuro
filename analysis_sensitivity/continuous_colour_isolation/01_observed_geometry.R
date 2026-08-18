#!/usr/bin/env Rscript

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
output_dir <- arg_value(
  "--output", "results/continuous_colour_isolation_human_context"
)
n_draws <- as.integer(arg_value("--draws", "10000"))
permutations <- as.integer(arg_value("--permutations", "2000"))
chunk_size <- as.integer(arg_value("--chunk-size", "250"))
seed <- as.integer(arg_value("--seed", "20260725"))

if (!is.finite(n_draws) || n_draws < 1L) stop("draws must be positive.", call. = FALSE)
if (!is.finite(permutations) || permutations < 1L) {
  stop("permutations must be positive.", call. = FALSE)
}
if (!is.finite(chunk_size) || chunk_size < 1L) {
  stop("chunk-size must be positive.", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required.", call. = FALSE)
}
source("R/candidate_null_tools.R")
source("R/continuous_colour_isolation.R")

cells_path <- file.path(
  reference_root,
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
human_path <- file.path(
  reference_root,
  "results/ecological_v21_local_human_neighbourhood/human_neighbourhood_cell_features.csv"
)
did_path <- file.path(
  reference_root,
  "results/ecological_v22_did_human_context/did_cell_context.csv"
)
final8_path <- file.path(final8_root, "final8_presence_draws10000.rds")
for (path in c(cells_path, human_path, did_path, final8_path)) {
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
model <- readRDS(final8_path)

v23_require_columns(
  cells,
  c(
    "exact_site_id", "x_km", "y_km", "longitude", "latitude",
    "spatial_fold", "n_observations", "n_pigmented", "n_white",
    "n_independent_sites", "natural_presence_probability"
  ),
  "cells"
)

human_index <- match(cells$exact_site_id, human$exact_site_id)
did_index <- match(cells$exact_site_id, did$exact_site_id)
if (anyNA(human_index) || anyNA(did_index)) {
  stop("Human-context tables do not align one-to-one with cells.", call. = FALSE)
}

features <- data.frame(
  exact_site_id = as.character(cells$exact_site_id),
  spatial_fold = as.integer(cells$spatial_fold),
  local_population_rank = as.numeric(human$local_population_rank[human_index]),
  population_5km_rank = as.numeric(human$population_5km_rank[human_index]),
  population_10km_rank = as.numeric(human$population_10km_rank[human_index]),
  population_25km_rank = as.numeric(human$population_25km_rank[human_index]),
  population_50km_rank = as.numeric(human$population_50km_rank[human_index]),
  did_proximity_rank = as.numeric(did$did_proximity_rank[did_index]),
  artificial_land_fraction_rank =
    as.numeric(human$artificial_land_fraction_rank[human_index]),
  forest_human_edge_rank =
    as.numeric(human$forest_human_edge_rank[human_index]),
  built_up_fraction_rank =
    as.numeric(human$built_up_fraction_rank[human_index]),
  agriculture_fraction_rank =
    as.numeric(human$agriculture_fraction_rank[human_index]),
  mountainness_rank = as.numeric(human$mountainness_rank[human_index]),
  observation_effort_rank =
    as.numeric(human$observation_effort_rank[human_index]),
  independent_site_support_rank =
    as.numeric(human$independent_site_support_rank[human_index]),
  stringsAsFactors = FALSE
)

registry <- data.frame(
  feature = c(
    "local_population_rank", "population_5km_rank",
    "population_10km_rank", "population_25km_rank",
    "population_50km_rank", "did_proximity_rank",
    "artificial_land_fraction_rank", "forest_human_edge_rank",
    "built_up_fraction_rank", "agriculture_fraction_rank",
    "mountainness_rank", "observation_effort_rank",
    "independent_site_support_rank"
  ),
  family = c(
    rep("population_scale", 5L), "dense_settlement",
    rep("land_cover", 4L), "natural_alternative",
    rep("observation_process", 2L)
  ),
  role = c(
    "population_focal", "primary_population_5km",
    "population_10km", "population_25km", "population_50km",
    "did_proximity", "artificial_land", "forest_human_edge",
    "built_land", "agriculture", "mountainness",
    "observation_effort", "independent_site_support"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  registry, file.path(output_dir, "continuous_isolation_feature_registry.csv"),
  row.names = FALSE
)

state <- as.numeric(cells$n_pigmented) > 0
colour <- ifelse(state, "pigmented", "white")
observed_pigmented_count <- sum(state)
if (observed_pigmented_count != 674L || sum(!state) != 631L) {
  stop(
    "Frozen colour-state counts changed: pigmented=", sum(state),
    ", white=", sum(!state), call. = FALSE
  )
}

distance <- v23_distance_matrix(cells)
neighbour_order <- v23_neighbour_order(distance)
any_colour_distances <- v23_any_colour_distances(distance)
observed_metrics <- v23_isolation_metrics(
  state, distance, neighbour_order, any_colour_distances
)
raw_isolation <- observed_metrics$same_colour_nn_km[, 1L]
relative_nn <- observed_metrics$relative_isolation_nn[, 1L]
relative_5nn <- observed_metrics$relative_isolation_5nn[, 1L]

count_within <- function(radius) {
  rowSums(distance <= radius, na.rm = TRUE)
}
cell_metrics <- data.frame(
  exact_site_id = as.character(cells$exact_site_id),
  longitude = as.numeric(cells$longitude),
  latitude = as.numeric(cells$latitude),
  x_km = as.numeric(cells$x_km),
  y_km = as.numeric(cells$y_km),
  spatial_fold = as.integer(cells$spatial_fold),
  colour = colour,
  pigmented = state,
  n_observations = as.integer(cells$n_observations),
  n_pigmented = as.integer(cells$n_pigmented),
  n_white = as.integer(cells$n_white),
  same_colour_nn_km = raw_isolation,
  any_colour_nn_km = observed_metrics$any_colour_nn_km,
  any_colour_5nn_km = observed_metrics$any_colour_5nn_km,
  any_colour_10nn_km = observed_metrics$any_colour_10nn_km,
  relative_isolation_nn = relative_nn,
  relative_isolation_5nn = relative_5nn,
  flower_cells_within_10km = count_within(10),
  flower_cells_within_25km = count_within(25),
  natural_presence_probability =
    as.numeric(cells$natural_presence_probability),
  features[, setdiff(names(features), c("exact_site_id", "spatial_fold")),
           drop = FALSE],
  check.names = FALSE,
  stringsAsFactors = FALSE
)
utils::write.csv(
  cell_metrics,
  file.path(output_dir, "continuous_isolation_cell_metrics.csv"),
  row.names = FALSE
)

metric_values <- list(
  raw_same_colour_nn = raw_isolation,
  relative_same_to_any_nn = relative_nn,
  relative_same_to_any_5nn = relative_5nn
)
correlation_rows <- list()
row_counter <- 0L
for (metric_name in names(metric_values)) {
  isolation <- metric_values[[metric_name]]
  for (colour_name in c("pigmented", "white")) {
    colour_keep <- colour == colour_name
    for (feature_index in seq_len(nrow(registry))) {
      feature <- registry$feature[feature_index]
      values <- as.numeric(features[[feature]])
      descriptive <- v23_safe_spearman(
        isolation[colour_keep], values[colour_keep]
      )
      permutation <- v23_feature_permutation_test(
        isolation[colour_keep], values[colour_keep],
        interaction(
          colour[colour_keep], features$spatial_fold[colour_keep],
          drop = TRUE
        ),
        permutations = permutations,
        seed = seed + row_counter + 1L
      )
      row_counter <- row_counter + 1L
      correlation_rows[[length(correlation_rows) + 1L]] <- data.frame(
        metric = metric_name,
        colour = colour_name,
        feature = feature,
        family = registry$family[feature_index],
        role = registry$role[feature_index],
        n = as.integer(descriptive[["n"]]),
        spearman_rho = as.numeric(descriptive[["rho"]]),
        asymptotic_p = as.numeric(descriptive[["asymptotic_p"]]),
        within_fold_feature_permutation_p = permutation$two_sided_p,
        permutations = permutations,
        stringsAsFactors = FALSE
      )
    }
  }
}
correlations <- do.call(rbind, correlation_rows)
utils::write.csv(
  correlations,
  file.path(output_dir, "continuous_isolation_correlations.csv"),
  row.names = FALSE
)

population_features <- c(
  "local_population_rank", "population_5km_rank",
  "population_10km_rank", "population_25km_rank",
  "population_50km_rank"
)
contrast_rows <- list()
for (metric_name in names(metric_values)) {
  isolation <- metric_values[[metric_name]]
  for (feature in registry$feature) {
    contrast <- v23_colour_rho_difference(
      isolation, state, as.numeric(features[[feature]])
    )
    contrast_rows[[length(contrast_rows) + 1L]] <- data.frame(
      metric = metric_name,
      feature = feature,
      family = registry$family[match(feature, registry$feature)],
      pigmented_rho = contrast[["pigmented_rho"]],
      white_rho = contrast[["white_rho"]],
      rho_difference = contrast[["rho_difference"]],
      stringsAsFactors = FALSE
    )
  }
}
observed_contrasts <- do.call(rbind, contrast_rows)
utils::write.csv(
  observed_contrasts,
  file.path(output_dir, "continuous_isolation_colour_contrasts.csv"),
  row.names = FALSE
)

primary_label_raw <- v23_fold_label_null(
  state, features$population_5km_rank, distance, neighbour_order,
  features$spatial_fold, permutations = permutations, seed = seed,
  metric = "raw"
)
primary_label_relative <- v23_fold_label_null(
  state, features$population_5km_rank, distance, neighbour_order,
  features$spatial_fold, permutations = permutations, seed = seed + 1L,
  metric = "relative_nn"
)
label_summary <- rbind(
  data.frame(
    metric = "raw_same_colour_nn",
    t(primary_label_raw$observed),
    fold_label_permutation_upper_p = primary_label_raw$upper_p,
    permutations = permutations,
    check.names = FALSE,
    stringsAsFactors = FALSE
  ),
  data.frame(
    metric = "relative_same_to_any_nn",
    t(primary_label_relative$observed),
    fold_label_permutation_upper_p = primary_label_relative$upper_p,
    permutations = permutations,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
)
utils::write.csv(
  label_summary,
  file.path(output_dir, "continuous_isolation_fold_label_summary.csv"),
  row.names = FALSE
)
label_null <- rbind(
  data.frame(
    metric = "raw_same_colour_nn",
    permutation = seq_len(permutations),
    primary_label_raw$null,
    check.names = FALSE,
    stringsAsFactors = FALSE
  ),
  data.frame(
    metric = "relative_same_to_any_nn",
    permutation = seq_len(permutations),
    primary_label_relative$null,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
)
utils::write.csv(
  label_null,
  file.path(output_dir, "continuous_isolation_fold_label_null.csv"),
  row.names = FALSE
)

top_summary <- v23_top_fraction_summary(
  raw_isolation, state, features, registry$feature, fraction = 0.10
)
utils::write.csv(
  top_summary,
  file.path(output_dir, "continuous_isolation_top10_summary.csv"),
  row.names = FALSE
)

lofo <- v23_leave_one_fold_out(
  raw_isolation, state, features, population_features,
  features$spatial_fold
)
utils::write.csv(
  lofo,
  file.path(output_dir, "continuous_isolation_leave_one_fold_out.csv"),
  row.names = FALSE
)

fold_rows <- list()
for (fold_value in sort(unique(features$spatial_fold))) {
  keep <- features$spatial_fold == fold_value
  for (feature in population_features) {
    contrast <- v23_colour_rho_difference(
      raw_isolation[keep], state[keep], as.numeric(features[[feature]][keep])
    )
    fold_rows[[length(fold_rows) + 1L]] <- data.frame(
      spatial_fold = fold_value,
      feature = feature,
      n_cells = sum(keep),
      pigmented_count = sum(state[keep]),
      white_count = sum(!state[keep]),
      pigmented_rho = contrast[["pigmented_rho"]],
      white_rho = contrast[["white_rho"]],
      rho_difference = contrast[["rho_difference"]],
      stringsAsFactors = FALSE
    )
  }
}
fold_specific <- do.call(rbind, fold_rows)
utils::write.csv(
  fold_specific,
  file.path(output_dir, "continuous_isolation_fold_specific.csv"),
  row.names = FALSE
)
