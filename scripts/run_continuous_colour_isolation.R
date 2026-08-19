#!/usr/bin/env Rscript

# Final all-cell human-context analysis for the paper.
#
# The response is threshold-free colour-state isolation.  Human context is
# represented by WorldPop exposure at focal/5/10/25/50-km scales.  The same
# geometry is replayed across 10,000 cross-fitted natural colour maps.  Earlier
# 16-event, DID and MLIT-land-cover candidate pipelines are intentionally not
# dependencies of this analysis.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/analysis_cells.R")
source("R/continuous_colour_isolation.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

cells_path <- arg_value("--cells", "results/analysis_cells/analysis_cells_1km.csv")
worldpop_path <- arg_value("--worldpop", Sys.getenv("HOTARUBUKURO_WORLDPOP_RASTER", ""))
final8_path <- arg_value(
  "--final8", "results/final8_presence_null/final8_presence_draws10000.rds"
)
output_dir <- arg_value("--output", "results/continuous_colour_isolation")
n_draws <- as.integer(arg_value("--draws", "10000"))
permutations <- as.integer(arg_value("--permutations", "2000"))
chunk_size <- as.integer(arg_value("--chunk-size", "250"))
seed <- as.integer(arg_value("--seed", "20260725"))

for (path in c(cells_path, worldpop_path, final8_path)) {
  if (!nzchar(path) || !file.exists(path)) stop("Missing required input: ", path, call. = FALSE)
}
if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required", call. = FALSE)
if (!is.finite(n_draws) || n_draws < 100L) stop("--draws must be >= 100", call. = FALSE)
if (!is.finite(permutations) || permutations < 100L) stop("--permutations must be >= 100", call. = FALSE)
if (!is.finite(chunk_size) || chunk_size < 1L) stop("--chunk-size must be positive", call. = FALSE)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
model <- readRDS(final8_path)
v23_require_columns(
  cells,
  c(
    "exact_site_id", "longitude", "latitude", "x_km", "y_km", "spatial_fold",
    "n_observations", "n_pigmented", "n_white", "n_independent_sites",
    "human_population"
  ),
  "analysis cells"
)

# Population exposure.  Focal population is inherited from the final phenotype
# table; broader exposure is recomputed directly from the public WorldPop raster
# with the same circular-distance implementation used in the locked analysis.
population <- terra::rast(worldpop_path)
radii <- c(5L, 10L, 25L, 50L)
population_sum <- ac_point_context(population, cells, radii, "sum")
features <- data.frame(
  exact_site_id = as.character(cells$exact_site_id),
  spatial_fold = as.integer(cells$spatial_fold),
  local_population_rank = ac_rank01(log1p(pmax(as.numeric(cells$human_population), 0))),
  population_5km_rank = ac_rank01(log1p(pmax(population_sum[, "5"], 0))),
  population_10km_rank = ac_rank01(log1p(pmax(population_sum[, "10"], 0))),
  population_25km_rank = ac_rank01(log1p(pmax(population_sum[, "25"], 0))),
  population_50km_rank = ac_rank01(log1p(pmax(population_sum[, "50"], 0))),
  observation_effort_rank = ac_rank01(log1p(as.numeric(cells$n_observations))),
  independent_site_support_rank = ac_rank01(log1p(as.numeric(cells$n_independent_sites))),
  stringsAsFactors = FALSE
)
population_features <- c(
  "local_population_rank", "population_5km_rank", "population_10km_rank",
  "population_25km_rank", "population_50km_rank"
)
control_features <- c("observation_effort_rank", "independent_site_support_rank")
feature_names <- c(population_features, control_features)
utils::write.csv(features, file.path(output_dir, "population_context.csv"), row.names = FALSE)

state <- as.numeric(cells$n_pigmented) > 0
colour <- ifelse(state, "pigmented", "white")
observed_pigmented_count <- sum(state)
if (nrow(cells) != 1305L || observed_pigmented_count != 674L || sum(!state) != 631L) {
  stop("Frozen colour-state geometry changed", call. = FALSE)
}

distance <- v23_distance_matrix(cells)
neighbour_order <- v23_neighbour_order(distance)
any_colour_distances <- v23_any_colour_distances(distance)
observed_metrics <- v23_isolation_metrics(
  state, distance, neighbour_order, any_colour_distances
)
metric_values <- list(
  raw_same_colour_nn = observed_metrics$same_colour_nn_km[, 1L],
  relative_same_to_any_nn = observed_metrics$relative_isolation_nn[, 1L],
  relative_same_to_any_5nn = observed_metrics$relative_isolation_5nn[, 1L]
)

cell_metrics <- data.frame(
  exact_site_id = cells$exact_site_id,
  longitude = cells$longitude,
  latitude = cells$latitude,
  x_km = cells$x_km,
  y_km = cells$y_km,
  spatial_fold = cells$spatial_fold,
  colour = colour,
  pigmented = state,
  n_observations = cells$n_observations,
  n_pigmented = cells$n_pigmented,
  n_white = cells$n_white,
  same_colour_nn_km = metric_values$raw_same_colour_nn,
  any_colour_nn_km = observed_metrics$any_colour_nn_km,
  relative_isolation_nn = metric_values$relative_same_to_any_nn,
  relative_isolation_5nn = metric_values$relative_same_to_any_5nn,
  features[, feature_names, drop = FALSE],
  check.names = FALSE,
  stringsAsFactors = FALSE
)
utils::write.csv(cell_metrics, file.path(output_dir, "isolation_cell_metrics.csv"), row.names = FALSE)

# Descriptive correlations and within-fold feature permutations.
correlation_rows <- list()
observed_contrast_rows <- list()
row_counter <- 0L
for (metric_name in names(metric_values)) {
  isolation <- metric_values[[metric_name]]
  for (feature in feature_names) {
    feature_value <- as.numeric(features[[feature]])
    contrast <- v23_colour_rho_difference(isolation, state, feature_value)
    observed_contrast_rows[[length(observed_contrast_rows) + 1L]] <- data.frame(
      metric = metric_name,
      feature = feature,
      pigmented_rho = contrast[["pigmented_rho"]],
      white_rho = contrast[["white_rho"]],
      rho_difference = contrast[["rho_difference"]],
      stringsAsFactors = FALSE
    )
    for (colour_name in c("pigmented", "white")) {
      keep <- colour == colour_name
      descriptive <- v23_safe_spearman(isolation[keep], feature_value[keep])
      row_counter <- row_counter + 1L
      permutation <- v23_feature_permutation_test(
        isolation[keep], feature_value[keep],
        interaction(colour[keep], features$spatial_fold[keep], drop = TRUE),
        permutations = permutations,
        seed = seed + row_counter
      )
      correlation_rows[[length(correlation_rows) + 1L]] <- data.frame(
        metric = metric_name,
        colour = colour_name,
        feature = feature,
        n = as.integer(descriptive[["n"]]),
        spearman_rho = as.numeric(descriptive[["rho"]]),
        asymptotic_p = as.numeric(descriptive[["asymptotic_p"]]),
        within_fold_feature_permutation_p = permutation$two_sided_p,
        stringsAsFactors = FALSE
      )
    }
  }
}
correlations <- do.call(rbind, correlation_rows)
observed_contrasts <- do.call(rbind, observed_contrast_rows)
utils::write.csv(correlations, file.path(output_dir, "isolation_correlations.csv"), row.names = FALSE)
utils::write.csv(observed_contrasts, file.path(output_dir, "isolation_colour_contrasts.csv"), row.names = FALSE)

loo <- v23_leave_one_fold_out(
  metric_values$raw_same_colour_nn, state, features,
  population_features, as.integer(cells$spatial_fold)
)
utils::write.csv(loo, file.path(output_dir, "population_leave_one_fold_out.csv"), row.names = FALSE)

# Align final-eight natural maps without importing the superseded candidate-null module.
index <- match(cells$exact_site_id, model$cell_id)
if (anyNA(index) || anyDuplicated(index)) {
  stop("Final-eight natural maps do not align one-to-one with analysis cells", call. = FALSE)
}
simulated_counts <- model$draws[index, , drop = FALSE]
if (ncol(simulated_counts) < n_draws) {
  stop("Final-eight natural maps contain fewer draws than requested", call. = FALSE)
}
simulated_counts <- simulated_counts[, seq_len(n_draws), drop = FALSE]

natural_features <- feature_names
n_features <- length(natural_features)
new_null_matrix <- function() {
  matrix(NA_real_, nrow = n_draws, ncol = n_features,
         dimnames = list(NULL, natural_features))
}
raw_pigmented <- new_null_matrix()
raw_white <- new_null_matrix()
raw_difference <- new_null_matrix()
relative_pigmented <- new_null_matrix()
relative_white <- new_null_matrix()
relative_difference <- new_null_matrix()
pigmented_count <- integer(n_draws)

chunks <- split(seq_len(n_draws), ceiling(seq_len(n_draws) / chunk_size))
for (chunk_index in seq_along(chunks)) {
  draw_indices <- chunks[[chunk_index]]
  state_chunk <- simulated_counts[, draw_indices, drop = FALSE] > 0
  same_chunk <- v23_nearest_same_matrix(state_chunk, distance, neighbour_order)
  relative_chunk <- log(sweep(same_chunk, 1L, observed_metrics$any_colour_nn_km, "/"))
  for (local_index in seq_along(draw_indices)) {
    draw <- draw_indices[[local_index]]
    simulated_state <- state_chunk[, local_index]
    pigmented_count[draw] <- sum(simulated_state)
    raw_value <- same_chunk[, local_index]
    relative_value <- relative_chunk[, local_index]
    for (feature_index in seq_along(natural_features)) {
      feature_value <- as.numeric(features[[natural_features[[feature_index]]]])
      raw_pigmented[draw, feature_index] <- v23_spearman_rho(
        raw_value[simulated_state], feature_value[simulated_state]
      )
      raw_white[draw, feature_index] <- v23_spearman_rho(
        raw_value[!simulated_state], feature_value[!simulated_state]
      )
      raw_difference[draw, feature_index] <-
        raw_pigmented[draw, feature_index] - raw_white[draw, feature_index]
      relative_pigmented[draw, feature_index] <- v23_spearman_rho(
        relative_value[simulated_state], feature_value[simulated_state]
      )
      relative_white[draw, feature_index] <- v23_spearman_rho(
        relative_value[!simulated_state], feature_value[!simulated_state]
      )
      relative_difference[draw, feature_index] <-
        relative_pigmented[draw, feature_index] - relative_white[draw, feature_index]
    }
  }
  message("[continuous isolation] natural-map chunk ", chunk_index, "/", length(chunks))
}

exact_count_draws <- which(pigmented_count == observed_pigmented_count)
if (length(exact_count_draws) >= 200L) {
  count_mode <- "exact_observed_pigmented_count"
  conditioned_draws <- exact_count_draws
} else {
  count_mode <- "nearest_pigmented_count_up_to_1000"
  conditioned_draws <- head(
    order(abs(pigmented_count - observed_pigmented_count), seq_len(n_draws)),
    min(1000L, n_draws)
  )
}
utils::write.csv(
  data.frame(
    observed_pigmented_count = observed_pigmented_count,
    n_natural_maps = n_draws,
    n_exact_count_maps = length(exact_count_draws),
    count_conditioning_mode = count_mode,
    n_conditioned_maps = length(conditioned_draws),
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "count_conditioning.csv"), row.names = FALSE
)

observed_for <- function(metric, feature) {
  observed_contrasts[
    observed_contrasts$metric == metric & observed_contrasts$feature == feature,
    , drop = FALSE
  ]
}
summary_rows <- list()
append_summary <- function(metric, feature, component, observed, simulated, mode, draw_index) {
  comparison <- v23_null_comparison(observed, simulated[draw_index], "greater")
  summary_rows[[length(summary_rows) + 1L]] <<- data.frame(
    metric = metric, feature = feature, component = component,
    null_mode = mode, observed_value = observed,
    n_null_maps = sum(is.finite(simulated[draw_index])),
    t(comparison), check.names = FALSE, stringsAsFactors = FALSE
  )
}
all_draws <- seq_len(n_draws)
for (feature_index in seq_along(natural_features)) {
  feature <- natural_features[[feature_index]]
  raw_observed <- observed_for("raw_same_colour_nn", feature)
  relative_observed <- observed_for("relative_same_to_any_nn", feature)
  for (mode in c("all_nondegenerate_maps", count_mode)) {
    selected <- if (mode == "all_nondegenerate_maps") all_draws else conditioned_draws
    append_summary("raw_same_colour_nn", feature, "pigmented_rho",
                   raw_observed$pigmented_rho, raw_pigmented[, feature_index], mode, selected)
    append_summary("raw_same_colour_nn", feature, "white_rho",
                   raw_observed$white_rho, raw_white[, feature_index], mode, selected)
    append_summary("raw_same_colour_nn", feature, "rho_difference",
                   raw_observed$rho_difference, raw_difference[, feature_index], mode, selected)
    append_summary("relative_same_to_any_nn", feature, "pigmented_rho",
                   relative_observed$pigmented_rho, relative_pigmented[, feature_index], mode, selected)
    append_summary("relative_same_to_any_nn", feature, "white_rho",
                   relative_observed$white_rho, relative_white[, feature_index], mode, selected)
    append_summary("relative_same_to_any_nn", feature, "rho_difference",
                   relative_observed$rho_difference, relative_difference[, feature_index], mode, selected)
  }
}
natural_summary <- do.call(rbind, summary_rows)
utils::write.csv(
  natural_summary, file.path(output_dir, "natural_map_summary.csv"), row.names = FALSE
)

population_indices <- match(population_features, natural_features)
observed_population_raw <- vapply(population_features, function(feature) {
  observed_for("raw_same_colour_nn", feature)$rho_difference
}, numeric(1))
observed_population_relative <- vapply(population_features, function(feature) {
  observed_for("relative_same_to_any_nn", feature)$rho_difference
}, numeric(1))
maxT <- data.frame(
  metric = c("raw_same_colour_nn", "relative_same_to_any_nn"),
  all_maps_population_scale_maxT_p = c(
    v23_maxT_upper_p(observed_population_raw, raw_difference[, population_indices, drop = FALSE]),
    v23_maxT_upper_p(observed_population_relative, relative_difference[, population_indices, drop = FALSE])
  ),
  conditioned_population_scale_maxT_p = c(
    v23_maxT_upper_p(
      observed_population_raw,
      raw_difference[conditioned_draws, population_indices, drop = FALSE]
    ),
    v23_maxT_upper_p(
      observed_population_relative,
      relative_difference[conditioned_draws, population_indices, drop = FALSE]
    )
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(maxT, file.path(output_dir, "population_scale_maxT.csv"), row.names = FALSE)

profile_rows <- lapply(seq_along(population_features), function(i) {
  feature <- population_features[[i]]
  observed <- observed_for("raw_same_colour_nn", feature)
  null <- natural_summary[
    natural_summary$metric == "raw_same_colour_nn" &
      natural_summary$feature == feature &
      natural_summary$component == "pigmented_rho" &
      natural_summary$null_mode == "all_nondegenerate_maps",
    , drop = FALSE
  ]
  data.frame(
    feature = feature,
    radius_km = c(0, 5, 10, 25, 50)[[i]],
    pigmented_rho = observed$pigmented_rho,
    white_rho = observed$white_rho,
    natural_pigmented_mean = null$null_mean,
    natural_pigmented_lower_95 = null$lower_95,
    natural_pigmented_upper_95 = null$upper_95,
    natural_pigmented_upper_tail_p = null$empirical_p,
    stringsAsFactors = FALSE
  )
})
profile <- do.call(rbind, profile_rows)
utils::write.csv(profile, file.path(output_dir, "population_scale_profile.csv"), row.names = FALSE)

# Frozen headline check.  This catches accidental changes to the final estimand
# while allowing the natural-map Monte Carlo summaries to be regenerated.
headline_raw <- observed_for("raw_same_colour_nn", "population_5km_rank")$pigmented_rho
headline_relative <- observed_for("relative_same_to_any_nn", "population_5km_rank")$pigmented_rho
if (abs(headline_raw - 0.251980) > 5e-6 || abs(headline_relative - 0.285498) > 5e-6) {
  stop(
    "Headline isolation-population estimates changed: raw=", headline_raw,
    ", relative=", headline_relative, call. = FALSE
  )
}
writeLines(
  c(
    "status=PASS",
    paste0("cells=", nrow(cells)),
    paste0("pigmented_cells=", observed_pigmented_count),
    paste0("raw_pigmented_rho_population_5km=", format(headline_raw, digits = 9)),
    paste0("relative_pigmented_rho_population_5km=", format(headline_relative, digits = 9)),
    "claim=human-context overlay; not provenance or causation"
  ),
  file.path(output_dir, "validation.txt")
)
cat("Completed final continuous colour-isolation analysis.\n")
