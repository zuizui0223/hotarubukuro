args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

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
mlit_dir <- arg_value(
  "--mlit-dir", "results/public_rasters/mlit_human_forest_edge_2021"
)
checkpoint_root <- arg_value(
  "--checkpoint-root",
  "results/ecological_v16_predictive_replication/checkpoints"
)
output_dir <- arg_value(
  "--output", "results/ecological_v19_human_landscape_extremes"
)
control_min_q <- as.numeric(arg_value("--control-min-q", "0.25"))
fractions <- as.numeric(strsplit(
  arg_value("--fractions", "0.025,0.05,0.10"), ","
)[[1]])
rf_trees <- as.integer(arg_value("--rf-trees", "150"))
rf_null_draws <- as.integer(arg_value("--rf-null-draws", "250"))
max_draws <- as.integer(arg_value("--max-draws", "1000"))
exclude_mlit_boundary <- tolower(arg_value(
  "--exclude-mlit-boundary", "false"
)) %in% c("true", "1", "yes")

hb_require_stage_packages("human_landscape_features")
hb_load_modules("human_landscape_features")

cells <- utils::read.csv(
  cells_path, check.names = FALSE, stringsAsFactors = FALSE
)
observations <- utils::read.csv(
  observations_path, check.names = FALSE, stringsAsFactors = FALSE
)
mlit_cells <- utils::read.csv(
  file.path(mlit_dir, "human_forest_edge_1km_cells.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
landscape <- v19_lookup_mlit_landscape(
  observations, mlit_cells,
  file.path(mlit_dir, "mlit_human_forest_edge_1km.tif")
)
features <- v19_landscape_features(cells, landscape$cells)
predictors <- v19_rf_predictors()
definitions <- v19_contrast_definitions()
complete_support <- stats::complete.cases(
  features[, unique(c(predictors, definitions$feature)), drop = FALSE]
)
if (exclude_mlit_boundary) {
  complete_support <- complete_support &
    !(features$primary_mesh_boundary %in% TRUE)
}

presence <- v18_align_result(
  readRDS(file.path(
    checkpoint_root,
    "national_environment_spde_presence_draws1000.rds"
  )),
  cells, "presence"
)
counts <- presence$draws
if (is.finite(max_draws) && max_draws > 0L && ncol(counts) > max_draws) {
  counts <- counts[, seq_len(max_draws), drop = FALSE]
}
trials <- pmax(as.numeric(cells$n_observations), 1)
shares <- sweep(counts, 1, trials, "/")
q_sim <- v18_simulation_tail_q(counts, "upper")
z_sim <- v18_z_matrix(shares)
rf_draw_indices <- unique(as.integer(round(seq(
  1, ncol(q_sim),
  length.out = min(max(rf_null_draws, 1L), ncol(q_sim))
))))
ids <- as.character(cells$exact_site_id)
observed_share <- as.numeric(cells$n_pigmented) / trials
observed_q <- v18_predictive_tail_q(
  as.numeric(cells$n_pigmented), counts, "upper"
)
observed_sd <- apply(shares, 1, stats::sd)
observed_z <- (observed_share - rowMeans(shares)) /
  pmax(observed_sd, 1e-12)

message("[v19] building environment- and geography-matched control options")
match_options <- v18_match_options(cells, presence$latent_mean)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

contrast_summaries <- list()
contrast_null_rows <- list()
global_rows <- list()
pair_rows <- list()
profile_rows <- list()
rf_null <- NULL
rf_observed <- NULL
rf_importance_observed <- NULL
rf_importance_null <- NULL

for (fraction in fractions) {
  tier <- paste0("top_", sprintf("%04g", 100 * fraction))
  observed_eligible <- as.numeric(cells$n_pigmented) > 0 & complete_support
  observed_cases <- v18_top_indices(
    observed_q, observed_z, observed_eligible, fraction, ids
  )
  observed_controls <- which(
    observed_eligible & observed_q >= control_min_q
  )
  observed_pairs <- v18_match_cases(
    observed_cases, observed_controls, match_options,
    observed_q, observed_z, ids
  )
  observed_contrast <- v19_pair_contrasts(
    observed_pairs, features, definitions$feature
  )
  pair_rows[[length(pair_rows) + 1L]] <- v19_pair_details(
    observed_pairs, features, definitions$feature, tier
  )
  profile_rows[[length(profile_rows) + 1L]] <- v19_profile_counts(
    observed_pairs, features, tier
  )

  null_contrast <- matrix(
    NA_real_, nrow = ncol(q_sim), ncol = nrow(definitions),
    dimnames = list(NULL, definitions$feature)
  )
  matched_pairs <- integer(ncol(q_sim))
  if (abs(fraction - 0.05) < 1e-10) {
    rf_metric_matrix <- matrix(
      NA_real_, nrow = ncol(q_sim), ncol = 3L,
      dimnames = list(NULL, c(
        "spatial_cv_auc", "paired_concordance", "brier_score"
      ))
    )
    rf_importance_matrix <- matrix(
      NA_real_, nrow = ncol(q_sim), ncol = length(predictors),
      dimnames = list(NULL, predictors)
    )
  }

  message("[v19] ", tier, ": rematching ", ncol(q_sim), " natural maps")
  for (draw in seq_len(ncol(q_sim))) {
    eligible <- counts[, draw] > 0 & complete_support
    cases <- v18_top_indices(
      q_sim[, draw], z_sim[, draw], eligible, fraction, ids
    )
    controls <- which(eligible & q_sim[, draw] >= control_min_q)
    pairs <- v18_match_cases(
      cases, controls, match_options,
      q_sim[, draw], z_sim[, draw], ids
    )
    matched_pairs[draw] <- nrow(pairs)
    null_contrast[draw, ] <- v19_pair_contrasts(
      pairs, features, definitions$feature
    )
    if (abs(fraction - 0.05) < 1e-10 && draw %in% rf_draw_indices) {
      pair_data <- v19_pair_dataset(pairs, features, predictors)
      rf <- v19_spatial_rf(
        pair_data, predictors, seed = 190100L + draw,
        num_trees = rf_trees
      )
      rf_metric_matrix[draw, ] <- rf$metrics
      rf_importance_matrix[draw, ] <- rf$importance
    }
    if (draw %% 100L == 0L) {
      message("[v19] ", tier, ": draw ", draw)
    }
  }

  null_data <- data.frame(
    tier = tier, fraction = fraction, draw = seq_len(ncol(q_sim)),
    matched_pairs = matched_pairs, null_contrast,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  contrast_null_rows[[length(contrast_null_rows) + 1L]] <- null_data
  contrast_summaries[[length(contrast_summaries) + 1L]] <-
    v19_contrast_summary(
      observed_contrast, null_data, definitions, tier
    )
  global_rows[[length(global_rows) + 1L]] <-
    v19_global_landscape_test(
      observed_contrast, null_data, predictors, tier
    )

  if (abs(fraction - 0.05) < 1e-10) {
    observed_pair_data <- v19_pair_dataset(
      observed_pairs, features, predictors
    )
    observed_rf <- v19_spatial_rf(
      observed_pair_data, predictors, seed = 1901L,
      num_trees = rf_trees
    )
    rf_observed <- observed_rf$metrics
    rf_importance_observed <- observed_rf$importance
    rf_null <- data.frame(
      draw = seq_len(nrow(rf_metric_matrix)),
      matched_pairs = matched_pairs,
      rf_metric_matrix,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    rf_importance_null <- rf_importance_matrix
    write.csv(
      observed_rf$predictions,
      file.path(output_dir, "landscape_rf_observed_predictions.csv"),
      row.names = FALSE
    )
  }
}

contrast_summary <- do.call(rbind, contrast_summaries)
contrast_null <- do.call(rbind, contrast_null_rows)
global_tests <- do.call(rbind, global_rows)
observed_pairs <- do.call(rbind, pair_rows)
profile_counts <- do.call(rbind, profile_rows)

rf_summary_rows <- lapply(names(rf_observed), function(metric) {
  null <- rf_null[[metric]]
  null <- null[is.finite(null)]
  alternative <- if (metric == "brier_score") "less" else "greater"
  comparison <- v18_null_comparison(
    rf_observed[[metric]], null, alternative
  )
  data.frame(
    metric = metric, observed_value = rf_observed[[metric]],
    n_observed_pairs = sum(observed_pairs$tier == "top_0005"),
    n_null_draws = length(null), t(comparison),
    stringsAsFactors = FALSE
  )
})
rf_summary <- do.call(rbind, rf_summary_rows)

importance_rows <- lapply(predictors, function(feature) {
  null <- rf_importance_null[, feature]
  null <- null[is.finite(null)]
  data.frame(
    feature = feature,
    observed_permutation_importance = rf_importance_observed[[feature]],
    null_mean = mean(null), null_sd = stats::sd(null),
    null_percentile = mean(null <= rf_importance_observed[[feature]]),
    interpret_only_if_rf_auc_exceeds_null = TRUE,
    stringsAsFactors = FALSE
  )
})
rf_importance <- do.call(rbind, importance_rows)

join_index <- match(cells$exact_site_id, landscape$cells$exact_site_id)
join_audit <- rbind(
  landscape$audit,
  data.frame(
    metric = c(
      "n_analysis_cells", "n_analysis_cells_joined",
      "n_analysis_cells_complete_features",
      "maximum_absolute_existing_edge_difference",
      "maximum_absolute_existing_road_difference"
    ),
    value = c(
      nrow(cells), sum(!is.na(join_index)), sum(complete_support),
      max(abs(
        as.numeric(cells$human_forest_edge) -
          landscape$cells$forest_human_edge_1km[join_index]
      ), na.rm = TRUE),
      max(abs(
        as.numeric(cells$road_access) -
          landscape$cells$major_road_distance_km[join_index]
      ), na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
)

collinearity <- v19_collinearity(features, predictors)
write.csv(
  features, file.path(output_dir, "landscape_cell_features.csv"),
  row.names = FALSE
)
write.csv(
  definitions, file.path(output_dir, "landscape_feature_definitions.csv"),
  row.names = FALSE
)
write.csv(
  join_audit, file.path(output_dir, "landscape_join_audit.csv"),
  row.names = FALSE
)
write.csv(
  collinearity, file.path(output_dir, "landscape_collinearity.csv"),
  row.names = FALSE
)
write.csv(
  observed_pairs, file.path(output_dir, "landscape_observed_pairs.csv"),
  row.names = FALSE
)
write.csv(
  contrast_summary, file.path(output_dir, "landscape_contrast_summary.csv"),
  row.names = FALSE
)
write.csv(
  contrast_null, file.path(output_dir, "landscape_contrast_null.csv"),
  row.names = FALSE
)
write.csv(
  global_tests, file.path(output_dir, "landscape_global_tests.csv"),
  row.names = FALSE
)
write.csv(
  profile_counts, file.path(output_dir, "landscape_profile_counts.csv"),
  row.names = FALSE
)
write.csv(
  rf_summary, file.path(output_dir, "landscape_rf_summary.csv"),
  row.names = FALSE
)
write.csv(
  rf_null, file.path(output_dir, "landscape_rf_null.csv"),
  row.names = FALSE
)
write.csv(
  rf_importance, file.path(output_dir, "landscape_rf_importance.csv"),
  row.names = FALSE
)

metadata <- data.frame(
  field = c(
    "analysis_spec_version", "generated_at", "primary_fraction",
    "sensitivity_fractions", "n_natural_maps", "n_rf_null_maps", "rf_algorithm",
    "rf_validation", "rf_predictors", "case_selector",
    "exclude_mlit_primary_mesh_boundary",
    "landscape_features_used_for_selection_or_matching",
    "residual_used_as_response", "claim_ceiling"
  ),
  value = c(
    v19_analysis_spec_version, as.character(Sys.time()), "0.05",
    paste(setdiff(fractions, 0.05), collapse = ","),
    as.character(ncol(q_sim)), as.character(length(rf_draw_indices)),
    paste0("ranger; ", rf_trees, " trees"),
    "leave-one-100-km-spatial-fold-out; paired cases and controls held together",
    paste(predictors, collapse = ";"),
    "cross-fitted one-sided posterior predictive pigmentation tail",
    as.character(exclude_mlit_boundary),
    "none",
    "false",
    paste(
      "multivariate landscape signature of natural-model departures;",
      "not horticultural origin probability"
    )
  ),
  stringsAsFactors = FALSE
)
write.csv(
  metadata, file.path(output_dir, "landscape_analysis_metadata.csv"),
  row.names = FALSE
)

readme <- c(
  "# v19 multivariate human-landscape signature of predictive extremes",
  "",
  paste(
    "Extreme unexpected-pigmented cells are compared with geographically,",
    "environmentally, observationally, and naturally matched pigmented controls."
  ),
  paste(
    "Held-out landscape axes comprise local and 25-km population context,",
    "MLIT 2021 forest-human interface, forest and managed-land fractions,",
    "major-road remoteness, and relative elevation."
  ),
  "",
  paste(
    "A spatially cross-validated random forest asks whether these axes jointly",
    "discriminate observed extremes. The complete selection, matching, and",
    "classification workflow is repeated for 1000 natural predictive maps;",
    "the natural-map distribution, not an asymptotic classifier test, is the",
    "inferential reference."
  ),
  "",
  paste(
    "Random-forest importance is descriptive and is interpreted only if",
    "observed spatial-CV discrimination exceeds the natural-map null.",
    "The analysis uses no residual as a response and does not identify",
    "horticultural origin, escape, or introgression."
  )
)
writeLines(readme, file.path(output_dir, "README.md"), useBytes = TRUE)

cat("v19 landscape analysis written to ", normalizePath(output_dir), "\n")
print(global_tests)
print(contrast_summary[contrast_summary$tier == "top_0005", ])
print(rf_summary)
