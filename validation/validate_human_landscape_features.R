args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) {
  args[[1L]]
} else {
  "results/ecological_v19_human_landscape_extremes"
}

read_output <- function(name) {
  utils::read.csv(
    file.path(output_dir, name), check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

close_enough <- function(x, y, tolerance = 1e-9) {
  length(x) == length(y) &&
    all((is.na(x) & is.na(y)) |
          (is.finite(x) & is.finite(y) & abs(x - y) <= tolerance))
}

checks <- list()
add_check <- function(check, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check, status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = detail, stringsAsFactors = FALSE
  )
}

features <- read_output("landscape_cell_features.csv")
definitions <- read_output("landscape_feature_definitions.csv")
pairs <- read_output("landscape_observed_pairs.csv")
summary <- read_output("landscape_contrast_summary.csv")
null <- read_output("landscape_contrast_null.csv")
global <- read_output("landscape_global_tests.csv")
profiles <- read_output("landscape_profile_counts.csv")
rf_summary <- read_output("landscape_rf_summary.csv")
rf_null <- read_output("landscape_rf_null.csv")
importance <- read_output("landscape_rf_importance.csv")
collinearity <- read_output("landscape_collinearity.csv")
join_audit <- read_output("landscape_join_audit.csv")
metadata <- read_output("landscape_analysis_metadata.csv")
metadata_value <- setNames(metadata$value, metadata$field)
exclude_boundary <- "exclude_mlit_primary_mesh_boundary" %in%
  names(metadata_value) &&
  metadata_value[["exclude_mlit_primary_mesh_boundary"]] == "TRUE"

add_check(
  "cell_grain",
  nrow(features) == 1307L && !anyDuplicated(features$exact_site_id),
  paste("rows=", nrow(features), "unique=", length(unique(features$exact_site_id)))
)
rank_columns <- grep("_rank$", names(features), value = TRUE)
rank_values <- unlist(features[rank_columns], use.names = FALSE)
add_check(
  "rank_bounds",
  all(rank_values[is.finite(rank_values)] >= 0 &
        rank_values[is.finite(rank_values)] <= 1),
  paste("range=", paste(range(rank_values, na.rm = TRUE), collapse = " to "))
)
add_check(
  "landscape_join_coverage",
  {
    expected_complete <- stats::complete.cases(
      features[, unique(c(
        definitions$feature,
        "local_population_rank", "regional_population_rank",
        "forest_human_edge_rank", "forest_cover_rank", "managed_land_rank",
        "road_remoteness_rank", "mountainness_rank"
      )), drop = FALSE]
    )
    if (exclude_boundary) {
      expected_complete <- expected_complete &
        !(features$primary_mesh_boundary %in% TRUE)
    }
    join_audit$value[join_audit$metric == "n_analysis_cells_joined"] ==
      1307 &&
      join_audit$value[
        join_audit$metric == "n_analysis_cells_complete_features"
      ] == sum(expected_complete)
  },
  paste(
    "joined=",
    join_audit$value[join_audit$metric == "n_analysis_cells_joined"],
    "complete=",
    join_audit$value[
      join_audit$metric == "n_analysis_cells_complete_features"
    ]
  )
)
add_check(
  "natural_null_completion",
  all(table(null$tier) == 1000L),
  paste(names(table(null$tier)), table(null$tier), collapse = "; ")
)
add_check(
  "rf_null_completion",
  all(vapply(
    rf_null[c("spatial_cv_auc", "paired_concordance", "brier_score")],
    function(x) sum(is.finite(x)), integer(1)
  ) == 250L),
  paste(
    names(rf_null)[3:5],
    vapply(rf_null[3:5], function(x) sum(is.finite(x)), integer(1)),
    collapse = "; "
  )
)

observed_recomputed <- numeric(nrow(summary))
null_mean_recomputed <- numeric(nrow(summary))
null_sd_recomputed <- numeric(nrow(summary))
p_recomputed <- numeric(nrow(summary))
two_sided_recomputed <- numeric(nrow(summary))
for (index in seq_len(nrow(summary))) {
  tier <- summary$tier[index]
  feature <- summary$feature[index]
  observed_value <- pairs[
    pairs$tier == tier, paste0("difference_", feature)
  ]
  observed_recomputed[index] <- mean(observed_value, na.rm = TRUE)
  simulated <- null[null$tier == tier, feature]
  simulated <- simulated[is.finite(simulated)]
  null_mean_recomputed[index] <- mean(simulated)
  null_sd_recomputed[index] <- stats::sd(simulated)
  upper <- (1 + sum(simulated >= observed_recomputed[index])) /
    (length(simulated) + 1)
  lower <- (1 + sum(simulated <= observed_recomputed[index])) /
    (length(simulated) + 1)
  direction <- summary$hypothesis_direction[index]
  p_recomputed[index] <- if (direction == "greater") {
    upper
  } else if (direction == "less") {
    lower
  } else {
    min(1, 2 * min(upper, lower))
  }
  two_sided_recomputed[index] <- min(1, 2 * min(upper, lower))
}
add_check(
  "observed_pair_contrasts",
  close_enough(
    observed_recomputed, summary$observed_case_control_difference
  ),
  paste("comparisons=", nrow(summary))
)
add_check(
  "null_moments",
  close_enough(null_mean_recomputed, summary$null_mean) &&
    close_enough(null_sd_recomputed, summary$null_sd),
  paste("comparisons=", nrow(summary))
)
add_check(
  "feature_empirical_p",
  close_enough(
    p_recomputed, summary$directional_or_two_sided_p
  ) && close_enough(two_sided_recomputed, summary$two_sided_p),
  paste("comparisons=", nrow(summary))
)

for (tier in unique(summary$tier)) {
  block <- summary[summary$tier == tier, , drop = FALSE]
  sim <- as.matrix(null[null$tier == tier, block$feature, drop = FALSE])
  center <- colMeans(sim)
  spread <- apply(sim, 2, stats::sd)
  spread[!is.finite(spread) | spread <= 1e-12] <- 1
  null_z <- sweep(sweep(sim, 2, center, "-"), 2, spread, "/")
  observed_z <- (
    block$observed_case_control_difference - center
  ) / spread
  sign <- ifelse(block$hypothesis_direction == "less", -1, 1)
  max_directional <- apply(sweep(null_z, 2, sign, "*"), 1, max)
  max_absolute <- apply(abs(null_z), 1, max)
  fwer <- vapply(seq_len(nrow(block)), function(index) {
    if (block$hypothesis_direction[index] == "two_sided") {
      (1 + sum(max_absolute >= abs(observed_z[index]))) /
        (length(max_absolute) + 1)
    } else {
      (1 + sum(max_directional >= observed_z[index] * sign[index])) /
        (length(max_directional) + 1)
    }
  }, numeric(1))
  add_check(
    paste0("maxT_", tier),
    close_enough(fwer, block$maxT_FWER_p),
    paste("features=", nrow(block))
  )
}

raw_features <- c(
  "local_population_rank", "regional_population_rank",
  "forest_human_edge_rank", "forest_cover_rank", "managed_land_rank",
  "road_remoteness_rank", "mountainness_rank"
)
global_recomputed <- numeric(nrow(global))
global_p_recomputed <- numeric(nrow(global))
for (index in seq_len(nrow(global))) {
  tier <- global$tier[index]
  sim <- as.matrix(null[null$tier == tier, raw_features, drop = FALSE])
  center <- colMeans(sim)
  covariance <- stats::cov(sim)
  eigenvalue <- eigen(
    covariance, symmetric = TRUE, only.values = TRUE
  )$values
  ridge <- max(max(eigenvalue) * 1e-6, 1e-8)
  inverse <- solve(covariance + diag(ridge, ncol(covariance)))
  sim_centered <- sweep(sim, 2, center, "-")
  sim_distance <- rowSums((sim_centered %*% inverse) * sim_centered)
  observed <- summary[
    match(
      paste(tier, raw_features),
      paste(summary$tier, summary$feature)
    ),
    "observed_case_control_difference"
  ]
  centered <- observed - center
  global_recomputed[index] <- as.numeric(
    t(centered) %*% inverse %*% centered
  )
  global_p_recomputed[index] <- (
    1 + sum(sim_distance >= global_recomputed[index])
  ) / (length(sim_distance) + 1)
}
add_check(
  "global_multivariate_distance",
  close_enough(global_recomputed, global$observed_value) &&
    close_enough(global_p_recomputed, global$empirical_p),
  paste("tiers=", nrow(global))
)

rf_observed_recomputed <- c(
  spatial_cv_auc = NA_real_,
  paired_concordance = NA_real_,
  brier_score = NA_real_
)
rf_p_recomputed <- numeric(nrow(rf_summary))
for (index in seq_len(nrow(rf_summary))) {
  metric <- rf_summary$metric[index]
  simulated <- rf_null[[metric]]
  simulated <- simulated[is.finite(simulated)]
  observed <- rf_summary$observed_value[index]
  rf_p_recomputed[index] <- if (metric == "brier_score") {
    (1 + sum(simulated <= observed)) / (length(simulated) + 1)
  } else {
    (1 + sum(simulated >= observed)) / (length(simulated) + 1)
  }
}
add_check(
  "rf_empirical_p",
  close_enough(rf_p_recomputed, rf_summary$empirical_p),
  paste("metrics=", nrow(rf_summary))
)
add_check(
  "rf_importance_guard",
  all(importance$interpret_only_if_rf_auc_exceeds_null %in% TRUE),
  "all feature importances are guarded by classifier performance"
)

profile_recomputed <- list()
for (tier in unique(profiles$tier)) {
  block <- pairs[pairs$tier == tier, , drop = FALSE]
  for (profile in unique(profiles$landscape_profile)) {
    profile_recomputed[[length(profile_recomputed) + 1L]] <- data.frame(
      tier = tier, landscape_profile = profile,
      case_count = sum(block$case_profile == profile),
      control_count = sum(block$control_profile == profile)
    )
  }
}
profile_recomputed <- do.call(rbind, profile_recomputed)
profile_index <- match(
  paste(profiles$tier, profiles$landscape_profile),
  paste(profile_recomputed$tier, profile_recomputed$landscape_profile)
)
add_check(
  "landscape_profile_counts",
  all(profiles$case_count == profile_recomputed$case_count[profile_index]) &&
    all(profiles$control_count ==
          profile_recomputed$control_count[profile_index]),
  paste("rows=", nrow(profiles))
)

correlation <- stats::cor(
  features[, raw_features], use = "pairwise.complete.obs",
  method = "spearman"
)
correlation_rows <- which(upper.tri(correlation), arr.ind = TRUE)
recomputed_max <- max(abs(correlation[correlation_rows]))
add_check(
  "collinearity_recalculation",
  close_enough(
    recomputed_max, max(collinearity$absolute_spearman_rho)
  ),
  paste("maximum absolute rho=", round(recomputed_max, 4))
)
add_check(
  "selection_independence",
  identical(
    metadata_value[["landscape_features_used_for_selection_or_matching"]],
    "none"
  ),
  paste(
    "selection/matching landscape role=",
    metadata_value[["landscape_features_used_for_selection_or_matching"]]
  )
)
add_check(
  "residual_free",
  identical(metadata_value[["residual_used_as_response"]], "false"),
  paste("residual_used_as_response=", metadata_value[["residual_used_as_response"]])
)
validation <- do.call(rbind, checks)
utils::write.csv(
  validation, file.path(output_dir, "landscape_independent_validation.csv"),
  row.names = FALSE
)
if (any(validation$status != "PASS")) {
  print(validation[validation$status != "PASS", ])
  stop("v19 independent validation failed.", call. = FALSE)
}
cat("v19 independent validation passed: ", nrow(validation), " checks\n")
