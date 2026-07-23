args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) {
  args[[1L]]
} else {
  "results/ecological_v20_local_white_isolates"
}

source("scripts/extreme_human_diagnostic_v11.R")
source("scripts/human_landscape_extremes_v12.R")
source("scripts/local_white_isolate_v13.R")

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
    check = check,
    status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = detail,
    stringsAsFactors = FALSE
  )
}

cells <- utils::read.csv(
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv",
  check.names = FALSE, stringsAsFactors = FALSE
)
configurations <- read_output("local_isolate_configurations.csv")
graph_support <- read_output("local_isolate_graph_support.csv")
candidates <- read_output("local_isolate_candidates.csv")
natural_summary <- read_output("local_isolate_natural_null_summary.csv")
natural_null <- read_output("local_isolate_natural_null.csv")
pairs <- read_output("local_isolate_observed_pairs.csv")
landscape_summary <- read_output("local_isolate_landscape_summary.csv")
landscape_null <- read_output("local_isolate_landscape_null.csv")
landscape_global <- read_output("local_isolate_landscape_global.csv")
auxiliary_summary <- read_output("local_isolate_auxiliary_facets_summary.csv")
auxiliary_null <- read_output("local_isolate_auxiliary_facets_null.csv")
metadata <- read_output("local_isolate_metadata.csv")
metadata_value <- setNames(metadata$value, metadata$field)

add_check(
  "configuration_registry",
  nrow(configurations) == 6L &&
    sum(configurations$role == "primary") == 1L &&
    !configurations$same_fold_only[
      configurations$role == "primary"
    ],
  paste(
    "configurations=", nrow(configurations),
    "primary uses all physical neighbours"
  )
)
add_check(
  "graph_cell_grain",
  nrow(graph_support) == nrow(cells) * nrow(configurations) &&
    !anyDuplicated(paste(
      graph_support$configuration, graph_support$exact_site_id
    )),
  paste("rows=", nrow(graph_support))
)
add_check(
  "natural_null_completion",
  nrow(natural_null) == 1000L * nrow(configurations) &&
    all(table(natural_null$configuration) == 1000L),
  paste("rows=", nrow(natural_null))
)

recomputed_natural <- natural_summary
for (index in seq_len(nrow(recomputed_natural))) {
  block <- natural_null[
    natural_null$configuration == recomputed_natural$configuration[index],
    recomputed_natural$metric[index]
  ]
  observed <- recomputed_natural$observed_value[index]
  upper <- (1 + sum(block >= observed)) / (length(block) + 1)
  lower <- (1 + sum(block <= observed)) / (length(block) + 1)
  recomputed_natural$null_mean[index] <- mean(block)
  recomputed_natural$null_sd[index] <- stats::sd(block)
  recomputed_natural$empirical_p[index] <- upper
  recomputed_natural$empirical_two_sided_p[index] <-
    min(1, 2 * min(upper, lower))
}
add_check(
  "natural_null_statistics",
  close_enough(
    recomputed_natural$null_mean, natural_summary$null_mean
  ) &&
    close_enough(recomputed_natural$null_sd, natural_summary$null_sd) &&
    close_enough(
      recomputed_natural$empirical_p, natural_summary$empirical_p
    ) &&
    close_enough(
      recomputed_natural$empirical_two_sided_p,
      natural_summary$empirical_two_sided_p
    ),
  paste("comparisons=", nrow(natural_summary))
)

primary_configuration <- configurations[
  configurations$role == "primary", , drop = FALSE
]
primary_graph <- v20_neighbour_graph(
  cells,
  radius_km = primary_configuration$radius_km,
  environment_caliper = primary_configuration$environment_caliper,
  minimum_neighbours = primary_configuration$minimum_neighbours,
  same_fold_only = primary_configuration$same_fold_only
)
primary_profile <- v20_local_profile(
  cells$n_pigmented, primary_graph,
  primary_configuration$maximum_neighbour_pigment_share
)
expected_candidate_ids <- sort(
  cells$exact_site_id[primary_profile$candidate[, 1L]]
)
add_check(
  "primary_candidate_reproduction",
  identical(expected_candidate_ids, sort(candidates$exact_site_id)),
  paste("candidates=", length(expected_candidate_ids))
)
add_check(
  "literal_white_neighbourhood",
  all(candidates$n_pigmented > 0) &&
    all(candidates$n_neighbours >= 3) &&
    all(candidates$neighbour_pigment_share == 0) &&
    all(candidates$mean_environmental_distance <= 1),
  paste(
    "candidate neighbour share range=",
    paste(range(candidates$neighbour_pigment_share), collapse = " to ")
  )
)

recompute_summary <- function(summary, null, pairs = NULL) {
  observed_recomputed <- numeric(nrow(summary))
  p_recomputed <- numeric(nrow(summary))
  two_sided_recomputed <- numeric(nrow(summary))
  for (index in seq_len(nrow(summary))) {
    feature <- summary$feature[index]
    if (is.null(pairs)) {
      observed_recomputed[index] <-
        summary$observed_case_control_difference[index]
    } else {
      observed_recomputed[index] <- mean(
        pairs[[paste0("difference_", feature)]], na.rm = TRUE
      )
    }
    simulated <- null[[feature]]
    simulated <- simulated[is.finite(simulated)]
    upper <- (1 + sum(simulated >= observed_recomputed[index])) /
      (length(simulated) + 1)
    lower <- (1 + sum(simulated <= observed_recomputed[index])) /
      (length(simulated) + 1)
    p_recomputed[index] <- if (
      summary$hypothesis_direction[index] == "less"
    ) {
      lower
    } else if (summary$hypothesis_direction[index] == "two_sided") {
      min(1, 2 * min(upper, lower))
    } else {
      upper
    }
    two_sided_recomputed[index] <- min(1, 2 * min(upper, lower))
  }
  list(
    observed = observed_recomputed,
    p = p_recomputed,
    two_sided = two_sided_recomputed
  )
}
recompute_maxT <- function(summary, null) {
  simulated <- as.matrix(null[, summary$feature, drop = FALSE])
  center <- colMeans(simulated, na.rm = TRUE)
  spread <- apply(simulated, 2, stats::sd, na.rm = TRUE)
  spread[!is.finite(spread) | spread <= 1e-12] <- 1
  null_z <- sweep(sweep(simulated, 2, center, "-"), 2, spread, "/")
  observed_z <- (
    summary$observed_case_control_difference - center
  ) / spread
  sign <- ifelse(summary$hypothesis_direction == "less", -1, 1)
  maximum_directional <- apply(
    sweep(null_z, 2, sign, "*"), 1, max, na.rm = TRUE
  )
  maximum_absolute <- apply(abs(null_z), 1, max, na.rm = TRUE)
  vapply(seq_len(nrow(summary)), function(index) {
    if (summary$hypothesis_direction[index] == "two_sided") {
      (1 + sum(
        maximum_absolute >= abs(observed_z[index]), na.rm = TRUE
      )) / (sum(is.finite(maximum_absolute)) + 1)
    } else {
      (1 + sum(
        maximum_directional >= observed_z[index] * sign[index],
        na.rm = TRUE
      )) / (sum(is.finite(maximum_directional)) + 1)
    }
  }, numeric(1))
}

landscape_recomputed <- recompute_summary(
  landscape_summary, landscape_null, pairs
)
add_check(
  "landscape_pair_statistics",
  close_enough(
    landscape_recomputed$observed,
    landscape_summary$observed_case_control_difference
  ) &&
    close_enough(
      landscape_recomputed$p,
      landscape_summary$directional_or_two_sided_p
    ) &&
    close_enough(
      landscape_recomputed$two_sided,
      landscape_summary$two_sided_p
    ),
  paste("pairs=", nrow(pairs), "features=", nrow(landscape_summary))
)
add_check(
  "landscape_maxT_statistics",
  close_enough(
    recompute_maxT(landscape_summary, landscape_null),
    landscape_summary$maxT_FWER_p
  ),
  paste("features=", nrow(landscape_summary))
)

raw_features <- v19_rf_predictors()
simulated <- as.matrix(landscape_null[, raw_features, drop = FALSE])
center <- colMeans(simulated)
covariance <- stats::cov(simulated)
eigenvalue <- eigen(
  covariance, symmetric = TRUE, only.values = TRUE
)$values
ridge <- max(max(eigenvalue) * 1e-6, 1e-8)
inverse <- solve(covariance + diag(ridge, ncol(covariance)))
simulated_centered <- sweep(simulated, 2, center, "-")
simulated_distance <- rowSums(
  (simulated_centered %*% inverse) * simulated_centered
)
observed_raw <- landscape_summary[
  match(raw_features, landscape_summary$feature),
  "observed_case_control_difference"
]
observed_centered <- observed_raw - center
observed_distance <- as.numeric(
  t(observed_centered) %*% inverse %*% observed_centered
)
observed_global_p <- (
  1 + sum(simulated_distance >= observed_distance)
) / (length(simulated_distance) + 1)
add_check(
  "landscape_global_statistics",
  close_enough(observed_distance, landscape_global$observed_value) &&
    close_enough(observed_global_p, landscape_global$empirical_p),
  paste(
    "Mahalanobis=", round(observed_distance, 4),
    "p=", round(observed_global_p, 4)
  )
)

checkpoint_root <-
  "results/ecological_v16_predictive_replication/checkpoints"
presence <- v18_align_result(
  readRDS(file.path(
    checkpoint_root,
    "national_environment_spde_presence_draws1000.rds"
  )),
  cells, "presence"
)
phenology <- v18_align_result(
  readRDS(file.path(
    checkpoint_root,
    "national_environment_year_spde_phenology_draws1000.rds"
  )),
  cells, "phenology"
)
intensity <- v18_align_result(
  readRDS(file.path(
    checkpoint_root,
    "national_environment_spde_intensity_draws1000.rds"
  )),
  cells, "intensity"
)
auxiliary_profile <- v18_profile(
  cells, presence, phenology, intensity
)
auxiliary_observed <- c(
  early_tail_depth = v18_pair_metric(
    pairs, auxiliary_profile$early_tail_depth
  ),
  dark_tail_depth = v18_pair_metric(
    pairs, auxiliary_profile$dark_tail_depth
  )
)
auxiliary_recomputed <- recompute_summary(
  auxiliary_summary, auxiliary_null
)
add_check(
  "auxiliary_facet_statistics",
  close_enough(
    auxiliary_observed[auxiliary_summary$feature],
    auxiliary_summary$observed_case_control_difference
  ) &&
    close_enough(
      auxiliary_recomputed$p,
      auxiliary_summary$directional_or_two_sided_p
    ),
  paste("features=", nrow(auxiliary_summary))
)
add_check(
  "auxiliary_maxT_statistics",
  close_enough(
    recompute_maxT(auxiliary_summary, auxiliary_null),
    auxiliary_summary$maxT_FWER_p
  ),
  paste("features=", nrow(auxiliary_summary))
)
add_check(
  "matching_uniqueness",
  !anyDuplicated(pairs$case_id) && !anyDuplicated(pairs$control_id) &&
    all(pairs$case_id %in% candidates$exact_site_id),
  paste("matched=", nrow(pairs), "of", nrow(candidates))
)
add_check(
  "held_out_human_and_auxiliary_axes",
  grepl("held out", metadata_value[["landscape_selection_role"]],
        fixed = TRUE) &&
    grepl("neither selects", metadata_value[["auxiliary_facet_role"]],
          fixed = TRUE),
  "human, land-use, early, and dark facets do not select candidates"
)
add_check(
  "no_residual_response",
  metadata_value[["residual_used_as_response"]] == "false",
  "local event and predictive-tail facets are used, not fitted residuals"
)
add_check(
  "final_Rmd_untouched",
  identical(
    unname(tools::md5sum("final.Rmd")),
    metadata_value[["final_Rmd_md5"]]
  ),
  paste("md5=", unname(tools::md5sum("final.Rmd")))
)

validation <- do.call(rbind, checks)
utils::write.csv(
  validation,
  file.path(output_dir, "local_isolate_independent_validation.csv"),
  row.names = FALSE
)
lines <- c(
  paste0(
    "# v20 local white-isolate independent validation: ",
    if (all(validation$status == "PASS")) "PASS" else "FAIL"
  ),
  "",
  vapply(seq_len(nrow(validation)), function(index) {
    paste0(
      "- **", validation$status[index], " - ",
      validation$check[index], "**: ", validation$detail[index]
    )
  }, character(1))
)
writeLines(
  lines, file.path(output_dir, "VALIDATION.md"), useBytes = TRUE
)
if (any(validation$status != "PASS")) {
  print(validation[validation$status != "PASS", ])
  stop("v20 validation failed.", call. = FALSE)
}
cat("v20 independent validation passed: ", nrow(validation), " checks\n")
