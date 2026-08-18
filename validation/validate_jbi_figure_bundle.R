#!/usr/bin/env Rscript
# Current integrated Broad + Bombus + human figure lock.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
hb_require_packages("digest")

output_dir <- hb_arg_value(args, "--output", "results/jbi_figure_bundle")
manifest_path <- file.path(output_dir, "figure_manifest.csv")
source_path <- file.path(output_dir, "figure_source_manifest.csv")
lock_path <- file.path(output_dir, "figure_numerical_lock.csv")

required <- c(
  manifest_path, source_path, lock_path,
  file.path(output_dir, "README.md"),
  file.path(output_dir, "figure_data", "panel_source_index.csv"),
  file.path(output_dir, "figure_data", "figure4_final_human_context_scenarios.csv"),
  file.path(output_dir, "figure_data", "figure4_current_broad_candidate_null.csv"),
  file.path(output_dir, "figure_data", "figure3_final8_environment_balance.csv"),
  file.path(output_dir, "figure_data", "figure4_continuous_cell_metrics.csv"),
  file.path(output_dir, "figure_data", "figure4_continuous_natural_summary.csv"),
  file.path(output_dir, "figure_data", "figure4_continuous_population_profile.csv")
)
missing <- required[!file.exists(required)]
if (length(missing)) stop("Missing figure-bundle outputs: ", paste(missing, collapse = ", "), call. = FALSE)

manifest <- hb_read_csv(manifest_path)
sources <- hb_read_csv(source_path)
lock <- hb_read_csv(lock_path)
checks <- list()
add_check <- function(check, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check, status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = as.character(detail), stringsAsFactors = FALSE
  )
}

add_check(
  "four_main_figures_two_formats",
  nrow(manifest) == 8L && identical(sort(unique(as.integer(manifest$figure))), 1:4) &&
    setequal(manifest$format, c("PNG", "PDF")) && all(table(manifest$figure) == 2L),
  paste("rows=", nrow(manifest), "figures=", paste(sort(unique(manifest$figure)), collapse = ","))
)

for (index in seq_len(nrow(manifest))) {
  path <- manifest$path[[index]]
  format <- manifest$format[[index]]
  exists <- file.exists(path)
  size <- if (exists) as.numeric(file.info(path)$size) else 0
  expected_hash <- as.character(manifest$sha256[[index]])
  actual_hash <- if (exists) unname(digest::digest(file = path, algo = "sha256")) else ""
  signature_ok <- FALSE
  if (exists && identical(format, "PNG")) {
    signature_ok <- identical(as.integer(readBin(path, what = "raw", n = 8L)), c(137L,80L,78L,71L,13L,10L,26L,10L))
  } else if (exists && identical(format, "PDF")) {
    signature_ok <- identical(rawToChar(readBin(path, what = "raw", n = 4L)), "%PDF")
  }
  add_check(
    paste0("figure_", manifest$figure[[index]], "_", tolower(format)),
    exists && is.finite(size) && size > 10000 && identical(actual_hash, expected_hash) && signature_ok,
    paste("path=", path, "size=", size, "hash_match=", identical(actual_hash, expected_hash), "signature=", signature_ok)
  )
}

source_exists <- file.exists(sources$path)
source_sizes <- ifelse(source_exists, as.numeric(file.info(sources$path)$size), NA_real_)
source_hashes <- vapply(seq_len(nrow(sources)), function(index) {
  if (!source_exists[[index]]) return("")
  unname(digest::digest(file = sources$path[[index]], algo = "sha256"))
}, character(1))
add_check(
  "source_manifest",
  all(source_exists) && all(source_sizes == as.numeric(sources$size_bytes)) && all(source_hashes == as.character(sources$sha256)),
  paste("sources=", nrow(sources), "missing=", sum(!source_exists), "hash_mismatches=", sum(source_hashes != as.character(sources$sha256)))
)

expected <- c(
  phenotype_observations = 1922,
  white_like_observations = 966,
  pigmented_observations = 956,
  cells_1km = 1305,
  pigmentation_boundary_a = 4.9687800109621,
  focal_pairs_5km = 67,
  focal_mean_contrast = 0.0358959,
  focal_median_contrast = -0.00277008,
  focal_positive_fraction = 0.4925373134,
  focal_signflip_p = 0.0271597284,
  candidate_count = 16,
  candidate_count_p = 0.278972102789721,
  candidate_fraction = 0.0407124681933842,
  candidate_fraction_p = 0.126087391260874,
  population_5km_current_broad_global_maxT_p = 0.0547945205479452,
  bombus_final8_selected_env_distance = 0.244081837853636,
  bombus_final8_all_edge_env_distance = 0.317519973222642,
  continuous_pigmented_cells = 674,
  continuous_white_cells = 631,
  continuous_pigmented_median_isolation_km = 3.60555127546399,
  continuous_raw_population_5km_pigmented_rho = 0.25197993694926,
  continuous_raw_population_5km_natural_mean = 0.132979942281226,
  continuous_raw_population_5km_upper_p = 0.0001999800019998,
  continuous_relative_population_5km_pigmented_rho = 0.285497536507115,
  continuous_relative_population_5km_natural_mean = 0.153615552893768,
  continuous_relative_population_5km_upper_p = 0.0008999100089991,
  continuous_raw_population_5km_direct_difference = 0.323523940415984,
  continuous_relative_population_5km_direct_difference = 0.20699177378826
)
lock_values <- setNames(as.numeric(lock$value), as.character(lock$metric))
missing_lock <- setdiff(names(expected), names(lock_values))
lock_difference <- if (!length(missing_lock)) abs(lock_values[names(expected)] - expected) else rep(Inf, length(expected))
lock_tolerance <- c(
  phenotype_observations=0, white_like_observations=0, pigmented_observations=0, cells_1km=0,
  pigmentation_boundary_a=1e-8, focal_pairs_5km=0, focal_mean_contrast=1e-5,
  focal_median_contrast=1e-5, focal_positive_fraction=1e-8, focal_signflip_p=1e-6,
  candidate_count=0, candidate_count_p=1e-10, candidate_fraction=1e-10, candidate_fraction_p=1e-10,
  population_5km_current_broad_global_maxT_p=1e-10,
  bombus_final8_selected_env_distance=1e-10, bombus_final8_all_edge_env_distance=1e-10,
  continuous_pigmented_cells=0, continuous_white_cells=0,
  continuous_pigmented_median_isolation_km=1e-10,
  continuous_raw_population_5km_pigmented_rho=1e-10,
  continuous_raw_population_5km_natural_mean=1e-10,
  continuous_raw_population_5km_upper_p=1e-12,
  continuous_relative_population_5km_pigmented_rho=1e-10,
  continuous_relative_population_5km_natural_mean=1e-10,
  continuous_relative_population_5km_upper_p=1e-12,
  continuous_raw_population_5km_direct_difference=1e-10,
  continuous_relative_population_5km_direct_difference=1e-10
)
add_check(
  "current_submission_numerical_lock",
  !length(missing_lock) && all(lock_difference <= lock_tolerance),
  paste("missing=", paste(missing_lock, collapse = ";"), "maximum_scaled_difference=",
        if (length(missing_lock)) "Inf" else max(lock_difference / pmax(lock_tolerance, 1e-15)))
)

human_final <- hb_read_csv(file.path(output_dir, "figure_data", "figure4_final_human_context_scenarios.csv"))
add_check(
  "figure4_current_broad_human_family",
  nrow(human_final) == 11L &&
    length(unique(as.character(human_final$configuration))) == 1L &&
    unique(as.character(human_final$configuration)) == "current_broad_primary_final8_model_final8_graph_caliper1" &&
    all(as.integer(human_final$n_null_draws) == 10000L) &&
    all(as.integer(human_final$observed_n_candidates) == 16L) &&
    abs(as.numeric(human_final$maxT_FWER_p[human_final$feature == "population_5km_rank"]) - 0.0547945205479452) < 1e-10,
  paste("rows=", nrow(human_final), "configs=", paste(unique(as.character(human_final$configuration)), collapse = ";"))
)


continuous_cells <- hb_read_csv(file.path(output_dir, "figure_data", "figure4_continuous_cell_metrics.csv"))
continuous_natural <- hb_read_csv(file.path(output_dir, "figure_data", "figure4_continuous_natural_summary.csv"))
continuous_primary <- continuous_natural[
  continuous_natural$metric == "relative_same_to_any_nn" &
    continuous_natural$feature == "population_5km_rank" &
    continuous_natural$component == "pigmented_rho" &
    continuous_natural$null_mode == "all_nondegenerate_maps",
  , drop = FALSE
]
add_check(
  "figure4_continuous_isolation_human_context",
  nrow(continuous_cells) == 1305L &&
    sum(as.character(continuous_cells$colour) == "pigmented") == 674L &&
    nrow(continuous_primary) == 1L &&
    abs(as.numeric(continuous_primary$observed_value) - 0.285497536507115) < 1e-10 &&
    abs(as.numeric(continuous_primary$null_mean) - 0.153615552893768) < 1e-10 &&
    abs(as.numeric(continuous_primary$empirical_p) - 0.0008999100089991) < 1e-12,
  if (nrow(continuous_primary)) sprintf(
    "cells=%d pigmented=%d rho=%.6f null=%.6f P=%.6f",
    nrow(continuous_cells),
    sum(as.character(continuous_cells$colour) == "pigmented"),
    as.numeric(continuous_primary$observed_value),
    as.numeric(continuous_primary$null_mean),
    as.numeric(continuous_primary$empirical_p)
  ) else "missing continuous primary row"
)

bombus_balance <- hb_read_csv(file.path(output_dir, "figure_data", "figure3_final8_environment_balance.csv"))
focal_balance <- bombus_balance[
  as.numeric(bombus_balance$radius_km) == 5 & abs(as.numeric(bombus_balance$transition_threshold) - 1) < 1e-12,
  , drop = FALSE
]
add_check(
  "figure3_final8_environment_balance",
  nrow(focal_balance) == 1L &&
    as.numeric(focal_balance$median_selected_final8) < as.numeric(focal_balance$median_all_local_edges_final8),
  if (nrow(focal_balance)) sprintf("selected=%.4f all=%.4f", as.numeric(focal_balance$median_selected_final8), as.numeric(focal_balance$median_all_local_edges_final8)) else "missing focal row"
)

add_check(
  "narrative_jobs_complete",
  all(nzchar(manifest$narrative_job)) && length(unique(manifest$narrative_job)) == 4L,
  paste("narrative_jobs=", length(unique(manifest$narrative_job)))
)

validation <- do.call(rbind, checks)
validation_path <- file.path(output_dir, "figure_bundle_validation.csv")
utils::write.csv(validation, validation_path, row.names = FALSE)
failed <- validation[validation$status == "FAIL", , drop = FALSE]
if (nrow(failed)) {
  print(failed)
  stop("JBI figure-bundle validation failed.", call. = FALSE)
}
cat("JBI figure-bundle validation passed: ", nrow(validation), " checks\n", sep = "")
