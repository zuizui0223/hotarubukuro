#!/usr/bin/env Rscript

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
  file.path(output_dir, "figure_data", "figure4_final_human_context_scenarios.csv")
)
missing <- required[!file.exists(required)]
if (length(missing)) {
  stop("Missing figure-bundle outputs: ", paste(missing, collapse = ", "), call. = FALSE)
}

manifest <- hb_read_csv(manifest_path)
sources <- hb_read_csv(source_path)
lock <- hb_read_csv(lock_path)

checks <- list()
add_check <- function(check, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check,
    status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

add_check(
  "four_main_figures_two_formats",
  nrow(manifest) == 8L &&
    identical(sort(unique(as.integer(manifest$figure))), 1:4) &&
    setequal(manifest$format, c("PNG", "PDF")) &&
    all(table(manifest$figure) == 2L),
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
    signature <- readBin(path, what = "raw", n = 8L)
    signature_ok <- identical(
      as.integer(signature), c(137L, 80L, 78L, 71L, 13L, 10L, 26L, 10L)
    )
  } else if (exists && identical(format, "PDF")) {
    signature <- rawToChar(readBin(path, what = "raw", n = 4L))
    signature_ok <- identical(signature, "%PDF")
  }
  add_check(
    paste0("figure_", manifest$figure[[index]], "_", tolower(format)),
    exists && is.finite(size) && size > 10000 &&
      identical(actual_hash, expected_hash) && signature_ok,
    paste(
      "path=", path, "size=", size,
      "hash_match=", identical(actual_hash, expected_hash),
      "signature=", signature_ok
    )
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
  all(source_exists) &&
    all(source_sizes == as.numeric(sources$size_bytes)) &&
    all(source_hashes == as.character(sources$sha256)),
  paste(
    "sources=", nrow(sources),
    "missing=", sum(!source_exists),
    "hash_mismatches=", sum(source_hashes != as.character(sources$sha256))
  )
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
  candidate_count = 17,
  candidate_count_p = 0.1995800420,
  candidate_fraction = 0.0473537604,
  candidate_fraction_p = 0.0873912609,
  population_5km_primary_global_maxT_p = 0.205879412058794,
  population_5km_primary_final8model_global_maxT_p = 0.236576342365763,
  population_5km_final8match_currentmodel_global_maxT_p = 0.0462953704629537,
  population_5km_final8match_final8model_global_maxT_p = 0.0551944805519448
)
lock_values <- setNames(as.numeric(lock$value), as.character(lock$metric))
missing_lock <- setdiff(names(expected), names(lock_values))
lock_difference <- if (!length(missing_lock)) {
  abs(lock_values[names(expected)] - expected)
} else rep(Inf, length(expected))
lock_tolerance <- c(
  phenotype_observations = 0,
  white_like_observations = 0,
  pigmented_observations = 0,
  cells_1km = 0,
  pigmentation_boundary_a = 1e-8,
  focal_pairs_5km = 0,
  focal_mean_contrast = 1e-5,
  focal_median_contrast = 1e-5,
  focal_positive_fraction = 1e-8,
  focal_signflip_p = 1e-6,
  candidate_count = 0,
  candidate_count_p = 1e-6,
  candidate_fraction = 1e-6,
  candidate_fraction_p = 1e-6,
  population_5km_primary_global_maxT_p = 1e-10,
  population_5km_primary_final8model_global_maxT_p = 1e-10,
  population_5km_final8match_currentmodel_global_maxT_p = 1e-10,
  population_5km_final8match_final8model_global_maxT_p = 1e-10
)
add_check(
  "current_submission_numerical_lock",
  !length(missing_lock) && all(lock_difference <= lock_tolerance),
  paste(
    "missing=", paste(missing_lock, collapse = ";"),
    "maximum_scaled_difference=",
    if (length(missing_lock)) "Inf" else max(lock_difference / pmax(lock_tolerance, 1e-15))
  )
)

human_final <- hb_read_csv(
  file.path(output_dir, "figure_data", "figure4_final_human_context_scenarios.csv")
)
expected_configurations <- c(
  "current_model_current_graph",
  "current_model_final8_support_matched",
  "final8_model_current_graph",
  "final8_model_final8_support_matched"
)
add_check(
  "figure4_final_human_context_scenarios",
  nrow(human_final) == 4L &&
    setequal(as.character(human_final$configuration), expected_configurations) &&
    all(as.integer(human_final$n_null_draws) == 10000L) &&
    all(as.integer(human_final$n_candidates) == 17L),
  paste(
    "rows=", nrow(human_final),
    "configs=", paste(as.character(human_final$configuration), collapse = ";")
  )
)

add_check(
  "narrative_jobs_complete",
  all(nzchar(manifest$narrative_job)) &&
    length(unique(manifest$narrative_job)) == 4L,
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
