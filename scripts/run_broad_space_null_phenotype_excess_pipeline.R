args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", name, "="), "", hit[[1L]])
}

output_dir <- arg_value("--output", "results/broad_space_null_phenotype_excess")
cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
n_samples <- as.integer(arg_value("--samples", "500"))
seed <- as.integer(arg_value("--seed", "20260725"))
max_pairs_per_fold <- as.integer(arg_value("--max-pairs-per-fold", "15000"))
n_geo_bins <- as.integer(arg_value("--geo-bins", "5"))

scientific_outputs <- c(
  "primary_space_null_excess_test.csv",
  "matched_distance_stratum_contrasts.csv",
  "heldout_pair_space_null_excess.csv",
  "secondary_pair_diagnostics.csv",
  "heldout_space_null_predictions.csv"
)

source("scripts/fit_broad_space_null_phenotype_excess.R", local = .GlobalEnv)

missing_scientific <- scientific_outputs[
  !file.exists(file.path(output_dir, scientific_outputs))
]
if (length(missing_scientific)) {
  stop(
    "Broad spatial-null analysis did not create: ",
    paste(missing_scientific, collapse = ", "),
    call. = FALSE
  )
}

environment_terms <- c(
  "broad_pc1_climate_50km",
  "broad_pc1_aridity_50km",
  "broad_pc1_topography_50km",
  "within_pc1_climate_50km",
  "within_pc1_aridity_50km",
  "within_pc1_topography_50km"
)
metadata <- data.frame(
  field = c(
    "analysis", "source_cells", "space_null", "cross_fitting", "spatial_folds",
    "posterior_samples", "seed", "max_pairs_per_fold", "geographic_distance_bins",
    "environment_terms", "environment_distance", "primary_estimand", "claim_boundary"
  ),
  value = c(
    "broad_cross_fitted_space_null_phenotype_excess",
    cells_path,
    "intercept + Matern SPDE only",
    "each tested pair lies wholly inside a held-out geographical fold",
    "1;2;3;4;5",
    as.character(n_samples),
    as.character(seed),
    as.character(max_pairs_per_fold),
    as.character(n_geo_bins),
    paste(environment_terms, collapse = ";"),
    "Euclidean distance in the frozen response-blind environmental basis, scaled on each training fold",
    "mean across fold-by-geographic-distance strata of observed(high-environment - low-environment phenotype divergence), compared with the same statistic under cross-fitted space-only posterior prediction",
    "F_ST/P_ST-inspired non-genetic null test; not F_ST, P_ST, Q_ST, drift, selection, or local adaptation"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  metadata,
  file.path(output_dir, "analysis_metadata.csv"),
  row.names = FALSE
)

primary_path <- file.path(output_dir, "primary_space_null_excess_test.csv")
primary <- utils::read.csv(primary_path, check.names = FALSE, stringsAsFactors = FALSE)
if (nrow(primary) != 2L || !setequal(
  primary$response,
  c("pigmentation_state", "conditional_intensity")
)) {
  stop("Unexpected primary Broad spatial-null result table.", call. = FALSE)
}

row_for <- function(response) {
  row <- primary[primary$response == response, , drop = FALSE]
  if (nrow(row) != 1L) stop("Missing response: ", response, call. = FALSE)
  row
}
assert_close <- function(observed, expected, tolerance, label) {
  if (!is.finite(observed) || abs(observed - expected) > tolerance) {
    stop(
      label, " changed: expected ", expected, " +/- ", tolerance,
      ", observed ", observed,
      call. = FALSE
    )
  }
}

state <- row_for("pigmentation_state")
intensity <- row_for("conditional_intensity")
assert_close(state$observed_contrast, 0.106802, 5e-4, "state observed contrast")
assert_close(state$space_null_median, 0.058240, 5e-4, "state space-null median")
assert_close(
  state$phenotype_excess_over_space_null,
  0.048562,
  5e-4,
  "state excess over spatial null"
)
assert_close(
  state$posterior_predictive_p_upper,
  0.03393,
  5e-4,
  "state posterior-predictive p"
)
assert_close(
  intensity$posterior_predictive_p_upper,
  0.87226,
  5e-4,
  "intensity posterior-predictive p"
)
if (any(as.integer(primary$n_fold_geo_strata) != 25L)) {
  stop("Expected 25 fold-by-distance strata for both responses.", call. = FALSE)
}

summary_lines <- c(
  "# Broad cross-fitted space-null phenotype excess",
  "",
  sprintf(
    paste0(
      "- Pigmentation state: observed high-minus-low environmental divergence ",
      "%.6f; space-null median %.6f; excess %.6f; one-sided posterior-predictive p %.5f."
    ),
    state$observed_contrast,
    state$space_null_median,
    state$phenotype_excess_over_space_null,
    state$posterior_predictive_p_upper
  ),
  sprintf(
    paste0(
      "- Conditional intensity: observed contrast %.6f; space-null median %.6f; ",
      "excess %.6f; one-sided posterior-predictive p %.5f."
    ),
    intensity$observed_contrast,
    intensity$space_null_median,
    intensity$phenotype_excess_over_space_null,
    intensity$posterior_predictive_p_upper
  ),
  "",
  paste0(
    "Interpretation: pigmentation-state divergence exceeds the cross-fitted spatial ",
    "expectation along environmental difference, whereas conditional intensity does not."
  ),
  paste0(
    "Claim boundary: this is environmental alignment beyond a fitted spatial expectation; ",
    "it does not establish selection, local adaptation, or a unique causal environmental mechanism."
  )
)
writeLines(summary_lines, file.path(output_dir, "RESULT_SUMMARY.md"))
message("Validated and locked the Broad spatial-null phenotype-excess result.")
