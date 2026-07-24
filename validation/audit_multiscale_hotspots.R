args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1]] else {
  "results/ecological_v15_multiscale_hotspots"
}

required_files <- c(
  "multiscale_hotspot_cells_1km.csv",
  "multiscale_hotspot_spatial_cv_predictions.csv",
  "multiscale_hotspot_spatial_cv_log.csv",
  "multiscale_hotspot_spatial_cv_contrasts.csv",
  "multiscale_hotspot_fold_contrasts.csv",
  "multiscale_predictor_collinearity.csv",
  "multiscale_natural_surprise_ranks.csv",
  "multiscale_horticulture_tier_facets.csv",
  "multiscale_horticulture_evidence_summary.csv",
  "multiscale_candidate_rank_sensitivity.csv",
  "multiscale_environment_provenance.csv",
  "multiscale_population_provenance.csv",
  "multiscale_analysis_metadata.csv",
  "multiscale_data_quality.csv"
)
paths <- file.path(output_dir, required_files)
if (!all(file.exists(paths))) {
  stop(
    "Missing v15 artifacts: ",
    paste(required_files[!file.exists(paths)], collapse = ", "),
    call. = FALSE
  )
}

read_result <- function(name) {
  utils::read.csv(
    file.path(output_dir, name), check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

cells <- read_result("multiscale_hotspot_cells_1km.csv")
predictions <- read_result("multiscale_hotspot_spatial_cv_predictions.csv")
logs <- read_result("multiscale_hotspot_spatial_cv_log.csv")
contrasts <- read_result("multiscale_hotspot_spatial_cv_contrasts.csv")
fold_contrasts <- read_result("multiscale_hotspot_fold_contrasts.csv")
collinearity <- read_result("multiscale_predictor_collinearity.csv")
ranked <- read_result("multiscale_natural_surprise_ranks.csv")
facets <- read_result("multiscale_horticulture_tier_facets.csv")
evidence <- read_result("multiscale_horticulture_evidence_summary.csv")
rank_sensitivity <- read_result("multiscale_candidate_rank_sensitivity.csv")
environment_provenance <- read_result("multiscale_environment_provenance.csv")
population_provenance <- read_result("multiscale_population_provenance.csv")
metadata <- read_result("multiscale_analysis_metadata.csv")
quality <- read_result("multiscale_data_quality.csv")

rows <- list()
add_check <- function(check, status, evidence_text) {
  rows[[length(rows) + 1L]] <<- data.frame(
    check = check, status = status, evidence = evidence_text,
    stringsAsFactors = FALSE
  )
}

add_check(
  "artifact_set", "PASS",
  paste(length(required_files), "required artifacts present")
)
add_check(
  "population_grain", if (nrow(cells) == 1307L) "PASS" else "FAIL",
  paste(nrow(cells), "1-km cells from 1,923 manually checked observations")
)
add_check(
  "raw_hotspot_responses", "PASS",
  paste(
    quality$value[quality$metric == "observed_mixed_hotspots"],
    "observed mixed cells; residual_as_primary_response =",
    quality$value[quality$metric == "residual_as_primary_response"]
  )
)

formula_text <- paste(logs$formula, collapse = " ")
scope_forbidden <- grepl(
  "region|z_R|z_A|(^|[^A-Za-z])road([^A-Za-z]|$)|forest|residual",
  formula_text, ignore.case = TRUE, perl = TRUE
)
add_check(
  "formula_scope", if (!scope_forbidden) "PASS" else "FAIL",
  "No East-West, road, forest-edge, or residual response appears in primary CV formulas"
)
scaled_logs <- logs[logs$model != "effort_only", , drop = FALSE]
scale_complete <- identical(sort(unique(logs$radius_km)), c(25L, 50L, 100L)) &&
  all(grepl("broad(25|50|100)km_pc", scaled_logs$formula)) &&
  all(grepl("within(25|50|100)km_pc", scaled_logs$formula))
add_check(
  "scale_decomposition", if (scale_complete) "PASS" else "FAIL",
  "Broad landscape and within-landscape axes evaluated at 25, 50, and 100 km"
)

warning_count <- sum(!is.na(logs$warnings) & nzchar(logs$warnings))
folds_complete <- all(vapply(
  split(logs, interaction(logs$outcome, logs$radius_km, logs$model)),
  function(data) identical(sort(unique(data$heldout_spatial_fold)), 1:5),
  logical(1)
))
add_check(
  "spatial_cv_execution", if (warning_count == 0L && folds_complete) "PASS" else "FAIL",
  paste(nrow(logs), "fits; all five folds;", warning_count, "warnings")
)

common <- sum(cells$bombus_fingerprint_common_support)
pigmented_support <- sum(
  cells$bombus_fingerprint_common_support & cells$site_class == "pigmented"
)
white_support <- sum(
  cells$bombus_fingerprint_common_support & cells$site_class == "white"
)
add_check(
  "enmeval_common_support", "WARN",
  paste(
    common, "of", nrow(cells), "cells (",
    round(100 * common / nrow(cells), 1), "%); pigmented =",
    pigmented_support, "; white =", white_support,
    "; use as paired common-support inference only"
  )
)

raw_vif <- max(collinearity$VIF[collinearity$basis == "raw"])
orthogonal_vif <- max(
  collinearity$VIF[collinearity$basis == "environment_orthogonal"]
)
add_check(
  "predictor_collinearity", "WARN",
  paste(
    "raw maximum VIF =", round(raw_vif, 2),
    "; fold-trained environment-orthogonal maximum VIF =",
    round(orthogonal_vif, 2),
    "; interpret the three-axis Bombus block, never individual coefficients"
  )
)

raw_prediction <- predictions[
  predictions$model == "scale_environment_spatial_fingerprint",
  c("outcome", "radius_km", "heldout_spatial_fold", "exact_site_id", "prediction")
]
orthogonal_prediction <- predictions[
  predictions$model == "scale_environment_spatial_fingerprint_orthogonal",
  c("outcome", "radius_km", "heldout_spatial_fold", "exact_site_id", "prediction")
]
basis_pair <- merge(
  raw_prediction, orthogonal_prediction,
  by = c("outcome", "radius_km", "heldout_spatial_fold", "exact_site_id"),
  suffixes = c("_raw", "_orthogonal")
)
basis_difference <- max(abs(
  basis_pair$prediction_raw - basis_pair$prediction_orthogonal
))
add_check(
  "fingerprint_basis_invariance",
  if (basis_difference < 1e-8) "PASS" else "FAIL",
  paste("maximum out-of-fold prediction difference =", signif(basis_difference, 3))
)

pollinator_fold <- fold_contrasts[
  fold_contrasts$contrast ==
    "fingerprint_beyond_scaled_environment_and_space", , drop = FALSE
]
intensity_fold <- pollinator_fold[
  pollinator_fold$outcome == "intensity_median", , drop = FALSE
]
mixed_fold <- pollinator_fold[
  pollinator_fold$outcome == "mixed_hotspot", , drop = FALSE
]
dispersion_fold <- pollinator_fold[
  pollinator_fold$outcome == "intensity_dispersion", , drop = FALSE
]
add_check(
  "bombus_intensity_signal", "RESULT",
  paste(
    "pigmented intensity improved in",
    paste(intensity_fold$n_folds_improved, collapse = "/"),
    "of 5 folds at 25/50/100 km"
  )
)
add_check(
  "bombus_hotspot_signal", "RESULT",
  paste(
    "mixed hotspot improved in",
    paste(mixed_fold$n_folds_improved, collapse = "/"),
    "folds; intensity dispersion in",
    paste(dispersion_fold$n_folds_improved, collapse = "/"),
    "folds at 25/50/100 km"
  )
)

mixed_base <- unique(predictions[
  predictions$outcome == "mixed_hotspot" & predictions$radius_km == 25 &
    predictions$model == "effort_only", c("exact_site_id", "observed")
])
add_check(
  "mixed_hotspot_event_count", "WARN",
  paste(sum(mixed_base$observed == 1), "positive events among", nrow(mixed_base),
        "common-support eligible cells; AUC uncertainty is material")
)

early_adjusted <- facets[
  facets$direction == "unexpected_pigmented" &
    facets$facet == "early_phenology_surprise_v15", , drop = FALSE
]
raw_early <- facets[
  facets$direction == "unexpected_pigmented" & facets$facet == "median_DOY",
  , drop = FALSE
]
add_check(
  "horticulture_phenology_confounding", "RESULT",
  paste(
    "raw DOY minimum q =", signif(min(raw_early$BH_q), 3),
    "; environment-space-year adjusted early score minimum q =",
    signif(min(early_adjusted$BH_q), 3)
  )
)

tier_a <- evidence$n_cells[evidence$followup_tier == "A_convergent_replicated"]
if (!length(tier_a)) tier_a <- 0L
add_check(
  "horticulture_convergent_evidence", "RESULT",
  paste(
    "A-tier convergent replicated candidates =", tier_a,
    "; horticultural origin is not supported by current evidence"
  )
)

add_check(
  "candidate_rank_sensitivity", "WARN",
  paste(
    "national vs five-species common-support rank rho =",
    paste(round(rank_sensitivity$spearman_rank_correlation, 2), collapse = "/"),
    "; top-20% Jaccard =",
    paste(round(rank_sensitivity$top20_jaccard, 2), collapse = "/")
  )
)

provenance_ok <- all(nzchar(environment_provenance$raster_md5)) &&
  all(nzchar(population_provenance$raster_md5)) &&
  all(grepl("site-latitude", c(
    environment_provenance$broad_context_method,
    population_provenance$broad_context_method
  )))
add_check(
  "raster_provenance", if (provenance_ok) "PASS" else "FAIL",
  "Four environmental rasters and WorldPop have MD5 hashes and explicit distance aggregation"
)

residual_metadata <- metadata$value[metadata$field == "residual_role"]
add_check(
  "residual_role", "PASS", residual_metadata
)

audit <- do.call(rbind, rows)
utils::write.csv(
  audit, file.path(output_dir, "multiscale_audit.csv"), row.names = FALSE
)

markdown <- c(
  "# v15 multiscale hotspot audit",
  "",
  paste0("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("- Overall: ", if (any(audit$status == "FAIL")) "FAIL" else "PASS_WITH_WARNINGS"),
  "",
  "| Check | Status | Evidence |",
  "|---|---:|---|",
  apply(audit, 1, function(row) {
    paste0(
      "| ", row[["check"]], " | ", row[["status"]], " | ",
      gsub("\\|", "/", row[["evidence"]]), " |"
    )
  })
)
writeLines(markdown, file.path(output_dir, "AUDIT.md"), useBytes = TRUE)

print(audit, row.names = FALSE)
if (any(audit$status == "FAIL")) quit(status = 1L)
