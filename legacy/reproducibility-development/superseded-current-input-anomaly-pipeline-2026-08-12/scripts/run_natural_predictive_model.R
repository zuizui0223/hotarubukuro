args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
as_bool <- hb_as_bool

# Stage 02 is the flower-colour natural reference only. Bombus is intentionally
# absent here and enters the active paper only in the local stage-03 limitation
# test. `--components` is retained for the high-draw submission presence lock.
all_components <- c(
  "national_environment_spde_presence",
  "national_environment_spde_intensity"
)
requested_components <- arg_value("--components", "all")
selected_components <- if (identical(tolower(requested_components), "all")) {
  all_components
} else {
  trimws(strsplit(requested_components, ",", fixed = TRUE)[[1L]])
}
unknown_components <- setdiff(selected_components, all_components)
if (length(unknown_components)) {
  stop(
    "Unknown --components values: ", paste(unknown_components, collapse = ", "),
    ". Valid components: ", paste(all_components, collapse = ", "),
    call. = FALSE
  )
}
if (!length(selected_components)) {
  stop("--components selected no model components.", call. = FALSE)
}
partial_scope <- !setequal(selected_components, all_components)

input_observations <- arg_value(
  "--observations",
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
)
input_cells <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
output_dir <- arg_value(
  "--output", "results/ecological_v16_predictive_replication"
)
n_draws <- as.integer(arg_value("--draws", "1000"))
force_rerun <- as_bool(arg_value("--force", "false"))
seed <- as.integer(arg_value("--seed", "20260725"))

if (!is.finite(n_draws) || n_draws < 100L) {
  stop("--draws must be at least 100.", call. = FALSE)
}

hb_require_stage_packages("natural_predictive_model")
hb_load_modules("natural_predictive_model")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
checkpoint_dir <- file.path(output_dir, "checkpoints")
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
observations <- utils::read.csv(
  input_observations, check.names = FALSE, stringsAsFactors = FALSE
)
cells <- utils::read.csv(
  input_cells, check.names = FALSE, stringsAsFactors = FALSE
)
cells$median_year_centered <- cells$median_year - 2024
cells$median_year_centered_squared <- cells$median_year_centered^2

quality <- v16_data_quality(observations, cells)
utils::write.csv(
  quality, file.path(output_dir, "predictive_replication_data_quality.csv"),
  row.names = FALSE
)

run_or_load <- function(model, expression) {
  if (!model %in% selected_components) {
    message("[v16] component not selected; skipping: ", model)
    return(NULL)
  }
  path <- file.path(checkpoint_dir, paste0(model, "_draws", n_draws, ".rds"))
  if (!force_rerun && file.exists(path)) {
    message("[v16] loading checkpoint: ", path)
    result <- readRDS(path)
    compatible_previous_component <- identical(
      result$analysis_spec_version, "v16.4_apredictor_projection"
    )
    if ((identical(
      result$analysis_spec_version, v16_analysis_spec_version
    ) || compatible_previous_component) &&
        identical(result$model, model) && ncol(result$draws) == n_draws) {
      if (compatible_previous_component) {
        message(
          "[v16] reusing unchanged v16.4 component under v16.5: ", model
        )
      }
      return(result)
    }
    message("[v16] checkpoint specification is stale; refitting: ", path)
  }
  result <- base::force(expression)
  rp_save_rds_atomic(result, path)
  result
}

environment_terms <- v16_environment_terms(50)

national_presence <- run_or_load(
  "national_environment_spde_presence",
  v16_crossfit_spde(
    cells, response = "n_pigmented", family = "binomial",
    environment_terms = environment_terms, trials = "n_observations",
    model = "national_environment_spde_presence",
    n_draws = n_draws, seed = seed
  )
)

national_intensity <- run_or_load(
  "national_environment_spde_intensity",
  v16_crossfit_spde(
    cells, response = "conditional_intensity_median", family = "gaussian",
    environment_terms = environment_terms,
    training_eligible = is.finite(cells$conditional_intensity_median),
    model = "national_environment_spde_intensity",
    n_draws = n_draws, seed = seed + 100000L
  )
)

all_results <- list(
  national_environment_spde_presence = national_presence,
  national_environment_spde_intensity = national_intensity
)
fitted_components <- names(all_results)[!vapply(all_results, is.null, logical(1))]
component_scope <- data.frame(
  field = c(
    "requested_components", "fitted_components", "skipped_components",
    "component_scope", "n_predictive_draws", "random_seed", "generated_utc",
    "commit", "pollinator_scope"
  ),
  value = c(
    paste(selected_components, collapse = ";"),
    paste(fitted_components, collapse = ";"),
    paste(setdiff(all_components, fitted_components), collapse = ";"),
    if (partial_scope) "partial" else "complete",
    n_draws, seed, format(Sys.time(), tz = "UTC", usetz = TRUE),
    rp_git_commit(), "Bombus excluded from national stage; local stage 03 only"
  ),
  stringsAsFactors = FALSE
)
rp_write_csv_atomic(
  component_scope,
  file.path(output_dir, "predictive_replication_component_scope.csv")
)
rp_write_csv_atomic(
  data.frame(
    model = fitted_components,
    n_cells = vapply(
      all_results[fitted_components], function(x) nrow(x$draws), integer(1)
    ),
    n_draws = vapply(
      all_results[fitted_components], function(x) ncol(x$draws), integer(1)
    ),
    checkpoint = file.path(
      "checkpoints", paste0(fitted_components, "_draws", n_draws, ".rds")
    ),
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "predictive_replication_component_checkpoints.csv")
)

if (partial_scope) {
  message(
    "[v16] partial component scope: wrote checkpoints for ",
    paste(fitted_components, collapse = ", "),
    "; full natural-model summary tables were skipped."
  )
  cat("v16 partial component run complete: ", output_dir, "\n", sep = "")
  quit(save = "no", status = 0)
}

# A restored snapshot can contain outputs from the superseded national-Bombus
# experiment. Remove them so a fresh active run cannot expose stale evidence.
stale_files <- c(
  "predictive_replication_bombus_paired_contrast.csv",
  "predictive_replication_fingerprint_orthogonalization.csv"
)
unlink(file.path(output_dir, stale_files), force = TRUE)
unlink(
  Sys.glob(file.path(checkpoint_dir, "common_support_environment_spde*")),
  force = TRUE
)

model_logs <- do.call(rbind, lapply(all_results, `[[`, "log"))
utils::write.csv(
  model_logs, file.path(output_dir, "predictive_replication_model_log.csv"),
  row.names = FALSE
)

performance <- do.call(rbind, list(
  v16_model_performance(
    national_presence, cells, "n_pigmented", "binomial", "n_observations"
  ),
  v16_model_performance(
    national_intensity, cells, "conditional_intensity_median", "gaussian"
  )
))
utils::write.csv(
  performance,
  file.path(output_dir, "predictive_replication_model_performance.csv"),
  row.names = FALSE
)

presence_calibration <- v16_presence_calibration(national_presence, cells)
utils::write.csv(
  presence_calibration,
  file.path(output_dir, "predictive_replication_presence_calibration.csv"),
  row.names = FALSE
)

fold_performance <- do.call(rbind, list(
  v16_fold_performance(
    national_presence, cells, "n_pigmented", "binomial", "n_observations"
  ),
  v16_fold_performance(
    national_intensity, cells, "conditional_intensity_median", "gaussian"
  )
))
utils::write.csv(
  fold_performance,
  file.path(output_dir, "predictive_replication_model_fold_performance.csv"),
  row.names = FALSE
)

national_null <- v16_candidate_null(
  national_presence, cells, national_intensity
)
null_summary <- national_null$summary
null_summary$BH_q_global <- stats::p.adjust(
  null_summary$empirical_p, method = "BH"
)
utils::write.csv(
  null_summary,
  file.path(output_dir, "predictive_replication_candidate_null_summary.csv"),
  row.names = FALSE
)

annotate_scores <- function(null_result, cell_data) {
  score <- null_result$scores
  index <- match(score$exact_site_id, cell_data$exact_site_id)
  additions <- cell_data[index, c(
    "longitude", "latitude", "x_km", "y_km", "spatial_fold",
    "n_exact_sites", "n_independent_sites", "n_years",
    "conditional_intensity_median", "median_DOY", "median_year",
    "log_population_sum_25km"
  ), drop = FALSE]
  score <- cbind(score, additions)
  score$local_colour_isolation <- null_result$observed_local_isolation
  score$conditional_intensity_surprise <- null_result$observed_intensity_surprise
  score
}

cell_scores <- annotate_scores(national_null, cells)
utils::write.csv(
  cell_scores,
  file.path(output_dir, "predictive_replication_cell_candidate_scores.csv"),
  row.names = FALSE
)

candidate_followup <- cell_scores[
  cell_scores$unexpected_pigmented_top20 %in% TRUE |
    cell_scores$unexpected_white_top20 %in% TRUE,
  , drop = FALSE
]
utils::write.csv(
  candidate_followup,
  file.path(output_dir, "predictive_replication_candidate_followup.csv"),
  row.names = FALSE
)

# Cross-model rank sensitivity is not an active estimand once the national
# Bombus comparator is removed. Keep an explicit not-applicable artifact so the
# reason is machine-readable rather than silently omitting the historical file.
rank_sensitivity <- data.frame(
  status = "not_applicable",
  reason = "single active pigmentation-presence natural model; no national Bombus comparator",
  stringsAsFactors = FALSE
)
utils::write.csv(
  rank_sensitivity,
  file.path(output_dir, "predictive_replication_candidate_rank_sensitivity.csv"),
  row.names = FALSE
)

stability <- v16_simulation_stability(national_presence, cells)
utils::write.csv(
  stability,
  file.path(output_dir, "predictive_replication_simulation_stability.csv"),
  row.names = FALSE
)

draw_manifest <- data.frame(
  model = names(all_results),
  checkpoint_analysis_spec_version = vapply(
    all_results, `[[`, character(1), "analysis_spec_version"
  ),
  n_cells = vapply(all_results, function(x) nrow(x$draws), integer(1)),
  n_draws = vapply(all_results, function(x) ncol(x$draws), integer(1)),
  checkpoint = file.path(
    "checkpoints", paste0(names(all_results), "_draws", n_draws, ".rds")
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  draw_manifest,
  file.path(output_dir, "predictive_replication_draw_manifest.csv"),
  row.names = FALSE
)

metadata <- data.frame(
  field = c(
    "analysis_version", "analysis_spec_version", "generated_at",
    "n_predictive_draws", "random_seed",
    "candidate_unit", "presence_likelihood", "intensity_likelihood",
    "spatial_validation", "natural_primary_model", "pollinator_scope",
    "candidate_selection_inputs", "AUC_definition",
    "heldout_candidate_facets", "white_intensity_modelled",
    "residual_as_primary_response", "simulation_map_type",
    "cross_fold_joint_dependence", "horticultural_claim_ceiling"
  ),
  value = c(
    "v16_natural_reference_only", v16_analysis_spec_version,
    format(Sys.time(), tz = "Asia/Tokyo"), n_draws, seed,
    "1-km cell with observed trial count fixed",
    "binomial n_pigmented out of n_observations",
    "Gaussian conditional on at least one observed pigmented flower",
    "five response-blind 100-km spatial folds; SPDE constrained to zero mean at training locations within each fold",
    "nationwide environment plus SPDE",
    "Bombus excluded from stage 02; pollinator inference begins at local stage 03",
    "cross-fitted one-sided presence predictive tail only",
    "trial-weighted image-level discrimination after aggregation; cell-level any and majority AUC are also reported",
    "population context; conditional intensity; natural-adjusted DOY; local isolation; observation effort",
    "false", "false",
    "cross-fitted predictive mosaic at observed sampling cells",
    "joint within heldout fold; folds combined as independent predictive mosaics",
    "follow-up priority; not horticultural origin or gene flow evidence"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  metadata,
  file.path(output_dir, "predictive_replication_metadata.csv"),
  row.names = FALSE
)

writeLines(
  c(
    "# v16 cross-fitted natural flower-colour reference",
    "",
    paste0("Generated with ", n_draws, " predictive draws per spatial-fold model."),
    "",
    "Stage 02 contains only the two flower-colour natural models: pigmentation presence and pigmented-only conditional intensity, each modelled with environment plus an SPDE field. Predicted Bombus variables are not fitted, compared, or scored nationally; the active pollinator hypothesis begins only in the local stage-03 limitation gate.",
    "",
    "The primary candidate score is a one-sided cross-fitted predictive tail probability for pigmented counts in a 1-km cell. Population, conditional intensity, and local isolation are not used to select candidates. Their observed cross-fitted facet maps are held fixed while the same candidate extraction is applied to each predictive replicate map; separate response models are not treated as a joint posterior merely by sharing a draw index.",
    "",
    "Predictive maps are generated only at observed sampling cells with observed trial counts fixed. They are not occurrence or flower-colour maps for unsampled Japan. Candidate departures do not establish horticultural origin; genetics, provenance, and field verification remain necessary."
  ),
  file.path(output_dir, "README.md")
)

cat("Completed v16 natural predictive reference: ", output_dir, "\n", sep = "")
print(performance)
