args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
as_bool <- hb_as_bool

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
cells$bombus_fingerprint_common_support <- as.logical(
  cells$bombus_fingerprint_common_support
)

quality <- v16_data_quality(observations, cells)
utils::write.csv(
  quality, file.path(output_dir, "predictive_replication_data_quality.csv"),
  row.names = FALSE
)

run_or_load <- function(model, expression) {
  path <- file.path(checkpoint_dir, paste0(model, "_draws", n_draws, ".rds"))
  if (!force_rerun && file.exists(path)) {
    message("[v16] loading checkpoint: ", path)
    result <- readRDS(path)
    compatible_previous_component <-
      model != "national_environment_year_spde_phenology" &&
      identical(
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
  saveRDS(result, path, compress = "gzip")
  result
}

environment_terms <- v16_environment_terms(50)
fingerprint_terms <- v16_fingerprint_terms()

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

national_phenology <- run_or_load(
  "national_environment_year_spde_phenology",
  v16_crossfit_spde(
    cells, response = "median_DOY", family = "gaussian",
    environment_terms = c(
      environment_terms, "median_year_centered",
      "median_year_centered_squared"
    ),
    training_eligible = is.finite(cells$median_DOY),
    model = "national_environment_year_spde_phenology",
    n_draws = n_draws, seed = seed + 200000L
  )
)

common <- cells[
  cells$bombus_fingerprint_common_support &
    stats::complete.cases(cells[c(environment_terms, fingerprint_terms)]),
  , drop = FALSE
]

common_reference <- run_or_load(
  "common_support_environment_spde_presence",
  v16_crossfit_spde(
    common, response = "n_pigmented", family = "binomial",
    environment_terms = environment_terms, trials = "n_observations",
    model = "common_support_environment_spde_presence",
    n_draws = n_draws, seed = seed + 300000L
  )
)

common_fingerprint <- run_or_load(
  "common_support_environment_spde_bombus_presence",
  v16_crossfit_spde(
    common, response = "n_pigmented", family = "binomial",
    environment_terms = environment_terms,
    fingerprint_terms = fingerprint_terms, trials = "n_observations",
    model = "common_support_environment_spde_bombus_presence",
    n_draws = n_draws, seed = seed + 400000L
  )
)

all_results <- list(
  national_environment_spde_presence = national_presence,
  national_environment_spde_intensity = national_intensity,
  national_environment_year_spde_phenology = national_phenology,
  common_support_environment_spde_presence = common_reference,
  common_support_environment_spde_bombus_presence = common_fingerprint
)

model_logs <- do.call(rbind, lapply(all_results, `[[`, "log"))
utils::write.csv(
  model_logs, file.path(output_dir, "predictive_replication_model_log.csv"),
  row.names = FALSE
)

orthogonalization <- do.call(rbind, lapply(all_results, function(result) {
  if (nrow(result$orthogonalization)) result$orthogonalization else NULL
}))
if (is.null(orthogonalization)) orthogonalization <- data.frame()
utils::write.csv(
  orthogonalization,
  file.path(output_dir, "predictive_replication_fingerprint_orthogonalization.csv"),
  row.names = FALSE
)

performance <- do.call(rbind, list(
  v16_model_performance(
    national_presence, cells, "n_pigmented", "binomial", "n_observations"
  ),
  v16_model_performance(
    national_intensity, cells, "conditional_intensity_median", "gaussian"
  ),
  v16_model_performance(
    national_phenology, cells, "median_DOY", "gaussian"
  ),
  v16_model_performance(
    common_reference, common, "n_pigmented", "binomial", "n_observations"
  ),
  v16_model_performance(
    common_fingerprint, common, "n_pigmented", "binomial", "n_observations"
  )
))
utils::write.csv(
  performance,
  file.path(output_dir, "predictive_replication_model_performance.csv"),
  row.names = FALSE
)

presence_calibration <- do.call(rbind, list(
  v16_presence_calibration(national_presence, cells),
  v16_presence_calibration(common_reference, common),
  v16_presence_calibration(common_fingerprint, common)
))
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
  ),
  v16_fold_performance(
    national_phenology, cells, "median_DOY", "gaussian"
  ),
  v16_fold_performance(
    common_reference, common, "n_pigmented", "binomial", "n_observations"
  ),
  v16_fold_performance(
    common_fingerprint, common, "n_pigmented", "binomial", "n_observations"
  )
))
utils::write.csv(
  fold_performance,
  file.path(output_dir, "predictive_replication_model_fold_performance.csv"),
  row.names = FALSE
)

bombus_reference <- fold_performance[
  fold_performance$model == "common_support_environment_spde_presence", ]
bombus_added <- fold_performance[
  fold_performance$model ==
    "common_support_environment_spde_bombus_presence", ]
bombus_paired <- merge(
  bombus_reference, bombus_added, by = "heldout_spatial_fold",
  suffixes = c("_reference", "_bombus")
)
bombus_paired$predictive_mass_improvement <-
  bombus_paired$primary_value_reference - bombus_paired$primary_value_bombus
bombus_paired$RMSE_improvement <-
  bombus_paired$RMSE_reference - bombus_paired$RMSE_bombus
bombus_paired$Brier_improvement <-
  bombus_paired$Bernoulli_Brier_score_reference -
    bombus_paired$Bernoulli_Brier_score_bombus
bombus_paired$AUC_improvement <-
  bombus_paired$AUC_bombus - bombus_paired$AUC_reference
utils::write.csv(
  bombus_paired,
  file.path(output_dir, "predictive_replication_bombus_paired_contrast.csv"),
  row.names = FALSE
)

national_null <- v16_candidate_null(
  national_presence, cells, national_intensity, national_phenology
)
common_reference_null <- v16_candidate_null(
  common_reference, common, national_intensity, national_phenology
)
common_fingerprint_null <- v16_candidate_null(
  common_fingerprint, common, national_intensity, national_phenology
)
null_summary <- do.call(rbind, list(
  national_null$summary,
  common_reference_null$summary,
  common_fingerprint_null$summary
))
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
    "log_population_sum_25km", "bombus_fingerprint_common_support"
  ), drop = FALSE]
  score <- cbind(score, additions)
  score$local_colour_isolation <- null_result$observed_local_isolation
  score$early_phenology_surprise <- null_result$observed_early_surprise
  score$conditional_intensity_surprise <- null_result$observed_intensity_surprise
  score
}

score_tables <- list(
  national_environment_spde_presence = annotate_scores(national_null, cells),
  common_support_environment_spde_presence = annotate_scores(
    common_reference_null, common
  ),
  common_support_environment_spde_bombus_presence = annotate_scores(
    common_fingerprint_null, common
  )
)
cell_scores <- do.call(rbind, score_tables)
rownames(cell_scores) <- NULL
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

rank_sensitivity <- v16_rank_sensitivity(score_tables)
utils::write.csv(
  rank_sensitivity,
  file.path(output_dir, "predictive_replication_candidate_rank_sensitivity.csv"),
  row.names = FALSE
)

stability <- do.call(rbind, list(
  v16_simulation_stability(national_presence, cells),
  v16_simulation_stability(common_reference, common),
  v16_simulation_stability(common_fingerprint, common)
))
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
    "phenology_likelihood", "spatial_validation", "natural_primary_model",
    "bombus_role", "bombus_abundance_claim", "candidate_selection_inputs",
    "AUC_definition",
    "heldout_candidate_facets", "white_intensity_modelled",
    "residual_as_primary_response", "simulation_map_type",
    "cross_fold_joint_dependence", "horticultural_claim_ceiling"
  ),
  value = c(
    "v16_predictive_replication", v16_analysis_spec_version,
    format(Sys.time(), tz = "Asia/Tokyo"), n_draws, seed,
    "1-km cell with observed trial count fixed",
    "binomial n_pigmented out of n_observations",
    "Gaussian conditional on at least one observed pigmented flower",
    "Gaussian DOY with environment, observation year, and SPDE",
    "five response-blind 100-km spatial folds; SPDE constrained to zero mean at training locations within each fold",
    "nationwide environment plus SPDE",
    "environment-orthogonal five-species ENMeval fingerprint on common support",
    "not abundance, visitation, or pollination effectiveness",
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
    "# v16 cross-fitted predictive replication",
    "",
    paste0("Generated with ", n_draws, " predictive draws per spatial-fold model."),
    "",
    "The primary candidate score is a one-sided cross-fitted predictive tail probability for pigmented counts in a 1-km cell. Population, phenology, conditional intensity, and local isolation are not used to select candidates. Their observed cross-fitted facet maps are held fixed while the same candidate extraction is applied to each predictive replicate map; separate response models are not treated as a joint posterior merely by sharing a draw index.",
    "",
    "The national model contains multiscale environment and an SPDE field constrained to zero mean at fold-specific training locations. Observation-level posterior predictions are drawn from INLA's projected APredictor. The five-species ENMeval fingerprint is evaluated only on its common support after fold-trained orthogonalization against environment. Its predictions are not abundance, visitation, or pollination effectiveness.",
    "",
    "Predictive maps are generated only at observed sampling cells with observed trial counts fixed. They are not occurrence or flower-colour maps for unsampled Japan. Candidate departures do not establish horticultural origin; genetics, provenance, and field verification remain necessary."
  ),
  file.path(output_dir, "README.md")
)

cat("Completed v16 predictive replication: ", output_dir, "\n", sep = "")
print(performance)
print(rank_sensitivity)
