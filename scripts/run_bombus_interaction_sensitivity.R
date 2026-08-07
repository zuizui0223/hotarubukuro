args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

artifact_root <- arg_value("--artifact-root", ".")
output_dir <- arg_value(
  "--output", "results/exploratory_bombus_interaction_sensitivity"
)
primary_env_match <- as.numeric(arg_value("--primary-env-match", "0.75"))
match_thresholds <- as.numeric(strsplit(
  arg_value("--env-match-thresholds", "0.5,0.75,1.0"), ","
)[[1]])

hb_load_modules("local_bombus_turnover")

path_in_artifact <- function(...) file.path(artifact_root, ...)
cells_path <- path_in_artifact(
  "results", "ecological_v15_multiscale_hotspots",
  "multiscale_hotspot_cells_1km.csv"
)
presence_path <- path_in_artifact(
  "results", "ecological_v16_predictive_replication", "checkpoints",
  "national_environment_spde_presence_draws1000.rds"
)
intensity_path <- path_in_artifact(
  "results", "ecological_v16_predictive_replication", "checkpoints",
  "national_environment_spde_intensity_draws1000.rds"
)

required_files <- c(cells_path, presence_path, intensity_path)
if (!all(file.exists(required_files))) {
  stop(
    "Missing canonical-artifact inputs: ",
    paste(required_files[!file.exists(required_files)], collapse = ", "),
    call. = FALSE
  )
}

cells <- utils::read.csv(
  cells_path, check.names = FALSE, stringsAsFactors = FALSE
)
presence <- v17_align_result(readRDS(presence_path), cells, "presence checkpoint")
intensity <- v17_align_result(readRDS(intensity_path), cells, "intensity checkpoint")
if (ncol(presence$draws) != ncol(intensity$draws) || ncol(presence$draws) < 1000L) {
  stop("Expected aligned presence/intensity checkpoints with >=1000 draws.",
       call. = FALSE)
}

species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
rank_columns <- paste0(species, "_within_species_rank")
environment_columns <- c(
  "broad50km_pc1", "broad50km_pc2", "within50km_pc1", "within50km_pc2"
)
v17_require_columns(
  cells,
  c(rank_columns, environment_columns, "spatial_fold", "n_observations",
    "n_pigmented", "conditional_intensity_median"),
  "cells"
)

# Cross-fitted residualization: every cell's Bombus profile is residualized by
# a model fitted only to the other spatial folds. This prevents the local test
# from reusing the focal fold to estimate the abiotic component removed from
# its predicted community signal.
crossfit_residualize_species <- function(cells, species_columns, env_columns) {
  folds <- sort(unique(cells$spatial_fold))
  residuals <- matrix(
    NA_real_, nrow = nrow(cells), ncol = length(species_columns),
    dimnames = list(NULL, species_columns)
  )
  audit <- list()
  for (fold in folds) {
    train <- cells$spatial_fold != fold &
      stats::complete.cases(cells[, c(species_columns, env_columns), drop = FALSE])
    test <- cells$spatial_fold == fold &
      stats::complete.cases(cells[, c(species_columns, env_columns), drop = FALSE])
    if (sum(train) < 20L || !any(test)) next
    for (column in species_columns) {
      formula <- stats::reformulate(env_columns, response = column)
      fit <- stats::lm(formula, data = cells[train, , drop = FALSE])
      prediction <- as.numeric(stats::predict(
        fit, newdata = cells[test, , drop = FALSE]
      ))
      residuals[test, column] <- as.numeric(cells[test, column]) - prediction
      audit[[length(audit) + 1L]] <- data.frame(
        heldout_fold = fold,
        species = sub("_within_species_rank$", "", column),
        n_train = sum(train),
        n_test = sum(test),
        training_R2 = summary(fit)$r.squared,
        stringsAsFactors = FALSE
      )
    }
  }
  if (any(!is.finite(residuals[stats::complete.cases(cells[, species_columns]), ]))) {
    stop("Cross-fitted Bombus residualization left missing values on common support.",
         call. = FALSE)
  }
  scaled <- apply(residuals, 2, v17_scale_vector)
  if (!is.matrix(scaled)) scaled <- matrix(scaled, ncol = length(species_columns))
  colnames(scaled) <- species_columns
  list(residual_z = scaled, audit = do.call(rbind, audit))
}

residualized <- crossfit_residualize_species(cells, rank_columns, environment_columns)
residual_z <- residualized$residual_z
residual_support <- rowMeans(residual_z)
residual_composition <- residual_z - residual_support

pair_turnover <- function(matrix, i, j) {
  sqrt(rowMeans((matrix[i, , drop = FALSE] - matrix[j, , drop = FALSE])^2))
}

edges <- v17_pair_graph(
  cells, radius_km = 25, k = 5L,
  same_fold_only = TRUE, common_support_only = TRUE
)
edges <- v17_add_pair_features(edges, cells, presence, intensity)
i <- edges$i
j <- edges$j
edges$env_orth_profile_turnover <- pair_turnover(residual_z, i, j)
edges$env_orth_composition_turnover <- pair_turnover(residual_composition, i, j)
edges$env_orth_support_difference <- abs(residual_support[i] - residual_support[j])

# Leave-one-species-out community-composition turnover. This asks whether the
# community-level result is an artefact of any single Bombus surface.
for (index in seq_along(species)) {
  kept <- setdiff(seq_along(species), index)
  z <- residual_z[, kept, drop = FALSE]
  composition <- z - rowMeans(z)
  edges[[paste0("env_orth_composition_without_", species[index])]] <-
    pair_turnover(composition, i, j)
}

run_test <- function(d, response, predictors) {
  if (!nrow(d)) return(NULL)
  v17_pair_predictive_test(d, cells, if (response == "presence") presence else intensity,
                           response = response, predictors = predictors)
}

summaries <- list()
nulls <- list()
for (threshold in match_thresholds) {
  matched <- edges[is.finite(edges$environmental_distance) &
                     edges$environmental_distance <= threshold, , drop = FALSE]
  predictors <- c(
    env_orth_composition_turnover = "environment_residualized_composition",
    env_orth_support_difference = "environment_residualized_total_support",
    env_orth_profile_turnover = "environment_residualized_full_profile",
    composition_hellinger_turnover = "raw_composition_context",
    fingerprint_turnover = "raw_fingerprint_context"
  )
  for (response in c("presence", "intensity")) {
    result <- run_test(matched, response, predictors)
    result$summary$environment_match_threshold <- threshold
    result$null$environment_match_threshold <- threshold
    summaries[[paste(threshold, response, sep = "_")]] <- result$summary
    nulls[[paste(threshold, response, sep = "_")]] <- result$null
  }
}
summary <- do.call(rbind, summaries)
null <- do.call(rbind, nulls)

# Primary multiplicity family: the two hierarchical flower responses for the
# predeclared 0.75 matched, environment-residualized composition test.
summary$BH_q_primary_matched_composition <- NA_real_
primary <- summary$environment_match_threshold == primary_env_match &
  summary$predictor == "env_orth_composition_turnover" &
  is.finite(summary$beta_empirical_p)
summary$BH_q_primary_matched_composition[primary] <- stats::p.adjust(
  summary$beta_empirical_p[primary], method = "BH"
)

# Jackknife is a robustness diagnostic, not a new family of confirmatory tests.
primary_edges <- edges[
  is.finite(edges$environmental_distance) &
    edges$environmental_distance <= primary_env_match,
  , drop = FALSE
]
jackknife_rows <- list()
for (omitted in species) {
  predictor <- paste0("env_orth_composition_without_", omitted)
  for (response in c("presence", "intensity")) {
    result <- run_test(
      primary_edges, response,
      stats::setNames("species_jackknife", predictor)
    )
    row <- result$summary[1, , drop = FALSE]
    row$omitted_species <- omitted
    jackknife_rows[[paste(omitted, response, sep = "_")]] <- row
  }
}
jackknife <- do.call(rbind, jackknife_rows)

# Fold-wise observed coefficients diagnose whether one spatial fold alone drives
# the primary matched composition result. These are descriptive; the predictive
# null remains the inferential reference.
fold_rows <- list()
for (fold in sort(unique(primary_edges$fold_i))) {
  d <- primary_edges[primary_edges$fold_i == fold, , drop = FALSE]
  for (response in c("presence", "intensity")) {
    if (response == "presence") {
      observed <- d$observed_presence_transition
      baseline <- d[, c(
        "geographic_distance_km", "environmental_distance",
        "natural_presence_difference", "natural_presence_uncertainty",
        "log_presence_pair_effort"
      ), drop = FALSE]
    } else {
      d <- d[as.logical(d$conditional_intensity_pair), , drop = FALSE]
      observed <- d$observed_intensity_transition
      baseline <- d[, c(
        "geographic_distance_km", "environmental_distance",
        "natural_intensity_difference", "log_intensity_pair_effort"
      ), drop = FALSE]
    }
    if (!nrow(d)) next
    baseline$geographic_distance_km <- log1p(baseline$geographic_distance_km)
    stat <- v17_fit_partial_statistic(
      observed, baseline, d$env_orth_composition_turnover
    )
    fold_rows[[paste(fold, response, sep = "_")]] <- data.frame(
      fold = fold, response = response, n_edges = stat[["n"]],
      observed_partial_beta = stat[["beta"]],
      observed_delta_r2 = stat[["delta_r2"]], stringsAsFactors = FALSE
    )
  }
}
fold_summary <- do.call(rbind, fold_rows)

# A priori directional mechanism probe: does the flower response move along the
# broad widespread-to-montane Bombus contrast? Failure here is informative: a
# positive turnover result can reflect multidimensional community reorganization
# without a simple one-dimensional guild replacement.
widespread <- match(c("ardens", "diversus"), species)
montane <- match(c("beaticola", "consobrinus", "honshuensis"), species)
guild_axis <- rowMeans(residual_z[, montane, drop = FALSE]) -
  rowMeans(residual_z[, widespread, drop = FALSE])
primary_edges$guild_axis_signed_difference <- guild_axis[primary_edges$j] -
  guild_axis[primary_edges$i]
for (column in environment_columns) {
  primary_edges[[paste0("signed_", column)]] <-
    cells[[column]][primary_edges$j] - cells[[column]][primary_edges$i]
}
primary_edges$signed_natural_presence_difference <-
  presence$latent_mean[primary_edges$j] - presence$latent_mean[primary_edges$i]
primary_edges$signed_natural_intensity_difference <-
  intensity$latent_mean[primary_edges$j] - intensity$latent_mean[primary_edges$i]
share <- cells$n_pigmented / pmax(cells$n_observations, 1)
primary_edges$signed_presence_transition <-
  share[primary_edges$j] - share[primary_edges$i]
intensity_observed <- cells$conditional_intensity_median
primary_edges$signed_intensity_transition <-
  intensity_observed[primary_edges$j] - intensity_observed[primary_edges$i]

signed_draws <- function(d, result, response) {
  if (response == "presence") {
    trials <- pmax(cells$n_observations, 1)
    values <- sweep(result$draws, 1, trials, "/")
  } else {
    values <- result$draws
  }
  values[d$j, , drop = FALSE] - values[d$i, , drop = FALSE]
}

signed_test <- function(response) {
  d <- primary_edges
  if (response == "presence") {
    observed <- d$signed_presence_transition
    baseline <- data.frame(
      geographic_distance_km = log1p(d$geographic_distance_km),
      d[, paste0("signed_", environment_columns), drop = FALSE],
      natural = d$signed_natural_presence_difference,
      effort = d$log_presence_pair_effort,
      check.names = FALSE
    )
    simulated <- signed_draws(d, presence, "presence")
  } else {
    d <- d[as.logical(d$conditional_intensity_pair), , drop = FALSE]
    observed <- d$signed_intensity_transition
    baseline <- data.frame(
      geographic_distance_km = log1p(d$geographic_distance_km),
      d[, paste0("signed_", environment_columns), drop = FALSE],
      natural = d$signed_natural_intensity_difference,
      effort = d$log_intensity_pair_effort,
      check.names = FALSE
    )
    simulated <- signed_draws(d, intensity, "intensity")
  }
  observed_stat <- v17_fit_partial_statistic(
    observed, baseline, d$guild_axis_signed_difference
  )
  null_stat <- t(vapply(seq_len(ncol(simulated)), function(draw) {
    v17_fit_partial_statistic(
      simulated[, draw], baseline, d$guild_axis_signed_difference
    )[c("beta", "delta_r2")]
  }, numeric(2)))
  comparison <- v17_null_comparison(observed_stat[["beta"]], null_stat[, "beta"])
  data.frame(
    response = response,
    n_edges = observed_stat[["n"]],
    observed_partial_beta = observed_stat[["beta"]],
    null_mean = comparison[["null_mean"]],
    upper_tail_p = comparison[["empirical_p"]],
    two_sided_p = comparison[["empirical_two_sided_p"]],
    stringsAsFactors = FALSE
  )
}
directional <- do.call(rbind, lapply(c("presence", "intensity"), signed_test))
directional$BH_q_two_responses <- stats::p.adjust(
  directional$two_sided_p, method = "BH"
)

primary_rows <- summary[primary, , drop = FALSE]
primary_presence <- primary_rows[primary_rows$response == "presence", , drop = FALSE]
primary_intensity <- primary_rows[primary_rows$response == "intensity", , drop = FALSE]
support_rows <- summary[
  summary$environment_match_threshold == primary_env_match &
    summary$predictor == "env_orth_support_difference", , drop = FALSE
]

jack_presence <- jackknife[jackknife$response == "presence", , drop = FALSE]
all_jackknife_positive <- all(jack_presence$observed_partial_beta > 0)
presence_supported <- nrow(primary_presence) == 1L &&
  primary_presence$observed_partial_beta > 0 &&
  primary_presence$BH_q_primary_matched_composition < 0.05
intensity_supported <- nrow(primary_intensity) == 1L &&
  primary_intensity$observed_partial_beta > 0 &&
  primary_intensity$BH_q_primary_matched_composition < 0.05
presence_support_p <- support_rows$beta_empirical_p[support_rows$response == "presence"]
support_not_primary_driver <- length(presence_support_p) == 1L &&
  is.finite(presence_support_p) && presence_support_p >= 0.05

interpretation_status <- if (presence_supported && support_not_primary_driver &&
                             all_jackknife_positive) {
  "composition_mosaic_survives_conservative_design"
} else if (presence_supported) {
  "matched_environment_residualized_correspondence_survives_with_caveats"
} else {
  "original_local_correspondence_does_not_survive_conservative_design"
}

interpretation <- data.frame(
  field = c(
    "status", "primary_environment_match_threshold",
    "presence_composition_beta", "presence_composition_p", "presence_composition_q",
    "intensity_composition_beta", "intensity_composition_p", "intensity_composition_q",
    "presence_support_difference_p", "all_presence_jackknife_betas_positive",
    "directional_guild_presence_beta", "directional_guild_presence_two_sided_p",
    "ecological_reading", "inference_ceiling"
  ),
  value = c(
    interpretation_status,
    format(primary_env_match, trim = TRUE),
    format(primary_presence$observed_partial_beta, digits = 8),
    format(primary_presence$beta_empirical_p, digits = 8),
    format(primary_presence$BH_q_primary_matched_composition, digits = 8),
    format(primary_intensity$observed_partial_beta, digits = 8),
    format(primary_intensity$beta_empirical_p, digits = 8),
    format(primary_intensity$BH_q_primary_matched_composition, digits = 8),
    format(presence_support_p, digits = 8),
    as.character(all_jackknife_positive),
    format(directional$observed_partial_beta[directional$response == "presence"], digits = 8),
    format(directional$two_sided_p[directional$response == "presence"], digits = 8),
    paste(
      "A surviving composition result means that, among nearby cells matched on",
      "measured abiotic context, flower-colour turnover is greater where the",
      "species-specific predicted Bombus habitat-support profile changes beyond",
      "what those abiotic axes predict. A null total-support or guild-direction",
      "result would favor multidimensional community reorganization over a simple",
      "more-Bombus or montane-versus-widespread gradient."
    ),
    paste(
      "Still conditional on archived SDM surfaces; this is a predicted-community",
      "mosaic correspondence, not observed visitation, interaction strength, or",
      "causal pollinator-mediated selection."
    )
  ),
  stringsAsFactors = FALSE
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(residualized$audit,
                 file.path(output_dir, "bombus_crossfit_environment_residualization.csv"),
                 row.names = FALSE)
utils::write.csv(edges, file.path(output_dir, "matched_pair_features.csv"), row.names = FALSE)
utils::write.csv(summary, file.path(output_dir, "matched_predictive_summary.csv"), row.names = FALSE)
utils::write.csv(null, file.path(output_dir, "matched_predictive_null.csv"), row.names = FALSE)
utils::write.csv(jackknife, file.path(output_dir, "species_jackknife_summary.csv"), row.names = FALSE)
utils::write.csv(fold_summary, file.path(output_dir, "foldwise_primary_beta.csv"), row.names = FALSE)
utils::write.csv(directional, file.path(output_dir, "directional_guild_probe.csv"), row.names = FALSE)
utils::write.csv(interpretation, file.path(output_dir, "interpretation_summary.csv"), row.names = FALSE)

readme <- c(
  "# Conservative Bombus interaction-proxy sensitivity",
  "",
  "This analysis does not search for a new association. It re-tests the existing",
  "local Bombus-community hypothesis under stricter response-blind controls:",
  "",
  "1. 25-km, same-fold, five-species-common-support local pairs;",
  "2. environmental matching on the four pre-existing broad/within 50-km axes;",
  "3. out-of-fold residualization of each species' predicted habitat-support rank",
  "   against those environmental axes;",
  "4. separation of community-composition turnover from total-support change;",
  "5. the same 1,000 flower natural-model predictive maps used as the null; and",
  "6. leave-one-species-out and fold-wise diagnostics.",
  "",
  "The primary environment-match threshold is 0.75 standardized RMS units; 0.5",
  "and 1.0 are fixed sensitivities. The primary predictor is environment-residualized",
  "community-composition turnover. Presence and conditional intensity form the same",
  "two-response multiplicity family used by the active paper.",
  "",
  "A directional widespread-versus-montane guild contrast is reported separately.",
  "It is a mechanism probe, not the primary test: a null directional result can",
  "coexist with positive multidimensional community turnover.",
  "",
  "Important ceiling: archived Bombus SDM surfaces remain fixed. This analysis",
  "reduces measured environmental confounding and community-definition dependence",
  "but does not propagate uncertainty from GBIF sampling, SDM model selection, or",
  "SDM parameter estimation."
)
writeLines(readme, file.path(output_dir, "README.md"), useBytes = TRUE)

cat("Wrote conservative Bombus sensitivity to ", normalizePath(output_dir), "\n")
print(interpretation)
