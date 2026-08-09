#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
hb_load_modules("local_bombus_turnover")

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
presence_path <- arg_value(
  "--presence",
  paste0(
    "results/ecological_v16_predictive_replication/checkpoints/",
    "national_environment_spde_presence_draws1000.rds"
  )
)
intensity_path <- arg_value(
  "--intensity",
  paste0(
    "results/ecological_v16_predictive_replication/checkpoints/",
    "national_environment_spde_intensity_draws1000.rds"
  )
)
sdm_root <- arg_value(
  "--sdm-root", "source_bombus/results/bombus_sdm_rebuild_A"
)
output_dir <- arg_value(
  "--output", "results/ecological_v17_bombus_effective_availability_refined"
)
env_match <- as.numeric(arg_value("--env-match", "0.75"))
k <- as.integer(arg_value("--k", "5"))
radii <- as.numeric(strsplit(arg_value("--radii", "10,25,50"), ",")[[1]])
primary_radius <- as.numeric(arg_value("--primary-radius", "25"))

required_packages <- c("ENMeval", "maxnet")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

required_files <- c(cells_path, presence_path, intensity_path)
if (!all(file.exists(required_files))) {
  stop(
    "Missing fresh flower-analysis inputs: ",
    paste(required_files[!file.exists(required_files)], collapse = ", "),
    call. = FALSE
  )
}

cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
presence <- v17_align_result(readRDS(presence_path), cells, "presence checkpoint")
intensity <- v17_align_result(readRDS(intensity_path), cells, "intensity checkpoint")
if (ncol(presence$draws) != ncol(intensity$draws) || ncol(presence$draws) < 1000L) {
  stop("Expected aligned presence/intensity checkpoints with >=1000 draws.", call. = FALSE)
}

species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
raw_columns <- paste0("bee_", species)
v17_require_columns(
  cells,
  c(
    "exact_site_id", "x_km", "y_km", "spatial_fold",
    "bombus_fingerprint_common_support", "n_observations", "n_pigmented",
    "conditional_intensity_median", raw_columns,
    "broad50km_pc1", "broad50km_pc2", "within50km_pc1", "within50km_pc2"
  ),
  "fresh cells"
)

# Species-specific monotone calibration against the habitat-support values at
# the exact occurrence cells stored in the seeded ENMevaluation object. This
# makes the cross-species scale occurrence-referenced without interpreting
# cloglog output as occurrence probability.
occurrence_reference <- function(short) {
  model_path <- file.path(sdm_root, "models", paste0(short, "_ENMeval.rds"))
  selected_path <- file.path(
    sdm_root, "evaluation", paste0(short, "_selected_model.csv")
  )
  if (!file.exists(model_path) || !file.exists(selected_path)) {
    stop("Missing seeded SDM model/evaluation for ", short, call. = FALSE)
  }
  evaluation <- readRDS(model_path)
  selected_table <- utils::read.csv(
    selected_path, check.names = FALSE, stringsAsFactors = FALSE
  )
  selected <- as.integer(selected_table$selected_row[[1]])
  models <- ENMeval::eval.models(evaluation)
  if (!is.finite(selected) || selected < 1L || selected > length(models)) {
    stop("Invalid selected ENMeval row for ", short, call. = FALSE)
  }
  model <- models[[selected]]
  occ <- as.data.frame(ENMeval::eval.occs(evaluation))
  occ_prediction <- suppressWarnings(as.numeric(stats::predict(
    model, newdata = occ, type = "cloglog", clamp = TRUE
  )))
  occ_prediction <- occ_prediction[is.finite(occ_prediction)]
  if (length(occ_prediction) < 20L) {
    stop("Too few occurrence-reference predictions for ", short, call. = FALSE)
  }
  reference_ecdf <- stats::ecdf(occ_prediction)
  raw <- as.numeric(cells[[paste0("bee_", short)]])
  score <- rep(NA_real_, length(raw))
  keep <- is.finite(raw)
  score[keep] <- as.numeric(reference_ecdf(raw[keep]))
  list(
    score = score,
    summary = data.frame(
      species = short,
      selected_row = selected,
      n_occurrence_reference = length(occ_prediction),
      occurrence_prediction_q10 = unname(stats::quantile(occ_prediction, 0.10)),
      occurrence_prediction_q50 = unname(stats::quantile(occ_prediction, 0.50)),
      occurrence_prediction_q90 = unname(stats::quantile(occ_prediction, 0.90)),
      flower_score_min = min(score, na.rm = TRUE),
      flower_score_median = stats::median(score, na.rm = TRUE),
      flower_score_max = max(score, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  )
}

reference <- lapply(species, occurrence_reference)
names(reference) <- species
reference_summary <- do.call(rbind, lapply(reference, `[[`, "summary"))
score_matrix <- do.call(cbind, lapply(reference, `[[`, "score"))
colnames(score_matrix) <- paste0(species, "_occurrence_reference")

raw_matrix <- as.matrix(cells[, raw_columns, drop = FALSE])
storage.mode(raw_matrix) <- "double"

exposures <- list(
  effective_occmax = pmax(score_matrix[, "ardens_occurrence_reference"],
                          score_matrix[, "diversus_occurrence_reference"]),
  effective_occmean = rowMeans(score_matrix[, c(
    "ardens_occurrence_reference", "diversus_occurrence_reference"
  ), drop = FALSE]),
  effective_rawmax = pmax(raw_matrix[, "bee_ardens"], raw_matrix[, "bee_diversus"]),
  all5_occmax = apply(score_matrix, 1, max),
  all5_occmean = rowMeans(score_matrix),
  ardens_occ = score_matrix[, "ardens_occurrence_reference"],
  diversus_occ = score_matrix[, "diversus_occurrence_reference"]
)
exposure_role <- c(
  effective_occmax = "primary_effective_guild",
  effective_occmean = "effective_guild_mean_sensitivity",
  effective_rawmax = "raw_effective_guild_max_sensitivity",
  all5_occmax = "all_five_best_species_sensitivity",
  all5_occmean = "all_five_mean_sensitivity",
  ardens_occ = "species_sensitivity",
  diversus_occ = "species_sensitivity"
)

cell_scores <- data.frame(
  exact_site_id = cells$exact_site_id,
  score_matrix,
  effective_occmax = exposures$effective_occmax,
  effective_occmean = exposures$effective_occmean,
  effective_rawmax = exposures$effective_rawmax,
  all5_occmax = exposures$all5_occmax,
  all5_occmean = exposures$all5_occmean,
  stringsAsFactors = FALSE
)

null_compare <- function(observed, simulated) {
  simulated <- simulated[is.finite(simulated)]
  if (!is.finite(observed) || !length(simulated)) {
    return(c(
      null_mean = NA_real_, null_sd = NA_real_, null_lower_95 = NA_real_,
      null_upper_95 = NA_real_, upper_p = NA_real_, two_sided_p = NA_real_
    ))
  }
  p_upper <- (1 + sum(simulated >= observed)) / (length(simulated) + 1)
  p_lower <- (1 + sum(simulated <= observed)) / (length(simulated) + 1)
  c(
    null_mean = mean(simulated),
    null_sd = stats::sd(simulated),
    null_lower_95 = unname(stats::quantile(simulated, 0.025)),
    null_upper_95 = unname(stats::quantile(simulated, 0.975)),
    upper_p = p_upper,
    two_sided_p = min(1, 2 * min(p_upper, p_lower))
  )
}

one_to_one_environment_nearest <- function(edges) {
  if (!nrow(edges)) return(edges)
  d <- edges[order(
    edges$environmental_distance,
    edges$geographic_distance_km,
    edges$i, edges$j
  ), , drop = FALSE]
  used <- integer()
  keep <- logical(nrow(d))
  for (r in seq_len(nrow(d))) {
    a <- d$i[[r]]
    b <- d$j[[r]]
    if (!(a %in% used) && !(b %in% used)) {
      keep[[r]] <- TRUE
      used <- c(used, a, b)
    }
  }
  d[keep, , drop = FALSE]
}

presence_values <- as.numeric(cells$n_pigmented) /
  pmax(as.numeric(cells$n_observations), 1)
presence_trials <- pmax(as.numeric(cells$n_observations), 1)
presence_draws <- sweep(presence$draws, 1, presence_trials, "/")
intensity_values <- as.numeric(cells$conditional_intensity_median)
intensity_draws <- intensity$draws

environment <- v17_environment_matrix(cells)

result_rows <- list()
null_rows <- list()
edge_output <- list()
row_id <- 0L
null_id <- 0L

analyse_exposure <- function(edges, exposure, exposure_name, radius, design,
                             response = c("presence", "intensity")) {
  response <- match.arg(response)
  i <- as.integer(edges$i)
  j <- as.integer(edges$j)
  ai <- exposure[i]
  aj <- exposure[j]
  keep <- is.finite(ai) & is.finite(aj) & abs(ai - aj) > 1e-12
  if (response == "intensity") {
    keep <- keep & is.finite(intensity_values[i]) & is.finite(intensity_values[j])
  }
  if (!any(keep)) return(NULL)
  d <- edges[keep, , drop = FALSE]
  i <- as.integer(d$i)
  j <- as.integer(d$j)
  ai <- exposure[i]
  aj <- exposure[j]
  j_high <- aj > ai
  high <- ifelse(j_high, j, i)
  low <- ifelse(j_high, i, j)
  delta_a <- abs(aj - ai)

  if (response == "presence") {
    observed_y <- presence_values
    simulated_y <- presence_draws
  } else {
    observed_y <- intensity_values
    simulated_y <- intensity_draws
  }
  delta_y <- observed_y[high] - observed_y[low]
  finite <- is.finite(delta_a) & is.finite(delta_y) & delta_a > 1e-12
  high <- high[finite]
  low <- low[finite]
  delta_a <- delta_a[finite]
  delta_y <- delta_y[finite]
  d <- d[finite, , drop = FALSE]
  if (length(delta_a) < 10L) return(NULL)

  denominator <- sum(delta_a^2)
  observed_slope <- sum(delta_a * delta_y) / denominator
  simulated_difference <- simulated_y[high, , drop = FALSE] -
    simulated_y[low, , drop = FALSE]
  simulated_slope <- colSums(
    sweep(simulated_difference, 1, delta_a, "*")
  ) / denominator

  observed_mean <- mean(delta_y)
  simulated_mean <- colMeans(simulated_difference)
  slope_cmp <- null_compare(observed_slope, simulated_slope)
  mean_cmp <- null_compare(observed_mean, simulated_mean)

  summary <- data.frame(
    radius_km = radius,
    design = design,
    response = response,
    exposure = exposure_name,
    exposure_role = unname(exposure_role[[exposure_name]]),
    is_primary = radius == primary_radius && design == "all_edges" &&
      response == "presence" && exposure_name == "effective_occmax",
    n_edges = length(delta_a),
    n_nodes = length(unique(c(high, low))),
    median_exposure_difference = stats::median(delta_a),
    q25_exposure_difference = unname(stats::quantile(delta_a, 0.25)),
    q75_exposure_difference = unname(stats::quantile(delta_a, 0.75)),
    observed_through_origin_slope = observed_slope,
    slope_null_mean = slope_cmp[["null_mean"]],
    slope_null_sd = slope_cmp[["null_sd"]],
    slope_null_lower_95 = slope_cmp[["null_lower_95"]],
    slope_null_upper_95 = slope_cmp[["null_upper_95"]],
    slope_upper_p = slope_cmp[["upper_p"]],
    slope_two_sided_p = slope_cmp[["two_sided_p"]],
    observed_mean_signed_difference = observed_mean,
    mean_difference_null_mean = mean_cmp[["null_mean"]],
    mean_difference_upper_p = mean_cmp[["upper_p"]],
    mean_difference_two_sided_p = mean_cmp[["two_sided_p"]],
    stringsAsFactors = FALSE
  )
  null <- data.frame(
    radius_km = radius,
    design = design,
    response = response,
    exposure = exposure_name,
    draw = seq_along(simulated_slope),
    through_origin_slope = simulated_slope,
    mean_signed_difference = simulated_mean,
    stringsAsFactors = FALSE
  )
  edge_detail <- data.frame(
    radius_km = radius,
    design = design,
    response = response,
    exposure = exposure_name,
    edge_id = d$edge_id,
    low_site = cells$exact_site_id[low],
    high_site = cells$exact_site_id[high],
    geographic_distance_km = d$geographic_distance_km,
    environmental_distance = d$environmental_distance,
    exposure_difference = delta_a,
    observed_signed_difference = delta_y,
    stringsAsFactors = FALSE
  )
  list(summary = summary, null = null, edges = edge_detail)
}

for (radius in radii) {
  message("[effective-Bombus] response-blind graph at ", radius, " km")
  edges <- v17_pair_graph(
    cells, radius_km = radius, k = k,
    same_fold_only = TRUE, common_support_only = TRUE
  )
  if (!nrow(edges)) next
  edges$environmental_distance <- v17_pair_distance(
    environment, as.integer(edges$i), as.integer(edges$j)
  )
  edges <- edges[
    is.finite(edges$environmental_distance) &
      edges$environmental_distance <= env_match,
    , drop = FALSE
  ]
  designs <- list(
    all_edges = edges,
    one_to_one = one_to_one_environment_nearest(edges)
  )
  for (design_name in names(designs)) {
    d <- designs[[design_name]]
    if (!nrow(d)) next
    for (exposure_name in names(exposures)) {
      for (response in c("presence", "intensity")) {
        result <- analyse_exposure(
          d, exposures[[exposure_name]], exposure_name,
          radius, design_name, response
        )
        if (is.null(result)) next
        row_id <- row_id + 1L
        result_rows[[row_id]] <- result$summary
        null_id <- null_id + 1L
        null_rows[[null_id]] <- result$null
        if (radius == primary_radius &&
            exposure_name == "effective_occmax" &&
            response == "presence") {
          edge_output[[design_name]] <- result$edges
        }
      }
    }
  }
}

summary <- do.call(rbind, result_rows)
null <- do.call(rbind, null_rows)
if (is.null(summary) || !nrow(summary)) {
  stop("No estimable refined Bombus contrasts.", call. = FALSE)
}

summary$BH_q_all_sensitivity_slopes <- stats::p.adjust(
  summary$slope_upper_p, method = "BH"
)
summary$BH_q_primary_exposure_scales <- NA_real_
scale_family <- summary$design == "all_edges" & summary$response == "presence" &
  summary$exposure == "effective_occmax" & is.finite(summary$slope_upper_p)
summary$BH_q_primary_exposure_scales[scale_family] <- stats::p.adjust(
  summary$slope_upper_p[scale_family], method = "BH"
)

primary <- summary[summary$is_primary, , drop = FALSE]
if (nrow(primary) != 1L) {
  stop("Expected exactly one primary refined Bombus result.", call. = FALSE)
}
primary_scales <- summary[
  summary$design == "all_edges" & summary$response == "presence" &
    summary$exposure == "effective_occmax" & summary$radius_km %in% radii,
  , drop = FALSE
]
scale_direction_positive <- nrow(primary_scales) == length(radii) &&
  all(primary_scales$observed_through_origin_slope > 0)
status <- if (primary$slope_upper_p < 0.05 && scale_direction_positive) {
  "exploratory_effective_availability_support_scale_consistent"
} else if (primary$slope_upper_p < 0.05) {
  "exploratory_primary_support_not_scale_consistent"
} else {
  "refined_directional_effect_not_supported"
}

metadata <- data.frame(
  field = c(
    "analysis_status", "analysis_timing", "primary_exposure",
    "primary_species", "primary_radius_km", "sensitivity_radii_km",
    "environment_caliper", "pair_graph", "primary_statistic",
    "natural_null", "claim_ceiling"
  ),
  value = c(
    status,
    "post-null hypothesis-refining exploratory reanalysis",
    "max occurrence-referenced cloglog support across B. ardens and B. diversus",
    "Bombus ardens; Bombus diversus",
    as.character(primary_radius), paste(radii, collapse = ","),
    as.character(env_match),
    paste0(k, " nearest response-blind neighbours; same heldout fold; common support"),
    "through-origin slope of signed pigmentation difference on effective-Bombus support difference",
    paste0(ncol(presence$draws), " Bombus-free environment+SPDE posterior-predictive flower maps"),
    "consistent with pollinator-mediated relaxation only; SDM support is not visitation or causal selection pressure"
  ),
  stringsAsFactors = FALSE
)

interpretation <- data.frame(
  field = c(
    "status", "primary_beta", "primary_null_mean", "primary_upper_p",
    "scale_direction_positive", "confirmatory_status"
  ),
  value = c(
    status,
    format(primary$observed_through_origin_slope, digits = 8),
    format(primary$slope_null_mean, digits = 8),
    format(primary$slope_upper_p, digits = 8),
    as.character(scale_direction_positive),
    "exploratory regardless of P because exposure was refined after inspection of the previous null result"
  ),
  stringsAsFactors = FALSE
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(reference_summary,
                 file.path(output_dir, "occurrence_reference_summary.csv"), row.names = FALSE)
utils::write.csv(cell_scores,
                 file.path(output_dir, "cell_effective_bombus_support.csv"), row.names = FALSE)
utils::write.csv(summary,
                 file.path(output_dir, "effective_availability_summary.csv"), row.names = FALSE)
utils::write.csv(null,
                 file.path(output_dir, "effective_availability_null.csv"), row.names = FALSE)
for (design_name in names(edge_output)) {
  utils::write.csv(
    edge_output[[design_name]],
    file.path(output_dir, paste0("primary_25km_", design_name, "_edges.csv")),
    row.names = FALSE
  )
}
utils::write.csv(metadata,
                 file.path(output_dir, "effective_availability_metadata.csv"), row.names = FALSE)
utils::write.csv(interpretation,
                 file.path(output_dir, "interpretation_summary.csv"), row.names = FALSE)

readme <- c(
  "# Refined effective-Bombus availability analysis",
  "",
  paste("Status:", status),
  "",
  "This is a post-null exploratory refinement and never replaces the previously reported null directional analysis.",
  "",
  paste0(
    "Primary exposure is the maximum occurrence-referenced habitat-support score across B. ardens and B. diversus, ",
    "the two Bombus taxa directly documented as predominant C. punctata pollinators in the relevant Japanese system."
  ),
  "",
  paste0(
    "The response-blind local design uses the fixed 25-km primary radius, same held-out spatial fold, ",
    "environmental distance <= ", env_match, ", and a Bombus-free environment+SPDE posterior-predictive null."
  ),
  "",
  paste0(
    "Primary observed slope = ", signif(primary$observed_through_origin_slope, 5),
    "; null mean = ", signif(primary$slope_null_mean, 5),
    "; one-sided empirical P = ", signif(primary$slope_upper_p, 5), "."
  )
)
writeLines(readme, file.path(output_dir, "README.md"), useBytes = TRUE)

cat("Refined effective-Bombus analysis written to ", normalizePath(output_dir), "\n", sep = "")
print(summary[order(summary$response, summary$design, summary$exposure, summary$radius_km), ])
