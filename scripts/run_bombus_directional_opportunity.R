args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

artifact_root <- arg_value("--artifact-root", ".")
output_dir <- arg_value(
  "--output", "results/exploratory_bombus_directional_opportunity"
)
match_thresholds <- as.numeric(strsplit(
  arg_value("--env-match-thresholds", "0.5,0.75,1.0"), ","
)[[1]])
primary_env_match <- as.numeric(arg_value("--primary-env-match", "0.75"))

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
if (ncol(presence$draws) < 1000L || ncol(intensity$draws) < 1000L) {
  stop("Expected at least 1000 flower posterior-predictive draws.", call. = FALSE)
}

species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
rank_columns <- paste0(species, "_within_species_rank")
v17_require_columns(
  cells,
  c(rank_columns, "n_observations", "n_pigmented",
    "conditional_intensity_median", "spatial_fold"),
  "cells"
)

edges <- v17_pair_graph(
  cells, radius_km = 25, k = 5L,
  same_fold_only = TRUE, common_support_only = TRUE
)
edges <- v17_add_pair_features(edges, cells, presence, intensity)

support <- as.matrix(cells[, rank_columns, drop = FALSE])
storage.mode(support) <- "double"
share <- cells$n_pigmented / pmax(cells$n_observations, 1)
observed_intensity <- cells$conditional_intensity_median

# Pair orientation uses Bombus predictions only. Flower colour is never used to
# choose which endpoint is the high-opportunity endpoint.
orient_pairs <- function(edges, min_agreement = 5L) {
  if (!nrow(edges)) return(data.frame())
  diff <- support[edges$j, , drop = FALSE] - support[edges$i, , drop = FALSE]
  sign_diff <- sign(diff)
  n_positive <- rowSums(sign_diff > 0)
  n_negative <- rowSums(sign_diff < 0)
  direction <- ifelse(
    n_positive >= min_agreement, 1L,
    ifelse(n_negative >= min_agreement, -1L, 0L)
  )
  keep <- direction != 0L
  d <- edges[keep, , drop = FALSE]
  direction <- direction[keep]
  d$agreement_species <- pmax(n_positive[keep], n_negative[keep])
  d$low_index <- ifelse(direction > 0L, d$i, d$j)
  d$high_index <- ifelse(direction > 0L, d$j, d$i)
  d$low_site <- cells$exact_site_id[d$low_index]
  d$high_site <- cells$exact_site_id[d$high_index]
  d$mean_rank_difference <- rowMeans(
    support[d$high_index, , drop = FALSE] -
      support[d$low_index, , drop = FALSE]
  )
  for (s in seq_along(species)) {
    d[[paste0("delta_", species[s])]] <-
      support[d$high_index, s] - support[d$low_index, s]
  }
  d
}

presence_draw_values <- sweep(
  presence$draws, 1, pmax(cells$n_observations, 1), "/"
)

null_compare <- function(observed, simulated) {
  simulated <- simulated[is.finite(simulated)]
  p_upper <- (1 + sum(simulated >= observed)) / (length(simulated) + 1)
  p_lower <- (1 + sum(simulated <= observed)) / (length(simulated) + 1)
  data.frame(
    observed = observed,
    null_mean = mean(simulated),
    null_sd = stats::sd(simulated),
    null_lower_95 = unname(stats::quantile(simulated, 0.025)),
    null_upper_95 = unname(stats::quantile(simulated, 0.975)),
    upper_tail_p = p_upper,
    two_sided_p = min(1, 2 * min(p_upper, p_lower)),
    percentile = mean(simulated <= observed),
    stringsAsFactors = FALSE
  )
}

evaluate_oriented <- function(d, response = c("presence", "intensity")) {
  response <- match.arg(response)
  if (!nrow(d)) return(NULL)
  low <- d$low_index
  high <- d$high_index
  if (response == "presence") {
    observed_pair <- share[high] - share[low]
    sim <- presence_draw_values[high, , drop = FALSE] -
      presence_draw_values[low, , drop = FALSE]
  } else {
    keep <- is.finite(observed_intensity[low]) & is.finite(observed_intensity[high])
    d <- d[keep, , drop = FALSE]
    low <- d$low_index
    high <- d$high_index
    if (!nrow(d)) return(NULL)
    observed_pair <- observed_intensity[high] - observed_intensity[low]
    sim <- intensity$draws[high, , drop = FALSE] -
      intensity$draws[low, , drop = FALSE]
  }
  observed_stat <- mean(observed_pair)
  null_stat <- colMeans(sim)
  comparison <- null_compare(observed_stat, null_stat)
  comparison$response <- response
  comparison$n_edges <- nrow(d)
  comparison$n_nodes <- length(unique(c(d$low_site, d$high_site)))
  comparison$mean_bombus_rank_difference <- mean(d$mean_rank_difference)
  comparison
}

all_pairs <- list()
summary_rows <- list()
null_rows <- list()
for (threshold in match_thresholds) {
  matched <- edges[
    is.finite(edges$environmental_distance) &
      edges$environmental_distance <= threshold,
    , drop = FALSE
  ]
  for (agreement in c(5L, 4L)) {
    oriented <- orient_pairs(matched, min_agreement = agreement)
    if (!nrow(oriented)) next
    design <- if (agreement == 5L) "strict_5_of_5_dominance" else "majority_4_of_5"
    oriented$environment_match_threshold <- threshold
    oriented$opportunity_design <- design
    all_pairs[[paste(threshold, design, sep = "_")]] <- oriented
    for (response in c("presence", "intensity")) {
      result <- evaluate_oriented(oriented, response)
      if (is.null(result)) next
      result$environment_match_threshold <- threshold
      result$opportunity_design <- design
      summary_rows[[paste(threshold, design, response, sep = "_")]] <- result

      low <- oriented$low_index
      high <- oriented$high_index
      if (response == "presence") {
        sim <- presence_draw_values[high, , drop = FALSE] -
          presence_draw_values[low, , drop = FALSE]
      } else {
        keep <- is.finite(observed_intensity[low]) & is.finite(observed_intensity[high])
        low <- low[keep]
        high <- high[keep]
        sim <- intensity$draws[high, , drop = FALSE] -
          intensity$draws[low, , drop = FALSE]
      }
      null_rows[[paste(threshold, design, response, sep = "_")]] <- data.frame(
        response = response,
        environment_match_threshold = threshold,
        opportunity_design = design,
        draw = seq_len(ncol(sim)),
        mean_directed_flower_difference = colMeans(sim),
        stringsAsFactors = FALSE
      )
    }
  }
}

summary <- do.call(rbind, summary_rows)
summary$BH_q_primary_two_responses <- NA_real_
primary <- summary$environment_match_threshold == primary_env_match &
  summary$opportunity_design == "strict_5_of_5_dominance"
summary$BH_q_primary_two_responses[primary] <- stats::p.adjust(
  summary$upper_tail_p[primary], method = "BH"
)

pairs <- do.call(rbind, all_pairs)
null <- do.call(rbind, null_rows)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  summary, file.path(output_dir, "directional_opportunity_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  pairs, file.path(output_dir, "directional_opportunity_pairs.csv"),
  row.names = FALSE
)
utils::write.csv(
  null, file.path(output_dir, "directional_opportunity_null.csv"),
  row.names = FALSE
)

primary_rows <- summary[primary, , drop = FALSE]
presence_row <- primary_rows[primary_rows$response == "presence", , drop = FALSE]
intensity_row <- primary_rows[primary_rows$response == "intensity", , drop = FALSE]
status <- if (nrow(presence_row) == 1L &&
              presence_row$observed > 0 &&
              presence_row$BH_q_primary_two_responses < 0.05) {
  "directional_pigmentation_prediction_supported_conditionally"
} else {
  "directional_pigmentation_prediction_not_supported"
}
interpretation <- data.frame(
  field = c(
    "status", "estimand", "primary_pair_definition", "ecological_prediction",
    "abundance_claim", "visitation_claim", "selection_claim",
    "sdm_uncertainty_scope"
  ),
  value = c(
    status,
    "relative Bombus opportunity direction without cross-species abundance calibration",
    "25-km same-fold environment-matched pairs with all five species higher at one endpoint",
    "the Bombus-dominant endpoint should have higher pigmented share",
    "not estimated",
    "not estimated; opportunity only",
    "direction consistent with attraction hypothesis if positive; selection strength not estimated",
    "fixed archived SDM surfaces; alternative-model uncertainty not propagated"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  interpretation, file.path(output_dir, "interpretation_summary.csv"),
  row.names = FALSE
)

readme <- c(
  "# Directional Bombus-opportunity sensitivity",
  "",
  "This exploratory analysis avoids defining a scalar 'Bombus pressure' from SDM suitability.",
  "Instead, an environmentally matched local pair is directional only when all five focal",
  "Bombus species have higher within-species predicted support at the same endpoint.",
  "Under the biological assumption that all focal Bombus taxa exert selection in the same",
  "pigmentation-favouring direction, this endpoint has greater potential Bombus encounter",
  "opportunity under any positive species weighting. The test asks whether pigmented share is",
  "higher at that endpoint than expected from the 1,000 flower natural-model predictive maps.",
  "",
  "This does not estimate Bombus abundance, visitation rate, pollination service or selection",
  "strength. The archived SDM surfaces remain fixed inputs."
)
writeLines(readme, file.path(output_dir, "README.md"), useBytes = TRUE)

print(summary)
print(interpretation)
