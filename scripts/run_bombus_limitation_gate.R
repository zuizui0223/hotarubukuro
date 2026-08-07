args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

artifact_root <- arg_value("--artifact-root", ".")
output_dir <- arg_value("--output", "exploratory-bombus-limitation")
env_match <- as.numeric(arg_value("--env-match", "0.75"))
low_thresholds <- as.numeric(strsplit(
  arg_value("--low-thresholds", "0.10,0.20,0.25,0.33"), ","
)[[1]])
available_threshold <- as.numeric(arg_value("--available-threshold", "0.50"))

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
required <- c(cells_path, presence_path, intensity_path)
if (!all(file.exists(required))) {
  stop("Missing frozen 1,909 inputs: ",
       paste(required[!file.exists(required)], collapse = ", "), call. = FALSE)
}

cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
presence <- v17_align_result(readRDS(presence_path), cells, "presence checkpoint")
intensity <- v17_align_result(readRDS(intensity_path), cells, "intensity checkpoint")

species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
rank_columns <- paste0(species, "_within_species_rank")
v17_require_columns(
  cells,
  c(rank_columns, "n_observations", "n_pigmented", "conditional_intensity_median",
    "spatial_fold", "x_km", "y_km", "bombus_fingerprint_common_support"),
  "cells"
)

# Gate-type ecological exposure: the best-supported focal Bombus species at a
# cell. Low values mean that every focal species has low relative support; this
# is a defensible "Bombus-limited" state without adding uncalibrated SDM values
# across species or calling them abundance/visitation.
support_matrix <- as.matrix(cells[, rank_columns, drop = FALSE])
storage.mode(support_matrix) <- "double"
cells$best_bombus_support_rank <- apply(support_matrix, 1, max, na.rm = TRUE)
all_missing <- apply(!is.finite(support_matrix), 1, all)
cells$best_bombus_support_rank[all_missing] <- NA_real_

# Response-blind local graph, then environmental matching before flower colour is
# inspected. Same-fold restriction preserves the joint flower posterior draws.
edges <- v17_pair_graph(
  cells, radius_km = 25, k = 5L,
  same_fold_only = TRUE, common_support_only = TRUE
)
edges <- v17_add_pair_features(edges, cells, presence, intensity)
edges <- edges[
  is.finite(edges$environmental_distance) &
    edges$environmental_distance <= env_match,
  , drop = FALSE
]

share <- cells$n_pigmented / pmax(cells$n_observations, 1)
obs_intensity <- cells$conditional_intensity_median
trials <- pmax(cells$n_observations, 1)
presence_draw_share <- sweep(presence$draws, 1, trials, "/")

null_compare <- function(observed, simulated) {
  simulated <- simulated[is.finite(simulated)]
  if (!is.finite(observed) || !length(simulated)) {
    return(c(null_mean = NA_real_, upper_p = NA_real_, two_sided_p = NA_real_))
  }
  p_upper <- (1 + sum(simulated >= observed)) / (length(simulated) + 1)
  p_lower <- (1 + sum(simulated <= observed)) / (length(simulated) + 1)
  c(
    null_mean = mean(simulated),
    upper_p = p_upper,
    two_sided_p = min(1, 2 * min(p_upper, p_lower))
  )
}

# Greedy one-to-one matching. Flower colour is never used. Candidate edges are
# ordered by environmental similarity, then geographic distance, so a cell is
# not repeatedly recycled into the same threshold contrast.
greedy_match <- function(candidate) {
  if (!nrow(candidate)) return(candidate)
  candidate <- candidate[order(
    candidate$environmental_distance,
    candidate$geographic_distance_km,
    candidate$low_i, candidate$high_i
  ), , drop = FALSE]
  used <- integer()
  keep <- logical(nrow(candidate))
  for (r in seq_len(nrow(candidate))) {
    a <- candidate$low_i[r]
    b <- candidate$high_i[r]
    if (!(a %in% used) && !(b %in% used)) {
      keep[r] <- TRUE
      used <- c(used, a, b)
    }
  }
  candidate[keep, , drop = FALSE]
}

make_oriented_pairs <- function(low_threshold) {
  i <- edges$i
  j <- edges$j
  si <- cells$best_bombus_support_rank[i]
  sj <- cells$best_bombus_support_rank[j]
  i_low <- is.finite(si) & si <= low_threshold
  j_low <- is.finite(sj) & sj <= low_threshold
  i_available <- is.finite(si) & si >= available_threshold
  j_available <- is.finite(sj) & sj >= available_threshold
  take_ij <- i_low & j_available
  take_ji <- j_low & i_available
  keep <- take_ij | take_ji
  if (!any(keep)) return(data.frame())
  d <- edges[keep, , drop = FALSE]
  take_ij <- take_ij[keep]
  d$low_i <- ifelse(take_ij, d$i, d$j)
  d$high_i <- ifelse(take_ij, d$j, d$i)
  d$low_support <- cells$best_bombus_support_rank[d$low_i]
  d$high_support <- cells$best_bombus_support_rank[d$high_i]
  d$low_threshold <- low_threshold
  d$available_threshold <- available_threshold
  greedy_match(d)
}

summaries <- list()
pair_tables <- list()
null_tables <- list()
for (low_threshold in low_thresholds) {
  d <- make_oriented_pairs(low_threshold)
  key <- format(low_threshold, trim = TRUE)
  if (!nrow(d)) next
  d$observed_presence_diff <- share[d$high_i] - share[d$low_i]
  d$observed_intensity_diff <- obs_intensity[d$high_i] - obs_intensity[d$low_i]
  d$intensity_pair <- is.finite(d$observed_intensity_diff)

  presence_observed <- mean(d$observed_presence_diff, na.rm = TRUE)
  presence_null <- colMeans(
    presence_draw_share[d$high_i, , drop = FALSE] -
      presence_draw_share[d$low_i, , drop = FALSE],
    na.rm = TRUE
  )
  p_cmp <- null_compare(presence_observed, presence_null)

  if (any(d$intensity_pair)) {
    di <- d[d$intensity_pair, , drop = FALSE]
    intensity_observed <- mean(di$observed_intensity_diff, na.rm = TRUE)
    intensity_null <- colMeans(
      intensity$draws[di$high_i, , drop = FALSE] -
        intensity$draws[di$low_i, , drop = FALSE],
      na.rm = TRUE
    )
    i_cmp <- null_compare(intensity_observed, intensity_null)
  } else {
    intensity_observed <- NA_real_
    intensity_null <- rep(NA_real_, ncol(intensity$draws))
    i_cmp <- c(null_mean = NA_real_, upper_p = NA_real_, two_sided_p = NA_real_)
  }

  summaries[[paste0(key, "_presence")]] <- data.frame(
    low_threshold = low_threshold,
    available_threshold = available_threshold,
    response = "pigmentation_share",
    n_pairs = nrow(d),
    observed_directed_difference = presence_observed,
    natural_null_mean = p_cmp[["null_mean"]],
    upper_tail_p = p_cmp[["upper_p"]],
    two_sided_p = p_cmp[["two_sided_p"]],
    stringsAsFactors = FALSE
  )
  summaries[[paste0(key, "_intensity")]] <- data.frame(
    low_threshold = low_threshold,
    available_threshold = available_threshold,
    response = "pigmented_only_intensity",
    n_pairs = sum(d$intensity_pair),
    observed_directed_difference = intensity_observed,
    natural_null_mean = i_cmp[["null_mean"]],
    upper_tail_p = i_cmp[["upper_p"]],
    two_sided_p = i_cmp[["two_sided_p"]],
    stringsAsFactors = FALSE
  )

  pair_tables[[key]] <- d
  null_tables[[paste0(key, "_presence")]] <- data.frame(
    low_threshold = low_threshold, response = "pigmentation_share",
    draw = seq_along(presence_null), statistic = presence_null
  )
  null_tables[[paste0(key, "_intensity")]] <- data.frame(
    low_threshold = low_threshold, response = "pigmented_only_intensity",
    draw = seq_along(intensity_null), statistic = intensity_null
  )
}

summary <- do.call(rbind, summaries)
summary$BH_q_within_threshold <- NA_real_
for (threshold in unique(summary$low_threshold)) {
  idx <- summary$low_threshold == threshold & is.finite(summary$upper_tail_p)
  summary$BH_q_within_threshold[idx] <- stats::p.adjust(
    summary$upper_tail_p[idx], method = "BH"
  )
}
summary$BH_q_all_gate_tests <- stats::p.adjust(summary$upper_tail_p, method = "BH")

pairs <- do.call(rbind, pair_tables)
null <- do.call(rbind, null_tables)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(summary, file.path(output_dir, "bombus_limitation_gate_summary.csv"), row.names = FALSE)
write.csv(pairs, file.path(output_dir, "bombus_limitation_gate_pairs.csv"), row.names = FALSE)
write.csv(null, file.path(output_dir, "bombus_limitation_gate_null.csv"), row.names = FALSE)

# Exploratory within-available gradient: only asks whether more of the best
# species-specific support is associated with darker pigmentation after the
# low-Bombus gate is removed. This is secondary because SDM support is not visit
# rate and a monotone dose-response is a stronger assumption than the gate.
available_cells <- is.finite(cells$best_bombus_support_rank) &
  cells$best_bombus_support_rank >= available_threshold &
  is.finite(obs_intensity)
intensity_gradient <- data.frame(
  n_cells = sum(available_cells),
  spearman_rho = if (sum(available_cells) >= 10) {
    suppressWarnings(stats::cor(
      cells$best_bombus_support_rank[available_cells],
      obs_intensity[available_cells], method = "spearman"
    ))
  } else NA_real_,
  stringsAsFactors = FALSE
)
write.csv(
  intensity_gradient,
  file.path(output_dir, "available_zone_intensity_gradient.csv"),
  row.names = FALSE
)

# Interpretation emphasizes pattern consistency across the fixed threshold grid,
# not whichever single threshold happens to be smallest.
presence_rows <- summary[summary$response == "pigmentation_share", , drop = FALSE]
all_positive <- nrow(presence_rows) > 0 &&
  all(presence_rows$observed_directed_difference > 0)
any_supported <- any(presence_rows$BH_q_all_gate_tests < 0.05, na.rm = TRUE)
status <- if (all_positive && any_supported) {
  "bombus_limitation_pattern_directionally_consistent_with_some_predictive_support"
} else if (all_positive) {
  "bombus_limitation_pattern_directionally_consistent_but_not_predictively_supported"
} else {
  "bombus_limitation_pattern_not_directionally_consistent"
}
interpretation <- data.frame(
  field = c(
    "status", "ecological_hypothesis", "exposure_definition",
    "environment_control", "spatial_control", "flower_null_role",
    "claim_ceiling"
  ),
  value = c(
    status,
    paste(
      "low focal-Bombus availability relaxes the benefit of pigmentation;",
      "available Bombus creates opportunity for attraction-mediated selection"
    ),
    paste(
      "Bombus-limited means max within-species support rank <= fixed threshold;",
      "available means at least one focal species rank >=", available_threshold
    ),
    paste("response-blind local pairs matched at environmental distance <=", env_match),
    "25-km same-heldout-fold one-to-one local matching; no second local SPDE fit",
    paste(
      "1000 national environment-plus-SPDE flower maps are used as a predictive",
      "reference/sensitivity, not as covariates in the local pair contrast"
    ),
    paste(
      "SDM support is predicted availability, not abundance, visitation or",
      "selection pressure; shared/unmeasured environment can remain"
    )
  ),
  stringsAsFactors = FALSE
)
write.csv(
  interpretation,
  file.path(output_dir, "interpretation_summary.csv"), row.names = FALSE
)

writeLines(c(
  "# Bombus limitation-gate analysis",
  "",
  "Biological prediction: if focal Bombus are effectively unavailable, the attraction benefit of pigmentation is relaxed and white flowers should be relatively more common. Pigment production cost is a possible reinforcing mechanism, not a directly measured premise.",
  "",
  paste0("Pairs: <=25 km, same held-out fold, environmental distance <= ", env_match, "."),
  paste0("Bombus-available endpoint: best focal-species support rank >= ", available_threshold, "."),
  paste0("Bombus-limited thresholds reported as a fixed grid: ", paste(low_thresholds, collapse = ", "), "."),
  "One-to-one greedy matching uses environmental and geographic similarity only; flower colour never defines pairs or orientation.",
  "",
  "Primary response: pigmentation-share difference (available minus limited). Secondary: pigmented-only intensity difference.",
  "The 1,000 flower natural-model maps are a predictive reference to ask whether the directed contrast exceeds broad natural geography; environment and SPDE are not refit in the local model.",
  "",
  "The SDM exposure remains predicted habitat availability. It cannot establish visitation, attraction, reproductive success or selection strength."
), file.path(output_dir, "README.md"))

print(summary)
print(intensity_gradient)
print(interpretation)
