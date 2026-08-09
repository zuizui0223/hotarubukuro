args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

artifact_root <- arg_value("--artifact-root", ".")
output_dir <- arg_value(
  "--output", "results/ecological_v17_bombus_limitation_gate"
)
env_match <- as.numeric(arg_value("--env-match", "0.75"))
low_thresholds <- as.numeric(strsplit(
  arg_value("--low-thresholds", "0.10,0.20,0.25,0.33"), ","
)[[1]])
primary_low_threshold <- as.numeric(arg_value("--primary-low-threshold", "0.33"))
available_threshold <- as.numeric(arg_value("--available-threshold", "0.50"))
if (!primary_low_threshold %in% low_thresholds) {
  stop("Primary low threshold must be included in the fixed threshold grid.", call. = FALSE)
}

hb_load_modules("bombus_limitation_gate")
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
  stop("Missing fresh stage-03 inputs: ", paste(required[!file.exists(required)], collapse = ", "), call. = FALSE)
}

cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
presence <- v17_align_result(readRDS(presence_path), cells, "presence checkpoint")
intensity <- v17_align_result(readRDS(intensity_path), cells, "intensity checkpoint")
if (ncol(presence$draws) != ncol(intensity$draws) || ncol(presence$draws) < 1000L) {
  stop("Expected aligned presence/intensity checkpoints with >=1000 draws.", call. = FALSE)
}

species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
rank_columns <- paste0(species, "_within_species_rank")
v17_require_columns(
  cells,
  c(
    rank_columns, "n_observations", "n_pigmented",
    "conditional_intensity_median", "spatial_fold", "x_km", "y_km",
    "bombus_fingerprint_common_support"
  ),
  "cells"
)

# Preserve the active exposure definition exactly: the maximum of the five
# species-specific support ranks. No threshold is moved when the fresh SDM
# leaves the fixed low-support gate empty.
support_matrix <- as.matrix(cells[, rank_columns, drop = FALSE])
storage.mode(support_matrix) <- "double"
cells$best_bombus_support_rank <- apply(support_matrix, 1, max, na.rm = TRUE)
all_missing <- apply(!is.finite(support_matrix), 1, all)
cells$best_bombus_support_rank[all_missing] <- NA_real_

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
  d$is_primary_gate <- abs(low_threshold - primary_low_threshold) < 1e-12
  greedy_match(d)
}

empty_summary_row <- function(low_threshold, response) {
  data.frame(
    low_threshold = low_threshold,
    available_threshold = available_threshold,
    is_primary_gate = abs(low_threshold - primary_low_threshold) < 1e-12,
    response = response,
    n_pairs = 0L,
    observed_directed_difference = NA_real_,
    natural_null_mean = NA_real_,
    upper_tail_p = NA_real_,
    two_sided_p = NA_real_,
    stringsAsFactors = FALSE
  )
}

summaries <- list()
pair_tables <- list()
null_tables <- list()
for (low_threshold in low_thresholds) {
  d <- make_oriented_pairs(low_threshold)
  key <- format(low_threshold, trim = TRUE)

  if (!nrow(d)) {
    summaries[[paste0(key, "_presence")]] <- empty_summary_row(
      low_threshold, "pigmentation_share"
    )
    summaries[[paste0(key, "_intensity")]] <- empty_summary_row(
      low_threshold, "pigmented_only_intensity"
    )
    null_tables[[paste0(key, "_presence")]] <- data.frame(
      low_threshold = low_threshold, response = "pigmentation_share",
      draw = seq_len(ncol(presence$draws)), statistic = NA_real_,
      stringsAsFactors = FALSE
    )
    null_tables[[paste0(key, "_intensity")]] <- data.frame(
      low_threshold = low_threshold, response = "pigmented_only_intensity",
      draw = seq_len(ncol(intensity$draws)), statistic = NA_real_,
      stringsAsFactors = FALSE
    )
    next
  }

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
    is_primary_gate = abs(low_threshold - primary_low_threshold) < 1e-12,
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
    is_primary_gate = abs(low_threshold - primary_low_threshold) < 1e-12,
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
    draw = seq_along(presence_null), statistic = presence_null,
    stringsAsFactors = FALSE
  )
  null_tables[[paste0(key, "_intensity")]] <- data.frame(
    low_threshold = low_threshold, response = "pigmented_only_intensity",
    draw = seq_along(intensity_null), statistic = intensity_null,
    stringsAsFactors = FALSE
  )
}

summary <- do.call(rbind, summaries)
rownames(summary) <- NULL
summary$BH_q_within_threshold <- NA_real_
for (threshold in unique(summary$low_threshold)) {
  idx <- summary$low_threshold == threshold & is.finite(summary$upper_tail_p)
  if (any(idx)) {
    summary$BH_q_within_threshold[idx] <- stats::p.adjust(
      summary$upper_tail_p[idx], method = "BH"
    )
  }
}
summary$BH_q_all_gate_tests <- NA_real_
finite_p <- is.finite(summary$upper_tail_p)
if (any(finite_p)) {
  summary$BH_q_all_gate_tests[finite_p] <- stats::p.adjust(
    summary$upper_tail_p[finite_p], method = "BH"
  )
}

if (length(pair_tables)) {
  pairs <- do.call(rbind, pair_tables)
  rownames(pairs) <- NULL
} else {
  # Schema-correct zero-row table: the fixed exposure gate can legitimately be
  # non-estimable after swapping the SDM, and that result must not be converted
  # into a different threshold or a different analysis.
  pairs <- edges[0, , drop = FALSE]
  for (name in c(
    "low_i", "high_i", "low_support", "high_support", "low_threshold",
    "available_threshold", "is_primary_gate", "observed_presence_diff",
    "observed_intensity_diff", "intensity_pair"
  )) pairs[[name]] <- vector(if (name %in% c("is_primary_gate", "intensity_pair")) "logical" else "numeric", 0L)
}
null <- do.call(rbind, null_tables)
rownames(null) <- NULL

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(summary, file.path(output_dir, "bombus_limitation_gate_summary.csv"), row.names = FALSE)
write.csv(pairs, file.path(output_dir, "bombus_limitation_gate_pairs.csv"), row.names = FALSE)
write.csv(null, file.path(output_dir, "bombus_limitation_gate_null.csv"), row.names = FALSE)

available_cells <- is.finite(cells$best_bombus_support_rank) &
  cells$best_bombus_support_rank >= available_threshold & is.finite(obs_intensity)
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
  file.path(output_dir, "available_zone_intensity_gradient.csv"), row.names = FALSE
)

support_diagnostics <- do.call(rbind, lapply(low_thresholds, function(threshold) {
  data.frame(
    low_threshold = threshold,
    n_cells_low = sum(is.finite(cells$best_bombus_support_rank) &
                        cells$best_bombus_support_rank <= threshold),
    n_cells_available = sum(is.finite(cells$best_bombus_support_rank) &
                              cells$best_bombus_support_rank >= available_threshold),
    min_best_support_rank = min(cells$best_bombus_support_rank, na.rm = TRUE),
    median_best_support_rank = stats::median(cells$best_bombus_support_rank, na.rm = TRUE),
    max_best_support_rank = max(cells$best_bombus_support_rank, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
write.csv(
  support_diagnostics,
  file.path(output_dir, "bombus_limitation_gate_support_diagnostics.csv"),
  row.names = FALSE
)

primary_presence <- summary[
  summary$is_primary_gate & summary$response == "pigmentation_share", , drop = FALSE
]
primary_intensity <- summary[
  summary$is_primary_gate & summary$response == "pigmented_only_intensity", , drop = FALSE
]
presence_rows <- summary[summary$response == "pigmentation_share", , drop = FALSE]
primary_estimable <- nrow(primary_presence) == 1L && primary_presence$n_pairs > 0L &&
  is.finite(primary_presence$observed_directed_difference)
all_positive <- any(is.finite(presence_rows$observed_directed_difference)) &&
  all(presence_rows$observed_directed_difference[
    is.finite(presence_rows$observed_directed_difference)
  ] > 0)
primary_supported <- primary_estimable &&
  is.finite(primary_presence$upper_tail_p) &&
  is.finite(primary_presence$BH_q_within_threshold) &&
  primary_presence$observed_directed_difference > 0 &&
  primary_presence$upper_tail_p < 0.05 &&
  primary_presence$BH_q_within_threshold < 0.05
all_grid_supported <- primary_estimable &&
  is.finite(primary_presence$BH_q_all_gate_tests) &&
  primary_presence$BH_q_all_gate_tests < 0.05
status <- if (!primary_estimable) {
  "lower_third_gate_not_estimable_no_fixed_gate_pairs"
} else if (primary_supported && all_grid_supported) {
  "lower_third_gate_supported_after_grid_correction"
} else if (primary_supported && all_positive) {
  "lower_third_gate_exploratory_support_not_gridwise_significant"
} else if (all_positive) {
  "directionally_consistent_without_primary_predictive_support"
} else {
  "limitation_pattern_not_directionally_consistent"
}

interpretation <- data.frame(
  field = c(
    "status", "ecological_hypothesis", "exposure_definition",
    "primary_gate", "primary_gate_history", "environment_control",
    "spatial_control", "flower_null_role", "claim_ceiling", "estimability_note"
  ),
  value = c(
    status,
    paste(
      "low focal-Bombus availability relaxes the attraction benefit of",
      "pigmentation; availability permits but does not measure Bombus-mediated selection"
    ),
    paste(
      "Bombus-limited means max within-species support rank <= fixed threshold;",
      "available means at least one focal species rank >=", available_threshold
    ),
    paste("lower-third gate:", primary_low_threshold,
          "versus at least one species at or above median support"),
    paste(
      "the 0.33 lower-third gate was adopted as the active biologically",
      "interpretable gate after exploratory design development; the complete",
      "0.10/0.20/0.25/0.33 grid and across-grid multiplicity are retained"
    ),
    paste("response-blind local pairs matched at environmental distance <=", env_match),
    "25-km same-heldout-fold one-to-one local matching; no second local SPDE fit",
    "1000 national environment-plus-SPDE flower maps are a predictive reference only",
    paste(
      "SDM support is predicted availability, not abundance, visitation or selection pressure;",
      "shared/unmeasured environment can remain"
    ),
    if (primary_estimable) {
      "fixed primary gate produced eligible matched pairs"
    } else {
      paste0(
        "fresh SDM produced no eligible fixed-gate pairs; thresholds were not changed; ",
        "minimum best-species rank=", signif(min(cells$best_bombus_support_rank, na.rm = TRUE), 6)
      )
    }
  ),
  stringsAsFactors = FALSE
)
write.csv(interpretation, file.path(output_dir, "interpretation_summary.csv"), row.names = FALSE)

metadata <- data.frame(
  field = c(
    "analysis_spec_version", "pair_radius_km", "environment_match_threshold",
    "primary_low_threshold", "available_threshold", "sensitivity_low_thresholds",
    "pair_reuse", "pair_orientation", "local_environment_model",
    "local_spatial_model", "n_flower_predictive_draws",
    "bombus_surface_role", "bombus_uncertainty", "inference_role",
    "primary_gate_estimable"
  ),
  value = c(
    "v17.2_bombus_limitation_gate", "25", as.character(env_match),
    as.character(primary_low_threshold), as.character(available_threshold),
    paste(low_thresholds, collapse = ","), "one-to-one",
    "Bombus-limited to Bombus-available; flower colour is never used",
    "none; environment is controlled by pre-outcome local matching",
    "none; broad space is restricted by local radius and same held-out fold",
    as.character(ncol(presence$draws)),
    "fresh seeded predicted habitat-support ranks",
    "fixed surfaces; SDM fitting/model-selection uncertainty not propagated",
    "mechanistically motivated local sensitivity with explicit post-development gate history",
    as.character(primary_estimable)
  ),
  stringsAsFactors = FALSE
)
write.csv(metadata, file.path(output_dir, "bombus_limitation_gate_metadata.csv"), row.names = FALSE)

writeLines(c(
  "# Bombus limitation-gate analysis — fresh SDM rerun",
  "",
  paste0("Fixed gate grid retained unchanged: ", paste(low_thresholds, collapse = ", "), "."),
  paste0("Primary fixed lower-third gate retained unchanged: ", primary_low_threshold, "."),
  paste0("Fresh-SDM minimum best-species support rank: ", signif(min(cells$best_bombus_support_rank, na.rm = TRUE), 6), "."),
  paste0("Primary matched pairs: ", primary_presence$n_pairs, "."),
  "No threshold, pair radius, environmental caliper, pair orientation, response, or flower natural model was changed.",
  if (!primary_estimable) "The fixed Bombus limitation contrast is therefore reported as not estimable under the fresh SDM." else "The fixed Bombus limitation contrast is estimable under the fresh SDM."
), file.path(output_dir, "README.md"))

print(summary)
print(support_diagnostics)
print(intensity_gradient)
print(interpretation)
if (nrow(primary_presence) != 1L || nrow(primary_intensity) != 1L) {
  stop("Primary lower-third gate rows are not uniquely defined.", call. = FALSE)
}
cat("Completed fresh-SDM Bombus limitation gate. Primary estimable=", primary_estimable, "\n", sep = "")
