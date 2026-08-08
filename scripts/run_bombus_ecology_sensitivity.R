#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) return(args[[i + 1L]])
  prefix <- paste0(flag, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

required_packages <- c("terra", "readr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

source("R/local_bombus_turnover.R")

artifact_root <- arg_value("--artifact-root")
sdm_dir <- arg_value("--sdm-dir", "results/bombus_sdm_rebuild_A/predictions")
occurrence_dir <- arg_value("--occurrence-dir", "results/bombus_occurrence_snapshot")
output_dir <- arg_value("--output", "results/bombus_ecology_sensitivity")
env_match <- as.numeric(arg_value("--env-match", "0.75"))
low_thresholds <- as.numeric(strsplit(
  arg_value("--low-thresholds", "0.10,0.20,0.25,0.33"), ","
)[[1]])
available_threshold <- as.numeric(arg_value("--available-threshold", "0.50"))
if (is.null(artifact_root) || !dir.exists(artifact_root)) {
  stop("--artifact-root must point to a restored canonical analysis snapshot.", call. = FALSE)
}
if (!dir.exists(sdm_dir)) stop("SDM directory not found: ", sdm_dir, call. = FALSE)
if (!dir.exists(occurrence_dir)) stop("Occurrence directory not found: ", occurrence_dir, call. = FALSE)
if (!all(is.finite(low_thresholds)) || any(low_thresholds <= 0 | low_thresholds >= available_threshold)) {
  stop("Low thresholds must be finite, positive, and below the available threshold.", call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
groups <- list(
  all_five = species,
  widespread = c("ardens", "diversus"),
  montane_alpine = c("beaticola", "consobrinus", "honshuensis")
)
for (sh in species) groups[[paste0("leave_out_", sh)]] <- setdiff(species, sh)
normalizations <- c("flower_cell_rank", "occurrence_site_ecdf")

cells_path <- file.path(
  artifact_root, "results", "ecological_v15_multiscale_hotspots",
  "multiscale_hotspot_cells_1km.csv"
)
presence_path <- file.path(
  artifact_root, "results", "ecological_v16_predictive_replication", "checkpoints",
  "national_environment_spde_presence_draws1000.rds"
)
intensity_path <- file.path(
  artifact_root, "results", "ecological_v16_predictive_replication", "checkpoints",
  "national_environment_spde_intensity_draws1000.rds"
)
required_inputs <- c(cells_path, presence_path, intensity_path)
if (!all(file.exists(required_inputs))) {
  stop(
    "Missing canonical flower-analysis reference input(s): ",
    paste(required_inputs[!file.exists(required_inputs)], collapse = ", "),
    call. = FALSE
  )
}

cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
v17_require_columns(
  cells,
  c(
    "exact_site_id", "longitude", "latitude", "x_km", "y_km", "spatial_fold",
    "n_observations", "n_pigmented", "conditional_intensity_median",
    "broad50km_pc1", "broad50km_pc2", "within50km_pc1", "within50km_pc2"
  ),
  "cells"
)
presence <- v17_align_result(readRDS(presence_path), cells, "presence checkpoint")
intensity <- v17_align_result(readRDS(intensity_path), cells, "intensity checkpoint")
if (ncol(presence$draws) != ncol(intensity$draws) || ncol(presence$draws) < 1000L) {
  stop("Expected aligned presence/intensity checkpoints with at least 1000 draws.", call. = FALSE)
}

rank_finite <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (any(keep)) out[keep] <- rank(x[keep], ties.method = "average") / sum(keep)
  out
}

occurrence_ecdf_support <- function(site_value, occurrence_value) {
  occurrence_value <- sort(as.numeric(occurrence_value[is.finite(occurrence_value)]))
  out <- rep(NA_real_, length(site_value))
  keep <- is.finite(site_value)
  if (!length(occurrence_value)) return(out)
  out[keep] <- findInterval(site_value[keep], occurrence_value) / length(occurrence_value)
  out
}

# terra's SpatRaster,matrix extract method does not accept ID.  Keep the
# coordinate input as a numeric matrix and normalize the one-layer return shape
# explicitly so this source-build remains compatible with the pinned terra API.
extract_single_layer <- function(r, xy, method = "simple") {
  value <- terra::extract(r, xy, method = method)
  if (is.data.frame(value) || is.matrix(value)) {
    if (ncol(value) != 1L) {
      stop("Expected one extracted raster layer; found ", ncol(value), call. = FALSE)
    }
    return(as.numeric(value[, 1]))
  }
  as.numeric(value)
}

site_raw <- matrix(NA_real_, nrow(cells), length(species), dimnames = list(NULL, species))
site_cell_rank <- site_raw
site_occ_ecdf <- site_raw
calibration_rows <- list()

site_xy <- cbind(as.numeric(cells$longitude), as.numeric(cells$latitude))
for (j in seq_along(species)) {
  sh <- species[[j]]
  raster_path <- file.path(sdm_dir, paste0(sh, ".tif"))
  occurrence_path <- file.path(occurrence_dir, paste0(sh, "_gbif.csv"))
  if (!file.exists(raster_path)) stop("Missing rebuilt SDM: ", raster_path, call. = FALSE)
  if (!file.exists(occurrence_path)) stop("Missing occurrence snapshot: ", occurrence_path, call. = FALSE)

  r <- terra::rast(raster_path)
  site_value <- extract_single_layer(r, site_xy, method = "bilinear")
  site_raw[, j] <- site_value
  site_cell_rank[, j] <- rank_finite(site_value)

  occ <- readr::read_csv(occurrence_path, show_col_types = FALSE)
  required_occ <- c("key", "decimalLongitude", "decimalLatitude")
  missing_occ <- setdiff(required_occ, names(occ))
  if (length(missing_occ)) {
    stop(sh, " occurrence snapshot lacks: ", paste(missing_occ, collapse = ", "), call. = FALSE)
  }
  occ$decimalLongitude <- suppressWarnings(as.numeric(occ$decimalLongitude))
  occ$decimalLatitude <- suppressWarnings(as.numeric(occ$decimalLatitude))
  occ <- occ[
    is.finite(occ$decimalLongitude) & is.finite(occ$decimalLatitude),
    , drop = FALSE
  ]
  occ <- occ[order(as.character(occ$key)), , drop = FALSE]
  occ_xy <- cbind(occ$decimalLongitude, occ$decimalLatitude)
  occ_cell <- terra::cellFromXY(r, occ_xy)
  keep_occ <- !is.na(occ_cell)
  occ <- occ[keep_occ, , drop = FALSE]
  occ_cell <- occ_cell[keep_occ]
  keep_unique <- !duplicated(occ_cell)
  occ <- occ[keep_unique, , drop = FALSE]
  occ_value <- extract_single_layer(
    r, cbind(occ$decimalLongitude, occ$decimalLatitude), method = "simple"
  )
  occ_value <- occ_value[is.finite(occ_value)]
  if (length(occ_value) < 20L) {
    stop("Too few finite occurrence-cell predictions for ", sh, call. = FALSE)
  }
  site_occ_ecdf[, j] <- occurrence_ecdf_support(site_value, occ_value)

  calibration_rows[[j]] <- data.frame(
    species = sh,
    n_unique_occurrence_cells_with_prediction = length(occ_value),
    suitability_q10 = as.numeric(stats::quantile(occ_value, 0.10, names = FALSE)),
    suitability_q20 = as.numeric(stats::quantile(occ_value, 0.20, names = FALSE)),
    suitability_q25 = as.numeric(stats::quantile(occ_value, 0.25, names = FALSE)),
    suitability_q33 = as.numeric(stats::quantile(occ_value, 0.33, names = FALSE)),
    suitability_q50 = as.numeric(stats::quantile(occ_value, 0.50, names = FALSE)),
    stringsAsFactors = FALSE
  )
}

common_support <- apply(site_raw, 1, function(x) all(is.finite(x)))
cells$bombus_fingerprint_common_support <- common_support

cell_support <- data.frame(
  exact_site_id = cells$exact_site_id,
  longitude = cells$longitude,
  latitude = cells$latitude,
  common_rebuilt_sdm_support = common_support,
  stringsAsFactors = FALSE
)
for (j in seq_along(species)) {
  sh <- species[[j]]
  cell_support[[paste0(sh, "_suitability")]] <- site_raw[, j]
  cell_support[[paste0(sh, "_flower_cell_rank")]] <- site_cell_rank[, j]
  cell_support[[paste0(sh, "_occurrence_site_ecdf")]] <- site_occ_ecdf[, j]
}
readr::write_csv(cell_support, file.path(output_dir, "cell_bombus_support.csv"), na = "")
readr::write_csv(
  do.call(rbind, calibration_rows),
  file.path(output_dir, "occurrence_support_calibration.csv"), na = ""
)

# The local pair graph is built without flower colour or Bombus support values.
# Bombus support is used only after geography, fold membership and environmental
# similarity have defined the eligible response-blind edge set.
edges <- v17_pair_graph(
  cells, radius_km = 25, k = 5L,
  same_fold_only = TRUE, common_support_only = TRUE
)
n_edges_before_environment_gate <- nrow(edges)
env_matrix <- v17_environment_matrix(cells)
if (nrow(edges)) {
  edges$environmental_distance <- v17_pair_distance(env_matrix, edges$i, edges$j)
  edges <- edges[
    is.finite(edges$environmental_distance) & edges$environmental_distance <= env_match,
    , drop = FALSE
  ]
}

share <- as.numeric(cells$n_pigmented) / pmax(as.numeric(cells$n_observations), 1)
obs_intensity <- as.numeric(cells$conditional_intensity_median)
trials <- pmax(as.numeric(cells$n_observations), 1)
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
    a <- candidate$low_i[[r]]
    b <- candidate$high_i[[r]]
    if (!(a %in% used) && !(b %in% used)) {
      keep[[r]] <- TRUE
      used <- c(used, a, b)
    }
  }
  candidate[keep, , drop = FALSE]
}

normalization_matrix <- list(
  flower_cell_rank = site_cell_rank,
  occurrence_site_ecdf = site_occ_ecdf
)

summary_rows <- list()
pair_rows <- list()
null_rows <- list()
support_rows <- list()

for (normalization in normalizations) {
  support_source <- normalization_matrix[[normalization]]
  for (group_name in names(groups)) {
    group_species <- groups[[group_name]]
    group_index <- match(group_species, species)
    group_matrix <- support_source[, group_index, drop = FALSE]
    best_support <- apply(group_matrix, 1, max, na.rm = TRUE)
    all_missing <- apply(!is.finite(group_matrix), 1, all)
    best_support[all_missing] <- NA_real_

    support_rows[[length(support_rows) + 1L]] <- data.frame(
      normalization = normalization,
      exposure_group = group_name,
      species_set = paste(group_species, collapse = ";"),
      n_finite_cells = sum(is.finite(best_support)),
      support_q10 = as.numeric(stats::quantile(best_support, 0.10, na.rm = TRUE, names = FALSE)),
      support_q25 = as.numeric(stats::quantile(best_support, 0.25, na.rm = TRUE, names = FALSE)),
      support_q50 = as.numeric(stats::quantile(best_support, 0.50, na.rm = TRUE, names = FALSE)),
      support_q75 = as.numeric(stats::quantile(best_support, 0.75, na.rm = TRUE, names = FALSE)),
      support_q90 = as.numeric(stats::quantile(best_support, 0.90, na.rm = TRUE, names = FALSE)),
      n_available_ge_50 = sum(is.finite(best_support) & best_support >= available_threshold),
      stringsAsFactors = FALSE
    )

    for (low_threshold in low_thresholds) {
      candidate <- data.frame()
      if (nrow(edges)) {
        i <- edges$i
        j <- edges$j
        si <- best_support[i]
        sj <- best_support[j]
        take_ij <- is.finite(si) & is.finite(sj) &
          si <= low_threshold & sj >= available_threshold
        take_ji <- is.finite(si) & is.finite(sj) &
          sj <= low_threshold & si >= available_threshold
        keep <- take_ij | take_ji
        if (any(keep)) {
          candidate <- edges[keep, , drop = FALSE]
          take_ij_keep <- take_ij[keep]
          candidate$low_i <- ifelse(take_ij_keep, candidate$i, candidate$j)
          candidate$high_i <- ifelse(take_ij_keep, candidate$j, candidate$i)
          candidate$low_support <- best_support[candidate$low_i]
          candidate$high_support <- best_support[candidate$high_i]
          candidate <- greedy_match(candidate)
        }
      }

      if (nrow(candidate)) {
        candidate$normalization <- normalization
        candidate$exposure_group <- group_name
        candidate$species_set <- paste(group_species, collapse = ";")
        candidate$low_threshold <- low_threshold
        candidate$available_threshold <- available_threshold
        candidate$observed_presence_diff <- share[candidate$high_i] - share[candidate$low_i]
        candidate$observed_intensity_diff <- obs_intensity[candidate$high_i] - obs_intensity[candidate$low_i]
        candidate$intensity_pair <- is.finite(candidate$observed_intensity_diff)
        pair_rows[[length(pair_rows) + 1L]] <- candidate

        presence_observed <- mean(candidate$observed_presence_diff, na.rm = TRUE)
        presence_null <- colMeans(
          presence_draw_share[candidate$high_i, , drop = FALSE] -
            presence_draw_share[candidate$low_i, , drop = FALSE],
          na.rm = TRUE
        )
        p_cmp <- null_compare(presence_observed, presence_null)

        di <- candidate[candidate$intensity_pair, , drop = FALSE]
        if (nrow(di)) {
          intensity_observed <- mean(di$observed_intensity_diff, na.rm = TRUE)
          intensity_null <- colMeans(
            intensity$draws[di$high_i, , drop = FALSE] -
              intensity$draws[di$low_i, , drop = FALSE],
            na.rm = TRUE
          )
          i_cmp <- null_compare(intensity_observed, intensity_null)
        } else {
          intensity_observed <- NA_real_
          intensity_null <- numeric()
          i_cmp <- c(null_mean = NA_real_, upper_p = NA_real_, two_sided_p = NA_real_)
        }
      } else {
        presence_observed <- NA_real_
        presence_null <- numeric()
        p_cmp <- c(null_mean = NA_real_, upper_p = NA_real_, two_sided_p = NA_real_)
        intensity_observed <- NA_real_
        intensity_null <- numeric()
        i_cmp <- c(null_mean = NA_real_, upper_p = NA_real_, two_sided_p = NA_real_)
      }

      summary_rows[[length(summary_rows) + 1L]] <- data.frame(
        normalization = normalization,
        exposure_group = group_name,
        species_set = paste(group_species, collapse = ";"),
        low_threshold = low_threshold,
        available_threshold = available_threshold,
        response = "pigmentation_share",
        n_pairs = nrow(candidate),
        observed_directed_difference = presence_observed,
        natural_null_mean = p_cmp[["null_mean"]],
        upper_tail_p = p_cmp[["upper_p"]],
        two_sided_p = p_cmp[["two_sided_p"]],
        stringsAsFactors = FALSE
      )
      summary_rows[[length(summary_rows) + 1L]] <- data.frame(
        normalization = normalization,
        exposure_group = group_name,
        species_set = paste(group_species, collapse = ";"),
        low_threshold = low_threshold,
        available_threshold = available_threshold,
        response = "pigmented_only_intensity",
        n_pairs = if (nrow(candidate)) sum(candidate$intensity_pair) else 0L,
        observed_directed_difference = intensity_observed,
        natural_null_mean = i_cmp[["null_mean"]],
        upper_tail_p = i_cmp[["upper_p"]],
        two_sided_p = i_cmp[["two_sided_p"]],
        stringsAsFactors = FALSE
      )

      if (length(presence_null)) {
        null_rows[[length(null_rows) + 1L]] <- data.frame(
          normalization = normalization,
          exposure_group = group_name,
          low_threshold = low_threshold,
          response = "pigmentation_share",
          draw = seq_along(presence_null),
          statistic = presence_null,
          stringsAsFactors = FALSE
        )
      }
      if (length(intensity_null)) {
        null_rows[[length(null_rows) + 1L]] <- data.frame(
          normalization = normalization,
          exposure_group = group_name,
          low_threshold = low_threshold,
          response = "pigmented_only_intensity",
          draw = seq_along(intensity_null),
          statistic = intensity_null,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

summary <- do.call(rbind, summary_rows)
summary$BH_q_within_group_threshold <- NA_real_
summary$BH_q_within_group_grid <- NA_real_
for (normalization in unique(summary$normalization)) {
  for (group_name in unique(summary$exposure_group)) {
    group_rows <- summary$normalization == normalization & summary$exposure_group == group_name
    finite_group <- group_rows & is.finite(summary$upper_tail_p)
    summary$BH_q_within_group_grid[finite_group] <- stats::p.adjust(
      summary$upper_tail_p[finite_group], method = "BH"
    )
    for (threshold in low_thresholds) {
      idx <- group_rows & summary$low_threshold == threshold & is.finite(summary$upper_tail_p)
      summary$BH_q_within_group_threshold[idx] <- stats::p.adjust(
        summary$upper_tail_p[idx], method = "BH"
      )
    }
  }
}
summary$BH_q_all_sensitivity_tests <- NA_real_
finite_all <- is.finite(summary$upper_tail_p)
summary$BH_q_all_sensitivity_tests[finite_all] <- stats::p.adjust(
  summary$upper_tail_p[finite_all], method = "BH"
)

pairs <- if (length(pair_rows)) do.call(rbind, pair_rows) else data.frame()
null <- if (length(null_rows)) do.call(rbind, null_rows) else data.frame()
support_diagnostic <- do.call(rbind, support_rows)

readr::write_csv(summary, file.path(output_dir, "bombus_ecology_sensitivity_summary.csv"), na = "")
readr::write_csv(pairs, file.path(output_dir, "bombus_ecology_sensitivity_pairs.csv"), na = "")
readr::write_csv(null, file.path(output_dir, "bombus_ecology_sensitivity_null.csv"), na = "")
readr::write_csv(support_diagnostic, file.path(output_dir, "bombus_group_support_diagnostic.csv"), na = "")
readr::write_csv(data.frame(
  metric = c(
    "n_active_cells", "n_rebuilt_sdm_common_support_cells",
    "n_response_blind_edges_before_environment_gate",
    "n_response_blind_edges_after_environment_gate",
    "AUC_weighting_used"
  ),
  value = c(
    nrow(cells), sum(common_support), n_edges_before_environment_gate,
    nrow(edges), "FALSE"
  ),
  stringsAsFactors = FALSE
), file.path(output_dir, "bombus_ecology_sensitivity_design.csv"), na = "")

# Compact comparison table for manuscript decision-making.  These are
# sensitivities, not automatically promoted manuscript claims.
focus <- summary[
  summary$low_threshold == 0.33 &
    summary$response == "pigmentation_share" &
    summary$exposure_group %in% c("all_five", "widespread", "montane_alpine"),
  , drop = FALSE
]
readr::write_csv(focus, file.path(output_dir, "lower_third_presence_focus.csv"), na = "")

writeLines(
  c(
    "Ecology-aligned Bombus availability sensitivity.",
    "No flower-colour response is used to fit SDMs, standardize support, construct the local graph, select pairs, or orient pairs.",
    "flower_cell_rank exactly asks how low a species is relative to the active flower-cell support distribution.",
    "occurrence_site_ecdf instead calibrates a cell against the distribution of rebuilt SDM suitability at unique GBIF occurrence cells for that species.",
    "For occurrence_site_ecdf, 0.10 means the cell is no better supported than roughly the lowest 10% of that species' unique occurrence cells; 0.50 corresponds to the median occurrence-cell support.",
    "AUC is reported as an SDM diagnostic but is never used to weight focal species.",
    "all_five, widespread, montane_alpine, and leave-one-species-out exposures are retained regardless of sign or significance.",
    "The same 25-km, same-fold, environment-matched design and the same 1000 environment-plus-SPDE flower predictive maps are used as the reference null."
  ),
  file.path(output_dir, "README.txt")
)

cat("Completed ecology-aligned Bombus availability sensitivities at ", output_dir, "\n", sep = "")
