#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
cat("checkpoint=source_pipeline_support PASS\n")
sys.source("R/local_bombus_turnover.R", envir = .GlobalEnv)
cat("checkpoint=source_current_pair_helper PASS\n")

artifact_root <- "."
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

cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
cat("checkpoint=read_cells n=", nrow(cells), " p=", ncol(cells), "\n", sep = "")
presence_raw <- readRDS(presence_path)
intensity_raw <- readRDS(intensity_path)
cat(
  "checkpoint=read_rds presence_names=", paste(names(presence_raw), collapse = ";"),
  " intensity_names=", paste(names(intensity_raw), collapse = ";"), "\n", sep = ""
)
cat(
  "raw_dims presence_draws=", paste(dim(presence_raw$draws), collapse = "x"),
  " intensity_draws=", paste(dim(intensity_raw$draws), collapse = "x"),
  " presence_cell_id=", length(presence_raw$cell_id),
  " intensity_cell_id=", length(intensity_raw$cell_id), "\n", sep = ""
)

presence <- v17_align_result(presence_raw, cells, "presence checkpoint")
cat(
  "checkpoint=align_presence draws=", paste(dim(presence$draws), collapse = "x"),
  " observed=", length(presence$observed),
  " trials=", length(presence$trials),
  " latent=", length(presence$latent_mean), "\n", sep = ""
)
intensity <- v17_align_result(intensity_raw, cells, "intensity checkpoint")
cat(
  "checkpoint=align_intensity draws=", paste(dim(intensity$draws), collapse = "x"),
  " observed=", length(intensity$observed),
  " trials=", length(intensity$trials),
  " latent=", length(intensity$latent_mean), "\n", sep = ""
)

species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
rank_columns <- paste0(species, "_within_species_rank")
support_matrix <- as.matrix(cells[, rank_columns, drop = FALSE])
storage.mode(support_matrix) <- "double"
cells$best_bombus_support_rank <- apply(support_matrix, 1, max, na.rm = TRUE)
all_missing <- apply(!is.finite(support_matrix), 1, all)
cells$best_bombus_support_rank[all_missing] <- NA_real_
cat(
  "checkpoint=support_ranks finite=", sum(is.finite(cells$best_bombus_support_rank)),
  " min=", min(cells$best_bombus_support_rank, na.rm = TRUE),
  " median=", stats::median(cells$best_bombus_support_rank, na.rm = TRUE),
  " max=", max(cells$best_bombus_support_rank, na.rm = TRUE), "\n", sep = ""
)

edges0 <- v17_pair_graph(
  cells, radius_km = 25, k = 5L,
  same_fold_only = TRUE, common_support_only = TRUE
)
cat("checkpoint=pair_graph edges=", nrow(edges0), " cols=", ncol(edges0), "\n", sep = "")
edges1 <- v17_add_pair_features(edges0, cells, presence, intensity)
cat(
  "checkpoint=pair_features edges=", nrow(edges1),
  " finite_env=", sum(is.finite(edges1$environmental_distance)),
  " finite_fp=", sum(is.finite(edges1$fingerprint_turnover)), "\n", sep = ""
)
edges <- edges1[
  is.finite(edges1$environmental_distance) & edges1$environmental_distance <= 0.75,
  , drop = FALSE
]
cat("checkpoint=environment_caliper edges=", nrow(edges), "\n", sep = "")

share <- cells$n_pigmented / pmax(cells$n_observations, 1)
obs_intensity <- cells$conditional_intensity_median
trials <- pmax(cells$n_observations, 1)
presence_draw_share <- sweep(presence$draws, 1, trials, "/")
cat(
  "checkpoint=draw_share dims=", paste(dim(presence_draw_share), collapse = "x"),
  " intensity_dims=", paste(dim(intensity$draws), collapse = "x"), "\n", sep = ""
)

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

make_oriented_pairs <- function(low_threshold, available_threshold = 0.50) {
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
  cat(
    "threshold=", low_threshold,
    " oriented_candidate_edges=", sum(keep), "\n", sep = ""
  )
  if (!any(keep)) return(data.frame())
  d <- edges[keep, , drop = FALSE]
  take_ij <- take_ij[keep]
  d$low_i <- ifelse(take_ij, d$i, d$j)
  d$high_i <- ifelse(take_ij, d$j, d$i)
  d$low_support <- cells$best_bombus_support_rank[d$low_i]
  d$high_support <- cells$best_bombus_support_rank[d$high_i]
  d$low_threshold <- low_threshold
  d$available_threshold <- available_threshold
  d$is_primary_gate <- abs(low_threshold - 0.33) < 1e-12
  greedy_match(d)
}

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

summaries <- list()
pair_tables <- list()
null_tables <- list()
for (low_threshold in c(0.10, 0.20, 0.25, 0.33)) {
  d <- make_oriented_pairs(low_threshold)
  key <- format(low_threshold, trim = TRUE)
  cat("threshold=", low_threshold, " matched_pairs=", nrow(d), "\n", sep = "")
  if (!nrow(d)) next
  d$observed_presence_diff <- share[d$high_i] - share[d$low_i]
  d$observed_intensity_diff <- obs_intensity[d$high_i] - obs_intensity[d$low_i]
  d$intensity_pair <- is.finite(d$observed_intensity_diff)
  cat(
    "threshold=", low_threshold,
    " intensity_pairs=", sum(d$intensity_pair), "\n", sep = ""
  )

  presence_observed <- mean(d$observed_presence_diff, na.rm = TRUE)
  presence_null <- colMeans(
    presence_draw_share[d$high_i, , drop = FALSE] -
      presence_draw_share[d$low_i, , drop = FALSE],
    na.rm = TRUE
  )
  p_cmp <- null_compare(presence_observed, presence_null)
  cat(
    "threshold=", low_threshold,
    " presence_null_len=", length(presence_null),
    " pcmp_len=", length(p_cmp), "\n", sep = ""
  )

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
  cat(
    "threshold=", low_threshold,
    " intensity_null_len=", length(intensity_null),
    " icmp_len=", length(i_cmp), "\n", sep = ""
  )

  cat("threshold=", low_threshold, " before_presence_summary\n", sep = "")
  summaries[[paste0(key, "_presence")]] <- data.frame(
    low_threshold = low_threshold,
    available_threshold = 0.50,
    is_primary_gate = abs(low_threshold - 0.33) < 1e-12,
    response = "pigmentation_share",
    n_pairs = nrow(d),
    observed_directed_difference = presence_observed,
    natural_null_mean = p_cmp[["null_mean"]],
    upper_tail_p = p_cmp[["upper_p"]],
    two_sided_p = p_cmp[["two_sided_p"]],
    stringsAsFactors = FALSE
  )
  cat("threshold=", low_threshold, " after_presence_summary\n", sep = "")

  summaries[[paste0(key, "_intensity")]] <- data.frame(
    low_threshold = low_threshold,
    available_threshold = 0.50,
    is_primary_gate = abs(low_threshold - 0.33) < 1e-12,
    response = "pigmented_only_intensity",
    n_pairs = sum(d$intensity_pair),
    observed_directed_difference = intensity_observed,
    natural_null_mean = i_cmp[["null_mean"]],
    upper_tail_p = i_cmp[["upper_p"]],
    two_sided_p = i_cmp[["two_sided_p"]],
    stringsAsFactors = FALSE
  )
  cat("threshold=", low_threshold, " after_intensity_summary\n", sep = "")

  pair_tables[[key]] <- d
  null_tables[[paste0(key, "_presence")]] <- data.frame(
    low_threshold = low_threshold, response = "pigmentation_share",
    draw = seq_along(presence_null), statistic = presence_null
  )
  cat("threshold=", low_threshold, " after_presence_null_table\n", sep = "")
  null_tables[[paste0(key, "_intensity")]] <- data.frame(
    low_threshold = low_threshold, response = "pigmented_only_intensity",
    draw = seq_along(intensity_null), statistic = intensity_null
  )
  cat("threshold=", low_threshold, " after_intensity_null_table\n", sep = "")
}

cat("checkpoint=before_rbind summaries=", length(summaries), " pairs=", length(pair_tables), " nulls=", length(null_tables), "\n", sep = "")
summary <- do.call(rbind, summaries)
pairs <- do.call(rbind, pair_tables)
null <- do.call(rbind, null_tables)
cat(
  "checkpoint=after_rbind summary_rows=", if (is.null(summary)) 0 else nrow(summary),
  " pair_rows=", if (is.null(pairs)) 0 else nrow(pairs),
  " null_rows=", if (is.null(null)) 0 else nrow(null), "\n", sep = ""
)
cat("DIAGNOSTIC PASS\n")
