#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  if (hit[length(hit)] == length(args)) stop("Missing value after ", flag, call. = FALSE)
  args[hit[length(hit)] + 1L]
}

cells_path <- arg_value("--cells")
pairs_path <- arg_value("--pairs")
output_dir <- arg_value("--output", "results/bombus_final8_environment_audit")
if (is.null(cells_path) || is.null(pairs_path)) stop("--cells and --pairs are required", call. = FALSE)
if (!file.exists(cells_path) || !file.exists(pairs_path)) stop("Missing audit input", call. = FALSE)

source("R/local_pair_graph.R")
cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
pairs <- utils::read.csv(pairs_path, check.names = FALSE, stringsAsFactors = FALSE)
final8 <- c(
  "env_Temperature_PC1", "env_precip_PC1", "env_TemperatureSeasonality",
  "env_PrecipSeasonality", "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS"
)
legacy4 <- c("broad50km_pc1", "broad50km_pc2", "within50km_pc1", "within50km_pc2")
lp_require_columns(cells, c("exact_site_id", "x_km", "y_km", "bombus_fingerprint_common_support", final8, legacy4), "cells")
lp_require_columns(pairs, c("radius_km", "transition_threshold", "white_site", "pigmented_site"), "pairs")

scale_matrix <- function(data, cols) {
  x <- as.matrix(data[, cols, drop = FALSE]); storage.mode(x) <- "double"
  x <- apply(x, 2, lp_scale_vector)
  if (!is.matrix(x)) x <- matrix(x, ncol = length(cols))
  colnames(x) <- cols
  x
}
X8 <- scale_matrix(cells, final8)
X4 <- scale_matrix(cells, legacy4)
idx <- setNames(seq_len(nrow(cells)), as.character(cells$exact_site_id))
wi <- unname(idx[as.character(pairs$white_site)])
pi <- unname(idx[as.character(pairs$pigmented_site)])
if (anyNA(wi) || anyNA(pi)) stop("Pair IDs do not align to cells", call. = FALSE)
pairs$environmental_distance_final8 <- lp_pair_distance(X8, wi, pi)
pairs$environmental_distance_legacy4 <- lp_pair_distance(X4, wi, pi)

radii <- sort(unique(as.numeric(pairs$radius_km)))
rows <- list(); rid <- 0L
for (radius in radii) {
  graph <- lp_pair_graph(cells, radius_km = radius, k = 5L, same_fold_only = FALSE, common_support_only = TRUE)
  if (!nrow(graph)) next
  gi <- as.integer(graph$i); gj <- as.integer(graph$j)
  all8 <- lp_pair_distance(X8, gi, gj)
  all4 <- lp_pair_distance(X4, gi, gj)
  thresholds <- sort(unique(as.numeric(pairs$transition_threshold[pairs$radius_km == radius])), decreasing = TRUE)
  for (threshold in thresholds) {
    use <- as.numeric(pairs$radius_km) == radius & abs(as.numeric(pairs$transition_threshold) - threshold) < 1e-12
    rid <- rid + 1L
    rows[[rid]] <- data.frame(
      radius_km = radius,
      transition_threshold = threshold,
      n_nonoverlapping_pairs = sum(use),
      median_selected_final8 = stats::median(pairs$environmental_distance_final8[use], na.rm = TRUE),
      median_all_local_edges_final8 = stats::median(all8, na.rm = TRUE),
      selected_to_all_ratio_final8 = stats::median(pairs$environmental_distance_final8[use], na.rm = TRUE) / stats::median(all8, na.rm = TRUE),
      median_selected_legacy4 = stats::median(pairs$environmental_distance_legacy4[use], na.rm = TRUE),
      median_all_local_edges_legacy4 = stats::median(all4, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
}
summary <- do.call(rbind, rows)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(summary, file.path(output_dir, "bombus_pair_environment_distance_audit.csv"), row.names = FALSE)
utils::write.csv(pairs, file.path(output_dir, "bombus_pairs_with_final8_distance.csv"), row.names = FALSE)

focal <- summary[summary$radius_km == 5 & abs(summary$transition_threshold - 1) < 1e-12, , drop = FALSE]
if (nrow(focal) != 1L) stop("Expected one focal 5-km pure-transition row", call. = FALSE)
status <- if (is.finite(focal$median_selected_final8) && is.finite(focal$median_all_local_edges_final8) && focal$median_selected_final8 < focal$median_all_local_edges_final8) "PASS" else "REVIEW"
writeLines(c(
  paste0("status=", status),
  paste0("focal_n_pairs=", focal$n_nonoverlapping_pairs),
  paste0("focal_final8_selected_median=", format(focal$median_selected_final8, digits = 10)),
  paste0("focal_final8_all_edges_median=", format(focal$median_all_local_edges_final8, digits = 10)),
  paste0("focal_final8_ratio=", format(focal$selected_to_all_ratio_final8, digits = 10))
), file.path(output_dir, "validation.txt"))
if (status != "PASS") stop("Final8 diagnostic does not show selected pairs closer than all local edges", call. = FALSE)
cat("Final8 Bombus environmental-distance audit PASS\n")
print(focal)
