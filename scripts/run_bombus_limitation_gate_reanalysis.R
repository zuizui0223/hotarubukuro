#!/usr/bin/env Rscript

# Reanalysis-only compatibility runner for the current stage-03 Bombus gate.
# It uses the repository's current pair helpers and the unchanged scientific
# thresholds. If at least one configured gate is estimable, the canonical stage
# is evaluated unchanged (apart from explicitly sourcing the current helper).
# If every configured all-species-low gate has zero eligible pairs, the runner
# records that non-estimability as a scientific result instead of crashing while
# constructing empty result tables. It never relaxes or selects a threshold.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
sys.source("R/local_bombus_turnover.R", envir = .GlobalEnv)
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
  stop(
    "Missing fresh reanalysis inputs: ",
    paste(required[!file.exists(required)], collapse = ", "), call. = FALSE
  )
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

count_oriented_candidates <- function(low_threshold) {
  i <- edges$i
  j <- edges$j
  si <- cells$best_bombus_support_rank[i]
  sj <- cells$best_bombus_support_rank[j]
  take_ij <- is.finite(si) & si <= low_threshold &
    is.finite(sj) & sj >= available_threshold
  take_ji <- is.finite(sj) & sj <= low_threshold &
    is.finite(si) & si >= available_threshold
  sum(take_ij | take_ji)
}
candidate_counts <- vapply(low_thresholds, count_oriented_candidates, integer(1))

# If any configured threshold is estimable, execute the canonical stage code.
if (any(candidate_counts > 0L)) {
  script_path <- "scripts/run_bombus_limitation_gate.R"
  code <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
  needle <- 'hb_load_modules("bombus_limitation_gate")'
  hit <- which(trimws(code) == needle)
  if (length(hit) != 1L) {
    stop("Expected exactly one Bombus limitation module-loading line.", call. = FALSE)
  }
  code[hit] <- 'sys.source("R/local_bombus_turnover.R", envir = .GlobalEnv)'
  eval(parse(text = code, keep.source = FALSE), envir = .GlobalEnv)
  quit(save = "no", status = 0L)
}

# No configured all-five-low endpoint exists. Preserve the complete fixed grid
# and report NA inferential statistics rather than changing the exposure rule.
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
responses <- c("pigmentation_share", "pigmented_only_intensity")
summary <- do.call(rbind, lapply(low_thresholds, function(threshold) {
  do.call(rbind, lapply(responses, function(response) {
    data.frame(
      low_threshold = threshold,
      available_threshold = available_threshold,
      is_primary_gate = abs(threshold - primary_low_threshold) < 1e-12,
      response = response,
      n_pairs = 0L,
      observed_directed_difference = NA_real_,
      natural_null_mean = NA_real_,
      upper_tail_p = NA_real_,
      two_sided_p = NA_real_,
      BH_q_within_threshold = NA_real_,
      BH_q_all_gate_tests = NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}))
utils::write.csv(
  summary, file.path(output_dir, "bombus_limitation_gate_summary.csv"),
  row.names = FALSE
)

empty_pairs <- edges[FALSE, , drop = FALSE]
for (nm in c(
  "low_i", "high_i", "low_support", "high_support",
  "low_threshold", "available_threshold", "is_primary_gate",
  "observed_presence_diff", "observed_intensity_diff", "intensity_pair"
)) {
  empty_pairs[[nm]] <- if (nm %in% c("is_primary_gate", "intensity_pair")) logical() else numeric()
}
utils::write.csv(
  empty_pairs, file.path(output_dir, "bombus_limitation_gate_pairs.csv"),
  row.names = FALSE
)
empty_null <- data.frame(
  low_threshold = numeric(), response = character(),
  draw = integer(), statistic = numeric(), stringsAsFactors = FALSE
)
utils::write.csv(
  empty_null, file.path(output_dir, "bombus_limitation_gate_null.csv"),
  row.names = FALSE
)

available_cells <- is.finite(cells$best_bombus_support_rank) &
  cells$best_bombus_support_rank >= available_threshold &
  is.finite(cells$conditional_intensity_median)
intensity_gradient <- data.frame(
  n_cells = sum(available_cells),
  spearman_rho = if (sum(available_cells) >= 10L) {
    suppressWarnings(stats::cor(
      cells$best_bombus_support_rank[available_cells],
      cells$conditional_intensity_median[available_cells], method = "spearman"
    ))
  } else NA_real_,
  stringsAsFactors = FALSE
)
utils::write.csv(
  intensity_gradient,
  file.path(output_dir, "available_zone_intensity_gradient.csv"),
  row.names = FALSE
)

rank_quantiles <- stats::quantile(
  cells$best_bombus_support_rank,
  probs = c(0, 0.01, 0.05, 0.10, 0.20, 0.25, 0.33, 0.50, 0.75, 1),
  na.rm = TRUE, names = FALSE
)
rank_diagnostic <- data.frame(
  metric = c(
    "n_cells", "finite_best_support", "best_support_min",
    "n_best_le_0.10", "n_best_le_0.20", "n_best_le_0.25",
    "n_best_le_0.33", "n_best_le_0.50",
    paste0("best_support_q", c("00", "01", "05", "10", "20", "25", "33", "50", "75", "100"))
  ),
  value = c(
    nrow(cells), sum(is.finite(cells$best_bombus_support_rank)),
    min(cells$best_bombus_support_rank, na.rm = TRUE),
    sum(cells$best_bombus_support_rank <= 0.10, na.rm = TRUE),
    sum(cells$best_bombus_support_rank <= 0.20, na.rm = TRUE),
    sum(cells$best_bombus_support_rank <= 0.25, na.rm = TRUE),
    sum(cells$best_bombus_support_rank <= 0.33, na.rm = TRUE),
    sum(cells$best_bombus_support_rank <= 0.50, na.rm = TRUE),
    rank_quantiles
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  rank_diagnostic,
  file.path(output_dir, "bombus_limitation_rank_diagnostic.csv"),
  row.names = FALSE
)

cor_matrix <- suppressWarnings(stats::cor(support_matrix, method = "spearman"))
cor_long <- do.call(rbind, lapply(seq_len(nrow(cor_matrix)), function(i) {
  data.frame(
    species_a = species[[i]], species_b = species,
    spearman_rho = as.numeric(cor_matrix[i, ]), stringsAsFactors = FALSE
  )
}))
utils::write.csv(
  cor_long,
  file.path(output_dir, "bombus_species_rank_correlations.csv"),
  row.names = FALSE
)

gate_counts <- data.frame(
  low_threshold = low_thresholds,
  n_cells_all_five_low = vapply(
    low_thresholds,
    function(x) sum(cells$best_bombus_support_rank <= x, na.rm = TRUE), integer(1)
  ),
  n_oriented_candidate_edges_after_25km_same_fold_env_match = candidate_counts,
  stringsAsFactors = FALSE
)
utils::write.csv(
  gate_counts, file.path(output_dir, "bombus_limitation_gate_eligibility.csv"),
  row.names = FALSE
)

interpretation <- data.frame(
  field = c(
    "status", "ecological_hypothesis", "non_estimability_reason",
    "primary_gate", "primary_gate_history", "environment_control",
    "spatial_control", "flower_null_role", "claim_ceiling"
  ),
  value = c(
    "not_estimable_no_all_species_low_cells",
    paste(
      "same fixed hypothesis as the active gate: low focal-Bombus availability",
      "would relax the attraction benefit of pigmentation"
    ),
    paste0(
      "under the rebuilt SDMs the minimum max-within-species support rank across ",
      nrow(cells), " fresh flower cells is ",
      format(min(cells$best_bombus_support_rank, na.rm = TRUE), digits = 8),
      "; therefore no cell satisfies the fixed all-five <=0.33 primary gate"
    ),
    paste(
      "unchanged lower-third gate:", primary_low_threshold,
      "versus at least one species at or above", available_threshold
    ),
    paste(
      "the complete 0.10/0.20/0.25/0.33 grid is retained; no threshold was",
      "altered after seeing the rebuilt SDMs"
    ),
    paste("response-blind local graph uses environmental distance <=", env_match),
    "25-km same-heldout-fold graph; no second local SPDE fit",
    paste(
      "the 1000 fresh flower natural maps exist but cannot be replayed because",
      "the fixed Bombus gate defines no matched pairs"
    ),
    paste(
      "non-estimability is evidence about the exposure geometry, not evidence",
      "for or against pollinator-mediated selection"
    )
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  interpretation, file.path(output_dir, "interpretation_summary.csv"),
  row.names = FALSE
)

metadata <- data.frame(
  field = c(
    "analysis_spec_version", "pair_radius_km", "environment_match_threshold",
    "primary_low_threshold", "available_threshold", "sensitivity_low_thresholds",
    "pair_reuse", "pair_orientation", "local_environment_model",
    "local_spatial_model", "n_flower_predictive_draws",
    "bombus_surface_role", "bombus_uncertainty", "inference_role"
  ),
  value = c(
    "v17.2_bombus_limitation_gate_reanalysis_nonestimable", "25",
    as.character(env_match), as.character(primary_low_threshold),
    as.character(available_threshold), paste(low_thresholds, collapse = ","),
    "one-to-one if eligible pairs exist",
    "Bombus-limited to Bombus-available; flower colour is never used",
    "none; environment is controlled by pre-outcome local matching",
    "none; broad space is restricted by local radius and same held-out fold",
    as.character(ncol(presence$draws)),
    "newly rebuilt seeded Bombus prediction ranks",
    "fixed selected SDM surfaces from repeated-build-validated source artifact",
    "same local mechanistic sensitivity; scientifically non-estimable under rebuilt exposure geometry"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  metadata, file.path(output_dir, "bombus_limitation_gate_metadata.csv"),
  row.names = FALSE
)

writeLines(c(
  "# Bombus limitation gate: non-estimable under rebuilt SDMs",
  "",
  paste0(
    "The fixed all-five-low gate was applied without changing its thresholds. ",
    "Across ", nrow(cells), " fresh 1-km flower cells, the minimum of the maximum ",
    "within-species support rank was ",
    format(min(cells$best_bombus_support_rank, na.rm = TRUE), digits = 8), "."
  ),
  "",
  paste0(
    "Therefore none of the retained low thresholds (",
    paste(low_thresholds, collapse = ", "),
    ") produced a Bombus-limited endpoint or matched pair."
  ),
  "",
  "No threshold was relaxed and no inferential p-value was manufactured. This outcome indicates that the original all-five-low exposure is structurally incompatible with the newly rebuilt species surfaces on the fresh flower-cell support."
), file.path(output_dir, "README.md"))

print(gate_counts)
print(rank_diagnostic)
cat("Bombus limitation gate is scientifically non-estimable under the unchanged rebuilt-SDM thresholds.\n")
