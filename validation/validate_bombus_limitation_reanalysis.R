#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
sys.source("R/local_bombus_turnover.R", envir = .GlobalEnv)
output_dir <- hb_arg_value(
  args, "--output", "results/ecological_v17_bombus_limitation_gate"
)
read_output <- function(name) hb_read_csv(file.path(output_dir, name))

summary <- read_output("bombus_limitation_gate_summary.csv")
eligibility <- read_output("bombus_limitation_gate_eligibility.csv")
rank_diag <- read_output("bombus_limitation_rank_diagnostic.csv")
metadata <- read_output("bombus_limitation_gate_metadata.csv")
interpretation <- read_output("interpretation_summary.csv")
cells <- hb_read_csv(
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
presence <- v17_align_result(
  readRDS(paste0(
    "results/ecological_v16_predictive_replication/checkpoints/",
    "national_environment_spde_presence_draws1000.rds"
  )), cells, "presence checkpoint"
)

meta <- setNames(metadata$value, metadata$field)
expected_thresholds <- c(0.10, 0.20, 0.25, 0.33)
expected_axes <- c(
  "broad50km_pc1", "broad50km_pc2", "within50km_pc1", "within50km_pc2"
)
species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
rank_columns <- paste0(species, "_within_species_rank")

checks <- list()
add <- function(check, pass, evidence) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check, status = if (isTRUE(pass)) "PASS" else "FAIL",
    evidence = evidence, stringsAsFactors = FALSE
  )
}

add(
  "fixed_gate_grid",
  identical(sort(unique(summary$low_threshold)), expected_thresholds),
  paste(sort(unique(summary$low_threshold)), collapse = ",")
)
add(
  "primary_gate_unchanged",
  abs(as.numeric(meta[["primary_low_threshold"]]) - 0.33) < 1e-12 &&
    abs(as.numeric(meta[["available_threshold"]]) - 0.50) < 1e-12,
  paste(meta[["primary_low_threshold"]], meta[["available_threshold"]], sep = " vs ")
)
add(
  "environment_caliper_unchanged",
  abs(as.numeric(meta[["environment_match_threshold"]]) - 0.75) < 1e-12,
  meta[["environment_match_threshold"]]
)
add(
  "fresh_environment_axes_present",
  all(expected_axes %in% names(cells)) &&
    all(vapply(cells[expected_axes], function(x) all(is.finite(x)), logical(1))),
  paste(expected_axes, collapse = ",")
)
add(
  "fresh_species_ranks_present",
  all(rank_columns %in% names(cells)) &&
    all(vapply(cells[rank_columns], function(x) all(is.finite(x)), logical(1))),
  paste(rank_columns, collapse = ",")
)

support <- as.matrix(cells[, rank_columns, drop = FALSE])
storage.mode(support) <- "double"
best <- apply(support, 1, max)
recomputed_min <- min(best)
add(
  "primary_limited_cells_absent",
  sum(best <= 0.33) == 0L,
  paste("n<=0.33=", sum(best <= 0.33), "; min=", format(recomputed_min, digits = 16))
)
add(
  "all_retained_low_gates_absent",
  all(vapply(expected_thresholds, function(x) sum(best <= x), integer(1)) == 0L),
  paste(vapply(expected_thresholds, function(x) sum(best <= x), integer(1)), collapse = ",")
)

edges <- v17_pair_graph(
  cells, radius_km = 25, k = 5L,
  same_fold_only = TRUE, common_support_only = TRUE
)
edges <- v17_add_pair_features(
  edges, cells, presence,
  v17_align_result(
    readRDS(paste0(
      "results/ecological_v16_predictive_replication/checkpoints/",
      "national_environment_spde_intensity_draws1000.rds"
    )), cells, "intensity checkpoint"
  )
)
edges <- edges[
  is.finite(edges$environmental_distance) & edges$environmental_distance <= 0.75,
  , drop = FALSE
]
recomputed_candidate_counts <- vapply(expected_thresholds, function(low) {
  si <- best[edges$i]
  sj <- best[edges$j]
  sum(
    (si <= low & sj >= 0.50) |
      (sj <= low & si >= 0.50)
  )
}, integer(1))
add(
  "oriented_pairs_absent_after_fixed_matching",
  all(recomputed_candidate_counts == 0L),
  paste(recomputed_candidate_counts, collapse = ",")
)
add(
  "eligibility_table_recalculation",
  identical(as.integer(eligibility$n_oriented_candidate_edges_after_25km_same_fold_env_match),
            as.integer(recomputed_candidate_counts)),
  paste(eligibility$n_oriented_candidate_edges_after_25km_same_fold_env_match, collapse = ",")
)
add(
  "summary_reports_zero_pairs_without_p_values",
  all(summary$n_pairs == 0L) &&
    all(is.na(summary$upper_tail_p)) &&
    all(is.na(summary$BH_q_all_gate_tests)),
  paste("rows=", nrow(summary), "; pair_sum=", sum(summary$n_pairs))
)
add(
  "rank_diagnostic_min_recalculation",
  abs(as.numeric(rank_diag$value[rank_diag$metric == "best_support_min"]) - recomputed_min) < 1e-12,
  format(recomputed_min, digits = 16)
)
add(
  "fresh_flower_draws_retained",
  ncol(presence$draws) == 1000L,
  paste("draws=", ncol(presence$draws))
)
add(
  "non_estimability_claim_ceiling",
  any(interpretation$field == "status" &
        interpretation$value == "not_estimable_no_all_species_low_cells") &&
    any(grepl("not evidence", interpretation$value, fixed = TRUE)),
  "non-estimability is not converted to evidence for/against causal selection"
)

validation <- do.call(rbind, checks)
utils::write.csv(
  validation,
  file.path(output_dir, "bombus_limitation_reanalysis_validation.csv"),
  row.names = FALSE
)
print(validation)
if (any(validation$status != "PASS")) {
  stop("Rebuilt-SDM Bombus limitation reanalysis validation failed.", call. = FALSE)
}
cat("Rebuilt-SDM Bombus limitation reanalysis validation passed.\n")
