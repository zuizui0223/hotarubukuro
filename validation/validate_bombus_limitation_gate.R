args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
output_dir <- hb_arg_value(
  args, "--output", "results/ecological_v17_bombus_limitation_gate"
)

read_output <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing output: ", path, call. = FALSE)
  hb_read_csv(path)
}
summary <- read_output("bombus_limitation_gate_summary.csv")
pairs <- read_output("bombus_limitation_gate_pairs.csv")
null <- read_output("bombus_limitation_gate_null.csv")
metadata <- read_output("bombus_limitation_gate_metadata.csv")
interpretation <- read_output("interpretation_summary.csv")
cells <- hb_read_csv(
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)

meta <- setNames(metadata$value, metadata$field)
primary_low <- as.numeric(meta[["primary_low_threshold"]])
available <- as.numeric(meta[["available_threshold"]])
env_match <- as.numeric(meta[["environment_match_threshold"]])

checks <- list()
add <- function(check, pass, evidence) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check, status = if (isTRUE(pass)) "PASS" else "FAIL",
    evidence = evidence, stringsAsFactors = FALSE
  )
}

primary_pairs <- pairs[abs(pairs$low_threshold - primary_low) < 1e-12, , drop = FALSE]
primary_presence <- summary[
  abs(summary$low_threshold - primary_low) < 1e-12 &
    summary$response == "pigmentation_share", , drop = FALSE
]
primary_intensity <- summary[
  abs(summary$low_threshold - primary_low) < 1e-12 &
    summary$response == "pigmented_only_intensity", , drop = FALSE
]

add("primary_rows_unique", nrow(primary_presence) == 1L && nrow(primary_intensity) == 1L,
    paste("presence=", nrow(primary_presence), "intensity=", nrow(primary_intensity)))
add("primary_pair_count_matches", nrow(primary_pairs) == primary_presence$n_pairs,
    paste("pairs=", nrow(primary_pairs), "summary=", primary_presence$n_pairs))
add("one_to_one_pairs",
    !anyDuplicated(c(primary_pairs$low_i, primary_pairs$high_i)),
    paste("endpoints=", 2L * nrow(primary_pairs)))
add("local_radius", all(primary_pairs$geographic_distance_km <= 25 + 1e-12),
    paste("max km=", max(primary_pairs$geographic_distance_km)))
add("environment_caliper",
    all(primary_pairs$environmental_distance <= env_match + 1e-12),
    paste("max env distance=", max(primary_pairs$environmental_distance)))
add("same_fold", all(primary_pairs$fold_i == primary_pairs$fold_j),
    paste("cross-fold=", sum(primary_pairs$fold_i != primary_pairs$fold_j)))
add("gate_orientation",
    all(primary_pairs$low_support <= primary_low + 1e-12) &&
      all(primary_pairs$high_support >= available - 1e-12),
    paste("low max=", max(primary_pairs$low_support),
          "high min=", min(primary_pairs$high_support)))

share <- cells$n_pigmented / pmax(cells$n_observations, 1)
observed <- mean(share[primary_pairs$high_i] - share[primary_pairs$low_i])
add("observed_presence_recalculation",
    abs(observed - primary_presence$observed_directed_difference) < 1e-12,
    paste("recomputed=", observed,
          "recorded=", primary_presence$observed_directed_difference))

primary_null <- null[
  abs(null$low_threshold - primary_low) < 1e-12 &
    null$response == "pigmentation_share", , drop = FALSE
]
recomputed_upper <- (1 + sum(primary_null$statistic >= observed)) /
  (nrow(primary_null) + 1)
add("predictive_draw_count", nrow(primary_null) == 1000L,
    paste("draws=", nrow(primary_null)))
add("upper_tail_recalculation",
    abs(recomputed_upper - primary_presence$upper_tail_p) < 1e-12,
    paste("recomputed=", recomputed_upper,
          "recorded=", primary_presence$upper_tail_p))

for (threshold in unique(summary$low_threshold)) {
  idx <- abs(summary$low_threshold - threshold) < 1e-12 &
    is.finite(summary$upper_tail_p)
  q <- stats::p.adjust(summary$upper_tail_p[idx], method = "BH")
  add(
    paste0("within_gate_BH_", threshold),
    max(abs(q - summary$BH_q_within_threshold[idx])) < 1e-12,
    paste("max diff=", max(abs(q - summary$BH_q_within_threshold[idx])))
  )
}
all_q <- stats::p.adjust(summary$upper_tail_p, method = "BH")
add("across_grid_BH",
    all((is.na(all_q) & is.na(summary$BH_q_all_gate_tests)) |
          abs(all_q - summary$BH_q_all_gate_tests) < 1e-12, na.rm = TRUE),
    "across-grid BH recomputed")
add("claim_ceiling_present",
    any(grepl("not abundance", interpretation$value, fixed = TRUE)) ||
      any(grepl("not abundance", metadata$value, fixed = TRUE)),
    "predicted availability is not visitation/selection")
add("post_development_history_recorded",
    any(grepl("after exploratory design development", interpretation$value, fixed = TRUE)),
    "primary-gate history is explicit")

validation <- do.call(rbind, checks)
write.csv(
  validation,
  file.path(output_dir, "bombus_limitation_gate_independent_validation.csv"),
  row.names = FALSE
)
print(validation)
if (any(validation$status != "PASS")) {
  stop("Bombus limitation-gate validation failed.", call. = FALSE)
}
cat("Bombus limitation-gate independent validation passed.\n")
