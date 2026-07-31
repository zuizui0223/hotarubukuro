args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/candidate_null_tools.R")
source("R/local_pigmented_isolates.R")
source("R/local_state_asymmetry.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
checkpoint_path <- arg_value(
  "--presence-checkpoint",
  paste0(
    "results/ecological_v16_predictive_replication/checkpoints/",
    "national_environment_spde_presence_draws1000.rds"
  )
)
output_dir <- arg_value(
  "--output", "results/ecological_v23_local_state_asymmetry"
)
max_draws <- as.integer(arg_value("--max-draws", "1000"))
pseudocount <- as.numeric(arg_value("--pseudocount", "0.5"))

if (!file.exists(cells_path)) stop("Missing cells file: ", cells_path)
if (!file.exists(checkpoint_path)) {
  stop("Missing presence checkpoint: ", checkpoint_path)
}
if (!is.finite(pseudocount) || pseudocount <= 0) {
  stop("--pseudocount must be finite and positive.")
}

cells <- utils::read.csv(
  cells_path, check.names = FALSE, stringsAsFactors = FALSE
)
v23_require_columns(
  cells,
  c(
    "exact_site_id", "n_pigmented", "n_observations",
    "n_independent_sites", "x_km", "y_km", "spatial_fold",
    "broad50km_pc1", "broad50km_pc2",
    "within50km_pc1", "within50km_pc2"
  ),
  "cells"
)
presence <- v18_align_result(
  readRDS(checkpoint_path), cells, "presence"
)
simulated_counts <- presence$draws
if (is.finite(max_draws) && max_draws > 0L &&
    ncol(simulated_counts) > max_draws) {
  simulated_counts <- simulated_counts[, seq_len(max_draws), drop = FALSE]
}
observed_counts <- matrix(as.numeric(cells$n_pigmented), ncol = 1L)
trials <- pmax(as.numeric(cells$n_observations), 1)

configurations <- v20_configuration_table()
primary_configuration <- configurations[
  configurations$role == "primary", , drop = FALSE
]
if (nrow(primary_configuration) != 1L) {
  stop("Expected exactly one primary local-isolate configuration.")
}
graph <- v20_neighbour_graph(
  cells,
  radius_km = primary_configuration$radius_km,
  environment_caliper = primary_configuration$environment_caliper,
  minimum_neighbours = primary_configuration$minimum_neighbours,
  same_fold_only = primary_configuration$same_fold_only
)

rules <- v23_state_rule_table()
summary_rows <- list()
null_rows <- list()
candidate_rows <- list()
for (index in seq_len(nrow(rules))) {
  rule <- rules[index, , drop = FALSE]
  message("[v23] evaluating state rule: ", rule$state_rule)
  observed <- v23_directional_profiles(
    observed_counts, trials, graph,
    white_max_share = rule$white_max_share,
    pigmented_min_share = rule$pigmented_min_share,
    pseudocount = pseudocount
  )
  simulated <- v23_directional_profiles(
    simulated_counts, trials, graph,
    white_max_share = rule$white_max_share,
    pigmented_min_share = rule$pigmented_min_share,
    pseudocount = pseudocount
  )
  summary_rows[[length(summary_rows) + 1L]] <-
    v23_asymmetry_summary(observed, simulated, rule$state_rule)
  null_block <- simulated$summary
  null_block$analysis_spec_version <- v23_analysis_spec_version
  null_block$analysis_status <- "post_hoc_diagnostic"
  null_block$state_rule <- rule$state_rule
  null_rows[[length(null_rows) + 1L]] <- null_block
  candidate_rows[[length(candidate_rows) + 1L]] <-
    v23_observed_candidates(observed, cells$exact_site_id, rule$state_rule)
}

summary <- do.call(rbind, summary_rows)
null <- do.call(rbind, null_rows)
candidates <- do.call(rbind, candidate_rows)

primary_metric <- summary[
  summary$state_rule == "legacy_any_pigmented" &
    summary$metric == "log_rate_ratio", , drop = FALSE
]
if (nrow(primary_metric) != 1L) {
  stop("Primary asymmetry metric was not generated exactly once.")
}

metadata <- data.frame(
  field = c(
    "analysis_spec_version", "generated_at", "analysis_status",
    "primary_state_rule", "primary_asymmetry_metric",
    "positive_direction", "neighbourhood_definition",
    "natural_reference", "pseudocount", "claim_ceiling"
  ),
  value = c(
    v23_analysis_spec_version,
    as.character(Sys.time()),
    "post_hoc symmetry diagnostic; not pre-specified",
    "legacy_any_pigmented",
    "log ratio of opportunity-normalized pigmented-in-white rate to white-in-pigmented rate",
    "positive values indicate relatively more pigmented-in-white events",
    paste(
      "locked primary local-isolate graph:",
      paste0(primary_configuration$radius_km, " km radius;"),
      paste0("environment caliper ", primary_configuration$environment_caliper, ";"),
      paste0("minimum neighbours ", primary_configuration$minimum_neighbours, ";"),
      paste0("same-fold-only ", primary_configuration$same_fold_only)
    ),
    paste(
      ncol(simulated_counts),
      "cross-fitted natural predictive maps with observed trial counts"
    ),
    as.character(pseudocount),
    paste(
      "directional local colour-state asymmetry relative to the fitted natural baseline;",
      "not a mechanism, transition rate, mutation rate, introduction rate, or evolutionary direction"
    )
  ),
  stringsAsFactors = FALSE
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  rules, file.path(output_dir, "local_state_asymmetry_rules.csv"),
  row.names = FALSE
)
utils::write.csv(
  summary, file.path(output_dir, "local_state_asymmetry_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  null, file.path(output_dir, "local_state_asymmetry_null.csv"),
  row.names = FALSE
)
utils::write.csv(
  candidates, file.path(output_dir, "local_state_asymmetry_candidates.csv"),
  row.names = FALSE
)
utils::write.csv(
  metadata, file.path(output_dir, "local_state_asymmetry_metadata.csv"),
  row.names = FALSE
)

readme <- c(
  "# v23 post hoc local colour-state asymmetry diagnostic",
  "",
  paste(
    "This stage applies the same response-blind 10-km environmental-neighbourhood",
    "graph to both directions of local colour-state discordance: a pigmented focal",
    "cell among white neighbours and a white focal cell among pigmented neighbours."
  ),
  "",
  paste(
    "The primary rule mirrors the locked pigmented-isolate definition. Pure-cell",
    "and 10/90 share rules are retained as classification sensitivities. Counts are",
    "normalized by the number of graph-supported focal cells in the corresponding",
    "state before the directional rates are compared."
  ),
  "",
  paste(
    "The observed log rate ratio is compared with the same statistic from each",
    "cross-fitted natural predictive map. This is a post hoc symmetry diagnostic.",
    "It does not estimate a biological transition, mutation, dispersal, planting,",
    "or evolutionary rate, and it must not be described as pre-specified."
  )
)
writeLines(readme, file.path(output_dir, "README.md"), useBytes = TRUE)

cat("v23 local state-asymmetry outputs written to ",
    normalizePath(output_dir), "\n", sep = "")
print(primary_metric)
