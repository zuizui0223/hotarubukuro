#!/usr/bin/env Rscript

# Current-Broad-primary human-context replay.
#
# The validated high-rep adjudication script is sourced first so that the same
# data restoration, final-eight-axis cross-fitted presence model, candidate
# detector, maxT family, and observation-process alternatives are reused.
# This wrapper changes the inferential hierarchy only:
#
#   PRIMARY: final-eight-axis Broad presence model + final-eight-axis RMS
#            environmental matching at the predeclared caliper of 1.
#   SENSITIVITIES: the historical four-PC graph and the support-calibrated
#                  final-eight-axis graph produced by the sourced audit.
#
# No colour response, candidate identity, or human-context variable is used to
# tune the primary environmental caliper.

source("analysis_sensitivity/run_human_context_highrep_final.R", local = FALSE)

required_objects <- c(
  "cells", "final8_model", "final_env", "make_graph", "run_one",
  "current_graph", "matched_graph", "features", "definitions",
  "effort", "effort_def", "output_dir", "n_draws", "seed",
  "candidate_membership"
)
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
if (length(missing_objects)) {
  stop(
    "Current-Broad wrapper could not recover objects from the validated audit: ",
    paste(missing_objects, collapse = ", "), call. = FALSE
  )
}

primary_label <- "current_broad_primary_final8_model_final8_graph_caliper1"
primary_graph <- make_graph(
  final_env,
  caliper = 1,
  label = "current_broad_final8_rms_caliper1_primary"
)

message("[current broad primary] replaying ", n_draws,
        " maps with final8 model and final8 RMS caliper=1")
primary <- run_one(final8_model, primary_graph, primary_label)

utils::write.csv(
  primary_graph$settings,
  file.path(output_dir, "current_broad_primary_graph_registry.csv"),
  row.names = FALSE
)
utils::write.csv(
  primary_graph$support,
  file.path(output_dir, "current_broad_primary_graph_support.csv"),
  row.names = FALSE
)
utils::write.csv(
  primary$event,
  file.path(output_dir, "current_broad_primary_candidate_null.csv"),
  row.names = FALSE
)
utils::write.csv(
  primary$human,
  file.path(output_dir, "current_broad_primary_human_maxT.csv"),
  row.names = FALSE
)
utils::write.csv(
  primary$effort,
  file.path(output_dir, "current_broad_primary_observation_bias.csv"),
  row.names = FALSE
)
utils::write.csv(
  primary$candidates,
  file.path(output_dir, "current_broad_primary_candidate_membership.csv"),
  row.names = FALSE
)

primary_human <- primary$human
human_test_rows <- primary_human$feature %in% definitions$feature[1:9]
population_rows <- primary_human$feature %in% definitions$feature[1:5]
best_human <- primary_human[human_test_rows, , drop = FALSE]
best_human <- best_human[which.min(best_human$maxT_FWER_p), , drop = FALSE]
best_population <- primary_human[population_rows, , drop = FALSE]
best_population <- best_population[which.min(best_population$maxT_FWER_p), , drop = FALSE]

primary_decision <- data.frame(
  scenario = primary_label,
  candidate_count = nrow(primary$candidates),
  supported_cells = sum(primary_graph$support$supported),
  mean_neighbours = mean(primary_graph$support$n_neighbours),
  best_population_scale = best_population$feature[1],
  best_population_contrast = best_population$observed_focal_minus_white_neighbour[1],
  best_population_directional_p = best_population$directional_or_two_sided_p[1],
  best_population_global_FWER = best_population$maxT_FWER_p[1],
  best_global_human_feature = best_human$feature[1],
  best_global_human_FWER = best_human$maxT_FWER_p[1],
  any_human_global_FWER_below_0_05 = any(
    primary_human$maxT_FWER_p[human_test_rows] < 0.05,
    na.rm = TRUE
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  primary_decision,
  file.path(output_dir, "current_broad_primary_decision.csv"),
  row.names = FALSE
)

candidate_set <- function(result) unique(as.character(result$exact_site_id))
primary_ids <- candidate_set(primary$candidates)
old4_ids <- unique(as.character(
  candidate_membership$exact_site_id[
    candidate_membership$scenario == "final8_model_current_graph"
  ]
))
matched8_ids <- unique(as.character(
  candidate_membership$exact_site_id[
    candidate_membership$scenario == "final8_model_final8_support_matched"
  ]
))
set_metrics <- function(reference_ids, label) {
  union_ids <- union(primary_ids, reference_ids)
  intersection_ids <- intersect(primary_ids, reference_ids)
  data.frame(
    comparison = label,
    primary_n = length(primary_ids),
    comparator_n = length(reference_ids),
    overlap_n = length(intersection_ids),
    jaccard = if (length(union_ids)) length(intersection_ids) / length(union_ids) else 1,
    primary_only_n = length(setdiff(primary_ids, reference_ids)),
    comparator_only_n = length(setdiff(reference_ids, primary_ids)),
    stringsAsFactors = FALSE
  )
}
overlap_audit <- rbind(
  set_metrics(old4_ids, "primary_final8_caliper1_vs_historical4pc_graph"),
  set_metrics(matched8_ids, "primary_final8_caliper1_vs_support_matched_final8_graph")
)
utils::write.csv(
  overlap_audit,
  file.path(output_dir, "current_broad_primary_candidate_overlap_audit.csv"),
  row.names = FALSE
)

primary_only <- setdiff(primary_ids, old4_ids)
old4_only <- setdiff(old4_ids, primary_ids)
matched_only <- setdiff(matched8_ids, primary_ids)
membership_changes <- data.frame(
  exact_site_id = c(primary_only, old4_only, matched_only),
  status = c(
    rep("primary_only_vs_historical4pc", length(primary_only)),
    rep("historical4pc_only_vs_primary", length(old4_only)),
    rep("support_matched_final8_only_vs_primary", length(matched_only))
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  membership_changes,
  file.path(output_dir, "current_broad_primary_membership_changes.csv"),
  row.names = FALSE
)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required for validation output", call. = FALSE)
}
jsonlite::write_json(
  list(
    status = "PASS",
    primary_definition = list(
      natural_reference = "final-eight-axis cross-fitted presence model",
      environmental_matching = "standardized final-eight-axis RMS distance",
      radius_km = 10,
      rms_caliper = 1,
      minimum_neighbours = 3,
      selection_uses_human_variables = FALSE,
      selection_uses_colour_beyond_pigmentation_state_event = FALSE,
      east_west_note = paste(
        "East/West remains a structural adjustment in the observation-level Broad model;",
        "it is not an environmental matching dimension. The cross-fitted natural",
        "reference uses the eight measured abiotic axes plus SPDE geography."
      )
    ),
    n_maps = n_draws,
    seed = seed,
    candidate_count = nrow(primary$candidates),
    supported_cells = sum(primary_graph$support$supported),
    mean_neighbours = mean(primary_graph$support$n_neighbours),
    best_population_scale = as.character(best_population$feature[1]),
    best_population_global_FWER = as.numeric(best_population$maxT_FWER_p[1]),
    best_global_human_feature = as.character(best_human$feature[1]),
    best_global_human_FWER = as.numeric(best_human$maxT_FWER_p[1]),
    hierarchy = paste(
      "Current Broad final8 x final8-caliper1 is primary; historical four-PC and",
      "support-calibrated final8 definitions are sensitivity analyses only."
    )
  ),
  file.path(output_dir, "current_broad_primary_validation.json"),
  pretty = TRUE,
  auto_unbox = TRUE
)

message("[current broad primary] PASS: ", nrow(primary$candidates),
        " observed candidates; best global human FWER=",
        signif(best_human$maxT_FWER_p[1], 4))
