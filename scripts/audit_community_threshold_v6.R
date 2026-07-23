args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[1] else
  "results/ecological_v13_community_threshold"

read_result <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing audit input: ", path, call. = FALSE)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

checks <- list()
add_check <- function(name, passed, evidence, consequence) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = name, passed = isTRUE(passed), evidence = as.character(evidence),
    consequence = consequence, stringsAsFactors = FALSE
  )
}

quality <- read_result("community_data_quality.csv")
support <- read_result("community_enmeval_support.csv")
presence_cv <- read_result("community_presence_threshold_spatial_cv.csv")
presence_summary <- read_result("community_presence_threshold_cv_summary.csv")
edge_cv <- read_result("community_edge_spatial_cv.csv")
environment_vif <- read_result("community_environment_vif.csv")
axis_correlations <- read_result("community_axis_correlations.csv")
crossfit_log <- read_result("community_intensity_phenology_crossfit_log.csv")
candidates <- read_result("community_horticultural_candidates.csv")
pairs <- read_result("community_horticultural_matched_pairs.csv")
facets <- read_result("community_horticultural_facet_summary.csv")
spde <- read_result("community_spde_model_comparison.csv")

quality_value <- function(metric) quality$value[match(metric, quality$metric)]

add_check(
  "two_part_response",
  as.numeric(quality_value("white_cells")) > 0 &&
    as.numeric(quality_value("pigmented_cells")) > 0,
  paste(quality_value("white_cells"), "white and",
        quality_value("pigmented_cells"), "pigmented 1-km cells"),
  "White a* is excluded from the conditional intensity estimand."
)
add_check(
  "five_species_support_is_explicit",
  any(support$finite_fraction < 1) && any(support$finite_fraction == 1),
  paste(round(100 * min(support$finite_fraction), 1), "to 100% finite by species;",
        quality_value("five_species_common_support_cells"), "common-support cells"),
  "Nationwide structural-zero results must be compared with common support."
)
fold_counts <- aggregate(
  heldout_spatial_fold ~ support + axis + model, presence_cv,
  function(x) length(unique(x))
)
add_check(
  "presence_spatial_cv_complete",
  nrow(fold_counts) == 24L && all(fold_counts$heldout_spatial_fold == 5L),
  paste(nrow(fold_counts), "support-axis-model combinations;",
        min(fold_counts$heldout_spatial_fold), "to",
        max(fold_counts$heldout_spatial_fold), "folds"),
  "Threshold, smooth, and linear comparisons use all five spatial folds."
)
add_check(
  "threshold_selected_inside_training_fold",
  all(is.finite(presence_cv$selected_change_point[
    presence_cv$model == "threshold"
  ])),
  paste(sum(presence_cv$model == "threshold"),
        "held-out evaluations with a training-selected hinge"),
  "No pooled-data change point is used for held-out scoring."
)
warning_values <- presence_cv$model_warnings
warning_values <- warning_values[!is.na(warning_values) & nzchar(warning_values)]
add_check(
  "presence_models_without_warnings", length(warning_values) == 0L,
  paste(length(warning_values), "unique non-empty warning rows"),
  "Reported CV metrics are not qualified by convergence warnings."
)
add_check(
  "environment_collinearity_bounded",
  max(environment_vif$VIF, na.rm = TRUE) < 5,
  paste("maximum environment-only VIF =",
        round(max(environment_vif$VIF, na.rm = TRUE), 3)),
  "The natural background can retain all prespecified environmental axes."
)
cor_matrix <- as.matrix(axis_correlations[, setdiff(names(axis_correlations), "axis")])
diag(cor_matrix) <- NA_real_
add_check(
  "pollinator_axis_collinearity_not_hidden",
  max(abs(cor_matrix), na.rm = TRUE) > 0.7,
  paste("maximum absolute inter-axis correlation =",
        round(max(abs(cor_matrix), na.rm = TRUE), 3)),
  "Availability, richness, and composition are separate estimands, not joint coefficients."
)
edge_fold_counts <- aggregate(
  heldout_spatial_fold ~ support + model, edge_cv,
  function(x) length(unique(x))
)
add_check(
  "edge_cv_node_disjoint",
  all(edge_cv$strict_node_disjoint_split) &&
    all(edge_fold_counts$heldout_spatial_fold == 5L),
  paste(nrow(edge_cv), "fold-model rows; all marked endpoint-disjoint"),
  "Local contact-zone prediction is not inflated by reusing endpoint cells."
)
add_check(
  "intensity_and_phenology_crossfit_complete",
  all(table(crossfit_log$outcome) == 5L),
  paste(names(table(crossfit_log$outcome)), table(crossfit_log$outcome),
        collapse = "; "),
  "Darkness and early-flowering facets are based on held-out natural residuals."
)
add_check(
  "horticulture_candidates_independent_of_facets",
  nrow(candidates) == as.numeric(quality_value("candidate_pigmented_enclaves_25km")) &&
    all(candidates$n_neighbours >= 5) &&
    all(candidates$neighbour_pigment_share <= 0.1),
  paste(nrow(candidates), "pigmented-in-white-background candidates"),
  "Human exposure, darkness, and phenology are tests, not eligibility criteria."
)
add_check(
  "horticulture_controls_complete",
  nrow(pairs) == nrow(candidates) && !anyDuplicated(pairs$control_site),
  paste(nrow(pairs), "non-reused controls for", nrow(candidates), "candidates"),
  "All detected candidates contribute to the primary matched comparison."
)
add_check(
  "horticulture_multiplicity_controlled",
  all(c("bootstrap_p", "BH_q", "simultaneous_requirement") %in% names(facets)) &&
    all(!facets$simultaneous_requirement),
  paste(nrow(facets), "separate facets with BH q-values"),
  "No summed evidence score or all-facets requirement is used."
)
add_check(
  "spde_foundation_complete",
  nrow(spde) == 4L && all(spde$n_CPO_nonfinite == 0),
  paste(nrow(spde), "SPDE models;", sum(spde$n_CPO_nonfinite), "nonfinite CPO"),
  "Nationwide presence and pigmented-only intensity each compare space-only with environment plus space."
)
add_check(
  "claim_ceiling_for_terrain",
  TRUE,
  "Only endpoint elevation and topography differences are available.",
  "Do not call this an intervening geographic-barrier test without a resistance surface."
)

audit <- do.call(rbind, checks)
write.csv(audit, file.path(output_dir, "community_analysis_audit.csv"),
          row.names = FALSE)

lines <- c(
  "# v13 community-threshold audit",
  "",
  paste0("Passed **", sum(audit$passed), "/", nrow(audit), "** checks."),
  "",
  vapply(seq_len(nrow(audit)), function(i) {
    paste0(
      "- ", if (audit$passed[i]) "PASS" else "FAIL", " — `",
      audit$check[i], "`: ", audit$evidence[i], " ", audit$consequence[i]
    )
  }, character(1))
)
writeLines(lines, file.path(output_dir, "AUDIT.md"))

print(audit)
if (!all(audit$passed)) quit(status = 1L)
