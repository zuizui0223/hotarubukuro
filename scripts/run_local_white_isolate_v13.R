args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = NULL) {
  prefix <- paste0(name, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  sub(prefix, "", hit[1], fixed = TRUE)
}

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
features_path <- arg_value(
  "--landscape-features",
  paste0(
    "results/ecological_v19_human_landscape_extremes/",
    "landscape_cell_features.csv"
  )
)
checkpoint_root <- arg_value(
  "--checkpoint-root",
  "results/ecological_v16_predictive_replication/checkpoints"
)
output_dir <- arg_value(
  "--output", "results/ecological_v20_local_white_isolates"
)
control_min_neighbour_share <- as.numeric(arg_value(
  "--control-min-neighbour-share", "0.25"
))

source("scripts/extreme_human_diagnostic_v11.R")
source("scripts/human_landscape_extremes_v12.R")
source("scripts/local_white_isolate_v13.R")

cells <- utils::read.csv(
  cells_path, check.names = FALSE, stringsAsFactors = FALSE
)
features <- utils::read.csv(
  features_path, check.names = FALSE, stringsAsFactors = FALSE
)
feature_index <- match(cells$exact_site_id, features$exact_site_id)
if (anyNA(feature_index) || anyDuplicated(feature_index)) {
  stop("Landscape features do not align one-to-one with cells.", call. = FALSE)
}
features <- features[feature_index, , drop = FALSE]
presence <- v18_align_result(
  readRDS(file.path(
    checkpoint_root,
    "national_environment_spde_presence_draws1000.rds"
  )),
  cells, "presence"
)
intensity <- v18_align_result(
  readRDS(file.path(
    checkpoint_root,
    "national_environment_spde_intensity_draws1000.rds"
  )),
  cells, "intensity"
)
phenology <- v18_align_result(
  readRDS(file.path(
    checkpoint_root,
    "national_environment_year_spde_phenology_draws1000.rds"
  )),
  cells, "phenology"
)
auxiliary_profile <- v18_profile(
  cells, presence, phenology, intensity
)
auxiliary_definitions <- data.frame(
  feature = c("early_tail_depth", "dark_tail_depth"),
  role = c("phenology_surprise", "pigmented_intensity_surprise"),
  hypothesis_direction = c("greater", "greater"),
  stringsAsFactors = FALSE
)
auxiliary_features <- data.frame(
  early_tail_depth = auxiliary_profile$early_tail_depth,
  dark_tail_depth = auxiliary_profile$dark_tail_depth,
  stringsAsFactors = FALSE
)
counts <- presence$draws
trials <- pmax(as.numeric(cells$n_observations), 1)
shares <- sweep(counts, 1, trials, "/")
observed_share <- as.numeric(cells$n_pigmented) / trials
observed_q <- v18_predictive_tail_q(
  as.numeric(cells$n_pigmented), counts, "upper"
)
observed_sd <- apply(shares, 1, stats::sd)
observed_z <- (observed_share - rowMeans(shares)) /
  pmax(observed_sd, 1e-12)
simulated_q <- v18_simulation_tail_q(counts, "upper")
simulated_z <- v18_z_matrix(shares)
ids <- as.character(cells$exact_site_id)

configurations <- v20_configuration_table()
summary_rows <- list()
null_rows <- list()
graph_rows <- list()
primary <- NULL

message("[v20] evaluating environment-similar white neighbourhoods")
for (row in seq_len(nrow(configurations))) {
  configuration <- configurations[row, ]
  graph <- v20_neighbour_graph(
    cells,
    radius_km = configuration$radius_km,
    environment_caliper = configuration$environment_caliper,
    minimum_neighbours = configuration$minimum_neighbours,
    same_fold_only = configuration$same_fold_only
  )
  observed_profile <- v20_local_profile(
    as.numeric(cells$n_pigmented), graph,
    configuration$maximum_neighbour_pigment_share
  )
  simulated_profile <- v20_local_profile(
    counts, graph,
    configuration$maximum_neighbour_pigment_share
  )
  metric <- v20_metric_rows(
    observed_profile, simulated_profile,
    observed_q, simulated_q, configuration$configuration
  )
  summary_rows[[length(summary_rows) + 1L]] <- metric$summary
  null_rows[[length(null_rows) + 1L]] <- metric$null
  graph_block <- graph$support
  graph_block$configuration <- configuration$configuration
  graph_rows[[length(graph_rows) + 1L]] <- graph_block
  if (configuration$role == "primary") {
    primary <- list(
      configuration = configuration,
      graph = graph,
      observed = observed_profile,
      simulated = simulated_profile
    )
  }
}
local_summary <- do.call(rbind, summary_rows)
local_null <- do.call(rbind, null_rows)
graph_support <- do.call(rbind, graph_rows)

predictors <- v19_rf_predictors()
definitions <- v19_contrast_definitions()
landscape_complete <- stats::complete.cases(
  features[, unique(c(predictors, definitions$feature)), drop = FALSE]
)
support <- primary$observed$supported & landscape_complete
observed_cases <- which(
  primary$observed$candidate[, 1L] & support
)
observed_controls <- which(
  primary$observed$present[, 1L] & support &
    !primary$observed$candidate[, 1L] &
    primary$observed$neighbour_pigment_share[, 1L] >=
    control_min_neighbour_share
)

message("[v20] matching local isolates to locally non-isolated pigmented cells")
match_options <- v18_match_options(cells, presence$latent_mean)
match_options <- v20_filter_match_options(
  match_options, primary$graph
)
observed_pairs <- v18_match_cases(
  observed_cases, observed_controls, match_options,
  observed_q, observed_z, ids
)
observed_contrast <- v19_pair_contrasts(
  observed_pairs, features, definitions$feature
)
observed_auxiliary <- v19_pair_contrasts(
  observed_pairs, auxiliary_features, auxiliary_definitions$feature
)
observed_pair_details <- v19_pair_details(
  observed_pairs, features, definitions$feature,
  "primary_local_white_isolate"
)
profile_counts <- v19_profile_counts(
  observed_pairs, features, "primary_local_white_isolate"
)

null_contrast <- matrix(
  NA_real_, nrow = ncol(counts), ncol = nrow(definitions),
  dimnames = list(NULL, definitions$feature)
)
null_auxiliary <- matrix(
  NA_real_, nrow = ncol(counts), ncol = nrow(auxiliary_definitions),
  dimnames = list(NULL, auxiliary_definitions$feature)
)
null_requested <- integer(ncol(counts))
null_matched <- integer(ncol(counts))
for (draw in seq_len(ncol(counts))) {
  simulated_support <- primary$simulated$supported & landscape_complete
  cases <- which(
    primary$simulated$candidate[, draw] & simulated_support
  )
  controls <- which(
    primary$simulated$present[, draw] & simulated_support &
      !primary$simulated$candidate[, draw] &
      primary$simulated$neighbour_pigment_share[, draw] >=
      control_min_neighbour_share
  )
  pairs <- v18_match_cases(
    cases, controls, match_options,
    simulated_q[, draw], simulated_z[, draw], ids
  )
  null_requested[draw] <- length(cases)
  null_matched[draw] <- nrow(pairs)
  null_contrast[draw, ] <- v19_pair_contrasts(
    pairs, features, definitions$feature
  )
  null_auxiliary[draw, ] <- v19_pair_contrasts(
    pairs, auxiliary_features, auxiliary_definitions$feature
  )
}
landscape_null <- data.frame(
  tier = "primary_local_white_isolate",
  draw = seq_len(ncol(counts)),
  requested_cases = null_requested,
  matched_pairs = null_matched,
  null_contrast,
  check.names = FALSE, stringsAsFactors = FALSE
)
landscape_summary <- v19_contrast_summary(
  observed_contrast, landscape_null, definitions,
  "primary_local_white_isolate"
)
landscape_global <- v19_global_landscape_test(
  observed_contrast, landscape_null, predictors,
  "primary_local_white_isolate"
)
auxiliary_null <- data.frame(
  tier = "primary_local_white_isolate",
  draw = seq_len(ncol(counts)),
  requested_cases = null_requested,
  matched_pairs = null_matched,
  null_auxiliary,
  check.names = FALSE, stringsAsFactors = FALSE
)
auxiliary_summary <- v19_contrast_summary(
  observed_auxiliary, auxiliary_null, auxiliary_definitions,
  "primary_local_white_isolate"
)
auxiliary_case_control <- data.frame(
  facet = c(
    "early_tail_q10", "dark_tail_q10", "both_early_and_dark_q10"
  ),
  case_count = c(
    sum(auxiliary_profile$early_tail_10[observed_pairs$case_index],
        na.rm = TRUE),
    sum(auxiliary_profile$dark_tail_10[observed_pairs$case_index],
        na.rm = TRUE),
    sum(
      auxiliary_profile$early_tail_10[observed_pairs$case_index] &
        auxiliary_profile$dark_tail_10[observed_pairs$case_index],
      na.rm = TRUE
    )
  ),
  control_count = c(
    sum(auxiliary_profile$early_tail_10[observed_pairs$control_index],
        na.rm = TRUE),
    sum(auxiliary_profile$dark_tail_10[observed_pairs$control_index],
        na.rm = TRUE),
    sum(
      auxiliary_profile$early_tail_10[observed_pairs$control_index] &
        auxiliary_profile$dark_tail_10[observed_pairs$control_index],
      na.rm = TRUE
    )
  ),
  n_pairs = nrow(observed_pairs),
  stringsAsFactors = FALSE
)
candidate_auxiliary <- auxiliary_profile[, c(
  "exact_site_id", "early_predictive_q", "early_tail_depth",
  "dark_predictive_q", "dark_tail_depth", "early_tail_10",
  "dark_tail_10", "convergence_count"
)]
candidate_table <- v20_candidate_table(
  cells, features, primary$graph, primary$observed,
  observed_q, observed_z
)
candidate_table <- merge(
  candidate_table, candidate_auxiliary,
  by = "exact_site_id", all.x = TRUE, sort = FALSE
)
candidate_table <- candidate_table[
  order(candidate_table$candidate_rank), , drop = FALSE
]

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(
  configurations, file.path(output_dir, "local_isolate_configurations.csv"),
  row.names = FALSE
)
write.csv(
  graph_support, file.path(output_dir, "local_isolate_graph_support.csv"),
  row.names = FALSE
)
write.csv(
  local_summary, file.path(output_dir, "local_isolate_natural_null_summary.csv"),
  row.names = FALSE
)
write.csv(
  local_null, file.path(output_dir, "local_isolate_natural_null.csv"),
  row.names = FALSE
)
write.csv(
  candidate_table, file.path(output_dir, "local_isolate_candidates.csv"),
  row.names = FALSE
)
write.csv(
  observed_pair_details,
  file.path(output_dir, "local_isolate_observed_pairs.csv"),
  row.names = FALSE
)
write.csv(
  landscape_summary,
  file.path(output_dir, "local_isolate_landscape_summary.csv"),
  row.names = FALSE
)
write.csv(
  landscape_null,
  file.path(output_dir, "local_isolate_landscape_null.csv"),
  row.names = FALSE
)
write.csv(
  landscape_global,
  file.path(output_dir, "local_isolate_landscape_global.csv"),
  row.names = FALSE
)
write.csv(
  auxiliary_summary,
  file.path(output_dir, "local_isolate_auxiliary_facets_summary.csv"),
  row.names = FALSE
)
write.csv(
  auxiliary_null,
  file.path(output_dir, "local_isolate_auxiliary_facets_null.csv"),
  row.names = FALSE
)
write.csv(
  auxiliary_case_control,
  file.path(output_dir, "local_isolate_auxiliary_facets_counts.csv"),
  row.names = FALSE
)
write.csv(
  profile_counts,
  file.path(output_dir, "local_isolate_landscape_profiles.csv"),
  row.names = FALSE
)
write.csv(
  match_options$settings,
  file.path(output_dir, "local_isolate_matching_settings.csv"),
  row.names = FALSE
)

metadata <- data.frame(
  field = c(
    "analysis_spec_version", "generated_at", "n_natural_maps",
    "primary_candidate_definition", "environment_definition",
    "cross_fold_joint_dependence", "fold_boundary_sensitivity",
    "landscape_selection_role", "auxiliary_facet_role",
    "residual_used_as_response", "horticultural_claim_ceiling",
    "final_Rmd_md5"
  ),
  value = c(
    v20_analysis_spec_version, as.character(Sys.time()),
    as.character(ncol(counts)),
    paste(
      "pigmented focal 1-km cell; at least three neighbours within 10 km;",
      "environmental distance <=1; all physical neighbours white"
    ),
    paste(
      "four standardized broad/within 50-km natural-environment PCs;",
      "RMS Euclidean distance"
    ),
    paste(
      "joint predictive draws within heldout fold;",
      "cross-fold cells are combined as independent out-of-fold mosaics"
    ),
    paste(
      "primary observed neighbourhood includes all physical neighbours;",
      "same-fold-only definition is reported as a sensitivity analysis"
    ),
    "population, land use, road access, and mountain context are held out",
    paste(
      "early flowering and pigmented-only darkness are held-out predictive",
      "tail diagnostics; neither selects local-isolate candidates"
    ),
    "false",
    paste(
      "local follow-up priority compatible with introduction;",
      "not introduction or horticultural provenance evidence"
    ),
    if (file.exists("final.Rmd")) unname(tools::md5sum("final.Rmd")) else NA
  ),
  stringsAsFactors = FALSE
)
write.csv(
  metadata, file.path(output_dir, "local_isolate_metadata.csv"),
  row.names = FALSE
)

readme <- c(
  "# v20 environment-similar white-neighbourhood isolates",
  "",
  paste(
    "The primary event is a pigmented 1-km focal cell with at least three",
    "nearby cells within 10 km that are similar on the natural environmental",
    "axes and are all observed white. The primary observed neighbourhood",
    "includes physical neighbours across spatial-fold boundaries."
  ),
  "",
  paste(
    "The same local event is counted in 1000 cross-fitted natural predictive",
    "maps with observed trial counts fixed. Cross-fold predictions form an",
    "independent out-of-fold mosaic, and a same-fold-only definition is retained",
    "as a boundary sensitivity analysis. Population, land use, road access,",
    "phenology, and intensity do not define local-isolate cases."
  ),
  "",
  paste(
    "Observed isolates are then matched to locally non-isolated pigmented cells",
    "on geography, environment, natural pigmentation expectation, observation",
    "effort, and local sampling support. Landscape contrasts remain diagnostic",
    "and do not identify introduction, escape, or provenance. Early-flowering",
    "and pigmented-only darkness tails are evaluated after candidate selection."
  ),
  "",
  "`final.Rmd` is intentionally not modified."
)
writeLines(readme, file.path(output_dir, "README.md"), useBytes = TRUE)

cat("v20 local white-isolate analysis written to ", normalizePath(output_dir), "\n")
print(local_summary)
print(landscape_global)
print(landscape_summary)
