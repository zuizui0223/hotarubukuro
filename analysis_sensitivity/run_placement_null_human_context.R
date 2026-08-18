#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  if (hit[length(hit)] == length(args)) {
    stop("Missing value after ", flag, call. = FALSE)
  }
  args[hit[length(hit)] + 1L]
}

reference_root <- arg_value("--reference-root", "reference-artifact")
final8_root <- arg_value("--final8-root", "final8-artifact")
human_replay_root <- arg_value(
  "--human-replay-root", "human-replay-artifact"
)
worldpop_raster <- arg_value("--worldpop-raster", "worldpop/worldpop.tif")
output_dir <- arg_value(
  "--output", "results/placement_null_human_context"
)
n_draws <- as.integer(arg_value("--draws", "10000"))
seed <- as.integer(arg_value("--seed", "20260725"))

if (n_draws != 10000L) {
  stop("The predeclared placement-null analysis requires 10,000 maps.",
       call. = FALSE)
}
set.seed(seed)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_packages <- c("jsonlite", "terra")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages)) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "),
       call. = FALSE)
}

source("R/candidate_null_tools.R")
source("R/human_landscape_features.R")
source("R/local_pigmented_isolates.R")
source("R/local_human_context.R")
source("R/spatial_context.R")
source("R/placement_null_human_context.R")

paths <- list(
  cells = file.path(
    reference_root,
    "results/ecological_v15_multiscale_hotspots/",
    "multiscale_hotspot_cells_1km.csv"
  ),
  human = file.path(
    reference_root,
    "results/ecological_v21_local_human_neighbourhood/",
    "human_neighbourhood_cell_features.csv"
  ),
  final8 = file.path(final8_root, "final8_presence_draws10000.rds"),
  frozen_candidates = file.path(
    human_replay_root, "current_broad_primary_candidate_membership.csv"
  ),
  frozen_null = file.path(
    human_replay_root, "current_broad_primary_candidate_null.csv"
  ),
  frozen_human = file.path(
    human_replay_root, "current_broad_primary_human_maxT.csv"
  ),
  frozen_graph = file.path(
    human_replay_root, "current_broad_primary_graph_support.csv"
  )
)
for (path in c(unlist(paths), worldpop_raster)) {
  if (!file.exists(path) || file.info(path)$size <= 0) {
    stop("Missing input: ", path, call. = FALSE)
  }
}

cells <- utils::read.csv(
  paths$cells, check.names = FALSE, stringsAsFactors = FALSE
)
human <- utils::read.csv(
  paths$human, check.names = FALSE, stringsAsFactors = FALSE
)
model <- readRDS(paths$final8)
frozen_candidates <- utils::read.csv(
  paths$frozen_candidates, check.names = FALSE, stringsAsFactors = FALSE
)
frozen_null <- utils::read.csv(
  paths$frozen_null, check.names = FALSE, stringsAsFactors = FALSE
)
frozen_human <- utils::read.csv(
  paths$frozen_human, check.names = FALSE, stringsAsFactors = FALSE
)
frozen_graph <- utils::read.csv(
  paths$frozen_graph, check.names = FALSE, stringsAsFactors = FALSE
)

v22_require_columns(cells, c(
  "exact_site_id", "longitude", "latitude", "x_km", "y_km",
  "spatial_fold", "n_independent_sites", "n_observations",
  "n_pigmented", v22_final8_environment_terms()
), "cells")
v22_require_columns(human, c(
  "exact_site_id", "population_5km_rank", "population_10km_rank",
  "population_25km_rank"
), "human features")
if (nrow(cells) != 1305L) {
  stop("The frozen analysis frame must contain 1,305 cells.", call. = FALSE)
}

human_index <- match(cells$exact_site_id, human$exact_site_id)
if (anyNA(human_index) || anyDuplicated(human_index)) {
  stop("Human features do not align one-to-one with cells.", call. = FALSE)
}
human <- human[human_index, , drop = FALSE]

aligned <- v18_align_result(model, cells, "final8 model")
if (ncol(aligned$draws) != n_draws) {
  stop("The final8 checkpoint does not contain 10,000 draws.", call. = FALSE)
}
if (!isTRUE(all.equal(
  as.numeric(aligned$observed), as.numeric(cells$n_pigmented),
  tolerance = 0
))) {
  stop("Observed final8 counts differ from the frozen cell table.",
       call. = FALSE)
}

message("[v22] rebuilding the frozen final-eight-axis neighbour graph")
graph <- v22_final8_neighbour_graph(
  cells, radius_km = 10, environment_caliper = 1,
  minimum_neighbours = 3L
)
graph_index <- match(cells$exact_site_id, frozen_graph$exact_site_id)
if (anyNA(graph_index) || anyDuplicated(graph_index)) {
  stop("Frozen graph support does not align to cells.", call. = FALSE)
}
frozen_graph <- frozen_graph[graph_index, , drop = FALSE]
if (!identical(
  as.integer(graph$support$n_neighbours),
  as.integer(frozen_graph$n_neighbours)
) || !identical(
  as.logical(graph$support$supported),
  as.logical(frozen_graph$supported)
)) {
  stop("Rebuilt graph differs from the frozen current-Broad graph.",
       call. = FALSE)
}
graph_numeric_columns <- intersect(
  c(
    "mean_neighbour_distance_km", "maximum_neighbour_distance_km",
    "mean_environmental_distance", "maximum_environmental_distance"
  ), names(frozen_graph)
)
graph_numeric_difference <- vapply(graph_numeric_columns, function(column) {
  max(abs(
    as.numeric(graph$support[[column]]) -
      as.numeric(frozen_graph[[column]])
  ), na.rm = TRUE)
}, numeric(1))
if (length(graph_numeric_difference) &&
    any(graph_numeric_difference > 1e-12, na.rm = TRUE)) {
  stop("Rebuilt graph numeric values differ from the frozen graph.",
       call. = FALSE)
}

message("[v22] reconstructing the predeclared 2/5/10/25-km ladder")
worldpop <- terra::rast(worldpop_raster)
exposure <- v22_population_exposure(
  worldpop, cells, radii_km = c(2, 5, 10, 25)
)
reconstruction_rows <- lapply(c(5, 10, 25), function(radius) {
  column <- paste0("population_", radius, "km_rank")
  difference <- abs(
    as.numeric(exposure[[column]]) - as.numeric(human[[column]])
  )
  data.frame(
    radius_km = radius,
    frozen_column = column,
    maximum_absolute_rank_difference = max(difference, na.rm = TRUE),
    mean_absolute_rank_difference = mean(difference, na.rm = TRUE),
    n_compared = sum(is.finite(difference)),
    stringsAsFactors = FALSE
  )
})
reconstruction_audit <- do.call(rbind, reconstruction_rows)
if (any(
  reconstruction_audit$maximum_absolute_rank_difference > 1e-12
)) {
  stop(
    "WorldPop reconstruction failed to reproduce frozen 5/10/25-km ranks.",
    call. = FALSE
  )
}

message("[v22] applying forward and reversed detectors to 10,000 maps")
observed_forward <- v22_directional_profile(
  matrix(aligned$observed, ncol = 1L), graph,
  "pigmented_among_white"
)
simulated_forward <- v22_directional_profile(
  aligned$draws, graph, "pigmented_among_white"
)
observed_reverse <- v22_directional_profile(
  matrix(aligned$observed, ncol = 1L), graph,
  "white_among_pigmented"
)
simulated_reverse <- v22_directional_profile(
  aligned$draws, graph, "white_among_pigmented"
)

observed_forward_ids <- sort(as.character(
  cells$exact_site_id[observed_forward$candidate[, 1L]]
))
frozen_forward_ids <- sort(as.character(frozen_candidates$exact_site_id))
if (!identical(observed_forward_ids, frozen_forward_ids)) {
  stop("Forward candidate identities differ from the frozen 16 cells.",
       call. = FALSE)
}
if (length(observed_forward_ids) != 16L) {
  stop("The forward detector must recover exactly 16 candidates.",
       call. = FALSE)
}

observed_q <- v18_predictive_tail_q(
  aligned$observed, aligned$draws, "upper"
)
simulated_q <- v18_simulation_tail_q(aligned$draws, "upper")
current_event <- v20_metric_rows(
  observed_forward, simulated_forward,
  observed_q, simulated_q,
  "current_broad_primary_final8_model_final8_graph_caliper1"
)$summary
for (metric in c("candidate_count", "candidate_fraction")) {
  recomputed <- current_event[current_event$metric == metric, ]
  frozen <- frozen_null[frozen_null$metric == metric, ]
  if (nrow(recomputed) != 1L || nrow(frozen) != 1L ||
      abs(recomputed$observed_value - frozen$observed_value) > 1e-12 ||
      abs(recomputed$empirical_p - frozen$empirical_p) > 1e-12) {
    stop("Frozen v20 calibration was not reproduced for ", metric,
         call. = FALSE)
  }
}

run_direction <- function(
    observed_profile, simulated_profile, direction) {
  results <- lapply(c(2, 5, 10, 25), function(radius) {
    column <- paste0("population_", radius, "km_rank")
    v22_placement_null(
      observed_profile$candidate,
      simulated_profile$candidate,
      exposure[[column]],
      radius_km = radius,
      direction = direction,
      feature = column
    )
  })
  list(
    summary = do.call(rbind, lapply(results, `[[`, "summary")),
    null = do.call(rbind, lapply(results, `[[`, "null"))
  )
}

forward <- run_direction(
  observed_forward, simulated_forward, "pigmented_among_white"
)
reverse <- run_direction(
  observed_reverse, simulated_reverse, "white_among_pigmented"
)
placement_null <- rbind(forward$null, reverse$null)
placement_summary <- v22_add_scale_maxT(
  rbind(forward$summary, reverse$summary), placement_null
)
scale_profile <- v22_scale_profile(placement_summary)

message("[v22] replaying the unchanged v21 local contrast at primary 5 km")
primary_features <- data.frame(
  population_5km_rank = exposure$population_5km_rank,
  stringsAsFactors = FALSE
)
primary_definition <- data.frame(
  feature = "population_5km_rank",
  role = "primary_population_5km",
  hypothesis_direction = "greater",
  stringsAsFactors = FALSE
)
analysis_support <- is.finite(primary_features$population_5km_rank)
observed_local <- v21_local_contrasts(
  observed_forward$present, observed_forward$candidate,
  graph, primary_features, primary_definition$feature,
  analysis_support
)
simulated_local <- v21_local_contrasts(
  simulated_forward$present, simulated_forward$candidate,
  graph, primary_features, primary_definition$feature,
  analysis_support
)
observed_local_value <- as.numeric(observed_local$contrast[1L, ])
names(observed_local_value) <- colnames(observed_local$contrast)
neighbour_replay <- v21_contrast_summary(
  observed_local_value,
  as.data.frame(simulated_local$contrast),
  primary_definition,
  "v22_secondary_identical_v21_replay"
)
frozen_primary <- frozen_human[
  frozen_human$feature == "population_5km_rank", , drop = FALSE
]
if (nrow(frozen_primary) != 1L ||
    abs(
      neighbour_replay$observed_focal_minus_white_neighbour -
        frozen_primary$observed_focal_minus_white_neighbour
    ) > 1e-12 ||
    abs(
      neighbour_replay$directional_or_two_sided_p -
        frozen_primary$directional_or_two_sided_p
    ) > 1e-12) {
  stop("The identical v21 5-km neighbour replay was not reproduced.",
       call. = FALSE)
}

profile_list <- list(
  pigmented_among_white = list(
    observed = observed_forward, simulated = simulated_forward
  ),
  white_among_pigmented = list(
    observed = observed_reverse, simulated = simulated_reverse
  )
)
detector_opportunity <- do.call(rbind, lapply(
  names(profile_list), function(direction) {
    profile <- profile_list[[direction]]
    simulated_count <- colSums(profile$simulated$candidate)
    simulated_opportunity <- colSums(profile$simulated$focal_eligible)
    data.frame(
      direction = direction,
      observed_supported_focal_opportunity = sum(
        profile$observed$focal_eligible[, 1L]
      ),
      observed_candidate_count = sum(
        profile$observed$candidate[, 1L]
      ),
      null_mean_supported_focal_opportunity = mean(simulated_opportunity),
      null_sd_supported_focal_opportunity = stats::sd(simulated_opportunity),
      null_mean_candidate_count = mean(simulated_count),
      null_sd_candidate_count = stats::sd(simulated_count),
      null_zero_candidate_draws = sum(simulated_count == 0L),
      stringsAsFactors = FALSE
    )
  }
))

forward_membership <- data.frame(
  direction = "pigmented_among_white",
  exact_site_id = cells$exact_site_id[observed_forward$candidate[, 1L]],
  longitude = cells$longitude[observed_forward$candidate[, 1L]],
  latitude = cells$latitude[observed_forward$candidate[, 1L]],
  stringsAsFactors = FALSE
)
reverse_membership <- data.frame(
  direction = "white_among_pigmented",
  exact_site_id = cells$exact_site_id[observed_reverse$candidate[, 1L]],
  longitude = cells$longitude[observed_reverse$candidate[, 1L]],
  latitude = cells$latitude[observed_reverse$candidate[, 1L]],
  stringsAsFactors = FALSE
)

utils::write.csv(
  placement_summary,
  file.path(output_dir, "placement_null_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  placement_null,
  file.path(output_dir, "placement_null_draws.csv"),
  row.names = FALSE
)
utils::write.csv(
  scale_profile,
  file.path(output_dir, "placement_null_scale_profile.csv"),
  row.names = FALSE
)
utils::write.csv(
  neighbour_replay,
  file.path(output_dir, "placement_null_neighbour_replay_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  detector_opportunity,
  file.path(output_dir, "placement_null_detector_opportunity.csv"),
  row.names = FALSE
)
utils::write.csv(
  reconstruction_audit,
  file.path(output_dir, "placement_null_worldpop_reconstruction_audit.csv"),
  row.names = FALSE
)
utils::write.csv(
  exposure,
  file.path(output_dir, "placement_null_population_exposure.csv"),
  row.names = FALSE
)
utils::write.csv(
  forward_membership,
  file.path(output_dir, "placement_null_forward_candidates.csv"),
  row.names = FALSE
)
utils::write.csv(
  reverse_membership,
  file.path(output_dir, "placement_null_reverse_candidates.csv"),
  row.names = FALSE
)
utils::write.csv(
  graph$settings,
  file.path(output_dir, "placement_null_graph_registry.csv"),
  row.names = FALSE
)
utils::write.csv(
  current_event,
  file.path(output_dir, "placement_null_retained_v20_calibration.csv"),
  row.names = FALSE
)

v22_profile_figure(
  placement_summary,
  file.path(output_dir, "placement_null_scale_profile")
)

forward_primary <- placement_summary[
  placement_summary$direction == "pigmented_among_white" &
    placement_summary$radius_km == 5, , drop = FALSE
]
reverse_primary <- placement_summary[
  placement_summary$direction == "white_among_pigmented" &
    placement_summary$radius_km == 5, , drop = FALSE
]
forward_profile <- scale_profile[
  scale_profile$direction == "pigmented_among_white", , drop = FALSE
]
reverse_profile <- scale_profile[
  scale_profile$direction == "white_among_pigmented", , drop = FALSE
]

validation <- list(
  status = "PASS",
  spec_version = v22_analysis_spec_version,
  preregistration_commit = "5247cfd1c60a7600fc6a10e2238ff5689c2bf67b",
  permanent_preregistration_ref =
    "prereg/v22-placement-null-human-context-2026-08-18",
  n_cells = nrow(cells),
  n_maps = n_draws,
  seed = seed,
  inputs = list(
    broad_artifact = 9022276431,
    broad_sha256 =
      "0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240",
    predictive_draw_artifact = 9094339466,
    predictive_draw_sha256 =
      "413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1",
    human_replay_artifact = 9119306089,
    human_replay_sha256 =
      "f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4",
    worldpop_md5 = unname(tools::md5sum(worldpop_raster))
  ),
  reconstruction = list(
    maximum_rank_difference = max(
      reconstruction_audit$maximum_absolute_rank_difference
    ),
    radii_reproduced_km = c(5, 10, 25)
  ),
  forward = list(
    observed_candidates = nrow(forward_membership),
    primary_radius_km = 5,
    observed_mean_exposure_rank = forward_primary$observed_value,
    null_mean = forward_primary$null_mean,
    observed_minus_null_mean =
      forward_primary$observed_minus_null_mean,
    empirical_p = forward_primary$empirical_p,
    maxT_FWER_p = forward_primary$maxT_FWER_p,
    profile_shape = forward_profile$profile_shape
  ),
  reverse = list(
    observed_candidates = nrow(reverse_membership),
    primary_radius_km = 5,
    observed_mean_exposure_rank = reverse_primary$observed_value,
    null_mean = reverse_primary$null_mean,
    observed_minus_null_mean =
      reverse_primary$observed_minus_null_mean,
    empirical_p = reverse_primary$empirical_p,
    maxT_FWER_p = reverse_primary$maxT_FWER_p,
    profile_shape = reverse_profile$profile_shape
  ),
  retained_results = list(
    v20_candidate_count_p = current_event$empirical_p[
      current_event$metric == "candidate_count"
    ],
    v20_candidate_fraction_p = current_event$empirical_p[
      current_event$metric == "candidate_fraction"
    ],
    v21_population_5km_contrast =
      neighbour_replay$observed_focal_minus_white_neighbour,
    v21_population_5km_directional_p =
      neighbour_replay$directional_or_two_sided_p
  ),
  claim_ceiling = paste(
    "Placement relative to the fitted natural process is not evidence of",
    "horticultural origin, gene flow, cultivar identity or pollen direction."
  )
)
jsonlite::write_json(
  validation,
  file.path(output_dir, "placement_null_validation.json"),
  pretty = TRUE, auto_unbox = TRUE, na = "null"
)

result_lines <- c(
  "# Placement-null human-context reanalysis",
  "",
  paste0("Spec: `", v22_analysis_spec_version, "`."),
  "",
  paste0(
    "Forward 5-km placement: observed mean rank ",
    formatC(forward_primary$observed_value, digits = 5, format = "f"),
    "; natural-placement mean ",
    formatC(forward_primary$null_mean, digits = 5, format = "f"),
    "; observed minus null ",
    formatC(
      forward_primary$observed_minus_null_mean,
      digits = 5, format = "f", flag = "+"
    ),
    "; upper-tail Monte Carlo P ",
    formatC(forward_primary$empirical_p, digits = 5, format = "f"),
    "; four-radius maxT P ",
    formatC(forward_primary$maxT_FWER_p, digits = 5, format = "f"),
    "."
  ),
  paste0(
    "Forward scale profile: `", forward_profile$profile_shape,
    "`; effects at 2/5/10/25 km = ",
    forward_profile$effect_profile, "."
  ),
  paste0(
    "Reverse 5-km placement: observed candidates ",
    nrow(reverse_membership), "; observed minus null ",
    formatC(
      reverse_primary$observed_minus_null_mean,
      digits = 5, format = "f", flag = "+"
    ),
    "; upper-tail Monte Carlo P ",
    formatC(reverse_primary$empirical_p, digits = 5, format = "f"),
    "."
  ),
  "",
  paste(
    "The 16 forward cells remain field/provenance targets.",
    "No result establishes horticultural origin, gene flow or cultivar identity."
  )
)
writeLines(result_lines, file.path(output_dir, "RESULT_SUMMARY.md"))

message(
  "[v22] PASS; forward 5-km effect=",
  signif(forward_primary$observed_minus_null_mean, 5),
  "; P=", signif(forward_primary$empirical_p, 5),
  "; profile=", forward_profile$profile_shape,
  "; reverse candidates=", nrow(reverse_membership)
)
