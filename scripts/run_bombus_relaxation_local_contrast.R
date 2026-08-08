args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
presence_path <- arg_value(
  "--presence",
  paste0(
    "results/ecological_v16_predictive_replication/checkpoints/",
    "national_environment_spde_presence_draws1000.rds"
  )
)
intensity_path <- arg_value(
  "--intensity",
  paste0(
    "results/ecological_v16_predictive_replication/checkpoints/",
    "national_environment_spde_intensity_draws1000.rds"
  )
)
output_dir <- arg_value(
  "--output", "results/ecological_v17_bombus_relaxation_local"
)
env_match <- as.numeric(arg_value("--env-match", "0.75"))
radii <- as.numeric(strsplit(arg_value("--radii", "10,25,50"), ",")[[1]])
primary_radius <- as.numeric(arg_value("--primary-radius", "25"))
k <- as.integer(arg_value("--k", "5"))

if (!primary_radius %in% radii) {
  stop("Primary radius must be included in the fixed radius grid.", call. = FALSE)
}

hb_load_modules("local_bombus_turnover")

required_files <- c(cells_path, presence_path, intensity_path)
if (!all(file.exists(required_files))) {
  stop(
    "Missing fresh-input prerequisites: ",
    paste(required_files[!file.exists(required_files)], collapse = ", "),
    call. = FALSE
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
    rank_columns, "bombus_total_habitat_support",
    "bombus_fingerprint_common_support", "n_observations", "n_pigmented",
    "conditional_intensity_median", "spatial_fold", "x_km", "y_km",
    "broad50km_pc1", "broad50km_pc2", "within50km_pc1", "within50km_pc2"
  ),
  "cells"
)

rank_matrix <- as.matrix(cells[, rank_columns, drop = FALSE])
storage.mode(rank_matrix) <- "double"
computed_total <- rowMeans(rank_matrix)
if (max(abs(computed_total - cells$bombus_total_habitat_support), na.rm = TRUE) > 1e-10) {
  stop("Existing all-five total-support index is inconsistent with rank mean.", call. = FALSE)
}

# These continuous indices are fixed before outcome inspection. They are habitat-
# support summaries only; they are not abundance, visitation, or occurrence
# probabilities. The all-five mean is primary. Guild splits are sensitivities.
cells$bombus_support_all5_mean <- as.numeric(cells$bombus_total_habitat_support)
cells$bombus_support_widespread_mean <- rowMeans(
  rank_matrix[, match(c("ardens", "diversus"), species), drop = FALSE]
)
cells$bombus_support_montane_mean <- rowMeans(
  rank_matrix[, match(c("beaticola", "consobrinus", "honshuensis"), species), drop = FALSE]
)

exposure_specs <- data.frame(
  exposure = c(
    "all5_total_support", "widespread_ardens_diversus_mean",
    "montane_beaticola_consobrinus_honshuensis_mean"
  ),
  column = c(
    "bombus_support_all5_mean", "bombus_support_widespread_mean",
    "bombus_support_montane_mean"
  ),
  role = c("primary_directional", "guild_sensitivity", "guild_sensitivity"),
  stringsAsFactors = FALSE
)

trials <- pmax(as.numeric(cells$n_observations), 1)
observed_share <- as.numeric(cells$n_pigmented) / trials
presence_draw_share <- sweep(presence$draws, 1, trials, "/")
observed_intensity <- as.numeric(cells$conditional_intensity_median)

safe_rbind <- function(parts) {
  parts <- parts[vapply(parts, function(x) !is.null(x) && nrow(x) > 0L, logical(1))]
  if (!length(parts)) return(data.frame())
  do.call(rbind, parts)
}

mean_null_comparison <- function(observed, simulated) {
  cmp <- v17_null_comparison(observed, simulated)
  c(
    null_mean = cmp[["null_mean"]],
    null_sd = cmp[["null_sd"]],
    null_lower_95 = cmp[["null_lower_95"]],
    null_upper_95 = cmp[["null_upper_95"]],
    empirical_p = cmp[["empirical_p"]],
    two_sided_p = cmp[["empirical_two_sided_p"]],
    percentile = cmp[["percentile"]]
  )
}

run_directional <- function(edges, exposure_name, exposure_column, exposure_role,
                            response = c("presence", "intensity")) {
  response <- match.arg(response)
  support <- as.numeric(cells[[exposure_column]])
  i <- edges$i
  j <- edges$j
  si <- support[i]
  sj <- support[j]
  finite <- is.finite(si) & is.finite(sj) & abs(si - sj) > 1e-12
  d <- edges[finite, , drop = FALSE]
  if (!nrow(d)) return(list(summary = data.frame(), null = data.frame()))
  i <- d$i
  j <- d$j
  si <- support[i]
  sj <- support[j]
  j_high <- sj > si
  high <- ifelse(j_high, j, i)
  low <- ifelse(j_high, i, j)
  delta_support <- abs(sj - si)
  mean_support <- (si + sj) / 2

  if (response == "presence") {
    observed <- observed_share[high] - observed_share[low]
    simulated <- presence_draw_share[high, , drop = FALSE] -
      presence_draw_share[low, , drop = FALSE]
    natural_signed <- presence$latent_mean[high] - presence$latent_mean[low]
    midpoint <- rowMeans(cbind(presence$latent_mean[high], presence$latent_mean[low]))
    midpoint <- pmin(pmax(midpoint, 0), 1)
    natural_uncertainty <- 4 * midpoint * (1 - midpoint)
    ni <- pmax(as.numeric(cells$n_observations[high]), 1)
    nj <- pmax(as.numeric(cells$n_observations[low]), 1)
    effort <- log1p(2 / (1 / ni + 1 / nj))
    baseline <- data.frame(
      log_geographic_distance = log1p(d$geographic_distance_km),
      environmental_distance = d$environmental_distance,
      natural_signed_difference = natural_signed,
      natural_uncertainty = natural_uncertainty,
      log_pair_effort = effort,
      mean_bombus_support = mean_support
    )
  } else {
    endpoint_ok <- is.finite(observed_intensity[high]) & is.finite(observed_intensity[low])
    d <- d[endpoint_ok, , drop = FALSE]
    high <- high[endpoint_ok]
    low <- low[endpoint_ok]
    delta_support <- delta_support[endpoint_ok]
    mean_support <- mean_support[endpoint_ok]
    if (!nrow(d)) return(list(summary = data.frame(), null = data.frame()))
    observed <- observed_intensity[high] - observed_intensity[low]
    simulated <- intensity$draws[high, , drop = FALSE] -
      intensity$draws[low, , drop = FALSE]
    natural_signed <- intensity$latent_mean[high] - intensity$latent_mean[low]
    npi <- pmax(as.numeric(cells$n_pigmented[high]), 1)
    npj <- pmax(as.numeric(cells$n_pigmented[low]), 1)
    effort <- log1p(2 / (1 / npi + 1 / npj))
    baseline <- data.frame(
      log_geographic_distance = log1p(d$geographic_distance_km),
      environmental_distance = d$environmental_distance,
      natural_signed_difference = natural_signed,
      log_pair_effort = effort,
      mean_bombus_support = mean_support
    )
  }

  observed_stat <- v17_fit_partial_statistic(observed, baseline, delta_support)
  null_stat <- t(vapply(seq_len(ncol(simulated)), function(draw) {
    v17_fit_partial_statistic(
      simulated[, draw], baseline, delta_support
    )[c("beta", "delta_r2")]
  }, numeric(2)))
  beta_cmp <- v17_null_comparison(observed_stat[["beta"]], null_stat[, "beta"])
  r2_cmp <- v17_null_comparison(observed_stat[["delta_r2"]], null_stat[, "delta_r2"])

  observed_mean <- mean(observed, na.rm = TRUE)
  null_mean_stat <- colMeans(simulated, na.rm = TRUE)
  mean_cmp <- mean_null_comparison(observed_mean, null_mean_stat)

  summary <- data.frame(
    response = response,
    radius_km = unique(d$radius_km),
    environmental_caliper = env_match,
    exposure = exposure_name,
    exposure_role = exposure_role,
    n_pairs = as.integer(observed_stat[["n"]]),
    n_nodes = length(unique(c(d$site_i, d$site_j))),
    n_spatial_folds = length(unique(d$fold_i)),
    observed_partial_beta = observed_stat[["beta"]],
    beta_null_mean = beta_cmp[["null_mean"]],
    beta_null_sd = beta_cmp[["null_sd"]],
    beta_null_lower_95 = beta_cmp[["null_lower_95"]],
    beta_null_upper_95 = beta_cmp[["null_upper_95"]],
    beta_empirical_p = beta_cmp[["empirical_p"]],
    beta_two_sided_p = beta_cmp[["empirical_two_sided_p"]],
    beta_percentile = beta_cmp[["percentile"]],
    observed_delta_r2 = observed_stat[["delta_r2"]],
    delta_r2_null_mean = r2_cmp[["null_mean"]],
    delta_r2_empirical_p = r2_cmp[["empirical_p"]],
    observed_mean_directed_difference = observed_mean,
    mean_difference_null_mean = mean_cmp[["null_mean"]],
    mean_difference_null_lower_95 = mean_cmp[["null_lower_95"]],
    mean_difference_null_upper_95 = mean_cmp[["null_upper_95"]],
    mean_difference_empirical_p = mean_cmp[["empirical_p"]],
    mean_difference_two_sided_p = mean_cmp[["two_sided_p"]],
    stringsAsFactors = FALSE
  )

  null <- data.frame(
    response = response,
    radius_km = unique(d$radius_km),
    exposure = exposure_name,
    draw = seq_len(nrow(null_stat)),
    partial_beta = null_stat[, "beta"],
    delta_r2 = null_stat[, "delta_r2"],
    mean_directed_difference = null_mean_stat,
    stringsAsFactors = FALSE
  )

  list(summary = summary, null = null)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

directional_summaries <- list()
directional_nulls <- list()
community_summaries <- list()
community_nulls <- list()
edge_diagnostics <- list()

for (radius in radii) {
  message("[Bombus relaxation] building response-blind pair graph at ", radius, " km")
  edges_all <- v17_pair_graph(
    cells, radius_km = radius, k = k,
    same_fold_only = TRUE, common_support_only = TRUE
  )
  edges_all <- v17_add_pair_features(edges_all, cells, presence, intensity)
  edges <- edges_all[
    is.finite(edges_all$environmental_distance) &
      edges_all$environmental_distance <= env_match,
    , drop = FALSE
  ]

  edge_diagnostics[[as.character(radius)]] <- data.frame(
    radius_km = radius,
    n_edges_before_environment_caliper = nrow(edges_all),
    n_edges_after_environment_caliper = nrow(edges),
    n_nodes_after_environment_caliper = length(unique(c(edges$site_i, edges$site_j))),
    environmental_caliper = env_match,
    stringsAsFactors = FALSE
  )

  if (!nrow(edges)) next

  for (row in seq_len(nrow(exposure_specs))) {
    spec <- exposure_specs[row, ]
    for (response in c("presence", "intensity")) {
      ans <- run_directional(
        edges,
        exposure_name = spec$exposure,
        exposure_column = spec$column,
        exposure_role = spec$role,
        response = response
      )
      key <- paste(radius, spec$exposure, response, sep = "__")
      directional_summaries[[key]] <- ans$summary
      directional_nulls[[key]] <- ans$null
    }
  }

  for (response in c("presence", "intensity")) {
    ans <- v17_pair_predictive_test(edges, cells, if (response == "presence") presence else intensity, response)
    key <- paste(radius, response, sep = "__")
    community_summaries[[key]] <- ans$summary
    community_nulls[[key]] <- ans$null
  }
}

directional_summary <- safe_rbind(directional_summaries)
directional_null <- safe_rbind(directional_nulls)
community_summary <- safe_rbind(community_summaries)
community_null <- safe_rbind(community_nulls)
edge_diagnostic <- safe_rbind(edge_diagnostics)

if (!nrow(directional_summary) || !nrow(community_summary)) {
  stop("Local Bombus analysis produced no estimable summaries.", call. = FALSE)
}

directional_summary$is_primary_test <-
  directional_summary$radius_km == primary_radius &
  directional_summary$response == "presence" &
  directional_summary$exposure == "all5_total_support"
directional_summary$BH_q_all_directional_tests <- stats::p.adjust(
  directional_summary$beta_empirical_p, method = "BH"
)

community_summary <- v17_adjust_multiplicity(community_summary)
community_summary$environmental_caliper <- env_match
community_summary$is_required_corroboration <-
  community_summary$radius_km == primary_radius &
  community_summary$response == "presence" &
  community_summary$predictor == "fingerprint_turnover"

primary_directional <- directional_summary[directional_summary$is_primary_test, , drop = FALSE]
primary_community <- community_summary[community_summary$is_required_corroboration, , drop = FALSE]
if (nrow(primary_directional) != 1L || nrow(primary_community) != 1L) {
  stop("Primary directional/corroborating test could not be identified uniquely.", call. = FALSE)
}

directional_supported <-
  is.finite(primary_directional$observed_partial_beta) &
  primary_directional$observed_partial_beta > 0 &
  is.finite(primary_directional$beta_empirical_p) &
  primary_directional$beta_empirical_p < 0.05
community_supported <-
  is.finite(primary_community$observed_partial_beta) &
  primary_community$observed_partial_beta > 0 &
  is.finite(primary_community$beta_empirical_p) &
  primary_community$beta_empirical_p < 0.05

status <- if (directional_supported && community_supported) {
  "convergent_local_sdm_support_consistent_with_relaxation"
} else if (directional_supported) {
  "directional_support_without_required_community_corroboration"
} else if (community_supported) {
  "community_turnover_alignment_without_directional_relaxation_support"
} else {
  "no_excess_local_sdm_support_for_relaxation_after_natural_null"
}

interpretation <- data.frame(
  field = c(
    "status", "causal_hypothesis", "primary_estimand", "primary_radius_km",
    "environmental_caliper", "pair_construction", "natural_null",
    "bombus_primary_exposure", "community_corroboration",
    "old_max_gate_role", "claim_ceiling"
  ),
  value = c(
    status,
    "lower effective bumblebee availability relaxes pollinator-mediated selection maintaining floral pigmentation",
    "signed local pigmentation change toward higher continuous all-five Bombus habitat support",
    as.character(primary_radius),
    as.character(env_match),
    "response-blind k-nearest local graph; same held-out spatial fold; five-species common support",
    paste0(ncol(presence$draws), " cross-fitted environment-plus-SPDE posterior predictive flower maps"),
    "mean of five within-species habitat-support ranks; habitat support only, not visitation or abundance",
    "absolute pigmentation turnover versus five-species support-plus-composition fingerprint turnover",
    "retired as primary; no threshold retuning after zero low-support cells in fresh SDMs",
    "association beyond fitted abiotic-plus-spatial flower null; consistent with mechanism but not causal identification"
  ),
  stringsAsFactors = FALSE
)

support_diagnostics <- data.frame(
  metric = c(
    "all5_total_support", "widespread_ardens_diversus_mean",
    "montane_beaticola_consobrinus_honshuensis_mean", "all5_max_rank"
  ),
  minimum = c(
    min(cells$bombus_support_all5_mean, na.rm = TRUE),
    min(cells$bombus_support_widespread_mean, na.rm = TRUE),
    min(cells$bombus_support_montane_mean, na.rm = TRUE),
    min(apply(rank_matrix, 1, max), na.rm = TRUE)
  ),
  median = c(
    stats::median(cells$bombus_support_all5_mean, na.rm = TRUE),
    stats::median(cells$bombus_support_widespread_mean, na.rm = TRUE),
    stats::median(cells$bombus_support_montane_mean, na.rm = TRUE),
    stats::median(apply(rank_matrix, 1, max), na.rm = TRUE)
  ),
  maximum = c(
    max(cells$bombus_support_all5_mean, na.rm = TRUE),
    max(cells$bombus_support_widespread_mean, na.rm = TRUE),
    max(cells$bombus_support_montane_mean, na.rm = TRUE),
    max(apply(rank_matrix, 1, max), na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  directional_summary,
  file.path(output_dir, "bombus_relaxation_directional_summary.csv"), row.names = FALSE
)
utils::write.csv(
  directional_null,
  file.path(output_dir, "bombus_relaxation_directional_null.csv"), row.names = FALSE
)
utils::write.csv(
  community_summary,
  file.path(output_dir, "bombus_relaxation_community_turnover_summary.csv"), row.names = FALSE
)
utils::write.csv(
  community_null,
  file.path(output_dir, "bombus_relaxation_community_turnover_null.csv"), row.names = FALSE
)
utils::write.csv(
  edge_diagnostic,
  file.path(output_dir, "bombus_relaxation_pair_diagnostics.csv"), row.names = FALSE
)
utils::write.csv(
  support_diagnostics,
  file.path(output_dir, "bombus_relaxation_support_diagnostics.csv"), row.names = FALSE
)
utils::write.csv(
  interpretation,
  file.path(output_dir, "bombus_relaxation_interpretation.csv"), row.names = FALSE
)

readme <- c(
  "# Fresh-SDM local Bombus-relaxation analysis",
  "",
  paste0("Status: **", status, "**"),
  "",
  paste0(
    "The primary test uses a response-blind 25-km local pair graph, same held-out ",
    "spatial fold, five-species common SDM support, and environmental distance <= ",
    env_match, ". Pairs are oriented from lower to higher continuous all-five ",
    "Bombus habitat support."
  ),
  "",
  paste0(
    "The observed directional flower-colour statistic is compared with the same ",
    "statistic on ", ncol(presence$draws), " cross-fitted environment-plus-SPDE ",
    "posterior predictive flower maps. Therefore shared environmental and broad ",
    "spatial structure expected under the natural model is represented in the null."
  ),
  "",
  "A required corroboration tests absolute flower-colour turnover against the five-species Bombus community fingerprint turnover on the same environmentally matched local pairs.",
  "",
  "The analysis does not estimate visitation, abundance, pollination effectiveness, or a causal selection coefficient."
)
writeLines(readme, file.path(output_dir, "README.md"), useBytes = TRUE)

cat("Bombus-relaxation local analysis written to ", normalizePath(output_dir), "\n", sep = "")
cat("Status: ", status, "\n", sep = "")
print(primary_directional)
print(primary_community)
