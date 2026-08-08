#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
hb_load_modules("local_bombus_turnover")

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
support_path <- arg_value(
  "--support",
  paste0(
    "results/ecological_v17_bombus_effective_availability_refined/",
    "cell_effective_bombus_support.csv"
  )
)
output_dir <- arg_value(
  "--output",
  "results/ecological_v18_bombus_local_sharp_transition"
)
radii <- as.numeric(strsplit(arg_value("--radii", "5,10,25"), ",")[[1]])
k <- as.integer(arg_value("--k", "5"))
thresholds <- as.numeric(strsplit(arg_value("--thresholds", "1,0.75,0.5"), ",")[[1]])
randomisations <- as.integer(arg_value("--randomisations", "100000"))
seed <- as.integer(arg_value("--seed", "20260808"))

if (!file.exists(cells_path) || !file.exists(support_path)) {
  stop("Missing local-transition input file.", call. = FALSE)
}
if (!length(radii) || any(!is.finite(radii)) || any(radii <= 0)) {
  stop("Radii must be positive.", call. = FALSE)
}
if (!length(thresholds) || any(!is.finite(thresholds)) ||
    any(thresholds <= 0 | thresholds > 1)) {
  stop("Transition thresholds must be in (0,1].", call. = FALSE)
}
if (!is.finite(randomisations) || randomisations < 1000L) {
  stop("Use at least 1000 sign-flip randomisations.", call. = FALSE)
}

cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
support <- utils::read.csv(support_path, check.names = FALSE, stringsAsFactors = FALSE)
idx <- match(cells$exact_site_id, support$exact_site_id)
if (anyNA(idx) || anyDuplicated(idx)) {
  stop("Effective-Bombus support cannot be aligned one-to-one with cells.", call. = FALSE)
}
support <- support[idx, , drop = FALSE]

v17_require_columns(
  cells,
  c(
    "exact_site_id", "x_km", "y_km", "longitude", "latitude",
    "bombus_fingerprint_common_support", "pigment_share", "n_observations",
    "elevation", "bee_ardens", "bee_diversus",
    "broad50km_pc1", "broad50km_pc2", "within50km_pc1", "within50km_pc2"
  ),
  "fresh cells"
)
v17_require_columns(
  support,
  c(
    "exact_site_id", "effective_occmax", "effective_rawmax", "all5_occmax",
    "beaticola_occurrence_reference", "consobrinus_occurrence_reference",
    "honshuensis_occurrence_reference"
  ),
  "effective-Bombus support"
)

support$montane_occmax <- pmax(
  as.numeric(support$beaticola_occurrence_reference),
  as.numeric(support$consobrinus_occurrence_reference),
  as.numeric(support$honshuensis_occurrence_reference)
)
exposure_names <- c(
  "effective_occmax", "effective_rawmax", "montane_occmax", "all5_occmax"
)
exposure_role <- c(
  effective_occmax = "documented_effective_guild_primary",
  effective_rawmax = "documented_effective_guild_raw_sensitivity",
  montane_occmax = "potential_montane_substitution_diagnostic",
  all5_occmax = "any_focal_bombus_sensitivity"
)

for (nm in exposure_names) {
  if (any(!is.finite(as.numeric(support[[nm]])))) {
    stop("Non-finite exposure values in ", nm, call. = FALSE)
  }
}

environment <- v17_environment_matrix(cells)
colour <- as.numeric(cells$pigment_share)
elevation <- as.numeric(cells$elevation)

signflip_mean_test <- function(values, B, seed_value) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (!length(values)) {
    return(c(observed = NA_real_, p = NA_real_, null_mean = NA_real_, null_sd = NA_real_))
  }
  observed <- mean(values)
  magnitudes <- abs(values)
  set.seed(seed_value)
  ge <- 0L
  total <- 0L
  null_sum <- 0
  null_sum_sq <- 0
  chunk <- 5000L
  while (total < B) {
    m <- min(chunk, B - total)
    signs <- matrix(
      sample(c(-1, 1), m * length(magnitudes), replace = TRUE),
      nrow = m, ncol = length(magnitudes)
    )
    simulated <- rowMeans(sweep(signs, 2, magnitudes, "*"))
    ge <- ge + sum(simulated >= observed)
    null_sum <- null_sum + sum(simulated)
    null_sum_sq <- null_sum_sq + sum(simulated^2)
    total <- total + m
  }
  null_mean <- null_sum / total
  null_var <- max(0, (null_sum_sq - total * null_mean^2) / max(1, total - 1L))
  c(
    observed = observed,
    p = (ge + 1) / (total + 1),
    null_mean = null_mean,
    null_sd = sqrt(null_var)
  )
}

joint_substitution_test <- function(delta_effective, delta_montane, B, seed_value) {
  de <- as.numeric(delta_effective)
  dm <- as.numeric(delta_montane)
  keep <- is.finite(de) & is.finite(dm)
  de <- de[keep]
  dm <- dm[keep]
  observed_subset <- de <= 0
  if (!length(de) || !any(observed_subset)) {
    return(c(
      n_pairs = length(de), n_subset = sum(observed_subset), observed = NA_real_,
      p = NA_real_, null_mean = NA_real_, null_sd = NA_real_
    ))
  }
  observed <- mean(dm[observed_subset])
  set.seed(seed_value)
  ge <- 0L
  total <- 0L
  null_sum <- 0
  null_sum_sq <- 0
  chunk <- 2500L
  while (total < B) {
    m <- min(chunk, B - total)
    signs <- matrix(sample(c(-1, 1), m * length(de), replace = TRUE), nrow = m)
    de_null <- sweep(signs, 2, de, "*")
    dm_null <- sweep(signs, 2, dm, "*")
    use <- de_null <= 0
    counts <- rowSums(use)
    numerator <- rowSums(dm_null * use)
    simulated <- numerator / pmax(counts, 1L)
    valid <- counts > 0
    simulated <- simulated[valid]
    ge <- ge + sum(simulated >= observed)
    null_sum <- null_sum + sum(simulated)
    null_sum_sq <- null_sum_sq + sum(simulated^2)
    total <- total + length(simulated)
  }
  null_mean <- null_sum / total
  null_var <- max(0, (null_sum_sq - total * null_mean^2) / max(1, total - 1L))
  c(
    n_pairs = length(de),
    n_subset = sum(observed_subset),
    observed = observed,
    p = (ge + 1) / (total + 1),
    null_mean = null_mean,
    null_sd = sqrt(null_var)
  )
}

greedy_nonoverlap <- function(edges) {
  if (!nrow(edges)) return(edges)
  d <- edges[order(
    -edges$abs_colour_difference,
    edges$geographic_distance_km,
    edges$site_i, edges$site_j
  ), , drop = FALSE]
  used <- integer()
  keep <- logical(nrow(d))
  for (r in seq_len(nrow(d))) {
    a <- as.integer(d$i[[r]])
    b <- as.integer(d$j[[r]])
    if (!(a %in% used) && !(b %in% used)) {
      keep[[r]] <- TRUE
      used <- c(used, a, b)
    }
  }
  d[keep, , drop = FALSE]
}

pair_details <- function(edges, radius, threshold) {
  if (!nrow(edges)) return(data.frame())
  i <- as.integer(edges$i)
  j <- as.integer(edges$j)
  d_colour <- colour[j] - colour[i]
  sharp <- is.finite(d_colour) & abs(d_colour) + 1e-12 >= threshold
  d <- edges[sharp, , drop = FALSE]
  if (!nrow(d)) return(data.frame())
  d$colour_difference <- d_colour[sharp]
  d$abs_colour_difference <- abs(d$colour_difference)
  d <- greedy_nonoverlap(d)
  i <- as.integer(d$i)
  j <- as.integer(d$j)
  j_pigmented <- d$colour_difference > 0
  pig <- ifelse(j_pigmented, j, i)
  white <- ifelse(j_pigmented, i, j)
  out <- data.frame(
    radius_km = radius,
    transition_threshold = threshold,
    white_site = as.character(cells$exact_site_id[white]),
    pigmented_site = as.character(cells$exact_site_id[pig]),
    geographic_distance_km = as.numeric(d$geographic_distance_km),
    abs_colour_difference = as.numeric(d$abs_colour_difference),
    white_pigment_share = colour[white],
    pigmented_pigment_share = colour[pig],
    white_n_observations = as.numeric(cells$n_observations[white]),
    pigmented_n_observations = as.numeric(cells$n_observations[pig]),
    white_longitude = as.numeric(cells$longitude[white]),
    white_latitude = as.numeric(cells$latitude[white]),
    pigmented_longitude = as.numeric(cells$longitude[pig]),
    pigmented_latitude = as.numeric(cells$latitude[pig]),
    white_elevation = elevation[white],
    pigmented_elevation = elevation[pig],
    signed_elevation_difference = elevation[pig] - elevation[white],
    absolute_elevation_difference = abs(elevation[pig] - elevation[white]),
    environmental_distance = v17_pair_distance(environment, white, pig),
    stringsAsFactors = FALSE
  )
  for (nm in exposure_names) {
    x <- as.numeric(support[[nm]])
    out[[paste0("white_", nm)]] <- x[white]
    out[[paste0("pigmented_", nm)]] <- x[pig]
    out[[paste0("delta_", nm)]] <- x[pig] - x[white]
  }
  out
}

summary_rows <- list()
all_edge_rows <- list()
pair_rows <- list()
substitution_rows <- list()
row_id <- 0L
all_id <- 0L
pair_id <- 0L
sub_id <- 0L

for (radius in radii) {
  message("[sharp-transition] local graph radius = ", radius, " km")
  edges <- v17_pair_graph(
    cells,
    radius_km = radius,
    k = k,
    same_fold_only = FALSE,
    common_support_only = TRUE
  )
  if (!nrow(edges)) next
  i_all <- as.integer(edges$i)
  j_all <- as.integer(edges$j)
  edges$environmental_distance <- v17_pair_distance(environment, i_all, j_all)
  all_graph_env_median <- stats::median(edges$environmental_distance, na.rm = TRUE)
  all_graph_elev_median <- stats::median(
    abs(elevation[i_all] - elevation[j_all]), na.rm = TRUE
  )

  for (threshold in thresholds) {
    d_colour <- colour[j_all] - colour[i_all]
    sharp <- is.finite(d_colour) & abs(d_colour) + 1e-12 >= threshold
    sharp_edges <- edges[sharp, , drop = FALSE]
    if (!nrow(sharp_edges)) next
    sharp_edges$colour_difference <- d_colour[sharp]
    sharp_edges$abs_colour_difference <- abs(sharp_edges$colour_difference)

    # All sharp edges are descriptive only because endpoints can repeat.
    i <- as.integer(sharp_edges$i)
    j <- as.integer(sharp_edges$j)
    pig <- ifelse(sharp_edges$colour_difference > 0, j, i)
    white <- ifelse(sharp_edges$colour_difference > 0, i, j)
    for (nm in exposure_names) {
      x <- as.numeric(support[[nm]])
      delta <- x[pig] - x[white]
      all_id <- all_id + 1L
      all_edge_rows[[all_id]] <- data.frame(
        radius_km = radius,
        transition_threshold = threshold,
        exposure = nm,
        exposure_role = unname(exposure_role[[nm]]),
        n_edges = length(delta),
        mean_signed_difference = mean(delta),
        median_signed_difference = stats::median(delta),
        proportion_positive = mean(delta > 0),
        stringsAsFactors = FALSE
      )
    }

    details <- pair_details(edges, radius, threshold)
    if (!nrow(details)) next
    pair_id <- pair_id + 1L
    pair_rows[[pair_id]] <- details

    for (nm in exposure_names) {
      delta <- as.numeric(details[[paste0("delta_", nm)]])
      test <- signflip_mean_test(
        delta,
        B = randomisations,
        seed_value = seed + as.integer(radius * 100) +
          as.integer(round(threshold * 1000)) + match(nm, exposure_names)
      )
      row_id <- row_id + 1L
      summary_rows[[row_id]] <- data.frame(
        radius_km = radius,
        transition_threshold = threshold,
        exposure = nm,
        exposure_role = unname(exposure_role[[nm]]),
        focal_strict_local_test = radius == 5 && abs(threshold - 1) < 1e-12 &&
          nm == "effective_occmax",
        n_nonoverlapping_pairs = nrow(details),
        median_geographic_distance_km = stats::median(details$geographic_distance_km),
        median_environmental_distance_selected = stats::median(
          details$environmental_distance, na.rm = TRUE
        ),
        median_environmental_distance_all_local_edges = all_graph_env_median,
        median_absolute_elevation_difference_selected = stats::median(
          details$absolute_elevation_difference, na.rm = TRUE
        ),
        median_absolute_elevation_difference_all_local_edges = all_graph_elev_median,
        mean_signed_bombus_difference = unname(test[["observed"]]),
        median_signed_bombus_difference = stats::median(delta),
        proportion_positive = mean(delta > 0),
        proportion_nonnegative = mean(delta >= 0),
        signflip_null_mean = unname(test[["null_mean"]]),
        signflip_null_sd = unname(test[["null_sd"]]),
        signflip_one_sided_p = unname(test[["p"]]),
        stringsAsFactors = FALSE
      )
    }

    de <- as.numeric(details$delta_effective_occmax)
    dm <- as.numeric(details$delta_montane_occmax)
    substitution <- joint_substitution_test(
      de, dm,
      B = randomisations,
      seed_value = seed + 900000L + as.integer(radius * 100) +
        as.integer(round(threshold * 1000))
    )
    subset <- de <= 0
    sub_id <- sub_id + 1L
    substitution_rows[[sub_id]] <- data.frame(
      radius_km = radius,
      transition_threshold = threshold,
      n_nonoverlapping_pairs = nrow(details),
      n_effective_not_higher = sum(subset),
      proportion_effective_higher = mean(de > 0),
      proportion_either_effective_or_montane_higher = mean(de > 0 | dm > 0),
      mean_montane_difference_when_effective_not_higher =
        if (any(subset)) mean(dm[subset]) else NA_real_,
      median_montane_difference_when_effective_not_higher =
        if (any(subset)) stats::median(dm[subset]) else NA_real_,
      substitution_joint_signflip_p = unname(substitution[["p"]]),
      mean_pigmented_minus_white_elevation_when_effective_not_higher =
        if (any(subset)) mean(details$signed_elevation_difference[subset]) else NA_real_,
      median_pigmented_minus_white_elevation_when_effective_not_higher =
        if (any(subset)) stats::median(details$signed_elevation_difference[subset]) else NA_real_,
      spearman_montane_delta_vs_elevation_delta =
        if (sum(subset) >= 3L) suppressWarnings(stats::cor(
          dm[subset], details$signed_elevation_difference[subset],
          method = "spearman", use = "complete.obs"
        )) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}

summary <- do.call(rbind, summary_rows)
all_edges <- do.call(rbind, all_edge_rows)
pairs <- do.call(rbind, pair_rows)
substitution <- do.call(rbind, substitution_rows)
if (is.null(summary) || !nrow(summary)) {
  stop("No sharp-transition analysis results were produced.", call. = FALSE)
}

# Multiplicity columns are diagnostic. The focal strict-local row remains
# explicitly exploratory because the whole refinement was designed post-null.
summary$BH_q_within_exposure_threshold <- NA_real_
for (nm in unique(summary$exposure)) {
  for (threshold in unique(summary$transition_threshold)) {
    use <- summary$exposure == nm & summary$transition_threshold == threshold
    summary$BH_q_within_exposure_threshold[use] <- stats::p.adjust(
      summary$signflip_one_sided_p[use], method = "BH"
    )
  }
}
summary$BH_q_all_tests <- stats::p.adjust(
  summary$signflip_one_sided_p, method = "BH"
)
substitution$BH_q_across_scales_within_threshold <- NA_real_
for (threshold in unique(substitution$transition_threshold)) {
  use <- substitution$transition_threshold == threshold
  substitution$BH_q_across_scales_within_threshold[use] <- stats::p.adjust(
    substitution$substitution_joint_signflip_p[use], method = "BH"
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(summary, file.path(output_dir, "sharp_transition_summary.csv"), row.names = FALSE)
utils::write.csv(all_edges, file.path(output_dir, "sharp_transition_all_edges_descriptive.csv"), row.names = FALSE)
utils::write.csv(pairs, file.path(output_dir, "sharp_transition_nonoverlapping_pairs.csv"), row.names = FALSE)
utils::write.csv(substitution, file.path(output_dir, "montane_substitution_diagnostic.csv"), row.names = FALSE)

focal <- summary[summary$focal_strict_local_test, , drop = FALSE]
pure_effective <- summary[
  abs(summary$transition_threshold - 1) < 1e-12 & summary$exposure == "effective_occmax",
  , drop = FALSE
]
pure_montane <- substitution[
  abs(substitution$transition_threshold - 1) < 1e-12,
  , drop = FALSE
]
interpretation <- data.frame(
  metric = c(
    "focal_5km_pure_n_pairs",
    "focal_5km_pure_mean_effective_delta",
    "focal_5km_pure_median_effective_delta",
    "focal_5km_pure_prop_effective_positive",
    "focal_5km_pure_signflip_p",
    "focal_5km_selected_env_distance_median",
    "focal_5km_all_local_env_distance_median",
    "pure_effective_direction_positive_all_scales",
    "pure_effective_significant_all_scales",
    "montane_substitution_10km_p",
    "montane_substitution_25km_p"
  ),
  value = c(
    focal$n_nonoverlapping_pairs,
    focal$mean_signed_bombus_difference,
    focal$median_signed_bombus_difference,
    focal$proportion_positive,
    focal$signflip_one_sided_p,
    focal$median_environmental_distance_selected,
    focal$median_environmental_distance_all_local_edges,
    all(pure_effective$mean_signed_bombus_difference > 0),
    all(pure_effective$signflip_one_sided_p < 0.05),
    pure_montane$substitution_joint_signflip_p[pure_montane$radius_km == 10],
    pure_montane$substitution_joint_signflip_p[pure_montane$radius_km == 25]
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(interpretation, file.path(output_dir, "interpretation_summary.csv"), row.names = FALSE)

readme <- c(
  "# Local sharp-transition Bombus analysis",
  "",
  "This analysis intentionally does not use the environment+SPDE predictive null.",
  "It asks a narrower descriptive/directional question within geographically local",
  "observed white-pigmented transitions. Environment is reported only as a pair",
  "similarity diagnostic. SDM-derived Bombus support remains environmentally entangled.",
  "",
  "Inferential rows use non-overlapping, Bombus-blind, sign-blind transition pairs",
  "and a 100,000-replicate sign-flip randomisation of transition orientation.",
  "",
  "The montane substitution table is diagnostic and does not establish that the",
  "montane/alpine Bombus taxa are effective pollinators of Campanula punctata."
)
writeLines(readme, file.path(output_dir, "README.md"))

cat("Completed local sharp-transition Bombus analysis at ", output_dir, "\n", sep = "")
cat("\nFocal 5-km pure-transition result:\n")
print(focal)
cat("\nPotential montane substitution, pure transitions:\n")
print(pure_montane)
