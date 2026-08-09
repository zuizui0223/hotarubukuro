#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/local_pair_graph.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

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
  "--output", "results/ecological_v18_bombus_local_sharp_transition"
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
  stop("Focal-pollinator support cannot be aligned one-to-one with cells.", call. = FALSE)
}
support <- support[idx, , drop = FALSE]

lp_require_columns(
  cells,
  c(
    "exact_site_id", "x_km", "y_km", "longitude", "latitude",
    "bombus_fingerprint_common_support", "pigment_share", "n_observations",
    "elevation", "broad50km_pc1", "broad50km_pc2",
    "within50km_pc1", "within50km_pc2"
  ),
  "fresh flower cells"
)
lp_require_columns(
  support,
  c("exact_site_id", "effective_occmax", "effective_rawmax"),
  "focal-pollinator support"
)

exposure_names <- c("effective_occmax", "effective_rawmax")
exposure_role <- c(
  effective_occmax = "primary_occurrence_referenced_focal_pollinators",
  effective_rawmax = "raw_cloglog_focal_pollinator_sensitivity"
)
for (nm in exposure_names) {
  if (any(!is.finite(as.numeric(support[[nm]])))) {
    stop("Non-finite exposure values in ", nm, call. = FALSE)
  }
}

environment <- lp_environment_matrix(cells)
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
    absolute_elevation_difference = abs(elevation[pig] - elevation[white]),
    environmental_distance = lp_pair_distance(environment, white, pig),
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
row_id <- 0L
all_id <- 0L
pair_id <- 0L

for (radius in radii) {
  message("[sharp-transition] local graph radius = ", radius, " km")
  edges <- lp_pair_graph(
    cells,
    radius_km = radius,
    k = k,
    same_fold_only = FALSE,
    common_support_only = TRUE
  )
  if (!nrow(edges)) next
  i_all <- as.integer(edges$i)
  j_all <- as.integer(edges$j)
  edges$environmental_distance <- lp_pair_distance(environment, i_all, j_all)
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
  }
}

summary <- do.call(rbind, summary_rows)
all_edges <- do.call(rbind, all_edge_rows)
pairs <- do.call(rbind, pair_rows)
if (is.null(summary) || !nrow(summary)) {
  stop("No sharp-transition analysis results were produced.", call. = FALSE)
}

summary$BH_q_within_exposure_threshold <- NA_real_
for (nm in unique(summary$exposure)) {
  for (threshold in unique(summary$transition_threshold)) {
    use <- summary$exposure == nm & summary$transition_threshold == threshold
    summary$BH_q_within_exposure_threshold[use] <- stats::p.adjust(
      summary$signflip_one_sided_p[use], method = "BH"
    )
  }
}
summary$BH_q_all_tests <- stats::p.adjust(summary$signflip_one_sided_p, method = "BH")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(summary, file.path(output_dir, "sharp_transition_summary.csv"), row.names = FALSE)
utils::write.csv(all_edges, file.path(output_dir, "sharp_transition_all_edges_descriptive.csv"), row.names = FALSE)
utils::write.csv(pairs, file.path(output_dir, "sharp_transition_nonoverlapping_pairs.csv"), row.names = FALSE)

focal <- summary[summary$focal_strict_local_test, , drop = FALSE]
pure_effective <- summary[
  abs(summary$transition_threshold - 1) < 1e-12 & summary$exposure == "effective_occmax",
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
    "pure_effective_significant_all_scales"
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
    all(pure_effective$signflip_one_sided_p < 0.05)
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(interpretation, file.path(output_dir, "interpretation_summary.csv"), row.names = FALSE)

writeLines(c(
  "# Local sharp-transition focal-Bombus analysis",
  "",
  "Primary question: do abrupt nearby white-pigmented boundaries align directionally",
  "with occurrence-referenced availability of B. ardens/B. diversus?",
  "",
  "Pair construction is Bombus-blind and sign-blind. Environment is reported only",
  "as a local-similarity diagnostic. The analysis does not use an environment+SPDE",
  "predictive null because the Bombus SDMs are themselves environment-derived.",
  "",
  "Primary exposure: effective_occmax. Raw-cloglog effective_rawmax is the direct",
  "exposure-scale sensitivity. Montane/alpine community effects are tested separately",
  "as Supporting Information guardrails."
), file.path(output_dir, "README.md"))

cat("Completed current local sharp-transition analysis at ", output_dir, "\n", sep = "")
print(focal)
