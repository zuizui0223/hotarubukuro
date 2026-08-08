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
  "--output", "results/ecological_v19_bombus_spatial_replication_test"
)
radii <- as.numeric(strsplit(arg_value("--radii", "5,10,25"), ",")[[1]])
background_windows <- as.numeric(strsplit(arg_value("--background-windows", "25,50,100"), ",")[[1]])
control_counts <- as.integer(strsplit(arg_value("--control-counts", "10,20,50"), ",")[[1]])
k <- as.integer(arg_value("--k", "5"))
randomisations <- as.integer(arg_value("--randomisations", "100000"))
seed <- as.integer(arg_value("--seed", "20260809"))

if (!file.exists(cells_path) || !file.exists(support_path)) {
  stop("Missing spatial-replication input file.", call. = FALSE)
}
if (!length(radii) || any(!is.finite(radii)) || any(radii <= 0)) {
  stop("Radii must be positive.", call. = FALSE)
}
if (!length(background_windows) || any(!is.finite(background_windows)) ||
    any(background_windows <= 0)) {
  stop("Background windows must be positive.", call. = FALSE)
}
if (!length(control_counts) || any(!is.finite(control_counts)) ||
    any(control_counts < 10L)) {
  stop("Control counts must be integers >= 10.", call. = FALSE)
}
if (!is.finite(randomisations) || randomisations < 1000L) {
  stop("Use at least 1000 matched-background randomisations.", call. = FALSE)
}

cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
support <- utils::read.csv(support_path, check.names = FALSE, stringsAsFactors = FALSE)
idx <- match(cells$exact_site_id, support$exact_site_id)
if (anyNA(idx) || anyDuplicated(idx)) {
  stop("Bombus support cannot be aligned one-to-one with flower cells.", call. = FALSE)
}
support <- support[idx, , drop = FALSE]

species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
occ_cols <- paste0(species, "_occurrence_reference")
rank_cols <- paste0(species, "_within_species_rank")

v17_require_columns(
  cells,
  c(
    "exact_site_id", "x_km", "y_km", "longitude", "latitude", "elevation",
    "pigment_share", "bombus_fingerprint_common_support", rank_cols
  ),
  "fresh flower cells"
)
v17_require_columns(
  support,
  c("exact_site_id", occ_cols, "effective_occmax"),
  "occurrence-referenced Bombus support"
)

colour <- as.numeric(cells$pigment_share)
elevation <- as.numeric(cells$elevation)
x_km <- as.numeric(cells$x_km)
y_km <- as.numeric(cells$y_km)

occ_support <- as.matrix(support[, occ_cols, drop = FALSE])
storage.mode(occ_support) <- "double"
rank_support <- as.matrix(cells[, rank_cols, drop = FALSE])
storage.mode(rank_support) <- "double"

hellinger_coordinates <- function(x) {
  total <- rowSums(x)
  relative <- x / pmax(total, 1e-12)
  relative[!is.finite(relative)] <- NA_real_
  sqrt(relative)
}
occ_hell <- hellinger_coordinates(occ_support)
rank_hell <- hellinger_coordinates(rank_support)

support$montane_occmax <- apply(
  occ_support[, c(
    "beaticola_occurrence_reference",
    "consobrinus_occurrence_reference",
    "honshuensis_occurrence_reference"
  ), drop = FALSE],
  1, max
)

edge_hellinger <- function(coordinates, i, j) {
  delta <- coordinates[i, , drop = FALSE] - coordinates[j, , drop = FALSE]
  sqrt(rowSums(delta^2)) / sqrt(2)
}

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

build_edge_table <- function(radius) {
  edges <- v17_pair_graph(
    cells,
    radius_km = radius,
    k = k,
    same_fold_only = FALSE,
    common_support_only = TRUE
  )
  if (!nrow(edges)) return(edges)
  i <- as.integer(edges$i)
  j <- as.integer(edges$j)
  edges$colour_difference <- colour[j] - colour[i]
  edges$abs_colour_difference <- abs(edges$colour_difference)
  edges$midpoint_x_km <- (x_km[i] + x_km[j]) / 2
  edges$midpoint_y_km <- (y_km[i] + y_km[j]) / 2
  edges$midpoint_elevation <- (elevation[i] + elevation[j]) / 2
  edges$absolute_elevation_difference <- abs(elevation[i] - elevation[j])
  edges$occurrence_reference_hellinger <- edge_hellinger(occ_hell, i, j)
  edges$within_species_rank_hellinger <- edge_hellinger(rank_hell, i, j)
  edges
}

select_pure_nonoverlap <- function(edges) {
  pure <- is.finite(edges$abs_colour_difference) &
    abs(edges$abs_colour_difference - 1) < 1e-12
  d <- edges[pure, , drop = FALSE]
  if (!nrow(d)) return(d)
  greedy_nonoverlap(d)
}

match_controls <- function(edges, transitions, background_window_km, n_controls) {
  control_pool <- edges[
    is.finite(edges$abs_colour_difference) &
      edges$abs_colour_difference < 1 - 1e-12,
    , drop = FALSE
  ]
  matched <- vector("list", nrow(transitions))
  retained <- logical(nrow(transitions))
  for (r in seq_len(nrow(transitions))) {
    focal <- transitions[r, , drop = FALSE]
    midpoint_distance <- sqrt(
      (control_pool$midpoint_x_km - focal$midpoint_x_km)^2 +
        (control_pool$midpoint_y_km - focal$midpoint_y_km)^2
    )
    candidate <- control_pool[
      is.finite(midpoint_distance) & midpoint_distance <= background_window_km &
        control_pool$i != focal$i & control_pool$j != focal$i &
        control_pool$i != focal$j & control_pool$j != focal$j,
      , drop = FALSE
    ]
    if (nrow(candidate) < 10L) next

    match_vars <- c(
      "geographic_distance_km",
      "midpoint_elevation",
      "absolute_elevation_difference"
    )
    candidate_matrix <- as.matrix(candidate[, match_vars, drop = FALSE])
    storage.mode(candidate_matrix) <- "double"
    target <- as.numeric(focal[1, match_vars])
    spread <- apply(candidate_matrix, 2, stats::sd, na.rm = TRUE)
    spread[!is.finite(spread) | spread <= 1e-12] <- 1
    difference <- sweep(candidate_matrix, 2, target, "-")
    difference <- sweep(difference, 2, spread, "/")
    candidate$matching_distance <- sqrt(rowSums(difference^2))
    candidate <- candidate[order(candidate$matching_distance, candidate$edge_id), , drop = FALSE]
    candidate <- head(candidate, n_controls)
    candidate$transition_edge_id <- focal$edge_id
    candidate$transition_row <- r
    candidate$background_window_km <- background_window_km
    candidate$requested_n_controls <- n_controls
    candidate$actual_n_controls <- nrow(candidate)
    matched[[r]] <- candidate
    retained[[r]] <- TRUE
  }
  list(
    transitions = transitions[retained, , drop = FALSE],
    controls = matched[retained]
  )
}

matched_background_test <- function(
    transitions, controls, metric, B, seed_value,
    radius, background_window_km, n_controls) {
  if (!nrow(transitions) || !length(controls)) return(NULL)
  case_value <- as.numeric(transitions[[metric]])
  control_value <- lapply(controls, function(x) as.numeric(x[[metric]]))
  valid <- is.finite(case_value) & vapply(
    control_value,
    function(x) length(x) >= 10L && all(is.finite(x)),
    logical(1)
  )
  transitions <- transitions[valid, , drop = FALSE]
  case_value <- case_value[valid]
  control_value <- control_value[valid]
  if (length(case_value) < 10L) return(NULL)

  control_median <- vapply(control_value, stats::median, numeric(1))
  excess <- case_value - control_median
  observed_mean <- mean(case_value)

  set.seed(seed_value)
  ge <- 0L
  total <- 0L
  null_sum <- 0
  null_sum_sq <- 0
  null_values <- numeric(B)
  chunk <- 5000L
  position <- 1L
  while (total < B) {
    m <- min(chunk, B - total)
    selected <- vapply(
      control_value,
      function(x) sample(x, m, replace = TRUE),
      numeric(m)
    )
    if (!is.matrix(selected)) selected <- matrix(selected, ncol = length(control_value))
    simulated <- rowMeans(selected)
    ge <- ge + sum(simulated >= observed_mean)
    null_sum <- null_sum + sum(simulated)
    null_sum_sq <- null_sum_sq + sum(simulated^2)
    null_values[position:(position + m - 1L)] <- simulated
    position <- position + m
    total <- total + m
  }
  null_mean <- null_sum / total
  null_var <- max(0, (null_sum_sq - total * null_mean^2) / max(1, total - 1L))

  detail <- data.frame(
    radius_km = radius,
    background_window_km = background_window_km,
    requested_n_controls = n_controls,
    metric = metric,
    transition_edge_id = transitions$edge_id,
    transition_midpoint_x_km = transitions$midpoint_x_km,
    transition_midpoint_y_km = transitions$midpoint_y_km,
    transition_midpoint_elevation = transitions$midpoint_elevation,
    transition_absolute_elevation_difference = transitions$absolute_elevation_difference,
    transition_geographic_distance_km = transitions$geographic_distance_km,
    transition_community_turnover = case_value,
    matched_control_median_turnover = control_median,
    matched_excess = excess,
    stringsAsFactors = FALSE
  )

  summary <- data.frame(
    radius_km = radius,
    background_window_km = background_window_km,
    requested_n_controls = n_controls,
    metric = metric,
    focal_spatial_match_test = radius == 5 &&
      background_window_km == 50 && n_controls == 20 &&
      metric == "occurrence_reference_hellinger",
    n_transition_edges = length(case_value),
    observed_mean_transition_turnover = observed_mean,
    observed_median_transition_turnover = stats::median(case_value),
    matched_null_mean_turnover = null_mean,
    matched_null_sd = sqrt(null_var),
    matched_null_lower_95 = unname(stats::quantile(null_values, 0.025)),
    matched_null_upper_95 = unname(stats::quantile(null_values, 0.975)),
    matched_background_one_sided_p = (ge + 1) / (total + 1),
    mean_matched_excess = mean(excess),
    median_matched_excess = stats::median(excess),
    proportion_matched_excess_positive = mean(excess > 0),
    stringsAsFactors = FALSE
  )
  list(summary = summary, detail = detail)
}

block_diagnostics <- function(detail, block_size_km = 100, shift_km = 0) {
  if (is.null(detail) || !nrow(detail)) return(list(blocks = data.frame(), loo = data.frame(), summary = data.frame()))
  block_x <- floor((detail$transition_midpoint_x_km - shift_km) / block_size_km)
  block_y <- floor((detail$transition_midpoint_y_km - shift_km) / block_size_km)
  block_id <- paste(block_x, block_y, sep = "::")
  ids <- unique(block_id)
  block_rows <- list()
  b <- 0L
  for (id in ids) {
    use <- block_id == id
    if (sum(use) < 3L) next
    b <- b + 1L
    block_rows[[b]] <- data.frame(
      radius_km = unique(detail$radius_km),
      block_size_km = block_size_km,
      grid_shift_km = shift_km,
      block_id = id,
      n_transitions = sum(use),
      mean_matched_excess = mean(detail$matched_excess[use]),
      median_matched_excess = stats::median(detail$matched_excess[use]),
      proportion_positive = mean(detail$matched_excess[use] > 0),
      stringsAsFactors = FALSE
    )
  }
  blocks <- if (length(block_rows)) do.call(rbind, block_rows) else data.frame()
  if (!nrow(blocks)) return(list(blocks = blocks, loo = data.frame(), summary = data.frame()))

  loo_rows <- lapply(seq_len(nrow(blocks)), function(i) {
    exclude <- block_id == blocks$block_id[[i]]
    data.frame(
      radius_km = unique(detail$radius_km),
      block_size_km = block_size_km,
      grid_shift_km = shift_km,
      excluded_block_id = blocks$block_id[[i]],
      excluded_n_transitions = sum(exclude),
      leave_one_block_out_mean_excess = mean(detail$matched_excess[!exclude]),
      stringsAsFactors = FALSE
    )
  })
  loo <- do.call(rbind, loo_rows)
  summary <- data.frame(
    radius_km = unique(detail$radius_km),
    block_size_km = block_size_km,
    grid_shift_km = shift_km,
    n_blocks_with_at_least_3 = nrow(blocks),
    n_positive_mean_blocks = sum(blocks$mean_matched_excess > 0),
    fraction_positive_mean_blocks = mean(blocks$mean_matched_excess > 0),
    minimum_leave_one_block_out_mean_excess = min(loo$leave_one_block_out_mean_excess),
    all_leave_one_block_out_positive = all(loo$leave_one_block_out_mean_excess > 0),
    stringsAsFactors = FALSE
  )
  list(blocks = blocks, loo = loo, summary = summary)
}

orient_pure_transitions <- function(transitions) {
  if (!nrow(transitions)) return(data.frame())
  i <- as.integer(transitions$i)
  j <- as.integer(transitions$j)
  d_colour <- colour[j] - colour[i]
  pig <- ifelse(d_colour > 0, j, i)
  white <- ifelse(d_colour > 0, i, j)
  montane <- as.numeric(support$montane_occmax)
  effective <- as.numeric(support$effective_occmax)
  data.frame(
    edge_id = transitions$edge_id,
    white_site = as.character(cells$exact_site_id[white]),
    pigmented_site = as.character(cells$exact_site_id[pig]),
    geographic_distance_km = transitions$geographic_distance_km,
    absolute_elevation_difference = abs(elevation[pig] - elevation[white]),
    signed_elevation_difference = elevation[pig] - elevation[white],
    delta_montane_occmax = montane[pig] - montane[white],
    delta_effective_occmax = effective[pig] - effective[white],
    stringsAsFactors = FALSE
  )
}

community_rows <- list()
community_detail_rows <- list()
block_rows <- list()
block_loo_rows <- list()
block_summary_rows <- list()
montane_rows <- list()
community_id <- 0L
detail_id <- 0L
block_id <- 0L
loo_id <- 0L
block_summary_id <- 0L
montane_id <- 0L

primary_detail_by_radius <- list()

for (radius in radii) {
  message("[spatial-replication] radius = ", radius, " km")
  edges <- build_edge_table(radius)
  if (!nrow(edges)) next
  transitions <- select_pure_nonoverlap(edges)
  if (!nrow(transitions)) next

  for (window in background_windows) {
    for (n_control in control_counts) {
      match <- match_controls(edges, transitions, window, n_control)
      for (metric in c("occurrence_reference_hellinger", "within_species_rank_hellinger")) {
        result <- matched_background_test(
          match$transitions,
          match$controls,
          metric = metric,
          B = randomisations,
          seed_value = seed + as.integer(radius * 100000) +
            as.integer(window * 100) + n_control * 10L + match(metric, c(
              "occurrence_reference_hellinger", "within_species_rank_hellinger"
            )),
          radius = radius,
          background_window_km = window,
          n_controls = n_control
        )
        if (is.null(result)) next
        community_id <- community_id + 1L
        community_rows[[community_id]] <- result$summary
        detail_id <- detail_id + 1L
        community_detail_rows[[detail_id]] <- result$detail
        if (window == 50 && n_control == 20 &&
            metric == "occurrence_reference_hellinger") {
          primary_detail_by_radius[[as.character(radius)]] <- result$detail
        }
      }
    }
  }

  # Near-equal-elevation montane/alpine guardrail.
  oriented <- orient_pure_transitions(transitions)
  for (elev_limit in c(50, 100)) {
    equal_elev <- is.finite(oriented$absolute_elevation_difference) &
      oriented$absolute_elevation_difference <= elev_limit
    for (subset_name in c("all_pure_transitions", "effective_not_higher")) {
      use <- equal_elev
      if (subset_name == "effective_not_higher") {
        use <- use & is.finite(oriented$delta_effective_occmax) &
          oriented$delta_effective_occmax <= 0
      }
      delta <- oriented$delta_montane_occmax[use]
      test <- signflip_mean_test(
        delta,
        B = randomisations,
        seed_value = seed + 7000000L + as.integer(radius * 1000) +
          elev_limit * 10L + match(subset_name, c(
            "all_pure_transitions", "effective_not_higher"
          ))
      )
      montane_id <- montane_id + 1L
      montane_rows[[montane_id]] <- data.frame(
        radius_km = radius,
        elevation_difference_limit_m = elev_limit,
        subset = subset_name,
        n_pairs = length(delta),
        mean_pigmented_minus_white_montane_support = unname(test[["observed"]]),
        median_pigmented_minus_white_montane_support =
          if (length(delta)) stats::median(delta, na.rm = TRUE) else NA_real_,
        proportion_positive = if (length(delta)) mean(delta > 0, na.rm = TRUE) else NA_real_,
        signflip_one_sided_p = unname(test[["p"]]),
        stringsAsFactors = FALSE
      )
    }
  }
}

community <- if (length(community_rows)) do.call(rbind, community_rows) else data.frame()
community_detail <- if (length(community_detail_rows)) do.call(rbind, community_detail_rows) else data.frame()
montane <- if (length(montane_rows)) do.call(rbind, montane_rows) else data.frame()
if (!nrow(community)) stop("No community spatial-match results were produced.", call. = FALSE)

community$BH_q_across_scales_within_match_spec <- NA_real_
for (metric in unique(community$metric)) {
  for (window in unique(community$background_window_km)) {
    for (n_control in unique(community$requested_n_controls)) {
      use <- community$metric == metric &
        community$background_window_km == window &
        community$requested_n_controls == n_control
      community$BH_q_across_scales_within_match_spec[use] <- stats::p.adjust(
        community$matched_background_one_sided_p[use], method = "BH"
      )
    }
  }
}

# Fixed 100-km block diagnostics for the primary match specification.
for (radius in radii) {
  detail <- primary_detail_by_radius[[as.character(radius)]]
  if (is.null(detail) || !nrow(detail)) next
  for (shift in c(0, 50)) {
    diagnostic <- block_diagnostics(detail, block_size_km = 100, shift_km = shift)
    if (nrow(diagnostic$blocks)) {
      block_id <- block_id + 1L
      block_rows[[block_id]] <- diagnostic$blocks
    }
    if (nrow(diagnostic$loo)) {
      loo_id <- loo_id + 1L
      block_loo_rows[[loo_id]] <- diagnostic$loo
    }
    if (nrow(diagnostic$summary)) {
      block_summary_id <- block_summary_id + 1L
      block_summary_rows[[block_summary_id]] <- diagnostic$summary
    }
  }
}

blocks <- if (length(block_rows)) do.call(rbind, block_rows) else data.frame()
block_loo <- if (length(block_loo_rows)) do.call(rbind, block_loo_rows) else data.frame()
block_summary <- if (length(block_summary_rows)) do.call(rbind, block_summary_rows) else data.frame()

montane$BH_q_across_scales_within_elevation_subset <- NA_real_
if (nrow(montane)) {
  for (limit in unique(montane$elevation_difference_limit_m)) {
    for (subset_name in unique(montane$subset)) {
      use <- montane$elevation_difference_limit_m == limit & montane$subset == subset_name
      montane$BH_q_across_scales_within_elevation_subset[use] <- stats::p.adjust(
        montane$signflip_one_sided_p[use], method = "BH"
      )
    }
  }
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(community, file.path(output_dir, "community_spatial_match_summary.csv"), row.names = FALSE)
utils::write.csv(community_detail, file.path(output_dir, "community_spatial_match_transition_detail.csv"), row.names = FALSE)
utils::write.csv(blocks, file.path(output_dir, "community_spatial_block_detail.csv"), row.names = FALSE)
utils::write.csv(block_loo, file.path(output_dir, "community_spatial_block_leave_one_out.csv"), row.names = FALSE)
utils::write.csv(block_summary, file.path(output_dir, "community_spatial_block_summary.csv"), row.names = FALSE)
utils::write.csv(montane, file.path(output_dir, "montane_equal_elevation_guardrail.csv"), row.names = FALSE)

primary <- community[community$focal_spatial_match_test, , drop = FALSE]
primary_scales <- community[
  community$metric == "occurrence_reference_hellinger" &
    community$background_window_km == 50 &
    community$requested_n_controls == 20,
  , drop = FALSE
]
rank_scales <- community[
  community$metric == "within_species_rank_hellinger" &
    community$background_window_km == 50 &
    community$requested_n_controls == 20,
  , drop = FALSE
]
block5 <- block_summary[block_summary$radius_km == 5, , drop = FALSE]
montane50 <- montane[
  montane$elevation_difference_limit_m == 50 &
    montane$subset == "all_pure_transitions",
  , drop = FALSE
]

interpretation <- data.frame(
  metric = c(
    "primary_5km_n_transitions",
    "primary_5km_mean_matched_excess",
    "primary_5km_median_matched_excess",
    "primary_5km_prop_excess_positive",
    "primary_5km_matched_p",
    "primary_5km_matched_q_across_scales",
    "occurrence_reference_direction_positive_all_scales",
    "occurrence_reference_p_lt_0.05_all_scales",
    "rank_metric_direction_positive_all_scales",
    "unshifted_5km_fraction_positive_blocks",
    "shifted_5km_fraction_positive_blocks",
    "unshifted_5km_all_loo_positive",
    "shifted_5km_all_loo_positive",
    "montane_equal50_any_scale_p_lt_0.05",
    "montane_equal50_any_scale_q_lt_0.05"
  ),
  value = c(
    if (nrow(primary)) primary$n_transition_edges else NA,
    if (nrow(primary)) primary$mean_matched_excess else NA,
    if (nrow(primary)) primary$median_matched_excess else NA,
    if (nrow(primary)) primary$proportion_matched_excess_positive else NA,
    if (nrow(primary)) primary$matched_background_one_sided_p else NA,
    if (nrow(primary)) primary$BH_q_across_scales_within_match_spec else NA,
    all(primary_scales$mean_matched_excess > 0),
    all(primary_scales$matched_background_one_sided_p < 0.05),
    all(rank_scales$mean_matched_excess > 0),
    if (nrow(block5)) block5$fraction_positive_mean_blocks[block5$grid_shift_km == 0] else NA,
    if (nrow(block5)) block5$fraction_positive_mean_blocks[block5$grid_shift_km == 50] else NA,
    if (nrow(block5)) block5$all_leave_one_block_out_positive[block5$grid_shift_km == 0] else NA,
    if (nrow(block5)) block5$all_leave_one_block_out_positive[block5$grid_shift_km == 50] else NA,
    any(montane50$signflip_one_sided_p < 0.05, na.rm = TRUE),
    any(montane50$BH_q_across_scales_within_elevation_subset < 0.05, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(interpretation, file.path(output_dir, "interpretation_summary.csv"), row.names = FALSE)

readme <- c(
  "# Spatially matched Bombus-boundary replication test",
  "",
  "This post-hoc diagnostic asks whether sharp white-pigmented flower-colour",
  "boundaries coincide with stronger predicted Bombus-community turnover than",
  "nearby non-pure-transition edges matched for edge length, midpoint elevation",
  "and elevation contrast. It does not use an environment+SPDE predictive null.",
  "",
  "A positive matched excess argues against broad spatial/elevational overlap as",
  "a sufficient explanation for the exact local boundary coincidence, but it does",
  "not identify a causal pollinator effect because the Bombus surfaces are SDMs.",
  "",
  "Montane/alpine Bombus results are guarded by near-equal-elevation restrictions",
  "and are supplementary unless an effect survives those restrictions."
)
writeLines(readme, file.path(output_dir, "README.md"))

cat("Completed spatially matched Bombus-boundary test at ", output_dir, "\n", sep = "")
cat("\nPrimary spatial-match row:\n")
print(primary)
cat("\nPrimary match settings across scales:\n")
print(primary_scales)
cat("\nMontane equal-elevation guardrail (<=50 m):\n")
print(montane50)
