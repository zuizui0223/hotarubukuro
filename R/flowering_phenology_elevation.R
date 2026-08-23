# Pre-result elevation guardrails for the flowering-date diagnostic.
# These functions do not alter the frozen H1/H2/H3 tests. They only ask whether
# the primary local date contrast survives tighter elevation matching in the
# mountain-route-enriched YAMAP sampling frame.

fp_build_cell_year_colour_elevation <- function(observations, cell_km = 1) {
  fp_require_columns(observations,
    c("exact_site_id", "x_km", "y_km", "year", "DOY", "pigmented_mixture50", "elevation"),
    "phenotype observations")
  keep <- is.finite(as.numeric(observations$x_km)) &
    is.finite(as.numeric(observations$y_km)) &
    is.finite(as.numeric(observations$year)) &
    is.finite(as.numeric(observations$DOY)) &
    is.finite(as.numeric(observations$elevation)) &
    as.numeric(observations$DOY) >= 1 & as.numeric(observations$DOY) <= 366 &
    as.numeric(observations$pigmented_mixture50) %in% c(0, 1) &
    !is.na(observations$exact_site_id) & nzchar(as.character(observations$exact_site_id))
  d <- observations[keep, , drop = FALSE]
  if (!nrow(d)) stop("No observations have finite elevation for the phenology guardrail", call. = FALSE)
  d$year <- as.integer(d$year)
  d$colour <- ifelse(as.numeric(d$pigmented_mixture50) == 1, "pigmented", "white")
  d$cell_id <- fp_cell_id(d$x_km, d$y_km, cell_km)

  site_key <- paste(d$cell_id, d$exact_site_id, d$year, d$colour, sep = "::")
  site_groups <- split(seq_len(nrow(d)), site_key)
  site_rows <- lapply(site_groups, function(index) {
    block <- d[index, , drop = FALSE]
    data.frame(
      cell_id = block$cell_id[[1L]],
      exact_site_id = as.character(block$exact_site_id[[1L]]),
      year = as.integer(block$year[[1L]]),
      colour = as.character(block$colour[[1L]]),
      elevation_m = fp_median_finite(block$elevation),
      stringsAsFactors = FALSE
    )
  })
  site_data <- do.call(rbind, site_rows)
  rownames(site_data) <- NULL

  cell_key <- paste(site_data$cell_id, site_data$year, site_data$colour, sep = "::")
  cell_groups <- split(seq_len(nrow(site_data)), cell_key)
  rows <- lapply(cell_groups, function(index) {
    block <- site_data[index, , drop = FALSE]
    data.frame(
      cell_id = block$cell_id[[1L]], year = as.integer(block$year[[1L]]),
      colour = as.character(block$colour[[1L]]),
      elevation_m = fp_median_finite(block$elevation_m), n_elevation_sites = nrow(block),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

fp_add_pair_elevation <- function(pair_years, cell_elevation) {
  if (!nrow(pair_years)) {
    pair_years$pigmented_elevation_m <- numeric()
    pair_years$white_elevation_m <- numeric()
    pair_years$elevation_diff_m <- numeric()
    pair_years$abs_elevation_diff_m <- numeric()
    return(pair_years)
  }
  fp_require_columns(cell_elevation, c("cell_id", "year", "colour", "elevation_m"),
    "cell-year-colour elevation")
  make_key <- function(cell, year, colour) paste(cell, year, colour, sep = "::")
  elevation_key <- make_key(cell_elevation$cell_id, cell_elevation$year, cell_elevation$colour)
  pig_index <- match(make_key(pair_years$pigmented_cell_id, pair_years$year, "pigmented"), elevation_key)
  white_index <- match(make_key(pair_years$white_cell_id, pair_years$year, "white"), elevation_key)
  out <- pair_years
  out$pigmented_elevation_m <- cell_elevation$elevation_m[pig_index]
  out$white_elevation_m <- cell_elevation$elevation_m[white_index]
  out$elevation_diff_m <- out$pigmented_elevation_m - out$white_elevation_m
  out$abs_elevation_diff_m <- abs(out$elevation_diff_m)
  out
}

fp_guardrail_subset_summary <- function(pair_years, scope, keep,
                                        permutations = 9999L, seed = 20260823L) {
  d <- pair_years[keep & is.finite(pair_years$delta_doy), , drop = FALSE]
  collapsed <- fp_collapse_geometric_pairs(d)
  x <- collapsed$delta_doy
  one <- fp_signflip_test(x, permutations, seed, "less")
  two <- fp_signflip_test(x, permutations, seed + 1L, "two.sided")
  data.frame(
    scope = scope,
    n_pair_years = nrow(d),
    n_unique_geometric_pairs = nrow(collapsed),
    mean_delta_doy_pigmented_minus_white = fp_mean_finite(x),
    median_delta_doy_pigmented_minus_white = fp_median_finite(x),
    proportion_pigmented_earlier = if (length(x)) mean(x < 0, na.rm = TRUE) else NA_real_,
    one_sided_signflip_p_pigmented_earlier = one$p,
    two_sided_signflip_p = two$p,
    stringsAsFactors = FALSE
  )
}

fp_elevation_guardrails <- function(primary_pair_years, permutations = 9999L,
                                    seed = 20260823L) {
  fp_require_columns(primary_pair_years,
    c("pigmented_cell_id", "white_cell_id", "year", "delta_doy", "distance_km",
      "elevation_diff_m", "abs_elevation_diff_m"), "elevation-enriched primary pairs")
  finite_elevation <- is.finite(primary_pair_years$abs_elevation_diff_m)
  summaries <- rbind(
    fp_guardrail_subset_summary(primary_pair_years, "same_1km_cell",
      primary_pair_years$distance_km == 0, permutations, seed + 10L),
    fp_guardrail_subset_summary(primary_pair_years, "abs_elevation_diff_le_100m",
      finite_elevation & primary_pair_years$abs_elevation_diff_m <= 100,
      permutations, seed + 20L),
    fp_guardrail_subset_summary(primary_pair_years, "abs_elevation_diff_le_250m",
      finite_elevation & primary_pair_years$abs_elevation_diff_m <= 250,
      permutations, seed + 30L)
  )

  d <- primary_pair_years[finite_elevation & is.finite(primary_pair_years$delta_doy), , drop = FALSE]
  if (nrow(d)) {
    key <- paste(d$pigmented_cell_id, d$white_cell_id, sep = "::")
    groups <- split(seq_len(nrow(d)), key)
    collapsed <- do.call(rbind, lapply(groups, function(index) {
      block <- d[index, , drop = FALSE]
      data.frame(
        geometric_pair_id = key[[index[[1L]]]],
        delta_doy = fp_median_finite(block$delta_doy),
        elevation_diff_m = fp_median_finite(block$elevation_diff_m),
        stringsAsFactors = FALSE
      )
    }))
    correlation <- fp_safe_spearman(collapsed$delta_doy, collapsed$elevation_diff_m)
    correlation_row <- data.frame(
      comparison = "delta_DOY_vs_signed_elevation_difference",
      n_unique_geometric_pairs = unname(correlation[["n"]]),
      spearman_rho = unname(correlation[["rho"]]),
      asymptotic_p = unname(correlation[["p"]]),
      direction_note = "positive rho means higher pigmented cells tend to be observed later relative to white matches",
      stringsAsFactors = FALSE
    )
  } else {
    correlation_row <- data.frame(
      comparison = "delta_DOY_vs_signed_elevation_difference",
      n_unique_geometric_pairs = 0L, spearman_rho = NA_real_, asymptotic_p = NA_real_,
      direction_note = "positive rho means higher pigmented cells tend to be observed later relative to white matches",
      stringsAsFactors = FALSE
    )
  }
  list(summary = summaries, correlation = correlation_row)
}
