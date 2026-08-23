# Flowering-date diagnostics for the exploratory anthropogenic provenance test.
#
# Photo date is treated as an observation-date phenology proxy, not as flowering
# onset. The primary contrast uses same-year mutual-nearest white/pigmented
# 1-km cell pairs within 5 km. Human/isolation features are joined only after
# pairs have been defined from phenotype, space and year.

fp_require_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop(label, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

fp_median_finite <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) stats::median(x) else NA_real_
}

fp_mean_finite <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

fp_cell_id <- function(x_km, y_km, cell_km = 1) {
  if (!is.finite(cell_km) || cell_km <= 0) stop("cell_km must be positive", call. = FALSE)
  paste0("cell-", format(cell_km, trim = TRUE), "km-",
         floor(as.numeric(x_km) / cell_km), "_", floor(as.numeric(y_km) / cell_km))
}

fp_empty_pairs <- function() {
  data.frame(
    pair_id = character(), year = integer(), pigmented_cell_id = character(),
    white_cell_id = character(), pigmented_doy = numeric(), white_doy = numeric(),
    delta_doy = numeric(), early_days = numeric(), distance_km = numeric(),
    pigmented_n_sites = integer(), white_n_sites = integer(), stringsAsFactors = FALSE
  )
}

fp_build_cell_year_colour <- function(observations, cell_km = 1) {
  required <- c("exact_site_id", "x_km", "y_km", "year", "DOY", "pigmented_mixture50")
  fp_require_columns(observations, required, "phenotype observations")
  keep <- is.finite(as.numeric(observations$x_km)) &
    is.finite(as.numeric(observations$y_km)) &
    is.finite(as.numeric(observations$year)) &
    is.finite(as.numeric(observations$DOY)) &
    as.numeric(observations$DOY) >= 1 & as.numeric(observations$DOY) <= 366 &
    as.numeric(observations$pigmented_mixture50) %in% c(0, 1) &
    !is.na(observations$exact_site_id) & nzchar(as.character(observations$exact_site_id))
  data <- observations[keep, , drop = FALSE]
  if (!nrow(data)) stop("No observations have valid phenotype/date/coordinates", call. = FALSE)
  data$year <- as.integer(data$year)
  data$DOY <- as.numeric(data$DOY)
  data$colour <- ifelse(as.numeric(data$pigmented_mixture50) == 1, "pigmented", "white")
  data$cell_id <- fp_cell_id(data$x_km, data$y_km, cell_km)

  site_key <- paste(data$cell_id, data$exact_site_id, data$year, data$colour, sep = "::")
  site_groups <- split(seq_len(nrow(data)), site_key)
  site_rows <- lapply(site_groups, function(index) {
    block <- data[index, , drop = FALSE]
    data.frame(
      cell_id = block$cell_id[[1L]], exact_site_id = as.character(block$exact_site_id[[1L]]),
      year = as.integer(block$year[[1L]]), colour = as.character(block$colour[[1L]]),
      site_doy = stats::median(block$DOY), x_km = fp_mean_finite(block$x_km),
      y_km = fp_mean_finite(block$y_km), n_images = nrow(block), stringsAsFactors = FALSE
    )
  })
  site_data <- do.call(rbind, site_rows)
  rownames(site_data) <- NULL

  cell_key <- paste(site_data$cell_id, site_data$year, site_data$colour, sep = "::")
  cell_groups <- split(seq_len(nrow(site_data)), cell_key)
  cell_rows <- lapply(cell_groups, function(index) {
    block <- site_data[index, , drop = FALSE]
    raw_x <- fp_mean_finite(block$x_km)
    raw_y <- fp_mean_finite(block$y_km)
    data.frame(
      cell_id = block$cell_id[[1L]], year = as.integer(block$year[[1L]]),
      colour = as.character(block$colour[[1L]]), median_doy = stats::median(block$site_doy),
      x_km = (floor(raw_x / cell_km) + 0.5) * cell_km,
      y_km = (floor(raw_y / cell_km) + 0.5) * cell_km,
      n_sites = nrow(block), n_images = sum(block$n_images), stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, cell_rows)
  rownames(out) <- NULL
  out <- out[order(out$year, out$cell_id, out$colour), , drop = FALSE]
  rownames(out) <- NULL
  out
}

fp_mutual_nearest_pairs <- function(cell_year_colour, max_distance_km = 5) {
  fp_require_columns(cell_year_colour,
    c("cell_id", "year", "colour", "median_doy", "x_km", "y_km", "n_sites"),
    "cell-year-colour table")
  if (!is.finite(max_distance_km) || max_distance_km < 0) stop("max_distance_km must be non-negative", call. = FALSE)
  rows <- list()
  for (year in sort(unique(as.integer(cell_year_colour$year)))) {
    block <- cell_year_colour[cell_year_colour$year == year, , drop = FALSE]
    pig <- block[block$colour == "pigmented", , drop = FALSE]
    white <- block[block$colour == "white", , drop = FALSE]
    if (!nrow(pig) || !nrow(white)) next
    dx <- outer(as.numeric(pig$x_km), as.numeric(white$x_km), "-")
    dy <- outer(as.numeric(pig$y_km), as.numeric(white$y_km), "-")
    distance <- sqrt(dx^2 + dy^2)
    nearest_white <- apply(distance, 1L, which.min)
    nearest_pigmented <- apply(distance, 2L, which.min)
    for (i in seq_len(nrow(pig))) {
      j <- nearest_white[[i]]
      d <- distance[i, j]
      if (nearest_pigmented[[j]] != i || !is.finite(d) || d > max_distance_km) next
      delta <- as.numeric(pig$median_doy[[i]]) - as.numeric(white$median_doy[[j]])
      rows[[length(rows) + 1L]] <- data.frame(
        pair_id = paste0(year, "::", pig$cell_id[[i]], "::", white$cell_id[[j]]),
        year = as.integer(year), pigmented_cell_id = as.character(pig$cell_id[[i]]),
        white_cell_id = as.character(white$cell_id[[j]]),
        pigmented_doy = as.numeric(pig$median_doy[[i]]), white_doy = as.numeric(white$median_doy[[j]]),
        delta_doy = delta, early_days = -delta, distance_km = d,
        pigmented_n_sites = as.integer(pig$n_sites[[i]]), white_n_sites = as.integer(white$n_sites[[j]]),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(fp_empty_pairs())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

fp_collapse_geometric_pairs <- function(pair_years) {
  fp_require_columns(pair_years,
    c("pigmented_cell_id", "white_cell_id", "delta_doy", "early_days", "distance_km", "year"),
    "pair-year table")
  if (!nrow(pair_years)) return(data.frame(
    geometric_pair_id = character(), pigmented_cell_id = character(), white_cell_id = character(),
    delta_doy = numeric(), early_days = numeric(), distance_km = numeric(), n_years = integer(),
    years = character(), stringsAsFactors = FALSE))
  key <- paste(pair_years$pigmented_cell_id, pair_years$white_cell_id, sep = "::")
  groups <- split(seq_len(nrow(pair_years)), key)
  rows <- lapply(groups, function(index) {
    block <- pair_years[index, , drop = FALSE]
    data.frame(
      geometric_pair_id = paste(block$pigmented_cell_id[[1L]], block$white_cell_id[[1L]], sep = "::"),
      pigmented_cell_id = block$pigmented_cell_id[[1L]], white_cell_id = block$white_cell_id[[1L]],
      delta_doy = fp_median_finite(block$delta_doy), early_days = fp_median_finite(block$early_days),
      distance_km = fp_median_finite(block$distance_km), n_years = length(unique(block$year)),
      years = paste(sort(unique(block$year)), collapse = ","), stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

fp_signflip_test <- function(delta, permutations = 9999L, seed = 20260823L,
                             alternative = c("less", "greater", "two.sided")) {
  alternative <- match.arg(alternative)
  x <- as.numeric(delta)
  x <- x[is.finite(x)]
  if (!length(x)) return(list(statistic = NA_real_, p = NA_real_, n = 0L))
  if (!is.finite(permutations) || permutations < 99L) stop("permutations must be >= 99", call. = FALSE)
  observed <- mean(x)
  set.seed(seed)
  null <- replicate(permutations, mean(x * sample(c(-1, 1), length(x), replace = TRUE)))
  p <- switch(alternative,
    less = (1 + sum(null <= observed)) / (permutations + 1),
    greater = (1 + sum(null >= observed)) / (permutations + 1),
    two.sided = (1 + sum(abs(null) >= abs(observed))) / (permutations + 1))
  list(statistic = observed, p = p, n = length(x))
}

fp_pair_summary <- function(pair_years, max_distance_km, permutations = 9999L, seed = 20260823L) {
  collapsed <- fp_collapse_geometric_pairs(pair_years)
  x <- collapsed$delta_doy
  one_sided <- fp_signflip_test(x, permutations, seed, "less")
  two_sided <- fp_signflip_test(x, permutations, seed + 1L, "two.sided")
  data.frame(
    max_distance_km = max_distance_km, n_pair_years = nrow(pair_years),
    n_unique_geometric_pairs = nrow(collapsed),
    n_same_cell_pair_years = sum(pair_years$distance_km == 0, na.rm = TRUE),
    mean_delta_doy_pigmented_minus_white = fp_mean_finite(x),
    median_delta_doy_pigmented_minus_white = fp_median_finite(x),
    proportion_pigmented_earlier = if (length(x)) mean(x < 0, na.rm = TRUE) else NA_real_,
    one_sided_signflip_p_pigmented_earlier = one_sided$p,
    two_sided_signflip_p = two_sided$p, stringsAsFactors = FALSE
  )
}

fp_safe_spearman <- function(x, y) {
  keep <- is.finite(as.numeric(x)) & is.finite(as.numeric(y))
  x <- as.numeric(x)[keep]
  y <- as.numeric(y)[keep]
  if (length(x) < 3L || length(unique(x)) < 2L || length(unique(y)) < 2L) {
    return(c(n = length(x), rho = NA_real_, p = NA_real_))
  }
  test <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE))
  c(n = length(x), rho = unname(test$estimate), p = test$p.value)
}

fp_blocked_spearman_permutation <- function(x, y, strata, permutations = 9999L,
                                            seed = 20260823L,
                                            alternative = c("greater", "less", "two.sided")) {
  alternative <- match.arg(alternative)
  keep <- is.finite(as.numeric(x)) & is.finite(as.numeric(y)) & !is.na(strata)
  x <- as.numeric(x)[keep]
  y <- as.numeric(y)[keep]
  strata <- as.character(strata)[keep]
  if (length(x) < 3L || length(unique(x)) < 2L || length(unique(y)) < 2L) {
    return(list(n = length(x), rho = NA_real_, p = NA_real_, movable_n = 0L))
  }
  observed <- suppressWarnings(stats::cor(x, y, method = "spearman"))
  groups <- split(seq_along(y), strata)
  movable <- sum(vapply(groups, length, integer(1)) > 1L)
  movable_n <- sum(vapply(groups, function(index) if (length(index) > 1L) length(index) else 0L, integer(1)))
  if (movable == 0L) return(list(n = length(x), rho = observed, p = NA_real_, movable_n = 0L))
  set.seed(seed)
  null <- numeric(permutations)
  for (b in seq_len(permutations)) {
    permuted <- y
    for (index in groups) if (length(index) > 1L) permuted[index] <- sample(y[index], length(index), replace = FALSE)
    null[[b]] <- suppressWarnings(stats::cor(x, permuted, method = "spearman"))
  }
  p <- switch(alternative,
    greater = (1 + sum(null >= observed, na.rm = TRUE)) / (1 + sum(is.finite(null))),
    less = (1 + sum(null <= observed, na.rm = TRUE)) / (1 + sum(is.finite(null))),
    two.sided = (1 + sum(abs(null) >= abs(observed), na.rm = TRUE)) / (1 + sum(is.finite(null))))
  list(n = length(x), rho = observed, p = p, movable_n = movable_n)
}

fp_human_pair_table <- function(collapsed_pairs, isolation_metrics) {
  fp_require_columns(isolation_metrics,
    c("exact_site_id", "colour", "spatial_fold", "same_colour_nn_km", "relative_isolation_nn",
      "local_population_rank", "population_5km_rank", "population_10km_rank",
      "population_25km_rank", "population_50km_rank"), "isolation metrics")
  if (!nrow(collapsed_pairs)) return(collapsed_pairs)
  pig <- isolation_metrics[isolation_metrics$colour == "pigmented", , drop = FALSE]
  index <- match(collapsed_pairs$pigmented_cell_id, pig$exact_site_id)
  out <- collapsed_pairs
  feature_columns <- c("spatial_fold", "same_colour_nn_km", "relative_isolation_nn",
    "local_population_rank", "population_5km_rank", "population_10km_rank",
    "population_25km_rank", "population_50km_rank")
  for (column in feature_columns) out[[column]] <- pig[[column]][index]
  out
}

fp_collapse_pigmented_cells <- function(human_pairs) {
  if (!nrow(human_pairs)) return(human_pairs)
  fp_require_columns(human_pairs, c("pigmented_cell_id", "early_days"), "human pair table")
  groups <- split(seq_len(nrow(human_pairs)), human_pairs$pigmented_cell_id)
  rows <- lapply(groups, function(index) {
    block <- human_pairs[index, , drop = FALSE]
    row <- data.frame(
      pigmented_cell_id = block$pigmented_cell_id[[1L]], early_days = fp_median_finite(block$early_days),
      median_pair_distance_km = fp_median_finite(block$distance_km),
      n_white_pair_cells = length(unique(block$white_cell_id)), stringsAsFactors = FALSE)
    for (column in c("spatial_fold", "same_colour_nn_km", "relative_isolation_nn",
      "local_population_rank", "population_5km_rank", "population_10km_rank",
      "population_25km_rank", "population_50km_rank")) row[[column]] <- fp_median_finite(block[[column]])
    row
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

fp_human_correlations <- function(pigmented_cells, permutations = 9999L, seed = 20260823L) {
  if (!nrow(pigmented_cells)) return(data.frame())
  features <- c(relative_isolation_nn = "primary_isolation",
    population_5km_rank = "primary_human_exposure",
    same_colour_nn_km = "sensitivity_isolation_raw",
    local_population_rank = "sensitivity_population_local",
    population_10km_rank = "sensitivity_population_10km",
    population_25km_rank = "sensitivity_population_25km",
    population_50km_rank = "sensitivity_population_50km")
  rows <- list()
  for (i in seq_along(features)) {
    feature <- names(features)[[i]]
    test <- fp_blocked_spearman_permutation(pigmented_cells$early_days, pigmented_cells[[feature]],
      pigmented_cells$spatial_fold, permutations = permutations, seed = seed + i, alternative = "greater")
    rows[[i]] <- data.frame(
      feature = feature, role = unname(features[[i]]),
      expected_direction = "positive: more human/isolated context -> pigmented observed earlier",
      n = test$n, spearman_rho = test$rho,
      within_spatial_fold_permutation_p_greater = test$p, movable_n = test$movable_n,
      stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  primary <- out$role %in% c("primary_isolation", "primary_human_exposure")
  out$holm_p_two_primary <- NA_real_
  if (sum(primary & is.finite(out$within_spatial_fold_permutation_p_greater)) > 0L) {
    out$holm_p_two_primary[primary] <- stats::p.adjust(
      out$within_spatial_fold_permutation_p_greater[primary], method = "holm")
  }
  out
}

fp_leave_one_fold_out <- function(pigmented_cells,
                                  features = c("relative_isolation_nn", "population_5km_rank")) {
  if (!nrow(pigmented_cells)) return(data.frame())
  folds <- sort(unique(pigmented_cells$spatial_fold[is.finite(pigmented_cells$spatial_fold)]))
  rows <- list()
  for (feature in features) {
    for (fold in folds) {
      keep <- pigmented_cells$spatial_fold != fold
      stat <- fp_safe_spearman(pigmented_cells$early_days[keep], pigmented_cells[[feature]][keep])
      rows[[length(rows) + 1L]] <- data.frame(
        feature = feature, held_out_spatial_fold = fold, n = stat[["n"]],
        spearman_rho = stat[["rho"]], asymptotic_p = stat[["p"]], stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}
