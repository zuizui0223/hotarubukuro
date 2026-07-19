# Core helpers for the revised floral-colour analysis.
# These functions deliberately use base R where possible so they can be tested in CI.

assert_columns <- function(data, columns) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

scale_numeric <- function(data, columns) {
  assert_columns(data, columns)
  out <- data
  for (column in columns) {
    x <- as.numeric(out[[column]])
    sx <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(sx) || sx == 0) stop("Cannot scale constant/non-finite predictor: ", column, call. = FALSE)
    out[[column]] <- as.numeric(scale(x))
  }
  out
}

# Preserve the observation table exactly. extractor must return one row per valid point.
row_safe_extract <- function(data, extractor, lon = "longitude", lat = "latitude", id = ".row_id") {
  assert_columns(data, c(lon, lat))
  if (!is.function(extractor)) stop("extractor must be a function", call. = FALSE)

  out <- data
  if (id %in% names(out)) stop("Reserved row id already exists: ", id, call. = FALSE)
  out[[id]] <- seq_len(nrow(out))

  valid <- is.finite(as.numeric(out[[lon]])) & is.finite(as.numeric(out[[lat]]))
  points <- out[valid, c(id, lon, lat), drop = FALSE]
  values <- extractor(points)
  if (!is.data.frame(values) || nrow(values) != nrow(points)) {
    stop("extractor must return a data.frame with one row per supplied point", call. = FALSE)
  }
  if (id %in% names(values)) values[[id]] <- NULL
  values[[id]] <- points[[id]]

  merged <- merge(out, values, by = id, all.x = TRUE, sort = FALSE)
  merged <- merged[match(seq_len(nrow(out)), merged[[id]]), , drop = FALSE]
  merged[[id]] <- NULL
  rownames(merged) <- NULL
  merged
}

# Continuous stacked suitability is not species richness. Return both estimands explicitly.
compute_bombus_metrics <- function(data, suitability_columns, thresholds = NULL) {
  assert_columns(data, suitability_columns)
  x <- as.matrix(data[, suitability_columns, drop = FALSE])
  storage.mode(x) <- "double"
  all_missing <- rowSums(is.finite(x)) == 0

  suitability_sum <- rowSums(x, na.rm = TRUE)
  suitability_mean <- rowMeans(x, na.rm = TRUE)
  suitability_sum[all_missing] <- NA_real_
  suitability_mean[all_missing] <- NA_real_

  result <- data.frame(
    bombus_suitability_sum = suitability_sum,
    bombus_suitability_mean = suitability_mean
  )

  if (!is.null(thresholds)) {
    if (is.null(names(thresholds)) || !all(suitability_columns %in% names(thresholds))) {
      stop("thresholds must be named for every suitability column", call. = FALSE)
    }
    present <- sweep(x, 2, thresholds[suitability_columns], FUN = ">=")
    potential_richness <- rowSums(present, na.rm = TRUE)
    potential_richness[all_missing] <- NA_real_
    result$potential_species_richness <- potential_richness
  }
  result
}

# Primary response: a* redness. Darkness is retained as a sensitivity outcome, not a CFA indicator.
make_colour_responses <- function(data, a = "a", lightness = "L") {
  assert_columns(data, c(a, lightness))
  data.frame(
    pigment_redness = as.numeric(data[[a]]),
    pigment_darkness = -as.numeric(data[[lightness]])
  )
}

# Exact variance identity with covariance retained. Half of 2*cov is assigned to each fitted component
# (a symmetric/Shapley allocation), so allocated proportions sum to one including residual variance.
covariance_aware_variance_allocation <- function(fixed, spatial, residual_variance) {
  stopifnot(length(fixed) == length(spatial), length(fixed) > 1)
  vf <- stats::var(fixed, na.rm = TRUE)
  vs <- stats::var(spatial, na.rm = TRUE)
  cfs <- stats::cov(fixed, spatial, use = "complete.obs")
  vr <- as.numeric(residual_variance)
  if (!is.finite(vr) || vr < 0) stop("residual_variance must be finite and non-negative", call. = FALSE)

  fixed_alloc <- vf + cfs
  spatial_alloc <- vs + cfs
  fitted_total <- vf + vs + 2 * cfs
  total <- fitted_total + vr

  data.frame(
    component = c("fixed_plus_half_covariance", "spatial_plus_half_covariance", "residual"),
    variance = c(fixed_alloc, spatial_alloc, vr),
    proportion = c(fixed_alloc, spatial_alloc, vr) / total,
    covariance_fixed_spatial = c(cfs, cfs, NA_real_),
    total_model_variance = total
  )
}

# Recommended PC-prior constructor arguments for inla.spde2.pcmatern.
spde_pc_prior <- function(range_km = 20, p_range = 0.05, sigma = 1, p_sigma = 0.05) {
  stopifnot(range_km > 0, sigma > 0, p_range > 0, p_range < 1, p_sigma > 0, p_sigma < 1)
  list(prior.range = c(range_km * 1000, p_range), prior.sigma = c(sigma, p_sigma))
}
