# Mechanism-oriented extension for the Campanula punctata flower-colour study.
#
# This file is sourced after ecological_analysis_v2.R.  It does not replace the
# locked environment + INLA-SPDE analysis.  It adds response-blind Bombus
# occurrence proxies, held-out evidence ladders, local matching, and a
# low-human-reference horticultural signature.

mechanism_required_base_functions <- c(
  "safe_z", "rank01", "normal_score", "write_csv_safe",
  "make_spatial_formula", "crossfit_gam", "fit_horticultural_qgams"
)
missing_base_functions <- mechanism_required_base_functions[
  !vapply(mechanism_required_base_functions, exists, logical(1), mode = "function")
]
if (length(missing_base_functions)) {
  stop(
    "Source scripts/ecological_analysis_v2.R before ecological_mechanism_v3.R. Missing: ",
    paste(missing_base_functions, collapse = ", "), call. = FALSE
  )
}

is_available_axis <- function(x, min_complete = 20L) {
  x <- as.numeric(x)
  sum(is.finite(x)) >= min_complete && is.finite(stats::sd(x, na.rm = TRUE)) &&
    stats::sd(x, na.rm = TRUE) > 0
}

read_bombus_occurrence_cache <- function(occurrence_dir, cell_km = 1) {
  require_packages("sf")
  species <- data.frame(
    short = c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis"),
    distribution_block = c("widespread", "widespread", "alpine", "alpine", "alpine"),
    stringsAsFactors = FALSE
  )
  files <- file.path(occurrence_dir, paste0(species$short, "_gbif.csv"))
  if (any(!file.exists(files))) {
    stop(
      "Missing frozen GBIF occurrence cache(s): ",
      paste(files[!file.exists(files)], collapse = ", "), call. = FALSE
    )
  }

  rows <- vector("list", nrow(species))
  manifest <- vector("list", nrow(species))
  for (i in seq_len(nrow(species))) {
    x <- utils::read.csv(files[i], check.names = FALSE, stringsAsFactors = FALSE)
    required <- c("decimalLongitude", "decimalLatitude")
    if (!all(required %in% names(x))) {
      stop("Occurrence cache lacks coordinates: ", files[i], call. = FALSE)
    }
    x$longitude <- suppressWarnings(as.numeric(x$decimalLongitude))
    x$latitude <- suppressWarnings(as.numeric(x$decimalLatitude))
    x$year <- if ("year" %in% names(x)) suppressWarnings(as.integer(x$year)) else NA_integer_
    x$month <- if ("month" %in% names(x)) suppressWarnings(as.integer(x$month)) else NA_integer_
    x$day <- if ("day" %in% names(x)) suppressWarnings(as.integer(x$day)) else NA_integer_
    date_stub <- suppressWarnings(as.Date(sprintf("2000-%02d-%02d", x$month, x$day)))
    x$event_doy <- suppressWarnings(as.integer(format(date_stub, "%j")))
    x$datasetKey <- if ("datasetKey" %in% names(x)) as.character(x$datasetKey) else NA_character_
    x$record_key <- if ("key" %in% names(x)) as.character(x$key) else NA_character_
    x <- x[
      is.finite(x$longitude) & is.finite(x$latitude) &
        x$longitude >= 120 & x$longitude <= 150 &
        x$latitude >= 20 & x$latitude <= 50,
      , drop = FALSE
    ]
    if (any(nzchar(x$record_key) & !is.na(x$record_key))) {
      key_ok <- nzchar(x$record_key) & !is.na(x$record_key)
      duplicate_key <- key_ok & duplicated(x$record_key)
      x <- x[!duplicate_key, , drop = FALSE]
    }
    x$short <- species$short[i]
    x$distribution_block <- species$distribution_block[i]
    x$source_file <- normalizePath(files[i], winslash = "/", mustWork = TRUE)
    rows[[i]] <- x[c(
      "short", "distribution_block", "longitude", "latitude", "year",
      "month", "day", "event_doy", "datasetKey", "record_key", "source_file"
    )]
    manifest[[i]] <- data.frame(
      short = species$short[i], distribution_block = species$distribution_block[i],
      file = normalizePath(files[i], winslash = "/", mustWork = TRUE),
      file_md5 = unname(tools::md5sum(files[i])), n_coordinate_records = nrow(x),
      stringsAsFactors = FALSE
    )
  }
  occ <- do.call(rbind, rows)
  pts <- sf::st_as_sf(occ, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  japan_laea <- paste(
    "+proj=laea +lat_0=36 +lon_0=137 +x_0=0 +y_0=0",
    "+datum=WGS84 +units=m +no_defs"
  )
  xy <- sf::st_coordinates(sf::st_transform(pts, japan_laea)) / 1000
  occ$x_km <- xy[, 1]
  occ$y_km <- xy[, 2]

  # Opportunistic datasets can contain bursts of records.  The primary proxy
  # uses distributional occupancy, not abundance: one record per species,
  # projected 1-km cell, and year.  This rule is fixed without using flower
  # colour or flower locations.
  occ$cell_x <- floor(occ$x_km / cell_km)
  occ$cell_y <- floor(occ$y_km / cell_km)
  seasonal <- occ[is.finite(occ$event_doy), , drop = FALSE]
  seasonal_key <- paste(
    seasonal$short, seasonal$cell_x, seasonal$cell_y,
    ifelse(is.finite(seasonal$year), seasonal$year, "unknown"),
    seasonal$event_doy, sep = "|"
  )
  seasonal <- seasonal[!duplicated(seasonal_key), , drop = FALSE]
  occ$year_key <- ifelse(is.finite(occ$year), as.character(occ$year), "unknown")
  occupancy_key <- paste(occ$short, occ$cell_x, occ$cell_y, occ$year_key, sep = "|")
  occ <- occ[!duplicated(occupancy_key), , drop = FALSE]
  dedup_n <- table(occ$short)
  manifest <- do.call(rbind, manifest)
  manifest$n_occupancy_records <- as.integer(dedup_n[manifest$short])
  seasonal_n <- table(seasonal$short)
  manifest$n_seasonal_records <- as.integer(seasonal_n[manifest$short])
  manifest$seasonal_deduplication <-
    "one record per species x 1-km cell x year x day-of-year"
  manifest$deduplication <- paste0(
    "one_record_per_species_", format(cell_km, trim = TRUE), "km_cell_year"
  )
  list(data = occ, seasonal_data = seasonal, manifest = manifest, species = species)
}

compute_bombus_occurrence_indices <- function(data, occurrences,
                                              radii_km = c(50, 100),
                                              minimum_kernel_support = 5,
                                              chunk_size = 100L) {
  required_data <- c("x_km", "y_km")
  required_occ <- c("x_km", "y_km", "short")
  stopifnot(all(required_data %in% names(data)), all(required_occ %in% names(occurrences)))
  species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
  if (!all(species %in% unique(occurrences$short))) {
    stop("Occurrence input does not cover all five prespecified Bombus species.", call. = FALSE)
  }

  out <- data
  definitions <- list()
  definition_i <- 0L
  n <- nrow(data)
  chunks <- split(seq_len(n), ceiling(seq_len(n) / as.integer(chunk_size)))
  for (radius in sort(unique(as.numeric(radii_km)))) {
    if (!is.finite(radius) || radius <= 0) stop("All Bombus radii must be positive.")
    sigma <- radius / 2
    kernel_by_species <- matrix(
      0, nrow = n, ncol = length(species), dimnames = list(NULL, species)
    )
    raw_total <- numeric(n)
    for (idx in chunks) {
      dx <- outer(data$x_km[idx], occurrences$x_km, "-")
      dy <- outer(data$y_km[idx], occurrences$y_km, "-")
      d2 <- dx^2 + dy^2
      inside <- d2 <= radius^2
      weights <- exp(-0.5 * d2 / sigma^2)
      weights[!inside] <- 0
      raw_total[idx] <- rowSums(inside)
      for (j in seq_along(species)) {
        kernel_by_species[idx, j] <- rowSums(
          weights[, occurrences$short == species[j], drop = FALSE]
        )
      }
    }
    suffix <- paste0("_", format(radius, scientific = FALSE, trim = TRUE), "km")
    for (sp in species) out[[paste0("occ_", sp, suffix)]] <- kernel_by_species[, sp]
    widespread <- rowSums(kernel_by_species[, c("ardens", "diversus"), drop = FALSE])
    alpine <- rowSums(kernel_by_species[, c("beaticola", "consobrinus", "honshuensis"), drop = FALSE])
    total <- widespread + alpine
    supported <- total >= minimum_kernel_support
    # Jeffreys smoothing avoids infinite logits.  The denominator is explicitly
    # the five study species, not all Japanese Bombus and not visitation effort.
    widespread_share <- (widespread + 0.5) / (total + 1)
    alpine_share <- (alpine + 0.5) / (total + 1)
    w_logit <- ifelse(supported, stats::qlogis(widespread_share), NA_real_)
    a_logit <- ifelse(supported, stats::qlogis(alpine_share), NA_real_)
    area <- pi * radius^2
    for (sp in species) {
      out[[paste0("Bombus_", sp, "_occ_density", suffix)]] <-
        safe_z(log1p(kernel_by_species[, sp] / area))
    }
    widespread_supported <- widespread >= minimum_kernel_support
    ardens_share <- (kernel_by_species[, "ardens"] + 0.5) / (widespread + 1)
    out[[paste0("Bombus_W_ardens_share", suffix)]] <- safe_z(ifelse(
      widespread_supported, stats::qlogis(ardens_share), NA_real_
    ))
    out[[paste0("Bombus_occ_effort", suffix)]] <- total
    out[[paste0("Bombus_occ_raw_records", suffix)]] <- raw_total
    out[[paste0("Bombus_occ_supported", suffix)]] <- supported
    out[[paste0("Bombus_W_occ", suffix)]] <- safe_z(w_logit)
    out[[paste0("Bombus_A_occ", suffix)]] <- safe_z(a_logit)
    out[[paste0("Bombus_W_occ_density", suffix)]] <- safe_z(log1p(widespread / area))
    out[[paste0("Bombus_A_occ_density", suffix)]] <- safe_z(log1p(alpine / area))
    out[[paste0("Bombus_total_occ_density", suffix)]] <- safe_z(log1p(total / area))

    definition_i <- definition_i + 1L
    definitions[[definition_i]] <- data.frame(
      radius_km = radius, kernel = "Gaussian truncated at radius; sigma = radius/2",
      deduplication = "one record per species x 1-km cell x year",
      effort_denominator = "five prespecified study Bombus species only",
      minimum_kernel_support = minimum_kernel_support,
      primary_widespread_index = paste0("Bombus_W_occ", suffix),
      density_sensitivity = paste0("Bombus_W_occ_density", suffix),
      total_density_sensitivity = paste0("Bombus_total_occ_density", suffix),
      widespread_species_sensitivities = paste(
        paste0("Bombus_", c("ardens", "diversus"), "_occ_density", suffix),
        collapse = " | "
      ),
      within_widespread_composition = paste0("Bombus_W_ardens_share", suffix),
      interpretation = paste(
        "response-blind local target-group occurrence composition;",
        "not abundance, visitation frequency, selection pressure, or calibrated probability"
      ),
      stringsAsFactors = FALSE
    )
  }
  list(data = out, definitions = do.call(rbind, definitions))
}

compute_bombus_seasonal_indices <- function(data, occurrences,
                                            radius_km = 100,
                                            temporal_bandwidth_days = 30,
                                            temporal_window_days = 60,
                                            minimum_spatial_support = 5,
                                            chunk_size = 100L) {
  required_data <- c("x_km", "y_km", "DOY")
  required_occ <- c("x_km", "y_km", "event_doy", "short")
  stopifnot(
    all(required_data %in% names(data)),
    all(required_occ %in% names(occurrences))
  )
  if (!nrow(occurrences)) stop("No dated Bombus occurrences are available.")
  sigma <- radius_km / 2
  n <- nrow(data)
  chunks <- split(seq_len(n), ceiling(seq_len(n) / as.integer(chunk_size)))
  spatial_total <- seasonal_total <- seasonal_w <- seasonal_a <- numeric(n)
  raw_spatial_support <- integer(n)
  for (idx in chunks) {
    d2 <- outer(data$x_km[idx], occurrences$x_km, "-")^2 +
      outer(data$y_km[idx], occurrences$y_km, "-")^2
    inside <- d2 <= radius_km^2
    spatial_weight <- exp(-0.5 * d2 / sigma^2)
    spatial_weight[!inside] <- 0
    doy_distance <- abs(outer(data$DOY[idx], occurrences$event_doy, "-"))
    doy_distance <- pmin(doy_distance, 366 - doy_distance)
    temporal_weight <- exp(-0.5 * (doy_distance / temporal_bandwidth_days)^2)
    temporal_weight[doy_distance > temporal_window_days] <- 0
    joint_weight <- spatial_weight * temporal_weight
    spatial_total[idx] <- rowSums(spatial_weight)
    seasonal_total[idx] <- rowSums(joint_weight)
    seasonal_w[idx] <- rowSums(
      joint_weight[, occurrences$short %in% c("ardens", "diversus"), drop = FALSE]
    )
    seasonal_a[idx] <- rowSums(
      joint_weight[, occurrences$short %in%
        c("beaticola", "consobrinus", "honshuensis"), drop = FALSE]
    )
    raw_spatial_support[idx] <- rowSums(inside)
  }
  supported <- raw_spatial_support >= minimum_spatial_support
  overlap <- ifelse(
    supported,
    stats::qlogis((seasonal_total + 0.5) / (spatial_total + 1)),
    NA_real_
  )
  suffix <- paste0(
    "_", format(radius_km, scientific = FALSE, trim = TRUE), "km_",
    format(temporal_bandwidth_days, scientific = FALSE, trim = TRUE), "d"
  )
  area <- pi * radius_km^2
  out <- data
  out[[paste0("Bombus_total_seasonal_density", suffix)]] <-
    safe_z(log1p(seasonal_total / area))
  out[[paste0("Bombus_W_seasonal_density", suffix)]] <-
    safe_z(log1p(seasonal_w / area))
  out[[paste0("Bombus_A_seasonal_density", suffix)]] <-
    safe_z(log1p(seasonal_a / area))
  out[[paste0("Bombus_temporal_overlap", suffix)]] <- safe_z(overlap)
  out[[paste0("Bombus_seasonal_spatial_support", suffix)]] <- raw_spatial_support
  definition <- data.frame(
    radius_km = radius_km,
    spatial_kernel = "Gaussian truncated at radius; sigma = radius/2",
    temporal_bandwidth_days = temporal_bandwidth_days,
    temporal_window_days = temporal_window_days,
    circular_year_days = 366,
    seasonal_density = paste0("Bombus_total_seasonal_density", suffix),
    temporal_overlap = paste0("Bombus_temporal_overlap", suffix),
    deduplication = "one record per species x 1-km cell x year x day-of-year",
    interpretation = paste(
      "response-blind spatiotemporal occurrence availability;",
      "not abundance, visitation frequency, or selection pressure"
    ),
    stringsAsFactors = FALSE
  )
  list(data = out, definition = definition)
}

gam_formula_v3 <- function(response, linear_terms = character(), spatial = FALSE,
                           k_space = 40) {
  rhs <- linear_terms
  if (spatial) rhs <- c(rhs, sprintf("s(x_km, y_km, k = %d, bs = 'tp')", k_space))
  if (!length(rhs)) rhs <- "1"
  stats::as.formula(paste(response, "~", paste(rhs, collapse = " + ")))
}

crossfit_incremental_gam <- function(data, response, base_terms, predictor,
                                     spatial = FALSE, k_space = 40,
                                     fold_col = "spatial_fold") {
  require_packages("mgcv")
  required <- unique(c(response, base_terms, predictor, fold_col, if (spatial) c("x_km", "y_km")))
  d <- data[stats::complete.cases(data[required]), , drop = FALSE]
  folds <- sort(unique(d[[fold_col]]))
  pred_base <- pred_full <- rep(NA_real_, nrow(d))
  fold_rows <- vector("list", length(folds))
  for (i in seq_along(folds)) {
    fold <- folds[i]
    test <- which(d[[fold_col]] == fold)
    train <- which(d[[fold_col]] != fold)
    k_use <- max(10L, min(as.integer(k_space), floor(length(train) / 20)))
    base_formula <- gam_formula_v3(response, base_terms, spatial, k_use)
    full_formula <- gam_formula_v3(response, c(base_terms, predictor), spatial, k_use)
    base_fit <- mgcv::gam(base_formula, data = d[train, , drop = FALSE], method = "REML")
    full_fit <- mgcv::gam(full_formula, data = d[train, , drop = FALSE], method = "REML")
    pred_base[test] <- as.numeric(stats::predict(base_fit, newdata = d[test, ], type = "response"))
    pred_full[test] <- as.numeric(stats::predict(full_fit, newdata = d[test, ], type = "response"))
    fold_rows[[i]] <- data.frame(
      fold = fold, n_train = length(train), n_test = length(test),
      base_formula = paste(deparse(base_formula), collapse = " "),
      full_formula = paste(deparse(full_formula), collapse = " "),
      stringsAsFactors = FALSE
    )
  }
  y <- d[[response]]
  denom <- sum((y - mean(y))^2)
  r2_base <- 1 - sum((y - pred_base)^2) / denom
  r2_full <- 1 - sum((y - pred_full)^2) / denom
  list(
    metrics = data.frame(
      n = nrow(d), heldout_R2_base = r2_base, heldout_R2_with_Bombus = r2_full,
      delta_heldout_R2 = r2_full - r2_base,
      RMSE_base = sqrt(mean((y - pred_base)^2)),
      RMSE_with_Bombus = sqrt(mean((y - pred_full)^2)),
      stringsAsFactors = FALSE
    ),
    predictions = data.frame(row_id = as.integer(rownames(d)), observed = y,
                             base = pred_base, with_Bombus = pred_full),
    log = do.call(rbind, fold_rows)
  )
}

fit_bombus_evidence_ladder <- function(data, response, predictor, env_terms,
                                       k_space = 40) {
  require_packages("mgcv")
  rungs <- list(
    landscape = list(base = character(), spatial = FALSE),
    environment_adjusted = list(base = env_terms, spatial = FALSE),
    space_adjusted = list(base = "region", spatial = TRUE),
    environment_space_adjusted = list(base = c("region", env_terms), spatial = TRUE)
  )
  coefficients <- cv_rows <- logs <- list()
  for (i in seq_along(rungs)) {
    rung <- names(rungs)[i]
    spec <- rungs[[i]]
    base_terms <- spec$base
    required <- unique(c(response, base_terms, predictor, "spatial_fold",
                         if (spec$spatial) c("x_km", "y_km")))
    d <- data[stats::complete.cases(data[required]), , drop = FALSE]
    if ("region" %in% base_terms && length(unique(as.character(d$region))) < 2L) {
      base_terms <- setdiff(base_terms, "region")
      required <- unique(c(response, base_terms, predictor, "spatial_fold",
                           if (spec$spatial) c("x_km", "y_km")))
      d <- data[stats::complete.cases(data[required]), , drop = FALSE]
    }
    k_use <- max(10L, min(as.integer(k_space), floor(nrow(d) / 20)))
    form <- gam_formula_v3(response, c(base_terms, predictor), spec$spatial, k_use)
    fit <- mgcv::gam(form, data = d, method = "REML")
    tab <- summary(fit)$p.table
    coefficients[[i]] <- data.frame(
      predictor = predictor, rung = rung, n = nrow(d),
      estimate = if (predictor %in% rownames(tab)) tab[predictor, 1] else NA_real_,
      se = if (predictor %in% rownames(tab)) tab[predictor, 2] else NA_real_,
      statistic = if (predictor %in% rownames(tab)) tab[predictor, 3] else NA_real_,
      p_value = if (predictor %in% rownames(tab)) tab[predictor, 4] else NA_real_,
      formula = paste(deparse(form), collapse = " "), stringsAsFactors = FALSE
    )
    cv <- crossfit_incremental_gam(
      data, response, base_terms, predictor, spec$spatial, k_space
    )
    cv$metrics$predictor <- predictor
    cv$metrics$rung <- rung
    cv_rows[[i]] <- cv$metrics
    cv$log$predictor <- predictor
    cv$log$rung <- rung
    logs[[i]] <- cv$log
  }
  list(
    coefficients = do.call(rbind, coefficients),
    heldout_metrics = do.call(rbind, cv_rows),
    log = do.call(rbind, logs)
  )
}

decompose_bombus_proxy <- function(data, predictor, env_terms, response = "response",
                                   k_space = 40) {
  required <- c(predictor, response, "region", env_terms, "x_km", "y_km", "spatial_fold")
  keep <- stats::complete.cases(data[required])
  d <- data[keep, , drop = FALSE]
  base_terms <- c("region", env_terms)
  if (length(unique(as.character(d$region))) < 2L) {
    base_terms <- setdiff(base_terms, "region")
  }
  cf <- crossfit_gam(d, predictor, base_terms, k_space = k_space)
  shared <- cf$predictions
  local <- d[[predictor]] - shared
  heldout_r2 <- 1 - sum(local^2) / sum((d[[predictor]] - mean(d[[predictor]]))^2)
  component_data <- data.frame(
    source_row = which(keep), predictor = predictor, observed = d[[predictor]],
    shared_biogeographic_oof = shared, local_component_oof = local,
    response = d[[response]], stringsAsFactors = FALSE
  )
  component_summary <- data.frame(
    predictor = predictor, n = nrow(d), heldout_R2_from_environment_space = heldout_r2,
    local_SD_fraction = stats::sd(local) / stats::sd(d[[predictor]]),
    colour_correlation_with_shared = stats::cor(d[[response]], shared),
    colour_correlation_with_local = stats::cor(d[[response]], local),
    interpretation = paste(
      "descriptive decomposition; shared component is retained as biological",
      "co-geography and local component is the conditionally identifiable contrast"
    ), stringsAsFactors = FALSE
  )
  list(data = component_data, summary = component_summary, log = cf$log)
}

match_local_bombus_contrasts <- function(data, predictor, env_terms,
                                         response = "response",
                                         radius_km = 100,
                                         low_quantile = 0.25,
                                         high_quantile = 0.75,
                                         env_caliper = 1.5,
                                         propensity_caliper_sd = 0.2) {
  required <- c(predictor, response, env_terms, "x_km", "y_km", "region")
  d <- data[stats::complete.cases(data[required]), , drop = FALSE]
  d$source_row <- which(stats::complete.cases(data[required]))
  d$exposure_group <- "middle"
  for (reg in levels(d$region)) {
    idx <- which(d$region == reg)
    q <- stats::quantile(d[[predictor]][idx], c(low_quantile, high_quantile), na.rm = TRUE)
    d$exposure_group[idx[d[[predictor]][idx] <= q[1]]] <- "low"
    d$exposure_group[idx[d[[predictor]][idx] >= q[2]]] <- "high"
  }
  extreme <- d$exposure_group %in% c("low", "high")
  propensity_data <- d[extreme, , drop = FALSE]
  propensity_data$is_high <- as.integer(propensity_data$exposure_group == "high")
  propensity_formula <- stats::reformulate(
    c(env_terms, "poly(x_km, 2, raw = TRUE)", "poly(y_km, 2, raw = TRUE)", "region"),
    response = "is_high"
  )
  propensity_fit <- suppressWarnings(stats::glm(
    propensity_formula, data = propensity_data, family = stats::binomial()
  ))
  d$propensity_logit <- NA_real_
  d$propensity_logit[extreme] <- as.numeric(stats::predict(propensity_fit, type = "link"))
  propensity_caliper <- propensity_caliper_sd * stats::sd(
    d$propensity_logit[extreme], na.rm = TRUE
  )
  if (!is.finite(propensity_caliper) || propensity_caliper <= 0) {
    propensity_caliper <- 0
  }
  env_scaled <- scale(as.matrix(d[env_terms]))
  candidates <- list()
  candidate_i <- 0L
  for (reg in levels(d$region)) {
    high <- which(d$region == reg & d$exposure_group == "high")
    low <- which(d$region == reg & d$exposure_group == "low")
    if (!length(high) || !length(low)) next
    for (h in high) {
      geo <- sqrt((d$x_km[low] - d$x_km[h])^2 + (d$y_km[low] - d$y_km[h])^2)
      nearby <- low[geo <= radius_km]
      if (!length(nearby)) next
      geo <- geo[geo <= radius_km]
      delta <- sweep(env_scaled[nearby, , drop = FALSE], 2, env_scaled[h, ], "-")
      env_distance <- sqrt(rowMeans(delta^2))
      propensity_distance <- abs(d$propensity_logit[nearby] - d$propensity_logit[h])
      ok <- is.finite(env_distance) & env_distance <= env_caliper &
        is.finite(propensity_distance) & propensity_distance <= propensity_caliper
      if (!any(ok)) next
      candidate_i <- candidate_i + 1L
      candidates[[candidate_i]] <- data.frame(
        high = h, low = nearby[ok], region = as.character(d$region[h]),
        geographic_distance_km = geo[ok], env_distance = env_distance[ok],
        propensity_distance = propensity_distance[ok],
        matching_cost = propensity_distance[ok] / pmax(propensity_caliper, 1e-8) +
          env_distance[ok] + geo[ok] / radius_km,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(candidates)) {
    return(list(
      pairs = data.frame(), balance = data.frame(),
      summary = data.frame(
        predictor = predictor, n_pairs = 0L,
        high_quantile = high_quantile, low_quantile = low_quantile,
        maximum_distance_km = radius_km, env_caliper = env_caliper,
        propensity_caliper_sd = propensity_caliper_sd,
        propensity_caliper = propensity_caliper,
        median_distance_km = NA_real_, mean_predictor_difference = NA_real_,
        mean_colour_difference = NA_real_, se = NA_real_, lower_95 = NA_real_,
        upper_95 = NA_real_, paired_t_p_value = NA_real_,
        wilcoxon_p_value = NA_real_, max_abs_environment_SMD = NA_real_,
        acceptable_balance = FALSE,
        interpretation = "no response-blind pairs satisfied the prespecified propensity, distance, and balance rules",
        stringsAsFactors = FALSE
      )
    ))
  }
  candidates <- do.call(rbind, candidates)
  candidates <- candidates[order(candidates$matching_cost, candidates$env_distance,
                                 candidates$geographic_distance_km), , drop = FALSE]
  used_high <- used_low <- integer()
  selected <- logical(nrow(candidates))
  for (i in seq_len(nrow(candidates))) {
    if (!(candidates$high[i] %in% used_high) && !(candidates$low[i] %in% used_low)) {
      selected[i] <- TRUE
      used_high <- c(used_high, candidates$high[i])
      used_low <- c(used_low, candidates$low[i])
    }
  }
  pairs <- candidates[selected, , drop = FALSE]
  pairs$pair_id <- seq_len(nrow(pairs))
  pairs$high_source_row <- d$source_row[pairs$high]
  pairs$low_source_row <- d$source_row[pairs$low]
  pairs$high_predictor <- d[[predictor]][pairs$high]
  pairs$low_predictor <- d[[predictor]][pairs$low]
  pairs$predictor_difference <- pairs$high_predictor - pairs$low_predictor
  pairs$high_colour <- d[[response]][pairs$high]
  pairs$low_colour <- d[[response]][pairs$low]
  pairs$colour_difference <- pairs$high_colour - pairs$low_colour

  balance <- do.call(rbind, lapply(env_terms, function(term) {
    high_value <- d[[term]][pairs$high]
    low_value <- d[[term]][pairs$low]
    pooled_sd <- sqrt((stats::var(high_value) + stats::var(low_value)) / 2)
    data.frame(
      predictor = predictor, term = term,
      standardized_mean_difference = if (is.finite(pooled_sd) && pooled_sd > 0) {
        (mean(high_value) - mean(low_value)) / pooled_sd
      } else 0,
      mean_absolute_pair_difference = mean(abs(high_value - low_value)),
      stringsAsFactors = FALSE
    )
  }))
  diff <- pairs$colour_difference
  n_pairs <- length(diff)
  se <- if (n_pairs > 1) stats::sd(diff) / sqrt(n_pairs) else NA_real_
  t_p <- if (n_pairs > 1) stats::t.test(diff)$p.value else NA_real_
  wilcox_p <- if (n_pairs > 1 && any(diff != 0)) {
    suppressWarnings(stats::wilcox.test(diff, exact = FALSE)$p.value)
  } else NA_real_
  summary <- data.frame(
    predictor = predictor, n_pairs = n_pairs,
    high_quantile = high_quantile, low_quantile = low_quantile,
    maximum_distance_km = radius_km, env_caliper = env_caliper,
    propensity_caliper_sd = propensity_caliper_sd,
    propensity_caliper = propensity_caliper,
    median_distance_km = stats::median(pairs$geographic_distance_km),
    mean_predictor_difference = mean(pairs$predictor_difference),
    mean_colour_difference = mean(diff), se = se,
    lower_95 = mean(diff) - stats::qnorm(0.975) * se,
    upper_95 = mean(diff) + stats::qnorm(0.975) * se,
    paired_t_p_value = t_p, wilcoxon_p_value = wilcox_p,
    max_abs_environment_SMD = max(abs(balance$standardized_mean_difference)),
    acceptable_balance = max(abs(balance$standardized_mean_difference)) <= 0.25,
    interpretation = paste(
      "high-minus-low response difference after response-blind greedy matching;",
      "observations are not selected or paired using flower colour"
    ), stringsAsFactors = FALSE
  )
  list(pairs = pairs, balance = balance, summary = summary)
}

define_low_human_reference <- function(data, exposure = "z_H", fraction = 0.30) {
  if (!(exposure %in% names(data)) || !is_available_axis(data[[exposure]])) {
    stop("A non-constant human-exposure axis is required for the reference design.", call. = FALSE)
  }
  if (!is.finite(fraction) || fraction <= 0.1 || fraction >= 0.5) {
    stop("Reference fraction must be between 0.1 and 0.5.", call. = FALSE)
  }
  out <- data
  out$human_exposure_percentile_within_region <- NA_real_
  out$low_human_reference <- FALSE
  definition <- list()
  for (reg in levels(out$region)) {
    idx <- which(out$region == reg & is.finite(out[[exposure]]))
    out$human_exposure_percentile_within_region[idx] <- rank01(out[[exposure]][idx])
    threshold <- stats::quantile(out[[exposure]][idx], fraction, na.rm = TRUE, type = 8)
    out$low_human_reference[idx] <- out[[exposure]][idx] <= threshold
    definition[[reg]] <- data.frame(
      region = reg, exposure = exposure, reference_fraction = fraction,
      threshold = threshold, n_total = length(idx),
      n_reference = sum(out$low_human_reference[idx]), stringsAsFactors = FALSE
    )
  }
  list(data = out, definition = do.call(rbind, definition))
}

crossfit_reference_qgam <- function(data, response, linear_terms,
                                    reference_col = "low_human_reference",
                                    taus = c(0.5, 0.9, 0.95),
                                    fold_col = "spatial_fold", k_space = 40,
                                    include_year = FALSE) {
  require_packages(c("qgam", "mgcv"))
  required <- unique(c(
    response, linear_terms, reference_col, fold_col, "x_km", "y_km",
    if (include_year) "year"
  ))
  complete <- stats::complete.cases(data[required])
  folds <- sort(unique(data[[fold_col]][complete]))
  pred <- matrix(
    NA_real_, nrow(data), length(taus),
    dimnames = list(NULL, paste0("q", formatC(taus * 100, width = 2, flag = "0")))
  )
  support_fraction <- rep(NA_real_, nrow(data))
  logs <- list()
  calibrations <- list()
  log_i <- calibration_i <- 0L
  numeric_support_terms <- linear_terms[
    vapply(data[linear_terms], function(x) is.numeric(x) || is.integer(x), logical(1))
  ]
  for (fold in folds) {
    test <- which(complete & data[[fold_col]] == fold)
    train <- which(
      complete & data[[reference_col]] & data[[fold_col]] != fold
    )
    if (length(train) < 100L) {
      stop("Too few low-human reference observations in fold training set: ", length(train))
    }
    k_use <- max(10L, min(as.integer(k_space), floor(length(train) / 20)))
    use_year <- include_year &&
      length(unique(data$year[train][is.finite(data$year[train])])) >= 9
    form <- make_spatial_formula(response, linear_terms, k_use, year_smooth = use_year)

    if (length(numeric_support_terms)) {
      in_range <- sapply(numeric_support_terms, function(term) {
        lim <- stats::quantile(data[[term]][train], c(0.01, 0.99), na.rm = TRUE, type = 8)
        data[[term]][test] >= lim[1] & data[[term]][test] <= lim[2]
      })
      if (is.null(dim(in_range))) in_range <- matrix(in_range, ncol = 1)
      support_fraction[test] <- rowMeans(in_range)
    } else {
      support_fraction[test] <- 1
    }

    for (j in seq_along(taus)) {
      warnings <- character()
      fit <- withCallingHandlers(
        qgam::qgam(
          form, data = data[train, , drop = FALSE], qu = taus[j],
          argGam = list(method = "REML")
        ),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      pred[test, j] <- as.numeric(stats::predict(fit, newdata = data[test, ], type = "response"))
      log_i <- log_i + 1L
      logs[[log_i]] <- data.frame(
        fold = fold, tau = taus[j], n_train_reference = length(train),
        n_test_all = length(test), formula = paste(deparse(form), collapse = " "),
        warnings = paste(unique(warnings), collapse = " | "), stringsAsFactors = FALSE
      )
      ref_test <- test[data[[reference_col]][test]]
      calibration_i <- calibration_i + 1L
      calibrations[[calibration_i]] <- data.frame(
        fold = fold, tau = taus[j], n_reference_test = length(ref_test),
        empirical_coverage = if (length(ref_test)) {
          mean(data[[response]][ref_test] <= pred[ref_test, j])
        } else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  if (any(complete & !stats::complete.cases(pred))) {
    stop("Reference qGAM failed to return all held-out predictions.", call. = FALSE)
  }
  list(
    predictions = as.data.frame(pred), support_fraction = support_fraction,
    common_support = support_fraction >= 0.8,
    log = do.call(rbind, logs), calibration = do.call(rbind, calibrations),
    complete = complete
  )
}

crossfit_reference_rq <- function(data, response, linear_terms,
                                  reference_col = "low_human_reference",
                                  tau = 0.1, fold_col = "spatial_fold",
                                  k_space = 15, include_year = TRUE) {
  require_packages(c("mgcv", "quantreg"))
  required <- unique(c(
    response, linear_terms, reference_col, fold_col, "x_km", "y_km",
    if (include_year) "year"
  ))
  complete <- stats::complete.cases(data[required])
  d <- data[complete, , drop = FALSE]
  source_row <- which(complete)
  # A linear year term is deliberate.  A natural-spline year term was rejected
  # after it produced singular fold-specific designs and grossly miscalibrated
  # held-out Q0.10 predictions.  Phenology seasonality is represented by the
  # response itself; year only absorbs a long-term observation-date drift.
  fixed_terms <- linear_terms
  if (include_year) fixed_terms <- c(fixed_terms, "year")
  fixed_formula <- stats::reformulate(fixed_terms)
  X_fixed <- stats::model.matrix(fixed_formula, data = d)
  k_use <- max(10L, min(as.integer(k_space), floor(nrow(d) / 20)))
  X_space <- mgcv::smoothCon(
    mgcv::s(x_km, y_km, k = k_use, bs = "tp"),
    data = d, absorb.cons = TRUE
  )[[1]]$X
  X <- cbind(X_fixed, X_space)
  prediction <- rep(NA_real_, nrow(data))
  support_fraction <- rep(NA_real_, nrow(data))
  logs <- calibrations <- list()
  support_terms <- c(linear_terms, if (include_year) "year")
  numeric_support_terms <- support_terms[
    vapply(data[support_terms], function(x) is.numeric(x) || is.integer(x), logical(1))
  ]
  folds <- sort(unique(d[[fold_col]]))
  for (i in seq_along(folds)) {
    fold <- folds[i]
    test_local <- which(d[[fold_col]] == fold)
    train_local <- which(d[[reference_col]] & d[[fold_col]] != fold)
    if (length(train_local) < 100L) {
      stop("Too few reference observations for held-out quantile regression.")
    }
    # The basis is built once, but rank is checked on each training fold.  This
    # prevents a fold-specific empty factor/basis direction from silently
    # yielding unusable coefficients.
    fold_qr <- qr(X[train_local, , drop = FALSE], tol = 1e-8)
    keep_cols <- sort(fold_qr$pivot[seq_len(fold_qr$rank)])
    X_train <- X[train_local, keep_cols, drop = FALSE]
    X_test <- X[test_local, keep_cols, drop = FALSE]
    warnings <- character()
    fit <- withCallingHandlers(
      quantreg::rq.fit.br(
        X_train, d[[response]][train_local], tau = tau
      ),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    if (any(grepl("singular|failed|non.?unique", warnings, ignore.case = TRUE)) ||
        any(!is.finite(fit$coefficients))) {
      stop(
        "Fold ", fold, " produced an invalid phenology quantile fit: ",
        paste(unique(warnings), collapse = " | "), call. = FALSE
      )
    }
    prediction[source_row[test_local]] <- as.numeric(X_test %*% fit$coefficients)
    if (length(numeric_support_terms)) {
      in_range <- sapply(numeric_support_terms, function(term) {
        lim <- stats::quantile(d[[term]][train_local], c(0.01, 0.99), na.rm = TRUE, type = 8)
        d[[term]][test_local] >= lim[1] & d[[term]][test_local] <= lim[2]
      })
      if (is.null(dim(in_range))) in_range <- matrix(in_range, ncol = 1)
      support_fraction[source_row[test_local]] <- rowMeans(in_range)
    } else support_fraction[source_row[test_local]] <- 1
    logs[[i]] <- data.frame(
      fold = fold, tau = tau, n_train_reference = length(train_local),
      n_test_all = length(test_local), method = "quantreg_rq_fit_br_rank_reduced",
      design_columns = ncol(X), retained_rank = length(keep_cols),
      fixed_formula = paste(deparse(fixed_formula), collapse = " "),
      spatial_basis = paste0("thin_plate_basis_k", k_use),
      warnings = paste(unique(warnings), collapse = " | "), stringsAsFactors = FALSE
    )
    reference_test <- test_local[d[[reference_col]][test_local]]
    calibrations[[i]] <- data.frame(
      fold = fold, tau = tau, n_reference_test = length(reference_test),
      empirical_coverage = mean(
        d[[response]][reference_test] <= prediction[source_row[reference_test]]
      ), stringsAsFactors = FALSE
    )
  }
  if (any(complete & !is.finite(prediction))) {
    stop("Reference quantile regression failed to return all held-out predictions.")
  }
  list(
    predictions = data.frame(q10 = prediction),
    support_fraction = support_fraction,
    common_support = support_fraction >= 0.8,
    log = do.call(rbind, logs), calibration = do.call(rbind, calibrations),
    decision_audit = data.frame(
      candidate = c(
        "qGAM Q0.10 with smooth year",
        "fixed-basis RQ Q0.10 with natural-spline year",
        "fixed-basis RQ Q0.10 with linear year and foldwise rank reduction"
      ),
      status = c("rejected", "rejected", "selected"),
      reason = c(
        "fold optimizer failure and incomplete held-out predictions",
        "singular designs and gross held-out Q0.10 miscalibration",
        "numerically valid held-out predictions; calibration must still be reported"
      ),
      selection_used_flower_colour = FALSE,
      stringsAsFactors = FALSE
    ),
    complete = complete
  )
}

regional_slope_rows <- function(fit, exposure, regions, model_name) {
  beta <- stats::coef(fit)
  V <- stats::vcov(fit)
  out <- list()
  for (i in seq_along(regions)) {
    reg <- regions[i]
    weights <- setNames(numeric(length(beta)), names(beta))
    if (exposure %in% names(weights)) weights[exposure] <- 1
    interaction <- paste0("region", reg, ":", exposure)
    reverse_interaction <- paste0(exposure, ":region", reg)
    if (reg != regions[1]) {
      if (interaction %in% names(weights)) weights[interaction] <- 1
      if (reverse_interaction %in% names(weights)) weights[reverse_interaction] <- 1
    }
    estimate <- sum(weights * beta)
    se <- sqrt(drop(t(weights) %*% V %*% weights))
    out[[i]] <- data.frame(
      model = model_name, region = reg, exposure = exposure,
      estimate = estimate, se = se,
      lower_95 = estimate - stats::qnorm(0.975) * se,
      upper_95 = estimate + stats::qnorm(0.975) * se,
      statistic = estimate / se,
      p_value = 2 * stats::pnorm(-abs(estimate / se)), stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

fit_joint_horticultural_signature <- function(data, exposure = "z_H",
                                               joint_col = "joint_colour90_early10",
                                               support_col = "joint_reference_common_support",
                                               k_space = 40) {
  require_packages("mgcv")
  required <- c(joint_col, exposure, "region", "x_km", "y_km", support_col)
  d <- data[stats::complete.cases(data[required]) & data[[support_col]], , drop = FALSE]
  if (nrow(d) < 100 || sum(d[[joint_col]]) < 5) {
    return(list(
      slopes = data.frame(), fisher = data.frame(), magnitude = data.frame(),
      diagnostics = data.frame(
        n = nrow(d), joint_events = sum(d[[joint_col]]),
        status = "insufficient_joint_events_for_model", stringsAsFactors = FALSE
      )
    ))
  }
  d[[joint_col]] <- as.integer(d[[joint_col]])
  k_use <- max(10L, min(as.integer(k_space), floor(nrow(d) / 20)))
  form_landscape <- stats::as.formula(
    paste(joint_col, "~ region +", exposure, "+ region:", exposure)
  )
  form_spatial <- stats::as.formula(paste(
    joint_col, "~ region +", exposure, "+ region:", exposure,
    sprintf("+ s(x_km, y_km, k = %d, bs = 'tp')", k_use)
  ))
  warnings <- character()
  fit_landscape <- withCallingHandlers(
    stats::glm(form_landscape, data = d, family = stats::binomial()),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  fit_spatial <- withCallingHandlers(
    mgcv::gam(form_spatial, data = d, family = stats::binomial(), method = "REML"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  slopes <- rbind(
    regional_slope_rows(fit_landscape, exposure, levels(d$region), "landscape"),
    regional_slope_rows(fit_spatial, exposure, levels(d$region), "space_adjusted")
  )

  fisher <- do.call(rbind, lapply(levels(d$region), function(reg) {
    x <- d[d$region == reg, , drop = FALSE]
    q <- stats::quantile(x[[exposure]], c(0.25, 0.75), na.rm = TRUE, type = 8)
    x <- x[x[[exposure]] <= q[1] | x[[exposure]] >= q[2], , drop = FALSE]
    x$high <- x[[exposure]] >= q[2]
    tab <- table(factor(x$high, levels = c(FALSE, TRUE)),
                 factor(x[[joint_col]], levels = c(0, 1)))
    ft <- stats::fisher.test(tab)
    data.frame(
      region = reg, exposure = exposure, n = nrow(x),
      low_events = tab[1, 2], low_total = sum(tab[1, ]),
      high_events = tab[2, 2], high_total = sum(tab[2, ]),
      odds_ratio = unname(ft$estimate), lower_95 = ft$conf.int[1],
      upper_95 = ft$conf.int[2], p_value = ft$p.value,
      contrast = "top_vs_bottom_exposure_quartile", stringsAsFactors = FALSE
    )
  }))

  diagnostics <- data.frame(
    n = nrow(d), joint_events = sum(d[[joint_col]]),
    event_rate = mean(d[[joint_col]]),
    warnings = paste(unique(warnings), collapse = " | "),
    status = if (length(warnings)) "fit_with_warning" else "fit_ok",
    stringsAsFactors = FALSE
  )
  list(slopes = slopes, fisher = fisher, diagnostics = diagnostics)
}

attach_image_quality_controls <- function(data, raw_colour_csv = "") {
  if (!nzchar(raw_colour_csv) || !file.exists(raw_colour_csv)) return(data)
  raw <- utils::read.csv(raw_colour_csv, check.names = TRUE, stringsAsFactors = FALSE)
  id <- if ("observation_id" %in% names(data) && "observation_id" %in% names(raw)) {
    "observation_id"
  } else if ("sample_id" %in% names(data) && "sample_id" %in% names(raw)) {
    "sample_id"
  } else {
    stop("No shared observation identifier for image-quality controls.", call. = FALSE)
  }
  wanted <- intersect(c(
    "image_width", "image_height", "visible_pixels", "mask_pixels",
    "mask_fraction_visible", "mask_component_count", "exposure_filtered_fraction",
    "possible_overexposure"
  ), names(raw))
  key <- match(as.character(data[[id]]), as.character(raw[[id]]))
  for (column in wanted) data[[column]] <- raw[[column]][key]
  numeric_columns <- setdiff(wanted, "possible_overexposure")
  for (column in numeric_columns) data[[column]] <- suppressWarnings(as.numeric(data[[column]]))
  if (all(c("image_width", "image_height") %in% names(data))) {
    data$image_pixel_area_log <- log1p(data$image_width * data$image_height)
  }
  if ("visible_pixels" %in% names(data)) {
    data$visible_pixels_log <- log1p(pmax(data$visible_pixels, 0))
  }
  if ("mask_pixels" %in% names(data)) {
    data$mask_pixels_log <- log1p(pmax(data$mask_pixels, 0))
  }
  if ("possible_overexposure" %in% names(data)) {
    data$possible_overexposure_flag <- as.integer(as_flag(data$possible_overexposure))
  }
  data
}

fit_human_colour_coherence <- function(data, exposure = "z_H", k_space = 40) {
  require_packages("mgcv")
  outcomes <- intersect(c("colour_L", "colour_C", "colour_b"), names(data))
  outcomes <- outcomes[vapply(data[outcomes], is_available_axis, logical(1))]
  if (!length(outcomes)) return(data.frame())
  rows <- list()
  for (i in seq_along(outcomes)) {
    outcome <- outcomes[i]
    d <- data[stats::complete.cases(data[c(outcome, exposure, "region", "x_km", "y_km")]), ]
    d$negative_control_z <- safe_z(d[[outcome]])
    k_use <- max(10L, min(as.integer(k_space), floor(nrow(d) / 20)))
    form <- gam_formula_v3(
      "negative_control_z", c("region", exposure, paste0("region:", exposure)),
      spatial = TRUE, k_space = k_use
    )
    fit <- mgcv::gam(form, data = d, method = "REML")
    slopes <- regional_slope_rows(
      fit, exposure, levels(d$region), paste0("colour_coherence_", outcome)
    )
    slopes$outcome <- outcome
    slopes$interpretation <- "secondary colour dimension; not an image-quality negative control"
    rows[[i]] <- slopes
  }
  do.call(rbind, rows)
}

fit_human_image_diagnostics <- function(data, exposure = "z_H", k_space = 40) {
  require_packages("mgcv")
  outcomes <- intersect(c(
    "image_pixel_area_log", "visible_pixels_log", "mask_pixels_log",
    "mask_fraction_visible", "mask_component_count", "exposure_filtered_fraction"
  ), names(data))
  outcomes <- outcomes[vapply(data[outcomes], is_available_axis, logical(1))]
  rows <- list()
  row_i <- 0L
  for (outcome in outcomes) {
    d <- data[stats::complete.cases(data[c(outcome, exposure, "region", "x_km", "y_km")]), ]
    d$image_diagnostic_z <- safe_z(d[[outcome]])
    k_use <- max(10L, min(as.integer(k_space), floor(nrow(d) / 20)))
    form <- gam_formula_v3(
      "image_diagnostic_z", c("region", exposure, paste0("region:", exposure)),
      spatial = TRUE, k_space = k_use
    )
    fit <- mgcv::gam(form, data = d, method = "REML")
    slopes <- regional_slope_rows(
      fit, exposure, levels(d$region), paste0("image_diagnostic_", outcome)
    )
    slopes$outcome <- outcome
    slopes$control_class <- if (outcome == "image_pixel_area_log") {
      "response_independent_image_property"
    } else "extraction_or_composition_diagnostic"
    row_i <- row_i + 1L
    rows[[row_i]] <- slopes
  }
  if ("possible_overexposure_flag" %in% names(data) &&
      length(unique(stats::na.omit(data$possible_overexposure_flag))) > 1) {
    outcome <- "possible_overexposure_flag"
    d <- data[stats::complete.cases(data[c(outcome, exposure, "region", "x_km", "y_km")]), ]
    k_use <- max(10L, min(as.integer(k_space), floor(nrow(d) / 20)))
    form <- gam_formula_v3(
      outcome, c("region", exposure, paste0("region:", exposure)),
      spatial = TRUE, k_space = k_use
    )
    fit <- mgcv::gam(form, data = d, family = stats::binomial(), method = "REML")
    slopes <- regional_slope_rows(
      fit, exposure, levels(d$region), paste0("image_diagnostic_", outcome)
    )
    slopes$outcome <- outcome
    slopes$control_class <- "response_dependent_automated_warning_sensitivity"
    row_i <- row_i + 1L
    rows[[row_i]] <- slopes
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

numeric_collinearity_audit <- function(data, terms, design = "unspecified") {
  terms <- unique(terms[terms %in% names(data)])
  terms <- terms[vapply(data[terms], is_available_axis, logical(1))]
  if (length(terms) < 2L) {
    return(list(vif = data.frame(), correlations = data.frame(),
                condition = data.frame()))
  }
  d <- data[stats::complete.cases(data[terms]), terms, drop = FALSE]
  vif <- do.call(rbind, lapply(terms, function(term) {
    others <- setdiff(terms, term)
    fit <- stats::lm(stats::reformulate(others, response = term), data = d)
    r2 <- summary(fit)$r.squared
    data.frame(
      design = design, term = term, n = nrow(d), R2_from_other_predictors = r2,
      VIF = 1 / pmax(1 - r2, 1e-8), stringsAsFactors = FALSE
    )
  }))
  correlation_matrix <- stats::cor(d)
  index <- which(upper.tri(correlation_matrix), arr.ind = TRUE)
  correlations <- data.frame(
    design = design,
    term_1 = rownames(correlation_matrix)[index[, 1L]],
    term_2 = colnames(correlation_matrix)[index[, 2L]],
    correlation = correlation_matrix[index], stringsAsFactors = FALSE
  )
  scaled <- scale(as.matrix(d))
  condition <- data.frame(
    design = design, n = nrow(d), p = ncol(d),
    condition_number = kappa(scaled, exact = TRUE),
    interpretation = paste(
      "diagnostic only: correlated ecological predictors are retained when",
      "they represent distinct prespecified processes; uncertainty and held-out",
      "performance, rather than a VIF deletion rule, bound the claim"
    ), stringsAsFactors = FALSE
  )
  list(vif = vif, correlations = correlations, condition = condition)
}

primary_mesh_boundary_flag <- function(longitude, latitude) {
  longitude <- as.numeric(longitude)
  latitude <- as.numeric(latitude)
  lat_primary <- floor(latitude * 1.5) / 1.5
  lon_primary <- floor(longitude)
  row_1km <- pmin(79L, pmax(0L, floor((latitude - lat_primary) * 120)))
  col_1km <- pmin(79L, pmax(0L, floor((longitude - lon_primary) * 80)))
  row_1km %in% c(0L, 79L) | col_1km %in% c(0L, 79L)
}

linear_coefficient_rows <- function(fit, terms, outcome, model, region,
                                    tau = NA_real_) {
  tab <- summary(fit)$p.table
  terms <- intersect(terms, rownames(tab))
  if (!length(terms)) return(data.frame())
  data.frame(
    outcome = outcome, model = model, region = region, tau = tau,
    term = terms, estimate = tab[terms, 1L], se = tab[terms, 2L],
    lower_95 = tab[terms, 1L] - stats::qnorm(0.975) * tab[terms, 2L],
    upper_95 = tab[terms, 1L] + stats::qnorm(0.975) * tab[terms, 2L],
    statistic = tab[terms, 3L], p_value = tab[terms, 4L],
    stringsAsFactors = FALSE, row.names = NULL
  )
}

fit_horticultural_interface_convergence <- function(data, env_terms, k_space = 40) {
  require_packages(c("mgcv", "qgam"))
  if (!all(c("z_H", "z_R") %in% names(data)) ||
      !is_available_axis(data$z_H) || !is_available_axis(data$z_R)) {
    return(list(
      status = data.frame(
        status = "not_available", reason = "both non-constant H and R are required",
        stringsAsFactors = FALSE
      ), coefficients = data.frame(), heldout = data.frame(),
      warnings = data.frame(), vif = data.frame(), correlations = data.frame(),
      condition = data.frame(), gates = data.frame(), boundary_audit = data.frame()
    ))
  }
  data$z_H_R <- data$z_H * data$z_R
  exposure_terms <- c("z_H", "z_R", "z_H_R")
  base_colour <- unique(c(env_terms, "Bombus_W"))
  date_terms <- intersect(c("DOY_z", "DOY_z2", "DOY_z3", "year_z"), names(data))
  # Long-term GDD (N) and Temperature_PC1 encode almost the same thermal
  # construct. In the phenology equation N substitutes for the mean-temperature
  # axis; both are still exposed in the cross-model correlation audit. This is
  # a process-based basis choice, not post-hoc VIF deletion of Bombus or R.
  phenology_environment <- env_terms
  if ("z_N" %in% names(data) && is_available_axis(data$z_N)) {
    phenology_environment <- setdiff(phenology_environment, "env_Temperature_PC1")
  }
  base_phenology <- unique(c(
    phenology_environment,
    if ("z_N" %in% names(data) && is_available_axis(data$z_N)) "z_N",
    if ("year_z" %in% names(data)) "year_z"
  ))
  audit_colour <- numeric_collinearity_audit(
    data, c(env_terms, "Bombus_W", "z_H", "z_R", "z_H_R"),
    design = "raw_colour"
  )
  audit_phenology <- numeric_collinearity_audit(
    data, c(base_phenology, "z_H", "z_R", "z_H_R"),
    design = "raw_DOY"
  )
  audit_all <- numeric_collinearity_audit(
    data, c(env_terms, "Bombus_W", "z_H", "z_R", "z_H_R", "z_N", "z_A"),
    design = "cross_model_pairwise_only"
  )
  audit <- list(
    vif = rbind(
      audit_colour$vif, audit_phenology$vif,
      if ("z_A" %in% names(data) && is_available_axis(data$z_A)) {
        numeric_collinearity_audit(
          data, c(env_terms, "Bombus_W", "z_H", "z_R", "z_H_R", "z_A"),
          design = "raw_colour_access_sensitivity"
        )$vif
      } else data.frame()
    ),
    correlations = audit_all$correlations,
    condition = rbind(
      audit_colour$condition, audit_phenology$condition,
      if ("z_A" %in% names(data) && is_available_axis(data$z_A)) {
        numeric_collinearity_audit(
          data, c(env_terms, "Bombus_W", "z_H", "z_R", "z_H_R", "z_A"),
          design = "raw_colour_access_sensitivity"
        )$condition
      } else data.frame()
    )
  )
  data$R_primary_mesh_boundary <- primary_mesh_boundary_flag(
    data$longitude, data$latitude
  )
  boundary_audit <- do.call(rbind, lapply(levels(data$region), function(reg) {
    idx <- data$region == reg
    data.frame(
      region = reg, n = sum(idx), n_primary_mesh_boundary = sum(
        data$R_primary_mesh_boundary[idx], na.rm = TRUE
      ),
      fraction_primary_mesh_boundary = mean(
        data$R_primary_mesh_boundary[idx], na.rm = TRUE
      ),
      interpretation = "flagged observations are retained in the primary model and excluded only in sensitivity",
      stringsAsFactors = FALSE
    )
  }))
  data$east_z_H <- as.integer(data$region == "East") * data$z_H
  data$east_z_R <- as.integer(data$region == "East") * data$z_R
  data$east_z_H_R <- as.integer(data$region == "East") * data$z_H_R
  coefficient_rows <- heldout_rows <- warning_rows <- list()
  coefficient_i <- heldout_i <- warning_i <- 0L

  pooled_base <- unique(c("region", base_colour))
  pooled_interactions <- c("east_z_H", "east_z_R", "east_z_H_R")
  pooled_required <- unique(c(
    "response", "spatial_fold", "x_km", "y_km", pooled_base,
    exposure_terms, pooled_interactions
  ))
  pooled <- data[stats::complete.cases(data[pooled_required]), , drop = FALSE]
  if (nrow(pooled) >= 300L) {
    k_use <- max(10L, min(as.integer(k_space), floor(nrow(pooled) / 20)))
    form <- gam_formula_v3(
      "response", c(pooled_base, exposure_terms, pooled_interactions),
      spatial = TRUE, k_space = k_use
    )
    fit <- mgcv::gam(form, data = pooled, method = "REML")
    coefficient_i <- coefficient_i + 1L
    coefficient_rows[[coefficient_i]] <- linear_coefficient_rows(
      fit, c(exposure_terms, pooled_interactions), "response",
      "pooled_region_interaction", "Pooled"
    )
    cv <- crossfit_incremental_gam(
      pooled, "response", c(pooled_base, exposure_terms), pooled_interactions,
      spatial = TRUE, k_space = k_use
    )
    heldout_i <- heldout_i + 1L
    heldout_rows[[heldout_i]] <- cbind(
      outcome = "response", model = "pooled_region_interaction_increment",
      region = "Pooled", added_terms = paste(pooled_interactions, collapse = "+"),
      cv$metrics, stringsAsFactors = FALSE
    )
  }
  for (reg in levels(data$region)) {
    regional <- data[data$region == reg, , drop = FALSE]
    required <- unique(c(
      "response", "DOY", "spatial_fold", "x_km", "y_km",
      base_colour, base_phenology, date_terms, exposure_terms
    ))
    regional <- regional[stats::complete.cases(regional[required]), , drop = FALSE]
    if (nrow(regional) < 150L) next
    k_use <- max(10L, min(as.integer(k_space), floor(nrow(regional) / 20)))

    model_specs <- list(
      raw_colour = list(outcome = "response", base = base_colour),
      raw_colour_date_adjusted = list(
        outcome = "response", base = unique(c(base_colour, date_terms))
      ),
      raw_DOY = list(outcome = "DOY", base = base_phenology)
    )
    if ("z_A" %in% names(regional) && is_available_axis(regional$z_A)) {
      model_specs$raw_colour_access_adjusted <- list(
        outcome = "response", base = unique(c(base_colour, "z_A"))
      )
    }
    for (model_name in names(model_specs)) {
      spec <- model_specs[[model_name]]
      form <- gam_formula_v3(
        spec$outcome, c(spec$base, exposure_terms), spatial = TRUE, k_space = k_use
      )
      warnings <- character()
      fit <- withCallingHandlers(
        mgcv::gam(form, data = regional, method = "REML"),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      coefficient_i <- coefficient_i + 1L
      coefficient_rows[[coefficient_i]] <- linear_coefficient_rows(
        fit, exposure_terms, spec$outcome, model_name, reg
      )
      cv_full <- crossfit_incremental_gam(
        regional, spec$outcome, spec$base, exposure_terms,
        spatial = TRUE, k_space = k_use
      )
      heldout_i <- heldout_i + 1L
      heldout_rows[[heldout_i]] <- cbind(
        outcome = spec$outcome, model = model_name, region = reg,
        added_terms = paste(exposure_terms, collapse = "+"),
        cv_full$metrics, stringsAsFactors = FALSE
      )
      cv_interaction <- crossfit_incremental_gam(
        regional, spec$outcome, c(spec$base, "z_H", "z_R"), "z_H_R",
        spatial = TRUE, k_space = k_use
      )
      heldout_i <- heldout_i + 1L
      heldout_rows[[heldout_i]] <- cbind(
        outcome = spec$outcome, model = paste0(model_name, "_interaction_increment"),
        region = reg, added_terms = "z_H_R",
        cv_interaction$metrics, stringsAsFactors = FALSE
      )
      warning_i <- warning_i + 1L
      warning_rows[[warning_i]] <- data.frame(
        outcome = spec$outcome, model = model_name, region = reg,
        warnings = paste(unique(warnings), collapse = " | "), stringsAsFactors = FALSE
      )
    }

    boundary_sensitivity <- regional[!regional$R_primary_mesh_boundary, , drop = FALSE]
    if (nrow(boundary_sensitivity) >= 150L) {
      form <- gam_formula_v3(
        "response", c(base_colour, exposure_terms), spatial = TRUE,
        k_space = max(10L, min(k_use, floor(nrow(boundary_sensitivity) / 20)))
      )
      fit <- mgcv::gam(form, data = boundary_sensitivity, method = "REML")
      coefficient_i <- coefficient_i + 1L
      coefficient_rows[[coefficient_i]] <- linear_coefficient_rows(
        fit, exposure_terms, "response", "raw_colour_exclude_R_boundary", reg
      )
      cv <- crossfit_incremental_gam(
        boundary_sensitivity, "response", base_colour, exposure_terms,
        spatial = TRUE, k_space = k_use
      )
      heldout_i <- heldout_i + 1L
      heldout_rows[[heldout_i]] <- cbind(
        outcome = "response", model = "raw_colour_exclude_R_boundary",
        region = reg, added_terms = paste(exposure_terms, collapse = "+"),
        cv$metrics, stringsAsFactors = FALSE
      )
    }

    for (tau in c(0.5, 0.9)) {
      form <- gam_formula_v3(
        "response", c(base_colour, exposure_terms), spatial = TRUE, k_space = k_use
      )
      warnings <- character()
      fit <- withCallingHandlers(
        qgam::qgam(
          form, data = regional, qu = tau,
          argGam = list(method = "REML")
        ),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      coefficient_i <- coefficient_i + 1L
      coefficient_rows[[coefficient_i]] <- linear_coefficient_rows(
        fit, exposure_terms, "response", "raw_colour_quantile", reg, tau
      )
      warning_i <- warning_i + 1L
      warning_rows[[warning_i]] <- data.frame(
        outcome = "response", model = "raw_colour_quantile", region = reg,
        tau = tau, warnings = paste(unique(warnings), collapse = " | "),
        stringsAsFactors = FALSE
      )
    }

    joint_required <- c(
      "joint_colour90_early10", "joint_reference_common_support",
      base_colour, exposure_terms, "x_km", "y_km"
    )
    joint <- regional[
      stats::complete.cases(regional[joint_required]) &
        regional$joint_reference_common_support,
      , drop = FALSE
    ]
    joint_events <- sum(joint$joint_colour90_early10)
    if (nrow(joint) >= 150L && joint_events >= 15L) {
      joint$joint_colour90_early10 <- as.integer(joint$joint_colour90_early10)
      form <- gam_formula_v3(
        "joint_colour90_early10", c(base_colour, exposure_terms),
        spatial = TRUE, k_space = max(10L, min(k_use, floor(nrow(joint) / 20)))
      )
      warnings <- character()
      fit <- withCallingHandlers(
        mgcv::gam(
          form, data = joint, family = stats::binomial(), method = "REML"
        ),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      coefficient_i <- coefficient_i + 1L
      coefficient_rows[[coefficient_i]] <- linear_coefficient_rows(
        fit, exposure_terms, "joint_colour90_early10", "joint_event", reg
      )
      warning_i <- warning_i + 1L
      warning_rows[[warning_i]] <- data.frame(
        outcome = "joint_colour90_early10", model = "joint_event", region = reg,
        joint_events = joint_events,
        warnings = paste(unique(warnings), collapse = " | "), stringsAsFactors = FALSE
      )
    } else {
      warning_i <- warning_i + 1L
      warning_rows[[warning_i]] <- data.frame(
        outcome = "joint_colour90_early10", model = "joint_event", region = reg,
        joint_events = joint_events,
        warnings = "not fitted: fewer than 15 joint events or 150 supported observations",
        stringsAsFactors = FALSE
      )
    }
  }
  coefficients <- if (length(coefficient_rows)) do.call(rbind, coefficient_rows) else data.frame()
  heldout <- if (length(heldout_rows)) do.call(rbind, heldout_rows) else data.frame()
  warnings <- if (length(warning_rows)) {
    fill_names <- unique(unlist(lapply(warning_rows, names)))
    do.call(rbind, lapply(warning_rows, function(x) {
      for (name in setdiff(fill_names, names(x))) x[[name]] <- NA
      x[fill_names]
    }))
  } else data.frame()

  get_coef <- function(reg, model, term, tau = NA_real_) {
    x <- coefficients[
      coefficients$region == reg & coefficients$model == model &
        coefficients$term == term &
        ((is.na(tau) & is.na(coefficients$tau)) | coefficients$tau == tau),
      , drop = FALSE
    ]
    if (nrow(x)) x[1L, , drop = FALSE] else data.frame()
  }
  get_delta <- function(reg, model) {
    x <- heldout[heldout$region == reg & heldout$model == model, , drop = FALSE]
    if (nrow(x)) x$delta_heldout_R2[1L] else NA_real_
  }
  west_h <- get_coef("West", "raw_colour", "z_H")
  west_r <- get_coef("West", "raw_colour", "z_R")
  west_hr <- get_coef("West", "raw_colour", "z_H_R")
  west_early_hr <- get_coef("West", "raw_DOY", "z_H_R")
  west_q50_hr <- get_coef("West", "raw_colour_quantile", "z_H_R", 0.5)
  west_q90_hr <- get_coef("West", "raw_colour_quantile", "z_H_R", 0.9)
  west_boundary_hr <- get_coef("West", "raw_colour_exclude_R_boundary", "z_H_R")
  west_access_hr <- get_coef("West", "raw_colour_access_adjusted", "z_H_R")
  pooled_east_difference <- get_coef(
    "Pooled", "pooled_region_interaction", "east_z_H_R"
  )
  gates <- data.frame(
    prediction = c(
      "West raw-colour H slope is positive",
      "West raw-colour R slope is positive",
      "West raw-colour H-by-R interaction is positive",
      "West H-R block improves spatially held-out raw-colour prediction",
      "West H-by-R interaction improves spatially held-out raw-colour prediction",
      "West H-by-R interaction shifts raw DOY earlier",
      "West Q0.90 H-by-R slope exceeds the Q0.50 slope",
      "West H-by-R interaction remains positive after excluding primary-mesh boundary cells",
      "West H-by-R interaction remains positive after major-road access adjustment",
      "Pooled East-minus-West H-by-R interaction is negative"
    ),
    observed = c(
      if (nrow(west_h)) west_h$estimate else NA_real_,
      if (nrow(west_r)) west_r$estimate else NA_real_,
      if (nrow(west_hr)) west_hr$estimate else NA_real_,
      get_delta("West", "raw_colour"),
      get_delta("West", "raw_colour_interaction_increment"),
      if (nrow(west_early_hr)) west_early_hr$estimate else NA_real_,
      if (nrow(west_q90_hr) && nrow(west_q50_hr)) {
        west_q90_hr$estimate - west_q50_hr$estimate
      } else NA_real_,
      if (nrow(west_boundary_hr)) west_boundary_hr$estimate else NA_real_,
      if (nrow(west_access_hr)) west_access_hr$estimate else NA_real_,
      if (nrow(pooled_east_difference)) pooled_east_difference$estimate else NA_real_
    ),
    expected_direction = c("positive", "positive", "positive", "positive",
                           "positive", "negative", "positive", "positive", "positive", "negative"),
    stringsAsFactors = FALSE
  )
  gates$status <- ifelse(
    !is.finite(gates$observed), "not_estimable",
    ifelse(
      (gates$expected_direction == "positive" & gates$observed > 0) |
        (gates$expected_direction == "negative" & gates$observed < 0),
      "direction_consistent", "direction_inconsistent"
    )
  )
  gates$interpretation <- paste(
    "directional diagnostic; no gate alone identifies horticultural provenance",
    "and statistical/predictive uncertainty remains decisive"
  )
  list(
    status = data.frame(status = "fitted", reason = "H and R available",
                        stringsAsFactors = FALSE),
    coefficients = coefficients, heldout = heldout, warnings = warnings,
    vif = audit$vif, correlations = audit$correlations,
    condition = audit$condition, gates = gates, boundary_audit = boundary_audit
  )
}

make_interface_evidence_rows <- function(interface) {
  if (!nrow(interface$coefficients)) {
    return(data.frame(
      domain = "Horticulture", criterion = "an independent human-forest interface axis R is available",
      status = "not_available", observed = interface$status$reason[1L],
      claim_effect = "multi-axis horticultural design", stringsAsFactors = FALSE
    ))
  }
  coef_row <- function(model, term) {
    x <- interface$coefficients[
      interface$coefficients$region == "West" &
        interface$coefficients$model == model & interface$coefficients$term == term &
        is.na(interface$coefficients$tau), , drop = FALSE
    ]
    if (nrow(x)) x[1L, , drop = FALSE] else data.frame()
  }
  heldout_delta <- function(model) {
    x <- interface$heldout[
      interface$heldout$region == "West" & interface$heldout$model == model,
      , drop = FALSE
    ]
    if (nrow(x)) x$delta_heldout_R2[1L] else NA_real_
  }
  hr <- coef_row("raw_colour", "z_H_R")
  early <- coef_row("raw_DOY", "z_H_R")
  boundary <- coef_row("raw_colour_exclude_R_boundary", "z_H_R")
  access <- coef_row("raw_colour_access_adjusted", "z_H_R")
  pooled_difference <- interface$coefficients[
    interface$coefficients$region == "Pooled" &
      interface$coefficients$model == "pooled_region_interaction" &
      interface$coefficients$term == "east_z_H_R", , drop = FALSE
  ]
  full_delta <- heldout_delta("raw_colour")
  interaction_delta <- heldout_delta("raw_colour_interaction_increment")
  boundary_delta <- heldout_delta("raw_colour_exclude_R_boundary")
  access_delta <- heldout_delta("raw_colour_access_adjusted")
  pooled_delta_row <- interface$heldout[
    interface$heldout$region == "Pooled" &
      interface$heldout$model == "pooled_region_interaction_increment",
    , drop = FALSE
  ]
  pooled_delta <- if (nrow(pooled_delta_row)) {
    pooled_delta_row$delta_heldout_R2[1L]
  } else NA_real_
  hr_support <- nrow(hr) && is.finite(hr$lower_95[1L]) && hr$lower_95[1L] > 0 &&
    is.finite(full_delta) && full_delta > 0 &&
    is.finite(interaction_delta) && interaction_delta > 0 &&
    nrow(boundary) && is.finite(boundary$lower_95[1L]) && boundary$lower_95[1L] > 0 &&
    is.finite(boundary_delta) && boundary_delta > 0
  access_available <- nrow(access) > 0L
  access_robust <- !access_available || (
    is.finite(access$lower_95[1L]) && access$lower_95[1L] > 0 &&
      is.finite(access_delta) && access_delta > 0
  )
  hr_support <- hr_support && access_robust
  early_support <- nrow(early) && is.finite(early$upper_95[1L]) && early$upper_95[1L] < 0
  region_specific <- nrow(pooled_difference) &&
    is.finite(pooled_difference$upper_95[1L]) && pooled_difference$upper_95[1L] < 0 &&
    is.finite(pooled_delta) && pooled_delta > 0
  max_vif <- if (nrow(interface$vif)) max(interface$vif$VIF, na.rm = TRUE) else NA_real_
  focal_vif <- interface$vif[interface$vif$term %in% c("z_H", "z_R", "z_H_R"), , drop = FALSE]
  max_focal_vif <- if (nrow(focal_vif)) max(focal_vif$VIF, na.rm = TRUE) else NA_real_
  data.frame(
    domain = rep("Horticulture", 5L),
    criterion = c(
      "an independent response-blind human-forest interface axis R is available",
      "West H-by-R raw-colour interaction is positive and improves spatially held-out prediction",
      "the pooled East-minus-West H-by-R contrast is negative",
      "West H-by-R interaction is associated with earlier raw observation DOY after natural phenology adjustment",
      "H, R, natural environment, and Bombus collinearity is quantified without automatic variable deletion"
    ),
    status = c(
      "design_pass",
      ifelse(hr_support, "pass", "fail_no_predictive_convergence"),
      ifelse(region_specific, "pass", "fail_or_inconclusive"),
      ifelse(early_support, "pass", "fail_or_inconclusive"),
      ifelse(is.finite(max_vif), "design_pass", "not_available")
    ),
    observed = c(
      "MLIT 2021 100-m forest-human adjacency aggregated to 1-km cells",
      if (nrow(hr)) paste(
        "coefficient", signif(hr$estimate[1L], 3), "CI",
        signif(hr$lower_95[1L], 3), "to", signif(hr$upper_95[1L], 3),
        "full heldout delta R2", signif(full_delta, 3),
        "interaction delta R2", signif(interaction_delta, 3),
        "boundary-excluded coefficient",
        if (nrow(boundary)) signif(boundary$estimate[1L], 3) else NA,
        "boundary-excluded heldout delta R2", signif(boundary_delta, 3),
        "access-adjusted coefficient",
        if (access_available) signif(access$estimate[1L], 3) else "not available",
        "access-adjusted heldout delta R2",
        if (access_available) signif(access_delta, 3) else "not available"
      ) else "not estimable",
      if (nrow(pooled_difference)) paste(
        "East-minus-West", signif(pooled_difference$estimate[1L], 3), "CI",
        signif(pooled_difference$lower_95[1L], 3), "to",
        signif(pooled_difference$upper_95[1L], 3),
        "heldout delta R2", signif(pooled_delta, 3)
      ) else "not estimable",
      if (nrow(early)) paste(
        "DOY coefficient", signif(early$estimate[1L], 3), "CI",
        signif(early$lower_95[1L], 3), "to", signif(early$upper_95[1L], 3)
      ) else "not estimable",
      paste(
        "maximum focal H/R/interaction VIF", signif(max_focal_vif, 3),
        "maximum full-design VIF", signif(max_vif, 3)
      )
    ),
    claim_effect = c(
      "separates propagule pressure H from establishment interface R",
      "multi-axis horticultural colour convergence",
      "formal regional specificity",
      "independent early-flowering consistency",
      "transparent shared-cause uncertainty"
    ),
    stringsAsFactors = FALSE
  )
}

block_bootstrap_tail_specificity <- function(data, env_terms, response = "response",
                                             exposure = "z_H", taus = c(0.5, 0.9),
                                             n_boot = 199L, seed = 42L,
                                             k_space = 40) {
  require_packages(c("mgcv", "quantreg"))
  if (length(taus) != 2L || !all(c(0.5, 0.9) %in% taus)) {
    stop("Tail-specificity contrast is prespecified as Q0.90 - Q0.50.")
  }
  required <- c(response, exposure, "region", "Bombus_W", env_terms,
                "x_km", "y_km", "spatial_block")
  d <- data[stats::complete.cases(data[required]), , drop = FALSE]
  fixed_formula <- stats::reformulate(
    c("region", env_terms, "Bombus_W", exposure, paste0("region:", exposure))
  )
  X_fixed <- stats::model.matrix(fixed_formula, data = d)
  k_use <- max(10L, min(as.integer(k_space), floor(nrow(d) / 20)))
  smooth_spec <- mgcv::s(x_km, y_km, k = k_use, bs = "tp")
  X_space <- mgcv::smoothCon(
    smooth_spec, data = d, absorb.cons = TRUE
  )[[1]]$X
  colnames(X_space) <- paste0("space_basis_", seq_len(ncol(X_space)))
  X <- cbind(X_fixed, X_space)
  y <- d[[response]]
  regions <- levels(d$region)

  regional_slope <- function(beta, reg) {
    value <- beta[exposure]
    interaction <- paste0("region", reg, ":", exposure)
    reverse <- paste0(exposure, ":region", reg)
    if (reg != regions[1]) {
      if (interaction %in% names(beta)) value <- value + beta[interaction]
      if (reverse %in% names(beta)) value <- value + beta[reverse]
    }
    unname(value)
  }
  fit_slopes <- function(index) {
    result <- matrix(NA_real_, nrow = length(regions), ncol = length(taus),
                     dimnames = list(regions, paste0("q", taus)))
    for (j in seq_along(taus)) {
      fit <- tryCatch(
        suppressWarnings(quantreg::rq.fit.fnb(X[index, , drop = FALSE], y[index], tau = taus[j])),
        error = function(e) NULL
      )
      if (is.null(fit)) next
      beta <- fit$coefficients
      names(beta) <- colnames(X)
      for (reg in regions) result[reg, j] <- regional_slope(beta, reg)
    }
    result
  }
  observed <- fit_slopes(seq_len(nrow(d)))
  blocks <- unique(d$spatial_block)
  set.seed(seed)
  bootstrap <- array(
    NA_real_, dim = c(n_boot, length(regions), length(taus)),
    dimnames = list(NULL, regions, paste0("q", taus))
  )
  for (b in seq_len(n_boot)) {
    sampled <- sample(blocks, length(blocks), replace = TRUE)
    index <- unlist(lapply(sampled, function(block) which(d$spatial_block == block)), use.names = FALSE)
    bootstrap[b, , ] <- fit_slopes(index)
  }
  rows <- list()
  for (i in seq_along(regions)) {
    reg <- regions[i]
    difference <- bootstrap[, i, which(taus == 0.9)] -
      bootstrap[, i, which(taus == 0.5)]
    difference <- difference[is.finite(difference)]
    observed_difference <- observed[i, which(taus == 0.9)] -
      observed[i, which(taus == 0.5)]
    rows[[i]] <- data.frame(
      region = reg, contrast = "Q0.90_H_slope_minus_Q0.50_H_slope",
      estimate = observed_difference,
      lower_95 = if (length(difference)) stats::quantile(difference, 0.025, type = 8) else NA_real_,
      upper_95 = if (length(difference)) stats::quantile(difference, 0.975, type = 8) else NA_real_,
      bootstrap_p_value = if (length(difference)) {
        2 * min(mean(difference <= 0), mean(difference >= 0))
      } else NA_real_,
      successful_bootstraps = length(difference), requested_bootstraps = n_boot,
      bootstrap_unit = "prespecified_50km_spatial_block",
      spatial_basis = paste0("thin_plate_basis_k", k_use), stringsAsFactors = FALSE
    )
  }
  slope_rows <- do.call(rbind, lapply(seq_along(regions), function(i) {
    do.call(rbind, lapply(seq_along(taus), function(j) data.frame(
      region = regions[i], tau = taus[j], estimate = observed[i, j],
      method = "linear_quantile_model_with_thin_plate_basis",
      stringsAsFactors = FALSE
    )))
  }))
  list(contrast = do.call(rbind, rows), slopes = slope_rows)
}

make_reviewer_evidence_matrix <- function(bombus_coefficients, bombus_heldout,
                                          bombus_matches, human_ladder,
                                          human_matching, human_decomposition,
                                          horticultural_slopes, joint_results,
                                          colour_coherence, tail_specificity,
                                          image_diagnostics) {
  get_bombus <- function(predictor, rung) {
    x <- bombus_coefficients[
      bombus_coefficients$predictor == predictor & bombus_coefficients$rung == rung,
      , drop = FALSE
    ]
    if (nrow(x)) x$estimate[1] else NA_real_
  }
  get_heldout <- function(predictor, rung) {
    x <- bombus_heldout[
      bombus_heldout$predictor == predictor & bombus_heldout$rung == rung,
      , drop = FALSE
    ]
    if (nrow(x)) x$delta_heldout_R2[1] else NA_real_
  }
  enm_landscape <- get_bombus("Bombus_W", "landscape")
  occurrence_predictors <- grep("^Bombus_W_occ_[0-9]+km$", unique(bombus_coefficients$predictor), value = TRUE)
  occurrence_radii <- suppressWarnings(as.numeric(sub("^Bombus_W_occ_([0-9]+)km$", "\\1", occurrence_predictors)))
  occurrence_primary <- if (length(occurrence_predictors) && any(is.finite(occurrence_radii))) {
    occurrence_predictors[which.max(occurrence_radii)]
  } else NA_character_
  occ_landscape <- if (!is.na(occurrence_primary)) get_bombus(occurrence_primary, "landscape") else NA_real_
  occ_conditional <- if (!is.na(occurrence_primary)) {
    get_bombus(occurrence_primary, "environment_space_adjusted")
  } else NA_real_
  match_row <- if (nrow(bombus_matches) && !is.na(occurrence_primary)) {
    bombus_matches[bombus_matches$predictor == occurrence_primary, , drop = FALSE]
  } else data.frame()
  match_ok <- nrow(match_row) && match_row$n_pairs >= 30 && isTRUE(match_row$acceptable_balance)
  match_direction <- if (nrow(match_row)) sign(match_row$mean_colour_difference[1]) else NA_real_
  composition_concordance <- all(is.finite(c(enm_landscape, occ_landscape))) &&
    sign(enm_landscape) == sign(occ_landscape)
  positive_widespread_prediction <- all(is.finite(c(enm_landscape, occ_landscape))) &&
    enm_landscape > 0 && occ_landscape > 0
  density_predictors <- grep("^Bombus_W_occ_density_[0-9]+km$", unique(bombus_coefficients$predictor), value = TRUE)
  density_landscape <- vapply(
    density_predictors, get_bombus, numeric(1), rung = "landscape"
  )
  proxy_family_concordance <- composition_concordance && length(density_landscape) &&
    all(is.finite(density_landscape)) && all(sign(density_landscape) == sign(enm_landscape))
  local_concordance <- match_ok && is.finite(occ_conditional) &&
    match_direction == sign(occ_conditional)
  occ_heldout_delta <- if (!is.na(occurrence_primary)) {
    get_heldout(occurrence_primary, "environment_space_adjusted")
  } else NA_real_
  conditional_predictive_support <- is.finite(occ_conditional) &&
    is.finite(occ_heldout_delta) && occ_conditional > 0 && occ_heldout_delta > 0
  alpine_enm <- get_bombus("Bombus_A", "landscape")
  alpine_density <- get_bombus("Bombus_A_occ_density_100km", "landscape")
  total_density <- get_bombus("Bombus_total_occ_density_100km", "landscape")
  total_density_conditional <- get_bombus(
    "Bombus_total_occ_density_100km", "environment_space_adjusted"
  )
  total_density_heldout <- get_heldout(
    "Bombus_total_occ_density_100km", "environment_space_adjusted"
  )
  seasonal_density <- get_bombus(
    "Bombus_total_seasonal_density_100km_30d", "environment_space_adjusted"
  )
  seasonal_density_heldout <- get_heldout(
    "Bombus_total_seasonal_density_100km_30d", "environment_space_adjusted"
  )
  temporal_overlap <- get_bombus(
    "Bombus_temporal_overlap_100km_30d", "environment_space_adjusted"
  )
  temporal_overlap_heldout <- get_heldout(
    "Bombus_temporal_overlap_100km_30d", "environment_space_adjusted"
  )
  seasonal_density_support <- all(is.finite(c(
    seasonal_density, seasonal_density_heldout
  ))) && seasonal_density > 0 && seasonal_density_heldout > 0
  temporal_overlap_support <- all(is.finite(c(
    temporal_overlap, temporal_overlap_heldout
  ))) && temporal_overlap > 0 && temporal_overlap_heldout > 0
  temporal_match <- bombus_matches[
    bombus_matches$predictor == "Bombus_temporal_overlap_100km_30d",
    , drop = FALSE
  ]
  temporal_match_ok <- nrow(temporal_match) && temporal_match$n_pairs[1] >= 30 &&
    isTRUE(temporal_match$acceptable_balance[1]) &&
    is.finite(temporal_match$lower_95[1]) && temporal_match$lower_95[1] > 0

  human_conditional <- human_ladder$coefficients[
    human_ladder$coefficients$rung == "environment_space_adjusted", , drop = FALSE
  ]
  human_heldout <- human_ladder$heldout_metrics[
    human_ladder$heldout_metrics$rung == "environment_space_adjusted", , drop = FALSE
  ]
  human_predictive_support <- nrow(human_conditional) && nrow(human_heldout) &&
    human_conditional$estimate[1] > 0 && human_heldout$delta_heldout_R2[1] > 0
  human_match <- human_matching$summary
  human_match_ok <- nrow(human_match) && human_match$n_pairs[1] >= 30 &&
    isTRUE(human_match$acceptable_balance[1])
  human_decomp <- human_decomposition$summary

  h90 <- horticultural_slopes[
    horticultural_slopes$tau == 0.9 & horticultural_slopes$region == "West",
    , drop = FALSE
  ]
  h50 <- horticultural_slopes[
    horticultural_slopes$tau == 0.5 & horticultural_slopes$region == "West",
    , drop = FALSE
  ]
  tail_row <- tail_specificity[tail_specificity$region == "West", , drop = FALSE]
  tail_specific <- nrow(tail_row) && is.finite(tail_row$lower_95[1]) && tail_row$lower_95[1] > 0
  joint_west <- joint_results$fisher[joint_results$fisher$region == "West", , drop = FALSE]
  joint_supported <- nrow(joint_west) && is.finite(joint_west$lower_95[1]) && joint_west$lower_95[1] > 1
  l_west <- colour_coherence[
    colour_coherence$region == "West" & colour_coherence$outcome == "colour_L", , drop = FALSE
  ]
  c_west <- colour_coherence[
    colour_coherence$region == "West" & colour_coherence$outcome == "colour_C", , drop = FALSE
  ]
  coherent_dark_pink <- nrow(h90) && nrow(l_west) && nrow(c_west) &&
    h90$lower_95[1] > 0 && l_west$upper_95[1] < 0 && c_west$lower_95[1] > 0
  independent_controls <- if (nrow(image_diagnostics)) {
    image_diagnostics[
      image_diagnostics$control_class == "response_independent_image_property" &
        image_diagnostics$region == "West",
      , drop = FALSE
    ]
  } else data.frame()
  independent_controls_east <- if (nrow(image_diagnostics)) {
    image_diagnostics[
      image_diagnostics$control_class == "response_independent_image_property" &
        image_diagnostics$region == "East",
      , drop = FALSE
    ]
  } else data.frame()
  image_diagnostic_flag <- nrow(independent_controls) && any(
    independent_controls$lower_95 > 0 | independent_controls$upper_95 < 0,
    na.rm = TRUE
  )
  image_diagnostic_status <- if (!nrow(independent_controls)) {
    "not_available"
  } else if (image_diagnostic_flag) "fail" else "pass"

  data.frame(
    domain = c(rep("Bombus", 11), rep("Horticulture", 7)),
    criterion = c(
      "ENMeval suitability and target-group occurrence composition have the same landscape direction",
      "the shared landscape direction matches the prespecified positive pinkness prediction",
      "suitability, occurrence composition, and absolute occurrence density all agree",
      "the environment-space occurrence coefficient is positive and improves held-out prediction",
      "at least 30 balanced occurrence-proxy pairs",
      "all-five-species total occurrence density is positive and improves held-out prediction",
      "seasonally weighted total density is positive and improves held-out prediction",
      "date-normalized temporal overlap is positive and improves held-out prediction",
      "at least 30 balanced temporal-overlap pairs have a positive colour contrast",
      "the alpine ENMeval and absolute-density blocks are evaluated separately",
      "environment-space conditional estimate is not substituted for shared geography",
      "H has a positive environment-space coefficient and improves held-out prediction",
      "at least 30 response-blind, environmentally balanced local H pairs",
      "West Q0.90 H slope exceeds Q0.50 H slope in spatial-block bootstrap",
      "West human gradient forms a coherent pinker, darker, more chromatic colour syndrome",
      "West joint colour-Q0.90 and early-Q0.10 odds ratio excludes one",
      "West response-independent image-property diagnostics show no corresponding association",
      "colour dimensions are treated as correlated phenotype, not independent replication"
    ),
    status = c(
      ifelse(composition_concordance, "descriptive_concordance", "fail"),
      ifelse(positive_widespread_prediction, "pass", "fail_opposite_prediction"),
      ifelse(proxy_family_concordance, "pass", "fail_proxy_definition_sensitive"),
      ifelse(conditional_predictive_support, "pass", "fail_no_predictive_gain"),
      ifelse(match_ok, "pass", "not_identifiable"),
      ifelse(
        all(is.finite(c(total_density_conditional, total_density_heldout))) &&
          total_density_conditional > 0 && total_density_heldout > 0,
        "sensitivity_pass", "fail_no_predictive_gain"
      ),
      ifelse(seasonal_density_support, "sensitivity_pass", "fail_no_predictive_gain"),
      ifelse(temporal_overlap_support, "sensitivity_pass", "fail_no_predictive_gain"),
      ifelse(temporal_match_ok, "sensitivity_pass", "not_identifiable"),
      ifelse(all(is.finite(c(alpine_enm, alpine_density))), "design_pass", "not_available"),
      "design_pass",
      ifelse(human_predictive_support, "pass", "fail_no_predictive_gain"),
      ifelse(human_match_ok, "pass", "not_identifiable"),
      ifelse(tail_specific, "pass", "fail"),
      ifelse(coherent_dark_pink, "descriptive_pass", "fail"),
      ifelse(joint_supported, "pass", "fail_or_underpowered"),
      image_diagnostic_status,
      "design_pass"
    ),
    observed = c(
      paste("ENM", signif(enm_landscape, 3), "occurrence", signif(occ_landscape, 3)),
      paste("expected positive; observed ENM", signif(enm_landscape, 3),
            "occurrence", signif(occ_landscape, 3)),
      paste("density slopes", paste(signif(density_landscape, 3), collapse = ";")),
      paste("conditional", signif(occ_conditional, 3), "heldout delta R2",
            signif(occ_heldout_delta, 3)),
      if (nrow(match_row)) paste("pairs", match_row$n_pairs[1], "max|SMD|", signif(match_row$max_abs_environment_SMD[1], 3)) else "no pairs",
      paste(
        "landscape", signif(total_density, 3), "conditional",
        signif(total_density_conditional, 3), "heldout delta R2",
        signif(total_density_heldout, 3)
      ),
      paste("conditional", signif(seasonal_density, 3), "heldout delta R2",
            signif(seasonal_density_heldout, 3)),
      paste("conditional", signif(temporal_overlap, 3), "heldout delta R2",
            signif(temporal_overlap_heldout, 3)),
      if (nrow(temporal_match)) paste(
        "pairs", temporal_match$n_pairs[1], "max|SMD|",
        signif(temporal_match$max_abs_environment_SMD[1], 3), "colour difference",
        signif(temporal_match$mean_colour_difference[1], 3), "CI",
        signif(temporal_match$lower_95[1], 3), "to",
        signif(temporal_match$upper_95[1], 3)
      ) else "no pairs",
      paste("alpine ENM", signif(alpine_enm, 3), "alpine density", signif(alpine_density, 3)),
      "shared and local components exported separately",
      if (nrow(human_conditional) && nrow(human_heldout)) paste(
        "conditional", signif(human_conditional$estimate[1], 3),
        "heldout delta R2", signif(human_heldout$delta_heldout_R2[1], 3),
        "H local SD fraction", if (nrow(human_decomp)) signif(human_decomp$local_SD_fraction[1], 3) else NA
      ) else "not available",
      if (nrow(human_match)) paste(
        "pairs", human_match$n_pairs[1], "max|SMD|",
        signif(human_match$max_abs_environment_SMD[1], 3)
      ) else "no pairs",
      if (nrow(tail_row)) paste("difference", signif(tail_row$estimate[1], 3), "CI", signif(tail_row$lower_95[1], 3), "to", signif(tail_row$upper_95[1], 3)) else "not available",
      if (nrow(h90) && nrow(l_west) && nrow(c_west)) paste("a*", signif(h90$estimate[1], 3), "L*", signif(l_west$estimate[1], 3), "C*", signif(c_west$estimate[1], 3)) else "not available",
      if (nrow(joint_west)) paste("OR", signif(joint_west$odds_ratio[1], 3), "CI", signif(joint_west$lower_95[1], 3), "to", signif(joint_west$upper_95[1], 3)) else "not available",
      if (nrow(independent_controls)) paste(
        "West non-null", sum(independent_controls$lower_95 > 0 | independent_controls$upper_95 < 0, na.rm = TRUE),
        "East non-null", sum(independent_controls_east$lower_95 > 0 | independent_controls_east$upper_95 < 0, na.rm = TRUE)
      ) else "not available",
      "a*, L*, b*, and C* are related measurements of the same extracted colour"
    ),
    claim_effect = c(
      "descriptive broad Bombus co-geography", "directional Bombus hypothesis",
      "robust Bombus triangulation", "conditional Bombus evidence",
      "local Bombus support", "total Bombus availability sensitivity",
      "seasonal-density sensitivity", "seasonally explicit Bombus sensitivity",
      "local seasonally explicit Bombus sensitivity",
      "distribution-type comparison",
      "prevents over-adjustment interpretation", "conditional human association",
      "local human association", "tail-specific human association",
      "phenotypic colour syndrome only", "early-colour horticultural consistency",
      "no evidence for a simple West image-property artefact",
      "prevents pseudo-replication of colour evidence"
    ),
    stringsAsFactors = FALSE
  )
}

run_ecological_mechanism_v3 <- function(analysis_data_csv, occurrence_dir, output_dir,
                                        raw_colour_csv = "",
                                        radii_km = c(50, 100),
                                        occurrence_support = 5,
                                        reference_fraction = 0.30,
                                        match_radius_km = 100,
                                        match_env_caliper = 1.5,
                                        match_propensity_caliper_sd = 0.2,
                                        tail_bootstraps = 199L,
                                        reference_k_space = 15L,
                                        random_seed = 42L,
                                        k_space = 40) {
  require_packages(c("sf", "mgcv", "qgam", "jsonlite"))
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion")
  set.seed(random_seed)
  if (!file.exists(analysis_data_csv)) {
    stop("Analysis data not found: ", analysis_data_csv, call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  data <- utils::read.csv(analysis_data_csv, check.names = FALSE, stringsAsFactors = FALSE)
  data$region <- factor(data$region, levels = c("West", "East"))
  if (!all(c("response", "Bombus_W", "spatial_fold", "x_km", "y_km", "z_H") %in% names(data))) {
    stop("The input is not a complete ecological v2 analysis_data.csv.", call. = FALSE)
  }
  env_terms <- grep("^env_", names(data), value = TRUE)
  if (!length(env_terms)) stop("No locked environmental axes found in analysis data.")

  # Repair legacy files written before missing optional rasters were preserved as
  # NA.  A constant zero R/A axis is unavailable, not evidence of zero exposure.
  for (axis in intersect(c("z_R", "z_A"), names(data))) {
    if (!is_available_axis(data[[axis]])) data[[axis]] <- NA_real_
  }
  data <- attach_image_quality_controls(data, raw_colour_csv)
  # Seasonal Bombus predictors are functions of the flower observation date.
  # Flexible polynomial date terms and observation year must therefore be in
  # every adjusted rung and in matching; otherwise colour phenology could be
  # misattributed to Bombus temporal overlap.
  data$DOY_z <- safe_z(data$DOY)
  data$DOY_z2 <- safe_z(data$DOY_z^2)
  data$DOY_z3 <- safe_z(data$DOY_z^3)
  data$year_z <- safe_z(data$year)

  occurrence <- read_bombus_occurrence_cache(occurrence_dir)
  occurrence_index <- compute_bombus_occurrence_indices(
    data, occurrence$data, radii_km = radii_km,
    minimum_kernel_support = occurrence_support
  )
  data <- occurrence_index$data
  seasonal_index <- NULL
  if (nrow(occurrence$seasonal_data)) {
    seasonal_index <- compute_bombus_seasonal_indices(
      data, occurrence$seasonal_data,
      radius_km = max(radii_km), temporal_bandwidth_days = 30,
      temporal_window_days = 60,
      minimum_spatial_support = occurrence_support
    )
    data <- seasonal_index$data
    write_csv_safe(
      seasonal_index$definition,
      file.path(output_dir, "bombus_seasonal_index_definition.csv")
    )
  }
  write_csv_safe(occurrence$manifest, file.path(output_dir, "bombus_occurrence_manifest.csv"))
  write_csv_safe(
    occurrence_index$definitions,
    file.path(output_dir, "bombus_occurrence_index_definitions.csv")
  )

  radius_labels <- paste0(format(sort(unique(radii_km)), scientific = FALSE, trim = TRUE), "km")
  occurrence_proxies <- paste0("Bombus_W_occ_", radius_labels)
  density_proxies <- paste0("Bombus_W_occ_density_", radius_labels)
  max_radius_label <- tail(radius_labels, 1)
  widespread_species_proxies <- c(
    paste0("Bombus_ardens_occ_density_", max_radius_label),
    paste0("Bombus_diversus_occ_density_", max_radius_label),
    paste0("Bombus_W_ardens_share_", max_radius_label)
  )
  seasonal_proxies <- c(
    paste0("Bombus_total_seasonal_density_", max_radius_label, "_30d"),
    paste0("Bombus_temporal_overlap_", max_radius_label, "_30d")
  )
  seasonal_adjustment_terms <- c(
    env_terms, "DOY_z", "DOY_z2", "DOY_z3", "year_z",
    if ("z_N" %in% names(data) && is_available_axis(data$z_N)) "z_N"
  )
  bombus_proxies <- c(
    "Bombus_W", occurrence_proxies, density_proxies, widespread_species_proxies,
    "Bombus_A", paste0("Bombus_A_occ_density_", max_radius_label),
    paste0("Bombus_total_occ_density_", max_radius_label), seasonal_proxies
  )
  bombus_proxies <- bombus_proxies[
    bombus_proxies %in% names(data) & vapply(data[bombus_proxies], is_available_axis, logical(1))
  ]
  ladder <- decomposition <- matching <- list()
  for (i in seq_along(bombus_proxies)) {
    proxy <- bombus_proxies[i]
    message("[v3] Bombus evidence ladder: ", proxy)
    proxy_data <- data
    if (proxy == "Bombus_A") {
      # The calibrated alpine ENM intersection has only four western records;
      # use its prespecified eastern common-support block, as in v2.
      proxy_data <- droplevels(data[data$region == "East" & is.finite(data$Bombus_A), , drop = FALSE])
    }
    proxy_env_terms <- if (proxy %in% seasonal_proxies) {
      seasonal_adjustment_terms
    } else env_terms
    ladder[[proxy]] <- fit_bombus_evidence_ladder(
      proxy_data, "response", proxy, proxy_env_terms, k_space
    )
    decomposition[[proxy]] <- decompose_bombus_proxy(
      proxy_data, proxy, proxy_env_terms, "response", k_space
    )
    if (proxy == "Bombus_W" || proxy %in% occurrence_proxies ||
        proxy %in% seasonal_proxies) {
      matching[[proxy]] <- match_local_bombus_contrasts(
        data, proxy, proxy_env_terms, radius_km = match_radius_km,
        env_caliper = match_env_caliper,
        propensity_caliper_sd = match_propensity_caliper_sd
      )
    }
  }
  bombus_coefficients <- do.call(rbind, lapply(ladder, `[[`, "coefficients"))
  bombus_heldout <- do.call(rbind, lapply(ladder, `[[`, "heldout_metrics"))
  bombus_logs <- do.call(rbind, lapply(ladder, `[[`, "log"))
  bombus_decomposition <- do.call(rbind, lapply(decomposition, `[[`, "summary"))
  bombus_component_data <- do.call(rbind, lapply(decomposition, `[[`, "data"))
  match_summary <- do.call(rbind, lapply(matching, `[[`, "summary"))
  match_pairs <- do.call(rbind, lapply(matching, `[[`, "pairs"))
  match_balance <- do.call(rbind, lapply(matching, `[[`, "balance"))
  write_csv_safe(bombus_coefficients, file.path(output_dir, "bombus_evidence_ladder_coefficients.csv"))
  write_csv_safe(bombus_heldout, file.path(output_dir, "bombus_evidence_ladder_heldout.csv"))
  write_csv_safe(bombus_logs, file.path(output_dir, "bombus_evidence_ladder_crossfit_log.csv"))
  write_csv_safe(bombus_decomposition, file.path(output_dir, "bombus_shared_local_decomposition.csv"))
  write_csv_safe(bombus_component_data, file.path(output_dir, "bombus_shared_local_observations.csv"))
  write_csv_safe(match_summary, file.path(output_dir, "bombus_local_match_summary.csv"))
  write_csv_safe(match_pairs, file.path(output_dir, "bombus_local_matched_pairs.csv"))
  write_csv_safe(match_balance, file.path(output_dir, "bombus_local_match_balance.csv"))

  # H is also a spatially structured exposure.  Apply the same ladder,
  # cross-fitted decomposition, and response-blind matching used for Bombus so
  # that a small post-spatial coefficient can be distinguished from a lack of
  # locally independent H variation.
  human_natural_terms <- c(env_terms, "Bombus_W")
  human_ladder <- fit_bombus_evidence_ladder(
    data, "response", "z_H", human_natural_terms, k_space
  )
  human_decomposition <- decompose_bombus_proxy(
    data, "z_H", human_natural_terms, "response", k_space
  )
  human_matching <- match_local_bombus_contrasts(
    data, "z_H", human_natural_terms, radius_km = match_radius_km,
    env_caliper = match_env_caliper,
    propensity_caliper_sd = match_propensity_caliper_sd
  )
  write_csv_safe(
    human_ladder$coefficients,
    file.path(output_dir, "human_H_evidence_ladder_coefficients.csv")
  )
  write_csv_safe(
    human_ladder$heldout_metrics,
    file.path(output_dir, "human_H_evidence_ladder_heldout.csv")
  )
  write_csv_safe(
    human_decomposition$summary,
    file.path(output_dir, "human_H_shared_local_decomposition.csv")
  )
  write_csv_safe(
    human_matching$summary,
    file.path(output_dir, "human_H_local_match_summary.csv")
  )
  write_csv_safe(
    human_matching$pairs,
    file.path(output_dir, "human_H_local_matched_pairs.csv")
  )
  write_csv_safe(
    human_matching$balance,
    file.path(output_dir, "human_H_local_match_balance.csv")
  )

  reference <- define_low_human_reference(data, "z_H", reference_fraction)
  data <- reference$data
  write_csv_safe(reference$definition, file.path(output_dir, "low_human_reference_definition.csv"))
  colour_terms <- c("region", env_terms, "Bombus_W")
  colour_reference <- crossfit_reference_qgam(
    data, "response", colour_terms, taus = c(0.5, 0.9, 0.95),
    k_space = reference_k_space
  )
  data$reference_colour_q50 <- colour_reference$predictions$q50
  data$reference_colour_q90 <- colour_reference$predictions$q90
  data$reference_colour_q95 <- colour_reference$predictions$q95
  data$colour_excess_q90 <- data$response - data$reference_colour_q90
  data$colour_excess_q95 <- data$response - data$reference_colour_q95
  data$colour_reference_support_fraction <- colour_reference$support_fraction
  data$colour_reference_common_support <- colour_reference$common_support
  write_csv_safe(
    colour_reference$calibration,
    file.path(output_dir, "low_human_colour_reference_calibration.csv")
  )
  write_csv_safe(
    colour_reference$log,
    file.path(output_dir, "low_human_colour_reference_crossfit_log.csv")
  )

  phenology_terms <- c("region", env_terms)
  if ("z_N" %in% names(data) && is_available_axis(data$z_N)) {
    phenology_terms <- c(phenology_terms, "z_N")
  }
  phenology_reference <- crossfit_reference_rq(
    data, "DOY", phenology_terms, tau = 0.1,
    k_space = reference_k_space, include_year = TRUE
  )
  data$reference_DOY_q10 <- phenology_reference$predictions$q10
  data$early_observation_excess <- data$reference_DOY_q10 - data$DOY
  data$phenology_reference_support_fraction <- phenology_reference$support_fraction
  data$phenology_reference_common_support <- phenology_reference$common_support
  data$joint_reference_common_support <-
    data$colour_reference_common_support & data$phenology_reference_common_support
  data$colour_upper90_reference <- data$response > data$reference_colour_q90
  data$colour_upper95_reference <- data$response > data$reference_colour_q95
  data$early10_reference <- data$DOY < data$reference_DOY_q10
  data$joint_colour90_early10 <- data$colour_upper90_reference & data$early10_reference
  data$joint_colour95_early10 <- data$colour_upper95_reference & data$early10_reference
  write_csv_safe(
    phenology_reference$calibration,
    file.path(output_dir, "low_human_phenology_reference_calibration.csv")
  )
  write_csv_safe(
    phenology_reference$log,
    file.path(output_dir, "low_human_phenology_reference_crossfit_log.csv")
  )
  write_csv_safe(
    phenology_reference$decision_audit,
    file.path(output_dir, "phenology_model_decision_audit.csv")
  )

  joint <- fit_joint_horticultural_signature(data, k_space = k_space)
  write_csv_safe(joint$slopes, file.path(output_dir, "horticultural_joint_signature_slopes.csv"))
  write_csv_safe(joint$fisher, file.path(output_dir, "horticultural_joint_signature_fisher.csv"))
  write_csv_safe(joint$diagnostics, file.path(output_dir, "horticultural_joint_signature_diagnostics.csv"))

  horticultural_tail <- fit_horticultural_qgams(
    data, "response", colour_terms, taus = c(0.5, 0.75, 0.9, 0.95),
    k_space = k_space
  )
  write_csv_safe(
    horticultural_tail$region_slopes,
    file.path(output_dir, "horticultural_raw_colour_quantile_slopes.csv")
  )
  write_csv_safe(
    horticultural_tail$contrasts,
    file.path(output_dir, "horticultural_raw_colour_quantile_contrasts.csv")
  )
  write_csv_safe(
    horticultural_tail$warnings,
    file.path(output_dir, "horticultural_raw_colour_quantile_warnings.csv")
  )
  tail_specificity <- block_bootstrap_tail_specificity(
    data, env_terms, n_boot = tail_bootstraps, k_space = k_space
  )
  write_csv_safe(
    tail_specificity$contrast,
    file.path(output_dir, "horticultural_tail_specificity_block_bootstrap.csv")
  )
  write_csv_safe(
    tail_specificity$slopes,
    file.path(output_dir, "horticultural_tail_specificity_linear_slopes.csv")
  )
  colour_coherence <- fit_human_colour_coherence(data, k_space = k_space)
  write_csv_safe(
    colour_coherence,
    file.path(output_dir, "horticultural_colour_dimension_coherence.csv")
  )
  image_diagnostics <- fit_human_image_diagnostics(data, k_space = k_space)
  write_csv_safe(
    image_diagnostics,
    file.path(output_dir, "horticultural_image_diagnostics.csv")
  )

  # H (human propagule pressure) and R (human-managed/forest interface) are
  # distinct processes. Their centered product is specified without reference
  # to flower colour, then checked on raw colour, date-adjusted colour, raw DOY,
  # upper quantiles, and spatially held-out prediction. The convergence gates
  # are not collapsed into an arbitrary weighted score.
  if (is_available_axis(data$z_R)) data$z_H_R <- data$z_H * data$z_R
  interface_convergence <- fit_horticultural_interface_convergence(
    data, env_terms, k_space = k_space
  )
  write_csv_safe(
    interface_convergence$status,
    file.path(output_dir, "horticultural_HR_status.csv")
  )
  write_csv_safe(
    interface_convergence$coefficients,
    file.path(output_dir, "horticultural_HR_coefficients.csv")
  )
  write_csv_safe(
    interface_convergence$heldout,
    file.path(output_dir, "horticultural_HR_heldout.csv")
  )
  write_csv_safe(
    interface_convergence$warnings,
    file.path(output_dir, "horticultural_HR_warnings.csv")
  )
  write_csv_safe(
    interface_convergence$vif,
    file.path(output_dir, "horticultural_HR_collinearity_vif.csv")
  )
  write_csv_safe(
    interface_convergence$correlations,
    file.path(output_dir, "horticultural_HR_collinearity_correlations.csv")
  )
  write_csv_safe(
    interface_convergence$condition,
    file.path(output_dir, "horticultural_HR_collinearity_condition.csv")
  )
  write_csv_safe(
    interface_convergence$gates,
    file.path(output_dir, "horticultural_HR_directional_gates.csv")
  )
  write_csv_safe(
    interface_convergence$boundary_audit,
    file.path(output_dir, "horticultural_R_boundary_audit.csv")
  )

  evidence_matrix <- make_reviewer_evidence_matrix(
    bombus_coefficients, bombus_heldout, match_summary,
    human_ladder, human_matching, human_decomposition,
    horticultural_tail$region_slopes, joint, colour_coherence,
    tail_specificity$contrast, image_diagnostics
  )
  evidence_matrix <- rbind(
    evidence_matrix, make_interface_evidence_rows(interface_convergence)
  )
  write_csv_safe(evidence_matrix, file.path(output_dir, "reviewer_evidence_matrix.csv"))

  id_cols <- intersect(
    c("sample_id", "observation_id", "exact_site_id", "col_hex"), names(data)
  )
  mechanism_cols <- unique(c(
    id_cols, "longitude", "latitude", "region", "spatial_fold", "response", "colour_L",
    "DOY", "Bombus_W", bombus_proxies,
    "z_H", "z_R", "z_H_R", "z_N", "z_A", "low_human_reference",
    "human_exposure_percentile_within_region",
    "reference_colour_q50", "reference_colour_q90", "reference_colour_q95",
    "colour_excess_q90", "colour_excess_q95", "reference_DOY_q10",
    "early_observation_excess", "joint_reference_common_support",
    "colour_upper90_reference", "colour_upper95_reference", "early10_reference",
    "joint_colour90_early10", "joint_colour95_early10"
  ))
  mechanism_cols <- intersect(mechanism_cols, names(data))
  write_csv_safe(data[mechanism_cols], file.path(output_dir, "mechanism_observation_results.csv"))
  write_csv_safe(data, file.path(output_dir, "analysis_data_mechanism_v3.csv"))

  summary_values <- list(
    pipeline = "ecological_mechanism_v3",
    analysis_data = file.path(
      basename(dirname(analysis_data_csv)), basename(analysis_data_csv)
    ),
    analysis_data_md5 = unname(tools::md5sum(analysis_data_csv)),
    occurrence_dir = basename(normalizePath(
      occurrence_dir, winslash = "/", mustWork = TRUE
    )),
    raw_colour_csv = if (nzchar(raw_colour_csv) && file.exists(raw_colour_csv)) {
      basename(normalizePath(raw_colour_csv, winslash = "/", mustWork = TRUE))
    } else NULL,
    n = nrow(data), radii_km = sort(unique(radii_km)),
    occurrence_support = occurrence_support,
    match_propensity_caliper_sd = match_propensity_caliper_sd,
    tail_bootstraps = tail_bootstraps,
    reference_k_space = reference_k_space,
    random_seed = random_seed,
    reference_fraction = reference_fraction,
    low_human_reference_n = sum(data$low_human_reference),
    colour_reference_common_support_n = sum(data$colour_reference_common_support, na.rm = TRUE),
    phenology_reference_common_support_n = sum(data$phenology_reference_common_support, na.rm = TRUE),
    joint90_early10_n = sum(data$joint_colour90_early10 & data$joint_reference_common_support, na.rm = TRUE),
    joint95_early10_n = sum(data$joint_colour95_early10 & data$joint_reference_common_support, na.rm = TRUE),
    available_human_axes = c("H", if (is_available_axis(data$z_R)) "R", if (is_available_axis(data$z_N)) "N", if (is_available_axis(data$z_A)) "A"),
    claim_ceiling = paste(
      "multi-source observational evidence consistent with Bombus association or",
      "horticultural influence; not causal selection or individual provenance"
    )
  )
  jsonlite::write_json(
    summary_values, file.path(output_dir, "run_summary.json"),
    pretty = TRUE, auto_unbox = TRUE, na = "null"
  )
  lines <- c(
    "# Mechanism-oriented ecological workflow v3",
    "",
    paste0("- Observations: ", nrow(data), "."),
    paste0("- Frozen GBIF occurrence records after occupancy deduplication: ", nrow(occurrence$data), "."),
    paste0("- Low-human reference observations: ", sum(data$low_human_reference), "."),
    paste0("- Q0.90-colour + Q0.10-early joint events in common support: ",
           summary_values$joint90_early10_n, "."),
    paste0("- Public human axes actually available: ",
           paste(summary_values$available_human_axes, collapse = ", "), "."),
    "- Bombus collinearity is reported as shared biogeographic structure, not used as an automatic deletion rule.",
    "- Low-human reference membership and Bombus occurrence indices are constructed without flower colour.",
    "- The horticultural result is a falsifiable multi-axis consistency test, not provenance assignment."
  )
  writeLines(lines, file.path(output_dir, "ANALYSIS_SUMMARY.md"), useBytes = TRUE)

  invisible(list(
    data = data, occurrence = occurrence, occurrence_index = occurrence_index,
    ladder = ladder, decomposition = decomposition, matching = matching,
    seasonal_index = seasonal_index,
    human_ladder = human_ladder, human_decomposition = human_decomposition,
    human_matching = human_matching,
    reference = reference, colour_reference = colour_reference,
    phenology_reference = phenology_reference, joint = joint,
    horticultural_tail = horticultural_tail,
    tail_specificity = tail_specificity,
    colour_coherence = colour_coherence,
    image_diagnostics = image_diagnostics,
    interface_convergence = interface_convergence,
    evidence_matrix = evidence_matrix, summary = summary_values
  ))
}
