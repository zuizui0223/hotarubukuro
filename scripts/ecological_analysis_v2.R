# Ecological analysis v2 for the Campanula punctata flower-colour study.
#
# Design principles:
#   * CIELAB a* is the primary, directly interpretable pinkness response.
#   * Environmental adjustment is specified before inspecting Bombus effects.
#   * Widespread and alpine Bombus blocks are kept separate; suitability values
#     are not combined as if they were independent occurrence probabilities.
#   * Upper-tail anomalies are out-of-block predictions and are used only to
#     identify observations for follow-up. Raw colour remains the inferential
#     response for the horticultural hypothesis.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing required R packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

write_csv_safe <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

safe_z <- function(x) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  if (!any(ok)) return(rep(NA_real_, length(x)))
  sx <- stats::sd(x[ok])
  out <- rep(NA_real_, length(x))
  if (!is.finite(sx) || sx == 0) {
    out[ok] <- 0
    return(out)
  }
  out[ok] <- (x[ok] - mean(x[ok])) / sx
  out
}

rank01 <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  if (any(ok)) out[ok] <- (rank(x[ok], ties.method = "average") - 0.5) / sum(ok)
  out
}

normal_score <- function(x) {
  p <- rank01(x)
  stats::qnorm(p)
}

as_flag <- function(x) {
  tolower(trimws(as.character(x))) %in% c("1", "true", "yes", "y")
}

srgb_to_lab_d65 <- function(r, g, b) {
  rgb <- pmin(pmax(cbind(as.numeric(r), as.numeric(g), as.numeric(b)) / 255, 0), 1)
  lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  xyz <- lin %*% t(matrix(c(
    0.4124564, 0.3575761, 0.1804375,
    0.2126729, 0.7151522, 0.0721750,
    0.0193339, 0.1191920, 0.9503041
  ), nrow = 3, byrow = TRUE))
  xyz <- sweep(xyz, 2, c(0.95047, 1, 1.08883), "/")
  eps <- (6 / 29)^3
  f <- ifelse(xyz > eps, xyz^(1 / 3), xyz / (3 * (6 / 29)^2) + 4 / 29)
  data.frame(
    L = 116 * f[, 2] - 16,
    a = 500 * (f[, 1] - f[, 2]),
    b = 200 * (f[, 2] - f[, 3]),
    check.names = FALSE
  )
}

audit_colour_qc <- function(raw, author_review_confirmed = TRUE) {
  rgb_cols <- if (all(c("median_R", "median_G", "median_B") %in% names(raw))) {
    c("median_R", "median_G", "median_B")
  } else if (all(c("R", "G", "B") %in% names(raw))) {
    c("R", "G", "B")
  } else {
    stop("No complete RGB triplet found in raw colour data.", call. = FALSE)
  }

  lab <- srgb_to_lab_d65(raw[[rgb_cols[1]]], raw[[rgb_cols[2]]], raw[[rgb_cols[3]]])
  raw$colour_L <- lab$L
  raw$colour_a <- lab$a
  raw$colour_b <- lab$b
  raw$colour_C <- sqrt(lab$a^2 + lab$b^2)

  status_value <- if ("qc_status" %in% names(raw)) {
    tolower(trimws(as.character(raw$qc_status)))
  } else rep("ok", nrow(raw))
  status_ok <- status_value == "ok"
  # manual_review_required is a warning state, not a failed measurement. In
  # particular, white and within-flower multicolour observations can trigger
  # it. Excluding every warning would condition inclusion on the response.
  status_analysis_eligible <- !(status_value %in% c(
    "fail", "no_mask", "missing_image", "unreadable_image"
  ))

  legacy_review_status_approved <- if ("manual_review_status" %in% names(raw)) {
    tolower(trimws(as.character(raw$manual_review_status))) %in% c(
      "not_required_by_automated_qc", "approved", "pass", "passed", "reviewed_ok"
    )
  } else rep(TRUE, nrow(raw))
  author_region_review_confirmed <- rep(isTRUE(author_review_confirmed), nrow(raw))

  no_duplicate <- if ("duplicate_image_sha256" %in% names(raw)) {
    !as_flag(raw$duplicate_image_sha256)
  } else rep(TRUE, nrow(raw))

  no_overexposure <- if ("possible_overexposure" %in% names(raw)) {
    !as_flag(raw$possible_overexposure)
  } else rep(TRUE, nrow(raw))

  enough_mask <- rep(TRUE, nrow(raw))
  if ("mask_pixels" %in% names(raw)) {
    enough_mask <- enough_mask & (is.na(raw$mask_pixels) | as.numeric(raw$mask_pixels) >= 50)
  }
  if ("mask_fraction_visible" %in% names(raw)) {
    enough_mask <- enough_mask &
      (is.na(raw$mask_fraction_visible) | as.numeric(raw$mask_fraction_visible) > 0)
  }

  raw$qc_automated_ok <- status_ok
  raw$qc_legacy_manual_status_approved <- legacy_review_status_approved
  raw$qc_author_region_review_confirmed <- author_region_review_confirmed
  raw$qc_no_duplicate <- no_duplicate
  raw$qc_no_overexposure <- no_overexposure
  raw$qc_enough_mask <- enough_mask
  valid_colour <- is.finite(raw$colour_a)
  raw$colour_qc_primary <- status_analysis_eligible & no_duplicate & enough_mask & valid_colour
  raw$colour_qc_no_warning_sensitivity <- status_ok & no_duplicate &
    no_overexposure & enough_mask & valid_colour
  raw$colour_qc_inclusive <- raw$colour_qc_primary
  raw$analysis_manual_review_status <- if (author_review_confirmed) {
    "author_confirmed_flower_colour_extraction_region"
  } else if ("manual_review_status" %in% names(raw)) {
    as.character(raw$manual_review_status)
  } else rep(NA_character_, nrow(raw))

  criteria <- c(
    "all_rows", "legacy_automated_status_ok",
    "author_extraction_region_review_confirmed", "legacy_manual_status_approved",
    "not_duplicate", "not_overexposed", "mask_adequate",
    "primary_hard_failures_removed", "no_automated_warning_sensitivity"
  )
  passed <- c(
    nrow(raw), sum(status_ok, na.rm = TRUE),
    sum(author_region_review_confirmed, na.rm = TRUE),
    sum(legacy_review_status_approved, na.rm = TRUE),
    sum(no_duplicate, na.rm = TRUE), sum(no_overexposure, na.rm = TRUE),
    sum(enough_mask, na.rm = TRUE), sum(raw$colour_qc_primary, na.rm = TRUE),
    sum(raw$colour_qc_no_warning_sensitivity, na.rm = TRUE)
  )
  audit <- data.frame(
    criterion = criteria,
    n_pass = passed,
    n_fail = nrow(raw) - passed,
    proportion_pass = passed / nrow(raw),
    stringsAsFactors = FALSE
  )
  list(data = raw, audit = audit)
}

extract_raster_value <- function(data, raster_path, name) {
  if (is.null(raster_path) || !nzchar(raster_path) || !file.exists(raster_path)) {
    data[[name]] <- NA_real_
    return(data)
  }
  r <- terra::rast(raster_path)
  pts <- terra::vect(data, geom = c("longitude", "latitude"), crs = "EPSG:4326")
  value <- terra::extract(r, pts, method = "bilinear")
  value <- value[, ncol(value), drop = TRUE]
  data[[name]] <- as.numeric(value)
  data
}

extract_bombus_predictions <- function(data, prediction_dir) {
  species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
  missing_files <- character()
  for (sp in species) {
    path <- file.path(prediction_dir, paste0(sp, ".tif"))
    if (!file.exists(path)) missing_files <- c(missing_files, path)
    data <- extract_raster_value(data, if (file.exists(path)) path else "", paste0("bee_", sp))
  }
  if (length(missing_files)) {
    stop("Missing freshly modelled Bombus predictions: ", paste(missing_files, collapse = ", "), call. = FALSE)
  }

  widespread <- paste0("bee_", c("ardens", "diversus"))
  alpine <- paste0("bee_", c("beaticola", "consobrinus", "honshuensis"))
  for (nm in c(widespread, alpine)) data[[paste0(nm, "_ns")]] <- normal_score(data[[nm]])

  w_ns <- paste0(widespread, "_ns")
  a_ns <- paste0(alpine, "_ns")
  data$Bombus_W_coverage <- rowSums(is.finite(as.matrix(data[widespread])))
  data$Bombus_A_coverage <- rowSums(is.finite(as.matrix(data[alpine])))
  data$Bombus_W <- ifelse(
    data$Bombus_W_coverage == length(widespread),
    rowMeans(as.matrix(data[w_ns])), NA_real_
  )
  # A is defined only on the common calibrated domain of the three alpine
  # SDMs. Outside that domain NA means "not evaluated", never absence.
  data$Bombus_A <- ifelse(
    data$Bombus_A_coverage == length(alpine),
    rowMeans(as.matrix(data[a_ns])), NA_real_
  )
  data$Bombus_W <- safe_z(data$Bombus_W)
  data$Bombus_A <- safe_z(data$Bombus_A)
  data
}

add_projected_coordinates <- function(data) {
  pts <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  japan_laea <- paste(
    "+proj=laea +lat_0=36 +lon_0=137 +x_0=0 +y_0=0",
    "+datum=WGS84 +units=m +no_defs"
  )
  xy <- sf::st_coordinates(sf::st_transform(pts, japan_laea)) / 1000
  data$x_km <- xy[, 1]
  data$y_km <- xy[, 2]
  data
}

assign_spatial_blocks <- function(data, block_km = 50, k = 5, east_west_cut = 136.5) {
  stopifnot(all(c("x_km", "y_km", "longitude") %in% names(data)))
  data$region <- factor(ifelse(data$longitude >= east_west_cut, "East", "West"),
                        levels = c("West", "East"))
  data$block_x <- floor(data$x_km / block_km)
  data$block_y <- floor(data$y_km / block_km)
  data$spatial_block <- paste(data$block_x, data$block_y, sep = "_")

  blocks <- unique(data[c("spatial_block", "block_x", "block_y", "region")])
  blocks$n_observations <- as.integer(table(data$spatial_block)[blocks$spatial_block])
  blocks <- blocks[order(blocks$region, -blocks$n_observations,
                         blocks$block_y, blocks$block_x), , drop = FALSE]
  blocks$fold <- NA_integer_
  for (reg in levels(data$region)) {
    idx <- which(blocks$region == reg)
    # Greedy bin packing balances point counts without ever splitting a spatial
    # block. This avoids folds consisting of one tiny tail block.
    load <- numeric(k)
    if (length(idx)) for (j in idx) {
      target <- which.min(load)
      blocks$fold[j] <- target
      load[target] <- load[target] + blocks$n_observations[j]
    }
  }
  data$spatial_fold <- blocks$fold[match(data$spatial_block, blocks$spatial_block)]
  if (anyNA(data$spatial_fold) || length(unique(data$spatial_fold)) != k) {
    stop("Unable to construct all requested spatial folds.", call. = FALSE)
  }
  data
}

compute_vif <- function(data, terms) {
  terms <- terms[vapply(data[terms], function(x) stats::sd(x, na.rm = TRUE) > 0, logical(1))]
  out <- lapply(terms, function(term) {
    others <- setdiff(terms, term)
    if (!length(others)) return(data.frame(term = term, VIF = 1))
    fit <- stats::lm(stats::reformulate(others, response = term), data = data)
    data.frame(term = term, VIF = 1 / pmax(1 - summary(fit)$r.squared, 1e-8))
  })
  do.call(rbind, out)
}

make_analysis_data <- function(anomaly_data, raw_colour, bombus_dir, raster_paths = list(),
                               strict_qc = FALSE, east_west_cut = 136.5,
                               block_km = 50, folds = 5,
                               author_review_confirmed = TRUE) {
  require_packages(c("sf", "terra"))
  qc <- audit_colour_qc(raw_colour, author_review_confirmed)
  id <- if ("observation_id" %in% names(anomaly_data) && "observation_id" %in% names(qc$data)) {
    "observation_id"
  } else "sample_id"
  keep_raw <- unique(c(
    id, "colour_a", "colour_L", "colour_b", "colour_C", "colour_qc_primary",
    "colour_qc_no_warning_sensitivity", "colour_qc_inclusive", "qc_status",
    "manual_review_status", "analysis_manual_review_status", "qc_flags",
    "possible_overexposure", "mask_pixels", "mask_fraction_visible"
  ))
  keep_raw <- intersect(keep_raw, names(qc$data))
  dat <- merge(anomaly_data, qc$data[keep_raw], by = id, all.x = TRUE, sort = FALSE)
  dat$analysis_qc_set <- if (strict_qc) dat$colour_qc_no_warning_sensitivity else dat$colour_qc_primary
  dat <- dat[!is.na(dat$analysis_qc_set) & dat$analysis_qc_set, , drop = FALSE]

  dat$colour_a_z <- safe_z(dat$colour_a)
  if (!("Pigment" %in% names(dat))) dat$Pigment <- NA_real_
  dat$Pigment_z_sensitivity <- safe_z(dat$Pigment)
  dat$response <- dat$colour_a_z
  dat$date_parsed <- as.Date(dat$date)
  dat$DOY <- as.integer(format(dat$date_parsed, "%j"))
  dat$year <- as.integer(format(dat$date_parsed, "%Y"))

  dat <- extract_bombus_predictions(dat, bombus_dir)
  dat <- add_projected_coordinates(dat)
  dat <- assign_spatial_blocks(dat, block_km, folds, east_west_cut)

  raster_registry <- data.frame(
    axis = c("H", "R", "N", "A"),
    variable = c("human_population", "human_forest_edge", "gdd5", "road_access"),
    path = c(raster_paths$H %||% "", raster_paths$R %||% "",
             raster_paths$N %||% "", raster_paths$A %||% ""),
    stringsAsFactors = FALSE
  )
  raster_registry$available <- file.exists(raster_registry$path) & nzchar(raster_registry$path)
  for (i in seq_len(nrow(raster_registry))) {
    dat <- extract_raster_value(dat, raster_registry$path[i], raster_registry$variable[i])
  }
  dat$log_human_population <- log1p(pmax(dat$human_population, 0))
  dat$z_H <- safe_z(dat$log_human_population)
  dat$z_R <- safe_z(dat$human_forest_edge)
  dat$z_N <- safe_z(dat$gdd5)
  # If A is distance to a road, reverse it so larger means more accessible.
  dat$z_A <- -safe_z(dat$road_access)

  soil_terms <- if (all(c("soil_PC1", "soil_PC2") %in% names(dat))) {
    c("soil_PC1", "soil_PC2")
  } else {
    c("soil_phys_PC1", "soil_nutrient_PC1", "soil_pH")
  }
  env_original <- c(
    "Temperature_PC1", "precip_PC1", "TemperatureSeasonality", "PrecipSeasonality",
    "topo_PC1", soil_terms, "RSDS"
  )
  env_original <- intersect(env_original, names(dat))
  env_terms <- paste0("env_", env_original)
  for (i in seq_along(env_original)) dat[[env_terms[i]]] <- safe_z(dat[[env_original[i]]])

  # The national analysis population must not be defined by the restricted M
  # areas of alpine Bombus SDMs. W is nationwide; A is analysed on its own
  # explicitly labelled common-support subset.
  required <- c("response", "longitude", "latitude", "x_km", "y_km", "Bombus_W", env_terms)
  dat <- dat[stats::complete.cases(dat[required]), , drop = FALSE]
  rownames(dat) <- NULL

  list(
    data = dat,
    qc_audit = qc$audit,
    raster_registry = raster_registry,
    environment_terms = env_terms,
    environment_original = env_original
  )
}

make_spatial_formula <- function(response, linear_terms, k_space = 40, year_smooth = FALSE) {
  rhs <- c(linear_terms, sprintf("s(x_km, y_km, k = %d, bs = 'tp')", k_space))
  if (year_smooth) rhs <- c(rhs, "s(year, k = 8, bs = 'tp')")
  stats::as.formula(paste(response, "~", paste(rhs, collapse = " + ")))
}

crossfit_qgam <- function(data, response, linear_terms, taus = c(0.5, 0.95),
                          fold_col = "spatial_fold", k_space = 40) {
  require_packages(c("qgam", "mgcv"))
  folds <- sort(unique(data[[fold_col]]))
  pred <- matrix(NA_real_, nrow(data), length(taus),
                 dimnames = list(NULL, paste0("q", formatC(taus * 100, width = 2, flag = "0"))))
  logs <- list()
  fit_index <- 0L
  for (fold in folds) {
    test <- which(data[[fold_col]] == fold)
    train <- which(data[[fold_col]] != fold)
    k_use <- max(15L, min(as.integer(k_space), floor(length(train) / 20)))
    form <- make_spatial_formula(response, linear_terms, k_use)
    for (j in seq_along(taus)) {
      fit_index <- fit_index + 1L
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
      pred[test, j] <- as.numeric(stats::predict(fit, newdata = data[test, , drop = FALSE], type = "response"))
      logs[[fit_index]] <- data.frame(
        fold = fold, tau = taus[j], n_train = length(train), n_test = length(test),
        formula = paste(deparse(form), collapse = " "),
        warnings = paste(unique(warnings), collapse = " | "), stringsAsFactors = FALSE
      )
    }
  }
  if (any(!is.finite(pred))) stop("Cross-fitted qGAM produced missing predictions.", call. = FALSE)
  list(predictions = as.data.frame(pred), log = do.call(rbind, logs))
}

crossfit_gam <- function(data, response, linear_terms, fold_col = "spatial_fold",
                         k_space = 40, include_year = FALSE) {
  require_packages("mgcv")
  pred <- rep(NA_real_, nrow(data))
  logs <- list()
  folds <- sort(unique(data[[fold_col]]))
  for (i in seq_along(folds)) {
    fold <- folds[i]
    test <- which(data[[fold_col]] == fold)
    train <- which(data[[fold_col]] != fold)
    k_use <- max(15L, min(as.integer(k_space), floor(length(train) / 20)))
    use_year <- include_year && length(unique(data$year[train][is.finite(data$year[train])])) >= 9
    form <- make_spatial_formula(response, linear_terms, k_use, year_smooth = use_year)
    fit <- mgcv::gam(form, data = data[train, , drop = FALSE], method = "REML")
    pred[test] <- as.numeric(stats::predict(fit, newdata = data[test, , drop = FALSE], type = "response"))
    logs[[i]] <- data.frame(
      fold = fold, n_train = length(train), n_test = length(test),
      formula = paste(deparse(form), collapse = " "), stringsAsFactors = FALSE
    )
  }
  if (any(!is.finite(pred))) stop("Cross-fitted GAM produced missing predictions.", call. = FALSE)
  list(predictions = pred, log = do.call(rbind, logs))
}

calibrate_upper_tail <- function(data, response = "response", q95 = "q95") {
  groups <- split(seq_len(nrow(data)), interaction(data$region, data$spatial_fold, drop = TRUE))
  rows <- lapply(groups, function(idx) {
    data.frame(
      region = as.character(data$region[idx[1]]),
      fold = data$spatial_fold[idx[1]],
      n = length(idx),
      observed_below_q95 = mean(data[[response]][idx] <= data[[q95]][idx]),
      upper_exceedance_rate = mean(data[[response]][idx] > data[[q95]][idx])
    )
  })
  overall <- data.frame(
    region = "All", fold = 0L, n = nrow(data),
    observed_below_q95 = mean(data[[response]] <= data[[q95]]),
    upper_exceedance_rate = mean(data[[response]] > data[[q95]])
  )
  rbind(overall, do.call(rbind, rows))
}

coefficient_row <- function(fit, term, model, predictor) {
  sm <- summary(fit)
  tab <- sm$p.table
  if (!(term %in% rownames(tab))) {
    return(data.frame(model = model, predictor = predictor, term = term,
                      estimate = NA_real_, se = NA_real_, statistic = NA_real_, p_value = NA_real_))
  }
  data.frame(
    model = model, predictor = predictor, term = term,
    estimate = tab[term, 1], se = tab[term, 2], statistic = tab[term, 3], p_value = tab[term, 4]
  )
}

fit_spatial_confounding_envelope <- function(data, response, env_terms,
                                             bee_terms = c("Bombus_W", "Bombus_A",
                                                           "bee_ardens_ns", "bee_diversus_ns",
                                                           "bee_beaticola_ns", "bee_consobrinus_ns",
                                                           "bee_honshuensis_ns"), k_space = 40) {
  require_packages("mgcv")
  output <- list()
  details <- list()
  z <- 0L
  base_terms <- c("region", env_terms)
  for (bee in bee_terms) {
    if (!(bee %in% names(data)) || stats::sd(data[[bee]], na.rm = TRUE) == 0) next
    complete <- stats::complete.cases(data[c(response, base_terms, bee, "x_km", "y_km", "spatial_fold")])
    d <- data[complete, , drop = FALSE]
    k_use <- max(15L, min(as.integer(k_space), floor(nrow(d) / 20)))

    env_formula <- stats::reformulate(c(base_terms, bee), response = response)
    env_fit <- stats::lm(env_formula, data = d)
    env_tab <- summary(env_fit)$coefficients
    z <- z + 1L
    output[[z]] <- data.frame(
      model = "environment_only", predictor = bee, term = bee,
      estimate = env_tab[bee, 1], se = env_tab[bee, 2], statistic = env_tab[bee, 3],
      p_value = env_tab[bee, 4], residual_sd_fraction = NA_real_, heldout_R2 = NA_real_
    )

    spatial_formula <- make_spatial_formula(response, c(base_terms, bee), k_use)
    spatial_fit <- mgcv::gam(spatial_formula, data = d, method = "REML")
    z <- z + 1L
    output[[z]] <- cbind(
      coefficient_row(spatial_fit, bee, "environment_plus_spatial", bee),
      residual_sd_fraction = NA_real_, heldout_R2 = NA_real_
    )

    bee_cf <- crossfit_gam(d, bee, base_terms, k_space = k_use)
    residual_name <- paste0(bee, "_spatialplus")
    d[[residual_name]] <- d[[bee]] - bee_cf$predictions
    spatialplus_formula <- make_spatial_formula(response, c(base_terms, residual_name), k_use)
    spatialplus_fit <- mgcv::gam(spatialplus_formula, data = d, method = "REML")
    heldout_r2 <- 1 - sum((d[[bee]] - bee_cf$predictions)^2) /
      sum((d[[bee]] - mean(d[[bee]]))^2)
    z <- z + 1L
    output[[z]] <- cbind(
      coefficient_row(spatialplus_fit, residual_name, "spatial_plus_crossfit", bee),
      residual_sd_fraction = stats::sd(d[[residual_name]]) / stats::sd(d[[bee]]),
      heldout_R2 = heldout_r2
    )
    details[[bee]] <- bee_cf$log
  }
  list(summary = do.call(rbind, output), crossfit_logs = details)
}

fit_horticultural_qgams <- function(data, response, natural_terms, taus = c(0.5, 0.75, 0.9, 0.95),
                                    k_space = 40, keep_fits = FALSE) {
  require_packages(c("qgam", "mgcv"))
  if (!any(is.finite(data$z_H))) {
    return(list(coefficients = data.frame(), region_slopes = data.frame(),
                contrasts = data.frame(), warnings = data.frame(), formula = NULL))
  }
  linear <- c(natural_terms, "z_H", "region:z_H")
  k_use <- max(15L, min(as.integer(k_space), floor(nrow(data) / 20)))
  form <- make_spatial_formula(response, linear, k_use)
  fits <- if (keep_fits) list() else NULL
  coef_rows <- list()
  slope_rows <- list()
  contrasts <- list()
  warning_rows <- list()
  for (i in seq_along(taus)) {
    tau <- taus[i]
    warnings <- character()
    fit <- withCallingHandlers(
      qgam::qgam(form, data = data, qu = tau, argGam = list(method = "REML")),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    warning_rows[[i]] <- data.frame(
      tau = tau, warnings = paste(unique(warnings), collapse = " | "),
      stringsAsFactors = FALSE
    )
    if (keep_fits) fits[[as.character(tau)]] <- fit
    tab <- summary(fit)$p.table
    wanted <- intersect(c("z_H", "regionEast:z_H"), rownames(tab))
    coef_rows[[i]] <- data.frame(
      tau = tau, term = wanted, estimate = tab[wanted, 1], se = tab[wanted, 2],
      statistic = tab[wanted, 3], p_value = tab[wanted, 4], row.names = NULL
    )
    beta <- stats::coef(fit)
    V <- stats::vcov(fit)
    h_term <- "z_H"
    interaction_term <- "regionEast:z_H"
    slope_for_region <- function(reg) {
      weights <- setNames(numeric(length(beta)), names(beta))
      weights[h_term] <- 1
      if (identical(reg, "East") && interaction_term %in% names(weights)) {
        weights[interaction_term] <- 1
      }
      estimate <- sum(weights * beta)
      se <- sqrt(drop(t(weights) %*% V %*% weights))
      data.frame(
        tau = tau, region = reg, estimate = estimate, se = se,
        lower_95 = estimate - stats::qnorm(0.975) * se,
        upper_95 = estimate + stats::qnorm(0.975) * se,
        statistic = estimate / se,
        p_value = 2 * stats::pnorm(-abs(estimate / se))
      )
    }
    slopes_i <- do.call(rbind, lapply(levels(data$region), slope_for_region))
    slope_rows[[i]] <- slopes_i
    delta_h <- diff(stats::quantile(data$z_H, c(0.1, 0.9), na.rm = TRUE))
    for (reg in levels(data$region)) {
      idx <- which(data$region == reg)
      slope <- slopes_i[slopes_i$region == reg, , drop = FALSE]
      contrasts[[length(contrasts) + 1L]] <- data.frame(
        tau = tau, region = reg, contrast = "H90_minus_H10",
        estimate = slope$estimate * delta_h,
        se = slope$se * delta_h,
        lower_95 = slope$lower_95 * delta_h,
        upper_95 = slope$upper_95 * delta_h,
        p_value = slope$p_value,
        n_reference = length(idx)
      )
    }
  }
  out <- list(
    coefficients = do.call(rbind, coef_rows),
    region_slopes = do.call(rbind, slope_rows),
    contrasts = do.call(rbind, contrasts),
    warnings = do.call(rbind, warning_rows),
    formula = form
  )
  if (keep_fits) out$fits <- fits
  out
}

pareto_front <- function(x) {
  x <- as.matrix(x)
  keep <- stats::complete.cases(x)
  out <- rep(FALSE, nrow(x))
  valid <- which(keep)
  for (ii in seq_along(valid)) {
    i <- valid[ii]
    dominated <- apply(x[valid, , drop = FALSE], 1, function(v) all(v >= x[i, ]) && any(v > x[i, ]))
    out[i] <- !any(dominated)
  }
  out
}

build_candidate_table <- function(data) {
  data$upper_excess_score <- rank01(data$upper_excess)
  data$early_score <- rank01(-data$early_anomaly)
  data$H_score <- rank01(data$z_H)
  data$R_score <- rank01(data$z_R)
  data$A_score <- rank01(data$z_A)
  data$upper_flag <- data$upper_excess > 0

  available <- c(
    upper_excess_score = any(is.finite(data$upper_excess_score)),
    early_score = any(is.finite(data$early_score)),
    H_score = any(is.finite(data$H_score)),
    R_score = any(is.finite(data$R_score)),
    A_score = any(is.finite(data$A_score))
  )
  axes <- names(available)[available]
  data$pareto_front <- FALSE
  idx <- which(data$upper_flag)
  if (length(idx) && length(axes) >= 2) {
    data$pareto_front[idx] <- pareto_front(data[idx, axes, drop = FALSE])
  }
  data$evidence_tier <- 0L
  data$evidence_tier[data$upper_flag] <- 1L
  data$evidence_tier[data$upper_flag & data$early_score >= 0.9] <- 2L
  data$evidence_tier[
    data$upper_flag & data$early_score >= 0.9 & data$H_score >= 0.9
  ] <- 3L
  data$robust_candidate <- data$evidence_tier >= 3L
  list(data = data, available_axes = data.frame(axis = names(available), available = unname(available)))
}

fit_one_inla_spde <- function(data, response, terms, model_name, mesh = NULL,
                              spde = NULL, family = "gaussian") {
  require_packages("INLA")
  coords <- as.matrix(data[c("x_km", "y_km")])
  if (is.null(mesh)) {
    mesh <- INLA::inla.mesh.2d(loc = coords, max.edge = c(20, 100), cutoff = 5)
  }
  if (is.null(spde)) {
    spde <- INLA::inla.spde2.pcmatern(
      mesh, alpha = 2,
      prior.range = c(100, 0.05),
      prior.sigma = c(1, 0.05)
    )
  }
  A <- INLA::inla.spde.make.A(mesh, loc = coords)
  X <- stats::model.matrix(stats::reformulate(terms), data = data)
  colnames(X)[colnames(X) == "(Intercept)"] <- "Intercept"
  X <- as.data.frame(X, check.names = FALSE)
  stack <- INLA::inla.stack(
    data = list(y = data[[response]]),
    A = list(A, 1),
    effects = list(spatial = seq_len(spde$n.spde), X = X),
    tag = "est"
  )
  rhs <- c("-1", colnames(X), "f(spatial, model = spde)")
  formula <- stats::as.formula(paste("y ~", paste(rhs, collapse = " + ")))
  fit <- INLA::inla(
    formula,
    data = INLA::inla.stack.data(stack),
    family = family,
    control.predictor = list(A = INLA::inla.stack.A(stack), compute = TRUE),
    # Posterior configurations are not consumed by this pipeline and are large
    # for a national mesh. Use INLA's tested platform default for threading;
    # forcing "4:1" caused result-collection failures on Windows INLA 25.10.19.
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = FALSE),
    # Windows INLA 25.10.19 devel reproducibly failed while collecting results
    # in quiet mode for this mesh, while the identical verbose call succeeded.
    # Capture stdout/stderr at the runner level to retain an auditable log.
    verbose = TRUE
  )
  idx <- INLA::inla.stack.index(stack, "est")$data
  metrics <- data.frame(
    model = model_name, family = family, n = nrow(data), mesh_vertices = mesh$n,
    DIC = fit$dic$dic, WAIC = fit$waic$waic, pWAIC = fit$waic$p.eff,
    mean_neglogCPO = mean(-log(fit$cpo$cpo[idx]), na.rm = TRUE),
    n_CPO_nonfinite = sum(!is.finite(fit$cpo$cpo[idx])), stringsAsFactors = FALSE
  )
  fixed <- as.data.frame(fit$summary.fixed)
  fixed$term <- rownames(fixed)
  fixed$model <- model_name
  rownames(fixed) <- NULL
  hyper <- as.data.frame(fit$summary.hyperpar)
  hyper$hyperparameter <- rownames(hyper)
  hyper$model <- model_name
  rownames(hyper) <- NULL
  list(fit = fit, metrics = metrics, fixed = fixed, hyper = hyper,
       mesh = mesh, spde = spde, formula = formula)
}

fit_inla_model_set <- function(data, response, env_terms) {
  require_packages("INLA")
  fit_shared_set <- function(d, terms_list, comparison_set) {
    coords <- as.matrix(d[c("x_km", "y_km")])
    mesh <- INLA::inla.mesh.2d(loc = coords, max.edge = c(20, 100), cutoff = 5)
    spde <- INLA::inla.spde2.pcmatern(
      mesh, alpha = 2,
      prior.range = c(100, 0.05),
      prior.sigma = c(1, 0.05)
    )
    out <- lapply(names(terms_list), function(nm) {
      message("[INLA] fitting ", comparison_set, " / ", nm,
              " (n = ", nrow(d), ", mesh = ", mesh$n, ")")
      result <- fit_one_inla_spde(d, response, terms_list[[nm]], nm, mesh, spde)
      message("[INLA] completed ", comparison_set, " / ", nm)
      result
    })
    names(out) <- names(terms_list)
    for (nm in names(out)) out[[nm]]$metrics$comparison_set <- comparison_set
    out
  }
  national_terms <- list(
    N_environment_space = c("region", env_terms),
    N_W_space_prespecified = c("region", env_terms, "Bombus_W")
  )
  fits <- fit_shared_set(data, national_terms, "national_W_support")

  alpine <- data[is.finite(data$Bombus_A), , drop = FALSE]
  region_n <- table(alpine$region)
  if (nrow(alpine) >= 100 && any(region_n < 20)) {
    dominant <- names(which.max(region_n))
    alpine <- alpine[alpine$region == dominant, , drop = FALSE]
  }
  alpine$region <- droplevels(alpine$region)
  alpine_base <- if (nlevels(alpine$region) > 1L) c("region", env_terms) else env_terms
  alpine_terms <- list(
    A_environment_space = alpine_base,
    A_W_space = c(alpine_base, "Bombus_W"),
    A_alpine_space = c(alpine_base, "Bombus_A"),
    A_W_alpine_space_prespecified = c(alpine_base, "Bombus_W", "Bombus_A")
  )
  if (nrow(alpine) >= 100) {
    alpine_fits <- fit_shared_set(alpine, alpine_terms, "alpine_common_support")
    fits <- c(fits, alpine_fits)
  }

  metrics <- do.call(rbind, lapply(fits, `[[`, "metrics"))
  metrics$delta_WAIC_within_set <- ave(
    metrics$WAIC, metrics$comparison_set, FUN = function(x) x - x[1]
  )
  list(
    fits = fits,
    metrics = metrics,
    fixed = do.call(rbind, lapply(fits, `[[`, "fixed")),
    hyper = do.call(rbind, lapply(fits, `[[`, "hyper")),
    model_terms = c(national_terms, alpine_terms),
    alpine_support_n = nrow(alpine)
  )
}

run_ecological_analysis_v2 <- function(anomaly_csv, raw_colour_csv, bombus_dir, output_dir,
                                       raster_paths = list(), strict_qc = FALSE,
                                       east_west_cut = 136.5, block_km = 50,
                                       folds = 5, run_inla = TRUE, k_space = 40,
                                       author_review_confirmed = TRUE) {
  require_packages(c("sf", "terra", "mgcv", "qgam", "jsonlite"))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  anomaly <- utils::read.csv(anomaly_csv, check.names = FALSE, stringsAsFactors = FALSE)
  raw <- utils::read.csv(raw_colour_csv, check.names = FALSE, stringsAsFactors = FALSE)
  prepared <- make_analysis_data(
    anomaly, raw, bombus_dir, raster_paths, strict_qc,
    east_west_cut, block_km, folds, author_review_confirmed
  )
  dat <- prepared$data
  env <- prepared$environment_terms
  natural_terms <- c("region", env, "Bombus_W")

  bombus_files <- file.path(
    bombus_dir,
    paste0(c("ardens", "beaticola", "consobrinus", "diversus", "honshuensis"), ".tif")
  )
  bombus_manifest <- data.frame(
    species = tools::file_path_sans_ext(basename(bombus_files)),
    path = normalizePath(bombus_files, winslash = "/", mustWork = TRUE),
    bytes = file.info(bombus_files)$size,
    md5 = unname(tools::md5sum(bombus_files)),
    stringsAsFactors = FALSE
  )
  write_csv_safe(bombus_manifest, file.path(output_dir, "bombus_prediction_manifest.csv"))
  selection_file <- file.path(dirname(bombus_dir), "ENMeval_AICc_selection.csv")
  if (file.exists(selection_file)) {
    selection <- utils::read.csv(selection_file, check.names = FALSE, stringsAsFactors = FALSE)
    write_csv_safe(selection, file.path(output_dir, "bombus_enmeval_selection.csv"))
  }

  write_csv_safe(prepared$qc_audit, file.path(output_dir, "colour_qc_audit.csv"))
  write_csv_safe(data.frame(
    field = c("review_scope", "review_status", "raw_csv_pending_interpretation"),
    value = c(
      "flower colour extraction region after subject extraction/cropping",
      if (author_review_confirmed) "author_confirmed" else "not_asserted",
      "legacy pipeline metadata; not treated as missing or failed measurement"
    )
  ), file.path(output_dir, "manual_review_provenance.csv"))
  write_csv_safe(prepared$raster_registry, file.path(output_dir, "horticultural_axis_registry.csv"))
  write_csv_safe(
    as.data.frame(table(dat$region, dat$spatial_fold), stringsAsFactors = FALSE),
    file.path(output_dir, "spatial_fold_counts.csv")
  )
  bombus_support <- do.call(rbind, lapply(
    c("bee_ardens", "bee_diversus", "bee_beaticola", "bee_consobrinus",
      "bee_honshuensis", "Bombus_W", "Bombus_A"),
    function(v) do.call(rbind, lapply(levels(dat$region), function(reg) {
      idx <- dat$region == reg
      data.frame(
        predictor = v, region = reg, n_total = sum(idx),
        n_evaluated = sum(is.finite(dat[[v]][idx])),
        proportion_evaluated = mean(is.finite(dat[[v]][idx]))
      )
    }))
  ))
  write_csv_safe(bombus_support, file.path(output_dir, "bombus_prediction_support.csv"))
  environment_vif <- compute_vif(dat, env)
  write_csv_safe(environment_vif, file.path(output_dir, "environment_vif_diagnostic.csv"))
  if (max(environment_vif$VIF, na.rm = TRUE) >= 5) {
    stop(
      "Prespecified environmental terms still have VIF >= 5; revise the environmental axes before inference.",
      call. = FALSE
    )
  }
  write_csv_safe(compute_vif(dat, c(env, "Bombus_W", "Bombus_A")),
                 file.path(output_dir, "environment_bombus_vif_diagnostic_only.csv"))

  natural_cf <- crossfit_qgam(
    dat, "response", natural_terms, taus = c(0.5, 0.95), k_space = k_space
  )
  dat$q50 <- natural_cf$predictions$q50
  dat$q95 <- natural_cf$predictions$q95
  dat$quantile_crossing <- dat$q95 < dat$q50
  dat$upper_excess <- dat$response - dat$q95
  dat$median_anomaly <- dat$response - dat$q50
  write_csv_safe(natural_cf$log, file.path(output_dir, "crossfit_natural_qgam_log.csv"))
  calibration <- calibrate_upper_tail(dat)
  write_csv_safe(calibration, file.path(output_dir, "upper_tail_calibration.csv"))

  early_terms <- c("region", env, if (any(is.finite(dat$z_N))) "z_N")
  early_complete <- is.finite(dat$DOY) & is.finite(dat$year)
  dat$early_expected <- NA_real_
  if (sum(early_complete) >= 100) {
    early_cf <- crossfit_gam(
      dat[early_complete, , drop = FALSE], "DOY", early_terms,
      k_space = k_space, include_year = TRUE
    )
    dat$early_expected[early_complete] <- early_cf$predictions
    write_csv_safe(early_cf$log, file.path(output_dir, "crossfit_phenology_gam_log.csv"))
  }
  dat$early_anomaly <- dat$DOY - dat$early_expected

  confounding <- fit_spatial_confounding_envelope(dat, "response", env, k_space = k_space)
  write_csv_safe(confounding$summary, file.path(output_dir, "bombus_spatial_confounding_envelope.csv"))

  horticulture <- fit_horticultural_qgams(dat, "response", natural_terms, k_space = k_space)
  write_csv_safe(horticulture$coefficients, file.path(output_dir, "raw_colour_horticultural_qgam_coefficients.csv"))
  write_csv_safe(horticulture$region_slopes, file.path(output_dir, "raw_colour_horticultural_H_region_slopes.csv"))
  write_csv_safe(horticulture$contrasts, file.path(output_dir, "raw_colour_horticultural_H_contrasts.csv"))

  candidates <- build_candidate_table(dat)
  dat <- candidates$data
  write_csv_safe(candidates$available_axes, file.path(output_dir, "candidate_axis_availability.csv"))

  inla_results <- NULL
  if (run_inla) {
    invisible(gc())
    inla_results <- fit_inla_model_set(dat, "response", env)
    write_csv_safe(inla_results$metrics, file.path(output_dir, "inla_pcspde_model_comparison.csv"))
    write_csv_safe(inla_results$fixed, file.path(output_dir, "inla_pcspde_fixed_effects.csv"))
    write_csv_safe(inla_results$hyper, file.path(output_dir, "inla_pcspde_hyperparameters.csv"))
  }

  id_cols <- intersect(c("sample_id", "observation_id", "exact_site_id", "col_hex"), names(dat))
  candidate_cols <- unique(c(
    id_cols, "longitude", "latitude", "region", "spatial_block", "spatial_fold",
    "response", "colour_a", "Pigment", "q50", "q95", "quantile_crossing",
    "upper_excess", "median_anomaly", "DOY", "early_expected", "early_anomaly",
    "analysis_manual_review_status", "qc_flags",
    "z_H", "z_R", "z_N", "z_A", "Bombus_W", "Bombus_A",
    "upper_excess_score", "early_score", "H_score", "R_score", "A_score",
    "upper_flag", "pareto_front", "evidence_tier", "robust_candidate"
  ))
  candidate_cols <- intersect(candidate_cols, names(dat))
  write_csv_safe(dat[candidate_cols], file.path(output_dir, "crossfitted_anomalies_and_candidates.csv"))
  write_csv_safe(dat, file.path(output_dir, "analysis_data.csv"))

  summary_values <- list(
    n = nrow(dat), no_warning_sensitivity = strict_qc,
    author_review_confirmed = author_review_confirmed,
    east_west_cut = east_west_cut, block_km = block_km, folds = folds,
    upper_exceedance_rate = mean(dat$upper_flag),
    q95_coverage = mean(dat$response <= dat$q95),
    quantile_crossing_rate = mean(dat$quantile_crossing),
    robust_candidate_n = sum(dat$robust_candidate),
    pareto_candidate_n = sum(dat$pareto_front),
    available_horticultural_axes = prepared$raster_registry$axis[prepared$raster_registry$available],
    anomaly_input_md5 = unname(tools::md5sum(anomaly_csv)),
    raw_colour_input_md5 = unname(tools::md5sum(raw_colour_csv)),
    response = "standardized CIELAB a*",
    anomaly_role = "cross-fitted candidate detection only",
    inla_run = run_inla
  )
  jsonlite::write_json(summary_values, file.path(output_dir, "run_summary.json"),
                       pretty = TRUE, auto_unbox = TRUE, na = "null")

  lines <- c(
    "# Ecological analysis v2: one-run summary",
    "",
    sprintf(paste0("- Primary response: standardized CIELAB a* (n = %d after excluding exact duplicates/",
                   "non-formable measurements and environmental complete-case filtering)."), nrow(dat)),
    sprintf("- Cross-fitted Q0.95 empirical coverage: %.3f; upper exceedance rate: %.3f.",
            mean(dat$response <= dat$q95), mean(dat$upper_flag)),
    sprintf("- Quantile crossing rate (Q0.95 < Q0.50): %.3f.", mean(dat$quantile_crossing)),
    sprintf("- Tier-3 candidates (upper excess + locally early + high H): %d.", sum(dat$robust_candidate)),
    sprintf("- Pareto-front upper-tail candidates: %d.", sum(dat$pareto_front)),
    paste0("- Public horticultural axes available in this run: ",
           paste(prepared$raster_registry$axis[prepared$raster_registry$available], collapse = ", "), "."),
    "- Residual/anomaly outputs are candidate detectors, not evidence of horticultural origin.",
    "- Horticultural inference uses raw colour qGAMs; no weighted candidate score is used."
  )
  writeLines(lines, file.path(output_dir, "ANALYSIS_SUMMARY.md"), useBytes = TRUE)

  invisible(list(
    data = dat, prepared = prepared, natural_crossfit = natural_cf,
    calibration = calibration, confounding = confounding,
    horticulture = horticulture, inla = inla_results,
    summary = summary_values
  ))
}
