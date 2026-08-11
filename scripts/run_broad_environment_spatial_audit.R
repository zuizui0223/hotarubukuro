#!/usr/bin/env Rscript

# Comprehensive observation-level broad-model audit for the current JBI paper.
#
# The audit preserves the current two-part response and INLA-SPDE framework.
# It asks:
#   (1) whether omitted, publicly available environmental proxies improve
#       transfer beyond the current eight-axis additive model;
#   (2) which seasonality interaction is the parsimonious conditional-intensity
#       extension after the mechanism and all-pairwise screens; and
#   (3) whether the current stationary Euclidean SPDE + East/West adjustment is
#       preferable to no-region, repeated-site and ocean-barrier alternatives.
#
# This script does not replace the separate 1-km-cell cross-fitted natural
# reference used to define the 17 local-departure targets.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  if (hit[length(hit)] == length(args)) {
    stop("Missing value after ", flag, call. = FALSE)
  }
  args[hit[length(hit)] + 1L]
}

input_csv <- arg_value(
  "--input-csv",
  "reference-artifact/results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
)
landscape_csv <- arg_value(
  "--landscape-csv",
  "reference-artifact/results/ecological_v19_human_landscape_extremes/landscape_cell_features.csv"
)
raster_dir <- arg_value("--raster-dir", "data/processed/rasters")
output_root <- arg_value("--output-dir", "results/broad_environment_spatial_audit")
outcome_filter <- arg_value("--outcome", "all")
bootstrap_reps <- as.integer(arg_value("--bootstrap-reps", "4000"))
seed <- as.integer(arg_value("--seed", "20260814"))

required_packages <- c("INLA", "Matrix", "jsonlite", "terra", "sf")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}
if (!file.exists(input_csv)) stop("Input CSV not found: ", input_csv, call. = FALSE)
if (!file.exists(landscape_csv)) {
  stop("Landscape CSV not found: ", landscape_csv, call. = FALSE)
}
if (!dir.exists(raster_dir)) stop("Raster directory not found: ", raster_dir, call. = FALSE)
if (!is.finite(bootstrap_reps) || bootstrap_reps < 1000L) {
  stop("--bootstrap-reps must be at least 1000", call. = FALSE)
}

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1",
  GOTO_NUM_THREADS = "1",
  BLIS_NUM_THREADS = "1",
  INLA_NUM_THREADS = "1",
  OMP_DYNAMIC = "FALSE"
)
set.seed(seed)

write_csv_to <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

rbind_nonempty <- function(items) {
  items <- items[
    vapply(items, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))
  ]
  if (!length(items)) return(data.frame())
  columns <- unique(unlist(lapply(items, names), use.names = FALSE))
  items <- lapply(items, function(x) {
    for (column in setdiff(columns, names(x))) x[[column]] <- NA
    x[columns]
  })
  do.call(rbind, items)
}

safe_z <- function(x, center = NULL, scale = NULL) {
  x <- as.numeric(x)
  keep <- is.finite(x)
  out <- rep(NA_real_, length(x))
  if (!any(keep)) return(out)
  if (is.null(center)) center <- mean(x[keep])
  if (is.null(scale)) scale <- stats::sd(x[keep])
  if (!is.finite(scale) || scale == 0) scale <- 1
  out[keep] <- (x[keep] - center) / scale
  attr(out, "center") <- center
  attr(out, "scale") <- scale
  out
}

binary_auc <- function(y, p) {
  keep <- is.finite(y) & is.finite(p)
  y <- as.integer(y[keep])
  p <- as.numeric(p[keep])
  n1 <- sum(y == 1L)
  n0 <- sum(y == 0L)
  if (!n1 || !n0) return(NA_real_)
  ranks <- rank(p, ties.method = "average")
  (sum(ranks[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

prediction_metrics <- function(y, pred, family) {
  keep <- is.finite(y) & is.finite(pred)
  y <- as.numeric(y[keep])
  pred <- as.numeric(pred[keep])
  if (family == "binomial") {
    pred <- pmin(pmax(pred, 1e-10), 1 - 1e-10)
    loss <- -mean(y * log(pred) + (1 - y) * log(1 - pred))
    data.frame(
      n = length(y), primary_loss = loss, log_loss = loss,
      brier = mean((y - pred)^2), AUC = binary_auc(y, pred),
      RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_,
      stringsAsFactors = FALSE
    )
  } else {
    mse <- mean((y - pred)^2)
    denom <- sum((y - mean(y))^2)
    data.frame(
      n = length(y), primary_loss = mse,
      log_loss = NA_real_, brier = NA_real_, AUC = NA_real_,
      RMSE = sqrt(mse), MAE = mean(abs(y - pred)),
      R2 = if (denom > 0) 1 - sum((y - pred)^2) / denom else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}

cluster_bootstrap_gain <- function(loss_gain, clusters, reps, seed_offset = 0L) {
  keep <- is.finite(loss_gain) & !is.na(clusters)
  loss_gain <- as.numeric(loss_gain[keep])
  clusters <- as.character(clusters[keep])
  unique_clusters <- unique(clusters)
  if (!length(unique_clusters)) {
    return(data.frame(
      mean_gain = NA_real_, lower_95 = NA_real_, upper_95 = NA_real_,
      probability_positive = NA_real_
    ))
  }
  set.seed(seed + as.integer(seed_offset))
  draws <- numeric(reps)
  for (b in seq_len(reps)) {
    sampled <- sample(unique_clusters, length(unique_clusters), replace = TRUE)
    idx <- unlist(lapply(sampled, function(cl) which(clusters == cl)), use.names = FALSE)
    draws[b] <- mean(loss_gain[idx])
  }
  data.frame(
    mean_gain = mean(loss_gain),
    lower_95 = unname(stats::quantile(draws, 0.025, na.rm = TRUE)),
    upper_95 = unname(stats::quantile(draws, 0.975, na.rm = TRUE)),
    probability_positive = mean(draws > 0, na.rm = TRUE)
  )
}

compute_vif <- function(data, terms, include_region = TRUE) {
  terms <- unique(terms[terms %in% names(data)])
  rhs <- terms
  if (include_region) rhs <- c("region", rhs)
  if (!length(rhs)) return(data.frame(term = character(), VIF = numeric()))
  model_matrix <- stats::model.matrix(
    stats::as.formula(paste("~", paste(rhs, collapse = " + "))), data = data
  )
  model_matrix <- model_matrix[, colnames(model_matrix) != "(Intercept)", drop = FALSE]
  if (!ncol(model_matrix)) return(data.frame(term = character(), VIF = numeric()))
  rows <- lapply(seq_len(ncol(model_matrix)), function(index) {
    y <- model_matrix[, index]
    x <- model_matrix[, -index, drop = FALSE]
    if (!ncol(x) || stats::sd(y) == 0) {
      return(data.frame(term = colnames(model_matrix)[index], VIF = 1))
    }
    fit <- stats::lm.fit(cbind(1, x), y)
    r2 <- 1 - sum(fit$residuals^2) / sum((y - mean(y))^2)
    data.frame(term = colnames(model_matrix)[index], VIF = 1 / pmax(1 - r2, 1e-10))
  })
  do.call(rbind, rows)
}

raster_path <- function(name) {
  path <- file.path(raster_dir, name)
  if (!file.exists(path) || file.info(path)$size <= 0) {
    stop("Required processed raster not found: ", path, call. = FALSE)
  }
  path
}

extract_raster <- function(data, path, name, method = "bilinear") {
  raster <- terra::rast(path)
  points <- terra::vect(data, geom = c("longitude", "latitude"), crs = "EPSG:4326")
  values <- terra::extract(raster, points, method = method)
  data[[name]] <- as.numeric(values[, ncol(values)])
  data
}

compute_coast_distance <- function(data, elevation_path) {
  elevation <- terra::rast(elevation_path)
  sea_cells <- terra::ifel(is.na(elevation), 1, NA)
  distance_to_sea <- terra::distance(sea_cells)
  points <- terra::vect(data, geom = c("longitude", "latitude"), crs = "EPSG:4326")
  values <- terra::extract(distance_to_sea, points, method = "bilinear")
  as.numeric(values[, ncol(values)]) / 1000
}

base_terms <- c(
  "env_Temperature_PC1", "env_precip_PC1", "env_TemperatureSeasonality",
  "env_PrecipSeasonality", "env_topo_PC1", "env_soil_PC1",
  "env_soil_PC2", "env_RSDS"
)
extended_terms <- c(
  "env_VPD", "env_SWB", "env_BIO6", "env_BIO13",
  "env_forest_fraction", "env_coast_distance"
)
observation_control_terms <- c(
  "env_DOY", "env_year2024", "env_year2025",
  "env_mask_fraction", "env_overexposure"
)

interaction_registry <- data.frame(
  interaction_id = c(
    "thermal_variability", "moisture_thermal_variability",
    "dryness_precip_variability", "dryness_radiation"
  ),
  term = c(
    "int_thermal_variability", "int_moisture_thermal_variability",
    "int_dryness_precip_variability", "int_dryness_radiation"
  ),
  x = c(
    "env_Temperature_PC1", "env_precip_PC1",
    "env_precip_PC1", "env_precip_PC1"
  ),
  y = c(
    "env_TemperatureSeasonality", "env_TemperatureSeasonality",
    "env_PrecipSeasonality", "env_RSDS"
  ),
  x_sign = c(1, 1, -1, -1), y_sign = c(1, 1, 1, 1),
  stringsAsFactors = FALSE
)

state_specs <- list(
  state_additive = list(extra = character(), interactions = character()),
  state_dryness_radiation = list(extra = character(), interactions = "dryness_radiation"),
  state_add_vpd = list(extra = "env_VPD", interactions = character()),
  state_add_swb = list(extra = "env_SWB", interactions = character()),
  state_hydro_demand_balance = list(extra = c("env_VPD", "env_SWB"), interactions = character()),
  state_add_bio6 = list(extra = "env_BIO6", interactions = character()),
  state_add_bio13 = list(extra = "env_BIO13", interactions = character()),
  state_climate_extremes = list(extra = c("env_BIO6", "env_BIO13"), interactions = character()),
  state_add_forest = list(extra = "env_forest_fraction", interactions = character()),
  state_add_coast = list(extra = "env_coast_distance", interactions = character()),
  state_habitat_context = list(extra = c("env_forest_fraction", "env_coast_distance"), interactions = character()),
  state_observation_time = list(extra = c("env_DOY", "env_year2024", "env_year2025"), interactions = character()),
  state_image_qc = list(extra = c("env_mask_fraction", "env_overexposure"), interactions = character()),
  state_observation_controls = list(extra = observation_control_terms, interactions = character()),
  state_full_extended = list(extra = extended_terms, interactions = character()),
  state_full_extended_dryrad = list(extra = extended_terms, interactions = "dryness_radiation")
)

intensity_specs <- list(
  intensity_additive = list(extra = character(), interactions = character()),
  intensity_thermal_variability = list(extra = character(), interactions = "thermal_variability"),
  intensity_moisture_thermal_variability = list(extra = character(), interactions = "moisture_thermal_variability"),
  intensity_joint_seasonality = list(extra = character(), interactions = c("thermal_variability", "moisture_thermal_variability")),
  intensity_stress_variability_bundle = list(extra = character(), interactions = c("thermal_variability", "dryness_precip_variability")),
  intensity_joint_core_plus_precip_variability = list(extra = character(), interactions = c("thermal_variability", "moisture_thermal_variability", "dryness_precip_variability")),
  intensity_thermal_add_vpd = list(extra = "env_VPD", interactions = "thermal_variability"),
  intensity_thermal_add_swb = list(extra = "env_SWB", interactions = "thermal_variability"),
  intensity_thermal_hydro = list(extra = c("env_VPD", "env_SWB"), interactions = "thermal_variability"),
  intensity_thermal_add_bio6 = list(extra = "env_BIO6", interactions = "thermal_variability"),
  intensity_thermal_add_bio13 = list(extra = "env_BIO13", interactions = "thermal_variability"),
  intensity_thermal_extremes = list(extra = c("env_BIO6", "env_BIO13"), interactions = "thermal_variability"),
  intensity_thermal_add_forest = list(extra = "env_forest_fraction", interactions = "thermal_variability"),
  intensity_thermal_add_coast = list(extra = "env_coast_distance", interactions = "thermal_variability"),
  intensity_thermal_habitat = list(extra = c("env_forest_fraction", "env_coast_distance"), interactions = "thermal_variability"),
  intensity_thermal_observation_time = list(extra = c("env_DOY", "env_year2024", "env_year2025"), interactions = "thermal_variability"),
  intensity_thermal_image_qc = list(extra = c("env_mask_fraction", "env_overexposure"), interactions = "thermal_variability"),
  intensity_thermal_observation_controls = list(extra = observation_control_terms, interactions = "thermal_variability"),
  intensity_thermal_full_extended = list(extra = extended_terms, interactions = "thermal_variability")
)

spatial_specs <- data.frame(
  spatial_spec = c(
    "stationary_region", "stationary_no_region",
    "stationary_region_site", "stationary_no_region_site",
    "barrier_region", "barrier_no_region",
    "barrier_region_site", "barrier_no_region_site"
  ),
  spatial_model = c(rep("stationary", 4), rep("barrier", 4)),
  include_region = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
  include_site = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

raw <- utils::read.csv(input_csv, check.names = FALSE, stringsAsFactors = FALSE)
landscape <- utils::read.csv(landscape_csv, check.names = FALSE, stringsAsFactors = FALSE)
landscape <- landscape[!duplicated(landscape$exact_site_id), c("exact_site_id", "forest_fraction")]
raw$.input_order <- seq_len(nrow(raw))
raw <- merge(raw, landscape, by = "exact_site_id", all.x = TRUE, sort = FALSE)
raw <- raw[order(raw$.input_order), , drop = FALSE]
raw$.input_order <- NULL
raw$region <- factor(raw$region, levels = c("West", "East"))

raw <- extract_raster(raw, raster_path("chelsa_vpdmean.tif"), "VPD")
raw <- extract_raster(raw, raster_path("chelsa_swb.tif"), "SWB")
raw <- extract_raster(raw, raster_path("chelsa_bio06.tif"), "BIO6")
raw <- extract_raster(raw, raster_path("chelsa_bio13.tif"), "BIO13")
elevation_path <- raster_path("elevation_30s.tif")
raw$coast_distance_km <- compute_coast_distance(raw, elevation_path)

raw$env_VPD <- raw$VPD
raw$env_SWB <- raw$SWB
raw$env_BIO6 <- raw$BIO6
raw$env_BIO13 <- raw$BIO13
raw$env_forest_fraction <- raw$forest_fraction
raw$env_coast_distance <- log1p(raw$coast_distance_km)
raw$env_DOY <- as.numeric(raw$DOY)
raw$env_year2024 <- as.integer(raw$year == 2024)
raw$env_year2025 <- as.integer(raw$year == 2025)
raw$env_mask_fraction <- as.numeric(raw$mask_fraction_visible)
raw$env_overexposure <- as.integer(
  tolower(trimws(as.character(raw$possible_overexposure))) %in% c("1", "true", "yes", "y")
)

variable_names <- c(base_terms, extended_terms, observation_control_terms)
variable_audit <- do.call(rbind, lapply(variable_names, function(term) {
  values <- raw[[term]]
  finite <- is.finite(values)
  data.frame(
    variable = term, n_finite = sum(finite), n_missing = sum(!finite),
    minimum = if (any(finite)) min(values[finite]) else NA_real_,
    median = if (any(finite)) stats::median(values[finite]) else NA_real_,
    maximum = if (any(finite)) max(values[finite]) else NA_real_,
    stringsAsFactors = FALSE
  )
}))
write_csv_to(variable_audit, file.path(output_root, "variable_data_audit.csv"))
write_csv_to(interaction_registry, file.path(output_root, "interaction_registry.csv"))
write_csv_to(spatial_specs, file.path(output_root, "spatial_spec_registry.csv"))

make_analysis_data <- function(data, response, extra_terms, interaction_ids, training_fold = NULL) {
  required <- unique(c(
    response, "observation_id", "region", "x_km", "y_km",
    "exact_site_id", "spatial_fold", "spatial_block", base_terms, extra_terms
  ))
  data <- data[stats::complete.cases(data[required]), , drop = FALSE]
  training_rows <- if (is.null(training_fold)) rep(TRUE, nrow(data)) else data$spatial_fold != training_fold
  if (!any(training_rows)) stop("No training observations remain", call. = FALSE)

  scaling_rows <- list()
  for (term in unique(c(base_terms, extra_terms))) {
    center <- mean(data[[term]][training_rows], na.rm = TRUE)
    scale <- stats::sd(data[[term]][training_rows], na.rm = TRUE)
    if (!is.finite(scale) || scale == 0) scale <- 1
    data[[term]] <- safe_z(data[[term]], center, scale)
    scaling_rows[[length(scaling_rows) + 1L]] <- data.frame(
      variable = term, type = "main", center = center, scale = scale,
      stringsAsFactors = FALSE
    )
  }

  interaction_terms <- character()
  for (interaction_id in interaction_ids) {
    registry_row <- interaction_registry[
      interaction_registry$interaction_id == interaction_id, , drop = FALSE
    ]
    if (nrow(registry_row) != 1L) stop("Unknown interaction: ", interaction_id, call. = FALSE)
    product <- registry_row$x_sign * data[[registry_row$x]] *
      registry_row$y_sign * data[[registry_row$y]]
    center <- mean(product[training_rows], na.rm = TRUE)
    scale <- stats::sd(product[training_rows], na.rm = TRUE)
    if (!is.finite(scale) || scale == 0) scale <- 1
    data[[registry_row$term]] <- safe_z(product, center, scale)
    interaction_terms <- c(interaction_terms, registry_row$term)
    scaling_rows[[length(scaling_rows) + 1L]] <- data.frame(
      variable = registry_row$term, type = "interaction",
      center = center, scale = scale, stringsAsFactors = FALSE
    )
  }

  list(
    data = data,
    terms = unique(c(base_terms, extra_terms, interaction_terms)),
    training_rows = training_rows,
    prediction_rows = which(!training_rows),
    scaling = if (length(scaling_rows)) do.call(rbind, scaling_rows) else data.frame()
  )
}

build_mesh <- function(data) {
  coordinates <- as.matrix(data[c("x_km", "y_km")])
  mesh <- INLA::inla.mesh.2d(loc = coordinates, max.edge = c(20, 100), cutoff = 5)
  list(coords = coordinates, mesh = mesh)
}

barrier_triangles_from_elevation <- function(mesh, elevation_path) {
  triangles <- mesh$graph$tv
  centres <- t(apply(triangles, 1, function(index) {
    colMeans(mesh$loc[index, 1:2, drop = FALSE])
  }))
  japan_laea <- paste(
    "+proj=laea +lat_0=36 +lon_0=137 +x_0=0 +y_0=0",
    "+datum=WGS84 +units=m +no_defs"
  )
  centre_points <- sf::st_as_sf(
    data.frame(x = centres[, 1] * 1000, y = centres[, 2] * 1000),
    coords = c("x", "y"), crs = japan_laea
  )
  centre_points <- sf::st_transform(centre_points, 4326)
  elevation_values <- terra::extract(
    terra::rast(elevation_path), terra::vect(centre_points)
  )
  barrier <- which(!is.finite(as.numeric(elevation_values[, ncol(elevation_values)])))
  if (!length(barrier) || length(barrier) >= nrow(triangles)) {
    stop("Unable to identify a non-empty ocean barrier triangle set", call. = FALSE)
  }
  barrier
}

build_spatial_model <- function(mesh, spatial_model, barrier_triangles) {
  if (spatial_model == "stationary") {
    INLA::inla.spde2.pcmatern(
      mesh, alpha = 2, prior.range = c(100, 0.05), prior.sigma = c(1, 0.05)
    )
  } else {
    INLA::inla.barrier.pcmatern(
      mesh, barrier.triangles = barrier_triangles,
      prior.range = c(100, 0.05), prior.sigma = c(1, 0.05),
      range.fraction = 0.2
    )
  }
}

empty_fit_result <- function(model_id, spatial_spec, family, n_train, n_predict, mesh_vertices, error_message) {
  list(
    metrics = data.frame(
      model_id = model_id, spatial_spec = spatial_spec, family = family,
      n_train = n_train, n_predict = n_predict, mesh_vertices = mesh_vertices,
      WAIC = NA_real_, pWAIC = NA_real_, DIC = NA_real_,
      mean_neglogCPO = NA_real_, n_CPO_nonfinite = NA_integer_,
      status = "ERROR", error = error_message, stringsAsFactors = FALSE
    ),
    fixed = data.frame(), hyper = data.frame(), prediction = data.frame(),
    error = error_message
  )
}

fit_one <- function(data, response, family, terms, spatial_spec, mesh_object,
                    barrier_triangles, training_rows, prediction_rows, model_id) {
  tryCatch({
    spec <- spatial_specs[spatial_specs$spatial_spec == spatial_spec, , drop = FALSE]
    if (nrow(spec) != 1L) stop("Unknown spatial spec: ", spatial_spec, call. = FALSE)

    spatial_model <- build_spatial_model(
      mesh_object$mesh, spec$spatial_model, barrier_triangles
    )
    projection_matrix <- INLA::inla.spde.make.A(
      mesh_object$mesh, loc = mesh_object$coords
    )

    fixed <- data.frame(intercept = 1, stringsAsFactors = FALSE)
    if (isTRUE(spec$include_region)) fixed$regionEast <- as.integer(data$region == "East")
    for (term in terms) fixed[[term]] <- as.numeric(data[[term]])
    if (isTRUE(spec$include_site)) {
      fixed$site_iid <- as.integer(factor(data$exact_site_id, levels = unique(data$exact_site_id)))
    }

    spatial_index <- seq_len(mesh_object$mesh$n)
    estimation_stack <- INLA::inla.stack(
      data = list(y = data[[response]][training_rows]),
      A = list(1, projection_matrix[training_rows, , drop = FALSE]),
      effects = list(
        fixed = fixed[training_rows, , drop = FALSE],
        spatial_field = spatial_index
      ),
      tag = "est"
    )
    stacks <- list(estimation_stack)
    if (length(prediction_rows)) {
      prediction_stack <- INLA::inla.stack(
        data = list(y = rep(NA_real_, length(prediction_rows))),
        A = list(1, projection_matrix[prediction_rows, , drop = FALSE]),
        effects = list(
          fixed = fixed[prediction_rows, , drop = FALSE],
          spatial_field = spatial_index
        ),
        tag = "pred"
      )
      stacks <- c(stacks, list(prediction_stack))
    }
    stack <- do.call(INLA::inla.stack, stacks)

    fixed_terms <- setdiff(names(fixed), "site_iid")
    right_hand_side <- c(fixed_terms, "f(spatial_field, model = spatial_model)")
    if (isTRUE(spec$include_site)) {
      right_hand_side <- c(
        right_hand_side,
        paste0(
          "f(site_iid, model = 'iid', hyper = list(prec = list(",
          "prior = 'pc.prec', param = c(1, 0.05))))"
        )
      )
    }
    formula <- stats::as.formula(
      paste("y ~ -1 +", paste(right_hand_side, collapse = " + "))
    )
    environment(formula) <- environment()

    fit <- INLA::inla(
      formula, family = family, data = INLA::inla.stack.data(stack),
      control.predictor = list(A = INLA::inla.stack.A(stack), compute = TRUE, link = 1),
      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = FALSE),
      control.inla = list(strategy = "adaptive"),
      num.threads = "1:1", verbose = FALSE
    )

    cpo <- as.numeric(fit$cpo$cpo)
    cpo_ok <- is.finite(cpo) & cpo > 0
    metrics <- data.frame(
      model_id = model_id, spatial_spec = spatial_spec, family = family,
      n_train = sum(training_rows), n_predict = length(prediction_rows),
      mesh_vertices = mesh_object$mesh$n,
      WAIC = fit$waic$waic, pWAIC = fit$waic$p.eff, DIC = fit$dic$dic,
      mean_neglogCPO = mean(-log(cpo[cpo_ok])),
      n_CPO_nonfinite = sum(!cpo_ok), status = "PASS", error = "",
      stringsAsFactors = FALSE
    )

    fixed_table <- fit$summary.fixed
    fixed_output <- data.frame(
      model_id = model_id, spatial_spec = spatial_spec,
      term = rownames(fixed_table), mean = fixed_table$mean,
      sd = fixed_table$sd, lower_95 = fixed_table$`0.025quant`,
      median = fixed_table$`0.5quant`, upper_95 = fixed_table$`0.975quant`,
      mode = fixed_table$mode, stringsAsFactors = FALSE
    )
    hyper_table <- fit$summary.hyperpar
    hyper_output <- if (is.null(hyper_table) || !nrow(hyper_table)) {
      data.frame()
    } else {
      data.frame(
        model_id = model_id, spatial_spec = spatial_spec,
        hyperparameter = rownames(hyper_table), mean = hyper_table$mean,
        sd = hyper_table$sd, lower_95 = hyper_table$`0.025quant`,
        median = hyper_table$`0.5quant`, upper_95 = hyper_table$`0.975quant`,
        mode = hyper_table$mode, stringsAsFactors = FALSE
      )
    }

    prediction <- data.frame()
    if (length(prediction_rows)) {
      prediction_index <- INLA::inla.stack.index(stack, "pred")$data
      prediction <- data.frame(
        model_id = model_id, spatial_spec = spatial_spec,
        row_index = prediction_rows,
        observation_id = data$observation_id[prediction_rows],
        observed = data[[response]][prediction_rows],
        predicted = fit$summary.fitted.values$mean[prediction_index],
        fold = data$spatial_fold[prediction_rows],
        spatial_block = data$spatial_block[prediction_rows],
        stringsAsFactors = FALSE
      )
    }
    list(metrics = metrics, fixed = fixed_output, hyper = hyper_output,
         prediction = prediction, error = NULL)
  }, error = function(error) {
    empty_fit_result(
      model_id, spatial_spec, family, sum(training_rows), length(prediction_rows),
      mesh_object$mesh$n, conditionMessage(error)
    )
  })
}

fit_model_grid <- function(data, response, family, model_specs,
                           spatial_spec = "stationary_region") {
  all_extra <- unique(unlist(lapply(model_specs, `[[`, "extra"), use.names = FALSE))
  common_required <- unique(c(
    response, "observation_id", "region", "x_km", "y_km",
    "exact_site_id", "spatial_fold", "spatial_block", base_terms, all_extra
  ))
  common_data <- data[stats::complete.cases(data[common_required]), , drop = FALSE]

  full_metrics <- fixed_rows <- hyper_rows <- prediction_rows <- scaling_rows <- list()
  for (model_id in names(model_specs)) {
    model_spec <- model_specs[[model_id]]
    prepared_full <- make_analysis_data(
      common_data, response, model_spec$extra, model_spec$interactions
    )
    full_data <- prepared_full$data
    full_mesh <- build_mesh(full_data)
    full_fit <- fit_one(
      full_data, response, family, prepared_full$terms, spatial_spec,
      full_mesh, integer(), prepared_full$training_rows, integer(), model_id
    )
    full_fit$metrics$max_VIF <- {
      vif <- compute_vif(full_data, prepared_full$terms, include_region = TRUE)
      if (nrow(vif)) max(vif$VIF, na.rm = TRUE) else NA_real_
    }
    full_metrics[[length(full_metrics) + 1L]] <- full_fit$metrics
    fixed_rows[[length(fixed_rows) + 1L]] <- full_fit$fixed
    hyper_rows[[length(hyper_rows) + 1L]] <- full_fit$hyper
    scaling_rows[[length(scaling_rows) + 1L]] <- transform(
      prepared_full$scaling, model_id = model_id, fold = 0L
    )

    for (fold in sort(unique(common_data$spatial_fold))) {
      prepared_fold <- make_analysis_data(
        common_data, response, model_spec$extra, model_spec$interactions,
        training_fold = fold
      )
      fold_data <- prepared_fold$data
      fold_mesh <- build_mesh(fold_data)
      fold_fit <- fit_one(
        fold_data, response, family, prepared_fold$terms, spatial_spec,
        fold_mesh, integer(), prepared_fold$training_rows,
        prepared_fold$prediction_rows, model_id
      )
      if (nrow(fold_fit$prediction)) {
        prediction_rows[[length(prediction_rows) + 1L]] <- fold_fit$prediction
      }
      scaling_rows[[length(scaling_rows) + 1L]] <- transform(
        prepared_fold$scaling, model_id = model_id, fold = fold
      )
    }
  }
  list(
    common_n = nrow(common_data),
    full_metrics = rbind_nonempty(full_metrics),
    fixed = rbind_nonempty(fixed_rows),
    hyper = rbind_nonempty(hyper_rows),
    predictions = rbind_nonempty(prediction_rows),
    scaling = rbind_nonempty(scaling_rows)
  )
}

fit_spatial_grid <- function(data, response, family, core_spec, grid_prefix) {
  prepared_full <- make_analysis_data(
    data, response, core_spec$extra, core_spec$interactions
  )
  full_data <- prepared_full$data
  full_mesh <- build_mesh(full_data)
  full_barrier <- barrier_triangles_from_elevation(full_mesh$mesh, elevation_path)
  metrics <- fixed_rows <- hyper_rows <- predictions <- list()

  for (spatial_spec in spatial_specs$spatial_spec) {
    model_id <- paste0(grid_prefix, "__", spatial_spec)
    full_fit <- fit_one(
      full_data, response, family, prepared_full$terms, spatial_spec,
      full_mesh, full_barrier, prepared_full$training_rows, integer(), model_id
    )
    spec_row <- spatial_specs[spatial_specs$spatial_spec == spatial_spec, , drop = FALSE]
    full_fit$metrics$max_VIF <- {
      vif <- compute_vif(
        full_data, prepared_full$terms,
        include_region = isTRUE(spec_row$include_region)
      )
      if (nrow(vif)) max(vif$VIF, na.rm = TRUE) else NA_real_
    }
    metrics[[length(metrics) + 1L]] <- full_fit$metrics
    fixed_rows[[length(fixed_rows) + 1L]] <- full_fit$fixed
    hyper_rows[[length(hyper_rows) + 1L]] <- full_fit$hyper

    for (fold in sort(unique(full_data$spatial_fold))) {
      prepared_fold <- make_analysis_data(
        data, response, core_spec$extra, core_spec$interactions,
        training_fold = fold
      )
      fold_data <- prepared_fold$data
      fold_mesh <- build_mesh(fold_data)
      fold_barrier <- barrier_triangles_from_elevation(fold_mesh$mesh, elevation_path)
      fold_fit <- fit_one(
        fold_data, response, family, prepared_fold$terms, spatial_spec,
        fold_mesh, fold_barrier, prepared_fold$training_rows,
        prepared_fold$prediction_rows, model_id
      )
      if (nrow(fold_fit$prediction)) {
        predictions[[length(predictions) + 1L]] <- fold_fit$prediction
      }
    }
  }
  list(
    full_metrics = rbind_nonempty(metrics), fixed = rbind_nonempty(fixed_rows),
    hyper = rbind_nonempty(hyper_rows), predictions = rbind_nonempty(predictions),
    barrier_triangles = full_barrier
  )
}

summarize_predictions <- function(predictions, family, reference_model, bootstrap_prefix) {
  if (!nrow(predictions)) {
    return(list(fold = data.frame(), pooled = data.frame(), bootstrap = data.frame()))
  }
  fold_rows <- lapply(
    split(predictions, interaction(predictions$model_id, predictions$fold, drop = TRUE)),
    function(group) {
      cbind(
        data.frame(
          model_id = group$model_id[1], spatial_spec = group$spatial_spec[1],
          fold = group$fold[1], stringsAsFactors = FALSE
        ),
        prediction_metrics(group$observed, group$predicted, family)
      )
    }
  )
  fold <- do.call(rbind, fold_rows)
  pooled_rows <- lapply(split(predictions, predictions$model_id), function(group) {
    cbind(
      data.frame(
        model_id = group$model_id[1], spatial_spec = group$spatial_spec[1],
        stringsAsFactors = FALSE
      ),
      prediction_metrics(group$observed, group$predicted, family)
    )
  })
  pooled <- do.call(rbind, pooled_rows)

  reference <- predictions[
    predictions$model_id == reference_model,
    c("observation_id", "observed", "predicted", "fold", "spatial_block")
  ]
  names(reference)[names(reference) == "predicted"] <- "reference_prediction"
  bootstrap_rows <- list()
  for (model_id in setdiff(unique(predictions$model_id), reference_model)) {
    candidate <- predictions[
      predictions$model_id == model_id, c("observation_id", "predicted")
    ]
    names(candidate)[2] <- "candidate_prediction"
    joined <- merge(reference, candidate, by = "observation_id")
    if (family == "binomial") {
      outcome <- joined$observed
      reference_probability <- pmin(pmax(joined$reference_prediction, 1e-10), 1 - 1e-10)
      candidate_probability <- pmin(pmax(joined$candidate_prediction, 1e-10), 1 - 1e-10)
      gain <- -(outcome * log(reference_probability) + (1 - outcome) * log(1 - reference_probability)) +
        (outcome * log(candidate_probability) + (1 - outcome) * log(1 - candidate_probability))
    } else {
      gain <- (joined$observed - joined$reference_prediction)^2 -
        (joined$observed - joined$candidate_prediction)^2
    }
    bootstrap <- cluster_bootstrap_gain(
      gain, joined$spatial_block, bootstrap_reps,
      seed_offset = sum(utf8ToInt(paste0(bootstrap_prefix, model_id)))
    )
    fold_gain <- aggregate(gain, by = list(fold = joined$fold), FUN = mean)
    bootstrap_rows[[length(bootstrap_rows) + 1L]] <- cbind(
      data.frame(
        model_id = model_id, reference_model = reference_model,
        folds_improved = sum(fold_gain$x > 0), folds_total = nrow(fold_gain),
        stringsAsFactors = FALSE
      ),
      bootstrap
    )
  }
  list(fold = fold, pooled = pooled, bootstrap = rbind_nonempty(bootstrap_rows))
}

build_decision_table <- function(full_metrics, pooled, bootstrap, reference_model) {
  if (!nrow(full_metrics)) return(data.frame())
  table <- merge(
    full_metrics,
    pooled[, c("model_id", "primary_loss", "log_loss", "brier", "AUC", "RMSE", "MAE", "R2"), drop = FALSE],
    by = "model_id", all.x = TRUE
  )
  table <- merge(table, bootstrap, by = "model_id", all.x = TRUE)
  reference <- table[table$model_id == reference_model, , drop = FALSE]
  if (nrow(reference) != 1L) stop("Reference model missing or duplicated: ", reference_model, call. = FALSE)
  table$delta_WAIC <- table$WAIC - reference$WAIC
  table$delta_mean_neglogCPO <- table$mean_neglogCPO - reference$mean_neglogCPO
  table$delta_primary_loss <- table$primary_loss - reference$primary_loss
  table$full_fit_improved <- table$delta_WAIC <= -2 | table$delta_mean_neglogCPO <= -0.001
  table$transfer_improved <- is.finite(table$lower_95) & table$lower_95 > 0 & table$folds_improved >= 4
  table$collinearity_ok <- is.finite(table$max_VIF) & table$max_VIF < 10
  table$strong_support <- table$status == "PASS" & table$full_fit_improved &
    table$transfer_improved & table$collinearity_ok
  table$is_reference <- table$model_id == reference_model
  table[order(table$primary_loss, table$WAIC), , drop = FALSE]
}

outcomes_all <- list(
  pigmentation_state = list(
    response = "pigmented_mixture50", family = "binomial",
    model_specs = state_specs, reference_model = "state_additive",
    core_spec = state_specs$state_additive
  ),
  conditional_intensity = list(
    response = "pigment_intensity_z", family = "gaussian",
    model_specs = intensity_specs, reference_model = "intensity_additive",
    core_spec = intensity_specs$intensity_thermal_variability
  )
)
if (outcome_filter == "all") {
  outcomes <- outcomes_all
} else {
  if (!(outcome_filter %in% names(outcomes_all))) stop("Unknown outcome: ", outcome_filter, call. = FALSE)
  outcomes <- outcomes_all[outcome_filter]
}

all_validations <- list()
for (outcome_name in names(outcomes)) {
  specification <- outcomes[[outcome_name]]
  outcome_dir <- file.path(output_root, outcome_name)
  dir.create(outcome_dir, recursive = TRUE, showWarnings = FALSE)

  message("[broad audit] environmental model grid: ", outcome_name)
  environmental <- fit_model_grid(
    raw, specification$response, specification$family,
    specification$model_specs, spatial_spec = "stationary_region"
  )
  environmental_summary <- summarize_predictions(
    environmental$predictions, specification$family,
    specification$reference_model, paste0(outcome_name, "_environment")
  )
  environmental_decision <- build_decision_table(
    environmental$full_metrics, environmental_summary$pooled,
    environmental_summary$bootstrap, specification$reference_model
  )

  message("[broad audit] spatial model grid: ", outcome_name)
  spatial_prefix <- paste0(outcome_name, "_core")
  spatial <- fit_spatial_grid(
    raw, specification$response, specification$family,
    specification$core_spec, spatial_prefix
  )
  spatial_reference <- paste0(spatial_prefix, "__stationary_region")
  spatial_summary <- summarize_predictions(
    spatial$predictions, specification$family,
    spatial_reference, paste0(outcome_name, "_spatial")
  )
  spatial_decision <- build_decision_table(
    spatial$full_metrics, spatial_summary$pooled,
    spatial_summary$bootstrap, spatial_reference
  )

  write_csv_to(environmental$full_metrics, file.path(outcome_dir, "environment_full_fit_metrics.csv"))
  write_csv_to(environmental$fixed, file.path(outcome_dir, "environment_fixed_effects.csv"))
  write_csv_to(environmental$hyper, file.path(outcome_dir, "environment_hyperparameters.csv"))
  write_csv_to(environmental$predictions, file.path(outcome_dir, "environment_blocked_predictions.csv"))
  write_csv_to(environmental_summary$fold, file.path(outcome_dir, "environment_blocked_fold_metrics.csv"))
  write_csv_to(environmental_summary$pooled, file.path(outcome_dir, "environment_blocked_pooled_metrics.csv"))
  write_csv_to(environmental_summary$bootstrap, file.path(outcome_dir, "environment_block_bootstrap_gain.csv"))
  write_csv_to(environmental$scaling, file.path(outcome_dir, "environment_scaling.csv"))
  write_csv_to(environmental_decision, file.path(outcome_dir, "environment_model_decision_table.csv"))

  write_csv_to(spatial$full_metrics, file.path(outcome_dir, "spatial_full_fit_metrics.csv"))
  write_csv_to(spatial$fixed, file.path(outcome_dir, "spatial_fixed_effects.csv"))
  write_csv_to(spatial$hyper, file.path(outcome_dir, "spatial_hyperparameters.csv"))
  write_csv_to(spatial$predictions, file.path(outcome_dir, "spatial_blocked_predictions.csv"))
  write_csv_to(spatial_summary$fold, file.path(outcome_dir, "spatial_blocked_fold_metrics.csv"))
  write_csv_to(spatial_summary$pooled, file.path(outcome_dir, "spatial_blocked_pooled_metrics.csv"))
  write_csv_to(spatial_summary$bootstrap, file.path(outcome_dir, "spatial_block_bootstrap_gain.csv"))
  write_csv_to(spatial_decision, file.path(outcome_dir, "spatial_model_decision_table.csv"))
  write_csv_to(data.frame(triangle = spatial$barrier_triangles), file.path(outcome_dir, "spatial_barrier_triangles.csv"))

  optional_errors <- rbind_nonempty(list(
    environmental$full_metrics[environmental$full_metrics$status != "PASS", , drop = FALSE],
    spatial$full_metrics[spatial$full_metrics$status != "PASS", , drop = FALSE]
  ))
  required_environment <- environmental$full_metrics[
    environmental$full_metrics$model_id == specification$reference_model, , drop = FALSE
  ]
  required_spatial <- spatial$full_metrics[
    spatial$full_metrics$model_id == spatial_reference, , drop = FALSE
  ]
  required_ok <- nrow(required_environment) == 1L &&
    required_environment$status[[1L]] == "PASS" &&
    nrow(required_spatial) == 1L && required_spatial$status[[1L]] == "PASS" &&
    specification$reference_model %in% unique(environmental$predictions$model_id) &&
    spatial_reference %in% unique(spatial$predictions$model_id)

  validation <- list(
    status = if (!required_ok) "FAIL" else if (nrow(optional_errors)) "PASS_WITH_OPTIONAL_MODEL_ERRORS" else "PASS",
    outcome = outcome_name, response = specification$response,
    family = specification$family, n_source = nrow(raw),
    n_common_environment_comparison = environmental$common_n,
    environmental_models = nrow(environmental$full_metrics),
    spatial_models = nrow(spatial$full_metrics),
    environmental_prediction_models = length(unique(environmental$predictions$model_id)),
    spatial_prediction_models = length(unique(spatial$predictions$model_id)),
    required_environment_reference = specification$reference_model,
    required_spatial_reference = spatial_reference,
    optional_model_errors = if (nrow(optional_errors)) optional_errors$error else character(),
    frozen_natural_reference_replaced = FALSE
  )
  jsonlite::write_json(
    validation, file.path(outcome_dir, "validation.json"),
    pretty = TRUE, auto_unbox = TRUE, null = "null"
  )
  all_validations[[outcome_name]] <- validation
}

manifest <- list(
  status = if (all(vapply(all_validations, function(x) x$status != "FAIL", logical(1)))) "PASS" else "FAIL",
  input_csv = normalizePath(input_csv, winslash = "/", mustWork = TRUE),
  input_md5 = unname(tools::md5sum(input_csv)),
  landscape_csv = normalizePath(landscape_csv, winslash = "/", mustWork = TRUE),
  raster_dir = normalizePath(raster_dir, winslash = "/", mustWork = TRUE),
  n_source_rows = nrow(raw), outcomes = names(outcomes),
  base_terms = base_terms, extended_terms = extended_terms,
  observation_control_terms = observation_control_terms,
  mesh = list(max_edge_km = c(20, 100), cutoff_km = 5),
  stationary_prior = list(range = c(100, 0.05), sigma = c(1, 0.05)),
  barrier_range_fraction = 0.2,
  site_iid_prior = "P(site SD > 1) = 0.05",
  bootstrap_reps = bootstrap_reps, seed = seed,
  interpretation = paste(
    "Observation-level coefficient and residual-spatial-structure audit only.",
    "The separate cell-level natural predictive reference and 17 local-departure",
    "targets are not replaced automatically."
  )
)
jsonlite::write_json(
  manifest, file.path(output_root, "analysis_manifest.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)
cat("Wrote broad environment/spatial audit to ", output_root, "\n", sep = "")
