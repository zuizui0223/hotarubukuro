#!/usr/bin/env Rscript

# Final sensitivity for the current JBI broad environmental/spatial act.
# Tests VPD/SWB hydroclimate completeness and stationary-versus-barrier SPDE
# structure without silently changing the separate local-departure reference.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  if (hit[length(hit)] == length(args)) stop("Missing value after ", flag, call. = FALSE)
  args[hit[length(hit)] + 1L]
}

input_csv <- arg_value(
  "--input-csv",
  "reference-artifact/results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
)
processed_dir <- arg_value("--processed-dir", "data/processed/rasters")
output_dir <- arg_value("--output-dir", "results/broad_environment_spatial_finalization")
outcome_filter <- arg_value("--outcome", "all")
bootstrap_reps <- as.integer(arg_value("--bootstrap-reps", "4000"))
seed <- as.integer(arg_value("--seed", "20260814"))
barrier_fraction <- as.numeric(arg_value("--barrier-range-fraction", "0.2"))

required_packages <- c("INLA", "Matrix", "sf", "terra", "rnaturalearth", "jsonlite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing required packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}
if (!file.exists(input_csv)) stop("Missing input: ", input_csv, call. = FALSE)
if (!dir.exists(processed_dir)) stop("Missing processed raster directory: ", processed_dir, call. = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(
  OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1",
  INLA_NUM_THREADS = "1", OMP_DYNAMIC = "FALSE"
)
set.seed(seed)

write_csv <- function(x, name) {
  path <- file.path(output_dir, name)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

safe_z <- function(x) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  out <- rep(NA_real_, length(x))
  if (!any(ok)) return(out)
  sx <- stats::sd(x[ok])
  if (!is.finite(sx) || sx == 0) out[ok] <- 0 else out[ok] <- (x[ok] - mean(x[ok])) / sx
  out
}

binary_auc <- function(y, p) {
  keep <- is.finite(y) & is.finite(p)
  y <- as.integer(y[keep]); p <- as.numeric(p[keep])
  n1 <- sum(y == 1L); n0 <- sum(y == 0L)
  if (!n1 || !n0) return(NA_real_)
  ranks <- rank(p, ties.method = "average")
  (sum(ranks[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

compute_vif <- function(data, terms) {
  terms <- unique(terms)
  terms <- terms[vapply(data[terms], function(x) {
    is.numeric(x) && stats::sd(x, na.rm = TRUE) > 0
  }, logical(1))]
  if (!length(terms)) return(data.frame(term = character(), VIF = numeric()))
  do.call(rbind, lapply(terms, function(term) {
    others <- setdiff(terms, term)
    if (!length(others)) return(data.frame(term = term, VIF = 1))
    cc <- stats::complete.cases(data[c(term, others)])
    fit <- stats::lm(stats::reformulate(others, response = term), data = data[cc, , drop = FALSE])
    data.frame(term = term, VIF = 1 / pmax(1 - summary(fit)$r.squared, 1e-10))
  }))
}

cluster_bootstrap_gain <- function(loss_gain, clusters, reps, seed_offset = 0L) {
  keep <- is.finite(loss_gain) & !is.na(clusters)
  loss_gain <- loss_gain[keep]; clusters <- as.character(clusters[keep])
  unique_clusters <- unique(clusters)
  if (!length(unique_clusters)) {
    return(c(mean_gain = NA_real_, lower_95 = NA_real_, upper_95 = NA_real_, probability_positive = NA_real_))
  }
  set.seed(seed + seed_offset)
  draws <- numeric(reps)
  for (b in seq_len(reps)) {
    sampled <- sample(unique_clusters, length(unique_clusters), replace = TRUE)
    idx <- unlist(lapply(sampled, function(cl) which(clusters == cl)), use.names = FALSE)
    draws[b] <- mean(loss_gain[idx])
  }
  c(
    mean_gain = mean(loss_gain),
    lower_95 = unname(stats::quantile(draws, 0.025, na.rm = TRUE)),
    upper_95 = unname(stats::quantile(draws, 0.975, na.rm = TRUE)),
    probability_positive = mean(draws > 0, na.rm = TRUE)
  )
}

presence_metrics <- function(y, p) {
  p <- pmin(pmax(as.numeric(p), 1e-8), 1 - 1e-8); y <- as.integer(y)
  data.frame(
    primary_loss = -mean(y * log(p) + (1 - y) * log(1 - p)),
    brier = mean((y - p)^2), AUC = binary_auc(y, p),
    RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_
  )
}

intensity_metrics <- function(y, p) {
  residual <- as.numeric(y) - as.numeric(p)
  denom <- sum((y - mean(y))^2)
  data.frame(
    primary_loss = mean(residual^2), brier = NA_real_, AUC = NA_real_,
    RMSE = sqrt(mean(residual^2)), MAE = mean(abs(residual)),
    R2 = if (denom > 0) 1 - sum(residual^2) / denom else NA_real_
  )
}

base_terms <- c(
  "env_Temperature_PC1", "env_precip_PC1",
  "env_TemperatureSeasonality", "env_PrecipSeasonality",
  "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS"
)

data <- utils::read.csv(input_csv, check.names = FALSE, stringsAsFactors = FALSE)
required <- c(
  base_terms, "region", "longitude", "latitude", "x_km", "y_km",
  "spatial_fold", "spatial_block", "pigmented_mixture50", "pigment_intensity_z"
)
missing <- setdiff(required, names(data))
if (length(missing)) stop("Missing analysis columns: ", paste(missing, collapse = ", "), call. = FALSE)
data$region <- factor(data$region, levels = c("West", "East"))

find_raster <- function(candidates) {
  hits <- unlist(lapply(candidates, function(name) {
    list.files(processed_dir, pattern = paste0("^", name, "$"), recursive = TRUE, full.names = TRUE)
  }))
  hits <- unique(hits[file.exists(hits)])
  if (length(hits) != 1L) {
    stop("Expected one processed raster among ", paste(candidates, collapse = ", "),
         "; found ", paste(hits, collapse = ", "), call. = FALSE)
  }
  hits[[1]]
}

vpd_path <- find_raster(c("chelsa_vpdmean.tif", "vpdmean_Japan_crop_30s.tif", "vpdmean.tif"))
swb_path <- find_raster(c("chelsa_swb.tif", "swb_Japan_crop_30s.tif", "swb.tif"))
pts <- terra::vect(data, geom = c("longitude", "latitude"), crs = "EPSG:4326")
extract_one <- function(path) {
  values <- terra::extract(terra::rast(path), pts, method = "bilinear")
  as.numeric(values[, ncol(values)])
}
data$VPD_raw <- extract_one(vpd_path)
data$SWB_raw <- extract_one(swb_path)
data$env_VPD <- safe_z(data$VPD_raw)
data$env_SWB <- safe_z(data$SWB_raw)
data$env_hydroclimate <- safe_z(data$env_precip_PC1 - data$env_VPD + data$env_SWB)
data$int_thermal_variability <- safe_z(data$env_Temperature_PC1 * data$env_TemperatureSeasonality)
data$int_moisture_thermal_variability <- safe_z(data$env_precip_PC1 * data$env_TemperatureSeasonality)

outcomes_all <- list(
  pigmentation_state = list(response = "pigmented_mixture50", family = "binomial"),
  conditional_intensity = list(response = "pigment_intensity_z", family = "gaussian")
)
if (identical(outcome_filter, "all")) {
  outcomes <- outcomes_all
} else {
  if (!(outcome_filter %in% names(outcomes_all))) stop("Unknown --outcome: ", outcome_filter)
  outcomes <- outcomes_all[outcome_filter]
}

water_registry <- list(
  current = base_terms,
  current_plus_vpd = c(base_terms, "env_VPD"),
  current_plus_swb = c(base_terms, "env_SWB"),
  current_plus_vpd_swb = c(base_terms, "env_VPD", "env_SWB"),
  hydroclimate_replace = c(
    "env_Temperature_PC1", "env_hydroclimate",
    "env_TemperatureSeasonality", "env_PrecipSeasonality",
    "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS"
  )
)

spatial_formula_registry <- list(
  pigmentation_state = list(state_additive = base_terms),
  conditional_intensity = list(
    intensity_thermal_variability = c(base_terms, "int_thermal_variability"),
    intensity_thermal_moisture_joint = c(
      base_terms, "int_thermal_variability", "int_moisture_thermal_variability"
    )
  )
)

projection_wkt <- paste(
  "+proj=laea +lat_0=36 +lon_0=137 +x_0=0 +y_0=0",
  "+datum=WGS84 +units=m +no_defs"
)

build_mesh <- function(d) {
  INLA::inla.mesh.2d(loc = as.matrix(d[c("x_km", "y_km")]), max.edge = c(20, 100), cutoff = 5)
}

build_spatial_model <- function(mesh, kind, d = NULL) {
  if (kind == "stationary") {
    return(INLA::inla.spde2.pcmatern(
      mesh, alpha = 2, prior.range = c(100, 0.05), prior.sigma = c(1, 0.05)
    ))
  }
  if (!identical(kind, "barrier")) stop("Unknown spatial model: ", kind)

  japan <- rnaturalearth::ne_countries(scale = "medium", country = "Japan", returnclass = "sf")
  japan <- sf::st_transform(sf::st_make_valid(japan), crs = projection_wkt)
  japan <- sf::st_union(japan)
  obs_sf <- sf::st_as_sf(
    data.frame(x = d$x_km * 1000, y = d$y_km * 1000),
    coords = c("x", "y"), crs = projection_wkt
  )
  land <- sf::st_union(japan, sf::st_union(sf::st_buffer(obs_sf, dist = 5000)))

  tv <- mesh$graph$tv
  tri_centres_km <- cbind(
    (mesh$loc[tv[, 1], 1] + mesh$loc[tv[, 2], 1] + mesh$loc[tv[, 3], 1]) / 3,
    (mesh$loc[tv[, 1], 2] + mesh$loc[tv[, 2], 2] + mesh$loc[tv[, 3], 2]) / 3
  )
  tri_sf <- sf::st_as_sf(
    data.frame(x = tri_centres_km[, 1] * 1000, y = tri_centres_km[, 2] * 1000),
    coords = c("x", "y"), crs = projection_wkt
  )
  inside <- lengths(sf::st_intersects(tri_sf, land)) > 0
  barrier_triangles <- which(!inside)
  if (!length(barrier_triangles)) stop("Barrier construction produced no sea triangles.")

  model <- INLA::inla.barrier.pcmatern(
    mesh = mesh, barrier.triangles = barrier_triangles,
    prior.range = c(100, 0.05), prior.sigma = c(1, 0.05),
    range.fraction = barrier_fraction
  )
  attr(model, "barrier_triangles") <- barrier_triangles
  model
}

make_fixed <- function(d, terms, include_region = TRUE) {
  out <- data.frame(intercept = 1)
  if (include_region) out$regionEast <- as.integer(d$region == "East")
  for (term in terms) out[[term]] <- as.numeric(d[[term]])
  out
}

fit_model <- function(d, response, family, terms, mesh, spatial_model,
                      include_region, train_rows, pred_rows = integer()) {
  fixed <- make_fixed(d, terms, include_region = include_region)
  coords <- as.matrix(d[c("x_km", "y_km")])
  A <- INLA::inla.spde.make.A(mesh = mesh, loc = coords)
  spatial_index <- list(spatial_field = seq_len(mesh$n))

  est_stack <- INLA::inla.stack(
    data = list(y = d[[response]][train_rows]),
    A = list(1, A[train_rows, , drop = FALSE]),
    effects = list(fixed[train_rows, , drop = FALSE], spatial_index), tag = "est"
  )
  stacks <- list(est_stack)
  if (length(pred_rows)) {
    pred_stack <- INLA::inla.stack(
      data = list(y = rep(NA_real_, length(pred_rows))),
      A = list(1, A[pred_rows, , drop = FALSE]),
      effects = list(fixed[pred_rows, , drop = FALSE], spatial_index), tag = "pred"
    )
    stacks <- c(stacks, list(pred_stack))
  }
  stack <- do.call(INLA::inla.stack, stacks)
  rhs <- paste(c(names(fixed), "f(spatial_field, model = spatial_model)"), collapse = " + ")
  formula <- stats::as.formula(paste0("y ~ -1 + ", rhs))
  fit <- INLA::inla(
    formula, family = family, data = INLA::inla.stack.data(stack),
    control.predictor = list(A = INLA::inla.stack.A(stack), compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = FALSE),
    control.inla = list(strategy = "adaptive"), num.threads = "1:1", verbose = FALSE
  )
  pred <- NULL
  if (length(pred_rows)) {
    idx <- INLA::inla.stack.index(stack, "pred")$data
    pred <- data.frame(
      row_index = pred_rows,
      mean = fit$summary.fitted.values$mean[idx],
      sd = fit$summary.fitted.values$sd[idx]
    )
  }
  list(fit = fit, pred = pred, formula = paste(deparse(formula), collapse = " "))
}

full_metrics <- function(fit) {
  cpo <- as.numeric(fit$cpo$cpo)
  cpo <- cpo[is.finite(cpo) & cpo > 0]
  c(
    DIC = fit$dic$dic, WAIC = fit$waic$waic, pWAIC = fit$waic$p.eff,
    mean_log_CPO = if (length(cpo)) mean(log(cpo)) else NA_real_
  )
}

extract_fixed <- function(fit, analysis, outcome, model_id) {
  tab <- fit$summary.fixed
  data.frame(
    analysis = analysis, outcome = outcome, model_id = model_id,
    term = rownames(tab), mean = tab[, "mean"], sd = tab[, "sd"],
    lower_95 = tab[, "0.025quant"], upper_95 = tab[, "0.975quant"],
    stringsAsFactors = FALSE
  )
}

extract_hyper <- function(fit, analysis, outcome, model_id) {
  tab <- fit$summary.hyperpar
  if (is.null(tab) || !nrow(tab)) return(data.frame())
  data.frame(
    analysis = analysis, outcome = outcome, model_id = model_id,
    hyperparameter = rownames(tab), mean = tab[, "mean"], sd = tab[, "sd"],
    lower_95 = tab[, "0.025quant"], upper_95 = tab[, "0.975quant"],
    stringsAsFactors = FALSE
  )
}

fit_comparison_grid <- function(d, outcome_name, response, family, model_terms,
                                common_required, reference_model, analysis_label) {
  keep <- stats::complete.cases(d[unique(c(
    response, common_required, "region", "x_km", "y_km", "spatial_fold", "spatial_block"
  ))])
  dd <- droplevels(d[keep, , drop = FALSE]); dd$.original_row <- which(keep)
  mesh <- build_mesh(dd)
  spatial_cache <- list()
  get_spatial <- function(kind) {
    if (is.null(spatial_cache[[kind]])) spatial_cache[[kind]] <<- build_spatial_model(mesh, kind, dd)
    spatial_cache[[kind]]
  }

  full_rows <- fixed_rows <- hyper_rows <- pred_rows <- fold_rows <- vif_rows <- list()
  for (model_id in names(model_terms)) {
    terms <- model_terms[[model_id]]$terms
    spatial_kind <- model_terms[[model_id]]$spatial_kind
    include_region <- model_terms[[model_id]]$include_region
    sm <- get_spatial(spatial_kind)

    message("[broad finalization] ", analysis_label, " ", outcome_name, " full ", model_id)
    fit <- fit_model(dd, response, family, terms, mesh, sm, include_region, seq_len(nrow(dd)))
    fm <- full_metrics(fit$fit)
    full_rows[[length(full_rows) + 1L]] <- data.frame(
      analysis = analysis_label, outcome = outcome_name, model_id = model_id,
      n = nrow(dd), spatial_kind = spatial_kind, include_region = include_region,
      DIC = fm["DIC"], WAIC = fm["WAIC"], pWAIC = fm["pWAIC"],
      mean_log_CPO = fm["mean_log_CPO"], stringsAsFactors = FALSE
    )
    fixed_rows[[length(fixed_rows) + 1L]] <- extract_fixed(fit$fit, analysis_label, outcome_name, model_id)
    hyper_rows[[length(hyper_rows) + 1L]] <- extract_hyper(fit$fit, analysis_label, outcome_name, model_id)

    design <- make_fixed(dd, terms, include_region)
    vifs <- compute_vif(design, setdiff(names(design), "intercept"))
    vifs$analysis <- analysis_label; vifs$outcome <- outcome_name; vifs$model_id <- model_id
    vif_rows[[length(vif_rows) + 1L]] <- vifs

    for (fold in sort(unique(dd$spatial_fold))) {
      train <- which(dd$spatial_fold != fold); test <- which(dd$spatial_fold == fold)
      cv <- fit_model(dd, response, family, terms, mesh, sm, include_region, train, test)
      pred <- cv$pred
      pred$analysis <- analysis_label; pred$outcome <- outcome_name
      pred$model_id <- model_id; pred$fold <- fold
      pred$observed <- dd[[response]][pred$row_index]
      pred$spatial_block <- dd$spatial_block[pred$row_index]
      pred$original_row <- dd$.original_row[pred$row_index]
      pred_rows[[length(pred_rows) + 1L]] <- pred

      met <- if (family == "binomial") presence_metrics(pred$observed, pred$mean) else intensity_metrics(pred$observed, pred$mean)
      met$analysis <- analysis_label; met$outcome <- outcome_name; met$model_id <- model_id
      met$fold <- fold; met$n_test <- nrow(pred)
      fold_rows[[length(fold_rows) + 1L]] <- met
    }
  }

  full <- do.call(rbind, full_rows); fixed <- do.call(rbind, fixed_rows)
  hyper_list <- hyper_rows[vapply(hyper_rows, nrow, integer(1)) > 0L]
  hyper <- if (length(hyper_list)) do.call(rbind, hyper_list) else data.frame()
  pred <- do.call(rbind, pred_rows); folds <- do.call(rbind, fold_rows); vifs <- do.call(rbind, vif_rows)
  ref <- pred[pred$model_id == reference_model, c("original_row", "observed", "mean", "spatial_block", "fold")]
  names(ref)[names(ref) == "mean"] <- "reference_prediction"

  summaries <- list()
  for (model_id in names(model_terms)) {
    cur <- pred[pred$model_id == model_id, c("original_row", "observed", "mean", "spatial_block", "fold")]
    names(cur)[names(cur) == "mean"] <- "candidate_prediction"
    paired <- merge(cur, ref[c("original_row", "reference_prediction")], by = "original_row", all.x = TRUE, sort = FALSE)
    met <- if (family == "binomial") presence_metrics(paired$observed, paired$candidate_prediction) else intensity_metrics(paired$observed, paired$candidate_prediction)
    if (family == "binomial") {
      y <- as.integer(paired$observed)
      p0 <- pmin(pmax(paired$reference_prediction, 1e-8), 1 - 1e-8)
      p1 <- pmin(pmax(paired$candidate_prediction, 1e-8), 1 - 1e-8)
      loss0 <- -(y * log(p0) + (1 - y) * log(1 - p0))
      loss1 <- -(y * log(p1) + (1 - y) * log(1 - p1))
    } else {
      loss0 <- (paired$observed - paired$reference_prediction)^2
      loss1 <- (paired$observed - paired$candidate_prediction)^2
    }
    gain <- loss0 - loss1
    fold_gain <- tapply(gain, paired$fold, mean)
    boot <- cluster_bootstrap_gain(
      gain, paired$spatial_block, bootstrap_reps,
      seed_offset = match(model_id, names(model_terms)) * 1000L + match(outcome_name, names(outcomes_all)) * 100L
    )
    full_row <- full[full$model_id == model_id, , drop = FALSE]
    ref_row <- full[full$model_id == reference_model, , drop = FALSE]
    max_vif <- max(vifs$VIF[vifs$model_id == model_id], na.rm = TRUE)
    summaries[[length(summaries) + 1L]] <- data.frame(
      analysis = analysis_label, outcome = outcome_name, model_id = model_id,
      reference_model = reference_model, n = nrow(paired),
      WAIC = full_row$WAIC,
      delta_WAIC_reference_minus_candidate = ref_row$WAIC - full_row$WAIC,
      mean_log_CPO = full_row$mean_log_CPO,
      delta_mean_log_CPO = full_row$mean_log_CPO - ref_row$mean_log_CPO,
      heldout_primary_loss = met$primary_loss, mean_primary_loss_gain = mean(gain),
      bootstrap_lower_95 = boot["lower_95"], bootstrap_upper_95 = boot["upper_95"],
      bootstrap_probability_positive = boot["probability_positive"],
      folds_improved = sum(fold_gain > 0, na.rm = TRUE), folds_total = length(fold_gain),
      max_VIF = max_vif,
      passes_strict_gain = model_id != reference_model &&
        is.finite(ref_row$WAIC - full_row$WAIC) && (ref_row$WAIC - full_row$WAIC) >= 2 &&
        is.finite(boot["lower_95"]) && boot["lower_95"] > 0 &&
        sum(fold_gain > 0, na.rm = TRUE) >= 4 && is.finite(max_vif) && max_vif < 10,
      stringsAsFactors = FALSE
    )
  }

  list(
    summary = do.call(rbind, summaries), full = full, fixed = fixed, hyper = hyper,
    predictions = pred, fold_metrics = folds, vif = vifs,
    n = nrow(dd), mesh_n = mesh$n,
    barrier_triangles = if (!is.null(spatial_cache$barrier)) length(attr(spatial_cache$barrier, "barrier_triangles")) else NA_integer_
  )
}

all_summary <- all_full <- all_fixed <- all_hyper <- all_pred <- all_fold <- all_vif <- list()
diagnostic_rows <- list()

for (outcome_name in names(outcomes)) {
  spec <- outcomes[[outcome_name]]
  water_models <- lapply(names(water_registry), function(id) {
    list(terms = water_registry[[id]], spatial_kind = "stationary", include_region = TRUE)
  })
  names(water_models) <- names(water_registry)
  water_required <- unique(c(unlist(water_registry), "env_VPD", "env_SWB", "env_hydroclimate"))
  water <- fit_comparison_grid(
    data, outcome_name, spec$response, spec$family,
    water_models, water_required, "current", "hydroclimate_completeness"
  )
  all_summary[[length(all_summary) + 1L]] <- water$summary
  all_full[[length(all_full) + 1L]] <- water$full
  all_fixed[[length(all_fixed) + 1L]] <- water$fixed
  if (nrow(water$hyper)) all_hyper[[length(all_hyper) + 1L]] <- water$hyper
  all_pred[[length(all_pred) + 1L]] <- water$predictions
  all_fold[[length(all_fold) + 1L]] <- water$fold_metrics
  all_vif[[length(all_vif) + 1L]] <- water$vif

  formula_sets <- spatial_formula_registry[[outcome_name]]
  for (formula_id in names(formula_sets)) {
    terms <- formula_sets[[formula_id]]
    spatial_models <- list(
      stationary_region = list(terms = terms, spatial_kind = "stationary", include_region = TRUE),
      stationary_noregion = list(terms = terms, spatial_kind = "stationary", include_region = FALSE),
      barrier_region = list(terms = terms, spatial_kind = "barrier", include_region = TRUE),
      barrier_noregion = list(terms = terms, spatial_kind = "barrier", include_region = FALSE)
    )
    spatial <- fit_comparison_grid(
      data, outcome_name, spec$response, spec$family,
      spatial_models, terms, "stationary_region", paste0("spatial_structure__", formula_id)
    )
    all_summary[[length(all_summary) + 1L]] <- spatial$summary
    all_full[[length(all_full) + 1L]] <- spatial$full
    all_fixed[[length(all_fixed) + 1L]] <- spatial$fixed
    if (nrow(spatial$hyper)) all_hyper[[length(all_hyper) + 1L]] <- spatial$hyper
    all_pred[[length(all_pred) + 1L]] <- spatial$predictions
    all_fold[[length(all_fold) + 1L]] <- spatial$fold_metrics
    all_vif[[length(all_vif) + 1L]] <- spatial$vif
    diagnostic_rows[[length(diagnostic_rows) + 1L]] <- data.frame(
      outcome = outcome_name, formula_id = formula_id, n = spatial$n,
      mesh_vertices = spatial$mesh_n, barrier_triangles = spatial$barrier_triangles,
      barrier_range_fraction = barrier_fraction, stringsAsFactors = FALSE
    )
  }
}

summary_table <- do.call(rbind, all_summary)
full_table <- do.call(rbind, all_full)
fixed_table <- do.call(rbind, all_fixed)
hyper_table <- if (length(all_hyper)) do.call(rbind, all_hyper) else data.frame()
prediction_table <- do.call(rbind, all_pred)
fold_table <- do.call(rbind, all_fold)
vif_table <- do.call(rbind, all_vif)
diagnostic_table <- do.call(rbind, diagnostic_rows)

write_csv(summary_table, "model_comparison_summary.csv")
write_csv(full_table, "full_fit_metrics.csv")
write_csv(fixed_table, "fixed_effect_posteriors.csv")
if (nrow(hyper_table)) write_csv(hyper_table, "spatial_hyperparameters.csv")
write_csv(prediction_table, "blocked_cv_predictions.csv")
write_csv(fold_table, "blocked_cv_fold_metrics.csv")
write_csv(vif_table, "vif_diagnostics.csv")
write_csv(diagnostic_table, "barrier_diagnostics.csv")

write_csv(data.frame(
  observation_id = if ("observation_id" %in% names(data)) data$observation_id else seq_len(nrow(data)),
  longitude = data$longitude, latitude = data$latitude,
  VPD_raw = data$VPD_raw, SWB_raw = data$SWB_raw,
  env_VPD = data$env_VPD, env_SWB = data$env_SWB,
  env_precip_PC1 = data$env_precip_PC1, env_hydroclimate = data$env_hydroclimate
), "hydroclimate_extracted_values.csv")

jsonlite::write_json(list(
  status = "PASS", source_rows = nrow(data), outcomes = names(outcomes),
  vpd_raster = normalizePath(vpd_path, winslash = "/", mustWork = TRUE),
  swb_raster = normalizePath(swb_path, winslash = "/", mustWork = TRUE),
  hydroclimate_definition = "z(env_precip_PC1 - z(VPD) + z(SWB))",
  spatial_variants = c("stationary_region", "stationary_noregion", "barrier_region", "barrier_noregion"),
  barrier_range_fraction = barrier_fraction,
  selection_rule = "delta_WAIC>=2; spatial-block bootstrap lower95>0; >=4/5 folds; maxVIF<10",
  manuscript_model_replaced_automatically = FALSE,
  local_departure_reference_changed = FALSE
), file.path(output_dir, "validation.json"), pretty = TRUE, auto_unbox = TRUE)

cat("Broad environment/spatial finalization sensitivity complete.\n")
