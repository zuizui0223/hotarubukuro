# Reproducible heavy-model stages restored from the original Code_S3 analysis.
#
# These models complement rather than replace the blocked-CV sensitivity models.
# CFA defines the original multivariate colour construct, SPDE-INLA estimates
# spatially structured associations, and qGAM diagnoses nonlinear conditional
# response patterns. All stages use stable analysis_id values and write tables.

require_heavy_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package '", package, "' is required for the heavy-model stage.", call. = FALSE)
  }
}

finite_complete <- function(data, variables) {
  stats::complete.cases(data[variables]) &
    apply(data[variables], 1L, function(row) all(is.finite(as.numeric(row))))
}

fit_pigment_cfa <- function(data) {
  require_heavy_package("lavaan")
  required <- c("analysis_id", "a", "L", "b")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("CFA input is missing: ", paste(missing, collapse = ", "), call. = FALSE)

  cfa_data <- data.frame(
    a = as.numeric(data$a),
    Lm = -as.numeric(data$L),
    C = sqrt(as.numeric(data$a)^2 + as.numeric(data$b)^2)
  )
  usable <- finite_complete(cfa_data, names(cfa_data))
  if (sum(usable) < 100L) stop("Too few complete colour records for CFA.", call. = FALSE)

  model <- "Pigment =~ 1*a + Lm + C"
  fit <- lavaan::cfa(
    model,
    data = cfa_data,
    estimator = "MLR",
    std.lv = FALSE,
    missing = "fiml"
  )
  if (!isTRUE(lavaan::inspect(fit, "converged"))) stop("Pigment CFA did not converge.", call. = FALSE)

  scores <- rep(NA_real_, nrow(data))
  scores[usable] <- as.numeric(lavaan::lavPredict(fit, newdata = cfa_data[usable, , drop = FALSE], type = "lv")[, "Pigment"])
  estimates <- lavaan::parameterEstimates(fit, standardized = TRUE)
  fit_measures <- data.frame(
    measure = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"),
    value = as.numeric(lavaan::fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))),
    stringsAsFactors = FALSE
  )
  list(
    scores = data.frame(analysis_id = data$analysis_id, Pigment = scores, stringsAsFactors = FALSE),
    estimates = estimates,
    fit_measures = fit_measures,
    model = model
  )
}

fit_spde_inla <- function(data, response = "Pigment", bombus = "Bombus_suitability_sum") {
  require_heavy_package("INLA")
  require_heavy_package("sf")
  predictors <- c(
    "topo_PC1", "RSDS", "Temperature_PC1", "precip_PC1",
    "soil_phys_PC1", "soil_nutrient_PC1", "soil_pH", bombus
  )
  required <- c("analysis_id", "longitude", "latitude", response, predictors)
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("SPDE-INLA input is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  keep <- finite_complete(data, c("longitude", "latitude", response, predictors))
  d <- data[keep, required, drop = FALSE]
  if (nrow(d) < 100L) stop("Too few complete records for SPDE-INLA.", call. = FALSE)

  sf_data <- sf::st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  sf_m <- sf::st_transform(sf_data, 3857)
  coordinates <- sf::st_coordinates(sf_m)
  mesh <- INLA::inla.mesh.2d(loc = coordinates, max.edge = c(20000, 100000), cutoff = 5000)
  spde <- INLA::inla.spde2.pcmatern(
    mesh,
    alpha = 2,
    prior.range = c(50000, 0.5),
    prior.sigma = c(1, 0.01)
  )
  A <- INLA::inla.spde.make.A(mesh, loc = coordinates)
  spatial_index <- INLA::inla.spde.make.index("spatial", spde$n.spde)

  X <- data.frame(Intercept = 1, d[predictors], check.names = FALSE)
  stack <- INLA::inla.stack(
    data = list(y = d[[response]]),
    A = list(A, 1),
    effects = list(spatial_index, X),
    tag = "est"
  )
  formula <- stats::as.formula(paste(
    "y ~ 0 +", paste(c("Intercept", predictors), collapse = " + "),
    "+ f(spatial, model = spde)"
  ))
  fit <- INLA::inla(
    formula,
    data = INLA::inla.stack.data(stack),
    family = "gaussian",
    control.predictor = list(A = INLA::inla.stack.A(stack), compute = TRUE),
    control.compute = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE)
  )

  fixed <- data.frame(term = rownames(fit$summary.fixed), fit$summary.fixed, row.names = NULL, check.names = FALSE)
  hyper <- data.frame(term = rownames(fit$summary.hyperpar), fit$summary.hyperpar, row.names = NULL, check.names = FALSE)
  index <- INLA::inla.stack.index(stack, "est")$data
  fitted <- data.frame(
    analysis_id = d$analysis_id,
    observed = d[[response]],
    fitted_mean = fit$summary.fitted.values$mean[index],
    fitted_sd = fit$summary.fitted.values$sd[index],
    stringsAsFactors = FALSE
  )
  fitted$residual <- fitted$observed - fitted$fitted_mean

  spatial_mean <- fit$summary.random$spatial$mean
  projector <- INLA::inla.mesh.projector(mesh, loc = coordinates)
  spatial_at_observations <- as.numeric(INLA::inla.mesh.project(projector, spatial_mean))
  linear_predictor <- fit$summary.linear.predictor$mean[index]
  fixed_part <- linear_predictor - spatial_at_observations
  observation_precision <- fit$summary.hyperpar[
    grep("Precision for the Gaussian observations", rownames(fit$summary.hyperpar)),
    "0.5quant"
  ]
  residual_variance <- if (length(observation_precision)) 1 / observation_precision[[1L]] else NA_real_
  components <- c(
    fixed = stats::var(fixed_part, na.rm = TRUE),
    spatial = stats::var(spatial_at_observations, na.rm = TRUE),
    residual = residual_variance
  )
  variance_decomposition <- data.frame(
    component = names(components),
    variance = as.numeric(components),
    proportion = as.numeric(components) / sum(components),
    stringsAsFactors = FALSE
  )
  criteria <- data.frame(
    metric = c("DIC", "WAIC"),
    value = c(fit$dic$dic, fit$waic$waic),
    stringsAsFactors = FALSE
  )
  list(fixed = fixed, hyper = hyper, fitted = fitted,
       variance_decomposition = variance_decomposition,
       criteria = criteria, records = nrow(d), formula = deparse(formula))
}

fit_residual_qgam <- function(data, residual_column = "residual") {
  require_heavy_package("qgam")
  predictors <- c("DOY", "Temperature_PC1", "precip_PC1", "RSDS", "Bombus_suitability_sum")
  required <- c("analysis_id", residual_column, predictors)
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("qGAM input is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  keep <- finite_complete(data, c(residual_column, predictors))
  d <- data[keep, required, drop = FALSE]
  if (nrow(d) < 100L) stop("Too few complete residual records for qGAM.", call. = FALSE)

  quantiles <- c(0.1, 0.5, 0.9)
  fits <- lapply(quantiles, function(q) {
    qgam::qgam(
      stats::as.formula(paste0(
        residual_column,
        " ~ s(DOY, k = 10) + s(Temperature_PC1, k = 10) + ",
        "s(precip_PC1, k = 10) + s(RSDS, k = 10) + ",
        "s(Bombus_suitability_sum, k = 10)"
      )),
      data = d,
      qu = q
    )
  })
  summary_rows <- do.call(rbind, lapply(seq_along(fits), function(i) {
    table <- as.data.frame(summary(fits[[i]])$s.table, check.names = FALSE)
    table$term <- rownames(table)
    table$quantile <- quantiles[[i]]
    rownames(table) <- NULL
    table
  }))
  predictions <- do.call(rbind, lapply(seq_along(fits), function(i) {
    data.frame(
      analysis_id = d$analysis_id,
      quantile = quantiles[[i]],
      fitted = as.numeric(stats::predict(fits[[i]], newdata = d, type = "response")),
      stringsAsFactors = FALSE
    )
  }))
  list(summary = summary_rows, predictions = predictions,
       quantiles = quantiles, records = nrow(d))
}
