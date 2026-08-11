#!/usr/bin/env Rscript

# Evaluate a predeclared set of ecologically motivated environmental
# interactions with the same two response-specific INLA-SPDE structures used
# by the current JBI broad analysis. This is a sensitivity/model-comparison
# analysis: it never replaces the frozen additive manuscript model silently.

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
output_dir <- arg_value("--output-dir", "results/environment_interaction_inla_screen")
bootstrap_reps <- as.integer(arg_value("--bootstrap-reps", "4000"))
seed <- as.integer(arg_value("--seed", "20260811"))
outcome_filter <- arg_value("--outcome", "all")

if (!file.exists(input_csv)) stop("Input data not found: ", input_csv, call. = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_packages <- c("INLA", "Matrix", "jsonlite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Missing required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

Sys.setenv(
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  INLA_NUM_THREADS = "1",
  OMP_DYNAMIC = "FALSE"
)
set.seed(seed)

write_csv <- function(x, name) {
  path <- file.path(output_dir, name)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

safe_z <- function(x) {
  x <- as.numeric(x)
  keep <- is.finite(x)
  out <- rep(NA_real_, length(x))
  if (!any(keep)) return(out)
  sx <- stats::sd(x[keep])
  if (!is.finite(sx) || sx == 0) {
    out[keep] <- 0
  } else {
    out[keep] <- (x[keep] - mean(x[keep])) / sx
  }
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

compute_vif <- function(data, terms) {
  terms <- unique(terms)
  terms <- terms[vapply(data[terms], function(x) {
    is.numeric(x) && stats::sd(x, na.rm = TRUE) > 0
  }, logical(1))]
  if (!length(terms)) return(data.frame(term = character(), VIF = numeric()))
  rows <- lapply(terms, function(term) {
    others <- setdiff(terms, term)
    if (!length(others)) return(data.frame(term = term, VIF = 1))
    cc <- stats::complete.cases(data[c(term, others)])
    fit <- stats::lm(stats::reformulate(others, response = term), data = data[cc, , drop = FALSE])
    r2 <- summary(fit)$r.squared
    data.frame(term = term, VIF = 1 / pmax(1 - r2, 1e-10))
  })
  do.call(rbind, rows)
}

cluster_bootstrap_gain <- function(loss_gain, clusters, reps, seed_offset = 0L) {
  keep <- is.finite(loss_gain) & !is.na(clusters)
  loss_gain <- loss_gain[keep]
  clusters <- as.character(clusters[keep])
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

base_terms <- c(
  "env_Temperature_PC1",
  "env_precip_PC1",
  "env_TemperatureSeasonality",
  "env_PrecipSeasonality",
  "env_topo_PC1",
  "env_soil_PC1",
  "env_soil_PC2",
  "env_RSDS"
)

registry <- data.frame(
  interaction_id = c(
    "thermal_radiation",
    "thermal_dryness",
    "dryness_radiation",
    "thermal_variability",
    "dryness_precip_variability",
    "thermal_terrain",
    "dryness_terrain",
    "radiation_terrain",
    "dryness_soil_resource",
    "dryness_soil_texture"
  ),
  term = c(
    "int_thermal_radiation",
    "int_thermal_dryness",
    "int_dryness_radiation",
    "int_thermal_variability",
    "int_dryness_precip_variability",
    "int_thermal_terrain",
    "int_dryness_terrain",
    "int_radiation_terrain",
    "int_dryness_soil_resource",
    "int_dryness_soil_texture"
  ),
  x = c(
    "env_Temperature_PC1",
    "env_Temperature_PC1",
    "env_Dryness",
    "env_Temperature_PC1",
    "env_Dryness",
    "env_Temperature_PC1",
    "env_Dryness",
    "env_RSDS",
    "env_Dryness",
    "env_Dryness"
  ),
  y = c(
    "env_RSDS",
    "env_Dryness",
    "env_RSDS",
    "env_TemperatureSeasonality",
    "env_PrecipSeasonality",
    "env_topo_PC1",
    "env_topo_PC1",
    "env_topo_PC1",
    "env_soil_PC1",
    "env_soil_PC2"
  ),
  mechanism_family = c(
    "climate_stress",
    "climate_stress",
    "climate_stress",
    "stress_variability",
    "stress_variability",
    "terrain_context",
    "terrain_context",
    "terrain_context",
    "substrate_buffering",
    "substrate_buffering"
  ),
  directional_expectation = c(
    "antagonistic_or_context_dependent",
    "context_dependent",
    "positive_joint_stress",
    "context_dependent",
    "positive_joint_stress",
    "context_dependent",
    "context_dependent",
    "context_dependent",
    "buffering_expected_but_PC_is_composite",
    "buffering_expected_silt_positive_sand_negative"
  ),
  rationale = c(
    "Light can induce floral anthocyanin whereas warm conditions can suppress it; tests whether the radiation association changes along the thermal axis.",
    "Water limitation and temperature jointly determine oxidative and developmental stress, but heat can also suppress anthocyanin biosynthesis.",
    "Joint water limitation and high solar load can increase radiative, thermal and oxidative stress and the potential benefit of flavonoid investment.",
    "Mean warm-season conditions may have different effects where annual thermal variability changes exposure to cold or heat extremes.",
    "The biological effect of low mean moisture may strengthen where precipitation is more intermittent.",
    "Terrain relief can create local microclimatic heterogeneity that modifies a broad thermal regime.",
    "Slope and local relief can alter drainage, exposure and refugial moisture, modifying the effect of climatic dryness.",
    "Terrain relief can alter local exposure and heat load, modifying the association with broad shortwave radiation.",
    "Organic matter, nutrient status, bulk density and pH can buffer or constrain physiological responses to climatic dryness; Soil PC1 is explicitly composite.",
    "The silt-positive, sand-negative texture axis can modify water retention and therefore the effect of climatic dryness."
  ),
  stringsAsFactors = FALSE
)

model_registry <- rbind(
  data.frame(model_id = "base_additive", model_class = "base", interaction_terms = "", stringsAsFactors = FALSE),
  data.frame(
    model_id = paste0("single_", registry$interaction_id),
    model_class = "single_interaction",
    interaction_terms = registry$term,
    stringsAsFactors = FALSE
  ),
  data.frame(
    model_id = c("bundle_climate_stress", "bundle_stress_variability", "bundle_terrain_context", "bundle_substrate_buffering", "all_mechanistic_interactions"),
    model_class = c("mechanism_bundle", "mechanism_bundle", "mechanism_bundle", "mechanism_bundle", "global_interaction_set"),
    interaction_terms = c(
      paste(registry$term[registry$mechanism_family == "climate_stress"], collapse = ";"),
      paste(registry$term[registry$mechanism_family == "stress_variability"], collapse = ";"),
      paste(registry$term[registry$mechanism_family == "terrain_context"], collapse = ";"),
      paste(registry$term[registry$mechanism_family == "substrate_buffering"], collapse = ";"),
      paste(registry$term, collapse = ";")
    ),
    stringsAsFactors = FALSE
  )
)

split_terms <- function(x) {
  if (is.na(x) || !nzchar(x)) character() else strsplit(x, ";", fixed = TRUE)[[1]]
}

data <- utils::read.csv(input_csv, check.names = FALSE, stringsAsFactors = FALSE)
required_columns <- unique(c(
  base_terms, "region", "x_km", "y_km", "spatial_fold", "spatial_block",
  "pigmented_mixture50", "pigment_intensity_z"
))
missing_columns <- setdiff(required_columns, names(data))
if (length(missing_columns)) {
  stop("Input data lack required columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

data$region <- factor(data$region, levels = c("West", "East"))
data$env_Dryness <- -as.numeric(data$env_precip_PC1)
for (i in seq_len(nrow(registry))) {
  raw_product <- as.numeric(data[[registry$x[i]]]) * as.numeric(data[[registry$y[i]]])
  data[[registry$term[i]]] <- safe_z(raw_product)
}

write_csv(registry, "interaction_registry.csv")
write_csv(model_registry, "model_registry.csv")

quadrant_rows <- list()
for (i in seq_len(nrow(registry))) {
  x <- data[[registry$x[i]]]
  y <- data[[registry$y[i]]]
  x_group <- ifelse(x >= 0, "high", "low")
  y_group <- ifelse(y >= 0, "high", "low")
  tab <- as.data.frame(table(x_group, y_group), stringsAsFactors = FALSE)
  names(tab) <- c("x_group", "y_group", "n")
  tab$interaction_id <- registry$interaction_id[i]
  tab$proportion <- tab$n / sum(tab$n)
  quadrant_rows[[i]] <- tab[c("interaction_id", "x_group", "y_group", "n", "proportion")]
}
quadrant_support <- do.call(rbind, quadrant_rows)
write_csv(quadrant_support, "interaction_quadrant_support.csv")

outcomes_all <- list(
  pigmentation_state = list(response = "pigmented_mixture50", family = "binomial"),
  conditional_intensity = list(response = "pigment_intensity_z", family = "gaussian")
)
if (identical(outcome_filter, "all")) {
  outcomes <- outcomes_all
} else {
  if (!(outcome_filter %in% names(outcomes_all))) {
    stop("Unknown --outcome: ", outcome_filter, call. = FALSE)
  }
  outcomes <- outcomes_all[outcome_filter]
}

make_fixed <- function(d, interaction_terms) {
  out <- data.frame(
    intercept = 1,
    regionEast = as.integer(d$region == "East"),
    stringsAsFactors = FALSE
  )
  for (term in c(base_terms, interaction_terms)) out[[term]] <- as.numeric(d[[term]])
  out
}

make_formula <- function(fixed_names, spde) {
  rhs <- paste(fixed_names, collapse = " + ")
  stats::as.formula(paste0("y ~ -1 + ", rhs, " + f(spatial_field, model = spde)"))
}

fit_spde <- function(d, response, family, interaction_terms, mesh, spde, train_rows, pred_rows = integer()) {
  fixed <- make_fixed(d, interaction_terms)
  coords <- as.matrix(d[c("x_km", "y_km")])
  A <- INLA::inla.spde.make.A(mesh = mesh, loc = coords)
  spatial_index <- INLA::inla.spde.make.index("spatial_field", spde$n.spde)

  est_stack <- INLA::inla.stack(
    data = list(y = d[[response]][train_rows]),
    A = list(1, A[train_rows, , drop = FALSE]),
    effects = list(fixed[train_rows, , drop = FALSE], spatial_index),
    tag = "est"
  )
  stacks <- list(est_stack)
  if (length(pred_rows)) {
    pred_stack <- INLA::inla.stack(
      data = list(y = rep(NA_real_, length(pred_rows))),
      A = list(1, A[pred_rows, , drop = FALSE]),
      effects = list(fixed[pred_rows, , drop = FALSE], spatial_index),
      tag = "pred"
    )
    stacks <- c(stacks, list(pred_stack))
  }
  stack <- do.call(INLA::inla.stack, stacks)
  formula <- make_formula(names(fixed), spde)

  fit <- INLA::inla(
    formula,
    family = family,
    data = INLA::inla.stack.data(stack),
    control.predictor = list(A = INLA::inla.stack.A(stack), compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = FALSE),
    control.inla = list(strategy = "adaptive"),
    num.threads = "1:1",
    verbose = FALSE
  )

  prediction <- NULL
  if (length(pred_rows)) {
    index <- INLA::inla.stack.index(stack, "pred")$data
    prediction <- data.frame(
      row_index = pred_rows,
      mean = fit$summary.fitted.values$mean[index],
      sd = fit$summary.fitted.values$sd[index],
      lower_95 = fit$summary.fitted.values$`0.025quant`[index],
      upper_95 = fit$summary.fitted.values$`0.975quant`[index]
    )
  }
  list(fit = fit, prediction = prediction, formula = paste(deparse(formula), collapse = " "))
}

extract_full_metrics <- function(fit, outcome, model_id, n) {
  cpo <- as.numeric(fit$cpo$cpo)
  cpo <- cpo[is.finite(cpo) & cpo > 0]
  data.frame(
    outcome = outcome,
    model_id = model_id,
    n = n,
    DIC = fit$dic$dic,
    WAIC = fit$waic$waic,
    WAIC_effective_parameters = fit$waic$p.eff,
    mean_log_CPO = if (length(cpo)) mean(log(cpo)) else NA_real_,
    sum_log_CPO = if (length(cpo)) sum(log(cpo)) else NA_real_,
    cpo_failure_count = if (is.null(fit$cpo$failure)) NA_integer_ else sum(fit$cpo$failure > 0),
    stringsAsFactors = FALSE
  )
}

extract_fixed <- function(fit, outcome, model_id) {
  tab <- fit$summary.fixed
  rows <- lapply(rownames(tab), function(term) {
    marginal <- fit$marginals.fixed[[term]]
    p_gt_zero <- if (is.null(marginal)) NA_real_ else 1 - INLA::inla.pmarginal(0, marginal)
    data.frame(
      outcome = outcome,
      model_id = model_id,
      term = term,
      mean = tab[term, "mean"],
      sd = tab[term, "sd"],
      lower_95 = tab[term, "0.025quant"],
      median = tab[term, "0.5quant"],
      upper_95 = tab[term, "0.975quant"],
      p_gt_zero = p_gt_zero,
      posterior_two_tail = if (is.finite(p_gt_zero)) 2 * min(p_gt_zero, 1 - p_gt_zero) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

extract_hyper <- function(fit, outcome, model_id) {
  tab <- fit$summary.hyperpar
  data.frame(
    outcome = outcome,
    model_id = model_id,
    hyperparameter = rownames(tab),
    mean = tab[, "mean"],
    sd = tab[, "sd"],
    lower_95 = tab[, "0.025quant"],
    median = tab[, "0.5quant"],
    upper_95 = tab[, "0.975quant"],
    stringsAsFactors = FALSE
  )
}

presence_metrics <- function(y, p) {
  p <- pmin(pmax(as.numeric(p), 1e-8), 1 - 1e-8)
  y <- as.integer(y)
  data.frame(
    log_loss = -mean(y * log(p) + (1 - y) * log(1 - p)),
    brier = mean((y - p)^2),
    AUC = binary_auc(y, p),
    RMSE = NA_real_,
    MAE = NA_real_,
    R2 = NA_real_
  )
}

intensity_metrics <- function(y, prediction) {
  residual <- as.numeric(y) - as.numeric(prediction)
  denom <- sum((y - mean(y))^2)
  data.frame(
    log_loss = NA_real_,
    brier = NA_real_,
    AUC = NA_real_,
    RMSE = sqrt(mean(residual^2)),
    MAE = mean(abs(residual)),
    R2 = if (denom > 0) 1 - sum(residual^2) / denom else NA_real_
  )
}

full_metric_rows <- list()
fixed_rows <- list()
hyper_rows <- list()
cv_prediction_rows <- list()
cv_fold_rows <- list()
vif_rows <- list()
formula_rows <- list()

for (outcome_name in names(outcomes)) {
  spec <- outcomes[[outcome_name]]
  keep <- stats::complete.cases(data[c(spec$response, base_terms, registry$term, "region", "x_km", "y_km", "spatial_fold", "spatial_block")])
  d <- droplevels(data[keep, , drop = FALSE])
  d$.original_row <- which(keep)
  coords <- as.matrix(d[c("x_km", "y_km")])
  mesh <- INLA::inla.mesh.2d(loc = coords, max.edge = c(20, 100), cutoff = 5)
  spde <- INLA::inla.spde2.pcmatern(
    mesh, alpha = 2,
    prior.range = c(100, 0.05),
    prior.sigma = c(1, 0.05)
  )
  message("[interaction screen] outcome=", outcome_name, " n=", nrow(d), " mesh=", mesh$n)

  for (m in seq_len(nrow(model_registry))) {
    model_id <- model_registry$model_id[m]
    interaction_terms <- split_terms(model_registry$interaction_terms[m])
    message("[interaction screen] full fit ", outcome_name, " / ", model_id)
    full <- fit_spde(
      d, spec$response, spec$family, interaction_terms,
      mesh, spde, seq_len(nrow(d))
    )
    full_metric_rows[[length(full_metric_rows) + 1L]] <- extract_full_metrics(
      full$fit, outcome_name, model_id, nrow(d)
    )
    fixed_rows[[length(fixed_rows) + 1L]] <- extract_fixed(full$fit, outcome_name, model_id)
    hyper_rows[[length(hyper_rows) + 1L]] <- extract_hyper(full$fit, outcome_name, model_id)
    formula_rows[[length(formula_rows) + 1L]] <- data.frame(
      outcome = outcome_name,
      model_id = model_id,
      formula = full$formula,
      stringsAsFactors = FALSE
    )

    fixed_for_vif <- make_fixed(d, interaction_terms)
    vif <- compute_vif(fixed_for_vif, setdiff(names(fixed_for_vif), "intercept"))
    vif$outcome <- outcome_name
    vif$model_id <- model_id
    vif_rows[[length(vif_rows) + 1L]] <- vif[c("outcome", "model_id", "term", "VIF")]

    folds <- sort(unique(d$spatial_fold))
    for (fold in folds) {
      train <- which(d$spatial_fold != fold)
      test <- which(d$spatial_fold == fold)
      message("[interaction screen] CV fit ", outcome_name, " / ", model_id, " / fold ", fold)
      cv <- fit_spde(
        d, spec$response, spec$family, interaction_terms,
        mesh, spde, train, test
      )
      pred <- cv$prediction
      pred$outcome <- outcome_name
      pred$model_id <- model_id
      pred$fold <- fold
      pred$observed <- d[[spec$response]][pred$row_index]
      pred$spatial_block <- d$spatial_block[pred$row_index]
      pred$original_row <- d$.original_row[pred$row_index]
      cv_prediction_rows[[length(cv_prediction_rows) + 1L]] <- pred

      metrics <- if (spec$family == "binomial") {
        presence_metrics(pred$observed, pred$mean)
      } else {
        intensity_metrics(pred$observed, pred$mean)
      }
      metrics$outcome <- outcome_name
      metrics$model_id <- model_id
      metrics$fold <- fold
      metrics$n_test <- nrow(pred)
      cv_fold_rows[[length(cv_fold_rows) + 1L]] <- metrics[c(
        "outcome", "model_id", "fold", "n_test", "log_loss", "brier", "AUC", "RMSE", "MAE", "R2"
      )]
    }
  }
}

full_metrics <- do.call(rbind, full_metric_rows)
fixed_effects <- do.call(rbind, fixed_rows)
hyperparameters <- do.call(rbind, hyper_rows)
cv_predictions <- do.call(rbind, cv_prediction_rows)
cv_fold_metrics <- do.call(rbind, cv_fold_rows)
vif_diagnostics <- do.call(rbind, vif_rows)
formula_registry <- do.call(rbind, formula_rows)

# BH adjustment is applied only to the ten predeclared single-interaction
# coefficient screens within each response. It is a transparency device, not a
# replacement for predictive validation.
single_ids <- paste0("single_", registry$interaction_id)
fixed_effects$posterior_two_tail_BH <- NA_real_
for (outcome_name in names(outcomes)) {
  idx <- fixed_effects$outcome == outcome_name &
    fixed_effects$model_id %in% single_ids &
    fixed_effects$term %in% registry$term
  fixed_effects$posterior_two_tail_BH[idx] <- stats::p.adjust(
    fixed_effects$posterior_two_tail[idx], method = "BH"
  )
}

aggregate_rows <- list()
comparison_rows <- list()
bootstrap_rows <- list()
for (outcome_name in names(outcomes)) {
  spec <- outcomes[[outcome_name]]
  pred_outcome <- cv_predictions[cv_predictions$outcome == outcome_name, , drop = FALSE]
  base <- pred_outcome[pred_outcome$model_id == "base_additive", c("original_row", "observed", "mean", "spatial_block")]
  names(base)[names(base) == "mean"] <- "base_prediction"

  for (model_id in model_registry$model_id) {
    current <- pred_outcome[pred_outcome$model_id == model_id, c("original_row", "observed", "mean", "spatial_block", "fold")]
    names(current)[names(current) == "mean"] <- "candidate_prediction"
    paired <- merge(
      current,
      base[c("original_row", "base_prediction")],
      by = "original_row", all.x = TRUE, sort = FALSE
    )
    metric <- if (spec$family == "binomial") {
      presence_metrics(paired$observed, paired$candidate_prediction)
    } else {
      intensity_metrics(paired$observed, paired$candidate_prediction)
    }
    metric$outcome <- outcome_name
    metric$model_id <- model_id
    metric$n <- nrow(paired)
    aggregate_rows[[length(aggregate_rows) + 1L]] <- metric[c(
      "outcome", "model_id", "n", "log_loss", "brier", "AUC", "RMSE", "MAE", "R2"
    )]

    if (spec$family == "binomial") {
      y <- as.integer(paired$observed)
      p0 <- pmin(pmax(paired$base_prediction, 1e-8), 1 - 1e-8)
      p1 <- pmin(pmax(paired$candidate_prediction, 1e-8), 1 - 1e-8)
      base_loss <- -(y * log(p0) + (1 - y) * log(1 - p0))
      candidate_loss <- -(y * log(p1) + (1 - y) * log(1 - p1))
    } else {
      base_loss <- (paired$observed - paired$base_prediction)^2
      candidate_loss <- (paired$observed - paired$candidate_prediction)^2
    }
    gain <- base_loss - candidate_loss
    boot <- cluster_bootstrap_gain(
      gain, paired$spatial_block, bootstrap_reps,
      seed_offset = match(outcome_name, names(outcomes)) * 1000L + match(model_id, model_registry$model_id)
    )
    bootstrap_rows[[length(bootstrap_rows) + 1L]] <- data.frame(
      outcome = outcome_name,
      model_id = model_id,
      primary_loss = if (spec$family == "binomial") "log_loss" else "squared_error",
      mean_loss_gain = boot[["mean_gain"]],
      lower_95 = boot[["lower_95"]],
      upper_95 = boot[["upper_95"]],
      bootstrap_probability_positive = boot[["probability_positive"]],
      stringsAsFactors = FALSE
    )
  }
}

cv_aggregate <- do.call(rbind, aggregate_rows)
bootstrap_gain <- do.call(rbind, bootstrap_rows)

for (outcome_name in names(outcomes)) {
  base_full <- full_metrics[full_metrics$outcome == outcome_name & full_metrics$model_id == "base_additive", ]
  base_cv <- cv_aggregate[cv_aggregate$outcome == outcome_name & cv_aggregate$model_id == "base_additive", ]
  for (model_id in model_registry$model_id) {
    full_row <- full_metrics[full_metrics$outcome == outcome_name & full_metrics$model_id == model_id, ]
    cv_row <- cv_aggregate[cv_aggregate$outcome == outcome_name & cv_aggregate$model_id == model_id, ]
    fold_candidate <- cv_fold_metrics[cv_fold_metrics$outcome == outcome_name & cv_fold_metrics$model_id == model_id, ]
    fold_base <- cv_fold_metrics[cv_fold_metrics$outcome == outcome_name & cv_fold_metrics$model_id == "base_additive", ]
    merged_fold <- merge(fold_candidate, fold_base, by = c("outcome", "fold"), suffixes = c("_candidate", "_base"))
    if (outcomes[[outcome_name]]$family == "binomial") {
      fold_gain <- merged_fold$log_loss_base - merged_fold$log_loss_candidate
      aggregate_gain <- base_cv$log_loss - cv_row$log_loss
      relative_gain <- aggregate_gain / base_cv$log_loss
    } else {
      fold_gain <- merged_fold$RMSE_base - merged_fold$RMSE_candidate
      aggregate_gain <- base_cv$RMSE - cv_row$RMSE
      relative_gain <- aggregate_gain / base_cv$RMSE
    }
    boot <- bootstrap_gain[bootstrap_gain$outcome == outcome_name & bootstrap_gain$model_id == model_id, ]
    model_vif <- vif_diagnostics[vif_diagnostics$outcome == outcome_name & vif_diagnostics$model_id == model_id, ]
    comparison_rows[[length(comparison_rows) + 1L]] <- data.frame(
      outcome = outcome_name,
      model_id = model_id,
      model_class = model_registry$model_class[match(model_id, model_registry$model_id)],
      delta_WAIC_base_minus_candidate = base_full$WAIC - full_row$WAIC,
      delta_mean_log_CPO_candidate_minus_base = full_row$mean_log_CPO - base_full$mean_log_CPO,
      aggregate_primary_loss_gain = aggregate_gain,
      relative_primary_loss_gain = relative_gain,
      folds_improved = sum(fold_gain > 0, na.rm = TRUE),
      folds_total = length(fold_gain),
      cluster_bootstrap_lower_95 = boot$lower_95,
      cluster_bootstrap_upper_95 = boot$upper_95,
      cluster_bootstrap_probability_positive = boot$bootstrap_probability_positive,
      max_VIF = if (nrow(model_vif)) max(model_vif$VIF, na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}
model_comparison <- do.call(rbind, comparison_rows)

# Transparent decision categories. A model is not promoted by in-sample fit
# alone. "strong" requires positive blocked predictive gain with a spatial-
# block bootstrap interval above zero, improvement in at least four folds and
# acceptable fixed-effect collinearity. Single terms additionally require a
# 95% CrI excluding zero after the predeclared family screen.
model_comparison$evidence_class <- "unsupported_or_mixed"
for (i in seq_len(nrow(model_comparison))) {
  row <- model_comparison[i, ]
  predictive_strong <- is.finite(row$cluster_bootstrap_lower_95) && row$cluster_bootstrap_lower_95 > 0 &&
    row$folds_improved >= 4 && is.finite(row$max_VIF) && row$max_VIF < 10
  predictive_suggestive <- is.finite(row$aggregate_primary_loss_gain) && row$aggregate_primary_loss_gain > 0 &&
    row$folds_improved >= 3
  if (row$model_id == "base_additive") {
    model_comparison$evidence_class[i] <- "reference"
  } else if (row$model_class == "single_interaction") {
    interaction_id <- sub("^single_", "", row$model_id)
    term <- registry$term[match(interaction_id, registry$interaction_id)]
    coef_row <- fixed_effects[
      fixed_effects$outcome == row$outcome & fixed_effects$model_id == row$model_id & fixed_effects$term == term,
      , drop = FALSE
    ]
    coefficient_strong <- nrow(coef_row) == 1L &&
      ((coef_row$lower_95 > 0) || (coef_row$upper_95 < 0)) &&
      is.finite(coef_row$posterior_two_tail_BH) && coef_row$posterior_two_tail_BH < 0.05
    if (predictive_strong && coefficient_strong) {
      model_comparison$evidence_class[i] <- "strong_coefficient_and_predictive_support"
    } else if (predictive_strong || (predictive_suggestive && coefficient_strong)) {
      model_comparison$evidence_class[i] <- "suggestive_requires_cautious_SI_interpretation"
    }
  } else if (predictive_strong) {
    model_comparison$evidence_class[i] <- "strong_bundle_predictive_support"
  } else if (predictive_suggestive) {
    model_comparison$evidence_class[i] <- "suggestive_bundle_support"
  }
}

# Confirm that the independently reconstructed additive model is aligned with
# the frozen manuscript-facing observation-level INLA-SPDE fit before any
# interaction comparison is interpreted. Tolerances allow only minor numerical
# differences from software/platform execution.
lock_targets <- data.frame(
  outcome = c(
    "pigmentation_state", "conditional_intensity", "conditional_intensity",
    "pigmentation_state", "conditional_intensity"
  ),
  quantity = c(
    "fixed:env_Temperature_PC1", "fixed:env_Temperature_PC1",
    "fixed:env_topo_PC1", "WAIC", "WAIC"
  ),
  expected = c(-0.542, -0.319, -0.138, 1577.233, 2573.818),
  tolerance = c(0.06, 0.04, 0.04, 12, 12),
  stringsAsFactors = FALSE
)
lock_rows <- lapply(seq_len(nrow(lock_targets)), function(i) {
  target <- lock_targets[i, ]
  if (startsWith(target$quantity, "fixed:")) {
    term <- sub("^fixed:", "", target$quantity)
    observed <- fixed_effects$mean[
      fixed_effects$outcome == target$outcome &
        fixed_effects$model_id == "base_additive" &
        fixed_effects$term == term
    ]
  } else {
    observed <- full_metrics$WAIC[
      full_metrics$outcome == target$outcome &
        full_metrics$model_id == "base_additive"
    ]
  }
  if (length(observed) != 1L) observed <- NA_real_
  data.frame(
    outcome = target$outcome, quantity = target$quantity,
    expected = target$expected, observed = observed,
    absolute_difference = abs(observed - target$expected),
    tolerance = target$tolerance,
    pass = is.finite(observed) && abs(observed - target$expected) <= target$tolerance,
    stringsAsFactors = FALSE
  )
})
base_model_lock_check <- do.call(rbind, lock_rows)
write_csv(base_model_lock_check, "base_model_lock_check.csv")
if (!all(base_model_lock_check$pass)) {
  stop("Additive INLA-SPDE reconstruction did not match the frozen manuscript model within tolerance.", call. = FALSE)
}

write_csv(full_metrics, "full_model_fit_metrics.csv")
write_csv(fixed_effects, "fixed_effect_posteriors.csv")
write_csv(hyperparameters, "spatial_hyperparameters.csv")
write_csv(cv_predictions, "blocked_cv_predictions.csv")
write_csv(cv_fold_metrics, "blocked_cv_fold_metrics.csv")
write_csv(cv_aggregate, "blocked_cv_aggregate_metrics.csv")
write_csv(bootstrap_gain, "blocked_cv_cluster_bootstrap_gain.csv")
write_csv(vif_diagnostics, "fixed_effect_vif.csv")
write_csv(formula_registry, "formula_registry.csv")
write_csv(model_comparison, "model_comparison_summary.csv")

single_rows <- list()
for (outcome_name in names(outcomes)) {
  for (i in seq_len(nrow(registry))) {
    model_id <- paste0("single_", registry$interaction_id[i])
    cmp <- model_comparison[model_comparison$outcome == outcome_name & model_comparison$model_id == model_id, ]
    coef <- fixed_effects[
      fixed_effects$outcome == outcome_name & fixed_effects$model_id == model_id & fixed_effects$term == registry$term[i],
      , drop = FALSE
    ]
    single_rows[[length(single_rows) + 1L]] <- data.frame(
      outcome = outcome_name,
      interaction_id = registry$interaction_id[i],
      term = registry$term[i],
      mechanism_family = registry$mechanism_family[i],
      directional_expectation = registry$directional_expectation[i],
      coefficient_mean = coef$mean,
      coefficient_lower_95 = coef$lower_95,
      coefficient_upper_95 = coef$upper_95,
      p_gt_zero = coef$p_gt_zero,
      posterior_two_tail_BH = coef$posterior_two_tail_BH,
      delta_WAIC_base_minus_candidate = cmp$delta_WAIC_base_minus_candidate,
      delta_mean_log_CPO_candidate_minus_base = cmp$delta_mean_log_CPO_candidate_minus_base,
      aggregate_primary_loss_gain = cmp$aggregate_primary_loss_gain,
      relative_primary_loss_gain = cmp$relative_primary_loss_gain,
      folds_improved = cmp$folds_improved,
      cluster_bootstrap_lower_95 = cmp$cluster_bootstrap_lower_95,
      cluster_bootstrap_upper_95 = cmp$cluster_bootstrap_upper_95,
      cluster_bootstrap_probability_positive = cmp$cluster_bootstrap_probability_positive,
      max_VIF = cmp$max_VIF,
      evidence_class = cmp$evidence_class,
      stringsAsFactors = FALSE
    )
  }
}
single_interaction_summary <- do.call(rbind, single_rows)
write_csv(single_interaction_summary, "single_interaction_summary.csv")

validation <- list(
  status = "PASS",
  source_input = normalizePath(input_csv, winslash = "/", mustWork = TRUE),
  n_source_rows = nrow(data),
  outcomes = lapply(names(outcomes), function(name) {
    list(
      outcome = name,
      n = unique(full_metrics$n[full_metrics$outcome == name]),
      models = sum(full_metrics$outcome == name),
      folds = sort(unique(cv_fold_metrics$fold[cv_fold_metrics$outcome == name]))
    )
  }),
  predeclared_interactions = nrow(registry),
  compared_models_per_outcome = nrow(model_registry),
  expected_models_total = nrow(model_registry) * length(outcomes),
  observed_models_total = nrow(full_metrics),
  bootstrap_reps = bootstrap_reps,
  seed = seed,
  manuscript_model_replaced = FALSE,
  interpretation = paste(
    "This sensitivity screen compares predeclared ecologically motivated interactions",
    "against the frozen additive INLA-SPDE structure. An interaction is not promoted",
    "from coefficient significance or WAIC alone; geographically blocked paired loss",
    "and spatial-block bootstrap support are required."
  )
)
if (nrow(full_metrics) != validation$expected_models_total) {
  stop("Model count mismatch: expected ", validation$expected_models_total, " observed ", nrow(full_metrics), call. = FALSE)
}
if (any(!is.finite(full_metrics$WAIC))) stop("Non-finite WAIC found.", call. = FALSE)
if (any(!is.finite(cv_predictions$mean))) stop("Non-finite blocked predictions found.", call. = FALSE)
if (any(model_comparison$max_VIF >= 50, na.rm = TRUE)) stop("Interaction design produced extreme VIF >= 50.", call. = FALSE)

jsonlite::write_json(
  validation,
  file.path(output_dir, "validation.json"),
  auto_unbox = TRUE, pretty = TRUE
)

strong <- model_comparison[grepl("^strong", model_comparison$evidence_class), , drop = FALSE]
suggestive <- model_comparison[grepl("^suggestive", model_comparison$evidence_class), , drop = FALSE]
report_lines <- c(
  "# Environmental interaction INLA-SPDE screen",
  "",
  paste0("- Source rows: **", nrow(data), "**"),
  paste0("- Predeclared interactions: **", nrow(registry), "**"),
  paste0("- Candidate models per response: **", nrow(model_registry), "**"),
  paste0("- Spatial-block bootstrap replicates: **", bootstrap_reps, "**"),
  "- Frozen additive manuscript model replaced: **NO**",
  "",
  "## Decision rule",
  "",
  "An interaction is not promoted from an in-sample coefficient or WAIC improvement alone. Strong support requires a posterior interval excluding zero after the predeclared family screen, positive geographically blocked predictive gain whose spatial-block bootstrap interval excludes zero, improvement in at least four of five folds, and maximum fixed-effect VIF below 10.",
  "",
  "## Strongly supported extensions",
  ""
)
if (nrow(strong)) {
  report_lines <- c(report_lines, apply(strong, 1, function(row) {
    paste0("- ", row[["outcome"]], " / `", row[["model_id"]], "`: ", row[["evidence_class"]])
  }))
} else {
  report_lines <- c(report_lines, "- None under the joint coefficient-plus-prediction rule.")
}
report_lines <- c(report_lines, "", "## Suggestive extensions", "")
if (nrow(suggestive)) {
  report_lines <- c(report_lines, apply(suggestive, 1, function(row) {
    paste0("- ", row[["outcome"]], " / `", row[["model_id"]], "`: ", row[["evidence_class"]])
  }))
} else {
  report_lines <- c(report_lines, "- None.")
}
report_lines <- c(
  report_lines,
  "",
  "## Interpretation ceiling",
  "",
  "The environmental rasters are long-term, spatially structured proxies. RSDS is total shortwave radiation rather than UV-B; Precipitation PC1 is not flower-level water status; Topography PC1 is slope/TRI/roughness rather than directly measured microclimate; soil PCs are composite axes. Any retained interaction is therefore a geographically predictive ecological context, not direct evidence of a molecular anthocyanin mechanism.",
  ""
)
writeLines(report_lines, file.path(output_dir, "REPORT.md"), useBytes = TRUE)

cat("Environment interaction INLA-SPDE screen completed: ", output_dir, "\n", sep = "")
