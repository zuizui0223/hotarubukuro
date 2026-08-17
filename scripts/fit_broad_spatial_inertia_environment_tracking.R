args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/natural_predictive_model.R")
source("R/broad_spatial_inertia_environment_tracking.R")

arg_value <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", name, "="), "", hit[[1L]])
}

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
output_dir <- arg_value(
  "--output", "results/broad_spatial_inertia_environment_tracking"
)
n_samples <- as.integer(arg_value("--samples", "500"))
seed <- as.integer(arg_value("--seed", "20260725"))
inla_verbose <- identical(tolower(arg_value("--inla-verbose", "false")), "true")

if (!file.exists(cells_path)) stop("Missing cell table: ", cells_path, call. = FALSE)
if (!is.finite(n_samples) || n_samples < 100L) stop("--samples must be >= 100", call. = FALSE)
if (!requireNamespace("INLA", quietly = TRUE)) stop("Package 'INLA' is required.", call. = FALSE)
if (!requireNamespace("Matrix", quietly = TRUE)) stop("Package 'Matrix' is required.", call. = FALSE)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
environment_terms <- v16_environment_terms(50)
v16_assert_columns(
  cells,
  c("exact_site_id", "x_km", "y_km", "spatial_fold", "n_pigmented",
    "n_observations", "conditional_intensity_median", environment_terms),
  "cells"
)

log_mean_exp <- function(x) {
  m <- max(x)
  if (!is.finite(m)) return(m)
  m + log(mean(exp(x - m)))
}

posterior_score <- function(eta, observed, family, trials = NULL, precision = NULL) {
  if (!is.matrix(eta)) eta <- matrix(eta, nrow = length(observed))
  if (family == "binomial") {
    probability <- stats::plogis(eta)
    point_lpd <- vapply(seq_along(observed), function(i) {
      log_mean_exp(stats::dbinom(
        observed[[i]], size = trials[[i]], prob = probability[i, ], log = TRUE
      ))
    }, numeric(1))
    predicted_mean <- rowMeans(probability)
    list(lpd = sum(point_lpd), point_lpd = point_lpd, mean = predicted_mean)
  } else {
    stopifnot(length(precision) == ncol(eta))
    point_lpd <- vapply(seq_along(observed), function(i) {
      log_mean_exp(stats::dnorm(
        observed[[i]], mean = eta[i, ], sd = sqrt(1 / precision), log = TRUE
      ))
    }, numeric(1))
    list(lpd = sum(point_lpd), point_lpd = point_lpd, mean = rowMeans(eta))
  }
}

sample_predictor <- function(fit, prediction_index, family, seed_value) {
  samples <- INLA::inla.posterior.sample(
    n = n_samples, result = fit,
    selection = list(APredictor = prediction_index),
    seed = as.integer(seed_value), num.threads = 1,
    parallel.configs = FALSE, add.names = TRUE
  )
  order <- v16_predictor_row_order(samples[[1L]], prediction_index, "APredictor")
  eta <- vapply(
    samples, function(sample) as.numeric(sample$latent[order, 1L]),
    numeric(length(prediction_index))
  )
  if (!is.matrix(eta)) eta <- matrix(eta, nrow = length(prediction_index))
  precision <- if (family == "gaussian") {
    vapply(samples, v16_observation_precision, numeric(1))
  } else NULL
  list(eta = eta, precision = precision)
}

fit_fixed_fold <- function(train, test, response, family, basis,
                           trials = NULL, seed_value) {
  train_X <- cbind(Intercept = 1, basis$train)
  test_X <- cbind(Intercept = 1, basis$test)
  all_X <- rbind(train_X, test_X)
  fit_data <- as.data.frame(all_X)
  fit_data$y <- c(train[[response]], rep(NA_real_, nrow(test)))
  fixed_terms <- colnames(all_X)
  formula <- stats::as.formula(
    paste("y ~ -1 +", paste(fixed_terms, collapse = " + "))
  )
  arguments <- list(
    formula = formula, data = fit_data, family = family,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(config = TRUE), verbose = inla_verbose
  )
  if (family == "binomial") {
    arguments$Ntrials <- c(train[[trials]], test[[trials]])
  }
  fit <- do.call(INLA::inla, arguments)
  prediction_index <- seq.int(nrow(train) + 1L, nrow(train) + nrow(test))
  sample_predictor(fit, prediction_index, family, seed_value)
}

fit_spde_fold <- function(train, test, response, family, basis, mesh,
                          trials = NULL, seed_value) {
  train_X <- cbind(Intercept = 1, basis$train)
  test_X <- cbind(Intercept = 1, basis$test)
  A_train <- INLA::inla.spde.make.A(mesh, loc = as.matrix(train[c("x_km", "y_km")]))
  A_test <- INLA::inla.spde.make.A(mesh, loc = as.matrix(test[c("x_km", "y_km")]))
  spde <- v16_make_spde(mesh, A_train)
  estimation_data <- list(y = train[[response]])
  prediction_data <- list(y = rep(NA_real_, nrow(test)))
  if (family == "binomial") {
    estimation_data$Ntrials <- train[[trials]]
    prediction_data$Ntrials <- test[[trials]]
  }
  stack_estimation <- INLA::inla.stack(
    data = estimation_data, A = list(A_train, 1),
    effects = list(spatial = seq_len(spde$n.spde), X = train_X),
    tag = "est", compress = FALSE, remove.unused = FALSE
  )
  stack_prediction <- INLA::inla.stack(
    data = prediction_data, A = list(A_test, 1),
    effects = list(spatial = seq_len(spde$n.spde), X = test_X),
    tag = "pred", compress = FALSE, remove.unused = FALSE
  )
  stack <- INLA::inla.stack(stack_estimation, stack_prediction)
  stack_data <- INLA::inla.stack.data(stack)
  fixed_terms <- colnames(train_X)
  formula <- stats::as.formula(
    paste(
      "y ~ -1 +", paste(fixed_terms, collapse = " + "),
      "+ f(spatial, model = spde)"
    ), env = environment()
  )
  arguments <- list(
    formula = formula, data = stack_data, family = family,
    control.predictor = list(A = INLA::inla.stack.A(stack), compute = TRUE, link = 1),
    control.compute = list(config = TRUE), verbose = inla_verbose
  )
  if (family == "binomial") arguments$Ntrials <- stack_data$Ntrials
  fit <- do.call(INLA::inla, arguments)
  prediction_index <- INLA::inla.stack.index(stack, "pred")$data
  sample_predictor(fit, prediction_index, family, seed_value)
}

empty_basis <- function(train, test) {
  list(
    train = data.frame(row.names = seq_len(nrow(train))),
    test = data.frame(row.names = seq_len(nrow(test)))
  )
}

responses <- list(
  pigmentation_state = list(
    response = "n_pigmented", family = "binomial", trials = "n_observations",
    eligible = rep(TRUE, nrow(cells))
  ),
  conditional_intensity = list(
    response = "conditional_intensity_median", family = "gaussian", trials = NULL,
    eligible = is.finite(cells$conditional_intensity_median)
  )
)
models <- c("null", "environment", "space", "environment_plus_space")
folds <- sort(unique(as.integer(cells$spatial_fold)))
mesh <- v16_make_mesh(cells)$mesh
score_rows <- list()
prediction_rows <- list()

for (response_name in names(responses)) {
  specification <- responses[[response_name]]
  for (fold in folds) {
    test_index_all <- which(as.integer(cells$spatial_fold) == fold)
    train_index <- which(
      as.integer(cells$spatial_fold) != fold & specification$eligible
    )
    train <- cells[train_index, , drop = FALSE]
    test_all <- cells[test_index_all, , drop = FALSE]
    score_eligible <- is.finite(test_all[[specification$response]])
    score_index <- which(score_eligible)
    if (!length(score_index)) next
    env_basis <- v16_fold_predictors(train, test_all, environment_terms)
    noenv_basis <- empty_basis(train, test_all)

    for (model_index in seq_along(models)) {
      model <- models[[model_index]]
      message("[tracking] ", response_name, " fold=", fold, " model=", model)
      basis <- if (model %in% c("environment", "environment_plus_space")) {
        env_basis
      } else noenv_basis
      sample_seed <- seed + 100000L * match(response_name, names(responses)) +
        1000L * match(fold, folds) + 10L * model_index
      sampled <- if (model %in% c("space", "environment_plus_space")) {
        fit_spde_fold(
          train, test_all, specification$response, specification$family,
          basis, mesh, specification$trials, sample_seed
        )
      } else {
        fit_fixed_fold(
          train, test_all, specification$response, specification$family,
          basis, specification$trials, sample_seed
        )
      }
      eta <- sampled$eta[score_index, , drop = FALSE]
      observed <- test_all[[specification$response]][score_index]
      trial_values <- if (!is.null(specification$trials)) {
        as.integer(test_all[[specification$trials]][score_index])
      } else NULL
      scored <- posterior_score(
        eta, observed, specification$family, trial_values, sampled$precision
      )
      if (specification$family == "binomial") {
        brier <- stats::weighted.mean(
          (observed / trial_values - scored$mean)^2, trial_values
        )
        rmse <- NA_real_
        mae <- NA_real_
      } else {
        brier <- NA_real_
        rmse <- sqrt(mean((observed - scored$mean)^2))
        mae <- mean(abs(observed - scored$mean))
      }
      score_rows[[length(score_rows) + 1L]] <- data.frame(
        response = response_name, fold = fold, model = model,
        log_predictive_density = scored$lpd,
        mean_log_predictive_density = scored$lpd / length(observed),
        n_scored_cells = length(observed), brier = brier, rmse = rmse, mae = mae,
        posterior_samples = n_samples, stringsAsFactors = FALSE
      )
      prediction_rows[[length(prediction_rows) + 1L]] <- data.frame(
        response = response_name, fold = fold, model = model,
        exact_site_id = test_all$exact_site_id[score_index],
        observed = observed, trials = if (is.null(trial_values)) NA_integer_ else trial_values,
        predicted_mean = scored$mean, point_log_predictive_density = scored$point_lpd,
        stringsAsFactors = FALSE
      )
      rm(sampled, eta, scored)
      invisible(gc())
    }
  }
}

scores <- do.call(rbind, score_rows)
predictions <- do.call(rbind, prediction_rows)
utils::write.csv(scores, file.path(output_dir, "fold_model_scores.csv"), row.names = FALSE)
utils::write.csv(predictions, file.path(output_dir, "heldout_predictions.csv"), row.names = FALSE)
summary_table <- broad_tracking_summarise(scores)
interpretation <- broad_tracking_interpretation(summary_table)
utils::write.csv(
  summary_table, file.path(output_dir, "shapley_predictive_decomposition.csv"), row.names = FALSE
)
utils::write.csv(
  interpretation, file.path(output_dir, "component_interpretation.csv"), row.names = FALSE
)

model_totals <- aggregate(
  log_predictive_density ~ response + model, data = scores, FUN = sum
)
utils::write.csv(model_totals, file.path(output_dir, "model_log_score_totals.csv"), row.names = FALSE)

metadata <- data.frame(
  field = c(
    "analysis", "source_cells", "spatial_folds", "posterior_samples",
    "seed", "environment_terms", "spde_mesh_vertices", "spde_prior_range",
    "spde_prior_sigma", "claim_boundary"
  ),
  value = c(
    "broad_spatial_inertia_vs_environmental_tracking",
    cells_path, paste(folds, collapse = ";"), n_samples, seed,
    paste(environment_terms, collapse = ";"), mesh$n,
    "P(range < 100 km)=0.05", "P(sigma > 1)=0.05",
    "non-genetic blocked predictive decomposition; not F_ST/P_ST/Q_ST"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(metadata, file.path(output_dir, "analysis_metadata.csv"), row.names = FALSE)

cat("Completed blocked Broad spatial-inertia/environment-tracking fits.\n")
print(model_totals)
print(summary_table)
print(interpretation)
