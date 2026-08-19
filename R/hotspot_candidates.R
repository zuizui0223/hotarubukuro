community_species <- function() {
  c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
}

community_environment_terms <- function(data) {
  preferred <- c(
    "env_Temperature_PC1", "env_precip_PC1",
    "env_TemperatureSeasonality", "env_PrecipSeasonality",
    "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS"
  )
  fallback <- c(
    "Temperature_PC1", "precip_PC1", "TemperatureSeasonality",
    "PrecipSeasonality", "topo_PC1", "soil_PC1", "soil_PC2", "RSDS"
  )
  terms <- intersect(preferred, names(data))
  if (length(terms) < 4L) terms <- intersect(fallback, names(data))
  terms[vapply(data[terms], function(x) all(is.finite(as.numeric(x))), logical(1))]
}

community_positive_rank <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (any(keep)) {
    out[keep] <- rank(x[keep], ties.method = "average") / sum(keep)
  }
  out
}

community_axes <- function(sites, structural_zero = TRUE) {
  species <- community_species()
  columns <- paste0("bee_", species)
  missing <- setdiff(columns, names(sites))
  if (length(missing)) {
    stop("Missing ENMeval columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  suitability <- as.matrix(sites[, columns, drop = FALSE])
  storage.mode(suitability) <- "double"
  observed <- is.finite(suitability)
  normalized <- apply(suitability, 2, community_positive_rank)
  if (is.null(dim(normalized))) normalized <- matrix(normalized, ncol = 1L)
  colnames(normalized) <- paste0(species, "_availability_rank")
  if (structural_zero) normalized[!is.finite(normalized)] <- 0

  complete <- rowSums(observed) == ncol(observed)
  analyzable <- if (structural_zero) rep(TRUE, nrow(sites)) else complete
  total <- rowSums(normalized, na.rm = FALSE)
  composition <- normalized / pmax(total, 1e-12)
  composition[!is.finite(composition)] <- NA_real_
  hellinger <- sqrt(composition)
  availability <- rowMeans(normalized, na.rm = FALSE)
  entropy <- -rowSums(ifelse(composition > 0, composition * log(composition), 0),
                      na.rm = FALSE)
  effective_richness <- exp(entropy)
  alpine_share <- rowSums(normalized[, 3:5, drop = FALSE], na.rm = FALSE) /
    pmax(total, 1e-12)

  composition_pc1 <- rep(NA_real_, nrow(sites))
  loadings <- data.frame()
  pc_rows <- analyzable & apply(hellinger, 1, function(x) all(is.finite(x)))
  if (sum(pc_rows) >= 10L) {
    pc <- stats::prcomp(hellinger[pc_rows, , drop = FALSE], center = TRUE,
                        scale. = FALSE)
    score <- pc$x[, 1]
    direction <- suppressWarnings(stats::cor(score, alpine_share[pc_rows]))
    if (is.finite(direction) && direction < 0) {
      score <- -score
      pc$rotation[, 1] <- -pc$rotation[, 1]
    }
    composition_pc1[pc_rows] <- score
    loadings <- data.frame(
      species = species,
      loading_pc1 = as.numeric(pc$rotation[, 1]),
      stringsAsFactors = FALSE
    )
  }
  metrics <- data.frame(
    bombus_coverage_n = rowSums(observed),
    bombus_common_support = complete,
    bombus_availability = availability,
    bombus_effective_richness = effective_richness,
    bombus_alpine_share = alpine_share,
    bombus_composition_pc1 = composition_pc1,
    stringsAsFactors = FALSE
  )
  list(
    metrics = metrics,
    normalized = normalized,
    composition = composition,
    hellinger = hellinger,
    loadings = loadings,
    analyzable = analyzable,
    structural_zero = structural_zero
  )
}

community_add_axes <- function(sites, structural_zero = TRUE) {
  axes <- community_axes(sites, structural_zero)
  out <- cbind(sites, axes$metrics, as.data.frame(axes$normalized))
  list(data = out, axes = axes)
}

community_gam_formula <- function(response, env_terms, n, added = "",
                                  k_space = 40L) {
  k_use <- max(10L, min(as.integer(k_space), floor(n / 20)))
  rhs <- c(env_terms, paste0("s(x_km, y_km, k = ", k_use, ")"), added)
  rhs <- rhs[nzchar(rhs)]
  stats::as.formula(paste(response, "~", paste(rhs, collapse = " + ")),
                    env = asNamespace("mgcv"))
}

community_capture_fit <- function(expr) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  list(value = value, warnings = paste(unique(warnings), collapse = " | "))
}

community_binomial_metrics <- function(data, probability) {
  probability <- pmin(pmax(as.numeric(probability), 1e-8), 1 - 1e-8)
  n <- data$n_pigmented + data$n_white
  log_loss <- -sum(
    data$n_pigmented * log(probability) + data$n_white * log(1 - probability)
  ) / sum(n)
  brier <- sum(n * (data$pigment_share - probability)^2) / sum(n)
  homogeneous <- data$site_class %in% c("white", "pigmented")
  auc <- transition_binary_auc(
    as.integer(data$site_class[homogeneous] == "pigmented"),
    probability[homogeneous]
  )
  data.frame(log_loss = log_loss, brier = brier, auc = auc)
}

community_threshold_candidates <- function(x, probabilities = seq(0.15, 0.85, 0.05)) {
  unique(as.numeric(stats::quantile(x, probabilities, na.rm = TRUE, names = FALSE)))
}

community_presence_threshold_cv <- function(sites, axes = c(
                                               "bombus_availability",
                                               "bombus_effective_richness",
                                               "bombus_alpine_share"
                                             ),
                                             support = c("nationwide_zero", "common_support"),
                                             k_space = 40L) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required.", call. = FALSE)
  }
  support <- match.arg(support)
  env_terms <- community_environment_terms(sites)
  rows <- list()
  idx <- 0L
  for (axis in axes) {
    required <- c(
      "pigment_share", "n_pigmented", "n_white", "n_observations",
      "site_class", "spatial_fold", "x_km", "y_km", env_terms, axis
    )
    d <- sites[stats::complete.cases(sites[required]), , drop = FALSE]
    if (support == "common_support") {
      d <- d[d$bombus_common_support, , drop = FALSE]
    }
    d$.axis <- as.numeric(d[[axis]])
    folds <- sort(unique(as.integer(round(d$spatial_fold))))
    for (fold in folds) {
      train <- d[d$spatial_fold != fold, , drop = FALSE]
      test <- d[d$spatial_fold == fold, , drop = FALSE]
      if (nrow(train) < 100L || nrow(test) < 20L) next
      base_formula <- community_gam_formula(
        "pigment_share", env_terms, nrow(train), k_space = k_space
      )
      linear_formula <- community_gam_formula(
        "pigment_share", env_terms, nrow(train), ".axis", k_space
      )
      smooth_formula <- community_gam_formula(
        "pigment_share", env_terms, nrow(train), "s(.axis, k = 5)", k_space
      )
      fits <- list(
        base = community_capture_fit(mgcv::gam(
          base_formula, data = train, weights = n_observations,
          family = stats::binomial(), method = "REML"
        )),
        linear = community_capture_fit(mgcv::gam(
          linear_formula, data = train, weights = n_observations,
          family = stats::binomial(), method = "REML"
        )),
        smooth = community_capture_fit(mgcv::gam(
          smooth_formula, data = train, weights = n_observations,
          family = stats::binomial(), method = "REML"
        ))
      )
      change_points <- community_threshold_candidates(train$.axis)
      threshold_fits <- lapply(change_points, function(change_point) {
        candidate <- train
        candidate$.hinge <- pmax(candidate$.axis - change_point, 0)
        formula <- community_gam_formula(
          "pigment_share", env_terms, nrow(train), ".axis + .hinge", k_space
        )
        captured <- community_capture_fit(mgcv::gam(
          formula, data = candidate, weights = n_observations,
          family = stats::binomial(), method = "REML"
        ))
        captured$change_point <- change_point
        captured
      })
      valid_threshold <- which(vapply(
        threshold_fits, function(x) inherits(x$value, "gam"), logical(1)
      ))
      if (length(valid_threshold)) {
        best <- valid_threshold[which.min(vapply(
          threshold_fits[valid_threshold], function(x) stats::AIC(x$value), numeric(1)
        ))]
        fits$threshold <- threshold_fits[[best]]
      }
      for (model in names(fits)) {
        captured <- fits[[model]]
        if (!inherits(captured$value, "gam")) next
        newdata <- test
        change_point <- if (model == "threshold") captured$change_point else NA_real_
        if (model == "threshold") {
          newdata$.hinge <- pmax(newdata$.axis - change_point, 0)
        }
        probability <- stats::predict(captured$value, newdata = newdata,
                                      type = "response")
        metrics <- community_binomial_metrics(test, probability)
        idx <- idx + 1L
        rows[[idx]] <- data.frame(
          support = support, axis = axis, heldout_spatial_fold = fold,
          model = model, n_train_cells = nrow(train), n_test_cells = nrow(test),
          n_test_observations = sum(test$n_observations),
          log_loss = metrics$log_loss, brier = metrics$brier, auc = metrics$auc,
          selected_change_point = change_point,
          model_warnings = captured$warnings,
          excludes_region_human_DOY = TRUE,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

community_presence_cv_summary <- function(cv) {
  if (!nrow(cv)) return(data.frame())
  groups <- split(cv, interaction(cv$support, cv$axis, drop = TRUE))
  rows <- lapply(groups, function(d) {
    base <- d[d$model == "base", , drop = FALSE]
    do.call(rbind, lapply(setdiff(unique(d$model), "base"), function(model) {
      added <- d[d$model == model, , drop = FALSE]
      paired <- merge(base, added, by = "heldout_spatial_fold",
                      suffixes = c("_base", "_added"))
      w <- paired$n_test_observations_base
      wm <- function(x) sum(x * w) / sum(w)
      data.frame(
        support = paired$support_base[1], axis = paired$axis_base[1], model = model,
        n_folds = nrow(paired), n_heldout_observations = sum(w),
        base_log_loss = wm(paired$log_loss_base),
        added_log_loss = wm(paired$log_loss_added),
        delta_log_loss = wm(paired$log_loss_base - paired$log_loss_added),
        delta_brier = wm(paired$brier_base - paired$brier_added),
        delta_auc = wm(paired$auc_added - paired$auc_base),
        median_change_point = if (model == "threshold") {
          stats::median(paired$selected_change_point_added, na.rm = TRUE)
        } else NA_real_,
        min_change_point = if (model == "threshold") {
          min(paired$selected_change_point_added, na.rm = TRUE)
        } else NA_real_,
        max_change_point = if (model == "threshold") {
          max(paired$selected_change_point_added, na.rm = TRUE)
        } else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
  })
  do.call(rbind, rows)
}

community_fit_presence_effects <- function(sites, axes = c(
                                             "bombus_availability",
                                             "bombus_effective_richness",
                                             "bombus_alpine_share"
                                           ),
                                           support = c("nationwide_zero",
                                                       "common_support"),
                                           k_space = 40L,
                                           grid_n = 61L) {
  support <- match.arg(support)
  env_terms <- community_environment_terms(sites)
  coefficient_rows <- curve_rows <- list()
  for (axis in axes) {
    required <- c(
      "pigment_share", "n_observations", "spatial_fold", "x_km", "y_km",
      env_terms, axis
    )
    d <- sites[stats::complete.cases(sites[required]), , drop = FALSE]
    if (support == "common_support") d <- d[d$bombus_common_support, , drop = FALSE]
    d$.axis <- as.numeric(d[[axis]])
    linear_formula <- community_gam_formula(
      "pigment_share", env_terms, nrow(d), ".axis", k_space
    )
    smooth_formula <- community_gam_formula(
      "pigment_share", env_terms, nrow(d), "s(.axis, k = 5)", k_space
    )
    linear <- mgcv::gam(
      linear_formula, data = d, weights = n_observations,
      family = stats::binomial(), method = "REML"
    )
    smooth <- mgcv::gam(
      smooth_formula, data = d, weights = n_observations,
      family = stats::binomial(), method = "REML"
    )
    tab <- summary(linear)$p.table
    coefficient_rows[[length(coefficient_rows) + 1L]] <- data.frame(
      support = support, axis = axis, model = "linear", term = "axis",
      estimate = tab[".axis", 1], se = tab[".axis", 2],
      lower_95 = tab[".axis", 1] - 1.96 * tab[".axis", 2],
      upper_95 = tab[".axis", 1] + 1.96 * tab[".axis", 2],
      p_value = tab[".axis", 4], selected_change_point = NA_real_,
      n_cells = nrow(d), n_observations = sum(d$n_observations),
      stringsAsFactors = FALSE
    )
    change_points <- community_threshold_candidates(d$.axis)
    threshold_fits <- lapply(change_points, function(change_point) {
      candidate <- d
      candidate$.hinge <- pmax(candidate$.axis - change_point, 0)
      formula <- community_gam_formula(
        "pigment_share", env_terms, nrow(d), ".axis + .hinge", k_space
      )
      fit <- mgcv::gam(
        formula, data = candidate, weights = n_observations,
        family = stats::binomial(), method = "REML"
      )
      list(fit = fit, change_point = change_point)
    })
    best <- which.min(vapply(threshold_fits, function(x) stats::AIC(x$fit), numeric(1)))
    threshold <- threshold_fits[[best]]
    threshold_tab <- summary(threshold$fit)$p.table
    for (term in intersect(c(".axis", ".hinge"), rownames(threshold_tab))) {
      coefficient_rows[[length(coefficient_rows) + 1L]] <- data.frame(
        support = support, axis = axis, model = "threshold",
        term = ifelse(term == ".axis", "slope_below", "slope_change_above"),
        estimate = threshold_tab[term, 1], se = threshold_tab[term, 2],
        lower_95 = threshold_tab[term, 1] - 1.96 * threshold_tab[term, 2],
        upper_95 = threshold_tab[term, 1] + 1.96 * threshold_tab[term, 2],
        p_value = threshold_tab[term, 4],
        selected_change_point = threshold$change_point,
        n_cells = nrow(d), n_observations = sum(d$n_observations),
        stringsAsFactors = FALSE
      )
    }
    grid <- seq(
      stats::quantile(d$.axis, 0.02), stats::quantile(d$.axis, 0.98),
      length.out = as.integer(grid_n)
    )
    average_prediction <- vapply(grid, function(value) {
      newdata <- d
      newdata$.axis <- value
      stats::weighted.mean(
        stats::predict(smooth, newdata = newdata, type = "response"),
        d$n_observations
      )
    }, numeric(1))
    curve_rows[[length(curve_rows) + 1L]] <- data.frame(
      support = support, axis = axis, axis_value = grid,
      average_predicted_pigmentation = average_prediction,
      n_cells = nrow(d), n_observations = sum(d$n_observations),
      stringsAsFactors = FALSE
    )
  }
  list(
    coefficients = do.call(rbind, coefficient_rows),
    curves = do.call(rbind, curve_rows)
  )
}

community_crossfit_background <- function(sites, k_space = 40L) {
  env_terms <- community_environment_terms(sites)
  required <- c(
    "pigment_share", "n_observations", "spatial_fold", "x_km", "y_km", env_terms
  )
  d <- sites[stats::complete.cases(sites[required]), , drop = FALSE]
  probability <- rep(NA_real_, nrow(sites))
  names(probability) <- sites$exact_site_id
  logs <- list()
  for (fold in sort(unique(as.integer(round(d$spatial_fold))))) {
    train <- d[d$spatial_fold != fold, , drop = FALSE]
    test <- d[d$spatial_fold == fold, , drop = FALSE]
    formula <- community_gam_formula(
      "pigment_share", env_terms, nrow(train), k_space = k_space
    )
    captured <- community_capture_fit(mgcv::gam(
      formula, data = train, weights = n_observations,
      family = stats::binomial(), method = "REML"
    ))
    if (!inherits(captured$value, "gam")) next
    p <- as.numeric(stats::predict(captured$value, newdata = test, type = "response"))
    probability[test$exact_site_id] <- pmin(pmax(p, 1e-8), 1 - 1e-8)
    logs[[length(logs) + 1L]] <- data.frame(
      heldout_spatial_fold = fold, n_train = nrow(train), n_test = nrow(test),
      formula = paste(deparse(formula), collapse = " "),
      warnings = captured$warnings, stringsAsFactors = FALSE
    )
  }
  list(probability = unname(probability[sites$exact_site_id]),
       log = do.call(rbind, logs), env_terms = env_terms)
}

community_environment_matrix <- function(sites) {
  terms <- community_environment_terms(sites)
  matrix <- as.matrix(sites[, terms, drop = FALSE])
  matrix <- apply(matrix, 2, transition_z)
  if (is.null(dim(matrix))) matrix <- matrix(matrix, ncol = 1L)
  colnames(matrix) <- terms
  matrix
}

community_edge_table <- function(sites, axes, background_probability,
                                 k = 5L, maximum_km = 100) {
  edges <- transition_local_edges(sites, k = k, maximum_km = maximum_km)
  site_index <- setNames(seq_len(nrow(sites)), sites$exact_site_id)
  i <- unname(site_index[edges$site_i])
  j <- unname(site_index[edges$site_j])
  env <- community_environment_matrix(sites)
  env_distance <- transition_pair_distance(env, i, j) / sqrt(ncol(env))
  edges$environmental_distance_v13 <- env_distance
  edges$bombus_availability_mean_v13 <- rowMeans(cbind(
    sites$bombus_availability[i], sites$bombus_availability[j]
  ))
  edges$bombus_availability_difference_v13 <- abs(
    sites$bombus_availability[i] - sites$bombus_availability[j]
  )
  edges$bombus_effective_richness_difference <- abs(
    sites$bombus_effective_richness[i] - sites$bombus_effective_richness[j]
  )
  edges$bombus_alpine_share_difference_v13 <- abs(
    sites$bombus_alpine_share[i] - sites$bombus_alpine_share[j]
  )
  edges$bombus_community_turnover_v13 <- transition_pair_distance(
    axes$hellinger, i, j
  ) / sqrt(2)
  edges$both_common_support <- sites$bombus_common_support[i] &
    sites$bombus_common_support[j]
  edges$elevation_difference <- abs(sites$elevation[i] - sites$elevation[j])
  if ("env_topo_PC1" %in% names(sites)) {
    edges$topography_endpoint_difference <- abs(
      sites$env_topo_PC1[i] - sites$env_topo_PC1[j]
    )
  } else if ("topo_PC1" %in% names(sites)) {
    edges$topography_endpoint_difference <- abs(sites$topo_PC1[i] - sites$topo_PC1[j])
  } else {
    edges$topography_endpoint_difference <- NA_real_
  }
  pi <- background_probability[i]
  pj <- background_probability[j]
  edges$expected_background_discordance <- pi * (1 - pj) + (1 - pi) * pj
  edges
}

community_edge_threshold_cv <- function(sites, edges, maximum_km = 25,
                                        common_support_only = FALSE) {
  site_fold <- setNames(as.integer(round(sites$spatial_fold)), sites$exact_site_id)
  d <- edges[edges$geographic_distance_km <= maximum_km, , drop = FALSE]
  if (common_support_only) d <- d[d$both_common_support, , drop = FALSE]
  d$fold_i <- unname(site_fold[d$site_i])
  d$fold_j <- unname(site_fold[d$site_j])
  base <- stats::as.formula(paste(
    "flower_discordant ~ log1p(geographic_distance_km) +",
    "environmental_distance_v13 + expected_background_discordance"
  ))
  additions <- list(
    base = character(),
    availability_level = "bombus_availability_mean_v13",
    availability_change = "bombus_availability_difference_v13",
    effective_richness_change = "bombus_effective_richness_difference",
    community_turnover = "bombus_community_turnover_v13",
    alpine_composition_change = "bombus_alpine_share_difference_v13",
    terrain_endpoint_proxy = c("elevation_difference", "topography_endpoint_difference")
  )
  rows <- list()
  z <- 0L
  folds <- sort(unique(c(d$fold_i, d$fold_j)))
  for (fold in folds[is.finite(folds)]) {
    train <- d[d$fold_i != fold & d$fold_j != fold, , drop = FALSE]
    test <- d[d$fold_i == fold & d$fold_j == fold, , drop = FALSE]
    if (nrow(train) < 50L || nrow(test) < 10L ||
        length(unique(train$flower_discordant)) < 2L) next
    for (model in names(additions)) {
      terms <- additions[[model]]
      formula <- if (length(terms)) {
        stats::update.formula(base, paste(". ~ . +", paste(terms, collapse = " + ")))
      } else base
      required <- all(c(all.vars(formula), "flower_discordant") %in% names(train))
      if (!required) next
      fit <- tryCatch(
        stats::glm(formula, data = train, family = stats::binomial()),
        error = function(e) NULL
      )
      if (is.null(fit)) next
      p <- as.numeric(stats::predict(fit, newdata = test, type = "response"))
      p <- pmin(pmax(p, 1e-8), 1 - 1e-8)
      y <- test$flower_discordant
      z <- z + 1L
      rows[[z]] <- data.frame(
        support = if (common_support_only) "common_support" else "nationwide_zero",
        heldout_spatial_fold = fold, model = model,
        n_train_edges = nrow(train), n_test_edges = nrow(test),
        log_loss = -mean(y * log(p) + (1 - y) * log(1 - p)),
        brier = mean((y - p)^2), auc = transition_binary_auc(y, p),
        strict_node_disjoint_split = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

community_edge_cv_summary <- function(cv) {
  if (!nrow(cv)) return(data.frame())
  groups <- split(cv, cv$support)
  do.call(rbind, lapply(groups, function(d) {
    base <- d[d$model == "base", , drop = FALSE]
    do.call(rbind, lapply(setdiff(unique(d$model), "base"), function(model) {
      added <- d[d$model == model, , drop = FALSE]
      paired <- merge(base, added, by = "heldout_spatial_fold",
                      suffixes = c("_base", "_added"))
      w <- paired$n_test_edges_base
      wm <- function(x) sum(x * w) / sum(w)
      data.frame(
        support = paired$support_base[1], model = model, n_folds = nrow(paired),
        n_heldout_edges = sum(w), base_log_loss = wm(paired$log_loss_base),
        added_log_loss = wm(paired$log_loss_added),
        delta_log_loss = wm(paired$log_loss_base - paired$log_loss_added),
        delta_brier = wm(paired$brier_base - paired$brier_added),
        delta_auc = wm(paired$auc_added - paired$auc_base),
        stringsAsFactors = FALSE
      )
    }))
  }))
}

community_crossfit_gaussian <- function(sites, response,
                                        weight = "n_observations",
                                        extra_terms = character(),
                                        k_space = 40L) {
  env_terms <- community_environment_terms(sites)
  required <- c(response, weight, "spatial_fold", "x_km", "y_km",
                env_terms, extra_terms)
  d <- sites[stats::complete.cases(sites[required]), , drop = FALSE]
  prediction <- rep(NA_real_, nrow(sites))
  logs <- list()
  for (fold in sort(unique(as.integer(round(d$spatial_fold))))) {
    train <- d[d$spatial_fold != fold, , drop = FALSE]
    test <- d[d$spatial_fold == fold, , drop = FALSE]
    train$.model_weight <- as.numeric(train[[weight]])
    formula <- community_gam_formula(
      response, c(env_terms, extra_terms), nrow(train), k_space = k_space
    )
    captured <- community_capture_fit(mgcv::gam(
      formula, data = train, weights = .model_weight,
      family = stats::gaussian(), method = "REML"
    ))
    if (!inherits(captured$value, "gam")) next
    prediction[match(test$exact_site_id, sites$exact_site_id)] <-
      as.numeric(stats::predict(captured$value, newdata = test, type = "response"))
    logs[[length(logs) + 1L]] <- data.frame(
      outcome = response, heldout_spatial_fold = fold,
      n_train = nrow(train), n_test = nrow(test),
      formula = paste(deparse(formula), collapse = " "),
      warnings = captured$warnings, stringsAsFactors = FALSE
    )
  }
  list(prediction = prediction, residual = as.numeric(sites[[response]]) - prediction,
       log = do.call(rbind, logs))
}

community_candidate_table <- function(sites, neighbourhoods, background_probability,
                                      intensity_residual, phenology_residual,
                                      radius_km = 25) {
  local <- neighbourhoods[neighbourhoods$radius_km == radius_km, , drop = FALSE]
  index <- match(sites$exact_site_id, local$exact_site_id)
  out <- sites
  out$neighbour_pigment_share <- local$neighbour_pigment_share[index]
  out$n_neighbours <- local$n_neighbours[index]
  out$natural_presence_probability_v13 <- background_probability
  out$intensity_natural_residual_v13 <- intensity_residual
  out$early_natural_score_v13 <- -phenology_residual
  out$local_pigmented_surprise <- ifelse(
    out$site_class == "pigmented", 1 - out$neighbour_pigment_share, NA_real_
  )
  out$natural_pigmented_surprise <- ifelse(
    out$site_class == "pigmented", 1 - out$natural_presence_probability_v13, NA_real_
  )
  out$anomaly_strength <- sqrt(pmax(out$local_pigmented_surprise, 0) *
                                 pmax(out$natural_pigmented_surprise, 0))
  out$descriptive_longitude_band <- ifelse(out$longitude < 136.5, "West", "East")
  out$horticultural_candidate <- out$site_class == "pigmented" &
    out$n_neighbours >= 5L & out$neighbour_pigment_share <= 0.1
  candidates <- out[out$horticultural_candidate, , drop = FALSE]
  candidates <- candidates[order(-candidates$anomaly_strength), , drop = FALSE]
  candidates$candidate_rank <- seq_len(nrow(candidates))
  candidates
}

community_match_horticultural_controls <- function(sites, candidates,
                                                    background_probability,
                                                    intensity_residual,
                                                    phenology_residual,
                                                    maximum_km = 150,
                                                    environment_caliper = 1.25) {
  if (!nrow(candidates)) return(data.frame())
  sites$natural_presence_probability_v13 <- background_probability
  sites$intensity_natural_residual_v13 <- intensity_residual
  sites$early_natural_score_v13 <- -phenology_residual
  candidate_index <- match(candidates$exact_site_id, sites$exact_site_id)
  eligible <- which(
    sites$site_class == "pigmented" &
      !(sites$exact_site_id %in% candidates$exact_site_id)
  )
  geo <- transition_distance_matrix(sites)
  env <- community_environment_matrix(sites)
  env_dist <- as.matrix(stats::dist(env)) / sqrt(ncol(env))
  used <- rep(FALSE, nrow(sites))
  pairs <- list()
  for (row in seq_len(nrow(candidates))) {
    i <- candidate_index[row]
    possible <- eligible[!used[eligible] & geo[i, eligible] <= maximum_km &
                           env_dist[i, eligible] <= environment_caliper]
    if (!length(possible)) next
    score <- geo[i, possible] / maximum_km +
      env_dist[i, possible] / environment_caliper +
      abs(sites$natural_presence_probability_v13[i] -
            sites$natural_presence_probability_v13[possible])
    j <- possible[which.min(score)]
    used[j] <- TRUE
    facet_columns <- intersect(c(
      "z_H", "z_R", "z_A", "intensity_natural_residual_v13",
      "early_natural_score_v13", "n_exact_sites", "n_years"
    ), names(sites))
    pair <- data.frame(
      candidate_site = sites$exact_site_id[i], control_site = sites$exact_site_id[j],
      candidate_rank = candidates$candidate_rank[row],
      anomaly_strength = candidates$anomaly_strength[row],
      geographic_distance_km = geo[i, j], environmental_distance = env_dist[i, j],
      candidate_block = paste(floor(sites$x_km[i] / 100),
                              floor(sites$y_km[i] / 100), sep = "_"),
      stringsAsFactors = FALSE
    )
    for (column in facet_columns) {
      pair[[paste0("delta_", column)]] <- sites[[column]][i] - sites[[column]][j]
    }
    pairs[[length(pairs) + 1L]] <- pair
  }
  if (length(pairs)) do.call(rbind, pairs) else data.frame()
}

community_cluster_bootstrap_mean <- function(values, clusters, repetitions = 2000L,
                                             seed = 491L) {
  keep <- is.finite(values) & !is.na(clusters)
  values <- values[keep]
  clusters <- as.character(clusters[keep])
  if (!length(values)) return(c(mean = NA, lower = NA, upper = NA, p = NA))
  groups <- split(values, clusters)
  set.seed(seed)
  boot <- replicate(repetitions, {
    sampled <- sample(names(groups), length(groups), replace = TRUE)
    mean(unlist(groups[sampled], use.names = FALSE))
  })
  p <- min(1, 2 * min(mean(boot <= 0), mean(boot >= 0)))
  c(
    mean = mean(values),
    lower = unname(stats::quantile(boot, 0.025)),
    upper = unname(stats::quantile(boot, 0.975)),
    p = p
  )
}

community_facet_summary <- function(pairs, repetitions = 2000L) {
  if (!nrow(pairs)) return(data.frame())
  columns <- grep("^delta_", names(pairs), value = TRUE)
  rows <- lapply(seq_along(columns), function(i) {
    column <- columns[i]
    estimate <- community_cluster_bootstrap_mean(
      pairs[[column]], pairs$candidate_block, repetitions, 491L + i
    )
    data.frame(
      facet = sub("^delta_", "", column), n_pairs = sum(is.finite(pairs[[column]])),
      n_spatial_blocks = length(unique(pairs$candidate_block[is.finite(pairs[[column]])])),
      mean_candidate_minus_control = estimate["mean"],
      lower_95 = estimate["lower"], upper_95 = estimate["upper"],
      bootstrap_p = estimate["p"], stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$BH_q <- stats::p.adjust(out$bootstrap_p, method = "BH")
  out$simultaneous_requirement <- FALSE
  out
}

community_cluster_bootstrap_slope <- function(x, y, clusters,
                                              repetitions = 2000L,
                                              seed = 491L) {
  keep <- is.finite(x) & is.finite(y) & !is.na(clusters)
  d <- data.frame(x = as.numeric(x[keep]), y = as.numeric(y[keep]),
                  cluster = as.character(clusters[keep]))
  if (nrow(d) < 6L || length(unique(d$x)) < 3L) {
    return(c(slope = NA, lower = NA, upper = NA, p = NA))
  }
  d$x <- as.numeric(scale(d$x))
  groups <- split(d, d$cluster)
  slope <- unname(stats::coef(stats::lm(y ~ x, data = d))["x"])
  set.seed(seed)
  boot <- replicate(repetitions, {
    sampled <- sample(names(groups), length(groups), replace = TRUE)
    block <- do.call(rbind, unname(groups[sampled]))
    if (length(unique(block$x)) < 2L) return(NA_real_)
    unname(stats::coef(stats::lm(y ~ x, data = block))["x"])
  })
  boot <- boot[is.finite(boot)]
  if (!length(boot)) return(c(slope = slope, lower = NA, upper = NA, p = NA))
  p <- min(1, 2 * min(mean(boot <= 0), mean(boot >= 0)))
  c(
    slope = slope,
    lower = unname(stats::quantile(boot, 0.025)),
    upper = unname(stats::quantile(boot, 0.975)),
    p = p
  )
}

community_facet_rank_trends <- function(pairs, repetitions = 2000L) {
  if (!nrow(pairs)) return(data.frame())
  columns <- grep("^delta_", names(pairs), value = TRUE)
  rows <- lapply(seq_along(columns), function(i) {
    column <- columns[i]
    estimate <- community_cluster_bootstrap_slope(
      pairs$anomaly_strength, pairs[[column]], pairs$candidate_block,
      repetitions, 1491L + i
    )
    data.frame(
      facet = sub("^delta_", "", column),
      n_pairs = sum(is.finite(pairs[[column]]) & is.finite(pairs$anomaly_strength)),
      slope_per_1sd_anomaly = estimate["slope"],
      lower_95 = estimate["lower"], upper_95 = estimate["upper"],
      bootstrap_p = estimate["p"], stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$BH_q <- stats::p.adjust(out$bootstrap_p, method = "BH")
  out$rank_defined_without_facet <- TRUE
  out
}

community_horticulture_matching_sensitivity <- function(
    sites, candidates, background_probability, intensity_residual,
    phenology_residual, maximum_distances_km = c(75, 150, 250),
    environment_calipers = c(0.75, 1.25), repetitions = 1000L) {
  rows <- list()
  for (maximum_km in maximum_distances_km) {
    for (caliper in environment_calipers) {
      pairs <- community_match_horticultural_controls(
        sites, candidates, background_probability, intensity_residual,
        phenology_residual, maximum_km, caliper
      )
      summary <- community_facet_summary(pairs, repetitions)
      trends <- community_facet_rank_trends(pairs, repetitions)
      if (nrow(summary)) {
        summary$analysis <- "candidate_minus_control"
        summary$maximum_km <- maximum_km
        summary$environment_caliper <- caliper
        rows[[length(rows) + 1L]] <- summary
      }
      if (nrow(trends)) {
        names(trends)[names(trends) == "slope_per_1sd_anomaly"] <-
          "mean_candidate_minus_control"
        trends$n_spatial_blocks <- NA_integer_
        trends$simultaneous_requirement <- FALSE
        trends$analysis <- "rank_slope_per_1sd_anomaly"
        trends$maximum_km <- maximum_km
        trends$environment_caliper <- caliper
        trends$rank_defined_without_facet <- NULL
        rows[[length(rows) + 1L]] <- trends[, names(summary), drop = FALSE]
      }
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

community_fit_spde_foundation <- function(data, env_terms) {
  if (!requireNamespace("INLA", quietly = TRUE)) {
    return(list(metrics = data.frame(), fixed = data.frame(), hyper = data.frame(),
                status = "INLA unavailable"))
  }
  outcomes <- list(
    presence = list(response = "pigmented_mixture50", family = "binomial",
                    subset = rep(TRUE, nrow(data))),
    intensity = list(response = "pigment_intensity_z", family = "gaussian",
                     subset = is.finite(data$pigment_intensity_z))
  )
  all_fits <- list()
  for (outcome in names(outcomes)) {
    spec <- outcomes[[outcome]]
    required <- c(spec$response, env_terms, "x_km", "y_km")
    d <- data[spec$subset & stats::complete.cases(data[required]), , drop = FALSE]
    coords <- as.matrix(d[c("x_km", "y_km")])
    mesh <- INLA::inla.mesh.2d(loc = coords, max.edge = c(20, 100), cutoff = 5)
    spde <- INLA::inla.spde2.pcmatern(
      mesh, alpha = 2, prior.range = c(100, 0.05), prior.sigma = c(1, 0.05)
    )
    term_sets <- list(space_only = character(), environment_space = env_terms)
    for (name in names(term_sets)) {
      model_name <- paste(outcome, name, sep = "_")
      message("[v13 SPDE] fitting ", model_name)
      fit <- fit_one_inla_spde(
        d, spec$response, term_sets[[name]], model_name, mesh, spde, spec$family
      )
      fit$metrics$comparison_set <- paste0(outcome, "_foundation")
      all_fits[[model_name]] <- fit
    }
  }
  metrics <- do.call(rbind, lapply(all_fits, `[[`, "metrics"))
  metrics$delta_WAIC_within_set <- ave(
    metrics$WAIC, metrics$comparison_set, FUN = function(x) x - min(x)
  )
  list(
    metrics = metrics,
    fixed = do.call(rbind, lapply(all_fits, `[[`, "fixed")),
    hyper = do.call(rbind, lapply(all_fits, `[[`, "hyper")),
    status = "completed"
  )
}
