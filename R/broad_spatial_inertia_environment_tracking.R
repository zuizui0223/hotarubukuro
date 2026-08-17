# Broad spatial inertia versus environmental tracking
#
# This analysis is inspired by the logic of F_ST/P_ST comparisons, but is
# explicitly non-genetic. It asks whether held-out flower-colour prediction is
# carried more strongly by measured environment or by residual continuous
# geography. It does not estimate F_ST, P_ST, Q_ST, heritability, or selection.

broad_tracking_log_score_binomial <- function(successes, trials, probability) {
  probability <- pmin(pmax(as.numeric(probability), 1e-8), 1 - 1e-8)
  sum(stats::dbinom(successes, size = trials, prob = probability, log = TRUE))
}

broad_tracking_log_score_gaussian <- function(observed, mean, sd) {
  sd <- pmax(as.numeric(sd), 1e-8)
  sum(stats::dnorm(observed, mean = mean, sd = sd, log = TRUE))
}

broad_tracking_shapley <- function(score_null, score_environment,
                                   score_space, score_full) {
  environment <- 0.5 * (
    (score_environment - score_null) + (score_full - score_space)
  )
  space <- 0.5 * (
    (score_space - score_null) + (score_full - score_environment)
  )
  data.frame(
    component = c("environment", "space"),
    shapley_predictive_gain = c(environment, space),
    stringsAsFactors = FALSE
  )
}

broad_tracking_summarise <- function(scores) {
  required <- c("response", "fold", "model", "log_predictive_density")
  missing <- setdiff(required, names(scores))
  if (length(missing)) stop("scores missing: ", paste(missing, collapse = ", "))
  models <- c("null", "environment", "space", "environment_plus_space")
  if (!all(models %in% unique(scores$model))) {
    stop("scores must contain null, environment, space, and environment_plus_space")
  }
  pieces <- split(scores, scores$response)
  out <- lapply(names(pieces), function(response) {
    x <- pieces[[response]]
    totals <- tapply(x$log_predictive_density, x$model, sum)
    phi <- broad_tracking_shapley(
      totals[["null"]], totals[["environment"]], totals[["space"]],
      totals[["environment_plus_space"]]
    )
    phi$response <- response
    phi$full_gain_over_null <- totals[["environment_plus_space"]] - totals[["null"]]
    phi$share_of_allocated_gain <- phi$shapley_predictive_gain /
      sum(phi$shapley_predictive_gain)
    phi[, c("response", "component", "shapley_predictive_gain",
            "share_of_allocated_gain", "full_gain_over_null")]
  })
  do.call(rbind, out)
}

broad_tracking_interpretation <- function(summary_table) {
  pieces <- split(summary_table, summary_table$response)
  do.call(rbind, lapply(names(pieces), function(response) {
    x <- pieces[[response]]
    env <- x$shapley_predictive_gain[x$component == "environment"]
    spatial <- x$shapley_predictive_gain[x$component == "space"]
    label <- if (env > spatial) {
      "environmental_tracking_dominant"
    } else if (spatial > env) {
      "spatial_inertia_dominant"
    } else {
      "balanced"
    }
    data.frame(response = response, interpretation = label,
               environment_minus_space = env - spatial,
               stringsAsFactors = FALSE)
  }))
}
