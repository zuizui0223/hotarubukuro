args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[1] else
  "results/ecological_v16_predictive_replication"

read_csv <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing result: ", path, call. = FALSE)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

manifest <- read_csv("predictive_replication_draw_manifest.csv")
scores <- read_csv("predictive_replication_cell_candidate_scores.csv")
performance <- read_csv("predictive_replication_model_performance.csv")

rows <- list()
add <- function(model, check, status, evidence) {
  rows[[length(rows) + 1L]] <<- data.frame(
    model = model, check = check, status = status, evidence = evidence,
    stringsAsFactors = FALSE
  )
}

for (index in seq_len(nrow(manifest))) {
  model <- manifest$model[index]
  path <- file.path(output_dir, manifest$checkpoint[index])
  if (!file.exists(path)) {
    add(model, "checkpoint_exists", "FAIL", path)
    next
  }
  result <- readRDS(path)
  dimensions_ok <- nrow(result$draws) == manifest$n_cells[index] &&
    ncol(result$draws) == manifest$n_draws[index] &&
    length(result$cell_id) == nrow(result$draws)
  add(
    model, "checkpoint_dimensions",
    if (dimensions_ok) "PASS" else "FAIL",
    paste0(nrow(result$draws), "x", ncol(result$draws),
           "; ids=", length(result$cell_id))
  )
  add(
    model, "finite_predictive_draws",
    if (all(is.finite(result$draws))) "PASS" else "FAIL",
    paste0("non-finite=", sum(!is.finite(result$draws)))
  )
  projection <- result$log$posterior_vs_INLA_fitted_correlation
  linear_projection <- result$log$posterior_eta_vs_INLA_linear_correlation
  projection_ok <- length(projection) == 5L &&
    all(projection >= 0.98) && all(linear_projection >= 0.98)
  add(
    model, "APredictor_projection",
    if (projection_ok) "PASS" else "FAIL",
    paste0(
      "min response correlation=", round(min(projection), 5),
      "; min eta correlation=", round(min(linear_projection), 5)
    )
  )

  if (grepl("presence$", model)) {
    reported <- scores[scores$model == model, , drop = FALSE]
    order <- match(result$cell_id, reported$exact_site_id)
    score_ok <- nrow(reported) == nrow(result$draws) && !anyNA(order)
    if (score_ok) {
      reported <- reported[order, , drop = FALSE]
      upper <- (1 + rowSums(result$draws >= reported$n_pigmented)) /
        (ncol(result$draws) + 1)
      lower <- (1 + rowSums(result$draws <= reported$n_pigmented)) /
        (ncol(result$draws) + 1)
      predicted <- rowMeans(result$draws / reported$n_observations)
      maximum_difference <- max(
        abs(upper - reported$unexpected_pigmented_q),
        abs(lower - reported$unexpected_white_q),
        abs(predicted - reported$predicted_pigment_share)
      )
      score_ok <- maximum_difference <= 1e-12
    } else maximum_difference <- Inf
    add(
      model, "candidate_score_recalculation",
      if (score_ok) "PASS" else "FAIL",
      paste0("maximum absolute difference=", maximum_difference)
    )
    observed_prevalence <- sum(reported$n_pigmented) /
      sum(reported$n_observations)
    predicted_prevalence <- stats::weighted.mean(
      reported$predicted_pigment_share, reported$n_observations
    )
    recorded <- performance[performance$model == model, , drop = FALSE]
    prevalence_ok <- nrow(recorded) == 1L &&
      abs(recorded$observed_prevalence - observed_prevalence) <= 1e-12 &&
      abs(recorded$predicted_prevalence - predicted_prevalence) <= 1e-12
    add(
      model, "prevalence_recalculation",
      if (prevalence_ok) "PASS" else "FAIL",
      paste0(
        "observed=", round(observed_prevalence, 6),
        "; predicted=", round(predicted_prevalence, 6)
      )
    )
  }
}

validation <- do.call(rbind, rows)
utils::write.csv(
  validation,
  file.path(output_dir, "predictive_replication_independent_validation.csv"),
  row.names = FALSE
)
if (any(validation$status == "FAIL")) {
  print(validation[validation$status == "FAIL", ], row.names = FALSE)
  stop("Independent predictive-replication validation failed.", call. = FALSE)
}
cat("Independent predictive-replication validation passed.\n")
