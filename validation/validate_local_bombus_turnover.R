args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) {
  args[1]
} else "results/ecological_v17_local_pair_turnover"

cells <- utils::read.csv(
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv",
  check.names = FALSE, stringsAsFactors = FALSE
)
presence_raw <- readRDS(paste0(
  "results/ecological_v16_predictive_replication/checkpoints/",
  "national_environment_spde_presence_draws1000.rds"
))
intensity_raw <- readRDS(paste0(
  "results/ecological_v16_predictive_replication/checkpoints/",
  "national_environment_spde_intensity_draws1000.rds"
))
edges <- utils::read.csv(
  file.path(output_dir, "local_pair_edges.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
summary_table <- utils::read.csv(
  file.path(output_dir, "local_pair_predictive_summary.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
null <- utils::read.csv(
  file.path(output_dir, "local_pair_predictive_null.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)

scale_independent <- function(x) {
  (x - mean(x)) / stats::sd(x)
}

fit_independent <- function(y, baseline, predictor) {
  keep <- is.finite(y) & is.finite(predictor) &
    stats::complete.cases(baseline)
  d <- data.frame(
    y = scale_independent(y[keep]),
    predictor = scale_independent(predictor[keep])
  )
  base_scaled <- as.data.frame(lapply(
    baseline[keep, , drop = FALSE], scale_independent
  ))
  names(base_scaled) <- paste0("base_", seq_len(ncol(base_scaled)))
  d <- cbind(d, base_scaled)
  formula <- stats::as.formula(paste(
    "y ~", paste(c(names(base_scaled), "predictor"), collapse = " + ")
  ))
  full <- stats::lm(formula, data = d)
  base <- stats::lm(stats::update(formula, . ~ . - predictor), data = d)
  c(
    beta = unname(stats::coef(full)[["predictor"]]),
    delta_r2 = summary(full)$r.squared - summary(base)$r.squared
  )
}

align_draws <- function(result) {
  index <- match(cells$exact_site_id, result$cell_id)
  if (anyNA(index)) stop("Checkpoint alignment failed.", call. = FALSE)
  list(
    draws = result$draws[index, , drop = FALSE],
    latent_mean = result$latent_mean[index]
  )
}

presence <- align_draws(presence_raw)
intensity <- align_draws(intensity_raw)
checks <- list()
add_check <- function(check, pass, evidence) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check, status = if (isTRUE(pass)) "PASS" else "FAIL",
    evidence = evidence, stringsAsFactors = FALSE
  )
}

for (response in c("presence", "intensity")) {
  d <- edges[edges$radius_km == 25, , drop = FALSE]
  if (response == "presence") {
    observed <- d$observed_presence_transition
    baseline <- d[, c(
      "geographic_distance_km", "environmental_distance",
      "natural_presence_difference", "natural_presence_uncertainty",
      "log_presence_pair_effort"
    ), drop = FALSE]
    checkpoint <- presence
    values <- sweep(
      checkpoint$draws, 1, pmax(cells$n_observations, 1), "/"
    )
  } else {
    d <- d[d$conditional_intensity_pair, , drop = FALSE]
    observed <- d$observed_intensity_transition
    baseline <- d[, c(
      "geographic_distance_km", "environmental_distance",
      "natural_intensity_difference", "log_intensity_pair_effort"
    ), drop = FALSE]
    checkpoint <- intensity
    values <- checkpoint$draws
  }
  baseline$geographic_distance_km <-
    log1p(baseline$geographic_distance_km)
  observed_stat <- fit_independent(
    observed, baseline, d$fingerprint_turnover
  )
  recorded <- summary_table[
    summary_table$response == response &
      summary_table$radius_km == 25 &
      summary_table$predictor == "fingerprint_turnover",
    , drop = FALSE
  ]
  add_check(
    paste0(response, "_observed_beta"),
    nrow(recorded) == 1L &&
      abs(observed_stat[["beta"]] -
            recorded$observed_partial_beta) < 1e-10,
    paste("recomputed=", observed_stat[["beta"]],
          "recorded=", recorded$observed_partial_beta)
  )
  add_check(
    paste0(response, "_observed_delta_r2"),
    nrow(recorded) == 1L &&
      abs(observed_stat[["delta_r2"]] -
            recorded$observed_delta_r2) < 1e-10,
    paste("recomputed=", observed_stat[["delta_r2"]],
          "recorded=", recorded$observed_delta_r2)
  )
  for (draw in c(1L, 500L, 1000L)) {
    simulated <- abs(values[d$i, draw] - values[d$j, draw])
    draw_stat <- fit_independent(
      simulated, baseline, d$fingerprint_turnover
    )
    recorded_draw <- null[
      null$response == response &
        null$radius_km == 25 &
        null$predictor == "fingerprint_turnover" &
        null$draw == draw,
      , drop = FALSE
    ]
    add_check(
      paste0(response, "_draw_", draw, "_beta"),
      nrow(recorded_draw) == 1L &&
        abs(draw_stat[["beta"]] - recorded_draw$partial_beta) < 1e-10,
      paste("recomputed=", draw_stat[["beta"]],
            "recorded=", recorded_draw$partial_beta)
    )
  }
}

primary <- summary_table[
  summary_table$predictor == "fingerprint_turnover" &
    summary_table$radius_km == 25,
  , drop = FALSE
]
recomputed_q <- stats::p.adjust(primary$beta_empirical_p, method = "BH")
add_check(
  "primary_BH_adjustment",
  max(abs(recomputed_q - primary$BH_q_primary_25km)) < 1e-12,
  paste("maximum difference=",
        max(abs(recomputed_q - primary$BH_q_primary_25km)))
)
add_check(
  "sharp_transition_support",
  sum(
    edges$radius_km == 25 &
      edges$observed_presence_transition >= 0.8
  ) >= 100L,
  paste(
    "25-km edges with observed presence transition >=0.8=",
    sum(
      edges$radius_km == 25 &
        edges$observed_presence_transition >= 0.8
    )
  )
)

validation <- do.call(rbind, checks)
write.csv(
  validation,
  file.path(output_dir, "local_pair_independent_validation.csv"),
  row.names = FALSE
)
print(validation)
if (any(validation$status != "PASS")) {
  stop("Independent local-pair validation failed.", call. = FALSE)
}
cat("Independent local-pair validation passed.\n")
