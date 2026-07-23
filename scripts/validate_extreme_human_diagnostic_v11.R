args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) {
  args[1]
} else "results/ecological_v18_extreme_human_diagnostic"

cells <- utils::read.csv(
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv",
  check.names = FALSE, stringsAsFactors = FALSE
)
checkpoint_root <- "results/ecological_v16_predictive_replication/checkpoints"
presence <- readRDS(file.path(
  checkpoint_root, "national_environment_spde_presence_draws1000.rds"
))
phenology <- readRDS(file.path(
  checkpoint_root, "national_environment_year_spde_phenology_draws1000.rds"
))
intensity <- readRDS(file.path(
  checkpoint_root, "national_environment_spde_intensity_draws1000.rds"
))
profile <- utils::read.csv(
  file.path(output_dir, "extreme_cell_tail_profiles.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
envelope <- utils::read.csv(
  file.path(output_dir, "extreme_global_envelope.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
matched_summary <- utils::read.csv(
  file.path(output_dir, "extreme_rematched_summary.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
matched_null <- utils::read.csv(
  file.path(output_dir, "extreme_rematched_null.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
pairs <- utils::read.csv(
  file.path(output_dir, "extreme_observed_matched_pairs.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)

align <- function(result) {
  index <- match(cells$exact_site_id, result$cell_id)
  if (anyNA(index)) stop("Checkpoint alignment failed.", call. = FALSE)
  result$draws[index, , drop = FALSE]
}
presence_draws <- align(presence)
phenology_draws <- align(phenology)
intensity_draws <- align(intensity)

checks <- list()
add_check <- function(check, pass, evidence) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check, status = if (isTRUE(pass)) "PASS" else "FAIL",
    evidence = evidence, stringsAsFactors = FALSE
  )
}

sample_index <- unique(c(
  1L, which.min(profile$unexpected_pigmented_q),
  which.min(profile$early_predictive_q),
  which.min(profile$dark_predictive_q)
))
for (index in sample_index) {
  colour_q <- (1 + sum(
    presence_draws[index, ] >= cells$n_pigmented[index]
  )) / (ncol(presence_draws) + 1)
  early_q <- (1 + sum(
    phenology_draws[index, ] <= cells$median_DOY[index]
  )) / (ncol(phenology_draws) + 1)
  dark_q <- if (is.finite(cells$conditional_intensity_median[index])) {
    (1 + sum(
      intensity_draws[index, ] >=
        cells$conditional_intensity_median[index]
    )) / (ncol(intensity_draws) + 1)
  } else NA_real_
  add_check(
    paste0("cell_", index, "_colour_tail"),
    abs(colour_q - profile$unexpected_pigmented_q[index]) < 1e-12,
    paste("recomputed=", colour_q,
          "recorded=", profile$unexpected_pigmented_q[index])
  )
  add_check(
    paste0("cell_", index, "_early_tail"),
    abs(early_q - profile$early_predictive_q[index]) < 1e-12,
    paste("recomputed=", early_q,
          "recorded=", profile$early_predictive_q[index])
  )
  if (is.finite(dark_q)) {
    add_check(
      paste0("cell_", index, "_dark_tail"),
      abs(dark_q - profile$dark_predictive_q[index]) < 1e-12,
      paste("recomputed=", dark_q,
            "recorded=", profile$dark_predictive_q[index])
    )
  }
}

observed_max <- max(
  profile$unexpected_pigmented_tail_depth[
    cells$n_pigmented > 0
  ],
  na.rm = TRUE
)
recorded_max <- envelope$observed_value[
  envelope$metric == "maximum_tail_depth"
]
add_check(
  "global_maximum_tail_depth",
  length(recorded_max) == 1L && abs(observed_max - recorded_max) < 1e-12,
  paste("recomputed=", observed_max, "recorded=", recorded_max)
)
observed_q05 <- sum(
  cells$n_pigmented > 0 & profile$unexpected_pigmented_q <= 0.05,
  na.rm = TRUE
)
recorded_q05 <- envelope$observed_value[
  envelope$metric == "candidate_count"
]
add_check(
  "absolute_q05_count",
  length(recorded_q05) == 1L && observed_q05 == recorded_q05,
  paste("recomputed=", observed_q05, "recorded=", recorded_q05)
)

top5_pairs <- pairs[pairs$tier == "top_0005", , drop = FALSE]
for (metric in c(
  "population_rank", "early_tail_depth", "dark_tail_depth",
  "convergence_count", "two_plus_facets"
)) {
  d <- top5_pairs[top5_pairs$metric == metric, , drop = FALSE]
  recomputed <- mean(d$case_value - d$control_value)
  recorded <- matched_summary$observed_value[
    matched_summary$tier == "top_0005" &
      matched_summary$metric == metric
  ]
  add_check(
    paste0("top5_", metric, "_contrast"),
    length(recorded) == 1L && abs(recomputed - recorded) < 1e-12,
    paste("recomputed=", recomputed, "recorded=", recorded)
  )
  null_values <- matched_null$matched_contrast[
    matched_null$tier == "top_0005" &
      matched_null$metric == metric
  ]
  recomputed_p <- (1 + sum(null_values >= recomputed, na.rm = TRUE)) /
    (sum(is.finite(null_values)) + 1)
  recorded_p <- matched_summary$empirical_p[
    matched_summary$tier == "top_0005" &
      matched_summary$metric == metric
  ]
  add_check(
    paste0("top5_", metric, "_p"),
    length(recorded_p) == 1L && abs(recomputed_p - recorded_p) < 1e-12,
    paste("recomputed=", recomputed_p, "recorded=", recorded_p)
  )
}

primary <- matched_summary[
  matched_summary$fraction == 0.05 &
    matched_summary$metric %in% c(
      "population_rank", "early_tail_depth", "dark_tail_depth",
      "convergence_count"
    ),
  , drop = FALSE
]
recomputed_q <- stats::p.adjust(primary$empirical_p, method = "BH")
add_check(
  "primary_BH_adjustment",
  max(abs(recomputed_q - primary$BH_q_primary_top5)) < 1e-12,
  paste("maximum difference=",
        max(abs(recomputed_q - primary$BH_q_primary_top5)))
)

validation <- do.call(rbind, checks)
write.csv(
  validation,
  file.path(output_dir, "extreme_independent_validation.csv"),
  row.names = FALSE
)
print(validation)
if (any(validation$status != "PASS")) {
  stop("Independent extreme diagnostic validation failed.", call. = FALSE)
}
cat("Independent extreme diagnostic validation passed.\n")

