args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) {
  args[[1L]]
} else {
  "results/ecological_v21_local_human_neighbourhood"
}

source("R/pipeline_support.R")
hb_load_modules("local_human_context")

read_output <- function(name) {
  utils::read.csv(
    file.path(output_dir, name), check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
close_enough <- function(x, y, tolerance = 1e-9) {
  length(x) == length(y) &&
    all((is.na(x) & is.na(y)) |
          (is.finite(x) & is.finite(y) & abs(x - y) <= tolerance))
}
finite_max <- function(value) {
  value <- value[is.finite(value)]
  if (length(value)) max(value) else NA_real_
}
checks <- list()
add_check <- function(check, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check,
    status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = detail,
    stringsAsFactors = FALSE
  )
}

class_cells <- read_output("mlit_landuse_class_cells_1km.csv")
join_audit <- read_output("human_neighbourhood_landuse_join_audit.csv")
features <- read_output("human_neighbourhood_cell_features.csv")
definitions <- read_output("human_neighbourhood_feature_definitions.csv")
summary <- read_output("human_neighbourhood_contrast_summary.csv")
null <- read_output("human_neighbourhood_contrast_null.csv")
global <- read_output("human_neighbourhood_global_summary.csv")
support <- read_output("human_neighbourhood_configuration_support.csv")
details <- read_output("human_neighbourhood_observed_details.csv")
quality_summary <- read_output("human_neighbourhood_quality_summary.csv")
quality_null <- read_output("human_neighbourhood_quality_null.csv")
convergence_summary <- read_output(
  "human_neighbourhood_convergence_summary.csv"
)
convergence_null <- read_output(
  "human_neighbourhood_convergence_null.csv"
)
followup <- read_output("human_neighbourhood_followup_candidates.csv")
population_summary <- read_output(
  "human_neighbourhood_population_scale_summary.csv"
)
population_null <- read_output(
  "human_neighbourhood_population_scale_null.csv"
)
population_context <- read_output(
  "human_neighbourhood_worldpop_multiscale.csv"
)
population_provenance <- read_output(
  "human_neighbourhood_worldpop_provenance.csv"
)
metadata <- read_output("human_neighbourhood_metadata.csv")
metadata_value <- setNames(metadata$value, metadata$field)

registry <- v21_landuse_registry()
class_sum <- rowSums(class_cells[, c(
  registry$feature, "other_land_fraction"
), drop = FALSE])
add_check(
  "mlit_class_cell_grain",
  nrow(class_cells) == 309600L &&
    !anyDuplicated(class_cells$mesh_1km),
  paste("rows=", nrow(class_cells))
)
add_check(
  "mlit_class_fractions",
  all(class_cells[, registry$feature] >= 0) &&
    all(class_cells[, registry$feature] <= 1) &&
    all(class_sum <= class_cells$represented_fraction + 1e-12) &&
    all(class_sum >= class_cells$represented_fraction - 1e-12),
  paste(
    "sum-minus-represented range=",
    paste(
      range(class_sum - class_cells$represented_fraction),
      collapse = " to "
    )
  )
)
join_value <- setNames(join_audit$value, join_audit$metric)
add_check(
  "landuse_join_coverage",
  join_value[["n_observations_linked"]] ==
    join_value[["n_observations"]] &&
    join_value[["n_cells_complete_classes"]] ==
    join_value[["n_cells"]],
  paste(
    "observations=", join_value[["n_observations_linked"]],
    "cells=", join_value[["n_cells_complete_classes"]]
  )
)
add_check(
  "analysis_cell_grain",
  nrow(features) == 1307L && !anyDuplicated(features$exact_site_id),
  paste("rows=", nrow(features))
)
population_log_columns <- grep(
  "^log_population_sum_", names(population_context), value = TRUE
)
population_correlation <- stats::cor(
  population_context[, population_log_columns, drop = FALSE],
  method = "spearman"
)
off_diagonal_population_correlation <-
  population_correlation[upper.tri(population_correlation)]
add_check(
  "worldpop_true_multiscale_separation",
  nrow(population_context) == 1307L &&
    max(off_diagonal_population_correlation) < 0.99 &&
    min(off_diagonal_population_correlation) < 0.8,
  paste(
    "Spearman range=",
    paste(
      round(range(off_diagonal_population_correlation), 3),
      collapse = " to "
    )
  )
)
add_check(
  "worldpop_provenance_hash",
  file.exists(population_provenance$source[1L]) &&
    identical(
      unname(tools::md5sum(population_provenance$source[1L])),
      population_provenance$md5[1L]
    ),
  paste("md5=", population_provenance$md5[1L])
)
add_check(
  "configuration_completion",
  nrow(support) == 7L &&
    all(support$n_natural_maps == 1000L) &&
    all(table(null$configuration) == 1000L),
  paste("configurations=", nrow(support), "null rows=", nrow(null))
)
primary <- "primary_10km_env1_all_white"
primary_details <- details[
  details$configuration == primary, , drop = FALSE
]
primary_summary <- summary[
  summary$configuration == primary, , drop = FALSE
]
add_check(
  "primary_local_coverage",
  nrow(primary_details) == 16L &&
    !anyDuplicated(primary_details$exact_site_id) &&
    all(primary_details$n_white_neighbours >= 3L),
  paste("focal cells=", nrow(primary_details))
)

recompute_summary <- function(summary_block, null_block, details_block = NULL) {
  observed <- numeric(nrow(summary_block))
  p <- numeric(nrow(summary_block))
  two_sided <- numeric(nrow(summary_block))
  for (index in seq_len(nrow(summary_block))) {
    feature <- summary_block$feature[index]
    observed[index] <- if (is.null(details_block)) {
      summary_block$observed_focal_minus_white_neighbour[index]
    } else {
      mean(details_block[[feature]], na.rm = TRUE)
    }
    simulated <- null_block[[feature]]
    simulated <- simulated[is.finite(simulated)]
    upper <- (1 + sum(simulated >= observed[index])) /
      (length(simulated) + 1)
    lower <- (1 + sum(simulated <= observed[index])) /
      (length(simulated) + 1)
    direction <- summary_block$hypothesis_direction[index]
    p[index] <- if (direction == "less") {
      lower
    } else if (direction == "two_sided") {
      min(1, 2 * min(upper, lower))
    } else {
      upper
    }
    two_sided[index] <- min(1, 2 * min(upper, lower))
  }
  simulated <- as.matrix(
    null_block[, summary_block$feature, drop = FALSE]
  )
  center <- colMeans(simulated, na.rm = TRUE)
  spread <- apply(simulated, 2, stats::sd, na.rm = TRUE)
  spread[!is.finite(spread) | spread <= 1e-12] <- 1
  null_z <- sweep(sweep(simulated, 2, center, "-"), 2, spread, "/")
  observed_z <- (observed - center) / spread
  sign <- ifelse(summary_block$hypothesis_direction == "less", -1, 1)
  maximum_directional <- apply(
    sweep(null_z, 2, sign, "*"), 1, finite_max
  )
  maximum_absolute <- apply(abs(null_z), 1, finite_max)
  fwer <- vapply(seq_len(nrow(summary_block)), function(index) {
    if (summary_block$hypothesis_direction[index] == "two_sided") {
      (1 + sum(
        maximum_absolute >= abs(observed_z[index]), na.rm = TRUE
      )) / (sum(is.finite(maximum_absolute)) + 1)
    } else {
      (1 + sum(
        maximum_directional >= observed_z[index] * sign[index],
        na.rm = TRUE
      )) / (sum(is.finite(maximum_directional)) + 1)
    }
  }, numeric(1))
  list(observed = observed, p = p, two_sided = two_sided, fwer = fwer)
}

primary_null <- null[null$configuration == primary, , drop = FALSE]
primary_recomputed <- recompute_summary(
  primary_summary, primary_null, primary_details
)
add_check(
  "primary_contrast_statistics",
  close_enough(
    primary_recomputed$observed,
    primary_summary$observed_focal_minus_white_neighbour
  ) &&
    close_enough(
      primary_recomputed$p,
      primary_summary$directional_or_two_sided_p
    ) &&
    close_enough(
      primary_recomputed$two_sided,
      primary_summary$two_sided_p
    ),
  paste("features=", nrow(primary_summary))
)
add_check(
  "primary_maxT_statistics",
  close_enough(primary_recomputed$fwer, primary_summary$maxT_FWER_p),
  paste("features=", nrow(primary_summary))
)

core <- v21_core_features()
simulated <- as.matrix(primary_null[, core, drop = FALSE])
simulated <- simulated[stats::complete.cases(simulated), , drop = FALSE]
center <- colMeans(simulated)
covariance <- stats::cov(simulated)
eigenvalue <- eigen(
  covariance, symmetric = TRUE, only.values = TRUE
)$values
ridge <- max(max(eigenvalue) * 1e-6, 1e-8)
inverse <- solve(covariance + diag(ridge, ncol(covariance)))
simulated_centered <- sweep(simulated, 2, center, "-")
simulated_distance <- rowSums(
  (simulated_centered %*% inverse) * simulated_centered
)
observed <- primary_summary[
  match(core, primary_summary$feature),
  "observed_focal_minus_white_neighbour"
]
observed_centered <- observed - center
observed_distance <- as.numeric(
  t(observed_centered) %*% inverse %*% observed_centered
)
observed_p <- (
  1 + sum(simulated_distance >= observed_distance)
) / (length(simulated_distance) + 1)
primary_global <- global[global$configuration == primary, ]
add_check(
  "primary_global_statistics",
  close_enough(observed_distance, primary_global$observed_value) &&
    close_enough(observed_p, primary_global$empirical_p),
  paste(
    "Mahalanobis=", round(observed_distance, 4),
    "p=", round(observed_p, 4)
  )
)

quality_recomputed <- recompute_summary(
  quality_summary, quality_null
)
add_check(
  "quality_null_statistics",
  close_enough(
    quality_recomputed$p,
    quality_summary$directional_or_two_sided_p
  ) &&
    close_enough(
      quality_recomputed$fwer, quality_summary$maxT_FWER_p
    ),
  paste("features=", nrow(quality_summary))
)
population_recomputed <- recompute_summary(
  population_summary, population_null
)
add_check(
  "population_scale_statistics",
  close_enough(
    population_recomputed$p,
    population_summary$directional_or_two_sided_p
  ) &&
    close_enough(
      population_recomputed$fwer, population_summary$maxT_FWER_p
    ),
  paste("scales=", nrow(population_summary))
)

convergence_p <- numeric(nrow(convergence_summary))
for (index in seq_len(nrow(convergence_summary))) {
  simulated <- convergence_null[
    convergence_null$spike_feature ==
      convergence_summary$spike_feature[index],
    convergence_summary$metric[index]
  ]
  convergence_p[index] <- (
    1 + sum(simulated >= convergence_summary$observed_value[index])
  ) / (length(simulated) + 1)
}
add_check(
  "convergence_empirical_p",
  close_enough(convergence_p, convergence_summary$empirical_p) &&
    close_enough(
      stats::p.adjust(convergence_p, method = "BH"),
      convergence_summary$BH_q
    ),
  paste("metrics=", nrow(convergence_summary))
)
add_check(
  "followup_convergence_flags",
  sum(followup$joint_q10_consensus_spike %in% TRUE) == 1L &&
    nrow(followup) == 16L &&
    identical(sort(followup$followup_rank), seq_len(16L)),
  paste(
    "candidates=", nrow(followup),
    "joint q10 and human spike=",
    sum(followup$joint_q10_consensus_spike %in% TRUE)
  )
)
add_check(
  "response_blind_human_features",
  grepl(
    "held out", metadata_value[["human_feature_role"]], fixed = TRUE
  ) &&
    !grepl(
      "human", metadata_value[["case_selector"]], fixed = TRUE
    ),
  "human variables do not define cases or neighbourhoods"
)
add_check(
  "no_residual_response",
  metadata_value[["residual_used_as_response"]] == "false",
  "flower-colour events and replicated maps are used"
)
validation <- do.call(rbind, checks)
utils::write.csv(
  validation,
  file.path(output_dir, "human_neighbourhood_independent_validation.csv"),
  row.names = FALSE
)
lines <- c(
  paste0(
    "# v21 local human-neighbourhood independent validation: ",
    if (all(validation$status == "PASS")) "PASS" else "FAIL"
  ),
  "",
  vapply(seq_len(nrow(validation)), function(index) {
    paste0(
      "- **", validation$status[index], " - ",
      validation$check[index], "**: ", validation$detail[index]
    )
  }, character(1))
)
writeLines(
  lines, file.path(output_dir, "VALIDATION.md"), useBytes = TRUE
)
if (any(validation$status != "PASS")) {
  print(validation[validation$status != "PASS", ])
  stop("v21 independent validation failed.", call. = FALSE)
}
cat("v21 independent validation passed: ", nrow(validation), " checks\n")
