args <- commandArgs(trailingOnly = TRUE)
positional <- args[!startsWith(args, "--")]
output_dir <- if (length(positional)) {
  positional[[1L]]
} else {
  "results/ecological_v21_local_human_neighbourhood"
}

# See validation/audit_phenotype.R. Two constants here describe the published
# run rather than the analysis: 1,307 cells (the 1-km aggregation of the
# published 1,923 observations) and 16 local-isolate candidates.
#
# The candidate count is a result, not a population size. The reconstruction
# fits the presence model on a different observation set, so it has no
# obligation to recover 16, and asserting it would be asserting that a
# different analysis must reproduce a number it cannot be expected to
# reproduce. Under --baseline reconstruction both are reported as
# not_applicable with the observed value beside the published one; under
# --baseline published both are enforced exactly as before.
#
# Every structural invariant conjoined with those counts stays enforced in both
# modes: candidate uniqueness, the >=3 white-neighbour definition, the
# follow-up ranks forming a permutation, and the population-scale correlation
# bounds.
baseline_argument <- grep("^--baseline=", args, value = TRUE)
baseline <- if (length(baseline_argument)) {
  sub("^--baseline=", "", baseline_argument[[1L]])
} else {
  "published"
}
if (!baseline %in% c("published", "reconstruction")) {
  stop(
    "--baseline must be 'published' or 'reconstruction'; got '", baseline, "'.",
    call. = FALSE
  )
}
published_cell_count <- 1307L
published_candidate_count <- 16L
# Not a population size: the 1-km MLIT land-use grid the published run built
# from its own copy of the primary-mesh archives.
published_mlit_cell_count <- 309600L

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
add_not_applicable <- function(check, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check, status = "not_applicable",
    detail = as.character(detail), stringsAsFactors = FALSE
  )
}
# Enforced under --baseline published, reported with its difference under
# --baseline reconstruction. Never silently dropped in either mode.
add_published_count <- function(check, observed, published) {
  if (identical(baseline, "published")) {
    add_check(check, observed == published, paste("observed=", observed))
  } else {
    add_not_applicable(check, paste0(
      "observed=", observed, ";published=", published,
      ";difference=", observed - published,
      ";reason=the reconstruction defines its own analysis population"
    ))
  }
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
# The 1-km MLIT land-use grid is built from the primary-mesh archives listed in
# the snapshot's own download manifest (v21_process_mlit_classes). It does not
# depend on the observation population at all, so a difference here is an input
# coverage difference in the MLIT product, not a 1,909-versus-1,923 effect. It
# is reported separately from the population checks for exactly that reason.
#
# Uniqueness is the structural invariant and is enforced in both modes: a
# duplicated 1-km mesh key would corrupt the per-observation lookup.
add_check(
  "mlit_class_cell_uniqueness",
  !anyDuplicated(class_cells$mesh_1km),
  paste(
    "rows=", nrow(class_cells),
    "unique=", length(unique(class_cells$mesh_1km))
  )
)
if (identical(baseline, "published")) {
  add_check(
    "mlit_class_cell_coverage",
    nrow(class_cells) == published_mlit_cell_count,
    paste("rows=", nrow(class_cells))
  )
} else {
  add_not_applicable("mlit_class_cell_coverage", paste0(
    "observed=", nrow(class_cells),
    ";published=", published_mlit_cell_count,
    ";difference=", nrow(class_cells) - published_mlit_cell_count,
    ";reason=MLIT primary-mesh coverage in the verified snapshot differs from",
    " the published run's cache; this is an input difference, not a",
    " consequence of the reconstruction's analysis population"
  ))
}
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
  !anyDuplicated(features$exact_site_id),
  paste("rows=", nrow(features))
)
add_published_count(
  "analysis_cell_population", nrow(features), published_cell_count
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
  nrow(population_context) == nrow(features) &&
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
  !anyDuplicated(primary_details$exact_site_id) &&
    all(primary_details$n_white_neighbours >= 3L),
  paste("focal cells=", nrow(primary_details))
)
add_published_count(
  "primary_local_candidate_count",
  nrow(primary_details), published_candidate_count
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
  identical(sort(followup$followup_rank), seq_len(nrow(followup))),
  paste(
    "candidates=", nrow(followup),
    "joint q10 and human spike=",
    sum(followup$joint_q10_consensus_spike %in% TRUE)
  )
)
add_published_count(
  "followup_candidate_count", nrow(followup), published_candidate_count
)
# "One leading joint candidate" is a published finding, not a structural
# invariant. Enforced against the published run, reported for the
# reconstruction so the difference is visible rather than asserted away.
add_published_count(
  "followup_joint_q10_consensus_spike_count",
  sum(followup$joint_q10_consensus_spike %in% TRUE), 1L
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
    if (any(validation$status == "FAIL")) "FAIL" else "PASS"
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
failed <- validation[validation$status == "FAIL", , drop = FALSE]
skipped <- validation[validation$status == "not_applicable", , drop = FALSE]
if (nrow(failed)) {
  print(failed)
  stop("v21 independent validation failed.", call. = FALSE)
}
if (nrow(skipped)) {
  cat("v21 checks not applicable under --baseline ", baseline, ":\n", sep = "")
  print(skipped)
}
cat(
  "v21 independent validation passed: ", sum(validation$status == "PASS"),
  " checks (", nrow(skipped), " not applicable)\n", sep = ""
)
