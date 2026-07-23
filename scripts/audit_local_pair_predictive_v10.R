args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) {
  args[1]
} else "results/ecological_v17_local_pair_turnover"

source("scripts/local_pair_predictive_v10.R")

read_output <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing output: ", path, call. = FALSE)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

edges <- read_output("local_pair_edges.csv")
summary <- read_output("local_pair_predictive_summary.csv")
null <- read_output("local_pair_predictive_null.csv")
stability <- read_output("local_pair_simulation_stability.csv")
metadata <- read_output("local_pair_metadata.csv")
independent_validation <- read_output("local_pair_independent_validation.csv")

checks <- list()
add_check <- function(check, pass, evidence, severity = "critical") {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check,
    status = if (isTRUE(pass)) "PASS" else "FAIL",
    severity = severity,
    evidence = evidence,
    stringsAsFactors = FALSE
  )
}
meta <- function(field) metadata$value[match(field, metadata$field)]

add_check(
  "analysis_version",
  identical(meta("analysis_spec_version"), v17_analysis_spec_version),
  paste("version=", meta("analysis_spec_version"))
)
pair_key <- paste(edges$radius_km, pmin(edges$i, edges$j),
                  pmax(edges$i, edges$j), sep = "::")
add_check(
  "unique_undirected_pairs", !anyDuplicated(pair_key),
  paste("duplicate pairs=", sum(duplicated(pair_key)))
)
add_check(
  "response_blind_pair_construction",
  identical(
    meta("pair_construction"),
    "response-blind undirected k-nearest graph on five-species common support"
  ),
  meta("pair_construction")
)
add_check(
  "same_fold_joint_support",
  all(edges$fold_i == edges$fold_j),
  paste("cross-fold edges=", sum(edges$fold_i != edges$fold_j))
)
add_check(
  "five_species_common_support",
  all(edges$both_common_support),
  paste("unsupported edges=", sum(!edges$both_common_support))
)
add_check(
  "fixed_scales",
  length(unique(edges$radius_km)) == 3L &&
    setequal(as.numeric(unique(edges$radius_km)), c(10, 25, 50)),
  paste("radii=", paste(sort(unique(edges$radius_km)), collapse = ","))
)
add_check(
  "hierarchical_intensity_support",
  all(
    is.finite(edges$observed_intensity_transition[
      edges$conditional_intensity_pair
    ])
  ) && all(
    is.na(edges$observed_intensity_transition[
      !edges$conditional_intensity_pair
    ])
  ),
  paste("conditional intensity pairs=",
        sum(edges$conditional_intensity_pair))
)
add_check(
  "community_block_not_species_coefficients",
  all(!grepl(
    "ardens|diversus|beaticola|consobrinus|honshuensis",
    summary$predictor
  )),
  paste("predictors=", paste(unique(summary$predictor), collapse = ","))
)
add_check(
  "predictive_draw_completion",
  all(table(interaction(
    null$response, null$radius_km, null$predictor, drop = TRUE
  )) == 1000L),
  paste("draw range=", paste(range(null$draw), collapse = " to "))
)
add_check(
  "finite_primary_statistics",
  all(is.finite(
    unlist(summary[
      summary$predictor_role == "primary_turnover",
      c("observed_partial_beta", "beta_empirical_p", "observed_delta_r2")
    ])
  )),
  paste("primary rows=", sum(summary$predictor_role == "primary_turnover"))
)
add_check(
  "probability_bounds",
  all(summary$beta_empirical_p >= 0 & summary$beta_empirical_p <= 1),
  paste("p range=", paste(range(summary$beta_empirical_p), collapse = " to "))
)
add_check(
  "residual_not_pair_response",
  identical(meta("residual_as_pair_response"), "false"),
  paste("residual_as_pair_response=", meta("residual_as_pair_response"))
)
add_check(
  "year_is_nuisance_only",
  grepl("nuisance", meta("observation_year_role"), fixed = TRUE),
  meta("observation_year_role"),
  severity = "medium"
)
add_check(
  "simulation_stability",
  max(stability$absolute_mean_difference, na.rm = TRUE) < 0.05,
  paste(
    "maximum half-mean difference=",
    signif(max(stability$absolute_mean_difference, na.rm = TRUE), 4)
  ),
  severity = "medium"
)
add_check(
  "independent_recalculation",
  nrow(independent_validation) >= 10L &&
    all(independent_validation$status == "PASS"),
  paste(
    "checks=", nrow(independent_validation),
    "failures=", sum(independent_validation$status != "PASS")
  )
)
current_md5 <- if (file.exists("final.Rmd")) {
  unname(tools::md5sum("final.Rmd"))
} else NA_character_
add_check(
  "final_Rmd_untouched",
  identical(current_md5, meta("final_Rmd_md5")),
  paste("current=", current_md5, "recorded=", meta("final_Rmd_md5"))
)

audit <- do.call(rbind, checks)
write.csv(audit, file.path(output_dir, "local_pair_audit.csv"), row.names = FALSE)
overall <- if (all(audit$status == "PASS")) "PASS" else "NEEDS_REVISION"
lines <- c(
  paste0("# v17 local-pair audit: ", overall),
  "",
  vapply(seq_len(nrow(audit)), function(i) {
    paste0(
      "- **", audit$status[i], " - ", audit$check[i], "**: ",
      audit$evidence[i]
    )
  }, character(1))
)
writeLines(lines, file.path(output_dir, "AUDIT.md"), useBytes = TRUE)
cat(overall, "\n")
print(audit)
if (!all(audit$status == "PASS")) quit(status = 1L)
