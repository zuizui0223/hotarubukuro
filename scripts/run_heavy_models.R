#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else "scripts/run_heavy_models.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "heavy_models.R"))

options <- list(
  input = file.path(repo_root, "results", "ci", "expanded_bombus", "analysis_data.csv"),
  output_dir = file.path(repo_root, "results", "ci", "heavy_models")
)
map <- c("--input" = "input", "--output-dir" = "output_dir")
i <- 1L
while (i <= length(args)) {
  if (!args[[i]] %in% names(map) || i == length(args)) stop("Invalid command-line arguments.", call. = FALSE)
  options[[map[[args[[i]]]]]] <- args[[i + 1L]]
  i <- i + 2L
}
options$input <- normalizePath(options$input, winslash = "/", mustWork = TRUE)
if (dir.exists(options$output_dir) && length(list.files(options$output_dir, all.files = TRUE, no.. = TRUE))) {
  stop("Output directory is not empty.", call. = FALSE)
}
dir.create(options$output_dir, recursive = TRUE, showWarnings = FALSE)

data <- utils::read.csv(options$input, stringsAsFactors = FALSE, check.names = FALSE)
if (!"analysis_id" %in% names(data) || anyDuplicated(data$analysis_id)) {
  stop("Heavy-model input requires unique analysis_id values.", call. = FALSE)
}

cfa <- fit_pigment_cfa(data)
utils::write.csv(cfa$scores, file.path(options$output_dir, "cfa_pigment_scores.csv"), row.names = FALSE, na = "")
utils::write.csv(cfa$estimates, file.path(options$output_dir, "cfa_parameter_estimates.csv"), row.names = FALSE, na = "")
utils::write.csv(cfa$fit_measures, file.path(options$output_dir, "cfa_fit_measures.csv"), row.names = FALSE, na = "")
writeLines(cfa$model, file.path(options$output_dir, "cfa_model.txt"))

data$Pigment <- cfa$scores$Pigment[match(data$analysis_id, cfa$scores$analysis_id)]
spde <- fit_spde_inla(data)
utils::write.csv(spde$fixed, file.path(options$output_dir, "spde_inla_fixed_effects.csv"), row.names = FALSE, na = "")
utils::write.csv(spde$hyper, file.path(options$output_dir, "spde_inla_hyperparameters.csv"), row.names = FALSE, na = "")
utils::write.csv(spde$fitted, file.path(options$output_dir, "spde_inla_fitted_residuals.csv"), row.names = FALSE, na = "")
utils::write.csv(spde$variance_decomposition, file.path(options$output_dir, "spde_inla_variance_decomposition.csv"), row.names = FALSE, na = "")
utils::write.csv(spde$criteria, file.path(options$output_dir, "spde_inla_information_criteria.csv"), row.names = FALSE, na = "")
writeLines(spde$formula, file.path(options$output_dir, "spde_inla_formula.txt"))

qgam_data <- merge(data, spde$fitted[c("analysis_id", "residual")], by = "analysis_id", all = FALSE, sort = FALSE)
qg <- fit_residual_qgam(qgam_data)
utils::write.csv(qg$summary, file.path(options$output_dir, "qgam_smooth_summary.csv"), row.names = FALSE, na = "")
utils::write.csv(qg$predictions, file.path(options$output_dir, "qgam_predictions.csv"), row.names = FALSE, na = "")

row_flow <- data.frame(
  stage = c("expanded_common_input", "cfa_scored", "spde_complete", "qgam_complete"),
  records = c(nrow(data), sum(is.finite(data$Pigment)), spde$records, qg$records),
  stringsAsFactors = FALSE
)
utils::write.csv(row_flow, file.path(options$output_dir, "row_flow.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(options$output_dir, "sessionInfo.txt"))

required <- c(
  "cfa_pigment_scores.csv", "cfa_parameter_estimates.csv", "cfa_fit_measures.csv",
  "spde_inla_fixed_effects.csv", "spde_inla_hyperparameters.csv",
  "spde_inla_fitted_residuals.csv", "spde_inla_variance_decomposition.csv",
  "spde_inla_information_criteria.csv", "qgam_smooth_summary.csv",
  "qgam_predictions.csv", "row_flow.csv", "sessionInfo.txt"
)
paths <- file.path(options$output_dir, required)
stopifnot(all(file.exists(paths)), all(file.info(paths)$size > 0))
cat("Heavy models complete: CFA, SPDE-INLA, residual qGAM\n")
print(row_flow, row.names = FALSE)
