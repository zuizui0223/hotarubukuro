args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = "") {
  idx <- match(flag, args)
  if (is.na(idx) || idx == length(args)) default else args[idx + 1L]
}

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[1L])
} else "scripts/rerun_pigmentation_residual_tail_v4.R"
repo_root <- normalizePath(
  file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE
)
source(file.path(repo_root, "scripts", "ecological_analysis_v2.R"))
source(file.path(repo_root, "scripts", "ecological_mechanism_v3.R"))
source(file.path(repo_root, "scripts", "pigmentation_hurdle_v4.R"))

output_dir <- arg_value(
  "--output-dir",
  file.path(repo_root, "results", "ecological_v11_pigmentation_hurdle")
)
analysis_csv <- arg_value(
  "--analysis-data",
  file.path(output_dir, "analysis_data_pigmentation_hurdle.csv")
)
k_space <- as.integer(arg_value("--k-space", "40"))
data <- utils::read.csv(
  analysis_csv, check.names = FALSE, stringsAsFactors = FALSE
)
data$region <- factor(data$region, levels = c("West", "East"))
environment_terms <- grep("^env_", names(data), value = TRUE)

natural_residuals <- build_hurdle_natural_residuals(data, environment_terms, k_space)
data <- natural_residuals$data
write_csv_safe(
  natural_residuals$log,
  file.path(output_dir, "pigmentation_natural_residual_crossfit_log.csv")
)
residual_tail <- fit_hurdle_residual_tail(data, k_space)
write_csv_safe(
  residual_tail$coefficients,
  file.path(output_dir, "pigmentation_residual_tail_HR_coefficients.csv")
)
write_csv_safe(
  residual_tail$heldout,
  file.path(output_dir, "pigmentation_residual_tail_HR_heldout.csv")
)
write_csv_safe(
  residual_tail$log,
  file.path(output_dir, "pigmentation_residual_tail_HR_crossfit_log.csv")
)
write_csv_safe(
  residual_tail$early_enrichment,
  file.path(output_dir, "pigmentation_residual_tail_early_enrichment.csv")
)
write_csv_safe(
  residual_tail$candidates,
  file.path(output_dir, "pigmentation_residual_tail_candidates.csv")
)
write_csv_safe(data, analysis_csv)

summary_path <- file.path(output_dir, "run_summary.json")
if (file.exists(summary_path) && requireNamespace("jsonlite", quietly = TRUE)) {
  run_summary <- jsonlite::read_json(summary_path, simplifyVector = TRUE)
  run_summary$residual_tail_run <- TRUE
  run_summary$residual_tail_thresholds <- c(0.80, 0.90, 0.95)
  run_summary$residual_role <- paste(
    "cross-fitted natural-model departures for exploratory tail convergence",
    "and candidate ranking; not horticultural labels"
  )
  jsonlite::write_json(
    run_summary, summary_path, pretty = TRUE, auto_unbox = TRUE, na = "null"
  )
}

cat("Completed cross-fitted residual-tail rerun: ", output_dir, "\n", sep = "")
