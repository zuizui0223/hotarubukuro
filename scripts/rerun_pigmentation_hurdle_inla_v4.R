args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = "") {
  idx <- match(flag, args)
  if (is.na(idx) || idx == length(args)) default else args[idx + 1L]
}

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[1L])
} else "scripts/rerun_pigmentation_hurdle_inla_v4.R"
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
data <- utils::read.csv(
  analysis_csv, check.names = FALSE, stringsAsFactors = FALSE
)
data$region <- factor(data$region, levels = c("West", "East"))
environment_terms <- grep("^env_", names(data), value = TRUE)

presence <- fit_hurdle_inla_model_set(
  data, "pigmented_mixture50", environment_terms, "binomial", "presence"
)
intensity <- fit_hurdle_inla_model_set(
  data, "pigment_intensity_z", environment_terms, "gaussian", "intensity"
)

write_csv_safe(
  rbind(presence$metrics, intensity$metrics),
  file.path(output_dir, "pigmentation_hurdle_inla_model_comparison.csv")
)
write_csv_safe(
  rbind(presence$fixed, intensity$fixed),
  file.path(output_dir, "pigmentation_hurdle_inla_fixed_effects.csv")
)
write_csv_safe(
  rbind(presence$hyper, intensity$hyper),
  file.path(output_dir, "pigmentation_hurdle_inla_hyperparameters.csv")
)

summary_path <- file.path(output_dir, "run_summary.json")
if (file.exists(summary_path) && requireNamespace("jsonlite", quietly = TRUE)) {
  run_summary <- jsonlite::read_json(summary_path, simplifyVector = TRUE)
  run_summary$inla_run <- TRUE
  run_summary$inla_model_count <- nrow(presence$metrics) + nrow(intensity$metrics)
  jsonlite::write_json(
    run_summary, summary_path, pretty = TRUE, auto_unbox = TRUE, na = "null"
  )
}

cat("Completed hurdle-only SPDE-INLA rerun: ", output_dir, "\n", sep = "")
