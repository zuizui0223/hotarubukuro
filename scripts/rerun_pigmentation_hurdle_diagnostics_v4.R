args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = "") {
  idx <- match(flag, args)
  if (is.na(idx) || idx == length(args)) default else args[idx + 1L]
}

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[1L])
} else "scripts/rerun_pigmentation_hurdle_diagnostics_v4.R"
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

collinearity <- fit_hurdle_collinearity_audit(data, environment_terms)
write_csv_safe(
  collinearity$vif,
  file.path(output_dir, "pigmentation_hurdle_collinearity_vif.csv")
)
write_csv_safe(
  collinearity$correlations,
  file.path(output_dir, "pigmentation_hurdle_collinearity_correlations.csv")
)
write_csv_safe(
  collinearity$condition,
  file.path(output_dir, "pigmentation_hurdle_collinearity_condition.csv")
)

bombus <- fit_primary_hurdle_bombus(data, environment_terms, k_space)
bind_part <- function(fits, part) do.call(rbind, lapply(fits, `[[`, part))
for (role in c("presence", "intensity")) {
  prefix <- if (role == "presence") {
    "pigmentation_presence_bombus"
  } else "pigmented_intensity_bombus"
  write_csv_safe(
    bind_part(bombus[[role]], "coefficients"),
    file.path(output_dir, paste0(prefix, "_coefficients.csv"))
  )
  write_csv_safe(
    bind_part(bombus[[role]], "heldout"),
    file.path(output_dir, paste0(prefix, "_heldout.csv"))
  )
  write_csv_safe(
    bind_part(bombus[[role]], "log"),
    file.path(output_dir, paste0(prefix, "_crossfit_log.csv"))
  )
}

summary_path <- file.path(output_dir, "run_summary.json")
if (file.exists(summary_path) && requireNamespace("jsonlite", quietly = TRUE)) {
  run_summary <- jsonlite::read_json(summary_path, simplifyVector = TRUE)
  run_summary$collinearity_audit <- TRUE
  run_summary$crossfit_warning_audit <- TRUE
  jsonlite::write_json(
    run_summary, summary_path, pretty = TRUE, auto_unbox = TRUE, na = "null"
  )
}

cat("Completed hurdle diagnostics rerun: ", output_dir, "\n", sep = "")
