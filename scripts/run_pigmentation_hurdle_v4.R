args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = "") {
  idx <- match(flag, args)
  if (is.na(idx) || idx == length(args)) default else args[idx + 1L]
}

as_bool <- function(x) tolower(x) %in% c("1", "true", "yes", "y")

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[1L])
} else "scripts/run_pigmentation_hurdle_v4.R"
repo_root <- normalizePath(
  file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE
)
source(file.path(repo_root, "scripts", "ecological_analysis_v2.R"))
source(file.path(repo_root, "scripts", "ecological_mechanism_v3.R"))
source(file.path(repo_root, "scripts", "pigmentation_hurdle_v4.R"))

analysis_csv <- arg_value(
  "--analysis-data",
  file.path(
    repo_root, "results", "ecological_v10_final_mechanism_HRNA",
    "analysis_data_mechanism_v3.csv"
  )
)
output_dir <- arg_value(
  "--output-dir",
  file.path(repo_root, "results", "ecological_v11_pigmentation_hurdle")
)

invisible(run_pigmentation_hurdle_v4(
  analysis_csv = analysis_csv,
  output_dir = output_dir,
  run_inla = as_bool(arg_value("--run-inla", "true")),
  k_space = as.integer(arg_value("--k-space", "40")),
  confidence_cut = as.numeric(arg_value("--confidence-cut", "0.8"))
))

cat("Completed pigmentation hurdle v4: ", output_dir, "\n", sep = "")
