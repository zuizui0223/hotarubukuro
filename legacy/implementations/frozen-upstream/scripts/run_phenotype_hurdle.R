args <- commandArgs(trailingOnly = TRUE)

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[1L])
} else "scripts/run_phenotype_hurdle.R"
repo_root <- normalizePath(
  file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE
)
source(file.path(repo_root, "R", "pipeline_support.R"))
arg_value <- function(flag, default = "") hb_arg_value(args, flag, default)
as_bool <- hb_as_bool
hb_require_stage_packages("phenotype")
hb_load_modules("phenotype", root = repo_root)

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

invisible(run_phenotype_hurdle(
  analysis_csv = analysis_csv,
  output_dir = output_dir,
  run_inla = as_bool(arg_value("--run-inla", "true")),
  k_space = as.integer(arg_value("--k-space", "40")),
  confidence_cut = as.numeric(arg_value("--confidence-cut", "0.8"))
))

cat("Completed two-part phenotype model: ", output_dir, "\n", sep = "")
