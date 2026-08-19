args <- commandArgs(trailingOnly = TRUE)

parse_numeric_vector <- function(x) {
  value <- suppressWarnings(as.numeric(strsplit(x, ",", fixed = TRUE)[[1]]))
  if (!length(value) || any(!is.finite(value))) {
    stop("Expected a comma-separated numeric vector, received: ", x, call. = FALSE)
  }
  value
}

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[1])
} else "scripts/run_natural_biotic_covariates.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "pipeline_support.R"))
arg_value <- function(flag, default = "") hb_arg_value(args, flag, default)
hb_require_stage_packages("phenotype")
hb_load_modules("phenotype", root = repo_root)

analysis_data_csv <- arg_value(
  "--analysis-data",
  file.path(repo_root, "results", "ecological_v9_final_public_HRNA_50km", "analysis_data.csv")
)
occurrence_dir <- arg_value("--occurrence-dir", Sys.getenv("HOTARUBUKURO_BOMBUS_OCCURRENCE_DIR"))
output_dir <- arg_value(
  "--output-dir", file.path(repo_root, "results", "ecological_v10_final_mechanism_HRNA")
)
raw_colour_csv <- arg_value("--raw-colour-csv", file.path(repo_root, "Data_S1.csv"))
if (!nzchar(occurrence_dir) || !dir.exists(occurrence_dir)) {
  stop(
    "Supply --occurrence-dir or HOTARUBUKURO_BOMBUS_OCCURRENCE_DIR. ",
    "It must contain the five frozen *_gbif.csv files produced by ",
    "scripts/fetch_bombus_occurrences.R.",
    call. = FALSE
  )
}

run_natural_biotic_covariates(
  analysis_data_csv = analysis_data_csv,
  occurrence_dir = occurrence_dir,
  output_dir = output_dir,
  raw_colour_csv = raw_colour_csv,
  radii_km = parse_numeric_vector(arg_value("--radii-km", "50,100")),
  occurrence_support = as.numeric(arg_value("--occurrence-support", "5")),
  reference_fraction = as.numeric(arg_value("--reference-fraction", "0.30")),
  match_radius_km = as.numeric(arg_value("--match-radius-km", "100")),
  match_env_caliper = as.numeric(arg_value("--match-env-caliper", "1.5")),
  match_propensity_caliper_sd = as.numeric(arg_value("--match-propensity-caliper-sd", "0.2")),
  tail_bootstraps = as.integer(arg_value("--tail-bootstraps", "199")),
  reference_k_space = as.integer(arg_value("--reference-k-space", "15")),
  random_seed = as.integer(arg_value("--random-seed", "42")),
  k_space = as.integer(arg_value("--k-space", "40"))
)

cat("Completed natural biotic-covariate analysis: ",
    normalizePath(output_dir, winslash = "/"), "\n", sep = "")
