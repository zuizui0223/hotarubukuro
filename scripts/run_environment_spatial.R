args <- commandArgs(trailingOnly = TRUE)

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[1]) else "scripts/run_environment_spatial.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "pipeline_support.R"))
arg_value <- function(flag, default = "") hb_arg_value(args, flag, default)
as_bool <- hb_as_bool
hb_require_stage_packages("phenotype")
hb_load_modules("phenotype", root = repo_root)

anomaly_csv <- arg_value("--anomaly-csv", Sys.getenv("HOTARUBUKURO_ANOMALY_CSV"))
raw_colour_csv <- arg_value("--raw-colour-csv", file.path(repo_root, "Data_S1.csv"))
bombus_dir <- arg_value("--bombus-dir", Sys.getenv("HOTARUBUKURO_BOMBUS_DIR"))
output_dir <- arg_value("--output-dir", file.path(repo_root, "results", "ecological_v2"))

if (!nzchar(anomaly_csv) || !file.exists(anomaly_csv)) {
  stop("Supply --anomaly-csv or HOTARUBUKURO_ANOMALY_CSV.", call. = FALSE)
}
if (!nzchar(bombus_dir) || !dir.exists(bombus_dir)) {
  stop("Supply --bombus-dir or HOTARUBUKURO_BOMBUS_DIR. It must contain fresh ENMeval predictions.",
       call. = FALSE)
}

raster_paths <- list(
  H = arg_value("--H-raster", Sys.getenv("HOTARUBUKURO_H_RASTER")),
  R = arg_value("--R-raster", Sys.getenv("HOTARUBUKURO_R_RASTER")),
  N = arg_value("--N-raster", Sys.getenv("HOTARUBUKURO_N_RASTER")),
  A = arg_value("--A-raster", Sys.getenv("HOTARUBUKURO_A_RASTER"))
)

run_environment_spatial_analysis(
  anomaly_csv = anomaly_csv,
  raw_colour_csv = raw_colour_csv,
  bombus_dir = bombus_dir,
  output_dir = output_dir,
  raster_paths = raster_paths,
  strict_qc = as_bool(arg_value("--strict-qc", "false")),
  east_west_cut = as.numeric(arg_value("--east-west-cut", "136.5")),
  block_km = as.numeric(arg_value("--block-km", "50")),
  folds = as.integer(arg_value("--folds", "5")),
  run_inla = as_bool(arg_value("--run-inla", "true")),
  k_space = as.integer(arg_value("--k-space", "40")),
  author_review_confirmed = as_bool(arg_value("--author-review-confirmed", "true"))
)

cat("Completed environment-spatial analysis: ",
    normalizePath(output_dir, winslash = "/"), "\n", sep = "")
