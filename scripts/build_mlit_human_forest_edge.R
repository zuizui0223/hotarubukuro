args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = "") {
  idx <- match(flag, args)
  if (is.na(idx) || idx == length(args)) default else args[idx + 1L]
}

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[1L])
} else "scripts/build_mlit_human_forest_edge.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "mlit_human_forest_edge.R"))

observation_csv <- arg_value(
  "--observation-csv",
  file.path(repo_root, "Data_S1.csv")
)
output_dir <- arg_value(
  "--output-dir",
  file.path(repo_root, "results", "public_rasters", "mlit_human_forest_edge_2021")
)
cache_dir <- arg_value(
  "--cache-dir",
  Sys.getenv(
    "HOTARUBUKURO_MLIT_CACHE",
    file.path(path.expand("~"), ".cache", "hotarubukuro", "mlit_l03_2021")
  )
)

build_mlit_human_forest_edge(observation_csv, output_dir, cache_dir)
cat(
  "Completed MLIT human-forest edge layer: ",
  normalizePath(output_dir, winslash = "/"), "\n", sep = ""
)
