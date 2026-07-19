#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_argument)) sub("^--file=", "", script_argument[[1]]) else "scripts/validate_sdm.R"
default_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
repo_root <- Sys.getenv("HOTARUBUKURO_ROOT", unset = default_root)
source(file.path(repo_root, "R", "raster_sources.R"))
source(file.path(repo_root, "R", "sdm.R"))

parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  result <- list()
  index <- 1L
  while (index <= length(args)) {
    token <- args[[index]]
    if (token == "--help") result$help <- TRUE
    if (token %in% c("--sdm-dir", "--manifest", "--report")) {
      if (index == length(args)) stop("Missing value after ", token, call. = FALSE)
      key <- gsub("-", "_", sub("^--", "", token))
      result[[key]] <- args[[index + 1L]]
      index <- index + 1L
    }
    index <- index + 1L
  }
  result
}

args <- parse_args()
if (isTRUE(args$help)) {
  cat("Usage: Rscript scripts/validate_sdm.R [--sdm-dir sdm] [--manifest sdm/manifest.csv] [--report path.csv]\n")
  quit(status = 0)
}
sdm_dir <- args$sdm_dir %||% file.path(repo_root, "sdm")
if (!grepl("^/", sdm_dir)) sdm_dir <- file.path(repo_root, sdm_dir)
manifest <- args$manifest %||% file.path(sdm_dir, "manifest.csv")
if (!grepl("^/", manifest)) manifest <- file.path(repo_root, manifest)
validation <- validate_sdm_collection(sdm_dir, manifest)
print_sdm_validation(validation)
if (!is.null(args$report)) {
  report <- if (grepl("^/", args$report)) args$report else file.path(repo_root, args$report)
  dir.create(dirname(report), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(validation$report, report, row.names = FALSE)
}
if (!validation$structural_ok) quit(status = 1)
