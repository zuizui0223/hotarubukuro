#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_argument)) sub("^--file=", "", script_argument[[1]]) else "scripts/prepare_rasters.R"
default_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
repo_root <- Sys.getenv("HOTARUBUKURO_ROOT", unset = default_root)
source(file.path(repo_root, "R", "raster_sources.R"))

usage <- function() {
  cat(paste(
    "Prepare public predictors on the canonical EPSG:4326 30 arc-second grid.",
    "",
    "Usage:",
    "  Rscript scripts/prepare_rasters.R [--only id1,id2] [--force] [--no-download]",
    "      [--dry-run] [--pipeline config/pipeline.yml]",
    "      [--config config/raster_sources.csv]",
    "",
    "Continuous data use bilinear interpolation; categorical data use nearest neighbour;",
    "WorldPop people-per-cell uses sum, then optional division by cell area for density.",
    sep = "\n"
  ))
}

args <- parse_cli_args()
if (isTRUE(args$help)) {
  usage()
  quit(status = 0)
}

pipeline_path <- resolve_repo_path(repo_root, args$pipeline %||% "config/pipeline.yml")
pipeline <- read_pipeline_config(pipeline_path)
registry_path <- resolve_repo_path(repo_root, args$config %||% pipeline$paths$source_registry)
cache_dir <- resolve_repo_path(repo_root, args$cache_dir %||% pipeline$paths$cache_dir)
processed_dir <- resolve_repo_path(repo_root, args$processed_dir %||% pipeline$paths$processed_dir)
manifest_path <- resolve_repo_path(repo_root, pipeline$paths$processed_manifest)
sources <- select_sources(read_raster_sources(registry_path), split_source_ids(args$only))
bbox <- pipeline_bbox(pipeline)
template <- canonical_grid(
  bbox, resolution_arcsec = pipeline$grid$resolution_arcsec,
  crs = pipeline$grid$crs,
  origin = c(pipeline$grid$origin_x, pipeline$grid$origin_y)
)
force <- if (is.null(args$force)) FALSE else as_flag(args$force, "force")
no_download <- if (is.null(args$no_download)) FALSE else as_flag(args$no_download, "no-download")
dry_run <- if (is.null(args$dry_run)) FALSE else as_flag(args$dry_run, "dry-run")

if (!nrow(sources)) stop("No enabled raster sources selected.", call. = FALSE)
cat("Canonical grid:", terra::ncol(template), "x", terra::nrow(template),
    "cells;", paste(terra::res(template), collapse = " x "), "degrees;",
    paste(as.vector(terra::ext(template)), collapse = ", "), "\n")
cat(paste0("- ", sources$source_id, " -> ",
           file.path(processed_dir, sources$output_name), " [",
           sources$resample_method, "]", collapse = "\n"), "\n")
if (dry_run) quit(status = 0)

if (no_download) {
  missing_cache <- vapply(seq_len(nrow(sources)), function(index) {
    !file.exists(source_cache_path(sources[index, , drop = FALSE], cache_dir))
  }, logical(1))
  if (any(missing_cache)) {
    stop("--no-download requested but cache is missing for: ",
         paste(sources$source_id[missing_cache], collapse = ", "), call. = FALSE)
  }
}

rows <- vector("list", nrow(sources))
for (index in seq_len(nrow(sources))) {
  source_row <- sources[index, , drop = FALSE]
  message("Preparing ", source_row$source_id[[1]], " ...")
  rows[[index]] <- prepare_source(
    source_row, cache_dir, processed_dir, template, bbox,
    force_download = FALSE, force_process = force,
    retries = as.integer(pipeline$download$retries),
    timeout_seconds = as.numeric(pipeline$download$timeout_seconds),
    quiet = isTRUE(pipeline$download$quiet)
  )
  rows[[index]]$cache_path <- repo_relative_path(rows[[index]]$cache_path, repo_root)
  rows[[index]]$processed_path <- repo_relative_path(rows[[index]]$processed_path, repo_root)
}
new_manifest <- do.call(rbind, rows)
if (file.exists(manifest_path)) {
  old_manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
  old_manifest <- old_manifest[!old_manifest$source_id %in% new_manifest$source_id, , drop = FALSE]
  new_manifest <- rbind(old_manifest, new_manifest)
}
new_manifest <- new_manifest[order(new_manifest$source_id), , drop = FALSE]
write_csv_atomic(new_manifest, manifest_path)
cat("Processed manifest:", repo_relative_path(manifest_path, repo_root), "\n")
