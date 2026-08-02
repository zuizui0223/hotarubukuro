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
    "Resampling is registry-defined: CHELSA/elevation use bilinear, SoilGrids uses",
    "masked area averaging, categorical data use nearest neighbour;",
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
registry <- read_raster_sources(registry_path)
sources <- select_sources(registry, split_source_ids(args$only))
raster_processing_code_sha256 <- sha256_file(
  file.path(repo_root, "R", "raster_sources.R")
)
raster_preparation_script_sha256 <- sha256_file(
  file.path(repo_root, "scripts", "prepare_rasters.R")
)
pipeline_config_sha256 <- sha256_file(pipeline_path)
raster_registry_sha256 <- sha256_file(registry_path)
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

# The SoilGrids WCS service encodes ocean/non-soil as numeric zero without a
# NoData flag. Fetch the bulk-density layer first and use bdod > 0 as one
# common native-grid validity mask for every SoilGrids property. This preserves
# legitimate zeros in other properties while preventing ocean zeros from
# contaminating local area averages along coasts.
soilgrids_selected <- grepl("^soilgrids_", sources$source_id)
soilgrids_mask_source <- registry[
  registry$source_id == "soilgrids_bdod_0_5cm",
  ,
  drop = FALSE
]
soilgrids_mask_path <- NULL
if (no_download) {
  required_cache_sources <- sources
  if (any(soilgrids_selected) &&
      !("soilgrids_bdod_0_5cm" %in% required_cache_sources$source_id)) {
    required_cache_sources <- rbind(required_cache_sources, soilgrids_mask_source)
  }
  missing_cache <- vapply(seq_len(nrow(required_cache_sources)), function(index) {
    path <- source_cache_path(required_cache_sources[index, , drop = FALSE], cache_dir)
    !file.exists(path) || file.info(path)$size <= 0
  }, logical(1))
  if (any(missing_cache)) {
    stop("--no-download requested but cache is missing for: ",
         paste(required_cache_sources$source_id[missing_cache], collapse = ", "), call. = FALSE)
  }
  invisible(lapply(seq_len(nrow(required_cache_sources)), function(index) {
    source_row <- required_cache_sources[index, , drop = FALSE]
    verify_expected_hash(
      source_cache_path(source_row, cache_dir),
      source_row$expected_sha256[[1L]]
    )
  }))
}
if (any(soilgrids_selected)) {
  if (nrow(soilgrids_mask_source) != 1L || !isTRUE(soilgrids_mask_source$enabled[[1L]])) {
    stop("Enabled SoilGrids processing requires soilgrids_bdod_0_5cm as its validity mask.", call. = FALSE)
  }
  soilgrids_mask_path <- materialize_source(
    soilgrids_mask_source,
    cache_dir,
    bbox,
    force = FALSE,
    retries = as.integer(pipeline$download$retries),
    timeout_seconds = as.numeric(pipeline$download$timeout_seconds),
    quiet = isTRUE(pipeline$download$quiet),
    allow_download = !no_download
  )
}

current_manifest <- if (file.exists(manifest_path)) {
  utils::read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  NULL
}
if (!is.null(current_manifest)) {
  current_manifest <- current_manifest[
    current_manifest$source_id %in% registry$source_id[registry$enabled],
    ,
    drop = FALSE
  ]
}
for (index in seq_len(nrow(sources))) {
  source_row <- sources[index, , drop = FALSE]
  message("Preparing ", source_row$source_id[[1]], " ...")
  row <- prepare_source(
    source_row, cache_dir, processed_dir, template, bbox,
    force_download = FALSE, force_process = force,
    retries = as.integer(pipeline$download$retries),
    timeout_seconds = as.numeric(pipeline$download$timeout_seconds),
    quiet = isTRUE(pipeline$download$quiet),
    validity_mask_path = if (grepl("^soilgrids_", source_row$source_id[[1L]])) {
      soilgrids_mask_path
    } else {
      NULL
    },
    processing_code_sha256 = raster_processing_code_sha256,
    preparation_script_sha256 = raster_preparation_script_sha256,
    allow_download = !no_download
  )
  row$cache_path <- repo_relative_path(row$cache_path, repo_root)
  row$processed_path <- repo_relative_path(row$processed_path, repo_root)
  row$pipeline_version <- as.integer(pipeline$version)
  row$pipeline_config_sha256 <- pipeline_config_sha256
  row$raster_registry_sha256 <- raster_registry_sha256
  row$raster_processing_code_sha256 <- raster_processing_code_sha256
  row$raster_preparation_script_sha256 <- raster_preparation_script_sha256
  if (!is.na(row$validity_mask_path[[1L]])) {
    row$validity_mask_path <- repo_relative_path(row$validity_mask_path, repo_root)
  }
  if (!is.null(current_manifest)) {
    current_manifest <- current_manifest[
      current_manifest$source_id != row$source_id[[1L]],
      ,
      drop = FALSE
    ]
    current_manifest <- bind_manifest_row(current_manifest, row)
  } else {
    current_manifest <- row
  }
  canonical_columns <- c(
    names(row),
    sort(setdiff(names(current_manifest), names(row)))
  )
  current_manifest <- current_manifest[canonical_columns]
  current_manifest <- current_manifest[order(current_manifest$source_id), , drop = FALSE]
  write_csv_atomic(current_manifest, manifest_path)
}
cat("Processed manifest:", repo_relative_path(manifest_path, repo_root), "\n")
