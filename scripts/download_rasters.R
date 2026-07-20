#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_argument)) sub("^--file=", "", script_argument[[1]]) else "scripts/download_rasters.R"
default_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
repo_root <- Sys.getenv("HOTARUBUKURO_ROOT", unset = default_root)
source(file.path(repo_root, "R", "raster_sources.R"))

usage <- function() {
  cat(paste(
    "Download or spatially subset the pinned public raster sources into data/cache.",
    "",
    "Usage:",
    "  Rscript scripts/download_rasters.R [--only id1,id2] [--force] [--dry-run]",
    "      [--pipeline config/pipeline.yml] [--config config/raster_sources.csv]",
    "      [--cache-dir data/cache/rasters]",
    "",
    "COG and SoilGrids WCS sources are cached only for the configured study extent.",
    "WorldClim is a zip archive; its checksum and the extracted TIFF checksum are recorded.",
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
registry_path <- resolve_repo_path(
  repo_root,
  args$config %||% pipeline$paths$source_registry
)
cache_dir <- resolve_repo_path(repo_root, args$cache_dir %||% pipeline$paths$cache_dir)
manifest_path <- resolve_repo_path(repo_root, pipeline$paths$download_manifest)
registry <- read_raster_sources(registry_path)
sources <- select_sources(registry, split_source_ids(args$only))

# Checksums in the registry are byte-level pins only for assets downloaded
# unchanged. COG/VRT and WCS entries are materialized as study-area GeoTIFFs by
# the runner's GDAL/terra stack. Their bytes can differ across GDAL versions
# while raster values and geometry remain equivalent, so enforcing a checksum
# recorded for a previously generated subset is invalid. For generated subsets
# we record the actual checksum, source URL, bbox, configuration and code hashes
# in the manifest and validate geometry/content during raster preparation.
generated_subset <- sources$access %in% c("cog", "vrt", "wcs")
sources$expected_sha256[generated_subset] <- NA_character_

pipeline_config_sha256 <- sha256_file(pipeline_path)
raster_registry_sha256 <- sha256_file(registry_path)
raster_processing_code_sha256 <- sha256_file(
  file.path(repo_root, "R", "raster_sources.R")
)
raster_download_script_sha256 <- sha256_file(
  file.path(repo_root, "scripts", "download_rasters.R")
)
bbox <- pipeline_bbox(pipeline)
force <- if (is.null(args$force)) FALSE else as_flag(args$force, "force")
dry_run <- if (is.null(args$dry_run)) FALSE else as_flag(args$dry_run, "dry-run")

if (!nrow(sources)) stop("No enabled raster sources selected.", call. = FALSE)
cat("Selected sources:\n")
cat(paste0("- ", sources$source_id, " [", sources$access, "] -> ",
           file.path(cache_dir, sources$cache_name), collapse = "\n"), "\n")
if (dry_run) quit(status = 0)

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
  message("Caching ", source_row$source_id[[1]], " ...")
  path <- materialize_source(
    source_row, cache_dir, bbox, force = force,
    retries = as.integer(pipeline$download$retries),
    timeout_seconds = as.numeric(pipeline$download$timeout_seconds),
    quiet = isTRUE(pipeline$download$quiet)
  )
  archive_path <- file.path(cache_dir, "archives", paste0(source_row$source_id[[1]], ".zip"))
  has_archive <- source_row$access[[1]] == "zip" && file.exists(archive_path)
  row <- data.frame(
    source_id = source_row$source_id[[1]], provider = source_row$provider[[1]],
    dataset_version = source_row$dataset_version[[1]], access = source_row$access[[1]],
    source_url = source_row$url[[1]], source_page = source_row$source_page[[1]],
    coverage_id = source_row$coverage_id[[1]],
    wcs_width = source_row$wcs_width[[1]], wcs_height = source_row$wcs_height[[1]],
    wcs_request_url = if (source_row$access[[1]] == "wcs") {
      soilgrids_wcs_request(source_row, bbox)
    } else {
      NA_character_
    },
    cache_path = repo_relative_path(path, repo_root),
    cache_sha256 = sha256_file(path), cache_bytes = unname(file.info(path)$size),
    archive_path = if (has_archive) repo_relative_path(archive_path, repo_root) else NA_character_,
    archive_sha256 = if (has_archive) sha256_file(archive_path) else NA_character_,
    archive_bytes = if (has_archive) unname(file.info(archive_path)$size) else NA_real_,
    cached_at_utc = format(file.info(path)$mtime, tz = "UTC", usetz = TRUE),
    expected_sha256 = source_row$expected_sha256[[1]],
    checksum_policy = if (source_row$access[[1]] %in% c("direct", "zip")) {
      "enforced_unchanged_asset"
    } else {
      "record_generated_subset"
    },
    pipeline_version = as.integer(pipeline$version),
    pipeline_config_sha256 = pipeline_config_sha256,
    raster_registry_sha256 = raster_registry_sha256,
    raster_processing_code_sha256 = raster_processing_code_sha256,
    raster_download_script_sha256 = raster_download_script_sha256,
    stringsAsFactors = FALSE
  )
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
cat("Download manifest:", repo_relative_path(manifest_path, repo_root), "\n")
