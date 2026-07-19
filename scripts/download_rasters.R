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
    "COG and SoilGrids VRT sources are cached only for the configured study extent.",
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
sources <- select_sources(read_raster_sources(registry_path), split_source_ids(args$only))
bbox <- pipeline_bbox(pipeline)
force <- if (is.null(args$force)) FALSE else as_flag(args$force, "force")
dry_run <- if (is.null(args$dry_run)) FALSE else as_flag(args$dry_run, "dry-run")

if (!nrow(sources)) stop("No enabled raster sources selected.", call. = FALSE)
cat("Selected sources:\n")
cat(paste0("- ", sources$source_id, " [", sources$access, "] -> ",
           file.path(cache_dir, sources$cache_name), collapse = "\n"), "\n")
if (dry_run) quit(status = 0)

rows <- vector("list", nrow(sources))
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
  rows[[index]] <- data.frame(
    source_id = source_row$source_id[[1]], provider = source_row$provider[[1]],
    dataset_version = source_row$dataset_version[[1]], access = source_row$access[[1]],
    source_url = source_row$url[[1]], source_page = source_row$source_page[[1]],
    cache_path = repo_relative_path(path, repo_root),
    cache_sha256 = sha256_file(path), cache_bytes = unname(file.info(path)$size),
    archive_path = if (has_archive) repo_relative_path(archive_path, repo_root) else NA_character_,
    archive_sha256 = if (has_archive) sha256_file(archive_path) else NA_character_,
    archive_bytes = if (has_archive) unname(file.info(archive_path)$size) else NA_real_,
    cached_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    expected_sha256 = source_row$expected_sha256[[1]],
    stringsAsFactors = FALSE
  )
}
new_manifest <- do.call(rbind, rows)
if (file.exists(manifest_path)) {
  old_manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
  old_manifest <- old_manifest[!old_manifest$source_id %in% new_manifest$source_id, , drop = FALSE]
  new_manifest <- rbind(old_manifest, new_manifest)
}
new_manifest <- new_manifest[order(new_manifest$source_id), , drop = FALSE]
write_csv_atomic(new_manifest, manifest_path)
cat("Download manifest:", repo_relative_path(manifest_path, repo_root), "\n")
