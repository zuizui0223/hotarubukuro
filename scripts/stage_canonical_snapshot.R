# Assemble and validate the canonical analysis-input snapshot.
#
# The snapshot is the versioned, immutable, checksummed boundary that the
# canonical analysis workflow starts from. It contains only analysis-ready
# inputs: the two-part phenotype analysis table and the four multiscale
# environment layers plus the population layer that the 1-km cell table is
# built from. Everything in it is validated here, before publication, so that a
# clean canonical run can trust it.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")
hb_require_packages(c("terra", "digest", "jsonlite"))

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

observations_path <- arg_value(
  "--observations",
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
)
raster_cache <- arg_value("--raster-cache", "reproduction_inputs/public_environment_cache")
worldpop_path <- arg_value("--worldpop", file.path(raster_cache, "population_count_Japan_crop.tif"))
staging <- arg_value("--staging", "snapshot_staging")
snapshot_id <- arg_value("--snapshot-id", "analysis-input-snapshot-v1")
release_tag <- arg_value("--release-tag", snapshot_id)
asset_name <- arg_value("--asset-name", paste0(snapshot_id, ".tar.gz"))
descriptor_path <- arg_value("--descriptor", "inputs/canonical_snapshot.json")
download_manifest <- arg_value("--download-manifest", "data/processed/raster_download_manifest.csv")
processed_manifest <- arg_value("--processed-manifest", "data/processed/raster_manifest.csv")
worldpop_url <- arg_value(
  "--worldpop-url",
  "https://data.worldpop.org/GIS/Population/Global_2000_2020_1km/2020/JPN/jpn_ppp_2020_1km_Aggregated.tif"
)

# The four environment layers the 1-km cell table decomposes at multiple scales,
# named exactly as multiscale_environment_sources() looks them up.
raster_layers <- c(
  elevation = "elevation_Japan_crop.tif",
  temperature = "bio10_Japan_crop_30s.tif",
  precipitation = "bio12_Japan_crop_30s.tif",
  radiation = "RSDS_Japan_crop_30s.tif"
)

find_one <- function(directory, filename) {
  hits <- list.files(
    directory, pattern = paste0("^", gsub(".", "\\.", filename, fixed = TRUE), "$"),
    recursive = TRUE, full.names = TRUE
  )
  if (length(hits) != 1L) {
    stop(
      "Expected exactly one '", filename, "' under ", directory,
      "; found ", length(hits), ".", call. = FALSE
    )
  }
  hits[[1L]]
}

# ---------------------------------------------------------------------------
# Validate the phenotype analysis table.
# ---------------------------------------------------------------------------
if (!file.exists(observations_path)) {
  stop("Missing phenotype analysis table: ", observations_path, call. = FALSE)
}
observations <- utils::read.csv(
  observations_path, check.names = FALSE, stringsAsFactors = FALSE
)
# The columns the downstream 1-km cell table and neighbourhood graph are built
# from. transition_required_columns() is the locked list; observation_id keeps
# the phenotype record auditable back to Data_S1.csv.
required_observation_columns <- c(
  "observation_id", "exact_site_id", "longitude", "latitude", "x_km", "y_km",
  "pigmented_mixture50", "pigmented_high_confidence", "colour_a", "DOY",
  "bee_ardens", "bee_diversus", "bee_beaticola", "bee_consobrinus",
  "bee_honshuensis"
)
missing_columns <- setdiff(required_observation_columns, names(observations))
if (length(missing_columns)) {
  stop(
    "Phenotype analysis table is missing required columns: ",
    paste(missing_columns, collapse = ", "), call. = FALSE
  )
}
if (!nrow(observations)) {
  stop("Phenotype analysis table is empty.", call. = FALSE)
}
if (anyNA(observations$longitude) || anyNA(observations$latitude)) {
  stop("Phenotype analysis table has missing coordinates.", call. = FALSE)
}
if (min(observations$longitude) < 122 || max(observations$longitude) > 154 ||
    min(observations$latitude) < 24 || max(observations$latitude) > 46) {
  stop("Phenotype coordinates fall outside the Japanese study window.",
       call. = FALSE)
}

# ---------------------------------------------------------------------------
# Validate the raster layers: CRS, resolution, extent, and value range.
# ---------------------------------------------------------------------------
raster_paths <- vapply(
  raster_layers, function(filename) find_one(raster_cache, filename),
  character(1)
)
if (!file.exists(worldpop_path)) {
  stop("Missing population raster: ", worldpop_path, call. = FALSE)
}

raster_checks <- list()
validate_raster <- function(path, label, expect_arcsec = TRUE) {
  layer <- terra::rast(path)
  crs_text <- terra::crs(layer, describe = TRUE)
  code <- paste0(crs_text$authority, ":", crs_text$code)
  resolution <- terra::res(layer)
  extent <- as.vector(terra::ext(layer))
  values_range <- terra::minmax(layer)
  if (!identical(code, "EPSG:4326")) {
    stop(label, " is not on EPSG:4326 (found ", code, ").", call. = FALSE)
  }
  if (expect_arcsec && abs(resolution[[1L]] - 1 / 120) > 1e-6) {
    stop(
      label, " is not on the 30 arc-second grid (resolution ",
      signif(resolution[[1L]], 8), " degrees).", call. = FALSE
    )
  }
  if (extent[[1L]] > 128 || extent[[2L]] < 143 ||
      extent[[3L]] > 30 || extent[[4L]] < 42) {
    stop(
      label, " does not cover the study extent 128-143E, 30-42N (found ",
      paste(signif(extent, 8), collapse = ", "), ").", call. = FALSE
    )
  }
  if (!all(is.finite(values_range))) {
    stop(label, " has no finite values.", call. = FALSE)
  }
  raster_checks[[length(raster_checks) + 1L]] <<- data.frame(
    layer = label,
    crs = code,
    resolution_degrees = resolution[[1L]],
    ncol = terra::ncol(layer),
    nrow = terra::nrow(layer),
    xmin = extent[[1L]], xmax = extent[[2L]],
    ymin = extent[[3L]], ymax = extent[[4L]],
    minimum = values_range[[1L]], maximum = values_range[[2L]],
    stringsAsFactors = FALSE
  )
  invisible(TRUE)
}

for (name in names(raster_paths)) {
  validate_raster(raster_paths[[name]], raster_layers[[name]])
}
validate_raster(worldpop_path, basename(worldpop_path), expect_arcsec = FALSE)
raster_validation <- do.call(rbind, raster_checks)

# ---------------------------------------------------------------------------
# Stage the snapshot.
# ---------------------------------------------------------------------------
unlink(staging, recursive = TRUE)
dir.create(file.path(staging, "analysis_inputs", "rasters"), recursive = TRUE)
dir.create(file.path(staging, "provenance"), recursive = TRUE)

staged <- list()
stage_file <- function(source, relative, role, source_name, source_url,
                       retrieved_utc) {
  destination <- file.path(staging, relative)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(source, destination, overwrite = TRUE)) {
    stop("Could not stage ", source, call. = FALSE)
  }
  staged[[length(staged) + 1L]] <<- data.frame(
    path = relative,
    bytes = as.numeric(file.info(destination)$size),
    sha256 = rp_sha256(destination),
    role = role,
    source = source_name,
    source_url = source_url,
    retrieved_utc = retrieved_utc,
    stringsAsFactors = FALSE
  )
  invisible(destination)
}

read_optional_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}
downloads <- read_optional_csv(download_manifest)
processed <- read_optional_csv(processed_manifest)

# Map a prepared raster filename back to the registry row that produced it, so
# the snapshot carries the real provider URL and retrieval time rather than a
# local path.
provenance_for <- function(filename) {
  default <- list(
    source = "unrecorded", url = NA_character_, retrieved = NA_character_
  )
  if (is.null(processed)) return(default)
  aliases <- c(
    elevation_Japan_crop.tif = "elevation_30s.tif",
    bio10_Japan_crop_30s.tif = "chelsa_bio10.tif",
    bio12_Japan_crop_30s.tif = "chelsa_bio12.tif",
    RSDS_Japan_crop_30s.tif = "chelsa_rsdsmean.tif"
  )
  output_name <- aliases[[filename]]
  if (is.null(output_name)) return(default)
  row <- processed[basename(processed$processed_path) == output_name, , drop = FALSE]
  if (!nrow(row)) return(default)
  source_id <- row$source_id[[1L]]
  download_row <- if (!is.null(downloads)) {
    downloads[downloads$source_id == source_id, , drop = FALSE]
  } else {
    NULL
  }
  list(
    source = paste0(row$provider[[1L]], " ", row$dataset_version[[1L]],
                    " (", source_id, ")"),
    url = if (!is.null(download_row) && nrow(download_row)) {
      download_row$source_url[[1L]]
    } else {
      NA_character_
    },
    retrieved = if (!is.null(download_row) && nrow(download_row)) {
      download_row$cached_at_utc[[1L]]
    } else {
      NA_character_
    }
  )
}

stage_file(
  observations_path, "analysis_inputs/analysis_data_pigmentation_hurdle.csv",
  "derived_checkpoint",
  "hotarubukuro two-part phenotype stage (01_phenotype)",
  NA_character_, format(Sys.time(), tz = "UTC", usetz = TRUE)
)
for (name in names(raster_paths)) {
  filename <- raster_layers[[name]]
  provenance <- provenance_for(filename)
  stage_file(
    raster_paths[[name]], file.path("analysis_inputs/rasters", filename),
    "immutable_input", provenance$source, provenance$url, provenance$retrieved
  )
}
stage_file(
  worldpop_path, file.path("analysis_inputs/rasters", basename(worldpop_path)),
  "immutable_input",
  "WorldPop Global 2000-2020 1km, Japan 2020, people per cell",
  worldpop_url, format(file.info(worldpop_path)$mtime, tz = "UTC", usetz = TRUE)
)

for (path in c(download_manifest, processed_manifest)) {
  if (file.exists(path)) {
    file.copy(path, file.path(staging, "provenance", basename(path)),
              overwrite = TRUE)
  }
}
rp_write_csv_atomic(
  raster_validation, file.path(staging, "provenance", "raster_validation.csv")
)

manifest <- do.call(rbind, staged)
manifest <- manifest[order(manifest$path), , drop = FALSE]
rownames(manifest) <- NULL
rp_write_csv_atomic(manifest, file.path(staging, "SNAPSHOT_MANIFEST.csv"))

rp_write_lines_atomic(
  c(
    paste0("# Canonical analysis-input snapshot: ", snapshot_id),
    "",
    paste0("Built from commit ", rp_git_commit(), " on ",
           format(Sys.time(), tz = "UTC", usetz = TRUE), "."),
    "",
    "This archive is the immutable starting point of the canonical analysis",
    "workflow. It contains analysis-ready inputs only:",
    "",
    "- `analysis_inputs/analysis_data_pigmentation_hurdle.csv`: the two-part",
    "  phenotype analysis table produced by the 01_phenotype stage from the",
    "  versioned `Data_S1.csv` colour measurements.",
    "- `analysis_inputs/rasters/`: the four environment layers that the 1-km",
    "  cell table decomposes at 25/50/100 km, plus the WorldPop population",
    "  layer used for population context.",
    "",
    "`SNAPSHOT_MANIFEST.csv` records the SHA-256, byte size, provider, source",
    "URL, and retrieval time of every member. `provenance/` carries the raster",
    "download and preparation manifests from the raw-data reconstruction that",
    "produced these layers.",
    "",
    "Downstream stages regenerate the 1-km cell table, the cross-fitted natural",
    "predictive maps, and the local colour-state asymmetry diagnostic. Nothing",
    "in this archive is a result; nothing in it may be edited by hand."
  ),
  file.path(staging, "SNAPSHOT_README.md")
)

descriptor <- list(
  snapshot_id = snapshot_id,
  release_tag = release_tag,
  asset_name = asset_name,
  asset_sha256 = "",
  built_from_commit = rp_git_commit(),
  built_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  contents = manifest
)
dir.create(dirname(descriptor_path), recursive = TRUE, showWarnings = FALSE)
rp_write_atomic(descriptor_path, function(temporary) {
  writeLines(
    jsonlite::toJSON(descriptor, auto_unbox = TRUE, pretty = TRUE, na = "null"),
    temporary, useBytes = TRUE
  )
})

cat("Staged canonical snapshot at ", normalizePath(staging), "\n", sep = "")
print(manifest[c("path", "bytes", "sha256")], row.names = FALSE)
