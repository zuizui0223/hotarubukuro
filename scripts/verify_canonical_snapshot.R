# Verify a restored canonical analysis-input snapshot before any analysis runs.
#
# Restoring an archive is not the same as trusting it. Every member is rehashed
# against the checksums committed in the descriptor, and the structural checks
# that were applied at publication time are applied again here, so a corrupted
# or substituted input fails immediately and loudly.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")
hb_require_packages(c("terra", "digest", "jsonlite"))

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
descriptor_path <- arg_value("--descriptor", "inputs/canonical_snapshot.json")
root <- arg_value("--root", "reproduction_inputs/snapshot")
report_dir <- arg_value("--report-dir", "reproducibility")

if (!file.exists(descriptor_path)) {
  stop(
    "Snapshot descriptor not found: ", descriptor_path, ". ",
    "The canonical workflow cannot start until the raw-data reconstruction ",
    "workflow has published a snapshot and its checksums have been committed.",
    call. = FALSE
  )
}
descriptor <- jsonlite::fromJSON(descriptor_path, simplifyDataFrame = TRUE)
contents <- descriptor$contents
if (is.null(contents) || !nrow(contents)) {
  stop("Snapshot descriptor lists no contents.", call. = FALSE)
}

archive_manifest <- file.path(root, "SNAPSHOT_MANIFEST.csv")
if (!file.exists(archive_manifest)) {
  stop("Restored snapshot has no SNAPSHOT_MANIFEST.csv at ", root, call. = FALSE)
}
archived <- utils::read.csv(
  archive_manifest, check.names = FALSE, stringsAsFactors = FALSE
)

failures <- character()
if (!setequal(archived$path, contents$path)) {
  failures <- c(failures, paste0(
    "snapshot contents differ from the committed descriptor: only-in-archive=",
    paste(setdiff(archived$path, contents$path), collapse = ","),
    "; only-in-descriptor=",
    paste(setdiff(contents$path, archived$path), collapse = ",")
  ))
}

rows <- list()
for (index in seq_len(nrow(contents))) {
  relative <- contents$path[[index]]
  path <- file.path(root, relative)
  status <- "PASS"
  detail <- ""
  if (!file.exists(path)) {
    status <- "FAIL"
    detail <- "member missing from the restored archive"
  } else {
    observed <- rp_sha256(path)
    if (!identical(observed, contents$sha256[[index]])) {
      status <- "FAIL"
      detail <- paste0("expected ", contents$sha256[[index]],
                       " observed ", observed)
    } else {
      detail <- observed
    }
  }
  if (status == "FAIL") {
    failures <- c(failures, paste0(relative, ": ", detail))
  }
  rows[[length(rows) + 1L]] <- data.frame(
    path = relative, status = status, detail = detail,
    role = contents$role[[index]],
    source = contents$source[[index]],
    source_url = contents$source_url[[index]],
    stringsAsFactors = FALSE
  )
}
verification <- do.call(rbind, rows)

observations_path <- file.path(
  root, "analysis_inputs", "analysis_data_pigmentation_hurdle.csv"
)
if (file.exists(observations_path)) {
  observations <- utils::read.csv(
    observations_path, check.names = FALSE, stringsAsFactors = FALSE
  )
  required <- c(
    "observation_id", "exact_site_id", "longitude", "latitude", "x_km", "y_km",
    "pigmented_mixture50", "pigmented_high_confidence", "colour_a", "DOY",
    "bee_ardens", "bee_diversus", "bee_beaticola", "bee_consobrinus",
    "bee_honshuensis"
  )
  missing <- setdiff(required, names(observations))
  if (length(missing)) {
    failures <- c(failures, paste0(
      "phenotype analysis table is missing columns: ",
      paste(missing, collapse = ", ")
    ))
  }
}

raster_dir <- file.path(root, "analysis_inputs", "rasters")
expected_rasters <- c(
  "elevation_Japan_crop.tif", "bio10_Japan_crop_30s.tif",
  "bio12_Japan_crop_30s.tif", "RSDS_Japan_crop_30s.tif",
  "population_count_Japan_crop.tif"
)
for (filename in expected_rasters) {
  path <- file.path(raster_dir, filename)
  if (!file.exists(path)) {
    failures <- c(failures, paste0("missing environment layer: ", filename))
    next
  }
  layer <- terra::rast(path)
  described <- terra::crs(layer, describe = TRUE)
  code <- paste0(described$authority, ":", described$code)
  if (!identical(code, "EPSG:4326")) {
    failures <- c(failures, paste0(filename, " is on ", code,
                                   " rather than EPSG:4326"))
  }
}

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
rp_write_csv_atomic(
  verification, file.path(report_dir, "snapshot_verification.csv")
)

if (length(failures)) {
  stop(
    "Canonical snapshot verification failed:\n  ",
    paste(failures, collapse = "\n  "), call. = FALSE
  )
}
cat("Canonical snapshot verified: ", nrow(verification), " members, tag ",
    descriptor$release_tag, "\n", sep = "")
