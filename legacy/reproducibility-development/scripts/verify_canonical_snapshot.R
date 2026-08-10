# Verify a restored canonical analysis-input snapshot, then materialise it.
#
# Restoring an archive is not the same as trusting it. Every member is rehashed
# against the checksums committed in the descriptor, and the structural checks
# applied at publication time are applied again here, so a corrupted or
# substituted input fails immediately and loudly rather than halfway through a
# multi-hour pipeline.
#
# With --materialise the verified inputs are then placed where the locked
# publication runner expects them, which is what removes the pipeline's
# dependence on one machine's directory layout.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")
hb_require_packages(c("terra", "digest", "jsonlite"))

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
descriptor_path <- arg_value("--descriptor", "inputs/canonical_snapshot.json")
root <- arg_value("--root", "reproduction_inputs/snapshot")
report_dir <- arg_value("--report-dir", "reproducibility")
materialise <- hb_as_bool(arg_value("--materialise", "false"))
mlit_target <- arg_value("--mlit-cache", "reproduction_inputs/mlit_l03_2021")
did_target <- arg_value("--did-cache", "reproduction_inputs/mlit_did_2015")

if (!file.exists(descriptor_path)) {
  stop(
    "Snapshot descriptor not found: ", descriptor_path, ".\n",
    "  expected: a committed descriptor naming the release tag, asset and ",
    "per-member checksums.\n",
    "  remediation: run the raw-data reconstruction workflow, then commit the ",
    "descriptor it prints.",
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
  stop(
    "Restored snapshot has no SNAPSHOT_MANIFEST.csv at ", root, ".\n",
    "  expected: the archive published by the reconstruction workflow.\n",
    "  remediation: check that the release asset downloaded and extracted.",
    call. = FALSE
  )
}
archived <- utils::read.csv(
  archive_manifest, check.names = FALSE, stringsAsFactors = FALSE
)

failures <- character()
if (!setequal(archived$path, contents$path)) {
  failures <- c(failures, paste0(
    "snapshot contents differ from the committed descriptor: only-in-archive=",
    paste(utils::head(setdiff(archived$path, contents$path), 10), collapse = ","),
    "; only-in-descriptor=",
    paste(utils::head(setdiff(contents$path, archived$path), 10), collapse = ",")
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
      detail <- paste0(
        "expected ", contents$sha256[[index]], " observed ", observed
      )
    } else {
      detail <- observed
    }
  }
  if (status == "FAIL") failures <- c(failures, paste0(relative, ": ", detail))
  rows[[length(rows) + 1L]] <- data.frame(
    path = relative, status = status, detail = detail,
    role = contents$role[[index]],
    component = contents$component[[index]],
    source = contents$source[[index]],
    stringsAsFactors = FALSE
  )
}
verification <- do.call(rbind, rows)

# ---------------------------------------------------------------------------
# Structural re-checks on the analysis-ready tables.
# ---------------------------------------------------------------------------
analysis_results <- file.path(root, "analysis_inputs", "results")
observations_path <- file.path(
  analysis_results, "ecological_v11_pigmentation_hurdle",
  "analysis_data_pigmentation_hurdle.csv"
)
cells_path <- file.path(
  analysis_results, "ecological_v15_multiscale_hotspots",
  "multiscale_hotspot_cells_1km.csv"
)
expect_columns <- function(path, required, label) {
  if (!file.exists(path)) {
    failures <<- c(failures, paste0("missing ", label, ": ", path))
    return(invisible(NULL))
  }
  table <- utils::read.csv(
    path, check.names = FALSE, stringsAsFactors = FALSE, nrows = 5L
  )
  absent <- setdiff(required, names(table))
  if (length(absent)) {
    failures <<- c(failures, paste0(
      label, " is missing columns: ", paste(absent, collapse = ", ")
    ))
  }
  invisible(NULL)
}
expect_columns(
  observations_path,
  c("observation_id", "exact_site_id", "longitude", "latitude", "x_km", "y_km",
    "pigmented_mixture50", "pigmented_high_confidence", "colour_a", "DOY",
    "bee_ardens", "bee_diversus", "bee_beaticola", "bee_consobrinus",
    "bee_honshuensis"),
  "phenotype analysis table"
)
expect_columns(
  cells_path,
  c("exact_site_id", "n_pigmented", "n_observations", "spatial_fold",
    "broad50km_pc1", "broad50km_pc2", "within50km_pc1", "within50km_pc2"),
  "1-km cell table"
)

worldpop <- file.path(
  root, "analysis_inputs", "rasters", "population_count_Japan_crop.tif"
)
if (!file.exists(worldpop)) {
  failures <- c(failures, "missing WorldPop raster in the snapshot")
} else {
  described <- terra::crs(terra::rast(worldpop), describe = TRUE)
  code <- paste0(described$authority, ":", described$code)
  if (!identical(code, "EPSG:4326")) {
    failures <- c(failures, paste0(
      "population_count_Japan_crop.tif is on ", code, " rather than EPSG:4326"
    ))
  }
}

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
rp_write_csv_atomic(
  verification, file.path(report_dir, "snapshot_verification.csv")
)
rp_write_manifest(
  data.frame(
    path = contents$path, bytes = contents$bytes, sha256 = contents$sha256,
    role = contents$role, note = contents$component, stringsAsFactors = FALSE
  ),
  file.path(report_dir, "canonical_input_manifest.csv")
)
if (!is.null(descriptor$components)) {
  rp_write_csv_atomic(
    descriptor$components,
    file.path(report_dir, "external_source_manifest.csv")
  )
}

if (length(failures)) {
  stop(
    "Canonical snapshot verification failed:\n  ",
    paste(failures, collapse = "\n  "), call. = FALSE
  )
}
cat("Canonical snapshot verified: ", nrow(verification), " members, tag ",
    descriptor$release_tag, "\n", sep = "")

# ---------------------------------------------------------------------------
# Materialise the verified inputs into the locations the locked runner uses.
# ---------------------------------------------------------------------------
if (materialise) {
  copy_tree <- function(from, to) {
    if (!dir.exists(from)) return(invisible(0L))
    files <- list.files(from, recursive = TRUE, all.files = FALSE)
    for (relative in files) {
      destination <- file.path(to, relative)
      dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(file.path(from, relative), destination, overwrite = TRUE)) {
        stop("Could not materialise ", relative, call. = FALSE)
      }
    }
    length(files)
  }
  placed <- c(
    results = copy_tree(analysis_results, "results"),
    mlit_cache = copy_tree(
      file.path(root, "analysis_inputs", "mlit_l03_2021"), mlit_target
    ),
    did_cache = copy_tree(
      file.path(root, "analysis_inputs", "mlit_did_2015"), did_target
    )
  )
  cat("Materialised inputs: ",
      paste(names(placed), placed, sep = "=", collapse = ", "), "\n", sep = "")
}
