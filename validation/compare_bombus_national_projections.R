#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) return(args[[i + 1L]])
  prefix <- paste0(flag, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

required <- c("terra", "readr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "), call. = FALSE)

a_dir <- arg_value("--a")
b_dir <- arg_value("--b")
output <- arg_value("--output", "reproducibility/bombus_national_projection_comparison.csv")
tolerance <- as.numeric(arg_value("--tolerance", "1e-12"))
if (is.null(a_dir) || is.null(b_dir)) stop("--a and --b are required.", call. = FALSE)

species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
rows <- vector("list", length(species))
for (i in seq_along(species)) {
  sh <- species[[i]]
  a_path <- file.path(a_dir, "predictions_national", paste0(sh, ".tif"))
  b_path <- file.path(b_dir, "predictions_national", paste0(sh, ".tif"))
  if (!file.exists(a_path) || !file.exists(b_path)) {
    rows[[i]] <- data.frame(
      component = paste0("national_prediction_", sh),
      status = "FAIL", max_abs_difference = NA_real_,
      detail = "national prediction file missing",
      stringsAsFactors = FALSE
    )
    next
  }
  a <- terra::rast(a_path)
  b <- terra::rast(b_path)
  same_geom <- isTRUE(terra::compareGeom(a, b, stopOnError = FALSE))
  max_diff <- if (same_geom) {
    as.numeric(terra::global(abs(a - b), "max", na.rm = TRUE)[1, 1])
  } else Inf
  pass <- same_geom && is.finite(max_diff) && max_diff <= tolerance
  rows[[i]] <- data.frame(
    component = paste0("national_prediction_", sh),
    status = if (pass) "PASS" else "FAIL",
    max_abs_difference = max_diff,
    detail = paste0("same_geometry=", same_geom, "; tolerance=", format(tolerance, scientific = TRUE)),
    stringsAsFactors = FALSE
  )
}

coverage_a <- file.path(a_dir, "flower_prediction_coverage_national_summary.csv")
coverage_b <- file.path(b_dir, "flower_prediction_coverage_national_summary.csv")
if (file.exists(coverage_a) && file.exists(coverage_b)) {
  ca <- readr::read_csv(coverage_a, show_col_types = FALSE)
  cb <- readr::read_csv(coverage_b, show_col_types = FALSE)
  same_cov <- isTRUE(all.equal(ca, cb, tolerance = 0, check.attributes = FALSE))
  rows[[length(rows) + 1L]] <- data.frame(
    component = "national_flower_coverage",
    status = if (same_cov) "PASS" else "FAIL",
    max_abs_difference = NA_real_,
    detail = if (same_cov) "exact" else "coverage summaries differ",
    stringsAsFactors = FALSE
  )
}

report <- do.call(rbind, rows)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(report, output, na = "")
print(report, row.names = FALSE)
if (any(report$status != "PASS")) {
  stop("Repeated national Bombus projection check failed.", call. = FALSE)
}
cat("Repeated national Bombus projection check passed.\n")
