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
output <- arg_value("--output", "reproducibility/bombus_sdm_rebuild_comparison.csv")
tolerance <- as.numeric(arg_value("--tolerance", "1e-12"))
if (is.null(a_dir) || is.null(b_dir)) stop("--a and --b are required.", call. = FALSE)

read_csv <- function(root, name) {
  path <- file.path(root, name)
  if (!file.exists(path)) stop("Missing rebuild file: ", path, call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE)
}

canonicalize_df <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  x <- x[, sort(names(x)), drop = FALSE]
  row_key <- do.call(paste, c(lapply(x, function(v) {
    if (is.numeric(v)) format(v, digits = 17, scientific = TRUE, trim = TRUE) else as.character(v)
  }), sep = "\r"))
  x[order(row_key), , drop = FALSE]
}

selected_a <- canonicalize_df(read_csv(a_dir, "ENMeval_selected_models.csv"))
selected_b <- canonicalize_df(read_csv(b_dir, "ENMeval_selected_models.csv"))
seed_a <- canonicalize_df(read_csv(a_dir, "seed_registry.csv"))
seed_b <- canonicalize_df(read_csv(b_dir, "seed_registry.csv"))

selected_equal <- isTRUE(all.equal(selected_a, selected_b, tolerance = 0, check.attributes = FALSE))
seed_equal <- identical(seed_a, seed_b)

species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
rows <- list(
  data.frame(
    component = "selected_model_table",
    status = if (selected_equal) "PASS" else "FAIL",
    max_abs_difference = NA_real_,
    detail = if (selected_equal) "exact after canonical row/column ordering" else "selected model outputs differ",
    stringsAsFactors = FALSE
  ),
  data.frame(
    component = "seed_registry",
    status = if (seed_equal) "PASS" else "FAIL",
    max_abs_difference = NA_real_,
    detail = if (seed_equal) "exact" else "seed registries differ",
    stringsAsFactors = FALSE
  )
)

for (sh in species) {
  a_path <- file.path(a_dir, "predictions", paste0(sh, ".tif"))
  b_path <- file.path(b_dir, "predictions", paste0(sh, ".tif"))
  if (!file.exists(a_path) || !file.exists(b_path)) {
    rows[[length(rows) + 1L]] <- data.frame(
      component = paste0("prediction_", sh), status = "FAIL",
      max_abs_difference = NA_real_, detail = "prediction file missing",
      stringsAsFactors = FALSE
    )
    next
  }
  a <- terra::rast(a_path)
  b <- terra::rast(b_path)
  same_geom <- isTRUE(terra::compareGeom(a, b, stopOnError = FALSE))
  max_diff <- if (same_geom) {
    as.numeric(terra::global(abs(a - b), "max", na.rm = TRUE)[1, 1])
  } else {
    Inf
  }
  pass <- same_geom && is.finite(max_diff) && max_diff <= tolerance
  rows[[length(rows) + 1L]] <- data.frame(
    component = paste0("prediction_", sh),
    status = if (pass) "PASS" else "FAIL",
    max_abs_difference = max_diff,
    detail = paste0("same_geometry=", same_geom, "; tolerance=", format(tolerance, scientific = TRUE)),
    stringsAsFactors = FALSE
  )
}

report <- do.call(rbind, rows)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(report, output, na = "")
print(report, row.names = FALSE)
if (any(report$status != "PASS")) {
  stop("Seeded Bombus SDM repeated-build check failed.", call. = FALSE)
}
cat("Seeded Bombus SDM repeated-build check passed.\n")
