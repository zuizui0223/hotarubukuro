#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(terra))
options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) > 0) args[[1]] else "Data_S1.csv"
env_dir <- if (length(args) > 1) args[[2]] else "data/processed/rasters"
out_dir <- if (length(args) > 2) args[[3]] else "results/environment_extraction_diagnostic"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fail <- function(...) stop(..., call. = FALSE)
`%||%` <- function(x, y) if (is.null(x)) y else x

required_cols <- c("observation_id", "latitude", "longitude")
d <- read.csv(input, check.names = FALSE, fileEncoding = "UTF-8-BOM")
missing_cols <- setdiff(required_cols, names(d))
if (length(missing_cols)) fail("Missing input columns: ", paste(missing_cols, collapse = ", "))
for (nm in c("latitude", "longitude")) d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
valid <- is.finite(d$latitude) & is.finite(d$longitude) &
  d$latitude >= 20 & d$latitude <= 50 & d$longitude >= 120 & d$longitude <= 150
d <- d[valid, , drop = FALSE]
if (!nrow(d)) fail("No valid occurrence coordinates")

files <- c(
  chelsa_bio05 = "chelsa_bio05.tif",
  chelsa_bio10 = "chelsa_bio10.tif",
  chelsa_gdd5 = "chelsa_gdd5.tif"
)
paths <- file.path(env_dir, files)
if (any(!file.exists(paths))) fail("Missing raster(s): ", paste(paths[!file.exists(paths)], collapse = ", "))

xy <- as.matrix(d[c("longitude", "latitude")])
storage.mode(xy) <- "double"
points_ll <- terra::vect(d, geom = c("longitude", "latitude"), crs = "EPSG:4326", keepgeom = TRUE)

summaries <- list()
extracted <- list()
for (i in seq_along(paths)) {
  nm <- names(paths)[[i]]
  path <- paths[[i]]
  r <- terra::rast(path)
  ex <- as.vector(terra::ext(r))
  rs <- terra::res(r)
  raster_crs <- terra::crs(r)
  projected_points <- tryCatch(
    if (terra::same.crs(points_ll, r)) points_ll else terra::project(points_ll, terra::crs(r)),
    error = function(e) structure(NULL, error = conditionMessage(e))
  )
  overlap <- if (is.null(projected_points)) {
    rep(FALSE, nrow(d))
  } else {
    projected_xy <- terra::crds(projected_points)
    projected_xy[, 1] >= ex[[1]] & projected_xy[, 1] <= ex[[2]] &
      projected_xy[, 2] >= ex[[3]] & projected_xy[, 2] <= ex[[4]]
  }

  matrix_values <- tryCatch(
    terra::extract(r, xy),
    error = function(e) structure(NULL, error = conditionMessage(e))
  )
  vector_values <- if (is.null(projected_points)) {
    structure(NULL, error = attr(projected_points, "error"))
  } else {
    tryCatch(
      terra::extract(r, projected_points, ID = FALSE),
      error = function(e) structure(NULL, error = conditionMessage(e))
    )
  }

  matrix_n <- if (is.null(matrix_values)) 0L else nrow(matrix_values)
  matrix_finite <- if (is.null(matrix_values)) 0L else sum(is.finite(matrix_values[[1]]))
  vector_n <- if (is.null(vector_values)) 0L else nrow(vector_values)
  vector_finite <- if (is.null(vector_values)) 0L else sum(is.finite(vector_values[[1]]))

  summaries[[nm]] <- data.frame(
    layer = nm,
    path = path,
    nrow = terra::nrow(r),
    ncol = terra::ncol(r),
    xmin = ex[[1]], xmax = ex[[2]], ymin = ex[[3]], ymax = ex[[4]],
    xres = rs[[1]], yres = rs[[2]],
    crs = raster_crs,
    occurrence_count = nrow(d),
    occurrence_inside_extent = sum(overlap),
    matrix_rows = matrix_n,
    matrix_finite = matrix_finite,
    matrix_error = attr(matrix_values, "error") %||% "",
    vector_rows = vector_n,
    vector_finite = vector_finite,
    vector_error = attr(vector_values, "error") %||% "",
    stringsAsFactors = FALSE
  )

  if (!is.null(vector_values) && vector_finite > 0) {
    extracted[[nm]] <- as.numeric(vector_values[[1]])
  } else if (!is.null(matrix_values) && matrix_finite > 0) {
    extracted[[nm]] <- as.numeric(matrix_values[[1]])
  } else {
    extracted[[nm]] <- rep(NA_real_, nrow(d))
  }
}

summary_df <- do.call(rbind, summaries)
write.csv(summary_df, file.path(out_dir, "raster_extraction_diagnostic.csv"), row.names = FALSE)

values <- as.data.frame(extracted, check.names = FALSE)
values$observation_id <- d$observation_id
values$longitude <- d$longitude
values$latitude <- d$latitude
write.csv(values, file.path(out_dir, "temperature_values.csv"), row.names = FALSE)

cat("=== raster extraction diagnostic ===\n")
print(summary_df[, c("layer", "xmin", "xmax", "ymin", "ymax", "occurrence_inside_extent", "matrix_rows", "matrix_finite", "vector_rows", "vector_finite")], row.names = FALSE)

X <- values[names(files)]
cc <- complete.cases(X)
cat("Complete temperature rows:", sum(cc), "of", nrow(X), "\n")
if (sum(cc) < 50L) fail("Temperature extraction still has fewer than 50 complete rows; inspect raster_extraction_diagnostic.csv")
if (any(vapply(X[cc, , drop = FALSE], sd, numeric(1)) <= 0)) fail("At least one temperature layer has zero variance")
fit <- prcomp(X[cc, , drop = FALSE], center = TRUE, scale. = TRUE)
write.csv(data.frame(variable = rownames(fit$rotation), PC1_loading = fit$rotation[, 1]),
          file.path(out_dir, "temperature_pc1_loadings.csv"), row.names = FALSE)
cat("Temperature PCA succeeded with", sum(cc), "rows.\n")
