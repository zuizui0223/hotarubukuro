#!/usr/bin/env Rscript

# Build the single landscape covariate used by the Broad model audit.
# The former human-candidate pipeline calculated this value as a side effect;
# keeping it here makes the Broad audit independent of superseded candidate,
# DID and random-forest analyses.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

observations_path <- arg_value(
  "--observations",
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
)
mlit_dir <- arg_value(
  "--mlit-dir", "results/public_rasters/mlit_human_forest_edge_2021"
)
output_path <- arg_value(
  "--output", "results/broad_landscape_context/forest_fraction_context.csv"
)

if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required", call. = FALSE)
if (!file.exists(observations_path)) stop("Missing observations: ", observations_path, call. = FALSE)
lookup_path <- file.path(mlit_dir, "human_forest_edge_1km_cells.csv")
template_path <- file.path(mlit_dir, "mlit_human_forest_edge_1km.tif")
if (!file.exists(lookup_path) || !file.exists(template_path)) {
  stop("Missing MLIT forest-context products in ", mlit_dir, call. = FALSE)
}

observations <- utils::read.csv(
  observations_path, check.names = FALSE, stringsAsFactors = FALSE
)
lookup <- utils::read.csv(lookup_path, check.names = FALSE, stringsAsFactors = FALSE)
required_observation <- c("exact_site_id", "longitude", "latitude", "x_km", "y_km")
required_lookup <- c("longitude", "latitude", "forest_fraction")
missing_observation <- setdiff(required_observation, names(observations))
missing_lookup <- setdiff(required_lookup, names(lookup))
if (length(missing_observation) || length(missing_lookup)) {
  stop(
    "Landscape-context inputs are incomplete: observation=",
    paste(missing_observation, collapse = ","), "; lookup=",
    paste(missing_lookup, collapse = ","), call. = FALSE
  )
}

raster <- terra::rast(template_path)
coordinates <- as.matrix(observations[, c("longitude", "latitude")])
raster_cell <- terra::cellFromXY(raster, coordinates)
raster_center <- terra::xyFromCell(raster, raster_cell)
observation_key <- paste(round(raster_center[, 1L], 7), round(raster_center[, 2L], 7))
lookup_key <- paste(round(as.numeric(lookup$longitude), 7), round(as.numeric(lookup$latitude), 7))
lookup_index <- match(observation_key, lookup_key)
forest_fraction <- as.numeric(lookup$forest_fraction[lookup_index])

mean_finite <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

original <- data.frame(
  exact_site_id = as.character(observations$exact_site_id),
  forest_fraction = forest_fraction,
  stringsAsFactors = FALSE
)
cell_id <- paste0(
  "cell-1km-", floor(as.numeric(observations$x_km)), "_",
  floor(as.numeric(observations$y_km))
)
grid <- data.frame(
  exact_site_id = cell_id,
  forest_fraction = forest_fraction,
  stringsAsFactors = FALSE
)
combined <- rbind(original, grid)
groups <- split(seq_len(nrow(combined)), combined$exact_site_id)
out <- do.call(rbind, lapply(groups, function(index) {
  data.frame(
    exact_site_id = combined$exact_site_id[index[[1L]]],
    forest_fraction = mean_finite(combined$forest_fraction[index]),
    stringsAsFactors = FALSE
  )
}))
rownames(out) <- NULL
if (!any(is.finite(out$forest_fraction))) stop("No forest-fraction values were recovered", call. = FALSE)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, output_path, row.names = FALSE)
cat(
  "Built Broad forest-fraction context for ", nrow(out),
  " original/grid identifiers; matched observations=", sum(is.finite(forest_fraction)),
  "/", nrow(observations), ".\n", sep = ""
)
