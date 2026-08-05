# Audit the Bombus raster extraction itself.
#
# The completeness audit established that all 54 reconstruction exclusions are
# driven by Bombus_W being NA, and that no environmental variable causes an
# exclusion on its own. That narrows the question to one implementation detail:
# how the fixed historical prediction surfaces are sampled at the fixed
# Data_S1 coordinates.
#
# This records, per observation and per species, whether the point is inside the
# raster extent, which cell contains it, whether that cell is NA, whether any of
# the four cells in the bilinear stencil is NA, and what each extraction method
# returns. It changes nothing and recommends nothing; it measures.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")
hb_require_packages(c("terra"))

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
observations_csv <- arg_value("--observations", "Data_S1.csv")
bombus_dir <- arg_value("--bombus-dir", "results/enmeval_aicc_reselected/predictions")
output_dir <- arg_value("--output-dir", "results/bombus_extraction_audit")
species <- c("ardens", "diversus")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

observations <- utils::read.csv(
  observations_csv, check.names = FALSE, stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
)
required <- c("observation_id", "longitude", "latitude")
absent <- setdiff(required, names(observations))
if (length(absent)) {
  stop("Observations lack: ", paste(absent, collapse = ", "), call. = FALSE)
}
coordinates <- cbind(
  as.numeric(observations$longitude), as.numeric(observations$latitude)
)

# ---------------------------------------------------------------------------
# Raster geometry, recorded before any extraction.
# ---------------------------------------------------------------------------
geometry_rows <- list()
per_species <- list()

for (name in species) {
  path <- file.path(bombus_dir, paste0(name, ".tif"))
  if (!file.exists(path)) stop("Missing surface: ", path, call. = FALSE)
  raster <- terra::rast(path)
  described <- terra::crs(raster, describe = TRUE)
  extent <- as.vector(terra::ext(raster))

  geometry_rows[[length(geometry_rows) + 1L]] <- data.frame(
    species = name,
    sha256 = rp_sha256(path),
    crs_authority = paste0(described$authority, ":", described$code),
    crs_name = described$name,
    nrow = terra::nrow(raster), ncol = terra::ncol(raster),
    xres = terra::res(raster)[[1L]], yres = terra::res(raster)[[2L]],
    xmin = extent[["xmin"]], xmax = extent[["xmax"]],
    ymin = extent[["ymin"]], ymax = extent[["ymax"]],
    n_cells = terra::ncell(raster),
    n_na_cells = terra::global(is.na(raster), "sum", na.rm = TRUE)[1, 1],
    stringsAsFactors = FALSE
  )

  inside <- coordinates[, 1] >= extent[["xmin"]] &
    coordinates[, 1] <= extent[["xmax"]] &
    coordinates[, 2] >= extent[["ymin"]] &
    coordinates[, 2] <= extent[["ymax"]]

  cell <- terra::cellFromXY(raster, coordinates)
  centre <- terra::xyFromCell(raster, cell)
  layer <- terra::values(raster, mat = FALSE)
  containing_value <- ifelse(is.na(cell), NA_real_, layer[cell])

  # The bilinear stencil is the 2x2 block of cell centres surrounding the point:
  # the containing cell plus its neighbours in the direction the point lies off
  # centre. Any NA among those four propagates into a bilinear result.
  resolution <- terra::res(raster)
  step_x <- ifelse(coordinates[, 1] >= centre[, 1], resolution[[1L]], -resolution[[1L]])
  step_y <- ifelse(coordinates[, 2] >= centre[, 2], resolution[[2L]], -resolution[[2L]])
  stencil <- cbind(
    cell,
    terra::cellFromXY(raster, cbind(centre[, 1] + step_x, centre[, 2])),
    terra::cellFromXY(raster, cbind(centre[, 1], centre[, 2] + step_y)),
    terra::cellFromXY(raster, cbind(centre[, 1] + step_x, centre[, 2] + step_y))
  )
  stencil_na <- apply(stencil, 1L, function(cells) {
    cells <- cells[!is.na(cells)]
    if (!length(cells)) return(NA)
    any(is.na(layer[cells]))
  })
  stencil_missing_cells <- rowSums(is.na(stencil))

  points <- terra::vect(
    data.frame(longitude = coordinates[, 1], latitude = coordinates[, 2]),
    geom = c("longitude", "latitude"), crs = "EPSG:4326"
  )
  bilinear <- terra::extract(raster, points, method = "bilinear")
  bilinear <- as.numeric(bilinear[, ncol(bilinear), drop = TRUE])
  simple <- terra::extract(raster, points, method = "simple")
  simple <- as.numeric(simple[, ncol(simple), drop = TRUE])

  per_species[[name]] <- data.frame(
    observation_id = observations$observation_id,
    longitude = coordinates[, 1],
    latitude = coordinates[, 2],
    species = name,
    inside_extent = inside,
    cell_index = cell,
    containing_cell_na = is.na(containing_value),
    stencil_has_na = stencil_na,
    stencil_cells_off_grid = stencil_missing_cells,
    value_simple = simple,
    value_bilinear = bilinear,
    stringsAsFactors = FALSE
  )
}

geometry <- do.call(rbind, geometry_rows)
rp_write_csv_atomic(geometry, file.path(output_dir, "raster_geometry.csv"))
cat("=== raster geometry ===\n")
print(geometry, row.names = FALSE)

detail <- do.call(rbind, per_species)
rp_write_csv_atomic(detail, file.path(output_dir, "extraction_detail.csv"))

# ---------------------------------------------------------------------------
# Bombus_W is defined only where both widespread species resolve, so the
# diagnosis is per observation across both species.
# ---------------------------------------------------------------------------
combine <- function(field, reducer) {
  values <- vapply(species, function(name) per_species[[name]][[field]],
                   per_species[[species[[1L]]]][[field]])
  apply(matrix(values, nrow = nrow(observations)), 1L, reducer)
}

bilinear_ok <- Reduce(`&`, lapply(species, function(name) {
  is.finite(per_species[[name]]$value_bilinear)
}))
simple_ok <- Reduce(`&`, lapply(species, function(name) {
  is.finite(per_species[[name]]$value_simple)
}))
any_outside <- Reduce(`|`, lapply(species, function(name) {
  !per_species[[name]]$inside_extent
}))
any_containing_na <- Reduce(`|`, lapply(species, function(name) {
  per_species[[name]]$containing_cell_na
}))
any_stencil_na <- Reduce(`|`, lapply(species, function(name) {
  isTRUE_or_false <- per_species[[name]]$stencil_has_na
  ifelse(is.na(isTRUE_or_false), FALSE, isTRUE_or_false)
}))

excluded <- !bilinear_ok
diagnosis <- data.frame(
  category = c(
    "observations",
    "Bombus_W resolvable under bilinear (current code)",
    "Bombus_W resolvable under simple (historical default)",
    "excluded under bilinear",
    "  of which outside the raster extent",
    "  of which the containing cell is NA",
    "  of which the containing cell is valid but the bilinear stencil has an NA",
    "recovered by simple extraction"
  ),
  n = c(
    nrow(observations),
    sum(bilinear_ok),
    sum(simple_ok),
    sum(excluded),
    sum(excluded & any_outside),
    sum(excluded & !any_outside & any_containing_na),
    sum(excluded & !any_outside & !any_containing_na & any_stencil_na),
    sum(excluded & simple_ok)
  ),
  stringsAsFactors = FALSE
)
rp_write_csv_atomic(diagnosis, file.path(output_dir, "extraction_diagnosis.csv"))
cat("\n=== extraction diagnosis ===\n")
print(diagnosis, row.names = FALSE)

excluded_detail <- detail[detail$observation_id %in%
                            observations$observation_id[excluded], , drop = FALSE]
rp_write_csv_atomic(
  excluded_detail, file.path(output_dir, "excluded_observations.csv")
)
cat("\nPer-observation detail written for", sum(excluded),
    "observations that bilinear cannot resolve.\n")
