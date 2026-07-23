MLIT_L03_BASE_URL <- paste0(
  "https://nlftp.mlit.go.jp/ksj/gml/data/L03-b/L03-b-21/",
  "L03-b-21_%s-jgd2011_GML.zip"
)

MLIT_FOREST_CODES <- "0500"
MLIT_HUMAN_MANAGED_CODES <- c(
  "0100", # rice paddy
  "0200", # other agricultural land
  "0700", # built-up land
  "0901", # road
  "0902", # railway
  "1000", # other artificial land
  "1600"  # golf course
)
MLIT_MAJOR_ROAD_CODE <- "0901"

require_mlit_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing required packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

primary_mesh_code <- function(longitude, latitude) {
  longitude <- as.numeric(longitude)
  latitude <- as.numeric(latitude)
  if (any(!is.finite(longitude) | !is.finite(latitude))) {
    stop("Longitude and latitude must be finite.", call. = FALSE)
  }
  lat_index <- floor(latitude * 1.5)
  lon_index <- floor(longitude) - 100L
  if (any(lat_index < 0L | lat_index > 99L | lon_index < 0L | lon_index > 99L)) {
    stop("Coordinates are outside the supported Japanese mesh-code domain.", call. = FALSE)
  }
  sprintf("%02d%02d", lat_index, lon_index)
}

decode_tertiary_mesh_centres <- function(mesh8) {
  mesh8 <- as.character(mesh8)
  if (any(nchar(mesh8) != 8L | grepl("[^0-9]", mesh8))) {
    stop("Third-order mesh codes must be eight digits.", call. = FALSE)
  }
  digit <- function(from, to = from) as.integer(substr(mesh8, from, to))
  primary_lat <- digit(1L, 2L)
  primary_lon <- digit(3L, 4L)
  secondary_lat <- digit(5L)
  secondary_lon <- digit(6L)
  tertiary_lat <- digit(7L)
  tertiary_lon <- digit(8L)
  data.frame(
    mesh_1km = mesh8,
    longitude = 100 + primary_lon + secondary_lon / 8 +
      tertiary_lon / 80 + 1 / 160,
    latitude = primary_lat / 1.5 + secondary_lat / 12 +
      tertiary_lat / 120 + 1 / 240,
    stringsAsFactors = FALSE
  )
}

classify_mlit_landuse <- function(code) {
  code <- as.character(code)
  out <- rep.int(3L, length(code))
  out[code %in% MLIT_FOREST_CODES] <- 1L
  out[code %in% MLIT_HUMAN_MANAGED_CODES] <- 2L
  out
}

add_interface_half_edges <- function(edge_contribution, transition, row_offset = 0L,
                                     col_offset = 0L) {
  index <- which(transition, arr.ind = TRUE)
  if (!nrow(index)) return(edge_contribution)
  target <- cbind(index[, 1L] + row_offset, index[, 2L] + col_offset)
  edge_contribution[target] <- edge_contribution[target] + 0.05
  edge_contribution
}

aggregate_mlit_landuse_dbf <- function(data) {
  require_mlit_packages("FNN")
  required <- c("L03b_001", "L03b_002")
  if (!all(required %in% names(data))) {
    stop("MLIT DBF is missing L03b_001 or L03b_002.", call. = FALSE)
  }
  mesh10 <- as.character(data$L03b_001)
  if (any(nchar(mesh10) != 10L | grepl("[^0-9]", mesh10))) {
    stop("L03b_001 must contain ten-digit 100 m mesh codes.", call. = FALSE)
  }
  land_class <- classify_mlit_landuse(data$L03b_002)
  digit <- function(position) as.integer(substr(mesh10, position, position))
  row <- 100L * digit(5L) + 10L * digit(7L) + digit(9L)
  col <- 100L * digit(6L) + 10L * digit(8L) + digit(10L)
  if (any(row < 0L | row > 799L | col < 0L | col > 799L)) {
    stop("Decoded 100 m mesh coordinates fall outside a primary mesh.", call. = FALSE)
  }

  grid <- matrix(0L, nrow = 800L, ncol = 800L)
  grid[cbind(row + 1L, col + 1L)] <- land_class
  edge_contribution <- matrix(0, nrow = 800L, ncol = 800L)

  horizontal <- (grid[, -800L, drop = FALSE] == 1L &
                   grid[, -1L, drop = FALSE] == 2L) |
    (grid[, -800L, drop = FALSE] == 2L &
       grid[, -1L, drop = FALSE] == 1L)
  edge_contribution <- add_interface_half_edges(edge_contribution, horizontal)
  edge_contribution <- add_interface_half_edges(
    edge_contribution, horizontal, col_offset = 1L
  )

  vertical <- (grid[-800L, , drop = FALSE] == 1L &
                 grid[-1L, , drop = FALSE] == 2L) |
    (grid[-800L, , drop = FALSE] == 2L &
       grid[-1L, , drop = FALSE] == 1L)
  edge_contribution <- add_interface_half_edges(edge_contribution, vertical)
  edge_contribution <- add_interface_half_edges(
    edge_contribution, vertical, row_offset = 1L
  )

  mesh8 <- substr(mesh10, 1L, 8L)
  edge_at_cell <- edge_contribution[cbind(row + 1L, col + 1L)]
  sum_by_mesh <- function(value) {
    as.numeric(rowsum(value, mesh8, reorder = TRUE)[, 1L])
  }
  keys <- sort(unique(mesh8))
  out <- data.frame(
    mesh_1km = keys,
    human_forest_edge_km_per_nominal_km2 = sum_by_mesh(edge_at_cell),
    forest_fraction = sum_by_mesh(as.numeric(land_class == 1L)) / 100,
    human_managed_fraction = sum_by_mesh(as.numeric(land_class == 2L)) / 100,
    represented_fraction = sum_by_mesh(rep.int(1, length(mesh8))) / 100,
    stringsAsFactors = FALSE
  )
  # Only interfaces between forest and human-managed cells are counted. Each
  # 100 m boundary contributes 0.05 km to each endpoint, hence 0.1 km overall.
  # Interfaces crossing the outer edge of a primary mesh cannot be recovered
  # without downloading neighbouring tiles; flag those 1 km cells explicitly.
  tertiary_row <- 10L * digit(5L) + digit(7L)
  tertiary_col <- 10L * digit(6L) + digit(8L)
  boundary_by_mesh <- tapply(
    tertiary_row %in% c(0L, 79L) | tertiary_col %in% c(0L, 79L),
    mesh8, any
  )
  out$primary_mesh_boundary <- as.logical(boundary_by_mesh[out$mesh_1km])
  key_digit <- function(position) as.integer(substr(out$mesh_1km, position, position))
  centre_row <- 100L * key_digit(5L) + 10L * key_digit(7L) + 4.5
  centre_col <- 100L * key_digit(6L) + 10L * key_digit(8L) + 4.5
  road <- as.character(data$L03b_002) == MLIT_MAJOR_ROAD_CODE
  out$major_road_fraction <- sum_by_mesh(as.numeric(road)) / 100
  if (any(road)) {
    nearest <- FNN::get.knnx(
      data = cbind(row[road] + 0.5, col[road] + 0.5),
      query = cbind(centre_row, centre_col), k = 1L
    )
    out$major_road_distance_km <- as.numeric(nearest$nn.dist[, 1L]) * 0.1
  } else {
    out$major_road_distance_km <- NA_real_
  }
  cbind(out, decode_tertiary_mesh_centres(out$mesh_1km)[, c("longitude", "latitude")])
}

mlit_archive_url <- function(primary_mesh) {
  sprintf(MLIT_L03_BASE_URL, primary_mesh)
}

download_mlit_archive <- function(primary_mesh, cache_dir) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(
    cache_dir, sprintf("L03-b-21_%s-jgd2011_GML.zip", primary_mesh)
  )
  if (!file.exists(archive) || file.info(archive)$size == 0) {
    utils::download.file(
      mlit_archive_url(primary_mesh), archive,
      mode = "wb", method = "libcurl", quiet = TRUE
    )
  }
  archive
}

process_mlit_primary_mesh <- function(primary_mesh, cache_dir, processed_dir) {
        require_mlit_packages(c("foreign", "FNN"))
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
  processed_path <- file.path(processed_dir, paste0(primary_mesh, "_edge_1km.rds"))
  if (file.exists(processed_path)) {
    cached <- readRDS(processed_path)
    required_columns <- c("major_road_fraction", "major_road_distance_km")
    if (all(required_columns %in% names(cached))) return(cached)
  }

  archive <- download_mlit_archive(primary_mesh, cache_dir)
  listing <- utils::unzip(archive, list = TRUE)
  dbf_name <- listing$Name[grepl("\\.dbf$", listing$Name, ignore.case = TRUE)]
  if (length(dbf_name) != 1L) {
    stop("Expected one DBF in ", archive, "; found ", length(dbf_name), call. = FALSE)
  }
  extract_dir <- tempfile(paste0("mlit_", primary_mesh, "_"))
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(archive, files = dbf_name, exdir = extract_dir, junkpaths = TRUE)
  dbf_path <- file.path(extract_dir, basename(dbf_name))
  raw <- foreign::read.dbf(dbf_path, as.is = TRUE)
  aggregated <- aggregate_mlit_landuse_dbf(raw)
  saveRDS(aggregated, processed_path, compress = "xz")
  attr(aggregated, "archive_path") <- archive
  aggregated
}

write_mlit_1km_raster <- function(cells, path, value_column, layer_name) {
  require_mlit_packages("terra")
  dx <- 1 / 80
  dy <- 1 / 120
  xmin <- floor(min(cells$longitude) / dx) * dx
  xmax <- ceiling(max(cells$longitude) / dx) * dx
  ymin <- floor(min(cells$latitude) / dy) * dy
  ymax <- ceiling(max(cells$latitude) / dy) * dy
  raster <- terra::rast(
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
    resolution = c(dx, dy), crs = "EPSG:6668"
  )
  names(raster) <- layer_name
  cell_index <- terra::cellFromXY(raster, cells[, c("longitude", "latitude")])
  values <- rep(NA_real_, terra::ncell(raster))
  values[cell_index] <- cells[[value_column]]
  terra::values(raster) <- values
  terra::writeRaster(
    raster, path, overwrite = TRUE, datatype = "FLT4S",
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES")
  )
  invisible(path)
}

write_mlit_edge_raster <- function(cells, path) {
  write_mlit_1km_raster(
    cells, path, "human_forest_edge_km_per_nominal_km2", "human_forest_edge"
  )
}

build_mlit_human_forest_edge <- function(observation_csv, output_dir, cache_dir) {
  require_mlit_packages(c("foreign", "FNN", "terra", "jsonlite"))
  observations <- utils::read.csv(
    observation_csv, check.names = FALSE, stringsAsFactors = FALSE
  )
  required <- c("longitude", "latitude")
  if (!all(required %in% names(observations))) {
    stop("Observation CSV must contain longitude and latitude.", call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  processed_dir <- file.path(output_dir, "processed_primary_meshes")
  meshes <- sort(unique(primary_mesh_code(
    observations$longitude, observations$latitude
  )))
  results <- vector("list", length(meshes))
  manifest <- vector("list", length(meshes))
  for (i in seq_along(meshes)) {
    message(sprintf("[%d/%d] MLIT primary mesh %s", i, length(meshes), meshes[i]))
    started <- Sys.time()
    value <- process_mlit_primary_mesh(meshes[i], cache_dir, processed_dir)
    archive <- file.path(
      cache_dir, sprintf("L03-b-21_%s-jgd2011_GML.zip", meshes[i])
    )
    results[[i]] <- value
    manifest[[i]] <- data.frame(
      primary_mesh = meshes[i],
      source_url = mlit_archive_url(meshes[i]),
      archive_bytes = file.info(archive)$size,
      archive_md5 = unname(tools::md5sum(archive)),
      cells_1km = nrow(value),
      elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      stringsAsFactors = FALSE
    )
  }
  cells <- do.call(rbind, results)
  cells <- cells[!duplicated(cells$mesh_1km), , drop = FALSE]
  rownames(cells) <- NULL
  manifest <- do.call(rbind, manifest)

  raster_path <- file.path(output_dir, "mlit_human_forest_edge_1km.tif")
  write_mlit_edge_raster(cells, raster_path)
  raster <- terra::rast(raster_path)
  road_raster_path <- file.path(output_dir, "mlit_major_road_distance_1km.tif")
  write_mlit_1km_raster(
    cells, road_raster_path, "major_road_distance_km", "road_access"
  )
  road_raster <- terra::rast(road_raster_path)
  extracted <- terra::extract(
    raster, observations[, c("longitude", "latitude")], ID = FALSE
  )
  road_extracted <- terra::extract(
    road_raster, observations[, c("longitude", "latitude")], ID = FALSE
  )
  observation_id <- if ("observation_id" %in% names(observations)) {
    observations$observation_id
  } else if ("sample_id" %in% names(observations)) {
    observations$sample_id
  } else seq_len(nrow(observations))
  observation_values <- data.frame(
    observation_id = observation_id,
    primary_mesh = primary_mesh_code(observations$longitude, observations$latitude),
    longitude = observations$longitude,
    latitude = observations$latitude,
    human_forest_edge = extracted[[1L]],
    major_road_distance_km = road_extracted[[1L]],
    stringsAsFactors = FALSE
  )
  # The raster value is authoritative. Boundary status is joined by the nearest
  # 1 km cell because hand-building an eight-digit key is vulnerable at floating
  # point boundaries.
  nearest_cell <- terra::cellFromXY(raster, observations[, c("longitude", "latitude")])
  raster_xy <- terra::xyFromCell(raster, nearest_cell)
  centre_key <- paste(round(cells$longitude, 7), round(cells$latitude, 7))
  obs_key <- paste(round(raster_xy[, 1L], 7), round(raster_xy[, 2L], 7))
  observation_values$primary_mesh_boundary <- cells$primary_mesh_boundary[
    match(obs_key, centre_key)
  ]

  utils::write.csv(cells, file.path(output_dir, "human_forest_edge_1km_cells.csv"), row.names = FALSE)
  utils::write.csv(manifest, file.path(output_dir, "download_manifest.csv"), row.names = FALSE)
  utils::write.csv(
    observation_values,
    file.path(output_dir, "observation_human_forest_edge.csv"), row.names = FALSE
  )
  class_definition <- data.frame(
    role = c("forest", rep("human_managed", length(MLIT_HUMAN_MANAGED_CODES))),
    code = c(MLIT_FOREST_CODES, MLIT_HUMAN_MANAGED_CODES),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    class_definition, file.path(output_dir, "landuse_class_definition.csv"),
    row.names = FALSE
  )
  summary <- list(
    source = "MLIT National Land Numerical Information L03-b 2021",
    licence = "CC BY 4.0",
    observation_file = basename(observation_csv),
    raster = basename(raster_path),
    road_access_raster = basename(road_raster_path),
    primary_meshes = meshes,
    n_primary_meshes = length(meshes),
    n_observations = nrow(observation_values),
    n_observations_with_value = sum(is.finite(observation_values$human_forest_edge)),
    n_observations_with_road_distance = sum(
      is.finite(observation_values$major_road_distance_km)
    ),
    n_observations_primary_boundary = sum(
      observation_values$primary_mesh_boundary %in% TRUE, na.rm = TRUE
    ),
    definition = paste(
      "Nominal kilometres of shared 100 m boundary between forest (0500)",
      "and human-managed land per third-order approximately 1 km mesh."
    ),
    role = paste(
      "R is a response-blind establishment/escape-interface proxy, not direct",
      "evidence that a flower is a horticultural cultivar."
    ),
    road_access_role = paste(
      "A is distance to the nearest MLIT 0901 major-road cell within the same",
      "primary mesh and is used only as an observation-access sensitivity."
    )
  )
  jsonlite::write_json(
    summary, file.path(output_dir, "run_summary.json"),
    pretty = TRUE, auto_unbox = TRUE
  )
  invisible(list(
    cells = cells, observations = observation_values,
    manifest = manifest, raster = raster_path, summary = summary
  ))
}
