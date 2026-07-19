# Public raster acquisition and harmonisation at 30 arc-seconds.
# Raw downloads are cached under data-raw/ and derived Japan rasters under data/rasters/.

require_namespace <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package required: ", package, call. = FALSE)
  }
}

safe_download <- function(url, destination, overwrite = FALSE) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(destination) && !overwrite) return(normalizePath(destination))
  temporary <- paste0(destination, ".part")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::download.file(url, temporary, mode = "wb", quiet = FALSE)
  if (!file.rename(temporary, destination)) stop("Could not move downloaded file to ", destination, call. = FALSE)
  normalizePath(destination)
}

chelsa_url <- function(variable) {
  base <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio"
  paste0(base, "/CHELSA_", variable, "_1981-2010_V.2.1.tif")
}

chelsa_variables_default <- function() {
  c("bio5", "bio10", "gdd5", "bio12", "bio14", "bio15", "bio18", "ai", "vpd_mean", "swb", "rsds_mean")
}

japan_extent <- function(buffer_degrees = 0.5) {
  require_namespace("terra")
  terra::ext(122 - buffer_degrees, 154 + buffer_degrees, 20 + 0 - buffer_degrees, 46 + buffer_degrees)
}

download_chelsa_japan <- function(variables = chelsa_variables_default(), raw_dir = "data-raw/chelsa",
                                  output_dir = "data/rasters/chelsa", overwrite = FALSE) {
  require_namespace("terra")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  outputs <- character(length(variables))
  names(outputs) <- variables
  for (variable in variables) {
    raw <- file.path(raw_dir, paste0("CHELSA_", variable, "_1981-2010_V.2.1.tif"))
    safe_download(chelsa_url(variable), raw, overwrite = overwrite)
    target <- file.path(output_dir, paste0(variable, "_30s_japan.tif"))
    if (!file.exists(target) || overwrite) {
      raster <- terra::rast(raw)
      clipped <- terra::crop(raster, japan_extent())
      names(clipped) <- variable
      terra::writeRaster(clipped, target, overwrite = TRUE,
                         wopt = list(gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")))
    }
    outputs[[variable]] <- normalizePath(target)
  }
  outputs
}

make_30s_template <- function(reference_file) {
  require_namespace("terra")
  reference <- terra::rast(reference_file)
  if (!isTRUE(all.equal(as.numeric(terra::res(reference)), c(1 / 120, 1 / 120), tolerance = 1e-8))) {
    warning("Reference raster is not exactly 30 arc-seconds; its native grid will be used")
  }
  reference[[1]]
}

# SRTM-derived 30 arc-second elevation obtained through geodata and aligned to CHELSA.
download_elevation_japan <- function(template, raw_dir = "data-raw/elevation",
                                     output = "data/rasters/topography/elevation_30s_japan.tif", overwrite = FALSE) {
  require_namespace("terra")
  require_namespace("geodata")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(output) || overwrite) {
    elevation <- geodata::elevation_30s(country = "JPN", path = raw_dir)
    elevation <- terra::rast(elevation)
    aligned <- terra::resample(elevation, template, method = "bilinear")
    names(aligned) <- "elevation"
    terra::writeRaster(aligned, output, overwrite = TRUE,
                       wopt = list(gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")))
  }
  normalizePath(output)
}

# SoilGrids is native 250 m. Download from the public SoilGrids file service via geodata,
# then aggregate/resample to the common CHELSA 30 arc-second grid.
download_soilgrids_japan <- function(template,
                                      variables = c("bdod", "cfvo", "sand", "silt", "nitrogen", "ocd", "soc", "phh2o"),
                                      depth = "0-5cm", statistic = "mean", raw_dir = "data-raw/soilgrids",
                                      output_dir = "data/rasters/soilgrids", overwrite = FALSE) {
  require_namespace("terra")
  require_namespace("geodata")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  outputs <- character(length(variables)); names(outputs) <- variables
  for (variable in variables) {
    target <- file.path(output_dir, paste0(variable, "_", depth, "_", statistic, "_30s_japan.tif"))
    if (!file.exists(target) || overwrite) {
      source <- geodata::soil_world(var = variable, depth = depth, stat = statistic, path = raw_dir)
      source <- terra::rast(source)
      source <- terra::crop(source, terra::ext(template))
      aligned <- terra::resample(source, template, method = "bilinear")
      names(aligned) <- paste(variable, depth, statistic, sep = "_")
      terra::writeRaster(aligned, target, overwrite = TRUE,
                         wopt = list(gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")))
    }
    outputs[[variable]] <- normalizePath(target)
  }
  outputs
}

worldpop_2020_japan_url <- function(un_adjusted = TRUE) {
  suffix <- if (un_adjusted) "_UNadj" else ""
  paste0("https://data.worldpop.org/GIS/Population_Density/Global_2000_2020_1km", suffix,
         "/2020/JPN/jpn_pd_2020_1km", suffix, ".tif")
}

download_worldpop_japan <- function(template, raw_dir = "data-raw/worldpop",
                                    output = "data/rasters/human/jpn_population_density_2020_30s.tif",
                                    un_adjusted = TRUE, overwrite = FALSE) {
  require_namespace("terra")
  raw <- file.path(raw_dir, basename(worldpop_2020_japan_url(un_adjusted)))
  safe_download(worldpop_2020_japan_url(un_adjusted), raw, overwrite = overwrite)
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(output) || overwrite) {
    population <- terra::rast(raw)
    aligned <- terra::resample(population, template, method = "bilinear")
    names(aligned) <- "population_density_2020"
    terra::writeRaster(aligned, output, overwrite = TRUE,
                       wopt = list(gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")))
  }
  normalizePath(output)
}

build_topography <- function(elevation_file, output_dir = "data/rasters/topography", overwrite = FALSE) {
  require_namespace("terra")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  elevation <- terra::rast(elevation_file)
  products <- list(
    slope = terra::terrain(elevation, v = "slope", unit = "radians"),
    aspect = terra::terrain(elevation, v = "aspect", unit = "radians"),
    TRI = terra::terrain(elevation, v = "TRI"),
    TPI = terra::terrain(elevation, v = "TPI"),
    roughness = terra::terrain(elevation, v = "roughness")
  )
  outputs <- character(length(products)); names(outputs) <- names(products)
  for (name in names(products)) {
    target <- file.path(output_dir, paste0(name, "_30s_japan.tif"))
    names(products[[name]]) <- name
    terra::writeRaster(products[[name]], target, overwrite = overwrite,
                       wopt = list(gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")))
    outputs[[name]] <- normalizePath(target)
  }
  outputs
}

prepare_public_rasters <- function(overwrite = FALSE) {
  chelsa <- download_chelsa_japan(overwrite = overwrite)
  template <- make_30s_template(unname(chelsa[[1]]))
  elevation <- download_elevation_japan(template, overwrite = overwrite)
  soil <- download_soilgrids_japan(template, overwrite = overwrite)
  population <- download_worldpop_japan(template, overwrite = overwrite)
  topography <- build_topography(elevation, overwrite = overwrite)
  manifest <- data.frame(
    group = c(rep("chelsa", length(chelsa)), rep("soilgrids", length(soil)), "elevation", "worldpop", rep("topography", length(topography))),
    variable = c(names(chelsa), names(soil), "elevation", "population_density_2020", names(topography)),
    path = c(unname(chelsa), unname(soil), elevation, population, unname(topography)),
    resolution = "30 arc-seconds",
    stringsAsFactors = FALSE
  )
  dir.create("data/rasters", recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(manifest, "data/rasters/manifest.csv", row.names = FALSE)
  manifest
}
