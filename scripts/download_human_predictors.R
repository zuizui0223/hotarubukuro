#!/usr/bin/env Rscript
# Download human-pressure predictors through versioned public-data clients.
# Output paths are repository-relative and no local user directory is assumed.

suppressPackageStartupMessages({
  library(terra)
  library(geodata)
})

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) args[[1]] else "data/cache/human"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

call_supported <- function(fun, values) {
  fml <- names(formals(fun))
  do.call(fun, values[intersect(names(values), fml)])
}

write_source_record <- function(id, object, provider, version, note = NA_character_) {
  files <- unique(unlist(sources(object)))
  data.frame(
    source_id = id,
    provider = provider,
    dataset_version = version,
    local_files = paste(files, collapse = ";"),
    downloaded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    note = note,
    stringsAsFactors = FALSE
  )
}

records <- list()

# WorldPop population surface. geodata selects the provider asset and caches it.
pop <- call_supported(
  geodata::population,
  list(country = "JPN", year = 2020, res = 30, path = out_dir)
)
names(pop) <- "population_density"
writeRaster(pop, file.path(out_dir, "population_density.tif"), overwrite = TRUE)
records[["population"]] <- write_source_record(
  "population_density", pop, "WorldPop via geodata", "2020",
  "Used as introduction pressure and interpreted separately from observation effort."
)

# Global human footprint combines built environment, access and land-use pressure.
foot <- call_supported(
  geodata::footprint,
  list(year = 2009, path = out_dir)
)
names(foot) <- "human_footprint"
writeRaster(foot, file.path(out_dir, "human_footprint.tif"), overwrite = TRUE)
records[["footprint"]] <- write_source_record(
  "human_footprint", foot, "Global Human Footprint via geodata", "2009",
  "Sensitivity predictor; temporal mismatch is reported explicitly."
)

# Land cover is converted later to buffer-scale built, cropland and forest-edge metrics.
lc <- call_supported(
  geodata::landcover,
  list(path = out_dir)
)
names(lc) <- "landcover"
writeRaster(lc, file.path(out_dir, "landcover.tif"), overwrite = TRUE, datatype = "INT2U")
records[["landcover"]] <- write_source_record(
  "landcover", lc, "ESA land cover via geodata", "provider-current-pinned-by-manifest",
  "Class codes are retained; derived classes are declared in config/anthropogenic.yml."
)

manifest <- do.call(rbind, records)
write.csv(manifest, file.path(out_dir, "source_manifest.csv"), row.names = FALSE)
cat("Human-pressure predictors written to", out_dir, "\n")
