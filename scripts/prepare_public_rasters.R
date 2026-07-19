#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
overwrite <- "--overwrite" %in% args

source("R/raster_sources.R")
manifest <- prepare_public_rasters(overwrite = overwrite)
print(manifest)
cat("\nPrepared", nrow(manifest), "public raster layers at the common 30 arc-second grid.\n")
cat("Manifest: data/rasters/manifest.csv\n")
