#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(terra))
options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) > 0) args[[1]] else "Data_S1.csv"
env_dir <- if (length(args) > 1) args[[2]] else "data/processed/rasters"
sdm_dir <- if (length(args) > 2) args[[3]] else "sdm"
out_dir <- if (length(args) > 3) args[[4]] else "results/all_extraction_diagnostic"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

d <- read.csv(input, check.names = FALSE, fileEncoding = "UTF-8-BOM")
for (nm in c("latitude", "longitude")) d[[nm]] <- as.numeric(d[[nm]])
d <- d[is.finite(d$latitude) & is.finite(d$longitude), , drop = FALSE]
points_ll <- terra::vect(d, geom = c("longitude", "latitude"), crs = "EPSG:4326")

files <- c(
  chelsa_bio05="chelsa_bio05.tif", chelsa_bio10="chelsa_bio10.tif", chelsa_gdd5="chelsa_gdd5.tif",
  chelsa_cmimean="chelsa_cmimean.tif", chelsa_vpdmean="chelsa_vpdmean.tif", chelsa_bio12="chelsa_bio12.tif",
  chelsa_bio14="chelsa_bio14.tif", chelsa_bio15="chelsa_bio15.tif", chelsa_swb="chelsa_swb.tif",
  chelsa_rsdsmean="chelsa_rsdsmean.tif", elevation="elevation_30s.tif",
  soilgrids_bdod_0_5cm="soilgrids_bdod_0_5cm_mean.tif",
  soilgrids_cfvo_0_5cm="soilgrids_cfvo_0_5cm_mean.tif",
  soilgrids_sand_0_5cm="soilgrids_sand_0_5cm_mean.tif",
  soilgrids_silt_0_5cm="soilgrids_silt_0_5cm_mean.tif",
  soilgrids_nitrogen_0_5cm="soilgrids_nitrogen_0_5cm_mean.tif",
  soilgrids_ocd_0_5cm="soilgrids_ocd_0_5cm_mean.tif",
  soilgrids_soc_0_5cm="soilgrids_soc_0_5cm_mean.tif",
  soilgrids_phh2o_0_5cm="soilgrids_phh2o_0_5cm_mean.tif"
)
paths <- file.path(env_dir, unname(files))
species <- c("ardens","beaticola","consobrinus","diversus","honshuensis")
paths <- c(paths, file.path(sdm_dir, paste0(species, ".tif")))
names(paths) <- c(names(files), species)

rows <- list()
values <- list()
for (nm in names(paths)) {
  path <- paths[[nm]]
  if (!file.exists(path)) stop("Missing raster: ", path)
  r <- terra::rast(path)
  ex <- as.vector(terra::ext(r))
  geographic <- length(ex) == 4L && all(is.finite(ex)) && ex[[1]] >= -180 && ex[[2]] <= 180 && ex[[3]] >= -90 && ex[[4]] <= 90
  if (!geographic) stop("Non-geographic extent for ", nm, ": ", paste(ex, collapse=", "))
  terra::crs(r) <- "EPSG:4326"
  x <- terra::extract(r, points_ll, ID = FALSE)
  v <- as.numeric(x[[1L]])
  message(nm, ": finite ", sum(is.finite(v)), "/", length(v))
  rows[[nm]] <- data.frame(layer=nm, path=path, finite=sum(is.finite(v)), total=length(v), xmin=ex[[1]], xmax=ex[[2]], ymin=ex[[3]], ymax=ex[[4]])
  values[[nm]] <- v
}

summary_df <- do.call(rbind, rows)
write.csv(summary_df, file.path(out_dir, "layer_extraction_summary.csv"), row.names=FALSE)
X <- as.data.frame(values, check.names=FALSE)
write.csv(X, file.path(out_dir, "extracted_values.csv"), row.names=FALSE)

pca_groups <- list(
  Temperature_PC1=c("chelsa_bio05","chelsa_bio10","chelsa_gdd5"),
  precip_PC1=c("chelsa_cmimean","chelsa_vpdmean","chelsa_bio12","chelsa_bio14","chelsa_bio15","chelsa_swb"),
  soil_phys_PC1=c("soilgrids_bdod_0_5cm","soilgrids_cfvo_0_5cm","soilgrids_sand_0_5cm","soilgrids_silt_0_5cm"),
  soil_nutrient_PC1=c("soilgrids_nitrogen_0_5cm","soilgrids_ocd_0_5cm","soilgrids_soc_0_5cm")
)
pca_rows <- list()
for (label in names(pca_groups)) {
  vars <- pca_groups[[label]]
  M <- X[vars]
  cc <- complete.cases(M)
  if (sum(cc) < 50L) stop("Invalid PCA group: ", label, " complete rows=", sum(cc))
  fit <- prcomp(M[cc,,drop=FALSE], center=TRUE, scale.=TRUE)
  pca_rows[[label]] <- data.frame(group=label, variable=rownames(fit$rotation), PC1_loading=fit$rotation[,1], complete_rows=sum(cc))
  message(label, ": complete rows ", sum(cc))
}
write.csv(do.call(rbind, pca_rows), file.path(out_dir, "pca_loadings.csv"), row.names=FALSE)

P <- as.matrix(X[species])
sdm_ok <- complete.cases(P) & apply(P, 1, function(z) all(is.finite(z) & z >= 0 & z <= 1))
indices <- data.frame(
  Bombus_suitability_sum=rowSums(P),
  Bombus_any_availability=1-apply(1-P,1,prod),
  Bombus_max_availability=apply(P,1,max),
  sdm_complete=sdm_ok
)
write.csv(indices, file.path(out_dir, "bombus_availability_indices.csv"), row.names=FALSE)
message("SDM complete rows: ", sum(sdm_ok), "/", length(sdm_ok))
