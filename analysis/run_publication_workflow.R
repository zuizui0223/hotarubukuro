#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(lavaan)
  library(INLA)
  library(qgam)
  library(yaml)
})
source("R/pigmentation_workflow.R")
set.seed(42)
INLA::inla.setOption(num.threads = "1:1")

args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1) args[[1]] else "Data_S1.csv"
env_dir <- if (length(args) >= 2) args[[2]] else "data/processed/rasters"
sdm_dir <- if (length(args) >= 3) args[[3]] else "sdm"
human_dir <- if (length(args) >= 4) args[[4]] else "data/cache/human"
out_dir <- if (length(args) >= 5) args[[5]] else "results/publication"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

z <- function(x) as.numeric(scale(x))
nonempty <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))
write_table <- function(x, name) write.csv(x, file.path(out_dir, name), row.names = FALSE)

extract_rasters <- function(paths, layer_names, data) {
  missing <- paths[!file.exists(paths)]
  if (length(missing)) stop("Missing raster(s): ", paste(missing, collapse = ", "), call. = FALSE)
  r <- rast(paths); names(r) <- layer_names
  pts <- vect(data[c("longitude", "latitude")], geom = c("longitude", "latitude"), crs = "EPSG:4326")
  if (!same.crs(pts, r)) pts <- project(pts, crs(r))
  values <- as.data.frame(extract(r, pts, ID = FALSE), check.names = FALSE)
  if (nrow(values) != nrow(data)) stop("Raster extraction changed record count.", call. = FALSE)
  values
}

buffer_fraction <- function(r, points, class_codes, radius_m, out_name) {
  binary <- classify(r, rbind(c(-Inf, Inf, NA)))
  values(r = binary) # force materialization check
  binary <- r %in% class_codes
  names(binary) <- out_name
  pts <- points
  if (!same.crs(pts, binary)) pts <- project(pts, crs(binary))
  out <- extract(binary, pts, buffer = radius_m, fun = mean, na.rm = TRUE, ID = FALSE)
  as.numeric(out[[1]])
}

# 1. Canonical identity and colour measurement contract.
d <- read.csv(input, check.names = FALSE, fileEncoding = "UTF-8-BOM")
needed <- c("observation_id", "date", "latitude", "longitude", "R", "G", "B")
missing <- setdiff(needed, names(d)); if (length(missing)) stop("Missing canonical columns: ", paste(missing, collapse = ", "))
if (anyDuplicated(d$observation_id)) stop("observation_id must be unique")
for (nm in c("latitude", "longitude", "R", "G", "B")) d[[nm]] <- as.numeric(d[[nm]])
d$date <- as.Date(gsub("/", "-", trimws(as.character(d$date))))
d$DOY <- as.integer(format(d$date, "%j"))
identity_bad <- rep(FALSE, nrow(d))
if ("duplicate_image_sha256" %in% names(d)) identity_bad <- identity_bad | nonempty(d$duplicate_image_sha256)
if ("photo_coordinate_qc_status" %in% names(d)) identity_bad <- identity_bad | grepl("duplicate", d$photo_coordinate_qc_status, ignore.case = TRUE)
valid <- !identity_bad & complete.cases(d[needed]) & is.finite(d$latitude) & is.finite(d$longitude)
write_table(d[!valid, intersect(c("observation_id", "source_row", "image_sha256", "qc_flags"), names(d)), drop = FALSE], "excluded_colour_records.csv")
d <- d[valid, , drop = FALSE]
reviewer_contract(d)
lab <- srgb_to_lab(d$R, d$G, d$B); d <- cbind(d, lab)
cfa <- fit_legacy_pigment_cfa(d); d$Pigment <- cfa$scores
write_table(cfa$loadings, "cfa_standardized_loadings.csv")
write_table(data.frame(indicator = c("a", "Lm", "C"),
                       mean = colMeans(cfa$indicators[c("a", "Lm", "C")], na.rm = TRUE),
                       sd = vapply(cfa$indicators[c("a", "Lm", "C")], sd, numeric(1), na.rm = TRUE)),
            "cfa_indicator_summary.csv")

# 2. Legacy environmental domains; AI alone is replaced by CHELSA CMI.
env_files <- c(
  bio05 = "chelsa_bio05.tif", bio10 = "chelsa_bio10.tif", gdd5 = "chelsa_gdd5.tif",
  cmi = "chelsa_cmimean.tif", vpd = "chelsa_vpdmean.tif", bio12 = "chelsa_bio12.tif",
  bio14 = "chelsa_bio14.tif", bio15 = "chelsa_bio15.tif", swb = "chelsa_swb.tif",
  RSDS = "chelsa_rsdsmean.tif", bdod = "soilgrids_bdod_0_5cm_mean.tif",
  cfvo = "soilgrids_cfvo_0_5cm_mean.tif", sand = "soilgrids_sand_0_5cm_mean.tif",
  silt = "soilgrids_silt_0_5cm_mean.tif", nitrogen = "soilgrids_nitrogen_0_5cm_mean.tif",
  ocd = "soilgrids_ocd_0_5cm_mean.tif", soc = "soilgrids_soc_0_5cm_mean.tif",
  soil_pH = "soilgrids_phh2o_0_5cm_mean.tif", elevation = "elevation_30s.tif"
)
d <- cbind(d, extract_rasters(file.path(env_dir, env_files), names(env_files), d))
elev <- rast(file.path(env_dir, env_files[["elevation"]]))
topo <- c(terrain(elev, "roughness"), terrain(elev, "slope", unit = "radians"), terrain(elev, "TRI"))
names(topo) <- c("roughness", "slope", "TRI")
pts <- vect(d[c("longitude", "latitude")], geom = c("longitude", "latitude"), crs = "EPSG:4326")
pts_topo <- if (!same.crs(pts, topo)) project(pts, crs(topo)) else pts
d <- cbind(d, as.data.frame(extract(topo, pts_topo, ID = FALSE)))
axes <- list(
  Temperature_PC1 = pca_axis(d, c("bio05", "bio10", "gdd5"), "Temperature_PC1"),
  precip_PC1 = pca_axis(d, c("cmi", "vpd", "bio12", "bio14", "bio15", "swb"), "precip_PC1"),
  soil_phys_PC1 = pca_axis(d, c("bdod", "cfvo", "sand", "silt"), "soil_phys_PC1"),
  soil_nutrient_PC1 = pca_axis(d, c("nitrogen", "ocd", "soc"), "soil_nutrient_PC1"),
  topo_PC1 = pca_axis(d, c("roughness", "slope", "TRI"), "topo_PC1")
)
for (nm in names(axes)) d[[nm]] <- axes[[nm]]$score
write_table(do.call(rbind, lapply(axes, `[[`, "loadings")), "environment_pca_loadings.csv")
write_table(data.frame(axis = names(axes), variance_explained_PC1 = vapply(axes, `[[`, numeric(1), "variance_explained")), "environment_pca_variance.csv")

# 3. Bombus potential availability and distribution-type diagnostics.
species <- c("ardens", "beaticola", "consobrinus", "diversus", "honshuensis")
d <- cbind(d, extract_rasters(file.path(sdm_dir, paste0(species, ".tif")), species, d))
d <- cbind(d, bombus_indices(d, species))
bombus_diagnostics <- cor(d[c("Bombus_availability", "Bombus_widespread", "Bombus_montane", "elevation", "Temperature_PC1", "precip_PC1", "topo_PC1")], use = "pairwise.complete.obs")
write.csv(bombus_diagnostics, file.path(out_dir, "bombus_environment_correlations.csv"))

# 4. One integrated natural model; shared space/environment/pollinator gradients are not interpreted as pure causal partitions.
natural_terms <- integrated_model_terms("Bombus_availability")
common <- c("Pigment", "longitude", "latitude", natural_terms)
cc <- complete.cases(d[common])
write_table(d[!cc, c("observation_id", "latitude", "longitude"), drop = FALSE], "excluded_natural_model_records.csv")
dm <- d[cc, , drop = FALSE]
for (nm in natural_terms) dm[[paste0("z_", nm)]] <- z(dm[[nm]])
dm$y <- z(dm$Pigment)
z_terms <- paste0("z_", natural_terms)

proj_jp <- "+proj=laea +lat_0=36 +lon_0=138 +datum=WGS84 +units=m +no_defs"
loc <- st_coordinates(st_transform(st_as_sf(dm, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE), proj_jp))
mesh <- inla.mesh.2d(loc = loc, boundary = inla.nonconvex.hull(loc, convex = -0.05, resolution = 100000),
                     max.edge = c(30000, 150000), cutoff = 5000, offset = c(30000, 150000))
spde <- inla.spde2.pcmatern(mesh, alpha = 2, prior.range = c(100000, 0.05), prior.sigma = c(1, 0.05))
A <- inla.spde.make.A(mesh, loc = loc)
fit_spde <- function(label, terms) {
  fixed <- data.frame(Intercept = 1, dm[terms], check.names = FALSE)
  stk <- inla.stack(data = list(y = dm$y), A = list(A, 1),
                    effects = list(spatial = seq_len(spde$n.spde), fixed = fixed), tag = "est")
  form <- as.formula(paste("y ~", paste(c("0 + Intercept", terms, "f(spatial, model = spde)"), collapse = " + ")))
  fit <- inla(form, family = "gaussian", data = inla.stack.data(stk),
              control.predictor = list(A = inla.stack.A(stk), compute = TRUE),
              control.compute = list(waic = TRUE, dic = TRUE, cpo = TRUE, config = TRUE), verbose = FALSE)
  list(label = label, fit = fit, stack = stk, terms = terms)
}
fits <- list(
  total = fit_spde("integrated_total_availability", z_terms),
  widespread = fit_spde("integrated_widespread_sensitivity", replace(z_terms, z_terms == "z_Bombus_availability", "z_Bombus_widespread")),
  montane = fit_spde("integrated_montane_sensitivity", replace(z_terms, z_terms == "z_Bombus_availability", "z_Bombus_montane"))
)
# Ensure sensitivity predictors exist and refit when required.
for (nm in c("Bombus_widespread", "Bombus_montane")) dm[[paste0("z_", nm)]] <- z(dm[[nm]])
fits$widespread <- fit_spde("integrated_widespread_sensitivity", replace(z_terms, z_terms == "z_Bombus_availability", "z_Bombus_widespread"))
fits$montane <- fit_spde("integrated_montane_sensitivity", replace(z_terms, z_terms == "z_Bombus_availability", "z_Bombus_montane"))
comparison <- do.call(rbind, lapply(fits, function(x) data.frame(
  model = x$label, n = nrow(dm), mesh_vertices = mesh$n,
  WAIC = x$fit$waic$waic, DIC = x$fit$dic$dic,
  mean_neglogCPO = mean(-log(x$fit$cpo$cpo[x$fit$cpo$cpo > 0 & is.finite(x$fit$cpo$cpo)]))
)))
comparison$delta_WAIC <- comparison$WAIC - min(comparison$WAIC)
write_table(comparison, "integrated_spde_model_diagnostics.csv")
write_table(transform(as.data.frame(fits$total$fit$summary.fixed), term = rownames(fits$total$fit$summary.fixed)), "integrated_spde_fixed_effects.csv")
write_table(transform(as.data.frame(fits$total$fit$summary.hyperpar), parameter = rownames(fits$total$fit$summary.hyperpar)), "integrated_spde_hyperparameters.csv")

idx <- inla.stack.index(fits$total$stack, "est")$data
yhat <- fits$total$fit$summary.fitted.values$mean[idx]
yhat_sd <- fits$total$fit$summary.fitted.values$sd[idx]
dm$predicted_pigment_z <- yhat
dm$predicted_sd <- yhat_sd
dm$resid_raw <- dm$y - yhat
dm$resid_zfit <- dm$resid_raw / pmax(yhat_sd, 1e-6)

# 5. Public human-pressure layers and buffer-scale satoyama context.
human_paths <- c(
  population_density = file.path(human_dir, "population_density.tif"),
  human_footprint = file.path(human_dir, "human_footprint.tif")
)
dm <- cbind(dm, extract_rasters(human_paths, names(human_paths), dm))
dm$pop_density_log <- log1p(pmax(dm$population_density, 0))
dm$human_footprint_z <- z(dm$human_footprint)
landcover_path <- file.path(human_dir, "landcover.tif")
if (!file.exists(landcover_path)) stop("Missing landcover.tif; run scripts/download_human_predictors.R")
lc <- rast(landcover_path)
pts_h <- vect(dm[c("longitude", "latitude")], geom = c("longitude", "latitude"), crs = "EPSG:4326")
dm$built_fraction_1000m <- buffer_fraction(lc, pts_h, 50, 1000, "built")
dm$cropland_fraction_1000m <- buffer_fraction(lc, pts_h, 40, 1000, "cropland")
dm$forest_fraction_1000m <- buffer_fraction(lc, pts_h, c(10, 20, 30), 1000, "forest")
# Edge opportunity proxy: mixed forest/nonforest buffers peak near 0.5 forest cover.
dm$forest_edge_density_1000m <- 4 * dm$forest_fraction_1000m * (1 - dm$forest_fraction_1000m)
dm$landscape_setting <- classify_landscape_setting(dm$elevation, dm$built_fraction_1000m,
                                                     dm$cropland_fraction_1000m, dm$forest_edge_density_1000m)

human_terms <- c("pop_density_log", "human_footprint_z", "built_fraction_1000m", "cropland_fraction_1000m", "forest_edge_density_1000m")
qfit <- fit_anthropogenic_qgam(dm, human_terms = human_terms, include_doy_interactions = TRUE)
qsummary <- do.call(rbind, lapply(seq_along(qfit$models), function(i) {
  sm <- summary(qfit$models[[i]])
  data.frame(quantile = qfit$taus[[i]], deviance_explained = sm$dev.expl,
             n = nrow(qfit$data), formula = paste(deparse(qfit$formula), collapse = " "))
}))
write_table(qsummary, "qgam_model_summary.csv")

# Candidate review table: extreme natural-model deviations, never automatic cultivar labels.
cut <- quantile(dm$resid_zfit, 0.95, na.rm = TRUE)
candidates <- dm[is.finite(dm$resid_zfit) & dm$resid_zfit >= cut,
                 intersect(c("observation_id", "source_row", "url", "date", "DOY", "latitude", "longitude",
                             "Pigment", "predicted_pigment_z", "resid_zfit", human_terms,
                             "elevation", "landscape_setting", "image_sha256", "qc_flags"), names(dm)), drop = FALSE]
candidates$horticultural_status <- "unreviewed_candidate_only"
write_table(candidates[order(-candidates$resid_zfit), ], "anthropogenic_outlier_review.csv")
write_table(dm, "analysis_data_with_predictions.csv")
writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))
cat("Publication workflow completed with", nrow(dm), "natural-model records.\n")
