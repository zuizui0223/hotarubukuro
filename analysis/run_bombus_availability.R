#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(INLA)
})

options(stringsAsFactors = FALSE)
set.seed(42)
INLA::inla.setOption(num.threads = "1:1")

args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1L) args[[1L]] else "Data_S1.csv"
env_dir <- if (length(args) >= 2L) args[[2L]] else "data/processed/rasters"
sdm_dir <- if (length(args) >= 3L) args[[3L]] else "sdm"
out_dir <- if (length(args) >= 4L) args[[4L]] else "results/bombus_availability"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

z <- function(x) as.numeric(scale(x))
nonempty <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))

pca_score <- function(dat, variables, output_name) {
  X <- dat[, variables, drop = FALSE]
  keep_variable <- vapply(X, function(x) is.numeric(x) && is.finite(sd(x, na.rm = TRUE)) && sd(x, na.rm = TRUE) > 0, logical(1))
  X <- X[, keep_variable, drop = FALSE]
  if (ncol(X) < 2L) stop(output_name, " requires at least two nonconstant layers.", call. = FALSE)
  complete <- complete.cases(X)
  if (sum(complete) < 50L) stop(output_name, " has too few complete rows.", call. = FALSE)
  fit <- prcomp(X[complete, , drop = FALSE], center = TRUE, scale. = TRUE)
  score <- rep(NA_real_, nrow(dat))
  score[complete] <- fit$x[, 1]
  loading <- data.frame(
    group = output_name,
    variable = rownames(fit$rotation),
    PC1_loading = fit$rotation[, 1],
    stringsAsFactors = FALSE
  )
  list(score = score, loading = loading, fit = fit)
}

extract_stack <- function(paths, names_out, data) {
  missing <- paths[!file.exists(paths)]
  if (length(missing)) stop("Missing raster(s): ", paste(missing, collapse = ", "), call. = FALSE)
  r <- rast(paths)
  names(r) <- names_out
  pts <- vect(data[c("longitude", "latitude")], geom = c("longitude", "latitude"), crs = "EPSG:4326")
  vals <- as.data.frame(extract(r, pts, ID = FALSE), check.names = FALSE)
  if (nrow(vals) != nrow(data)) stop("Raster extraction changed row count.", call. = FALSE)
  vals
}

# -----------------------------------------------------------------------------
# 1. Canonical PR #4 data contract and apparent colour response.
# -----------------------------------------------------------------------------
if (!file.exists(input)) stop("Input not found: ", input, call. = FALSE)
d <- read.csv(input, check.names = FALSE, fileEncoding = "UTF-8-BOM")
required <- c("observation_id", "date", "latitude", "longitude", "R", "G", "B")
missing <- setdiff(required, names(d))
if (length(missing)) stop("Canonical input is missing: ", paste(missing, collapse = ", "), call. = FALSE)
if (anyDuplicated(d$observation_id)) stop("observation_id is not unique.", call. = FALSE)

n_input <- nrow(d)
d$date <- as.Date(gsub("/", "-", trimws(as.character(d$date))))
for (nm in c("latitude", "longitude", "R", "G", "B")) d[[nm]] <- as.numeric(d[[nm]])
d$DOY <- as.integer(format(d$date, "%j"))

identity_excluded <- rep(FALSE, nrow(d))
if ("duplicate_image_sha256" %in% names(d)) identity_excluded <- identity_excluded | nonempty(d$duplicate_image_sha256)
if ("photo_coordinate_qc_status" %in% names(d)) identity_excluded <- identity_excluded | grepl("duplicate", d$photo_coordinate_qc_status, ignore.case = TRUE)

complete_input <- complete.cases(d[c("date", "latitude", "longitude", "R", "G", "B")]) &
  is.finite(d$latitude) & is.finite(d$longitude) &
  d$latitude >= 20 & d$latitude <= 50 & d$longitude >= 120 & d$longitude <= 150

excluded_identity <- d[identity_excluded, intersect(c("observation_id", "photo_id", "image_sha256", "duplicate_image_sha256", "latitude", "longitude"), names(d)), drop = FALSE]
excluded_input <- d[!complete_input, intersect(c("observation_id", "date", "latitude", "longitude", "R", "G", "B"), names(d)), drop = FALSE]
d <- d[!identity_excluded & complete_input, , drop = FALSE]
if (nrow(d) < 50L) stop("Too few usable canonical records.", call. = FALSE)

rgb <- pmin(pmax(as.matrix(d[c("R", "G", "B")]) / 255, 0), 1)
linear <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
xyz <- linear %*% t(matrix(c(
  0.4124564, 0.3575761, 0.1804375,
  0.2126729, 0.7151522, 0.0721750,
  0.0193339, 0.1191920, 0.9503041
), nrow = 3, byrow = TRUE))
xyz <- sweep(xyz, 2, c(0.95047, 1, 1.08883), "/")
epsilon <- (6 / 29)^3
f <- ifelse(xyz > epsilon, xyz^(1 / 3), xyz / (3 * (6 / 29)^2) + 4 / 29)
d$L <- 116 * f[, 2] - 16
d$a <- 500 * (f[, 1] - f[, 2])
d$b <- 200 * (f[, 2] - f[, 3])

# -----------------------------------------------------------------------------
# 2. Legacy environmental structure. AI is replaced only by CHELSA CMI.
# -----------------------------------------------------------------------------
env_files <- c(
  chelsa_bio05 = "chelsa_bio05.tif",
  chelsa_bio10 = "chelsa_bio10.tif",
  chelsa_gdd5 = "chelsa_gdd5.tif",
  chelsa_cmimean = "chelsa_cmimean.tif",
  chelsa_vpdmean = "chelsa_vpdmean.tif",
  chelsa_bio12 = "chelsa_bio12.tif",
  chelsa_bio14 = "chelsa_bio14.tif",
  chelsa_bio15 = "chelsa_bio15.tif",
  chelsa_swb = "chelsa_swb.tif",
  chelsa_rsdsmean = "chelsa_rsdsmean.tif",
  soilgrids_bdod_0_5cm = "soilgrids_bdod_0_5cm_mean.tif",
  soilgrids_cfvo_0_5cm = "soilgrids_cfvo_0_5cm_mean.tif",
  soilgrids_sand_0_5cm = "soilgrids_sand_0_5cm_mean.tif",
  soilgrids_silt_0_5cm = "soilgrids_silt_0_5cm_mean.tif",
  soilgrids_nitrogen_0_5cm = "soilgrids_nitrogen_0_5cm_mean.tif",
  soilgrids_ocd_0_5cm = "soilgrids_ocd_0_5cm_mean.tif",
  soilgrids_soc_0_5cm = "soilgrids_soc_0_5cm_mean.tif",
  soilgrids_phh2o_0_5cm = "soilgrids_phh2o_0_5cm_mean.tif"
)
env_paths <- file.path(env_dir, unname(env_files))
env_values <- extract_stack(env_paths, names(env_files), d)
d <- cbind(d, env_values)

# Derive the same topographic family used by the legacy workflow.
elevation_path <- file.path(env_dir, "elevation_30s.tif")
if (!file.exists(elevation_path)) stop("Missing elevation raster: ", elevation_path, call. = FALSE)
elevation <- rast(elevation_path)
topography <- c(
  terrain(elevation, v = "roughness"),
  terrain(elevation, v = "slope", unit = "radians"),
  terrain(elevation, v = "TRI")
)
names(topography) <- c("roughness", "slope", "TRI")
topography_values <- extract_stack(rep(elevation_path, 1), "elevation_unused", d)
pts_topo <- vect(d[c("longitude", "latitude")], geom = c("longitude", "latitude"), crs = "EPSG:4326")
if (!same.crs(pts_topo, topography)) pts_topo <- project(pts_topo, crs(topography))
topo_values <- as.data.frame(extract(topography, pts_topo, ID = FALSE))
if (nrow(topo_values) != nrow(d)) stop("Topography extraction changed row count.", call. = FALSE)
d <- cbind(d, topo_values)

pca_temperature <- pca_score(d, c("chelsa_bio05", "chelsa_bio10", "chelsa_gdd5"), "Temperature_PC1")
pca_moisture <- pca_score(d, c("chelsa_cmimean", "chelsa_vpdmean", "chelsa_bio12", "chelsa_bio14", "chelsa_bio15", "chelsa_swb"), "precip_PC1")
pca_soil_phys <- pca_score(d, c("soilgrids_bdod_0_5cm", "soilgrids_cfvo_0_5cm", "soilgrids_sand_0_5cm", "soilgrids_silt_0_5cm"), "soil_phys_PC1")
pca_soil_nutrient <- pca_score(d, c("soilgrids_nitrogen_0_5cm", "soilgrids_ocd_0_5cm", "soilgrids_soc_0_5cm"), "soil_nutrient_PC1")
pca_topography <- pca_score(d, c("roughness", "slope", "TRI"), "topo_PC1")

d$Temperature_PC1 <- pca_temperature$score
d$precip_PC1 <- pca_moisture$score
d$soil_phys_PC1 <- pca_soil_phys$score
d$soil_nutrient_PC1 <- pca_soil_nutrient$score
d$topo_PC1 <- pca_topography$score
d$soil_pH <- d$soilgrids_phh2o_0_5cm
d$RSDS <- d$chelsa_rsdsmean

pca_loadings <- do.call(rbind, list(
  pca_temperature$loading,
  pca_moisture$loading,
  pca_soil_phys$loading,
  pca_soil_nutrient$loading,
  pca_topography$loading
))

# -----------------------------------------------------------------------------
# 3. Five legacy Bombus SDMs and alternative availability summaries.
# -----------------------------------------------------------------------------
species <- c("ardens", "beaticola", "consobrinus", "diversus", "honshuensis")
sdm_paths <- file.path(sdm_dir, paste0(species, ".tif"))
sdm_values <- extract_stack(sdm_paths, species, d)
d <- cbind(d, sdm_values)
P <- as.matrix(d[species])
valid_sdm <- complete.cases(P) & apply(P, 1, function(x) all(is.finite(x) & x >= 0 & x <= 1))
excluded_sdm <- d[!valid_sdm, c("observation_id", "latitude", "longitude"), drop = FALSE]
d <- d[valid_sdm, , drop = FALSE]
P <- as.matrix(d[species])

d$Bombus_suitability_sum <- rowSums(P)
d$Bombus_any_availability <- 1 - apply(1 - P, 1, prod)
d$Bombus_max_availability <- apply(P, 1, max)
d$spatial_widespread <- 1 - apply(1 - as.matrix(d[c("ardens", "diversus")]), 1, prod)
d$spatial_montane <- 1 - apply(1 - as.matrix(d[c("beaticola", "consobrinus", "honshuensis")]), 1, prod)

# -----------------------------------------------------------------------------
# 4. One common complete-case cohort and fixed legacy environment specification.
# -----------------------------------------------------------------------------
environment_terms_raw <- c(
  "DOY", "topo_PC1", "Temperature_PC1", "precip_PC1",
  "soil_nutrient_PC1", "soil_phys_PC1", "soil_pH", "RSDS"
)
common_vars <- c("a", "latitude", "longitude", environment_terms_raw,
                 "Bombus_suitability_sum", "Bombus_any_availability", "Bombus_max_availability")
complete_model <- complete.cases(d[common_vars]) & vapply(seq_len(nrow(d)), function(i) all(is.finite(as.numeric(d[i, common_vars]))), logical(1))
excluded_model <- d[!complete_model, c("observation_id", "latitude", "longitude"), drop = FALSE]
d <- d[complete_model, , drop = FALSE]
if (nrow(d) < 50L) stop("Too few common complete cases.", call. = FALSE)

standardize <- c(environment_terms_raw, "Bombus_suitability_sum", "Bombus_any_availability", "Bombus_max_availability")
for (nm in standardize) d[[paste0("z_", nm)]] <- z(d[[nm]])
d$y <- z(d$a)

environment_terms <- paste0("z_", environment_terms_raw)
model_terms <- list(
  environment_only = environment_terms,
  environment_plus_bombus_sum = c(environment_terms, "z_Bombus_suitability_sum"),
  environment_plus_bombus_any = c(environment_terms, "z_Bombus_any_availability"),
  environment_plus_bombus_max = c(environment_terms, "z_Bombus_max_availability")
)

# -----------------------------------------------------------------------------
# 5. Reviewed national SPDE-INLA model.
# -----------------------------------------------------------------------------
laea_japan <- "+proj=laea +lat_0=36 +lon_0=138 +datum=WGS84 +units=m +no_defs"
pts_sf <- st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
pts_m <- st_transform(pts_sf, crs = laea_japan)
loc_m <- st_coordinates(pts_m)

boundary <- inla.nonconvex.hull(loc_m, convex = -0.05, resolution = 100000)
mesh <- inla.mesh.2d(
  loc = loc_m,
  boundary = boundary,
  max.edge = c(30000, 150000),
  cutoff = 5000,
  offset = c(30000, 150000)
)
spde <- inla.spde2.pcmatern(
  mesh = mesh,
  alpha = 2,
  prior.range = c(100000, 0.05),
  prior.sigma = c(1, 0.05)
)
A <- inla.spde.make.A(mesh, loc = loc_m)

fit_one <- function(model_name, terms) {
  X <- data.frame(Intercept = 1, d[terms], check.names = FALSE)
  stack <- inla.stack(
    data = list(y = d$y),
    A = list(A, 1),
    effects = list(spatial = seq_len(spde$n.spde), fixed = X),
    tag = "est"
  )
  formula <- as.formula(paste("y ~", paste(c("0 + Intercept", terms, "f(spatial, model = spde)"), collapse = " + ")))
  fit <- inla(
    formula,
    family = "gaussian",
    data = inla.stack.data(stack),
    control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
    control.compute = list(waic = TRUE, dic = TRUE, cpo = TRUE, config = TRUE),
    control.inla = list(strategy = "adaptive"),
    verbose = FALSE
  )
  if (is.null(fit$waic$waic) || !is.finite(fit$waic$waic)) stop("INLA failed for ", model_name, call. = FALSE)
  list(name = model_name, terms = terms, formula = formula, fit = fit)
}

fits <- Map(fit_one, names(model_terms), model_terms)
names(fits) <- names(model_terms)

model_comparison <- do.call(rbind, lapply(fits, function(obj) {
  cpo <- obj$fit$cpo$cpo
  data.frame(
    model = obj$name,
    fixed_terms = paste(obj$terms, collapse = " + "),
    n = nrow(d),
    mesh_vertices = mesh$n,
    WAIC = obj$fit$waic$waic,
    pWAIC = obj$fit$waic$p.eff,
    DIC = obj$fit$dic$dic,
    mean_neglogCPO = mean(-log(cpo[is.finite(cpo) & cpo > 0])),
    failed_CPO = sum(!is.finite(cpo) | cpo <= 0),
    stringsAsFactors = FALSE
  )
}))
model_comparison$delta_WAIC <- model_comparison$WAIC - min(model_comparison$WAIC)
model_comparison <- model_comparison[order(model_comparison$WAIC), , drop = FALSE]

fixed_effects <- do.call(rbind, lapply(fits, function(obj) {
  out <- as.data.frame(obj$fit$summary.fixed)
  out$term <- rownames(out)
  out$model <- obj$name
  rownames(out) <- NULL
  out[c("model", "term", setdiff(names(out), c("model", "term")))]
}))

hyperparameters <- do.call(rbind, lapply(fits, function(obj) {
  out <- as.data.frame(obj$fit$summary.hyperpar)
  out$parameter <- rownames(out)
  out$model <- obj$name
  rownames(out) <- NULL
  out[c("model", "parameter", setdiff(names(out), c("model", "parameter")))]
}))

# -----------------------------------------------------------------------------
# 6. Same-cohort spatially blocked predictive sensitivity.
# -----------------------------------------------------------------------------
axis1 <- prcomp(scale(loc_m), center = FALSE, scale. = FALSE)$x[, 1]
breaks <- unique(quantile(axis1, probs = seq(0, 1, length.out = 6), type = 1))
if (length(breaks) != 6L) stop("Could not construct five spatial folds.", call. = FALSE)
d$fold <- cut(axis1, breaks = breaks, include.lowest = TRUE, labels = FALSE)

cv_rows <- list()
for (model_name in names(model_terms)) {
  terms <- model_terms[[model_name]]
  formula <- as.formula(paste("y ~", paste(terms, collapse = " + ")))
  for (fold in 1:5) {
    train <- d$fold != fold
    test <- d$fold == fold
    fit <- lm(formula, data = d[train, , drop = FALSE])
    prediction <- predict(fit, newdata = d[test, , drop = FALSE])
    denom <- sum((d$y[test] - mean(d$y[train]))^2)
    cv_rows[[paste(model_name, fold)]] <- data.frame(
      model = model_name,
      fold = fold,
      n_test = sum(test),
      RMSE = sqrt(mean((d$y[test] - prediction)^2)),
      MAE = mean(abs(d$y[test] - prediction)),
      Q2 = if (denom > 0) 1 - sum((d$y[test] - prediction)^2) / denom else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}
blocked_cv <- do.call(rbind, cv_rows)
blocked_cv_summary <- aggregate(cbind(RMSE, MAE, Q2) ~ model, blocked_cv, mean)

spatial_group_diagnostic <- data.frame(
  index = c("spatial_widespread", "spatial_montane"),
  cor_longitude = c(cor(d$spatial_widespread, d$longitude), cor(d$spatial_montane, d$longitude)),
  cor_latitude = c(cor(d$spatial_widespread, d$latitude), cor(d$spatial_montane, d$latitude)),
  cor_projected_axis1 = c(cor(d$spatial_widespread, axis1), cor(d$spatial_montane, axis1)),
  cor_DOY = c(cor(d$spatial_widespread, d$DOY), cor(d$spatial_montane, d$DOY)),
  stringsAsFactors = FALSE
)

row_flow <- data.frame(
  stage = c("canonical_input", "identity_excluded", "incomplete_input_excluded", "after_input_qc", "missing_sdm_excluded", "model_incomplete_excluded", "common_model_cohort"),
  n = c(n_input, nrow(excluded_identity), nrow(excluded_input), n_input - nrow(excluded_identity) - nrow(excluded_input), nrow(excluded_sdm), nrow(excluded_model), nrow(d)),
  stringsAsFactors = FALSE
)

write.csv(model_comparison, file.path(out_dir, "spde_model_comparison.csv"), row.names = FALSE)
write.csv(fixed_effects, file.path(out_dir, "spde_fixed_effects.csv"), row.names = FALSE)
write.csv(hyperparameters, file.path(out_dir, "spde_hyperparameters.csv"), row.names = FALSE)
write.csv(blocked_cv, file.path(out_dir, "blocked_cv_by_fold.csv"), row.names = FALSE)
write.csv(blocked_cv_summary, file.path(out_dir, "blocked_cv_summary.csv"), row.names = FALSE)
write.csv(spatial_group_diagnostic, file.path(out_dir, "spatial_group_diagnostic.csv"), row.names = FALSE)
write.csv(pca_loadings, file.path(out_dir, "environment_pca_loadings.csv"), row.names = FALSE)
write.csv(row_flow, file.path(out_dir, "row_flow.csv"), row.names = FALSE)
write.csv(excluded_identity, file.path(out_dir, "excluded_identity.csv"), row.names = FALSE)
write.csv(excluded_input, file.path(out_dir, "excluded_incomplete_input.csv"), row.names = FALSE)
write.csv(excluded_sdm, file.path(out_dir, "excluded_missing_sdm.csv"), row.names = FALSE)
write.csv(excluded_model, file.path(out_dir, "excluded_model_incomplete.csv"), row.names = FALSE)
write.csv(
  d[c("observation_id", "date", "latitude", "longitude", "DOY", "L", "a", "b", "fold",
      "Temperature_PC1", "precip_PC1", "topo_PC1", "soil_phys_PC1", "soil_nutrient_PC1", "soil_pH", "RSDS",
      species, "Bombus_suitability_sum", "Bombus_any_availability", "Bombus_max_availability",
      "spatial_widespread", "spatial_montane")],
  file.path(out_dir, "analysis_data.csv"), row.names = FALSE
)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))

print(row_flow)
print(model_comparison)
print(blocked_cv_summary)
