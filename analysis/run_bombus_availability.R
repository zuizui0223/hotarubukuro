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
sdm_dir <- if (length(args) >= 2L) args[[2L]] else "sdm"
out_dir <- if (length(args) >= 3L) args[[3L]] else "results/bombus_availability"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

species <- c("ardens", "beaticola", "consobrinus", "diversus", "honshuensis")
sdm_paths <- file.path(sdm_dir, paste0(species, ".tif"))
if (!file.exists(input)) stop("Input not found: ", input, call. = FALSE)
if (!all(file.exists(sdm_paths))) {
  stop("Missing Bombus raster(s): ", paste(basename(sdm_paths[!file.exists(sdm_paths)]), collapse = ", "), call. = FALSE)
}

# -----------------------------------------------------------------------------
# 1. Read the actual analysis input and derive apparent flower colour.
# -----------------------------------------------------------------------------
d <- read.csv(input, check.names = FALSE, fileEncoding = "UTF-8-BOM")
required <- c("date", "latitude", "longitude", "R", "G", "B")
missing <- setdiff(required, names(d))
if (length(missing)) stop("Input is missing: ", paste(missing, collapse = ", "), call. = FALSE)

d$observation_id <- seq_len(nrow(d))
d$date <- as.Date(gsub("/", "-", trimws(as.character(d$date))))
for (nm in c("latitude", "longitude", "R", "G", "B")) d[[nm]] <- as.numeric(d[[nm]])
d$DOY <- as.integer(format(d$date, "%j"))
complete_input <- complete.cases(d[required]) &
  is.finite(d$latitude) & is.finite(d$longitude) &
  d$latitude >= 20 & d$latitude <= 50 & d$longitude >= 120 & d$longitude <= 150
excluded <- d[!complete_input, c("observation_id", required), drop = FALSE]
d <- d[complete_input, , drop = FALSE]
if (nrow(d) < 50L) stop("Too few complete records.", call. = FALSE)

# Explicit IEC 61966-2-1 sRGB -> CIELAB D65. This remains display-referred colour.
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
# 2. Attach the five retained Bombus suitability surfaces by row-safe extraction.
# -----------------------------------------------------------------------------
sdm <- terra::rast(sdm_paths)
names(sdm) <- species
if (terra::nlyr(sdm) != length(species)) stop("Unexpected SDM layer count.", call. = FALSE)
points <- terra::vect(d[c("longitude", "latitude")], geom = c("longitude", "latitude"), crs = "EPSG:4326")
pred <- as.data.frame(terra::extract(sdm, points, ID = FALSE), check.names = FALSE)
if (nrow(pred) != nrow(d)) stop("SDM extraction changed row count.", call. = FALSE)
for (nm in species) d[[nm]] <- as.numeric(pred[[nm]])

complete_sdm <- complete.cases(d[species])
sdm_excluded <- d[!complete_sdm, c("observation_id", "latitude", "longitude"), drop = FALSE]
d <- d[complete_sdm, , drop = FALSE]
P <- as.matrix(d[species])
if (any(!is.finite(P)) || any(P < 0 | P > 1)) stop("SDM values must be finite and within [0,1].", call. = FALSE)

d$Bombus_suitability_sum <- rowSums(P)
d$Bombus_any_availability <- 1 - apply(1 - P, 1, prod)
d$Bombus_max_availability <- apply(P, 1, max)

# Broad-distribution versus montane summaries are spatial diagnostics, not
# pollinator functional groups and not candidate biological effects.
widespread <- c("ardens", "diversus")
montane <- c("beaticola", "consobrinus", "honshuensis")
d$spatial_widespread <- 1 - apply(1 - as.matrix(d[widespread]), 1, prod)
d$spatial_montane <- 1 - apply(1 - as.matrix(d[montane]), 1, prod)

# -----------------------------------------------------------------------------
# 3. Standardize the response and fixed effects once on the common cohort.
# -----------------------------------------------------------------------------
z <- function(x) as.numeric(scale(x))
d$y <- z(d$a)
d$z_DOY <- z(d$DOY)
indices <- c("Bombus_suitability_sum", "Bombus_any_availability", "Bombus_max_availability")
for (nm in indices) d[[paste0("z_", nm)]] <- z(d[[nm]])

# -----------------------------------------------------------------------------
# 4. Build a national Japan mesh in a metric equal-area projection.
#    The old EPSG:3857 mesh is not reused because its distance distortion is
#    inappropriate for nationwide ecological range parameters.
# -----------------------------------------------------------------------------
laea_japan <- "+proj=laea +lat_0=36 +lon_0=138 +datum=WGS84 +units=m +no_defs"
pts_sf <- sf::st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
pts_m <- sf::st_transform(pts_sf, crs = laea_japan)
loc_m <- sf::st_coordinates(pts_m)

boundary <- INLA::inla.nonconvex.hull(loc_m, convex = -0.05, resolution = 100000)
mesh <- INLA::inla.mesh.2d(
  loc = loc_m,
  boundary = boundary,
  max.edge = c(30000, 150000),
  cutoff = 5000,
  offset = c(30000, 150000)
)

# PC priors are explicit: P(range < 100 km)=0.05 and P(sigma > 1)=0.05
# on the standardized response scale.
spde <- INLA::inla.spde2.pcmatern(
  mesh = mesh,
  alpha = 2,
  prior.range = c(100000, 0.05),
  prior.sigma = c(1, 0.05)
)
A <- INLA::inla.spde.make.A(mesh, loc = loc_m)

# -----------------------------------------------------------------------------
# 5. Fit only the four models needed by the paper.
# -----------------------------------------------------------------------------
model_terms <- list(
  spatial_DOY = c("z_DOY"),
  spatial_DOY_sum = c("z_DOY", "z_Bombus_suitability_sum"),
  spatial_DOY_any = c("z_DOY", "z_Bombus_any_availability"),
  spatial_DOY_max = c("z_DOY", "z_Bombus_max_availability")
)

fit_one <- function(model_name, terms) {
  X <- data.frame(Intercept = 1, d[terms], check.names = FALSE)
  stack <- INLA::inla.stack(
    data = list(y = d$y),
    A = list(A, 1),
    effects = list(spatial = seq_len(spde$n.spde), fixed = X),
    tag = "est"
  )
  rhs <- paste(c("0 + Intercept", terms, "f(spatial, model = spde)"), collapse = " + ")
  formula <- as.formula(paste("y ~", rhs))
  fit <- INLA::inla(
    formula,
    family = "gaussian",
    data = INLA::inla.stack.data(stack),
    control.predictor = list(A = INLA::inla.stack.A(stack), compute = TRUE),
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
# 6. Spatially blocked non-SPDE prediction check on the identical cohort.
#    This is a prediction sensitivity check, not a substitute for SPDE inference.
# -----------------------------------------------------------------------------
axis1 <- stats::prcomp(scale(loc_m), center = FALSE, scale. = FALSE)$x[, 1]
breaks <- unique(stats::quantile(axis1, probs = seq(0, 1, length.out = 6), type = 1))
if (length(breaks) != 6L) stop("Could not construct five spatial folds.", call. = FALSE)
d$fold <- cut(axis1, breaks = breaks, include.lowest = TRUE, labels = FALSE)

cv_formula <- function(terms) as.formula(paste("y ~", paste(terms, collapse = " + ")))
cv_rows <- list()
for (model_name in names(model_terms)) {
  terms <- model_terms[[model_name]]
  form <- cv_formula(terms)
  prediction <- rep(NA_real_, nrow(d))
  for (fold in 1:5) {
    train <- d$fold != fold
    test <- d$fold == fold
    lm_fit <- lm(form, data = d[train, , drop = FALSE])
    prediction[test] <- predict(lm_fit, newdata = d[test, , drop = FALSE])
    denom <- sum((d$y[test] - mean(d$y[train]))^2)
    cv_rows[[paste(model_name, fold)]] <- data.frame(
      model = model_name,
      fold = fold,
      n_test = sum(test),
      RMSE = sqrt(mean((d$y[test] - prediction[test])^2)),
      MAE = mean(abs(d$y[test] - prediction[test])),
      Q2 = if (denom > 0) 1 - sum((d$y[test] - prediction[test])^2) / denom else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}
blocked_cv <- do.call(rbind, cv_rows)
blocked_cv_summary <- aggregate(cbind(RMSE, MAE, Q2) ~ model, blocked_cv, mean)

# -----------------------------------------------------------------------------
# 7. Diagnose the broad-distribution/montane split strictly as spatial structure.
# -----------------------------------------------------------------------------
spatial_group_diagnostic <- data.frame(
  index = c("spatial_widespread", "spatial_montane"),
  cor_longitude = c(cor(d$spatial_widespread, d$longitude), cor(d$spatial_montane, d$longitude)),
  cor_latitude = c(cor(d$spatial_widespread, d$latitude), cor(d$spatial_montane, d$latitude)),
  cor_projected_axis1 = c(cor(d$spatial_widespread, axis1), cor(d$spatial_montane, axis1)),
  cor_DOY = c(cor(d$spatial_widespread, d$DOY), cor(d$spatial_montane, d$DOY)),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# 8. Write only analysis-ready tables and compact diagnostics.
# -----------------------------------------------------------------------------
write.csv(model_comparison, file.path(out_dir, "spde_model_comparison.csv"), row.names = FALSE)
write.csv(fixed_effects, file.path(out_dir, "spde_fixed_effects.csv"), row.names = FALSE)
write.csv(hyperparameters, file.path(out_dir, "spde_hyperparameters.csv"), row.names = FALSE)
write.csv(blocked_cv, file.path(out_dir, "blocked_cv_by_fold.csv"), row.names = FALSE)
write.csv(blocked_cv_summary, file.path(out_dir, "blocked_cv_summary.csv"), row.names = FALSE)
write.csv(spatial_group_diagnostic, file.path(out_dir, "spatial_group_diagnostic.csv"), row.names = FALSE)
write.csv(
  d[c("observation_id", "date", "latitude", "longitude", "DOY", "L", "a", "b", "fold",
      species, "Bombus_suitability_sum", "Bombus_any_availability", "Bombus_max_availability",
      "spatial_widespread", "spatial_montane")],
  file.path(out_dir, "analysis_data.csv"), row.names = FALSE
)
write.csv(excluded, file.path(out_dir, "excluded_incomplete_input.csv"), row.names = FALSE)
write.csv(sdm_excluded, file.path(out_dir, "excluded_missing_sdm.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))

cat("Completed minimal end-to-end workflow with", nrow(d), "records.\n")
print(model_comparison)
print(blocked_cv_summary)
print(spatial_group_diagnostic)
