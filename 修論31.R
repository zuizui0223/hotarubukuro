---
  title: "Flower color pipeline"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

## FINAL PIPELINE

### 0. Packages

```{r packages}
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(sf)
  library(terra)
  library(lavaan)
  library(tibble)
  library(INLA)
  library(qgam)
  library(mgcv)
  library(ggplot2)
  library(forcats)
})

select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
across <- dplyr::across

out_root <- path.expand("~/zui_results")
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

inla_dir <- file.path(out_root, "INLA_outputs")
plot_dir <- file.path(inla_dir, "plots")
gam_dir  <- file.path(out_root, "qGAM_outputs_final")

dir.create(inla_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(gam_dir,  recursive = TRUE, showWarnings = FALSE)
```

### 1. Load CSV & compute HSV / Lab / C / Lm / hex

```{r load-color}
in_csv <- "~/Desktop/1006.csv"
df <- read.csv(in_csv, stringsAsFactors = FALSE)

stopifnot(all(c("R","G","B") %in% names(df)))
stopifnot(all(c("longitude","latitude") %in% names(df)))

if (!("sample_id" %in% names(df))) {
  df$sample_id <- seq_len(nrow(df))
}

## HSV
hsv_mat <- t(grDevices::rgb2hsv(df$R, df$G, df$B))
colnames(hsv_mat) <- c("H","S","V")
df <- cbind(df, as.data.frame(hsv_mat))

## Lab
rgb01 <- pmin(pmax(as.matrix(df[, c("R","G","B")]) / 255, 0), 1)
lab_mat <- grDevices::convertColor(rgb01, from = "sRGB", to = "Lab")
df$L <- lab_mat[,1]
df$a <- lab_mat[,2]
df$b <- lab_mat[,3]
df$C  <- sqrt(df$a^2 + df$b^2)
df$Lm <- -df$L

## hex
rng_rgb <- range(c(df$R, df$G, df$B), na.rm = TRUE)
is_01 <- (rng_rgb[1] >= 0 && rng_rgb[2] <= 1)
R <- df$R; G <- df$G; B <- df$B
if (is_01) {
  R <- pmin(pmax(round(R*255),0),255)
  G <- pmin(pmax(round(G*255),0),255)
  B <- pmin(pmax(round(B*255),0),255)
}
df$col_hex <- rgb(R,G,B, maxColorValue = 255)

cat("✓ Loaded & color features computed. n =", nrow(df), "\n")
```

### 2. Raster extraction

```{r raster-extraction}
chelsa_dir <- "~/Desktop/stress2"
soil_dir   <- "~/Desktop/soil2"
bee_dir    <- "~/Desktop/bee5"
human_dir  <- "~/Desktop/human"

coords <- df %>%
  select(longitude, latitude) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(complete.cases(.))

extract_folder_terra <- function(path, coords){
  files <- list.files(path, pattern = "\\.tif$", full.names = TRUE)
  if (!length(files)) return(NULL)
  
  pts <- terra::vect(coords, geom = c("longitude","latitude"), crs = "EPSG:4326")
  
  vals_list <- lapply(files, function(f){
    r <- terra::rast(f)
    pts_use <- if (!terra::same.crs(pts, r)) terra::project(pts, terra::crs(r)) else pts
    terra::extract(r, pts_use, ID = FALSE)[,1]
  })
  
  out <- as.data.frame(vals_list)
  names(out) <- tools::file_path_sans_ext(basename(files))
  out
}

chelsa_vals <- extract_folder_terra(chelsa_dir, coords)
soil_vals   <- extract_folder_terra(soil_dir, coords)
bee_vals    <- extract_folder_terra(bee_dir, coords)

n_row <- nrow(coords)
if (!is.null(chelsa_vals)) stopifnot(nrow(chelsa_vals) == n_row)
if (!is.null(soil_vals))   stopifnot(nrow(soil_vals)   == n_row)
if (!is.null(bee_vals))    stopifnot(nrow(bee_vals)    == n_row)

df <- cbind(df[seq_len(n_row), ], chelsa_vals, soil_vals, bee_vals)

## unique coords
df2 <- df %>% distinct(longitude, latitude, .keep_all = TRUE)
nm_all <- names(df2)

cat("✓ Raster extracted. df2 n =", nrow(df2), "\n")
```

### 3. DEM-derived topography

```{r dem-topography}
dem_file <- file.path(soil_dir, "elevation.tif")

if (file.exists(dem_file)) {
  dem_r <- terra::rast(dem_file)
  
  slope_r  <- terra::terrain(dem_r, v = "slope", unit = "radians")
  aspect_r <- terra::terrain(dem_r, v = "aspect", unit = "radians")
  tri_r    <- terra::terrain(dem_r, v = "TRI")
  tpi_r    <- terra::terrain(dem_r, v = "TPI")
  
  names(slope_r)  <- "slope"
  names(aspect_r) <- "aspect"
  names(tri_r)    <- "TRI"
  names(tpi_r)    <- "TPI"
  
  topo_stack <- c(slope_r, aspect_r, tri_r, tpi_r)
  
  pts <- terra::vect(
    df2 %>% dplyr::select(longitude, latitude),
    geom = c("longitude", "latitude"),
    crs = "EPSG:4326"
  )
  
  pts_use <- if (!terra::same.crs(pts, topo_stack)) {
    terra::project(pts, terra::crs(topo_stack))
  } else {
    pts
  }
  
  topo_vals <- terra::extract(topo_stack, pts_use, ID = FALSE)
  df2 <- cbind(df2, topo_vals)
  
  df2$northness <- cos(df2$aspect)
  df2$eastness  <- sin(df2$aspect)
  
  cat("✓ DEM-derived topographic variables added from:", dem_file, "\n")
} else {
  cat("! DEM file not found:", dem_file, "\n")
}
```

### 4. PCA

```{r pca}
set.seed(42)

## ---------- Temperature PCA ----------
temp_vars <- grep(
  "^(CHELSA_bio5_1981-2010_V\\.2\\.1|CHELSA_bio10_1981-2010_V\\.2\\.1|CHELSA_gdd5_1981-2010_V\\.2\\.1)$",
  names(df2), value = TRUE
)

Temperature_PC1 <- rep(NA_real_, nrow(df2))
temp_pc_obj <- NULL

if (length(temp_vars) >= 1) {
  Xt <- df2[, temp_vars, drop = FALSE]
  cc <- complete.cases(Xt)
  if (sum(cc) >= 3 && ncol(Xt) >= 2) {
    temp_pc_obj <- prcomp(Xt[cc, ], scale. = TRUE)
    Temperature_PC1[cc] <- temp_pc_obj$x[,1]
  } else if (ncol(Xt) == 1) {
    Temperature_PC1 <- as.numeric(scale(Xt[[1]]))
  }
}
df2$Temperature_PC1 <- Temperature_PC1

## ---------- Precip PCA ----------
precip_vars <- grep(
  "^(CHELSA_ai_1981-2010_V\\.2\\.1|CHELSA_vpd_mean_1981-2010_V\\.2\\.1|CHELSA_bio1[245]_1981-2010_V\\.2\\.1|CHELSA_swb_1981-2010_V\\.2\\.1)$",
  names(df2), value = TRUE
)

precip_PC1 <- rep(NA_real_, nrow(df2))
precip_pc_obj <- NULL

if (length(precip_vars) >= 1) {
  Xp <- df2[, precip_vars, drop = FALSE]
  cc <- complete.cases(Xp)
  if (sum(cc) >= 3 && ncol(Xp) >= 2) {
    precip_pc_obj <- prcomp(Xp[cc, ], scale. = TRUE)
    precip_PC1[cc] <- precip_pc_obj$x[,1]
  } else if (ncol(Xp) == 1) {
    precip_PC1 <- as.numeric(scale(Xp[[1]]))
  }
}
df2$precip_PC1 <- precip_PC1

## ---------- Soil physical PCA ----------
soil_phys_vars <- grep(
  "^(bdod_0[.-]5cm_mean|cfvo_0[.-]5cm_mean|sand_0[.-]5cm_mean|silt_0[.-]5cm_mean)$",
  names(df2), value = TRUE
)

soil_phys_PC1 <- rep(NA_real_, nrow(df2))
soil_phys_pc_obj <- NULL

if (length(soil_phys_vars) >= 1) {
  Xp <- df2[, soil_phys_vars, drop = FALSE]
  keep <- vapply(Xp, function(v) is.numeric(v) && sd(v, na.rm = TRUE) > 0, logical(1))
  Xp <- Xp[, keep, drop = FALSE]
  
  if (ncol(Xp) >= 2) {
    cc <- complete.cases(Xp)
    if (sum(cc) >= 3) {
      soil_phys_pc_obj <- prcomp(Xp[cc, ], scale. = TRUE)
      soil_phys_PC1[cc] <- soil_phys_pc_obj$x[,1]
    }
  } else if (ncol(Xp) == 1) {
    soil_phys_PC1 <- as.numeric(scale(Xp[[1]]))
  }
}
df2$soil_phys_PC1 <- soil_phys_PC1

## ---------- Soil chemical PCA ----------
soil_chem_vars <- grep(
  "^(nitrogen_0[.-]5cm_mean|ocd_0[.-]5cm_mean|soc_0[.-]5cm_mean|phh2o_0[.-]5cm_mean)$",
  names(df2), value = TRUE
)

soil_chem_PC1 <- rep(NA_real_, nrow(df2))
soil_chem_pc_obj <- NULL

if (length(soil_chem_vars) >= 1) {
  Xc <- df2[, soil_chem_vars, drop = FALSE]
  keep <- vapply(Xc, function(v) is.numeric(v) && sd(v, na.rm = TRUE) > 0, logical(1))
  Xc <- Xc[, keep, drop = FALSE]
  
  if (ncol(Xc) >= 2) {
    cc <- complete.cases(Xc)
    if (sum(cc) >= 3) {
      soil_chem_pc_obj <- prcomp(Xc[cc, ], scale. = TRUE)
      soil_chem_PC1[cc] <- soil_chem_pc_obj$x[,1]
    }
  } else if (ncol(Xc) == 1) {
    soil_chem_PC1 <- as.numeric(scale(Xc[[1]]))
  }
}
df2$soil_chem_PC1 <- soil_chem_PC1

## ---------- Topography PCA ----------
topo_vars <- intersect(c("roughness", "slope", "TRI"), names(df2))

topo_PC1 <- rep(NA_real_, nrow(df2))
topo_pc_obj <- NULL

if (length(topo_vars) >= 2) {
  Xt <- df2[, topo_vars, drop = FALSE]
  keep <- vapply(Xt, function(v) is.numeric(v) && sd(v, na.rm = TRUE) > 0, logical(1))
  Xt <- Xt[, keep, drop = FALSE]
  
  if (ncol(Xt) >= 2) {
    cc <- complete.cases(Xt)
    if (sum(cc) >= 3) {
      topo_pc_obj <- prcomp(Xt[cc, ], scale. = TRUE)
      topo_PC1[cc] <- topo_pc_obj$x[, 1]
    }
  } else if (ncol(Xt) == 1) {
    topo_PC1 <- as.numeric(scale(Xt[[1]]))
  }
}
df2$topo_PC1 <- topo_PC1

cat("✓ Climate / soil / topography PCA done.\n")
```

### 5. RSDS unify

```{r rsds}
rsds_candidates <- grep("rsds", names(df2), value = TRUE, ignore.case = TRUE)
stopifnot(length(rsds_candidates) > 0)
df2$RSDS <- df2[[rsds_candidates[1]]]
```

### 6. CFA Pigment latent factor

```{r cfa-pigment}
stopifnot(all(c("a","Lm","C") %in% names(df2)))

model_pig <- ' Pigment =~ 1*a + Lm + C '
fit_pig <- lavaan::cfa(
  model_pig,
  data      = df2[, c("a","Lm","C")],
  estimator = "MLR",
  std.lv    = FALSE,
  missing   = "fiml"
)

stopifnot(lavaan::inspect(fit_pig, "converged"))
df2$Pigment <- as.numeric(lavaan::lavPredict(fit_pig, type = "lv")[, "Pigment"])

cat("✓ Pigment factor computed.\n")
```

### 7. Bee richness

```{r bee-richness}
bee_cols <- intersect(c("ardens","beaticola","consobrinus","diversus","honshuensis"), names(df2))
stopifnot(length(bee_cols) > 0)
df2$Bee_Richness <- rowSums(df2[, bee_cols], na.rm = TRUE)
```

### 8. Export PCA loadings

```{r export-loadings}
if (!is.null(temp_pc_obj)) {
  temp_load <- as.data.frame(temp_pc_obj$rotation) %>% tibble::rownames_to_column("variable")
  write_csv(temp_load, file.path(inla_dir, "temperature_pca_loadings.csv"))
}
if (!is.null(precip_pc_obj)) {
  precip_load <- as.data.frame(precip_pc_obj$rotation) %>% tibble::rownames_to_column("variable")
  write_csv(precip_load, file.path(inla_dir, "precip_pca_loadings.csv"))
}
if (!is.null(soil_phys_pc_obj)) {
  soil_phys_load <- as.data.frame(soil_phys_pc_obj$rotation) %>% tibble::rownames_to_column("variable")
  write_csv(soil_phys_load, file.path(inla_dir, "soil_physical_pca_loadings.csv"))
}
if (!is.null(soil_chem_pc_obj)) {
  soil_chem_load <- as.data.frame(soil_chem_pc_obj$rotation) %>% tibble::rownames_to_column("variable")
  write_csv(soil_chem_load, file.path(inla_dir, "soil_chemical_pca_loadings.csv"))
}
if (!is.null(topo_pc_obj)) {
  topo_load <- as.data.frame(topo_pc_obj$rotation) %>% tibble::rownames_to_column("variable")
  write_csv(topo_load, file.path(inla_dir, "topography_pca_loadings.csv"))
}
```

### 9. Predictor correlation check

```{r predictor-correlation}
need_corr <- c(
  "Pigment",
  "topo_PC1", "TPI", "RSDS",
  "Temperature_PC1", "precip_PC1",
  "soil_phys_PC1", "soil_chem_PC1",
  "Bee_Richness",
  "longitude", "latitude"
)

topo_extra <- intersect(c("northness","eastness"), names(df2))

dat_corr <- df2 %>%
  select(
    Pigment,
    topo_PC1, TPI, RSDS,
    Temperature_PC1, precip_PC1,
    soil_phys_PC1, soil_chem_PC1,
    Bee_Richness,
    longitude, latitude,
    any_of(c("northness","eastness")),
    any_of("date")
  ) %>%
  filter(complete.cases(
    Pigment, topo_PC1, TPI, RSDS,
    Temperature_PC1, precip_PC1,
    soil_phys_PC1, soil_chem_PC1,
    Bee_Richness
  ))

vars_check <- c(
  "topo_PC1", "TPI", "RSDS", "Temperature_PC1", "precip_PC1",
  "soil_phys_PC1", "soil_chem_PC1", "Bee_Richness",
  intersect(c("northness","eastness"), names(dat_corr))
)

cor_dat <- dat_corr %>%
  select(all_of(vars_check)) %>%
  filter(complete.cases(.))

cor_mat <- cor(cor_dat, use = "pairwise.complete.obs")
write.csv(cor_mat, file.path(inla_dir, "predictor_correlation_matrix_topopc1_split_soil.csv"), row.names = TRUE)

cor_df <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
names(cor_df) <- c("var1","var2","r")

high_cor <- cor_df %>%
  filter(var1 != var2) %>%
  mutate(abs_r = abs(r)) %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(var1, var2)), collapse = "___")) %>%
  ungroup() %>%
  distinct(pair, .keep_all = TRUE) %>%
  filter(abs_r >= 0.7) %>%
  arrange(desc(abs_r))

write_csv(high_cor, file.path(inla_dir, "predictor_high_correlation_pairs_topopc1_split_soil.csv"))
cat("✓ Predictor correlation check done.\n")
```

### 10. SPDE-INLA for adaptive gradients

```{r spde-inla}
need_inla <- c(
  "Pigment",
  "topo_PC1",
  "RSDS",
  "Temperature_PC1",
  "precip_PC1",
  "soil_phys_PC1",
  "Bee_Richness",
  "longitude", "latitude"
)

dat_inla <- df2 %>%
  dplyr::select(all_of(c(need_inla, intersect("date", names(df2))))) %>%
  dplyr::filter(complete.cases(across(all_of(need_inla))))

stopifnot(nrow(dat_inla) >= 50)

dat_inla <- dat_inla %>%
  dplyr::mutate(
    y = Pigment
  )

cat("✓ INLA data ready. n =", nrow(dat_inla), "\n")

sf_dat <- sf::st_as_sf(dat_inla, coords = c("longitude","latitude"), crs = 4326)
sf_m   <- sf::st_transform(sf_dat, 3857)
coords_m <- sf::st_coordinates(sf_m)

mesh <- INLA::inla.mesh.2d(
  loc      = coords_m,
  max.edge = c(20000, 100000),
  cutoff   = 5000
)

spde <- INLA::inla.spde2.matern(mesh, alpha = 2)
A    <- INLA::inla.spde.make.A(mesh, loc = coords_m)

X <- data.frame(
  Intercept       = 1,
  topo_PC1        = dat_inla$topo_PC1,
  RSDS            = dat_inla$RSDS,
  Temperature_PC1 = dat_inla$Temperature_PC1,
  precip_PC1      = dat_inla$precip_PC1,
  soil_phys_PC1   = dat_inla$soil_phys_PC1,
  Bee_Richness    = dat_inla$Bee_Richness
)

stk <- INLA::inla.stack(
  data    = list(y = dat_inla$y),
  A       = list(A, 1),
  effects = list(
    s = 1:spde$n.spde,
    X = X
  ),
  tag     = "est"
)

formula_main <- y ~ 0 + Intercept +
  topo_PC1 + RSDS + Temperature_PC1 + precip_PC1 +
  soil_phys_PC1 + Bee_Richness +
  f(s, model = spde)

fit_main <- INLA::inla(
  formula = formula_main,
  data    = INLA::inla.stack.data(stk),
  family  = "gaussian",
  control.predictor = list(A = INLA::inla.stack.A(stk), compute = TRUE),
  control.compute   = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE)
)

cat("✓ Main adaptive-gradient SPDE-INLA model fitted.\n")
```

### 11. Fixed effects summary

```{r fixed-effects-summary}
fixef_main <- fit_main$summary.fixed %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term")

write_csv(fixef_main, file.path(inla_dir, "adaptive_spde_inla_fixed_effects_rawPigment.csv"))
print(fixef_main)
```

### 12. Variance decomposition

```{r variance-decomposition}
IDX <- INLA::inla.stack.index(stk, "est")$data

proj_obs <- INLA::inla.mesh.projector(mesh, loc = coords_m)
s_obs <- as.numeric(INLA::inla.mesh.project(proj_obs, fit_main$summary.random$s$mean))

eta <- fit_main$summary.linear.predictor$mean[IDX]
fixed_part <- eta - s_obs

prec_med <- fit_main$summary.hyperpar["Precision for the Gaussian observations", "0.5quant"]
resid_var <- 1 / prec_med

v_fixed   <- var(fixed_part, na.rm = TRUE)
v_spatial <- var(s_obs, na.rm = TRUE)
v_resid   <- resid_var
v_total   <- v_fixed + v_spatial + v_resid

var_decomp <- data.frame(
  component  = c("fixed", "spatial", "residual"),
  variance   = c(v_fixed, v_spatial, v_resid),
  proportion = c(v_fixed, v_spatial, v_resid) / v_total
)

write_csv(var_decomp, file.path(inla_dir, "adaptive_spde_inla_variance_decomposition_rawPigment.csv"))
print(var_decomp)
```

### 13. Spatial parameters

```{r spatial-parameters}
sh <- fit_main$summary.hyperpar
tau   <- exp(sh[grep("Theta1", rownames(sh)), "0.5quant"])
kappa <- exp(sh[grep("Theta2", rownames(sh)), "0.5quant"])
range_km <- sqrt(8) / kappa / 1000
sigma    <- 1 / (sqrt(4*pi) * tau * kappa)

spatial_pars <- data.frame(
  range_km = range_km,
  sigma    = sigma
)

write_csv(spatial_pars, file.path(inla_dir, "adaptive_spde_inla_spatial_parameters_rawPigment.csv"))
print(spatial_pars)
```

### 14. Residuals from SPDE-INLA

```{r residuals-spde}
y_obs    <- INLA::inla.stack.data(stk)$y[IDX]
yhat     <- fit_main$summary.fitted.values$mean[IDX]
yhat_sd  <- fit_main$summary.fitted.values$sd[IDX]

resid_raw  <- y_obs - yhat
resid_zfit <- resid_raw / pmax(as.numeric(yhat_sd), 1e-6)

dat_used <- dat_inla
dat_used$yhat       <- yhat
dat_used$yhat_sd    <- yhat_sd
dat_used$resid_raw  <- resid_raw
dat_used$resid_zfit <- resid_zfit

write_csv(dat_used, file.path(inla_dir, "adaptive_spde_inla_residuals_rawPigment.csv"))
cat("✓ Residuals exported.\n")
```

### 15. Fixed effect plot

```{r fixed-effect-plot}
coef_plot_tbl <- fixef_main %>%
  dplyr::filter(term != "Intercept") %>%
  dplyr::mutate(
    signif95 = (`0.025quant` > 0) | (`0.975quant` < 0),
    sig_label = ifelse(signif95, "95% CI excludes 0", "95% CI overlaps 0")
  )

term_labels <- c(
  topo_PC1        = "topography PC1",
  RSDS            = "solar radiation",
  Temperature_PC1 = "temperature PC1",
  precip_PC1      = "desiccation PC1",
  soil_phys_PC1   = "soil physical PC1",
  Bee_Richness    = "bumblebee richness"
)

coef_plot_tbl <- coef_plot_tbl %>%
  dplyr::mutate(
    term_pretty = dplyr::recode(term, !!!term_labels, .default = term),
    term_pretty = forcats::fct_reorder(term_pretty, mean)
  )

write_csv(coef_plot_tbl, file.path(inla_dir, "adaptive_spde_inla_fixed_effects_for_plot_rawPigment.csv"))

p_fixef <- ggplot(coef_plot_tbl,
                  aes(x = mean, y = term_pretty, shape = sig_label)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_errorbarh(aes(xmin = `0.025quant`, xmax = `0.975quant`), height = 0.18) +
  geom_point(size = 3) +
  labs(
    x = "Posterior mean coefficient",
    y = NULL,
    shape = NULL,
    title = "Fixed effects in the adaptive-gradient SPDE-INLA model",
    subtitle = "Bars show 95% credible intervals"
  ) +
  theme_bw(base_size = 12)

print(p_fixef)

ggsave(
  filename = file.path(plot_dir, "adaptive_spde_inla_fixed_effects_rawPigment.png"),
  plot = p_fixef,
  width = 7.5,
  height = 4.8,
  dpi = 300
)

sig_tbl <- coef_plot_tbl %>%
  dplyr::transmute(
    term,
    term_pretty,
    mean,
    lower95 = `0.025quant`,
    upper95 = `0.975quant`,
    signif95
  ) %>%
  dplyr::arrange(desc(signif95), desc(abs(mean)))

write_csv(sig_tbl, file.path(inla_dir, "adaptive_spde_inla_significance_summary_rawPigment.csv"))
print(sig_tbl)
```

### 16. qGAM on standardized residuals

```{r qgam}
MODE <- "fit"
POSITIVE_THRESH <- 1.5

K_SPACE <- 60
K_ELEV  <- 10
K_PD    <- 10
K_DOY   <- 12
K_INT2  <- c(8, 8)

ADD_DOYxPD <- FALSE
taus <- c(0.75, 0.85, 0.90, 0.95)

q_lo <- 0.10
q_hi <- 0.90
SPACE_AVERAGE <- "marginal"
FIXED_LONLAT  <- c(longitude = NA_real_, latitude = NA_real_)
elev0 <- median(df2$elevation, na.rm = TRUE)

if ("date" %in% names(dat_used)) {
  date_parsed <- suppressWarnings(as.Date(
    dat_used$date,
    tryFormats = c(
      "%Y-%m-%d","%Y/%m/%d","%Y.%m.%d","%Y%m%d",
      "%m/%d/%Y","%m-%d-%Y",
      "%Y-%m-%d %H:%M","%Y/%m/%d %H:%M",
      "%Y-%m-%d %H:%M:%S","%Y/%m/%d %H:%M:%S"
    )
  ))
  dat_used$DOY <- as.integer(strftime(date_parsed, "%j"))
} else {
  dat_used$DOY <- NA_integer_
}

human_tifs <- list.files(human_dir, pattern="\\.tif$", full.names=TRUE)
stopifnot(length(human_tifs) > 0)
r_human <- terra::rast(human_tifs[1])

pts_ll <- terra::vect(
  data.frame(lon = dat_used$longitude, lat = dat_used$latitude),
  geom = c("lon","lat"), crs = "EPSG:4326"
)
if (!terra::same.crs(pts_ll, r_human)) {
  pts_ll <- terra::project(pts_ll, terra::crs(r_human))
}
vals <- terra::extract(r_human, pts_ll, ID=FALSE)[,1]

dat_used$pop_density_raw <- vals
dat_used$pop_density_log <- log1p(vals)

if (!("elevation" %in% names(dat_used))) {
  dat_used <- dat_used %>%
    left_join(
      df2 %>% dplyr::select(longitude, latitude, elevation) %>% distinct(),
      by = c("longitude", "latitude")
    )
}

dat_used <- dat_used %>%
  dplyr::filter(
    is.finite(resid_zfit),
    is.finite(pop_density_log),
    is.finite(longitude), is.finite(latitude),
    is.finite(elevation)
  )

df_fit  <- dat_used
df_dark <- dat_used %>% dplyr::filter(resid_zfit > POSITIVE_THRESH)

df_fit_used <- if (MODE == "dark") df_dark else df_fit
df_pred     <- df_fit_used

use_DOY <- all(is.finite(df_fit_used$DOY)) && length(unique(df_fit_used$DOY)) >= 8

base_terms <- paste0(
  "s(longitude, latitude, k=", K_SPACE, ") + ",
  "s(elevation, k=", K_ELEV, ") + ",
  "s(pop_density_log, k=", K_PD, ")"
)

doy_terms <- if (use_DOY) {
  if (ADD_DOYxPD) {
    paste0(
      " + s(DOY, bs='tp', k=", K_DOY, ")",
      " + ti(DOY, pop_density_log, k=c(", K_INT2[1], ",", K_INT2[2], "))"
    )
  } else {
    paste0(" + s(DOY, bs='tp', k=", K_DOY, ")")
  }
} else ""

form_q <- as.formula(paste0("resid_zfit ~ ", base_terms, doy_terms))
cat("qGAM formula:\n")
print(form_q)

mods <- lapply(taus, function(tau) qgam::qgam(form_q, data = df_fit_used, qu = tau))
names(mods) <- paste0("q", taus)

sink(file.path(gam_dir, paste0("qGAM_summary_MODE_", MODE, ".txt")))
cat("===== DATA USED =====\n")
cat("MODE      =", MODE, "\n")
cat("n_df_used =", nrow(df_fit_used), "\n")
cat("use_DOY   =", use_DOY, "\n")
cat("POSITIVE_THRESH =", POSITIVE_THRESH, "\n\n")
cat("===== FORMULA =====\n")
print(form_q)
cat("\n")
cat("===== qGAM summaries =====\n\n")
for (nm in names(mods)) {
  cat("-----", nm, "-----\n")
  print(summary(mods[[nm]]))
  cat("\n")
}
sink()

write_csv(df_fit_used, file.path(gam_dir, paste0("df_used_MODE_", MODE, ".csv")))
```

### 17. qGAM effect-size helpers

```{r qgam-effect-size}
make_newdata_pair <- function(df_space,
                              elev0, DOY0,
                              pd_lo, pd_hi,
                              doy_lo, doy_hi,
                              space_average = c("marginal","fixed"),
                              fixed_lonlat = c(longitude=NA_real_, latitude=NA_real_)) {
  space_average <- match.arg(space_average)
  
  nd_pd_lo <- df_space; nd_pd_hi <- df_space
  nd_pd_lo$elevation <- elev0; nd_pd_hi$elevation <- elev0
  nd_pd_lo$pop_density_log <- pd_lo; nd_pd_hi$pop_density_log <- pd_hi
  if ("DOY" %in% names(df_space)) {
    nd_pd_lo$DOY <- DOY0
    nd_pd_hi$DOY <- DOY0
  }
  
  pd_mid <- mean(c(pd_lo, pd_hi))
  nd_doy_lo <- df_space; nd_doy_hi <- df_space
  nd_doy_lo$elevation <- elev0; nd_doy_hi$elevation <- elev0
  nd_doy_lo$pop_density_log <- pd_mid; nd_doy_hi$pop_density_log <- pd_mid
  if ("DOY" %in% names(df_space)) {
    nd_doy_lo$DOY <- doy_lo
    nd_doy_hi$DOY <- doy_hi
  }
  
  if (space_average == "fixed") {
    lon0 <- fixed_lonlat[["longitude"]]
    lat0 <- fixed_lonlat[["latitude"]]
    if (!is.finite(lon0)) lon0 <- median(df_space$longitude, na.rm=TRUE)
    if (!is.finite(lat0)) lat0 <- median(df_space$latitude,  na.rm=TRUE)
    
    nd_pd_lo$longitude <- lon0; nd_pd_lo$latitude <- lat0
    nd_pd_hi$longitude <- lon0; nd_pd_hi$latitude <- lat0
    nd_doy_lo$longitude <- lon0; nd_doy_lo$latitude <- lat0
    nd_doy_hi$longitude <- lon0; nd_doy_hi$latitude <- lat0
  }
  
  list(
    nd_pd_lo  = nd_pd_lo,
    nd_pd_hi  = nd_pd_hi,
    nd_doy_lo = nd_doy_lo,
    nd_doy_hi = nd_doy_hi
  )
}

deltaQ_PD <- function(mod, df_space, elev0, DOY0, pd_lo, pd_hi,
                      space_average = SPACE_AVERAGE, fixed_lonlat = FIXED_LONLAT) {
  nd <- make_newdata_pair(
    df_space,
    elev0 = elev0, DOY0 = DOY0,
    pd_lo = pd_lo, pd_hi = pd_hi,
    doy_lo = NA_real_, doy_hi = NA_real_,
    space_average = space_average,
    fixed_lonlat = fixed_lonlat
  )
  q_lo_hat <- mean(predict(mod, newdata = nd$nd_pd_lo, type = "response"), na.rm=TRUE)
  q_hi_hat <- mean(predict(mod, newdata = nd$nd_pd_hi, type = "response"), na.rm=TRUE)
  c(deltaQ_PD = q_hi_hat - q_lo_hat, Q_lo = q_lo_hat, Q_hi = q_hi_hat)
}

deltaQ_DOY <- function(mod, df_space, elev0, pd0, doy_lo, doy_hi,
                       space_average = SPACE_AVERAGE, fixed_lonlat = FIXED_LONLAT) {
  nd_lo <- df_space; nd_hi <- df_space
  nd_lo$elevation <- elev0; nd_hi$elevation <- elev0
  nd_lo$pop_density_log <- pd0; nd_hi$pop_density_log <- pd0
  nd_lo$DOY <- doy_lo; nd_hi$DOY <- doy_hi
  
  if (space_average == "fixed") {
    lon0 <- fixed_lonlat[["longitude"]]
    lat0 <- fixed_lonlat[["latitude"]]
    if (!is.finite(lon0)) lon0 <- median(df_space$longitude, na.rm=TRUE)
    if (!is.finite(lat0)) lat0 <- median(df_space$latitude,  na.rm=TRUE)
    nd_lo$longitude <- lon0; nd_lo$latitude <- lat0
    nd_hi$longitude <- lon0; nd_hi$latitude <- lat0
  }
  
  q_lo_hat <- mean(predict(mod, newdata = nd_lo, type = "response"), na.rm=TRUE)
  q_hi_hat <- mean(predict(mod, newdata = nd_hi, type = "response"), na.rm=TRUE)
  c(deltaQ_DOY = q_hi_hat - q_lo_hat, Q_lo = q_lo_hat, Q_hi = q_hi_hat)
}

pd_lo0  <- as.numeric(quantile(df_fit_used$pop_density_log, q_lo, na.rm=TRUE))
pd_hi0  <- as.numeric(quantile(df_fit_used$pop_density_log, q_hi, na.rm=TRUE))
pd_mid0 <- mean(c(pd_lo0, pd_hi0))

DOY0    <- if (use_DOY) median(df_fit_used$DOY, na.rm=TRUE) else NA_real_
doy_lo0 <- if (use_DOY) as.numeric(quantile(df_fit_used$DOY, q_lo, na.rm=TRUE)) else NA_real_
doy_hi0 <- if (use_DOY) as.numeric(quantile(df_fit_used$DOY, q_hi, na.rm=TRUE)) else NA_real_

point_PD <- lapply(taus, function(tau){
  mod <- mods[[paste0("q", tau)]]
  out <- deltaQ_PD(mod, df_space = df_pred, elev0 = elev0, DOY0 = DOY0, pd_lo = pd_lo0, pd_hi = pd_hi0)
  data.frame(
    tau = tau,
    deltaQ_PD = as.numeric(out["deltaQ_PD"]),
    Q_lo_PD   = as.numeric(out["Q_lo"]),
    Q_hi_PD   = as.numeric(out["Q_hi"])
  )
}) |> bind_rows()

point_DOY <- if (use_DOY) {
  lapply(taus, function(tau){
    mod <- mods[[paste0("q", tau)]]
    out <- deltaQ_DOY(mod, df_space = df_pred, elev0 = elev0, pd0 = pd_mid0, doy_lo = doy_lo0, doy_hi = doy_hi0)
    data.frame(
      tau = tau,
      deltaQ_DOY = as.numeric(out["deltaQ_DOY"]),
      Q_lo_DOY   = as.numeric(out["Q_lo"]),
      Q_hi_DOY   = as.numeric(out["Q_hi"])
    )
  }) |> bind_rows()
} else {
  data.frame(tau = taus, deltaQ_DOY = NA_real_, Q_lo_DOY = NA_real_, Q_hi_DOY = NA_real_)
}

point_DOY_by_PD <- if (use_DOY) {
  lapply(taus, function(tau){
    mod <- mods[[paste0("q", tau)]]
    lo <- deltaQ_DOY(mod, df_space = df_pred, elev0 = elev0, pd0 = pd_lo0, doy_lo = doy_lo0, doy_hi = doy_hi0)
    hi <- deltaQ_DOY(mod, df_space = df_pred, elev0 = elev0, pd0 = pd_hi0, doy_lo = doy_lo0, doy_hi = doy_hi0)
    data.frame(
      tau = tau,
      deltaQ_DOY_lowPD  = as.numeric(lo["deltaQ_DOY"]),
      deltaQ_DOY_highPD = as.numeric(hi["deltaQ_DOY"]),
      diff              = as.numeric(hi["deltaQ_DOY"]) - as.numeric(lo["deltaQ_DOY"])
    )
  }) |> bind_rows()
} else {
  data.frame(tau = taus, deltaQ_DOY_lowPD = NA_real_, deltaQ_DOY_highPD = NA_real_, diff = NA_real_)
}

res_point <- point_PD %>%
  left_join(point_DOY, by = "tau") %>%
  left_join(point_DOY_by_PD, by = "tau") %>%
  mutate(
    pd_lo = pd_lo0,
    pd_hi = pd_hi0,
    DOY0 = DOY0,
    elev0 = elev0,
    space_average = SPACE_AVERAGE,
    MODE = MODE
  )

write_csv(res_point, file.path(gam_dir, paste0("point_effect_sizes_MODE_", MODE, ".csv")))
print(res_point)
```

### 18. qGAM effect plots

```{r qgam-effect-plots}
plot_smooth_1d <- function(mod, data, varname, file_stub, n = 200) {
  xseq <- seq(min(data[[varname]], na.rm = TRUE),
              max(data[[varname]], na.rm = TRUE),
              length.out = n)
  
  nd <- data[rep(1, n), , drop = FALSE]
  nd[[varname]] <- xseq
  
  if ("longitude" %in% names(nd)) nd$longitude <- median(data$longitude, na.rm = TRUE)
  if ("latitude"  %in% names(nd)) nd$latitude  <- median(data$latitude,  na.rm = TRUE)
  if ("elevation" %in% names(nd) && varname != "elevation") nd$elevation <- median(data$elevation, na.rm = TRUE)
  if ("pop_density_log" %in% names(nd) && varname != "pop_density_log") nd$pop_density_log <- median(data$pop_density_log, na.rm = TRUE)
  if ("DOY" %in% names(nd) && varname != "DOY" && all(is.finite(data$DOY))) nd$DOY <- median(data$DOY, na.rm = TRUE)
  
  pr <- predict(mod, newdata = nd, se.fit = TRUE, type = "response")
  plot_df <- data.frame(
    x = xseq,
    fit = as.numeric(pr$fit),
    se  = as.numeric(pr$se.fit)
  ) %>%
    mutate(
      lo = fit - 1.96 * se,
      hi = fit + 1.96 * se
    )
  
  p <- ggplot(plot_df, aes(x = x, y = fit)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) +
    geom_line() +
    theme_bw(base_size = 12) +
    labs(x = varname, y = "Predicted upper-tail residual")
  
  print(p)
  
  ggsave(
    filename = file.path(gam_dir, paste0(file_stub, ".png")),
    plot = p,
    width = 5.8,
    height = 4.2,
    dpi = 300
  )
  
  write_csv(plot_df, file.path(gam_dir, paste0(file_stub, ".csv")))
}

mod_plot <- mods[["q0.95"]]

plot_smooth_1d(mod_plot, df_fit_used, "pop_density_log", "qGAM_q095_pop_density_log")
if (use_DOY) plot_smooth_1d(mod_plot, df_fit_used, "DOY", "qGAM_q095_DOY")
plot_smooth_1d(mod_plot, df_fit_used, "elevation", "qGAM_q095_elevation")

cat("\n✅ qGAM with log(pop_density) DONE\n")
cat(" - qGAM outputs:", gam_dir, "\n")
```

