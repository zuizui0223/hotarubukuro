## =========================================================
## 0. パッケージ & ユーティリティ
## =========================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr)
  library(ggplot2); library(forcats); library(scales)
  library(patchwork); library(cowplot)
  library(sf)
  library(terra)
  library(rnaturalearth)
  library(ggspatial)
  library(vegan)
  library(lavaan)
  library(mgcv)
  library(INLA)
  library(DiagrammeR); library(DiagrammeRsvg); library(rsvg)
})

## dplyr verbs を強制固定（他パッケージの select 等と競合しないように）
select    <- dplyr::select
filter    <- dplyr::filter
mutate    <- dplyr::mutate
left_join <- dplyr::left_join
across    <- dplyr::across
bind_rows <- dplyr::bind_rows

`%||%`    <- function(a,b) if(is.null(a)||length(a)==0||all(is.na(a))) b else a
zscale    <- function(x) if (is.numeric(x)) as.numeric(scale(x)) else x
pick_first <- function(v) if (length(v)) v[1] else character(0)

## 出力ルート（必ず書ける場所：ホーム直下）
out_root <- path.expand("~/sem_results")
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)


## =========================================================
## 1. データ読み込み & HSV / Lab / Pigment の計算
## =========================================================
df <- read.csv("~/Desktop/1006.csv", stringsAsFactors = FALSE)

stopifnot(all(c("R","G","B") %in% names(df)))

## HSV
hsv_mat <- t(grDevices::rgb2hsv(df$R, df$G, df$B))
colnames(hsv_mat) <- c("H","S","V")
df <- cbind(df, as.data.frame(hsv_mat))

## Lab（0–255 を 0–1 に落としてから）
rgb01 <- pmin(pmax(as.matrix(df[, c("R","G","B")]) / 255, 0), 1)
lab_mat <- grDevices::convertColor(rgb01, from = "sRGB", to = "Lab")
df$L <- lab_mat[,1]
df$a <- lab_mat[,2]
df$b <- lab_mat[,3]
df$C <- sqrt(df$a^2 + df$b^2)
df$Lm <- -df$L

## 色確認
print(head(df[, c("R","G","B","H","S","V","L","a","b","C","Lm")]))

## col_hex の作成
rng_rgb <- range(c(df$R, df$G, df$B), na.rm = TRUE)
is_01 <- (rng_rgb[1] >= 0 && rng_rgb[2] <= 1)
R <- df$R; G <- df$G; B <- df$B
if (is_01) {
  R <- pmin(pmax(round(R*255),0),255)
  G <- pmin(pmax(round(G*255),0),255)
  B <- pmin(pmax(round(B*255),0),255)
}
df$col_hex <- rgb(R,G,B, maxColorValue = 255)

## =========================================================
## 2. 環境ラスター値の抽出（CHELSA, soil, bee）
## =========================================================
## フォルダパス（必要に応じて書き換え）
chelsa_dir <- "~/Desktop/stress2"
soil_dir   <- "~/Desktop/soil2"
bee_dir    <- "~/Desktop/bee4"
human_dir  <- "~/Desktop/human"   # 人口密度用

## 緯度経度を numeric に
stopifnot(all(c("longitude","latitude") %in% names(df)))
coords <- df %>%
  select(longitude, latitude) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(complete.cases(.))
print(head(coords))

## terra でラスター抽出（NA がある場所は NA）
extract_folder_terra <- function(path, coords) {
  files <- list.files(path, pattern="\\.tif$", full.names=TRUE)
  if (!length(files)) return(NULL)
  pts <- vect(coords, geom = c("longitude","latitude"), crs = "EPSG:4326")
  vals_list <- lapply(files, function(f){
    r <- rast(f)
    if (!same.crs(pts, r)) {
      pts_use <- project(pts, crs(r))
    } else {
      pts_use <- pts
    }
    val <- terra::extract(r, pts_use, ID = FALSE)[,1]
    val
  })
  out <- as.data.frame(vals_list)
  names(out) <- tools::file_path_sans_ext(basename(files))
  out
}

## 各フォルダから抽出
chelsa_vals <- extract_folder_terra(chelsa_dir, coords)
soil_vals   <- extract_folder_terra(soil_dir, coords)
bee_vals    <- extract_folder_terra(bee_dir, coords)

## 抽出行数と coords 行数を合わせる（ズレたらエラー）
n_row <- nrow(coords)
if (!is.null(chelsa_vals)) stopifnot(nrow(chelsa_vals) == n_row)
if (!is.null(soil_vals))   stopifnot(nrow(soil_vals)   == n_row)
if (!is.null(bee_vals))    stopifnot(nrow(bee_vals)    == n_row)

## 元 df は coords と同じ順番だと仮定（1006.csv の順番を保ったまま distinct しているなら OK）
df <- cbind(df[seq_len(n_row), ], chelsa_vals, soil_vals, bee_vals)

## 重複経緯度の除去
df2 <- df %>%
  distinct(longitude, latitude, .keep_all = TRUE)

nm_all <- names(df2)


## =========================================================
## 3. 気候・土壌 PCA（Temperature_PC1 / precip_PC1 / soil_PC1）
## =========================================================
set.seed(42)

## Temperature
temp_vars <- grep(
  "^(CHELSA_bio5_1981-2010_V\\.2\\.1|CHELSA_bio10_1981-2010_V\\.2\\.1|CHELSA_gdd5_1981-2010_V\\.2\\.1)$",
  nm_all, value = TRUE
)
Temperature_PC1 <- rep(NA_real_, nrow(df2))
if (length(temp_vars) >= 1) {
  Xt <- df2[, temp_vars, drop = FALSE]
  cc <- complete.cases(Xt)
  if (sum(cc) >= 3 && ncol(Xt) >= 2) {
    pc <- prcomp(Xt[cc, ], scale. = TRUE)
    Temperature_PC1[cc] <- pc$x[,1]
  } else if (ncol(Xt) == 1) {
    Temperature_PC1 <- as.numeric(scale(Xt[[1]]))
  }
}

## Precipitation
precip_vars <- grep(
  "^(CHELSA_ai_1981-2010_V\\.2\\.1|CHELSA_vpd_mean_1981-2010_V\\.2\\.1|CHELSA_bio1[245]_1981-2010_V\\.2\\.1|CHELSA_swb_1981-2010_V\\.2\\.1)$",
  nm_all, value = TRUE
)
precip_PC1 <- rep(NA_real_, nrow(df2))
if (length(precip_vars) >= 1) {
  Xp <- df2[, precip_vars, drop = FALSE]
  cc <- complete.cases(Xp)
  if (sum(cc) >= 3 && ncol(Xp) >= 2) {
    pc <- prcomp(Xp[cc, ], scale. = TRUE)
    precip_PC1[cc] <- pc$x[,1]
  } else if (ncol(Xp) == 1) {
    precip_PC1 <- as.numeric(scale(Xp[[1]]))
  }
}

## Soil
soil_pca_vars <- grep(
  "^(ocd_0[.-]5cm_mean|soc_0[.-]5cm_mean|bdod_0[.-]5cm_mean|phh2o_0[.-]5cm_mean|nitrogen_0[.-]5cm_mean)$",
  nm_all, value = TRUE
)
soil_PC1 <- rep(NA_real_, nrow(df2))
if (length(soil_pca_vars) >= 1) {
  Xs <- df2[, soil_pca_vars, drop = FALSE]
  keep <- vapply(Xs, function(v) is.numeric(v) && sd(v, na.rm = TRUE) > 0, logical(1))
  Xs <- Xs[, keep, drop = FALSE]
  if (ncol(Xs) >= 2) {
    cc <- complete.cases(Xs)
    if (sum(cc) >= 3) {
      pc <- prcomp(Xs[cc, ], scale. = TRUE)
      soil_PC1[cc] <- pc$x[,1]
    }
  } else if (ncol(Xs) == 1) {
    soil_PC1 <- as.numeric(scale(Xs[[1]]))
  }
}

df2$Temperature_PC1 <- Temperature_PC1
df2$precip_PC1      <- precip_PC1
df2$soil_PC1        <- soil_PC1

## RSDS 列の統一（どれか1つあれば OK）
rsds_candidates <- grep("rsds", nm_all, value = TRUE)
if (!length(rsds_candidates)) stop("rsds 系の列が df2 に見つかりません")
df2$RSDS <- df2[[ rsds_candidates[1] ]]

## Elevation / roughness が無ければエラー
stopifnot(all(c("elevation","roughness") %in% names(df2)))

## =========================================================
## 4. Pigment latent factor (CFA)
## =========================================================
if (!("Lm" %in% names(df2))) {
  stop("Lm 列が見つかりません（L から -L で作ってください）")
}

df_cfa <- df2[, c("a","Lm","C")]
model_pig <- '
  Pigment =~ 1*a + Lm + C
'
fit_pig <- lavaan::cfa(
  model_pig,
  data      = df_cfa,
  estimator = "MLR",
  std.lv    = FALSE,
  missing   = "fiml"
)
stopifnot(lavaan::inspect(fit_pig, "converged"))
df2$Pigment <- as.numeric(lavaan::lavPredict(fit_pig, type="lv")[,"Pigment"])

## =========================================================
## 5. INLA: Pigment ~ env + Bee richness + 空間 (SPDE)
## =========================================================
USE_Z <- TRUE

z <- function(x) if (USE_Z) as.numeric(scale(x)) else as.numeric(x)

## Bee 列
bee_cols <- intersect(c("ardens","beaticola","consobrinus","diversus","honshuensis"), names(df2))
if (!length(bee_cols)) stop("Bee suitability 列が見つかりません")

dat0 <- df2
dat0$Bee_Richness <- rowSums(dat0[, bee_cols], na.rm = TRUE)

need_cols <- c("Pigment","elevation","roughness",
               "Temperature_PC1","precip_PC1","soil_PC1","RSDS",
               "Bee_Richness","longitude","latitude")
miss <- setdiff(need_cols, names(dat0))
if (length(miss)) stop("INLA 用に不足している列: ", paste(miss, collapse=", "))

cc_all <- complete.cases(dat0[, need_cols])
dat    <- dat0[cc_all, , drop = FALSE]
stopifnot(nrow(dat) >= 50)
cat("✅ INLA 用 完全ケース数:", nrow(dat), "\n")

## Z 化
dat$y_resp      <- z(dat$Pigment)
dat$z_elev      <- z(dat$elevation)
dat$z_rough     <- z(dat$roughness)
dat$z_RSDS      <- z(dat$RSDS)
dat$z_TempPC1   <- z(dat$Temperature_PC1)
dat$z_PrecipPC1 <- z(dat$precip_PC1)
dat$z_SoilPC1   <- z(dat$soil_PC1)
dat$z_BeeRich   <- z(dat$Bee_Richness)

## メートル座標
sf_dat   <- st_as_sf(dat, coords = c("longitude","latitude"), crs = 4326)
sf_m     <- st_transform(sf_dat, 3857)
coords_m <- as.matrix(st_coordinates(sf_m))

mesh_m <- INLA::inla.mesh.2d(
  loc      = coords_m,
  max.edge = c(20000, 100000)
)
spde_m <- INLA::inla.spde2.matern(mesh_m, alpha = 2)
A_m    <- INLA::inla.spde.make.A(mesh_m, loc = coords_m)

X_fix <- data.frame(
  Intercept   = 1,
  z_elev      = dat$z_elev,
  z_rough     = dat$z_rough,
  z_RSDS      = dat$z_RSDS,
  z_TempPC1   = dat$z_TempPC1,
  z_PrecipPC1 = dat$z_PrecipPC1,
  z_SoilPC1   = dat$z_SoilPC1,
  z_BeeRich   = dat$z_BeeRich
)

stk_m <- INLA::inla.stack(
  data    = list(y = dat$y_resp),
  A       = list(A_m, 1),
  effects = list(s = 1:spde_m$n.spde, X_fix),
  tag     = "est"
)

form_full <- y ~ 1 + z_elev + z_rough + z_RSDS +
  z_TempPC1 + z_PrecipPC1 + z_SoilPC1 + z_BeeRich +
  f(s, model = spde_m)
form_fix <- y ~ 1 + z_elev + z_rough + z_RSDS +
  z_TempPC1 + z_PrecipPC1 + z_SoilPC1 + z_BeeRich
form_sp  <- y ~ 1 + f(s, model = spde_m)

fit_full <- INLA::inla(
  formula = form_full,
  data    = INLA::inla.stack.data(stk_m),
  family  = "gaussian",
  control.predictor = list(
    A       = INLA::inla.stack.A(stk_m),
    compute = TRUE
  ),
  control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
)
fit_fix <- INLA::inla(
  formula = form_fix,
  data    = INLA::inla.stack.data(stk_m),
  family  = "gaussian",
  control.predictor = list(
    A       = INLA::inla.stack.A(stk_m),
    compute = TRUE
  ),
  control.compute   = list(dic = TRUE, waic = TRUE)
)
fit_sp <- INLA::inla(
  formula = form_sp,
  data    = INLA::inla.stack.data(stk_m),
  family  = "gaussian",
  control.predictor = list(
    A       = INLA::inla.stack.A(stk_m),
    compute = TRUE
  ),
  control.compute   = list(dic = TRUE, waic = TRUE)
)

IDX       <- INLA::inla.stack.index(stk_m, "est")$data
y_obs     <- INLA::inla.stack.data(stk_m)$y[IDX]
yhat_full <- fit_full$summary.fitted.values$mean[IDX]
yhat_fix  <- fit_fix$summary.fitted.values$mean[IDX]
yhat_sp   <- fit_sp$summary.fitted.values$mean[IDX]

SS <- function(y, yhat){
  ybar <- mean(y, na.rm=TRUE)
  sse  <- sum((y - yhat)^2, na.rm=TRUE)
  sst  <- sum((y - ybar)^2, na.rm=TRUE)
  1 - sse/sst
}
R2_full <- SS(y_obs, yhat_full)
R2_fix  <- SS(y_obs, yhat_fix)
R2_sp   <- SS(y_obs, yhat_sp)

part_R2_space_given_fix  <- as.numeric(R2_full - R2_fix)
part_R2_envpoll_given_sp <- as.numeric(R2_full - R2_sp)

res_R2 <- data.frame(
  model = c("full","fixed_only","spatial_only"),
  R2    = c(R2_full, R2_fix, R2_sp),
  part_R2_space_given_fix  = c(part_R2_space_given_fix, NA, NA),
  part_R2_envpoll_given_sp = c(part_R2_envpoll_given_sp, NA, NA)
)
print(res_R2)

## range / sigma
get_range_sigma_from_theta <- function(fit, comp_name = "s"){
  sh <- fit$summary.hyperpar
  r1 <- grep(sprintf("Theta1.*%s", comp_name),
             rownames(sh), value = TRUE, ignore.case = TRUE)
  r2 <- grep(sprintf("Theta2.*%s", comp_name),
             rownames(sh), value = TRUE, ignore.case = TRUE)
  stopifnot(length(r1) == 1, length(r2) == 1)
  tau_med   <- exp(sh[r1,"0.5quant"])
  kappa_med <- exp(sh[r2,"0.5quant"])
  range_med <- sqrt(8) / kappa_med
  sigma_med <- 1/(sqrt(4*pi) * tau_med * kappa_med)
  list(range = range_med, sigma = sigma_med)
}
rs <- get_range_sigma_from_theta(fit_full, "s")
range_km  <- as.numeric(rs$range) / 1000
sigma_val <- as.numeric(rs$sigma)
cat(sprintf("Matern range ≈ %.1f km, sigma ≈ %.3f\n", range_km, sigma_val))

## INLA 出力をホーム配下に保存
inla_dir <- file.path(out_root, "INLA_outputs")
dir.create(inla_dir, showWarnings = FALSE)

fx_out <- fit_full$summary.fixed
fx_out$significant <- with(fx_out, `0.025quant` * `0.975quant` > 0)
write_csv(fx_out, file.path(inla_dir, "fixed_effects_summary.csv"))
write_csv(res_R2, file.path(inla_dir, "model_R2_summary.csv"))

writeLines(sprintf("Range ≈ %.1f km\nSigma ≈ %.3f", range_km, sigma_val),
           file.path(inla_dir,"spatial_range_sigma.txt"))

sink(file.path(inla_dir,"INLA_summary_report.txt"))
cat("=== SPDE–INLA spatial regression summary ===\n\n")
cat("N =", nrow(dat), "complete cases\n")
cat(sprintf("WAIC: full=%.2f  fixed=%.2f  spatial=%.2f\n",
            fit_full$waic$waic, fit_fix$waic$waic, fit_sp$waic$waic))
cat(sprintf("DIC : full=%.2f  fixed=%.2f  spatial=%.2f\n\n",
            fit_full$dic$dic, fit_fix$dic$dic, fit_sp$dic$dic))
print(res_R2)
cat("\n--- Fixed effects (standardized; USE_Z =", USE_Z, ") ---\n")
print(fx_out)
cat(sprintf("\nMatern range ≈ %.1f km, sigma ≈ %.3f\n", range_km, sigma_val))
sink()

cat("\n=== INLA part finished ===\n")

id_est <- INLA::inla.stack.index(stk_m, "est")$data

## dat はすでに complete.cases 済みなので、id_est で行だけ抜く
dat_used <- dat[id_est, , drop = FALSE]
stopifnot(nrow(dat_used) == length(id_est))

y_obs    <- dat_used$y_resp
yhat     <- fit_full$summary.fitted.values$mean[id_est]
yhat_sd  <- fit_full$summary.fitted.values$sd[id_est]

resid_raw  <- y_obs - yhat
resid_zfit <- resid_raw / pmax(as.numeric(yhat_sd), 1e-6)

dat_used$resid_zfit <- resid_zfit
cat("✓ dat_used と residual が完全同期しました\n")

## DOY を date から計算（dat0 に date が入っている前提）
date_parsed <- suppressWarnings(as.Date(dat_used$date,
                                        tryFormats = c(
                                          "%Y-%m-%d","%Y/%m/%d","%Y.%m.%d","%Y%m%d",
                                          "%m/%d/%Y","%m-%d-%Y",
                                          "%Y-%m-%d %H:%M","%Y/%m/%d %H:%M",
                                          "%Y-%m-%d %H:%M:%S","%Y/%m/%d %H:%M:%S"
                                        )))
dat_used$DOY <- as.integer(strftime(date_parsed, "%j"))


human_tifs <- list.files(human_dir, pattern="\\.tif$", full.names = TRUE)
if (length(human_tifs) == 0){
  stop("人口密度 raster (.tif) が見つかりません: human_dir = ", human_dir)
}

r_human <- terra::rast(human_tifs[1])

pts_ll <- terra::vect(
  data.frame(lon = dat_used$longitude, lat = dat_used$latitude),
  geom = c("lon","lat"), crs = "EPSG:4326"
)
if (!terra::same.crs(pts_ll, r_human)){
  pts_ll <- terra::project(pts_ll, terra::crs(r_human))
}

vals <- terra::extract(r_human, pts_ll, ID = FALSE)[,1]
dat_used$pop_density_raw <- vals
dat_used$pop_density     <- as.numeric(scale(log1p(vals)))


df_dark <- dat_used %>%
  filter(
    is.finite(resid_zfit),
    is.finite(pop_density),
    is.finite(longitude), is.finite(latitude),
    is.finite(elevation),
    is.finite(DOY),
    resid_zfit > 0            # 濃色 tail のみ
  ) %>%
  mutate(
    elevation_z = as.numeric(scale(elevation))
  )

z_tail <- df_dark$resid_zfit
z_win  <- pmin(z_tail, quantile(z_tail, 0.99, na.rm = TRUE))
w_dark <- 1 + (z_win / sd(z_win, na.rm = TRUE))
w_dark <- pmin(w_dark, 4)
df_dark$w_dark <- w_dark

cat("濃色 tail サンプル数:", nrow(df_dark), "\n")

form_gauss <- resid_zfit ~
  s(DOY, bs = "tp", k = 20) +
  s(longitude, latitude, k = 300) +
  s(elevation_z, k = 15) +
  s(pop_density, k = 12) +
  ti(elevation_z, pop_density, k = c(15, 12))

m_dark_unw <- gam(form_gauss, data = df_dark, method = "REML")
m_dark_w   <- gam(form_gauss, data = df_dark, method = "REML",
                  weights = df_dark$w_dark)

## 図 (a) DOY
doy_grid <- data.frame(
  DOY         = seq(quantile(df_dark$DOY, .02, na.rm = TRUE),
                    quantile(df_dark$DOY, .98, na.rm = TRUE),
                    length.out = 200),
  longitude   = median(df_dark$longitude, na.rm = TRUE),
  latitude    = median(df_dark$latitude,  na.rm = TRUE),
  elevation_z = 0,
  pop_density = 0
)

pr_doy <- predict(
  m_dark_w,
  newdata = doy_grid,
  type   = "terms",
  terms  = "s(DOY)",
  se.fit = TRUE
)

plot_doy <- doy_grid %>%
  mutate(
    est = as.numeric(pr_doy$fit[,1]),
    se  = as.numeric(pr_doy$se.fit[,1]),
    lo  = est - 1.96 * se,
    up  = est + 1.96 * se
  )

fig_dir <- file.path(out_root, "GAM_figs")
dir.create(fig_dir, showWarnings = FALSE)

p_doy <- ggplot(plot_doy, aes(DOY, est)) +
  geom_ribbon(aes(ymin = lo, ymax = up), fill = "grey80") +
  geom_line(linewidth = 1) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold")) +
  labs(
    x = "Day of year (DOY)",
    y = "Partial effect on residual darkness (link)",
    title = "(a) Seasonal smooth: s(DOY)"
  )

ggsave(file.path(fig_dir,"Fig_season_DOY_popdensity.pdf"),
       p_doy, width = 7, height = 4)
ggsave(file.path(fig_dir,"Fig_season_DOY_popdensity.png"),
       p_doy, width = 7, height = 4, dpi = 300)

## Elevation × pop_density のプロットも同様に作れる（省略可）

## GAM summary
sink(file.path(fig_dir,"GAM_summaries.txt"))
cat("===== GAM (unweighted) =====\n")
print(summary(m_dark_unw))
cat("\n\n===== GAM (weighted) =====\n")
print(summary(m_dark_w))
sink()

cat("✅ INLA → 濃色 tail → GAM まで完了\n")

## =========================================================
## 7. SEM（5種 Bombus：bee_prob を媒介変数に）
## =========================================================
nm_all <- names(df2)
if (!("Lm" %in% nm_all)) { stop("df2 に Lm がありません") }
stopifnot(all(c("Lm","a","C") %in% nm_all))

sem_dir <- file.path(out_root, "sem_species_beePROB")
dir.create(sem_dir, recursive = TRUE, showWarnings = FALSE)

diag_dir <- file.path(sem_dir, "diagrams")
sig_dir  <- file.path(sem_dir, "diagrams_sigonly")
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(sig_dir,  recursive = TRUE, showWarnings = FALSE)

df_base <- df2[, c("Lm","a","C","elevation","roughness","RSDS"), drop=FALSE]
df_base$Temperature_PC1 <- df2$Temperature_PC1
df_base$precip_PC1      <- df2$precip_PC1
df_base$soil_PC1        <- df2$soil_PC1
df_base <- df_base %>% mutate(across(where(is.numeric), zscale))

model_spec <- '
  Pigment =~ 1*a + Lm + C
  bee_prob ~ elevation + roughness + precip_PC1 + Temperature_PC1 + soil_PC1 + RSDS
  Pigment ~ bee_prob + elevation + roughness + precip_PC1 + Temperature_PC1 + soil_PC1 + RSDS
'

preds <- c("elevation","roughness","precip_PC1","Temperature_PC1","soil_PC1","RSDS")
species_list <- c("ardens","beaticola","consobrinus","diversus","honshuensis")

check_ready <- function(dat){
  need <- c("Lm","a","C","elevation","roughness","RSDS",
            "Temperature_PC1","precip_PC1","soil_PC1","bee_prob")
  miss <- setdiff(need, names(dat))
  if (length(miss)) return(paste("欠損列:", paste(miss, collapse=", ")))
  if (sum(complete.cases(dat[,c("Lm","a","C","bee_prob")])) < 20) return("有効サンプル不足")
  zvar <- sapply(dat[, need], sd, na.rm = TRUE)
  if (any(is.na(zvar) | zvar == 0)) return("ゼロ分散あり")
  return(NA_character_)
}

fits <- list()
paths_all <- list(); ind_all_std <- list(); tot_all_std <- list()
summaries <- list(); fail_log <- list()
measures <- c("cfi","tli","rmsea","srmr","aic","bic")

for (sp in species_list) {
  message("\n⏳ SEM: ", sp)
  if (!sp %in% names(df2)) { fail_log[[sp]] <- "df2 に列なし"; next }
  
  dat_sem <- as.data.frame(df_base)
  dat_sem$bee_prob <- zscale(df2[[sp]])
  
  why <- check_ready(dat_sem)
  if (!is.na(why)) { fail_log[[sp]] <- why; next }
  
  fit <- tryCatch(
    lavaan::sem(
      model_spec, data=dat_sem,
      estimator="MLR", missing="fiml",
      std.lv=FALSE, fixed.x=FALSE
    ),
    error = function(e){ fail_log[[sp]] <<- e$message; NULL }
  )
  if (is.null(fit)) next
  fits[[sp]] <- fit
  
  fm <- tryCatch(
    lavaan::fitMeasures(fit, measures),
    error = function(e) setNames(rep(NA_real_, length(measures)), measures)
  )
  summaries[[sp]] <- data.frame(
    species=sp, cfi=fm["cfi"], tli=fm["tli"], rmsea=fm["rmsea"],
    srmr=fm["srmr"], aic=fm["aic"], bic=fm["bic"]
  )
  
  ss <- as.data.frame(lavaan::standardizedSolution(fit))
  
  paths_std <- ss %>%
    filter(op=="~", lhs %in% c("bee_prob","Pigment")) %>%
    mutate(species = sp)
  paths_all[[sp]] <- paths_std
  
  a_tbl <- ss %>% 
    filter(op=="~", lhs=="bee_prob", rhs %in% preds) %>%
    dplyr::select(rhs, a_std = est.std)
  b_val <- ss %>%
    filter(op=="~", lhs=="Pigment", rhs=="bee_prob") %>%
    pull(est.std)
  c_tbl <- ss %>%
    filter(op=="~", lhs=="Pigment", rhs %in% preds) %>%
    dplyr::select(rhs, c_std = est.std)
  
  if (length(b_val) && !is.na(b_val)) {
    ind_tbl <- a_tbl %>%
      mutate(indirect_std = a_std * b_val, species = sp)
    tot_tbl <- c_tbl %>%
      left_join(ind_tbl, by="rhs") %>%
      mutate(total_std = c_std + indirect_std, species = sp)
    ind_all_std[[sp]] <- ind_tbl
    tot_all_std[[sp]] <- tot_tbl
  }
  message("   ✅ done: ", sp)
}

## CSV 保存
if (length(summaries))
  bind_rows(summaries) %>%
  write_csv(file.path(sem_dir,"sem_fit_indices_beeProb.csv"))
if (length(paths_all))
  bind_rows(paths_all) %>%
  write_csv(file.path(sem_dir,"sem_paths_standardized_beeProb.csv"))
if (length(ind_all_std))
  bind_rows(ind_all_std) %>%
  write_csv(file.path(sem_dir,"sem_indirect_effects_beeProb.csv"))
if (length(tot_all_std))
  bind_rows(tot_all_std) %>%
  write_csv(file.path(sem_dir,"sem_total_effects_beeProb.csv"))

## Diagram 関数
safe <- function(x) gsub('"','\\"', x)
make_diagram <- function(fit, sp, sig_only=FALSE, alpha=0.05) {
  ss <- as.data.frame(lavaan::standardizedSolution(fit))
  reg_edges <- ss %>%
    filter(op=="~", lhs %in% c("bee_prob","Pigment")) %>%
    { if(sig_only) filter(., pvalue<alpha) else . }
  reg_df <- data.frame(
    from  = reg_edges$rhs,
    to    = reg_edges$lhs,
    coef  = sprintf("%.2f", reg_edges$est.std),
    color = ifelse(reg_edges$est.std >= 0, "#2B8CBE", "#E34A33"),
    width = scales::rescale(abs(reg_edges$est.std), c(1.5, 3.5))
  )
  meas_edges <- ss %>%
    filter(op=="=~", lhs=="Pigment", rhs %in% c("Lm","a","C"))
  meas_df <- data.frame(
    from  = "Pigment",
    to    = meas_edges$rhs,
    coef  = sprintf("%.2f", meas_edges$est.std),
    color = "#636363",
    width = 1.8
  )
  edges <- rbind(reg_df, meas_df)
  nodes <- c(preds, "bee_prob","Pigment","Lm","a","C")
  types <- c(rep("predictor", length(preds)), "mediator","latent",rep("indicator",3))
  fill_col <- c(
    predictor="#f2f0f7", mediator="#fee6ce",
    latent="#deebf7", indicator="#e5f5e0"
  )
  node_df <- data.frame(id=nodes, type=types)
  gr <- paste0(
    "digraph G {\nrankdir=LR;\n",
    'node [shape=box, style=rounded, fontname="Helvetica"];\n',
    paste(apply(node_df,1,function(x)
      sprintf('%s [label="%s", fillcolor="%s", style=filled];',
              x[['id']], safe(x[['id']]), fill_col[[x[['type']]]])
    ), collapse="\n"),
    "\n",
    paste(apply(edges,1,function(x)
      sprintf('%s -> %s [label="%s", color="%s", penwidth=%.2f];',
              x[['from']], x[['to']], x[['coef']],
              x[['color']], as.numeric(x[['width']]))
    ), collapse="\n"),
    "\n}"
  )
  outname <- ifelse(sig_only,
                    file.path(sig_dir, paste0("SEM_", sp, "_sig_beeProb.png")),
                    file.path(diag_dir, paste0("SEM_", sp, "_beeProb.png")))
  svg <- export_svg(DiagrammeR(gr))
  rsvg_png(charToRaw(svg), file=outname, width=2000, height=1200)
  message("Saved: ", outname)
}

for (sp in species_list) {
  if (!is.null(fits[[sp]])) {
    make_diagram(fits[[sp]], sp, sig_only=FALSE)
    make_diagram(fits[[sp]], sp, sig_only=TRUE)
  }
}

cat("\n✅ INLA + GAM + SEM まで一通り終了。出力は ~/sem_results 以下に保存されています。\n")