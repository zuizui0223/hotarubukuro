
df <- read.csv("~/Desktop/1006.csv")

hsv_matrix <- t(grDevices::rgb2hsv(df$R, df$G, df$B))
colnames(hsv_matrix) <- c("H","S","V")
df <- cbind(df, as.data.frame(hsv_matrix))
print(head(df[, c("R", "G", "B", "H", "S", "V")]))

## 前提：df に R,G,B (0–255) がある
stopifnot(all(c("R","G","B") %in% names(df)))
rgb01 <- pmin(pmax(as.matrix(df[, c("R","G","B")])/255, 0), 1)

lab_mat <- grDevices::convertColor(rgb01, from = "sRGB", to = "Lab")
df$L <- lab_mat[,1]
df$a <- lab_mat[,2]
df$b <- lab_mat[,3]
df$C <- sqrt(df$a^2 + df$b^2)   # C*ab（彩度）
df$Lm <- -df$L   # “暗いほど大” の向きに合わせる
head(df[, c("R", "G", "B", "L", "a", "b", "C", "Lm")])
summary(df[, c("L", "a", "b", "C", "Lm")])
cor(df[, c("L", "a", "b", "C", "Lm")])

# ── 1) 必要パッケージ ─────────────────────────────────────────────
install.packages(c("sf","ggplot2","rnaturalearth","rnaturalearthdata","ggspatial","scales"))
# hi-res 図化が必要なら：install.packages("ragg")
library(sf)
library(ggplot2)
library(rnaturalearth)
library(ggspatial)
library(scales)

# ── 2) データ準備（色列を hex に統一） ────────────────────────────
# df: data.frame with columns: latitude, longitude, and either:
#   (A) R,G,B in 0-255  or 0-1
#   (B) col_hex like "#AABBCC"

df_plot <- df

# (A) R,G,B から hex を作る場合（0-255 でも 0-1 でもOK）
if(all(c("R","G","B") %in% names(df_plot)) && !"col_hex" %in% names(df_plot)){
  # 値が 0–1 なら 255 倍、0–255 ならそのまま使う
  rng <- range(c(df_plot$R, df_plot$G, df_plot$B), na.rm = TRUE)
  is_01 <- (rng[1] >= 0 && rng[2] <= 1)
  R <- df_plot$R; G <- df_plot$G; B <- df_plot$B
  if(is_01){
    R <- pmin(pmax(round(R*255),0),255)
    G <- pmin(pmax(round(G*255),0),255)
    B <- pmin(pmax(round(B*255),0),255)
  } else {
    R <- pmin(pmax(round(R),0),255)
    G <- pmin(pmax(round(G),0),255)
    B <- pmin(pmax(round(B),0),255)
  }
  df_plot$col_hex <- rgb(R, G, B, maxColorValue = 255)
}

stopifnot(all(c("latitude","longitude","col_hex") %in% names(df_plot)))


library(sf)
library(ggplot2)
library(rnaturalearth)
library(ggspatial)
library(scales)

# df_plot に longitude, latitude, col_hex が含まれている前提
jp <- ne_countries(scale = "large", country = "Japan", returnclass = "sf")

# ── 図の作成 ────────────────────────────────
p <- ggplot() +
  # ベースマップ（白背景、薄い海岸線）
  geom_sf(data = jp, fill = "grey97", color = "grey60", linewidth = 0.25) +
  
  # RGB値をそのまま各点の色に使用
  geom_point(
    data = df_plot,
    aes(x = longitude, y = latitude),
    color = df_plot$col_hex,
    size = 2.3, stroke = 0, alpha = 1
  ) +
  
  # 日本本土のみ表示（北海道〜九州）
  coord_sf(
    xlim = c(128, 146),   # 南西諸島を除く
    ylim = c(30, 46), 
    expand = FALSE
  ) +
  
  # スケールバー・方位記号（控えめ）
  annotation_scale(
    location = "bl", width_hint = 0.2,
    text_cex = 0.6, line_width = 0.3
  ) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.6, "cm"), pad_y = unit(0.6, "cm"),
    height = unit(0.8, "cm"), width = unit(0.8, "cm"),
    style = north_arrow_fancy_orienteering(text_size = 5)
  ) +
  
  # テーマ
  theme_void(base_size = 9) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position = "none",
    plot.margin = margin(5,5,5,5)
  )

# ── 高解像度出力（A4横）────────────────────────
ggsave(
  "map_rgb_points_JapanMainland_A4_landscape.pdf", p,
  width = 297, height = 210, units = "mm", device = cairo_pdf
)

ggsave(
  "map_rgb_points_JapanMainland_A4_landscape.png", p,
  width = 297, height = 210, units = "mm", dpi = 600, type = "cairo"
)



library(dplyr)

# 修正コード
coords <- df %>%
  dplyr::select(longitude, latitude) %>%  # <- dplyr::select を明示
  dplyr::mutate(across(everything(), as.numeric)) %>%
  dplyr::filter(complete.cases(.))

# 確認
print(head(coords))
library(terra)

library(raster)

# 汎用関数：指定フォルダの tif を一枚ずつ抽出してデータフレーム化
extract_folder <- function(path, coords) {
  files <- list.files(path, pattern="\\.tif$", full.names=TRUE)
  vals <- lapply(files, function(f) {
    r <- raster(f)
    raster::extract(r, coords)
  })
  df <- as.data.frame(vals)
  names(df) <- tools::file_path_sans_ext(basename(files))
  df
}

# 各フォルダごとに抽出
chelsa_vals <- extract_folder("~/Desktop/stress2", coords)
soil_vals   <- extract_folder("~/Desktop/soil2", coords)
bee_vals    <- extract_folder("~/Desktop/bee4", coords)

# 結合
df <- cbind(df[1:nrow(chelsa_vals), ], chelsa_vals, soil_vals, bee_vals)
# 1. フォルダパスの定義
bee_dir <- "~/Desktop/bee4"

# 2. フォルダ内のすべての .tif ファイルのパスを取得
bee_raster_files <- list.files(bee_dir, pattern = "\\.tif$", full.names = TRUE)

# 3. すべてのラスターを読み込み、スタックとして集積
# rast() は単一のファイル名リストから SpatRasterCollection または SpatRaster を作成できます。
bee_rasters <- lapply(bee_raster_files, rast)

bee_stack <- rast(bee_rasters) 




suppressPackageStartupMessages({
  library(dplyr)
  library(spdep)
  library(adespatial)
})

# 重複を経緯度で除去（あなたの元コード踏襲）
df2 <- df %>%
  dplyr::distinct(longitude, latitude, .keep_all = TRUE)



## =========================================================
## 0) 必要パッケージ
## =========================================================
library(dplyr)
library(vegan)
library(adespatial)
library(Hmsc)
library(grDevices)   # rgb2hsv



climate_vars <- grep("^CHELSA", names(df2), value = TRUE)


# Soil
soil_vars <- c(
  "bdod_0-5cm_mean","elevation",
  "nitrogen_0-5cm_mean","ocd_0-5cm_mean","phh2o_0-5cm_mean","roughness","soc_0-5cm_mean")
soil_vars <- intersect(soil_vars, names(df2))

# Bee（5種想定／あれば拾う）
bee_vars  <- intersect(c("ardens","beaticola","consobrinus","diversus","honshuensis"), names(df2))




## =========================================
## 前準備
## =========================================
library(vegan)
set.seed(42)

stopifnot(exists("df2"))



## 必要パッケージ
suppressPackageStartupMessages({
  library(stats)
  library(utils)
  library(vegan)
  library(lavaan)
  library(mgcv)
})

## ============ ユーティリティ ============
strip_backticks <- function(x) gsub("^`|`$", "", x)
find_cols <- function(patterns, nm) {
  if (!is.vector(patterns)) patterns <- c(patterns)
  unique(unlist(lapply(patterns, function(p) grep(p, nm, value = TRUE))))
}
num_scale <- function(df){
  out <- df
  for(nm in names(out)){
    if(is.numeric(out[[nm]])) out[[nm]] <- as.numeric(scale(out[[nm]]))
  }
  out
}
drop_zerovar <- function(df){
  keep <- vapply(df, function(x) !is.numeric(x) || sd(x, na.rm=TRUE) > 0, logical(1))
  df[, keep, drop = FALSE]
}

############################################################
## Full pipeline (SEM for 5 Bombus spp.)
## - Build PC1: Temperature / Precipitation / Soil
## - Assemble SEM data
## - Per-species SEM (bee_abund as mediator) with diagnostics
## - Export fit indices / standardized paths / indirect & total effects
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stats)
  library(lavaan)
})

set.seed(42)

# ---------- Helpers ----------
`%||%` <- function(a, b) if (is.null(a) || length(a)==0 || is.na(a)) b else a
zscale <- function(x) if (is.numeric(x)) as.numeric(scale(x)) else x
pick_first <- function(v) if (length(v)) v[1] else character(0)

# ---------- Inputs ----------
stopifnot(exists("df2"))
nm_all <- names(df2)

out_dir <- "results/sem_species"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# ========= 4) 温度/降水/土壌（窒素含む）の PCA（PC1 作成） =========
# --- Temperature PC1: BIO5, BIO10, GDD5 ---
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

# --- Precipitation PC1: AI, VPD, BIO12/14/15, さらに SWB も拾えるように ---
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
    precip_PC1[cc] <- pc$x[, 1]
  } else if (ncol(Xp) == 1) {
    precip_PC1 <- as.numeric(scale(Xp[[1]]))
  }
}

# --- Soil PC1: ocd / soc / bdod / sand / nitrogen（ゼロ分散除外）---
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

# PCA 要約出力（確認用）
show_pca_summary <- function(df, vars, prefix) {
  vars <- intersect(vars, names(df))
  if (!length(vars)) return(invisible(NULL))
  X <- df[, vars, drop = FALSE]
  X <- X[complete.cases(X), , drop = FALSE]
  if (nrow(X) < 3) return(invisible(NULL))
  pr <- prcomp(X, scale. = TRUE)
  cat("\n======", prefix, "PCA summary ======\n")
  print(summary(pr))
  cat("\n-- Loadings (rotation) --\n")
  print(round(pr$rotation, 3))
  cat("\n-- Explained variance (%) --\n")
  print(round(100 * (pr$sdev^2 / sum(pr$sdev^2)), 2))
}

show_pca_summary(df2, temp_vars,   "Temperature")
show_pca_summary(df2, precip_vars, "Precipitation")
show_pca_summary(df2, soil_pca_vars, "Soil ")

df2$Temperature_PC1 <- Temperature_PC1
df2$precip_PC1      <- precip_PC1
df2$soil_PC1        <- soil_PC1
df2$RSDS <- df2$CHELSA_rsds_1981.2010_mean_V.2.1


## 1) 前提チェック
stopifnot(exists("df2"))
nm_all <- names(df2)

## 2) 暗さ Lm の作成（Lm = -L）
if (!("Lm" %in% nm_all)) {
  stopifnot("L" %in% nm_all)
  df2$Lm <- -df2$L   # 暗いほど＋
}

## 3) CFA 用データ
df_cfa  <- df2[, c("a", "Lm", "C")]

## 4) CFA モデル
## a を主因子にするために "1*a" を基準にする
model_pig <- '
  Pigment =~ 1*a + Lm + C
'

fit_pig <- lavaan::cfa(
  model_pig,
  data      = df_cfa,
  estimator = "MLR",
  std.lv    = FALSE,   # a を基準 → latent variance は推定される
  missing   = "fiml"
)

## 5) 収束確認
stopifnot(lavaan::inspect(fit_pig, "converged"))

## 6) Pigment スコア生成
df2$Pigment <- as.numeric(lavaan::lavPredict(fit_pig, type="lv")[, "Pigment"])

## 7) 結果表示
summary(fit_pig, fit.measures=TRUE, standardized=TRUE)


## =========================================================
##  INLA → 濃色 tail 抽出 → GAM（人口密度・季節・標高）
##  前提: df2 には少なくとも以下の列がある:
##   date, longitude, latitude,
##   Pigment, elevation, roughness,
##   Temperature_PC1, precip_PC1, soil_PC1,
##   CHELSA_rsds_1981-2010_mean_V.2.1,
##   ardens, beaticola, consobrinus, diversus, honshuensis
## =========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(readr)
  library(mgcv)
  library(INLA)
  library(sf)
  library(terra)
})

## -------- ユーザー設定 --------
USE_Z <- TRUE   # TRUE: 連続変数は Z スコア化
human_dir <- "~/Desktop/human"   # 人口密度 rasters フォルダ

## -------- 前提チェック --------
stopifnot(exists("df2"))

## =========================================================
## 1. 補助関数：RSDS の統一 & z スケール
## =========================================================

ensure_RSDS <- function(d){
  candidates <- c(
    "CHELSA_rsds_1981.2010_mean_V.2.1",
    "CHELSA_rsds_1981-2010_mean_V.2.1",
    "CHELSA_rsds_mean_1981-2010_V.2.1",
    "rsds_mean", "RSDS", "solar_radiation", "rsds"
  )
  hit <- intersect(candidates, names(d))
  if (length(hit) == 0){
    stop("RSDS の元列が見つかりません。grep('rsds|solar', names(df2)) で確認して下さい。")
  }
  d$RSDS <- d[[ hit[1] ]]
  d
}

z <- function(x) if (USE_Z) as.numeric(scale(x)) else as.numeric(x)

## =========================================================
## 2. INLA 用データ準備（Bee Richness + 環境）
## =========================================================

dat0 <- df2
dat0 <- ensure_RSDS(dat0)

sp_cols <- intersect(
  c("ardens","beaticola","consobrinus","diversus","honshuensis"),
  names(dat0)
)
stopifnot(length(sp_cols) > 0)

dat0$Bee_Richness <- rowSums(dat0[, sp_cols], na.rm = TRUE)

need_cols <- c(
  "Pigment","elevation","roughness",
  "Temperature_PC1","precip_PC1","soil_PC1","RSDS",
  "Bee_Richness","longitude","latitude"
)

miss <- setdiff(need_cols, names(dat0))
if (length(miss)) stop("不足列: ", paste(miss, collapse=", "))

## 完全ケース（行は絞るが列は全部残すので date なども生き残る）
cc_all <- stats::complete.cases(dat0[, need_cols])
dat    <- dat0[cc_all, , drop = FALSE]
stopifnot(nrow(dat) >= 50)
cat("✅ INLA 用 完全ケース数:", nrow(dat), "\n")

## Z スケール
dat$y_resp      <- z(dat$Pigment)
dat$z_elev      <- z(dat$elevation)
dat$z_rough     <- z(dat$roughness)
dat$z_RSDS      <- z(dat$RSDS)
dat$z_TempPC1   <- z(dat$Temperature_PC1)
dat$z_PrecipPC1 <- z(dat$precip_PC1)
dat$z_SoilPC1   <- z(dat$soil_PC1)
dat$z_BeeRich   <- z(dat$Bee_Richness)



sf_dat   <- sf::st_as_sf(dat, coords = c("longitude","latitude"), crs = 4326)
sf_m     <- sf::st_transform(sf_dat, 3857)  # メートル座標
coords_m <- as.matrix(sf::st_coordinates(sf_m))

mesh_m <- INLA::inla.mesh.2d(
  loc      = coords_m,
  max.edge = c(20000, 100000)  # 20km / 100km（調整可）
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

stopifnot(nrow(X_fix) == nrow(dat), nrow(A_m) == nrow(dat))

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



fx <- fit_full$summary.fixed
fx$significant <- with(fx, `0.025quant` * `0.975quant` > 0)
print(fx[order(-abs(fx$mean)), ])

cat(sprintf("\nWAIC  full=%.2f  fix=%.2f  sp=%.2f\n",
            fit_full$waic$waic, fit_fix$waic$waic, fit_sp$waic$waic))
cat(sprintf("DIC   full=%.2f  fix=%.2f  sp=%.2f\n",
            fit_full$dic$dic,  fit_fix$dic$dic,  fit_sp$dic$dic))

IDX       <- INLA::inla.stack.index(stk_m, tag="est")$data
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

get_range_sigma_from_theta <- function(fit, comp_name = "s"){
  sh <- fit$summary.hyperpar; stopifnot(!is.null(sh))
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

## ---- 出力保存 ----
dir.create("INLA_outputs", showWarnings = FALSE)

fx_out <- fit_full$summary.fixed
fx_out$significant <- with(fx_out, `0.025quant` * `0.975quant` > 0)
readr::write_csv(fx_out, "INLA_outputs/fixed_effects_summary.csv")
readr::write_csv(res_R2, "INLA_outputs/model_R2_summary.csv")
writeLines(sprintf("Range ≈ %.1f km\nSigma ≈ %.3f", range_km, sigma_val),
           "INLA_outputs/spatial_range_sigma.txt")

sink("INLA_outputs/INLA_summary_report.txt")
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

## =========================================================
## 5. 残差（濃色 tail）を dat と同期
## =========================================================

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

## =========================================================
## 6. 人口密度 raster 抽出 → log1p → Z スケール
## =========================================================

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

## =========================================================
## 7. 濃色 tail 抽出 & weight 作成
## =========================================================

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

## =========================================================
## 8. GAM：season + space + elevation + population density
## =========================================================

form_gauss <- resid_zfit ~
  s(DOY, bs = "tp", k = 20) +
  s(longitude, latitude, k = 300) +
  s(elevation_z, k = 15) +
  s(pop_density, k = 12) +
  ti(elevation_z, pop_density, k = c(15, 12))

m_dark_unw <- gam(form_gauss, data = df_dark, method = "REML")
m_dark_w   <- gam(form_gauss, data = df_dark, method = "REML",
                  weights = df_dark$w_dark)

## =========================================================
## 9. 図 (a) DOY smooth, (b) Elevation × pop_density interaction
## =========================================================

# (a) s(DOY)
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

ggsave("Fig_season_DOY_popdensity.pdf", p_doy, width = 7, height = 4)
ggsave("Fig_season_DOY_popdensity.png", p_doy, width = 7, height = 4, dpi = 300)

# (b) Elevation × pop_density
qL <- quantile(df_dark$elevation_z, c(.10, .30))
qM <- quantile(df_dark$elevation_z, c(.45, .55))
qH <- quantile(df_dark$elevation_z, c(.70, .90))

band_df <- df_dark %>%
  mutate(band = case_when(
    elevation_z >= qL[1] & elevation_z <= qL[2] ~ "Low elevation",
    elevation_z >= qM[1] & elevation_z <= qM[2] ~ "Mid elevation",
    elevation_z >= qH[1] & elevation_z <= qH[2] ~ "High elevation"
  )) %>%
  filter(!is.na(band))

band_summ <- band_df %>%
  group_by(band) %>%
  summarise(
    elev_med = median(elevation_z),
    x_min    = quantile(pop_density, .01),
    x_max    = quantile(pop_density, .99),
    .groups = "drop"
  )

mk_grid <- function(band_row, n = 200){
  data.frame(
    band        = band_row$band,
    pop_density = seq(band_row$x_min, band_row$x_max, length.out = n),
    elevation_z = band_row$elev_med,
    DOY         = median(df_dark$DOY),
    longitude   = median(df_dark$longitude),
    latitude    = median(df_dark$latitude)
  )
}

pred_grid <- do.call(rbind, lapply(split(band_summ, band_summ$band), mk_grid))
pred      <- predict(m_dark_w, newdata = pred_grid, type = "link", se.fit = TRUE)

pred_df <- pred_grid %>%
  mutate(
    fit = as.numeric(pred$fit),
    se  = as.numeric(pred$se.fit),
    lwr = fit - 1.96 * se,
    upr = fit + 1.96 * se
  )

p_int <- ggplot(pred_df, aes(pop_density, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.18) +
  geom_line(linewidth = 1.0) +
  facet_wrap(~ band, nrow = 1, scales = "free_x") +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold")) +
  labs(
    x = "Population density (log-scaled z)",
    y = "Partial effect on residual darkness (link)",
    title = "(b) Elevation × Population density"
  )

ggsave("Fig_interaction_elevation_popdensity.pdf", p_int, width = 10, height = 3.6)
ggsave("Fig_interaction_elevation_popdensity.png", p_int, width = 10, height = 3.6, dpi = 300)

## =========================================================
## 10. GAM summary 出力
## =========================================================

out_file <- "GAM_summaries.txt"
sink(out_file)
cat("=============================================\n")
cat("         GAM SUMMARY: UNWEIGHTED\n")
cat("=============================================\n\n")
print(summary(m_dark_unw))
cat("\n\n\n")
cat("=============================================\n")
cat("           GAM SUMMARY: WEIGHTED\n")
cat("=============================================\n\n")
print(summary(m_dark_w))
cat("\n")
sink()

cat("📄 GAM summaries saved to:", out_file, "\n")
cat("✅ All INLA → tail → GAM pipeline finished.\n")


# ============================================================
# 0. Setup
# ============================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr)
  library(ggplot2); library(forcats); library(scales)
  library(patchwork); library(cowplot)
  library(lavaan); library(DiagrammeR); library(DiagrammeRsvg); library(rsvg)
})

`%||%` <- function(a,b) if(is.null(a)||length(a)==0||all(is.na(a))) b else a
zscale  <- function(x) if (is.numeric(x)) as.numeric(scale(x)) else x
pick_first <- function(v) if (length(v)) v[1] else character(0)

stopifnot(exists("df2"))
nm_all <- names(df2)

if (!("Lm" %in% nm_all)) { stopifnot("L" %in% nm_all); df2$Lm <- -df2$L }
stopifnot(all(c("Lm","a","C") %in% names(df2)))

# ============================================================
# 1. Output directories
# ============================================================
out_dir <- "results/sem_species_beePROB"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)

diag_dir <- file.path(out_dir, "diagrams")
dir.create(diag_dir, recursive=TRUE, showWarnings=FALSE)

sig_dir <- file.path(out_dir, "diagrams_sigonly")
dir.create(sig_dir, recursive=TRUE, showWarnings=FALSE)

# ============================================================
# 2. Build Base Frame
# ============================================================
elev_col  <- pick_first(grep("^elevation$", nm_all, value=TRUE))
rough_col <- pick_first(grep("^roughness$", nm_all, value=TRUE))
rsds_col  <- pick_first(grep("CHELSA[_-]?rsds", nm_all, value=TRUE))
stopifnot(length(elev_col)==1, length(rough_col)==1, length(rsds_col)==1)

df_base <- df2[, c("Lm","a","C", elev_col, rough_col, rsds_col), drop=FALSE]
names(df_base)[names(df_base)==elev_col]  <- "elevation"
names(df_base)[names(df_base)==rough_col] <- "roughness"
names(df_base)[names(df_base)==rsds_col]  <- "RSDS"

df_base$Temperature_PC1 <- Temperature_PC1 %||% rep(NA_real_, nrow(df2))
df_base$precip_PC1      <- precip_PC1      %||% rep(NA_real_, nrow(df2))
df_base$soil_PC1        <- soil_PC1        %||% rep(NA_real_, nrow(df2))

df_base <- df_base %>% mutate(across(where(is.numeric), zscale))

# ============================================================
# 3. SEM Model
# ============================================================
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

# ============================================================
# 4. Fit SEM for each species
# ============================================================
fits <- list()
paths_all <- list(); ind_all_std <- list(); tot_all_std <- list()
summaries <- list(); fail_log <- list()
measures <- c("cfi","tli","rmsea","srmr","aic","bic")

for (sp in species_list) {
  message("\n⏳ SEM: ", sp)
  
  if (!sp %in% names(df2)) { fail_log[[sp]] <- "df2 に列なし"; next }
  
  dat <- df_base
  dat$bee_prob <- zscale(df2[[sp]])
  
  why <- check_ready(dat)
  if (!is.na(why)) { fail_log[[sp]] <- why; next }
  
  fit <- tryCatch(
    sem(model_spec, data=dat, estimator="MLR", missing="fiml",
        std.lv=FALSE, fixed.x=FALSE),
    error=function(e){ fail_log[[sp]] <- e$message; return(NULL) }
  )
  if (is.null(fit)) next
  
  fits[[sp]] <- fit
  
  # ---- Fit indices ----
  fm <- tryCatch(fitMeasures(fit, measures),
                 error=function(e) setNames(rep(NA_real_,length(measures)),measures))
  
  summaries[[sp]] <- data.frame(
    species=sp, cfi=fm["cfi"], tli=fm["tli"], rmsea=fm["rmsea"],
    srmr=fm["srmr"], aic=fm["aic"], bic=fm["bic"]
  )
  
  # ---- Extract standardized paths ----
  ss <- standardizedSolution(fit)
  
  paths_std <- ss %>%
    filter(op=="~", lhs %in% c("bee_prob","Pigment")) %>%
    mutate(species = sp)
  paths_all[[sp]] <- paths_std
  
  # ---- Indirect and total ----
  a_tbl <- ss %>% filter(op=="~", lhs=="bee_prob", rhs %in% preds) %>%
    select(rhs, a_std = est.std)
  b_val <- ss %>% filter(op=="~", lhs=="Pigment", rhs=="bee_prob") %>% pull(est.std)
  c_tbl <- ss %>% filter(op=="~", lhs=="Pigment", rhs %in% preds) %>%
    select(rhs, c_std = est.std)
  
  if (length(b_val) && !is.na(b_val)) {
    ind_tbl <- a_tbl %>% mutate(indirect_std = a_std * b_val, species = sp)
    tot_tbl <- c_tbl %>% left_join(ind_tbl, by="rhs") %>%
      mutate(total_std = c_std + indirect_std, species = sp)
    ind_all_std[[sp]] <- ind_tbl
    tot_all_std[[sp]] <- tot_tbl
  }
  
  message("   ✅ done: ", sp)
}

# ============================================================
# 5. Save CSV
# ============================================================
if (length(summaries))
  bind_rows(summaries) |> write_csv(file.path(out_dir, "sem_fit_indices_beeProb.csv"))

if (length(paths_all))
  bind_rows(paths_all) |> write_csv(file.path(out_dir, "sem_paths_standardized_beeProb.csv"))

if (length(ind_all_std))
  bind_rows(ind_all_std) |> write_csv(file.path(out_dir,"sem_indirect_effects_beeProb.csv"))

if (length(tot_all_std))
  bind_rows(tot_all_std) |> write_csv(file.path(out_dir,"sem_total_effects_beeProb.csv"))

# ============================================================
# 6. Diagram functions
# ============================================================
if (!exists("lab_map")) lab_map <- setNames(character(0), character(0))

safe <- function(x) gsub('"','\\"', x)

make_diagram <- function(fit, sp, sig_only=FALSE, alpha=0.05) {
  ss <- standardizedSolution(fit)
  
  reg_edges <- ss %>% 
    filter(op=="~", lhs %in% c("bee_prob","Pigment")) %>%
    { if(sig_only) filter(., pvalue<alpha) else . }
  
  reg_df <- data.frame(
    from = reg_edges$rhs,
    to   = reg_edges$lhs,
    coef = sprintf("%.2f", reg_edges$est.std),
    color = ifelse(reg_edges$est.std >= 0, "#2B8CBE", "#E34A33"),
    width = scales::rescale(abs(reg_edges$est.std), c(1.5, 3.5))
  )
  
  meas_edges <- ss %>% filter(op=="=~", lhs=="Pigment", rhs %in% c("Lm","a","C"))
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
                    file.path(diag_dir, paste0("SEM_", sp, "_beeProb.png"))
  )
  
  svg <- export_svg(DiagrammeR(gr))
  rsvg_png(charToRaw(svg), file=outname, width=2000, height=1200)
  message("Saved: ", outname)
}

# ============================================================
# 7. Generate Diagrams (full & sig-only)
# ============================================================
for (sp in species_list) {
  if (!is.null(fits[[sp]])) {
    make_diagram(fits[[sp]], sp, sig_only=FALSE)
    make_diagram(fits[[sp]], sp, sig_only=TRUE)
  }
}

message("\n🖼 Diagrams completed.\n")

# ============================================================
# 8. Paper-ready figures
# ============================================================
figdir <- file.path(out_dir, "figures_en")
dir.create(figdir, recursive=TRUE, showWarnings=FALSE)

paths_std <- read_csv(file.path(out_dir,"sem_paths_standardized_beeProb.csv"))
ind_std   <- read_csv(file.path(out_dir,"sem_indirect_effects_beeProb.csv"))
tot_std   <- read_csv(file.path(out_dir,"sem_total_effects_beeProb.csv"))

pred_order <- c("elevation","precip_PC1","Temperature_PC1","soil_PC1","RSDS","roughness")
lab_en <- c(elevation="Elevation",precip_PC1="Aridity",
            Temperature_PC1="Heat",soil_PC1="Soil environment",
            RSDS="Solar radiation",roughness="Roughness",
            bee_prob="Pollinator suitability")

dir_pig_std <- paths_std %>%
  filter(lhs=="Pigment", rhs %in% pred_order) %>%
  transmute(species, effect=rhs, type="Direct (std)", value=est.std)

ind2_std <- ind_std %>%
  filter(rhs %in% pred_order) %>%
  transmute(species, effect=rhs, type="Indirect via pollinator (std)", value=indirect_std)

tot2_std <- tot_std %>%
  filter(rhs %in% pred_order) %>%
  transmute(species, effect=rhs, type="Total (std)", value=total_std)

eff_all <- bind_rows(dir_pig_std, ind2_std, tot2_std) %>%
  mutate(effect=factor(effect,levels=pred_order,labels=lab_en[pred_order]),
         type=factor(type,levels=c("Direct (std)",
                                   "Indirect via pollinator (std)",
                                   "Total (std)")))

# ---- Fig1 ----
sum_mean <- eff_all %>% group_by(type,effect) %>%
  summarise(n=n(), mean=mean(value,na.rm=TRUE),
            sd=sd(value,na.rm=TRUE), .groups="drop") %>%
  mutate(se=sd/sqrt(n), ci_l=mean-1.96*se, ci_u=mean+1.96*se,
         sig=ci_l*ci_u > 0)

p_mean <- ggplot(sum_mean, aes(x=mean,y=fct_rev(effect))) +
  geom_vline(xintercept=0, linetype=2, color="grey60") +
  geom_pointrange(aes(xmin=ci_l, xmax=ci_u, color=sig)) +
  facet_grid(.~type) +
  scale_color_manual(values=c("FALSE"="grey70","TRUE"="black")) +
  theme_bw(11)

ggsave(file.path(figdir,"fig1_mean_beeProb.png"), p_mean, width=8.5, height=5.2, dpi=300)

# ---- Fig2 ----
heat_df <- tot2_std %>% mutate(effect=factor(effect,levels=pred_order,labels=lab_en[pred_order]),
                               label=sprintf("%.2f", value))

p_heat <- ggplot(heat_df, aes(x=effect, y=species, fill=value)) +
  geom_tile(color="white") +
  geom_text(aes(label=label), size=3) +
  scale_fill_gradient2(low="#b2182b", mid="white", high="#2166ac", midpoint=0) +
  theme_bw(11)

ggsave(file.path(figdir,"fig2_heatmap_beeProb.png"), p_heat, width=8.5, height=3.8, dpi=300)

message("\n=============================\n  All SEM figures saved.\n=============================\n")

