suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(usdm)
  library(tools)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(ENMeval)   # v2.0.5.2
})

set.seed(42)
env_dir <- "~/Desktop/beeenv"              # 環境TIFフォルダ
occ_dir <- "data/gbif/species_native"      # 在来種オカレンスCSVフォルダ
out_dir <- "data/clean"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "stack"), recursive = TRUE, showWarnings = FALSE)


env_files <- list.files(env_dir, pattern = "\\.tif$", full.names = TRUE)
stopifnot("環境TIFが見つかりません" = length(env_files) > 0)

is_wc <- grepl("worldcover", basename(env_files), ignore.case = TRUE)
wc_files   <- env_files[is_wc]
cont_files <- env_files[!is_wc]
if (length(wc_files) != 1) message("⚠️ WorldCover が1枚でない場合は is_wc の条件を調整してください。")


occ_csvs <- list.files(occ_dir, pattern = "\\.csv$", full.names = TRUE)
stopifnot("在来種CSVが見つかりません" = length(occ_csvs) > 0)
occ_all  <- bind_rows(lapply(occ_csvs, read.csv))
occ_all  <- occ_all %>% distinct(decimalLongitude, decimalLatitude, .keep_all = TRUE)
occ_pts  <- vect(occ_all, geom = c("decimalLongitude","decimalLatitude"), crs = "EPSG:4326")
cat("✅ オカレンス数:", nrow(occ_all), "\n")


## =========================================================
## 0. template（解像度・CRS の基準）
## =========================================================

tmpl_path <- if (length(cont_files) >= 1) cont_files[1] else wc_files[1]
tmpl  <- rast(tmpl_path)
crs_t <- crs(tmpl)
res_t <- res(tmpl)


## =========================================================
## 1. オカレンス基準で解析 extent を決める（最重要）
## =========================================================

occ_ext <- ext(occ_pts)

## 余白（1度）を追加（必要に応じて調整）
occ_ext <- ext(
  xmin(occ_ext) - 1,
  xmax(occ_ext) + 1,
  ymin(occ_ext) - 1,
  ymax(occ_ext) + 1
)

## template を解析域に切る
grid <- crop(tmpl, occ_ext)


## =========================================================
## 2. 環境ラスタを grid に完全整列
## =========================================================

align_one <- function(f, method){
  r <- rast(f)
  
  ## CRS を合わせる
  if (!isTRUE(all.equal(crs(r), crs_t))) {
    r <- project(r, crs_t, method = method)
  }
  
  ## extent を grid に制限
  r <- crop(r, grid)
  
  ## 解像度・原点を grid に合わせる
  r <- resample(r, grid, method = method)
  
  r
}

cont_aligned <- if (length(cont_files)) {
  rast(lapply(cont_files, align_one, method = "bilinear"))
} else NULL

wc_aligned <- if (length(wc_files)) {
  rast(lapply(wc_files, align_one, method = "near"))
} else NULL

if (!is.null(cont_aligned)) {
  names(cont_aligned) <- file_path_sans_ext(basename(cont_files))
}
if (!is.null(wc_aligned)) {
  names(wc_aligned) <- file_path_sans_ext(basename(wc_files))
}


## =========================================================
## 3. VIF（continuous のみ）
## =========================================================

if (!is.null(cont_aligned)) {
  
  vals <- terra::extract(cont_aligned, occ_pts, ID = FALSE) |> 
    as.data.frame()
  
  vals <- vals[stats::complete.cases(vals), , drop = FALSE]
  
  stopifnot(
    "❌ 抽出値が空です（extent / CRS を確認）" = nrow(vals) > 0
  )
  
  vif_res   <- usdm::vifstep(vals, th = 10)
  keep_vars <- as.character(vif_res@results$Variables)
  
  cont_kept <- if (length(keep_vars)) {
    subset(cont_aligned, keep_vars)
  } else NULL
  
  write.csv(
    vif_res@results,
    file.path(out_dir, "vif_results_continuous.csv"),
    row.names = FALSE
  )
  
  cat(
    "📝 VIF通過（連続変数）:",
    if (length(keep_vars)) paste(keep_vars, collapse = ", ") else "(なし)",
    "\n"
  )
  
} else {
  cont_kept <- NULL
  message("⚠️ 連続変数が無いため VIF をスキップしました。")
}


final_stack <- c(if(!is.null(cont_kept)) cont_kept, if(!is.null(wc_aligned)) wc_aligned)
stopifnot("出力レイヤがありません" = nlyr(final_stack) > 0)

out_stack <- file.path(out_dir, "stack", "env_stack_final.tif")
writeRaster(final_stack, out_stack, overwrite = TRUE)
cat("💾 保存:", out_stack, "\n")

cat("📏 レイヤ数:", nlyr(final_stack), " / 解像度:", paste(res(final_stack), collapse = " x "), "\n")
cat("📐 範囲:", as.vector(ext(final_stack)), "\n")


library(terra)
library(sf)
library(dplyr)
library(rnaturalearth)

# ============================================================
# 1. 北海道・南西諸島を除いた「本土ポリゴン」を作成
# ============================================================
cat("🗺️ 本土ポリゴンを作成中...\n")

# 日本地図データの取得 (高解像度)
jp_all <- ne_countries(country = "Japan", scale = 10, returnclass = "sf")

# バラバラの島に分解してフィルタリング
jp_parts <- st_cast(jp_all, "POLYGON")
jp_parts$area_m2 <- as.numeric(st_area(jp_parts))

jp_mainland_parts <- jp_parts %>%
  bind_cols(st_coordinates(st_centroid(.))) %>%
  rename(lon = X, lat = Y) %>%
  filter(
    # 面積フィルタ: 200km^2以上 (本州・四国・九州・佐渡・淡路・対馬など主要島のみ)
    area_m2 > 200 * 1e6,
    
    # 緯度フィルタ: 北海道(約41.5度以北)と南西諸島(30度以南)を除外
    lat < 41.5, 
    lat > 30.0   
  )

# 結合して1つのポリゴンにする
jp_mainland <- st_union(jp_mainland_parts) %>% st_as_sf()
jp_mainland <- vect(jp_mainland) # terra形式に変換

cat("✅ ポリゴン作成完了: 本州・四国・九州・主要属島のみが含まれます。\n")


# ============================================================
# 2. 環境変数を読み込んでマスク処理（Crop & Mask）
# ============================================================
cat("🔄 ラスタデータのマスク処理を開始...\n")

# 現在のスタックを読み込み
# ※ ファイルパスはご自身の環境に合わせて調整してください
input_tif <- "data/clean/stack/env_stack_final.tif" 
env <- rast(input_tif)

# 1) Crop: ポリゴンの範囲ギリギリまで画像を切り取る（余白を削除）
env_final <- crop(env, jp_mainland)

# 2) Mask: ポリゴンの外側（海、北海道、沖縄）をNAにする
env_final <- mask(env_final, jp_mainland)


# ============================================================
# 3. 結果の確認と保存
# ============================================================

# NA率の確認（全レイヤーでほぼ同じ値になるはずです）
na_rates <- global(env_final, "isNA")$isNA / ncell(env_final)
cat("\n📊 処理後のNA率確認 (全て揃っていればOK):\n")
print(head(sort(na_rates, decreasing = TRUE)))


# 保存
output_tif <- "data/clean/stack/env_stack_mainland_masked.tif"
writeRaster(env_final, output_tif, overwrite = TRUE)

cat("\n🎉 完了しました！\n")
cat("💾 保存先:", output_tif, "\n")
cat("これで全レイヤーが「北海道・沖縄抜きの日本本土」の形に統一されました。\n")



env <- rast(output_tif)

# Categorical変数の指定
cat_idx <- grep("worldcover", names(env), ignore.case = TRUE)
if (length(cat_idx) == 0) {
  categoricals <- NULL
} else {
  categoricals <- names(env)[cat_idx]
  # 明示的にfactor化（重要）
  for(cat_name in categoricals){
    env[[cat_name]] <- as.factor(env[[cat_name]])
  }
}

# 背景点生成
bg_xy <- terra::spatSample(
  env,
  size   = 10000,
  method = "random",
  values = FALSE,
  xy     = TRUE,
  na.rm  = TRUE
) |> as.data.frame()
colnames(bg_xy) <- c("lon","lat")

# 解析対象種
targets <- c(
  "Bombus_beaticola",
  "Bombus_ardens",
  "Bombus_honshuensis",
  "Bombus_diversus",
  "Bombus_consobrinus"
)

out_dir <- "data/clean"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


out_root <- "data/clean/ENMeval"
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# ---- メインループ ----
for (sp in targets) {
  message("\n==== Running ENMevaluate for ", sp, " ====")
  
  csv <- file.path(occ_dir, paste0(sp, ".csv"))
  if (!file.exists(csv)) { warning("CSV not found: ", csv); next }
  
  df <- read.csv(csv, check.names = FALSE)
  
  # 座標列の自動検出
  nl <- tolower(names(df))
  li <- which(nl %in% c("longitude","lon","decimallongitude","x"))[1]
  ai <- which(nl %in% c("latitude","lat","decimallatitude","y"))[1]
  
  if(is.na(li) || is.na(ai)){ warning("座標列が見つかりません: ", sp); next }
  
  occ <- df[, c(li, ai)]
  names(occ) <- c("lon","lat")
  occ <- occ[complete.cases(occ),]
  
  # 同一セル除去
  cid <- cellFromXY(env, occ)
  occ <- occ[!duplicated(cid) & !is.na(cid),]
  n_occ <- nrow(occ)
  
  # ★デバッグ: 有効地点数の確認
  message("  -> Effective occurrences (n_occ) for ", sp, ": ", n_occ)
  
  if (n_occ < 5) {
    message("  -> SKIP: Too few occurrences (< 5).")
    next
  }
  
  # ENMevaluate実行 (tryCatchでエラー回避)
  e <- tryCatch(
    ENMevaluate(
      occs         = occ,
      envs         = env,
      bg           = bg_xy,
      algorithm    = "maxnet",
      partitions   = "block",
      tune.args    = list(fc=c("L","LQ","LQH"), rm=1:5),
      categoricals = categoricals,
      parallel     = TRUE,
      numCores     = 8  
    ),
    error = function(err){
      warning(sp, " -> ENMevaluate FAILED: ", conditionMessage(err))
      return(NULL)
    }
  )
  
  if (is.null(e)) next
  
  # 結果保存
  sp_dir <- file.path(out_root, sp)
  dir.create(sp_dir, showWarnings=FALSE, recursive=TRUE)
  
  write.csv(eval.results(e),
            file.path(sp_dir, paste0(sp, "_results.csv")),
            row.names=FALSE)
  
  # コンソールに上位結果を表示
  message("--- Top results for ", sp, " ---")
  print(head(eval.results(e), 3))
  
  saveRDS(e, file.path(sp_dir, paste0(sp, "_enmeval.rds")))
  message("✔ saved ENMevaluate for ", sp)
}

# ============================================================
# 3. Best Model Selection & Prediction + Response Curves
# ============================================================

library(maxnet)

# ベストモデル選択関数（変更なし）
select_best <- function(res){
  if ("cbi.val.avg" %in% names(res) && any(is.finite(res$cbi.val.avg))) {
    max_cbi <- max(res$cbi.val.avg, na.rm = TRUE)
    cand <- which(res$cbi.val.avg == max_cbi)
    if ("auc.val.avg" %in% names(res)) {
      auc_sub <- res$auc.val.avg[cand]
      if (any(is.finite(auc_sub))) cand <- cand[which.max(auc_sub)]
    }
    if ("or.10p.val.avg" %in% names(res)) {
      or_sub <- res$or.10p.val.avg[cand]
      if (any(is.finite(or_sub))) cand <- cand[which.min(or_sub)]
    }
    return(cand[1])
  }
  if ("AICc" %in% names(res) && any(is.finite(res$AICc))) {
    return(which.min(res$AICc))
  }
  if ("auc.val.avg" %in% names(res) && any(is.finite(res$auc.val.avg))) {
    return(which.max(res$auc.val.avg))
  }
  1L
}

# 予測用ラッパー（変更なし）
predict_maxnet_rast <- function(env_rast, model, categoricals=NULL){
  terra::predict(
    env_rast,
    model,
    fun = function(m, newdata, ...) {
      if (!is.null(categoricals)) {
        for (cn in categoricals) {
          if (cn %in% colnames(newdata)) {
            newdata[[cn]] <- factor(as.character(newdata[[cn]]))
          }
        }
      }
      as.numeric(predict(m, newdata = as.data.frame(newdata), type = "cloglog"))
    },
    na.rm = TRUE
  )
}

# ============================================================
# Prediction loop
# ============================================================

for (sp in targets) {
  sp_dir <- file.path(out_root, sp)
  rds <- file.path(sp_dir, paste0(sp, "_enmeval.rds"))
  if (!file.exists(rds)) next
  
  e   <- readRDS(rds)
  res <- eval.results(e)
  
  best_i <- select_best(res)
  model  <- eval.models(e)[[best_i]]
  
  message("-> Predicting ", sp,
          " | best row = ", best_i,
          " | fc=", res$fc[best_i],
          ", rm=", res$rm[best_i])
  
  # =========================
  # Prediction map
  # =========================
  pred <- predict_maxnet_rast(env, model, categoricals)
  out_tif <- file.path(sp_dir, sprintf("%s_best_cloglog.tif", sp))
  writeRaster(pred, out_tif, overwrite = TRUE)
  
  # =========================
  # Response curves (STANDARD)
  # =========================
  rc_type <- if (grepl("^Bombus", sp)) "logistic" else "cloglog"
  
  png(
    filename = file.path(sp_dir, sprintf("%s_response_curves.png", sp)),
    width = 2000, height = 1600, res = 200
  )
  plot(model, type = rc_type)
  dev.off()
  
  # =========================
  # Metadata
  # =========================
  write.csv(
    cbind(species = sp, res[best_i, , drop = FALSE]),
    file.path(sp_dir, sprintf("%s_best_meta.csv", sp)),
    row.names = FALSE
  )
  
  message("✔ saved prediction + response curves for ", sp)
}

cat("\n🎉 ALL DONE — Prediction & standard response curves completed!\n")