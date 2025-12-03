
# パッケージ
library(terra)
library(sf)
library(rnaturalearth)

dir.create("data/raw", TRUE, TRUE)
dir.create("data/clean", TRUE, TRUE)

# 1) ダウンロード
url  <- "https://data.worldpop.org/GIS/Population_Density/Global_2000_2020_1km/2020/JPN/jpn_pd_2020_1km.tif"
fraw <- "data/raw/worldpop_jpn_pd_2020_1km.tif"
if(!file.exists(fraw)) download.file(url, fraw, mode="wb")

# 2) 日本ポリゴンと1 kmテンプレ
jpn  <- rnaturalearth::ne_countries(country="japan", scale="medium", returnclass="sf")
tmpl <- rast(ext(vect(jpn)), resolution=0.008333, crs="EPSG:4326")  # ≈1 km

# 3) 読み込み・クリップ・テンプレへ統一
wp  <- rast(fraw)                             # 単位: 人/km²
wp  <- crop(wp, vect(jpn), snap="out")        # 念のため日本範囲に切り出し
wp1 <- project(wp, crs(tmpl), method="bilinear") |> resample(tmpl, "bilinear")

# 4) 保存
writeRaster(wp1, "data/clean/worldpop_jpn_pd_2020_1km.tif", overwrite=TRUE)


# 必要パッケージ
pkgs <- c("rgbif","sf","rnaturalearth","dplyr","readr")
new  <- pkgs[!pkgs %in% installed.packages()[,1]]; if(length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only=TRUE))

dir.create("data/gbif", recursive=TRUE, showWarnings=FALSE)

# すでに取得済み：
bombus_key <- 1340278

# 1) 「北海道以外」のポリゴンを作成
jp_admin1 <- rnaturalearth::ne_states(country="japan", returnclass="sf")

# 都道府県名の列を推定（環境ごとに列名が違う対策）
nmcol <- intersect(c("name","name_en","nameascii","name_ja","name_en"), names(jp_admin1))[1]
stopifnot(!is.na(nmcol))

# 北海道以外を抽出 → 1つのポリゴンに union
poly_no_hkd <- jp_admin1 |>
  dplyr::filter(.data[[nmcol]] != "Hokkaido") |>
  st_make_valid() |>
  st_union() |>
  st_make_valid()

# 軽量化（任意）
poly_no_hkd <- st_simplify(poly_no_hkd, dTolerance=0.001)
wkt <- st_as_text(poly_no_hkd)

Sys.setenv(
  GBIF_USER  = "zuizui",
  GBIF_PWD   = "rich761327",
  GBIF_EMAIL = "rachelzhang0223@gmail.com"
)

u<-Sys.getenv("GBIF_USER"); p<-Sys.getenv("GBIF_PWD"); e<-Sys.getenv("GBIF_EMAIL")
# 1) predicate を作成
preds <- list(
  rgbif::pred("taxonKey", bombus_key),
  rgbif::pred("hasCoordinate", TRUE),
  rgbif::pred_not(rgbif::pred("hasGeospatialIssue", TRUE)),
  rgbif::pred_within(wkt),          # ← geometryは pred_within が確実
  rgbif::pred("country", "JP")
  # 例: rgbif::pred_gte("year", 2000),
  #     rgbif::pred_lte("coordinateUncertaintyInMeters", 1000)
)

# 2) do.call で個々の predicate を展開して渡す
key <- do.call(
  rgbif::occ_download,
  c(preds, list(format = "SIMPLE_CSV", user = u, pwd = p, email = e))
)
cat("GBIF download key:", key, "\n")
library(rgbif); library(sf); library(readr); library(dplyr)

dir.create("data/gbif", recursive=TRUE, showWarnings=FALSE)

# 1) 完了待ち
rgbif::occ_download_wait("0001808-251025141854904")

# 2) ZIP取得 & 展開せずに読み込み
zipf <- rgbif::occ_download_get("0001808-251025141854904",
                                path="data/gbif", overwrite=TRUE)
occ  <- rgbif::occ_download_import(zipf)  # data.frame

# 3) 「北海道以外」の保険クリップ（wkt は前に作った poly_no_hkd のWKTを再利用）
#    もし環境が新規なら poly_no_hkd を再作成してください
# pts化 → ポリゴンに含まれる点だけ抽出
pts <- st_as_sf(occ, coords=c("decimalLongitude","decimalLatitude"),
                crs=4326, remove=FALSE)
inside <- st_intersects(pts, poly_no_hkd, sparse=FALSE)[,1]
occ_out <- occ[inside, ]

# 4) （任意）ちょい整形：重複や明らかな変な点の除去例
#    同一点重複を落とす（gbifID はユニークだが念のため）
occ_out <- occ_out |>
  distinct(decimalLongitude, decimalLatitude, eventDate, .keep_all=TRUE)

# 5) 保存（CSV と地理付きGPKG）
keep <- intersect(c("gbifID","scientificName","species",
                    "decimalLatitude","decimalLongitude",
                    "eventDate","year","basisOfRecord",
                    "occurrenceStatus","datasetKey",
                    "institutionCode","collectionCode","countryCode","locality"),
                  names(occ_out))
readr::write_csv(occ_out[, keep],
                 "data/gbif/gbif_bombus_JP_noHokkaido.csv")

pts_out <- st_as_sf(occ_out[, keep],
                    coords=c("decimalLongitude","decimalLatitude"),
                    crs=4326, remove=FALSE)
st_write(pts_out, "data/gbif/gbif_bombus_JP_noHokkaido.gpkg", delete_dsn=TRUE)

cat("✅ 保存:\n - data/gbif/gbif_bombus_JP_noHokkaido.csv\n - data/gbif/gbif_bombus_JP_noHokkaido.gpkg\n")



library(dplyr); library(readr); library(stringr); library(sf)

csv_path <- "data/gbif/gbif_bombus_JP_noHokkaido.csv"
occ <- readr::read_csv(csv_path, show_col_types = FALSE)

# 種名整備
occ$species_fixed <- if("species" %in% names(occ) && any(!is.na(occ$species))){
  occ$species
} else if("scientificName" %in% names(occ)){
  stringr::str_trim(stringr::str_extract(occ$scientificName, "^[A-Za-z]+\\s+[A-Za-z\\-]+"))
} else {
  NA_character_
}
occ <- occ %>% filter(!is.na(species_fixed), species_fixed != "")

# 保存フォルダ
dir.create("data/gbif/species_filtered", recursive = TRUE, showWarnings = FALSE)

############################################################
# 在来のマルハナバチだけを保存（>=20件は任意で切替可）
############################################################

library(dplyr); library(readr); library(stringr); library(sf)

# 元CSV（北海道以外でDLしたやつ）
csv_path <- "data/gbif/gbif_bombus_JP_noHokkaido.csv"
occ <- readr::read_csv(csv_path, show_col_types = FALSE)

# 種名整備（speciesが空なら scientificName から属+種を抽出）
occ$species_fixed <- if("species" %in% names(occ) && any(!is.na(occ$species))){
  occ$species
} else if("scientificName" %in% names(occ)){
  stringr::str_trim(stringr::str_extract(occ$scientificName, "^[A-Za-z]+\\s+[A-Za-z\\-]+"))
} else NA_character_

occ <- occ %>% filter(!is.na(species_fixed), species_fixed != "")

# 在来12種（外来: B. terrestris は除外）
native_species <- c(
  "Bombus ardens",
  "Bombus beaticola",
  "Bombus consobrinus",
  "Bombus deuteronymus",
  "Bombus diversus",
  "Bombus honshuensis",
  "Bombus hypnorum",
  "Bombus hypocrita",
  "Bombus ignitus",
  "Bombus pseudobaicalensis",
  "Bombus schrencki",
  "Bombus ussurensis"
)

native_occ <- occ %>% filter(species_fixed %in% native_species)

# （任意）20件以上だけに限定したい場合は下の1行を有効化
# native_occ <- native_occ %>% group_by(species_fixed) %>% filter(n() >= 20) %>% ungroup()

# 出力ディレクトリ
dir.create("data/gbif/species_native", recursive = TRUE, showWarnings = FALSE)

# 種ごとに書き出し
native_list <- split(native_occ, native_occ$species_fixed)

for(sp in names(native_list)){
  df <- native_list[[sp]]
  fname_base <- stringr::str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  csv_file <- file.path("data/gbif/species_native", paste0(fname_base, ".csv"))
  readr::write_csv(df, csv_file)
  
  # GPKGも保存（座標があれば）
  if(all(c("decimalLongitude","decimalLatitude") %in% names(df))){
    sfobj <- st_as_sf(df, coords = c("decimalLongitude","decimalLatitude"),
                      crs = 4326, remove = FALSE)
    gpkg_file <- file.path("data/gbif/species_native", paste0(fname_base, ".gpkg"))
    st_write(sfobj, gpkg_file, delete_dsn = TRUE, quiet = TRUE)
  }
  cat("✅", sp, ":", nrow(df), "records →", csv_file, "\n")
}

# まとめ（在来種の件数表）
counts <- native_occ %>% count(species_fixed, sort = TRUE)
readr::write_csv(counts, "data/gbif/species_native/_counts_native.csv")

cat("🎉 完了: data/gbif/species_native/ に在来種ファイルを保存しました。\n")





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


tmpl_path <- if (length(cont_files) >= 1) cont_files[1] else wc_files[1]
tmpl  <- rast(tmpl_path)
crs_t <- crs(tmpl); res_t <- res(tmpl)


to_tmpl_extent <- function(f, mthd = "bilinear"){
  r <- rast(f)
  if (!isTRUE(all.equal(crs(r), crs_t))) r <- project(r, crs_t, method = mthd)
  if (!isTRUE(all.equal(res(r), res_t))) r <- resample(r, tmpl, method = mthd)
  ext(r)
}
ext_list <- c(
  lapply(cont_files, to_tmpl_extent, mthd = "bilinear"),
  lapply(wc_files,   to_tmpl_extent, mthd = "near")
)
common_ext <- Reduce(intersect, ext_list)


grid <- crop(extend(tmpl, common_ext), common_ext)


align_one <- function(f, mthd){
  r <- rast(f)
  if (!isTRUE(all.equal(crs(r), crs_t))) r <- project(r, crs_t, method = mthd)
  r <- resample(r, grid, method = mthd)  
  r
}
cont_aligned <- if (length(cont_files)) rast(lapply(cont_files, align_one, mthd = "bilinear")) else NULL
wc_aligned   <- if (length(wc_files))   rast(lapply(wc_files,   align_one, mthd = "near"))     else NULL

if (!is.null(cont_aligned)) names(cont_aligned) <- file_path_sans_ext(basename(cont_files))
if (!is.null(wc_aligned))   names(wc_aligned)   <- file_path_sans_ext(basename(wc_files))


if (!is.null(cont_aligned)) {
  vals <- terra::extract(cont_aligned, occ_pts, ID = FALSE) |> as.data.frame()
  vals <- vals[stats::complete.cases(vals), , drop = FALSE]
  stopifnot("抽出値が空です（座標系や範囲を確認）" = nrow(vals) > 0)
  
  vif_res   <- usdm::vifstep(vals, th = 10)   
  keep_vars <- as.character(vif_res@results$Variables)
  
  cont_kept <- if (length(keep_vars)) subset(cont_aligned, keep_vars) else NULL
  write.csv(vif_res@results, file.path(out_dir, "vif_results_continuous.csv"), row.names = FALSE)
  cat("📝 VIF通過（連続）:", if(length(keep_vars)) paste(keep_vars, collapse = ", ") else "(なし)", "\n")
} else {
  cont_kept <- NULL
  message("⚠️ 連続変数が見つからなかったため、VIF はスキップしました。")
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

dir.create(out_root, recursive = TRUE, showWarnings=FALSE)

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
# 3. Best Model Selection & Prediction
# ============================================================

# ベストモデル選択関数
select_best <- function(res){
  # 1) CBI 最大
  if ("cbi.val.avg" %in% names(res) && any(is.finite(res$cbi.val.avg))) {
    max_cbi <- max(res$cbi.val.avg, na.rm = TRUE)
    cand <- which(res$cbi.val.avg == max_cbi)
    
    # 2) AUC 最大 (タイブレーク)
    if ("auc.val.avg" %in% names(res)) {
      auc_sub <- res$auc.val.avg[cand]
      if (any(is.finite(auc_sub))) {
        cand <- cand[which.max(auc_sub)]
      }
    }
    
    # 3) OR10 最小 (タイブレーク)
    if ("or.10p.val.avg" %in% names(res)) {
      or_sub <- res$or.10p.val.avg[cand]
      if (any(is.finite(or_sub))) {
        cand <- cand[which.min(or_sub)]
      }
    }
    return(cand[1])
  }
  
  # フォールバック: AICc最小
  if ("AICc" %in% names(res) && any(is.finite(res$AICc))) {
    return(which.min(res$AICc))
  }
  
  # フォールバック: AUC最大
  if ("auc.val.avg" %in% names(res) && any(is.finite(res$auc.val.avg))) {
    return(which.max(res$auc.val.avg))
  }
  
  return(1L)
}

# 予測用ラッパー関数
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

# 予測ループ
for (sp in targets) {
  sp_dir <- file.path(out_root, sp)
  rds <- file.path(sp_dir, paste0(sp, "_enmeval.rds"))
  if (!file.exists(rds)) next
  
  e <- readRDS(rds)
  res <- eval.results(e)
  
  # ベストモデル選択
  best_i <- select_best(res)
  model <- eval.models(e)[[best_i]]
  
  message("-> Predicting ", sp, " | best row = ", best_i,
          " | fc=", res$fc[best_i], ", rm=", res$rm[best_i])
  
  # 予測マップ作成
  pred <- predict_maxnet_rast(env, model, categoricals)
  out_tif <- file.path(sp_dir, sprintf("%s_best_cloglog.tif", sp))
  writeRaster(pred, out_tif, overwrite=TRUE)
  
  # メタデータ保存
  write.csv(cbind(species=sp, res[best_i, , drop=FALSE]),
            file.path(sp_dir, sprintf("%s_best_meta.csv", sp)),
            row.names = FALSE)
  
  message("✔ saved prediction for ", sp)
}

cat("\n🎉 ALL DONE — Processing Complete!\n")