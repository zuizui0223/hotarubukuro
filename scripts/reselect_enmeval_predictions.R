# Re-select already fitted ENMeval candidates by minimum finite AICc and rebuild
# species prediction rasters without downloading occurrences or refitting models.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = "") {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1L]
}

source("scripts/ecological_analysis_v2.R")
require_packages(c("terra", "sf", "ENMeval", "maxnet"))

source_root <- arg_value("--source-root", "")
output_root <- arg_value("--output-root", "results/enmeval_aicc_reselected")
if (!dir.exists(source_root)) stop("Source output root not found: ", source_root, call. = FALSE)

cache_root <- file.path(source_root, "public_environment_cache")
model_dir <- file.path(source_root, "bombus_sdm", "models")
evaluation_dir <- file.path(source_root, "bombus_sdm", "evaluation")
prediction_dir <- file.path(output_root, "predictions")
dir.create(prediction_dir, recursive = TRUE, showWarnings = FALSE)

find_one <- function(pattern) {
  hit <- list.files(cache_root, pattern = pattern, recursive = TRUE, full.names = TRUE)
  if (length(hit) != 1L) stop("Expected one raster matching ", pattern, "; found ", length(hit))
  hit
}

files <- c(
  bio4 = find_one("^bio4_Japan_crop_30s\\.tif$"),
  bio6 = find_one("^bio6_Japan_crop_30s\\.tif$"),
  bio13 = find_one("^bio13_Japan_crop_30s\\.tif$"),
  CMI = find_one("^CMI_Japan_crop_30s\\.tif$"),
  RSDS = find_one("^RSDS_Japan_crop_30s\\.tif$"),
  bdod = find_one("^bdod_0-5cm_mean_30s\\.tif$"),
  cfvo = find_one("^cfvo_0-5cm_mean_30s\\.tif$"),
  sand = find_one("^sand_0-5cm_mean_30s\\.tif$"),
  silt = find_one("^silt_0-5cm_mean_30s\\.tif$"),
  nitrogen = find_one("^nitrogen_0-5cm_mean_30s\\.tif$"),
  soil_pH = find_one("^phh2o_0-5cm_mean_30s\\.tif$"),
  soc = find_one("^soc_0-5cm_mean_30s\\.tif$"),
  elevation = find_one("^elevation_Japan_crop\\.tif$")
)

sdm_env <- terra::rast(unname(files))
names(sdm_env) <- names(files)
dem <- terra::rast(files[["elevation"]])
slope <- terra::resample(terra::terrain(dem, v = "slope", unit = "degrees"), sdm_env[[1]], method = "bilinear")
roughness <- terra::resample(terra::terrain(dem, v = "roughness"), sdm_env[[1]], method = "bilinear")
names(slope) <- "slope"
names(roughness) <- "roughness"
sdm_env <- c(sdm_env, slope, roughness)

predict_cloglog <- function(env, model) {
  terra::predict(
    env, model,
    fun = function(m, newdata, ...) {
      as.numeric(stats::predict(m, newdata = as.data.frame(newdata), type = "cloglog"))
    },
    na.rm = TRUE
  )
}

species <- c("ardens", "beaticola", "consobrinus", "diversus", "honshuensis")
selection <- vector("list", length(species))
for (i in seq_along(species)) {
  sp <- species[i]
  eval_obj <- readRDS(file.path(model_dir, paste0(sp, "_ENMeval.rds")))
  results <- ENMeval::eval.results(eval_obj)
  if (!("AICc" %in% names(results)) || !any(is.finite(results$AICc))) {
    stop("No finite AICc for ", sp, call. = FALSE)
  }
  selected <- which.min(results$AICc)
  model <- ENMeval::eval.models(eval_obj)[[selected]]
  accessible <- sf::st_read(file.path(model_dir, paste0(sp, "_accessible_area_M.gpkg")), quiet = TRUE)
  accessible_v <- terra::vect(accessible)
  env_i <- terra::mask(terra::crop(sdm_env, accessible_v), accessible_v)
  prediction <- predict_cloglog(env_i, model)
  names(prediction) <- sp
  terra::writeRaster(prediction, file.path(prediction_dir, paste0(sp, ".tif")), overwrite = TRUE)

  wanted <- intersect(
    c("fc", "rm", "tune.args", "auc.val.avg", "auc.val.sd", "cbi.val.avg",
      "cbi.val.sd", "or.10p.avg", "or.10p.sd", "AICc", "delta.AICc", "w.AIC", "ncoef"),
    names(results)
  )
  selection[[i]] <- cbind(
    data.frame(species = sp, selected_row = selected, selection_rule = "minimum_finite_AICc"),
    results[selected, wanted, drop = FALSE]
  )
  rm(eval_obj, model, accessible, accessible_v, env_i, prediction)
  invisible(gc())
}

selection <- do.call(rbind, selection)
write_csv_safe(selection, file.path(output_root, "ENMeval_AICc_selection.csv"))
writeLines(
  c(
    "Predictions were regenerated from previously fitted ENMeval candidate models.",
    "Selection rule: minimum finite AICc; spatial-block AUC, CBI, and OR10 are validation diagnostics, not sequential optimization filters.",
    "Output scale: maxnet cloglog relative suitability, not occurrence probability, abundance, or visitation."
  ),
  file.path(output_root, "README.txt")
)
cat("Wrote AICc-reselected predictions to ", normalizePath(prediction_dir, winslash = "/"), "\n", sep = "")
