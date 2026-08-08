#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) return(args[[i + 1L]])
  prefix <- paste0(flag, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

required_packages <- c("yaml", "terra", "ENMeval", "readr", "digest", "jsonlite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

config_path <- arg_value("--config", "config/bombus_sdm.yml")
build_dir <- arg_value("--build-dir")
raster_dir <- arg_value("--raster-dir", "data/processed/rasters")
flower_data <- arg_value("--flower-data", "Data_S1.csv")
if (is.null(build_dir)) stop("--build-dir is required.", call. = FALSE)

cfg <- yaml::read_yaml(config_path)
projection_domain <- as.character(cfg$projection$domain %||% "full_prepared_grid")
projection_clamp <- if (is.null(cfg$projection$clamp)) TRUE else isTRUE(cfg$projection$clamp)
if (!identical(projection_domain, "full_prepared_grid")) {
  stop("Only projection.domain=full_prepared_grid is supported.", call. = FALSE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
sha256_file <- function(path) unname(digest::digest(file = path, algo = "sha256"))
write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path, na = "")
  invisible(path)
}

species <- do.call(rbind, lapply(cfg$species, function(x) {
  data.frame(
    scientific_name = as.character(x$scientific_name),
    short = as.character(x$short),
    stringsAsFactors = FALSE
  )
}))
if (nrow(species) != 5L || anyDuplicated(species$short)) {
  stop("Expected five uniquely named Bombus species.", call. = FALSE)
}

predictor_paths <- vapply(names(cfg$predictors), function(name) {
  if (identical(name, "derived")) return(NA_character_)
  file.path(raster_dir, as.character(cfg$predictors[[name]]))
}, character(1))
predictor_paths <- predictor_paths[!is.na(predictor_paths)]
missing_rasters <- predictor_paths[!file.exists(predictor_paths)]
if (length(missing_rasters)) {
  stop("Missing prepared rasters: ", paste(missing_rasters, collapse = ", "), call. = FALSE)
}

full_env <- terra::rast(unname(predictor_paths))
names(full_env) <- names(predictor_paths)
if (!all(vapply(seq_len(terra::nlyr(full_env)), function(i) {
  terra::compareGeom(full_env[[1]], full_env[[i]], stopOnError = FALSE)
}, logical(1)))) {
  stop("Prepared Bombus SDM rasters do not share one national grid.", call. = FALSE)
}

dem <- full_env[["elevation"]]
slope <- terra::terrain(dem, v = "slope", unit = "degrees")
roughness <- terra::terrain(dem, v = "roughness")
names(slope) <- "slope"
names(roughness) <- "roughness"
full_env <- c(full_env, slope, roughness)

predict_cloglog <- function(env, model, clamp = TRUE) {
  terra::predict(
    env, model,
    fun = function(m, newdata, ...) {
      as.numeric(stats::predict(
        m,
        newdata = as.data.frame(newdata),
        type = "cloglog",
        clamp = clamp
      ))
    },
    na.rm = TRUE
  )
}

selected_path <- file.path(build_dir, "ENMeval_selected_models.csv")
if (!file.exists(selected_path)) stop("Missing selected-model table: ", selected_path, call. = FALSE)
selected <- readr::read_csv(selected_path, show_col_types = FALSE)

prediction_dir <- file.path(build_dir, "predictions_national")
dir.create(prediction_dir, recursive = TRUE, showWarnings = FALSE)
manifest_rows <- vector("list", nrow(species))

for (i in seq_len(nrow(species))) {
  sp <- species$scientific_name[[i]]
  sh <- species$short[[i]]
  row <- selected[selected$short == sh, , drop = FALSE]
  if (nrow(row) != 1L) stop("Expected one selected model for ", sh, call. = FALSE)

  kept_vars <- strsplit(as.character(row$kept_predictors[[1]]), ";", fixed = TRUE)[[1]]
  kept_vars <- kept_vars[nzchar(kept_vars)]
  missing_vars <- setdiff(kept_vars, names(full_env))
  if (length(missing_vars)) {
    stop(sh, " national projection lacks predictors: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  model_path <- file.path(build_dir, "models", paste0(sh, "_ENMeval.rds"))
  if (!file.exists(model_path)) stop("Missing ENMeval object: ", model_path, call. = FALSE)
  eval_obj <- readRDS(model_path)
  model_index <- as.integer(row$selected_row[[1]])
  models <- ENMeval::eval.models(eval_obj)
  if (!is.finite(model_index) || model_index < 1L || model_index > length(models)) {
    stop("Invalid selected model index for ", sh, call. = FALSE)
  }
  best_model <- models[[model_index]]

  message("Projecting ", sp, " to full national predictor grid")
  pred <- predict_cloglog(full_env[[kept_vars]], best_model, clamp = projection_clamp)
  names(pred) <- sh
  pred_file <- file.path(prediction_dir, paste0(sh, ".tif"))
  terra::writeRaster(
    pred, pred_file, overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE", "TILED=YES")
  )

  finite_n <- as.numeric(terra::global(is.finite(pred), "sum", na.rm = TRUE)[1, 1])
  manifest_rows[[i]] <- data.frame(
    species = sp,
    short = sh,
    projection_domain = projection_domain,
    clamp = projection_clamp,
    n_predictors = length(kept_vars),
    kept_predictors = paste(kept_vars, collapse = ";"),
    finite_cells = finite_n,
    prediction_min = as.numeric(terra::global(pred, "min", na.rm = TRUE)[1, 1]),
    prediction_max = as.numeric(terra::global(pred, "max", na.rm = TRUE)[1, 1]),
    prediction_sha256 = sha256_file(pred_file),
    stringsAsFactors = FALSE
  )
  rm(eval_obj, models, best_model, pred)
  invisible(gc())
}

manifest <- do.call(rbind, manifest_rows)
write_csv(manifest, file.path(build_dir, "national_projection_manifest.csv"))

if (file.exists(flower_data)) {
  flowers <- readr::read_csv(flower_data, show_col_types = FALSE)
  if (all(c("longitude", "latitude") %in% names(flowers))) {
    flower_xy <- data.frame(
      longitude = suppressWarnings(as.numeric(flowers$longitude)),
      latitude = suppressWarnings(as.numeric(flowers$latitude))
    )
    points <- terra::vect(flower_xy, geom = c("longitude", "latitude"), crs = "EPSG:4326")
    coverage <- data.frame(observation_index = seq_len(nrow(flowers)), stringsAsFactors = FALSE)
    for (sh in species$short) {
      r <- terra::rast(file.path(prediction_dir, paste0(sh, ".tif")))
      value <- terra::extract(r, points, method = "bilinear", ID = FALSE)[[1]]
      coverage[[sh]] <- as.numeric(value)
    }
    coverage$all_five_finite <- stats::complete.cases(coverage[species$short])
    write_csv(coverage, file.path(build_dir, "flower_prediction_coverage_national.csv"))
    summary <- data.frame(
      metric = c(paste0("finite_", species$short), "all_five_finite", "flower_rows"),
      n = c(
        vapply(species$short, function(sh) sum(is.finite(coverage[[sh]])), integer(1)),
        sum(coverage$all_five_finite),
        nrow(flowers)
      ),
      stringsAsFactors = FALSE
    )
    write_csv(summary, file.path(build_dir, "flower_prediction_coverage_national_summary.csv"))
  }
}

jsonlite::write_json(
  list(
    projection_domain = projection_domain,
    clamp = projection_clamp,
    calibration_domain = "species-specific accessible area M",
    interpretation = paste(
      "M constrains calibration and target-group background; final suitability is projected",
      "to the common national predictor grid. Values outside M are model transfer, not observed occurrence."
    ),
    config_sha256 = sha256_file(config_path)
  ),
  file.path(build_dir, "national_projection_metadata.json"),
  pretty = TRUE, auto_unbox = TRUE
)

cat("Completed national Bombus projection at ", prediction_dir, "\n", sep = "")
