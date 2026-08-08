#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) return(args[[i + 1L]])
  prefix <- paste0(flag, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

required_packages <- c(
  "yaml", "terra", "sf", "ENMeval", "maxnet", "usdm",
  "readr", "digest", "jsonlite"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

config_path <- arg_value("--config", "config/bombus_sdm.yml")
occurrence_dir <- arg_value("--occurrence-dir", "results/bombus_occurrence_snapshot")
raster_dir <- arg_value("--raster-dir", "data/processed/rasters")
output_dir <- arg_value("--output-dir", "results/bombus_sdm_source_build")
flower_data <- arg_value("--flower-data", "Data_S1.csv")
seed_override <- arg_value("--seed", NA_character_)

cfg <- yaml::read_yaml(config_path)
base_seed <- if (!is.na(seed_override)) as.integer(seed_override) else as.integer(cfg$reproducibility$base_seed)
if (length(base_seed) != 1L || !is.finite(base_seed) || base_seed <= 0L) {
  stop("Bombus SDM seed must be one positive integer.", call. = FALSE)
}

thread_vars <- c(
  "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS", "GOTO_NUM_THREADS",
  "BLIS_NUM_THREADS", "GDAL_NUM_THREADS"
)
thread_values <- as.list(rep(as.character(cfg$reproducibility$threads), length(thread_vars)))
names(thread_values) <- thread_vars
do.call(Sys.setenv, thread_values)
Sys.setenv(OMP_DYNAMIC = "FALSE")
options(mc.cores = 1L)
RNGkind(
  kind = cfg$reproducibility$rng_kind,
  normal.kind = cfg$reproducibility$normal_kind,
  sample.kind = cfg$reproducibility$sample_kind
)
set.seed(base_seed)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
model_dir <- file.path(output_dir, "models")
evaluation_dir <- file.path(output_dir, "evaluation")
prediction_dir <- file.path(output_dir, "predictions")
accessible_dir <- file.path(output_dir, "accessible_area")
for (d in c(model_dir, evaluation_dir, prediction_dir, accessible_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

sha256_file <- function(path) {
  unname(digest::digest(file = path, algo = "sha256"))
}

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
if (anyDuplicated(species$short) || nrow(species) != 5L) {
  stop("Expected five uniquely named Bombus species in config.", call. = FALSE)
}

seed_rows <- list()
stage_seed <- function(species_index, stage_offset, species_short, stage_name) {
  value <- as.integer(base_seed + species_index * 1000L + stage_offset)
  seed_rows[[length(seed_rows) + 1L]] <<- data.frame(
    species = species_short,
    stage = stage_name,
    seed = value,
    stringsAsFactors = FALSE
  )
  value
}

predictor_paths <- vapply(names(cfg$predictors), function(name) {
  if (identical(name, "derived")) return(NA_character_)
  file.path(raster_dir, as.character(cfg$predictors[[name]]))
}, character(1))
predictor_paths <- predictor_paths[!is.na(predictor_paths)]
missing_rasters <- predictor_paths[!file.exists(predictor_paths)]
if (length(missing_rasters)) {
  stop("Missing prepared SDM rasters: ", paste(missing_rasters, collapse = ", "), call. = FALSE)
}

sdm_env <- terra::rast(unname(predictor_paths))
names(sdm_env) <- names(predictor_paths)
if (!all(vapply(seq_len(terra::nlyr(sdm_env)), function(i) terra::is.lonlat(sdm_env[[i]]), logical(1)))) {
  stop("All base Bombus SDM rasters must be longitude/latitude layers.", call. = FALSE)
}
if (!all(vapply(seq_len(terra::nlyr(sdm_env)), function(i) {
  terra::compareGeom(sdm_env[[1]], sdm_env[[i]], stopOnError = FALSE)
}, logical(1)))) {
  stop("Prepared Bombus SDM rasters do not share one grid.", call. = FALSE)
}

dem <- sdm_env[["elevation"]]
slope <- terra::terrain(dem, v = "slope", unit = "degrees")
roughness <- terra::terrain(dem, v = "roughness")
names(slope) <- "slope"
names(roughness) <- "roughness"
sdm_env <- c(sdm_env, slope, roughness)

occurrence_list <- vector("list", nrow(species))
names(occurrence_list) <- species$short
occurrence_manifest <- list()
for (i in seq_len(nrow(species))) {
  sh <- species$short[[i]]
  path <- file.path(occurrence_dir, paste0(sh, "_gbif.csv"))
  if (!file.exists(path)) stop("Missing frozen occurrence CSV: ", path, call. = FALSE)
  x <- readr::read_csv(path, show_col_types = FALSE)
  required <- c("key", "decimalLongitude", "decimalLatitude")
  absent <- setdiff(required, names(x))
  if (length(absent)) stop(sh, " occurrence CSV lacks: ", paste(absent, collapse = ", "), call. = FALSE)
  x$decimalLongitude <- suppressWarnings(as.numeric(x$decimalLongitude))
  x$decimalLatitude <- suppressWarnings(as.numeric(x$decimalLatitude))
  x <- x[is.finite(x$decimalLongitude) & is.finite(x$decimalLatitude), , drop = FALSE]
  x <- x[order(as.character(x$key)), , drop = FALSE]
  x <- x[!duplicated(as.character(x$key)), , drop = FALSE]
  occurrence_list[[sh]] <- x
  occurrence_manifest[[i]] <- data.frame(
    species = species$scientific_name[[i]],
    short = sh,
    n_records = nrow(x),
    occurrence_csv = normalizePath(path, winslash = "/", mustWork = TRUE),
    occurrence_sha256 = sha256_file(path),
    stringsAsFactors = FALSE
  )
}
write_csv(do.call(rbind, occurrence_manifest), file.path(output_dir, "occurrence_input_manifest.csv"))

as_xy <- function(x, lon = "lon", lat = "lat") {
  out <- cbind(as.numeric(x[[lon]]), as.numeric(x[[lat]]))
  colnames(out) <- c("lon", "lat")
  storage.mode(out) <- "double"
  out
}

pooled <- do.call(rbind, lapply(seq_len(nrow(species)), function(i) {
  x <- occurrence_list[[species$short[[i]]]]
  data.frame(
    short = species$short[[i]],
    key = as.character(x$key),
    lon = x$decimalLongitude,
    lat = x$decimalLatitude,
    stringsAsFactors = FALSE
  )
}))
pooled <- pooled[order(pooled$short, pooled$key), , drop = FALSE]
pooled_cells <- terra::cellFromXY(sdm_env[[1]], as_xy(pooled))
pooled <- pooled[!is.na(pooled_cells), , drop = FALSE]
pooled_cells <- terra::cellFromXY(sdm_env[[1]], as_xy(pooled))
pooled <- pooled[!duplicated(pooled_cells), , drop = FALSE]

make_species_M <- function(occ_df, species_short) {
  pts <- sf::st_as_sf(occ_df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  pts_ea <- sf::st_transform(pts, as.character(cfg$accessible_area$crs_equal_area))
  bb <- sf::st_bbox(pts_ea)
  range_diag_m <- sqrt(
    (bb[["xmax"]] - bb[["xmin"]])^2 +
      (bb[["ymax"]] - bb[["ymin"]])^2
  )
  buffer_km <- max(
    as.numeric(cfg$accessible_area$min_buffer_km),
    min(
      as.numeric(cfg$accessible_area$max_buffer_km),
      as.numeric(cfg$accessible_area$range_fraction) * range_diag_m / 1000
    )
  )
  m_ea <- sf::st_buffer(
    sf::st_convex_hull(sf::st_union(pts_ea)),
    dist = buffer_km * 1000
  )
  m_ll <- sf::st_transform(sf::st_as_sf(m_ea), 4326)
  m_ll$species <- species_short
  m_ll$buffer_km <- buffer_km
  m_ll$n_occurrence_cells <- nrow(occ_df)
  m_ll
}

predict_cloglog <- function(env, model) {
  terra::predict(
    env, model,
    fun = function(m, newdata, ...) {
      as.numeric(stats::predict(
        m, newdata = as.data.frame(newdata), type = "cloglog"
      ))
    },
    na.rm = TRUE
  )
}

capture_warnings <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = unique(messages))
}

metrics <- list()
warning_rows <- list()
selected_rows <- list()
for (i in seq_len(nrow(species))) {
  sp <- species$scientific_name[[i]]
  sh <- species$short[[i]]
  message("=== ", sp, " ===")

  occ <- occurrence_list[[sh]]
  occ_i <- data.frame(
    lon = occ$decimalLongitude,
    lat = occ$decimalLatitude,
    key = as.character(occ$key),
    stringsAsFactors = FALSE
  )
  occ_cells <- terra::cellFromXY(sdm_env[[1]], as_xy(occ_i))
  occ_i <- occ_i[!is.na(occ_cells), , drop = FALSE]
  occ_cells <- terra::cellFromXY(sdm_env[[1]], as_xy(occ_i))
  occ_i <- occ_i[!duplicated(occ_cells), , drop = FALSE]
  if (nrow(occ_i) < as.integer(cfg$model$minimum_occurrence_cells)) {
    stop("Too few one-per-cell records for ", sp, ": ", nrow(occ_i), call. = FALSE)
  }

  species_M_sf <- make_species_M(occ_i, sh)
  gpkg <- file.path(accessible_dir, paste0(sh, "_accessible_area_M.gpkg"))
  if (file.exists(gpkg)) unlink(gpkg)
  sf::st_write(species_M_sf, gpkg, quiet = TRUE)
  species_M <- terra::vect(species_M_sf)

  species_env <- terra::mask(
    terra::crop(sdm_env, species_M, snap = "out"), species_M
  )
  if (terra::ncell(species_env) < 1000L) {
    stop("Accessible area is too small for ", sp, call. = FALSE)
  }

  vif_seed <- stage_seed(i, 11L, sh, "vif_domain_sample")
  set.seed(vif_seed)
  domain_sample <- terra::spatSample(
    species_env,
    size = min(as.integer(cfg$predictor_screening$domain_sample_n), terra::ncell(species_env)),
    method = "random",
    na.rm = TRUE,
    values = TRUE,
    as.df = TRUE
  )
  domain_sample <- domain_sample[stats::complete.cases(domain_sample), , drop = FALSE]
  variable_sd <- vapply(domain_sample, stats::sd, numeric(1), na.rm = TRUE)
  domain_sample <- domain_sample[, is.finite(variable_sd) & variable_sd > 0, drop = FALSE]
  if (nrow(domain_sample) < as.integer(cfg$predictor_screening$minimum_complete_cells)) {
    stop("Too few complete environmental cells in M for ", sp, call. = FALSE)
  }
  vif_fit <- usdm::vifstep(domain_sample, th = as.numeric(cfg$predictor_screening$vif_threshold))
  kept_vars <- as.character(vif_fit@results$Variables)
  if (!length(kept_vars)) stop("No SDM predictor survived VIF screening for ", sp, call. = FALSE)
  species_env_kept <- species_env[[kept_vars]]
  vif_table <- as.data.frame(vif_fit@results)
  vif_table$species <- sp
  vif_table$short <- sh
  write_csv(vif_table, file.path(evaluation_dir, paste0(sh, "_sdm_predictor_vif.csv")))

  pooled_cells_i <- terra::cellFromXY(species_env_kept[[1]], as_xy(pooled))
  bg_pool <- pooled[!is.na(pooled_cells_i), c("lon", "lat"), drop = FALSE]
  occ_cells_i <- terra::cellFromXY(species_env_kept[[1]], as_xy(occ_i))
  bg_cells_i <- terra::cellFromXY(species_env_kept[[1]], as_xy(bg_pool))
  bg_pool <- bg_pool[
    !is.na(bg_cells_i) & !bg_cells_i %in% occ_cells_i & !duplicated(bg_cells_i),
    , drop = FALSE
  ]
  bg_pool <- bg_pool[order(bg_pool$lon, bg_pool$lat), , drop = FALSE]

  bg_seed <- stage_seed(i, 21L, sh, "target_group_background")
  if (nrow(bg_pool) > as.integer(cfg$background$target_group_n)) {
    set.seed(bg_seed)
    bg_pool <- bg_pool[
      sort(sample.int(nrow(bg_pool), as.integer(cfg$background$target_group_n), replace = FALSE)),
      , drop = FALSE
    ]
  }

  if (nrow(bg_pool) < as.integer(cfg$background$minimum_n) &&
      isTRUE(cfg$background$fallback_random_within_M)) {
    fallback_seed <- stage_seed(i, 31L, sh, "fallback_background")
    set.seed(fallback_seed)
    fallback <- terra::spatSample(
      species_env_kept,
      size = min(as.integer(cfg$background$target_group_n), terra::ncell(species_env_kept)),
      method = "random",
      na.rm = TRUE,
      values = FALSE,
      xy = TRUE
    )
    bg_pool <- as.data.frame(fallback)
    names(bg_pool) <- c("lon", "lat")
    fallback_cells <- terra::cellFromXY(species_env_kept[[1]], as_xy(bg_pool))
    bg_pool <- bg_pool[
      !is.na(fallback_cells) & !fallback_cells %in% occ_cells_i & !duplicated(fallback_cells),
      , drop = FALSE
    ]
    bg_pool <- bg_pool[order(bg_pool$lon, bg_pool$lat), , drop = FALSE]
  }
  if (nrow(bg_pool) < as.integer(cfg$background$minimum_n)) {
    stop("Too few background cells inside M for ", sp, ": ", nrow(bg_pool), call. = FALSE)
  }

  enmeval_seed <- stage_seed(i, 41L, sh, "ENMevaluate")
  set.seed(enmeval_seed)
  eval_capture <- capture_warnings(ENMeval::ENMevaluate(
    occs = as.data.frame(as_xy(occ_i)),
    envs = species_env_kept,
    bg = as.data.frame(as_xy(bg_pool)),
    algorithm = as.character(cfg$model$algorithm),
    partitions = as.character(cfg$model$partitions),
    tune.args = list(
      fc = unlist(cfg$model$feature_classes, use.names = FALSE),
      rm = as.numeric(unlist(cfg$model$regularization_multipliers, use.names = FALSE))
    ),
    parallel = FALSE,
    quiet = TRUE
  ))
  eval_obj <- eval_capture$value
  results <- ENMeval::eval.results(eval_obj)
  if (!("AICc" %in% names(results)) || !any(is.finite(results$AICc))) {
    stop("No finite AICc candidate for ", sp, call. = FALSE)
  }
  selected <- which.min(results$AICc)
  best_model <- ENMeval::eval.models(eval_obj)[[selected]]

  pred <- predict_cloglog(species_env_kept, best_model)
  pred <- terra::mask(pred, species_M)
  names(pred) <- sh
  pred_file <- file.path(prediction_dir, paste0(sh, ".tif"))
  terra::writeRaster(
    pred, pred_file, overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE", "TILED=YES")
  )

  saveRDS(eval_obj, file.path(model_dir, paste0(sh, "_ENMeval.rds")), version = 3)
  write_csv(results, file.path(evaluation_dir, paste0(sh, "_all_models.csv")))

  selected_row <- results[selected, , drop = FALSE]
  selected_row$species <- sp
  selected_row$short <- sh
  selected_row$selected_row <- selected
  selected_row$selection_rule <- as.character(cfg$model$selection_rule)
  selected_row$kept_predictors <- paste(kept_vars, collapse = ";")
  selected_row$n_occurrence_cells <- nrow(occ_i)
  selected_row$n_background <- nrow(bg_pool)
  selected_row$prediction_sha256 <- sha256_file(pred_file)
  selected_rows[[i]] <- selected_row
  write_csv(selected_row, file.path(evaluation_dir, paste0(sh, "_selected_model.csv")))

  metrics[[i]] <- data.frame(
    species = sp,
    short = sh,
    n_occurrence_cells = nrow(occ_i),
    n_background = nrow(bg_pool),
    n_predictors = length(kept_vars),
    selected_row = selected,
    AICc = as.numeric(results$AICc[[selected]]),
    buffer_km = as.numeric(species_M_sf$buffer_km[[1]]),
    prediction_min = as.numeric(terra::global(pred, "min", na.rm = TRUE)[1, 1]),
    prediction_max = as.numeric(terra::global(pred, "max", na.rm = TRUE)[1, 1]),
    prediction_sha256 = sha256_file(pred_file),
    stringsAsFactors = FALSE
  )

  if (length(eval_capture$warnings)) {
    warning_rows[[i]] <- data.frame(
      species = sp,
      short = sh,
      warning = eval_capture$warnings,
      stringsAsFactors = FALSE
    )
  }

  rm(eval_obj, best_model, species_env, species_env_kept, pred)
  invisible(gc())
}

selected_table <- do.call(rbind, selected_rows)
metrics_table <- do.call(rbind, metrics)
warning_table <- if (length(warning_rows)) do.call(rbind, warning_rows) else data.frame(
  species = character(), short = character(), warning = character()
)
seed_table <- do.call(rbind, seed_rows)
write_csv(selected_table, file.path(output_dir, "ENMeval_selected_models.csv"))
write_csv(metrics_table, file.path(output_dir, "bombus_sdm_manifest.csv"))
write_csv(warning_table, file.path(output_dir, "ENMeval_warnings.csv"))
write_csv(seed_table, file.path(output_dir, "seed_registry.csv"))

raster_inputs <- data.frame(
  predictor = names(predictor_paths),
  path = normalizePath(unname(predictor_paths), winslash = "/", mustWork = TRUE),
  sha256 = vapply(unname(predictor_paths), sha256_file, character(1)),
  stringsAsFactors = FALSE
)
write_csv(raster_inputs, file.path(output_dir, "raster_input_manifest.csv"))

if (file.exists(flower_data)) {
  flowers <- readr::read_csv(flower_data, show_col_types = FALSE)
  if (all(c("longitude", "latitude") %in% names(flowers))) {
    points <- terra::vect(
      data.frame(
        longitude = as.numeric(flowers$longitude),
        latitude = as.numeric(flowers$latitude)
      ),
      geom = c("longitude", "latitude"), crs = "EPSG:4326"
    )
    coverage <- data.frame(
      observation_index = seq_len(nrow(flowers)),
      stringsAsFactors = FALSE
    )
    for (sh in species$short) {
      r <- terra::rast(file.path(prediction_dir, paste0(sh, ".tif")))
      value <- terra::extract(r, points, method = "bilinear", ID = FALSE)[[1]]
      coverage[[sh]] <- as.numeric(value)
    }
    coverage$all_five_finite <- stats::complete.cases(coverage[species$short])
    write_csv(coverage, file.path(output_dir, "flower_prediction_coverage.csv"))
    write_csv(data.frame(
      metric = c(
        paste0("finite_", species$short), "all_five_finite"
      ),
      n = c(
        vapply(species$short, function(sh) sum(is.finite(coverage[[sh]])), integer(1)),
        sum(coverage$all_five_finite)
      ),
      stringsAsFactors = FALSE
    ), file.path(output_dir, "flower_prediction_coverage_summary.csv"))
  }
}

all_files <- list.files(output_dir, recursive = TRUE, full.names = TRUE)
all_files <- all_files[file.info(all_files)$isdir %in% FALSE]
all_files <- all_files[basename(all_files) != "file_manifest.csv"]
file_manifest <- data.frame(
  path = substring(normalizePath(all_files, winslash = "/", mustWork = TRUE), nchar(normalizePath(output_dir, winslash = "/", mustWork = TRUE)) + 2L),
  bytes = as.numeric(file.info(all_files)$size),
  sha256 = vapply(all_files, sha256_file, character(1)),
  stringsAsFactors = FALSE
)
file_manifest <- file_manifest[order(file_manifest$path), , drop = FALSE]
write_csv(file_manifest, file.path(output_dir, "file_manifest.csv"))

jsonlite::write_json(
  list(
    config = normalizePath(config_path, winslash = "/", mustWork = TRUE),
    config_sha256 = sha256_file(config_path),
    base_seed = base_seed,
    RNGkind = RNGkind(),
    thread_variables = as.list(stats::setNames(rep(as.character(cfg$reproducibility$threads), length(thread_vars)), thread_vars)),
    algorithm = cfg$model$algorithm,
    partitions = cfg$model$partitions,
    feature_classes = cfg$model$feature_classes,
    regularization_multipliers = cfg$model$regularization_multipliers,
    selection_rule = cfg$model$selection_rule,
    prediction_scale = cfg$model$prediction_scale
  ),
  file.path(output_dir, "source_build_metadata.json"),
  pretty = TRUE, auto_unbox = TRUE
)

cat("Completed seeded Bombus SDM source build at ", output_dir, "\n", sep = "")
