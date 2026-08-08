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
for (d in c(model_dir, evaluation_dir, prediction_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

sha256_file <- function(path) unname(digest::digest(file = path, algo = "sha256"))
write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path, na = "")
  invisible(path)
}
as_xy <- function(x, lon = "lon", lat = "lat") {
  out <- cbind(as.numeric(x[[lon]]), as.numeric(x[[lat]]))
  colnames(out) <- c("lon", "lat")
  storage.mode(out) <- "double"
  out
}
inside_domain <- function(lon, lat) {
  is.finite(lon) & is.finite(lat) &
    lon >= as.numeric(cfg$study_domain$longitude_min) &
    lon <= as.numeric(cfg$study_domain$longitude_max) &
    lat >= as.numeric(cfg$study_domain$latitude_min) &
    lat <= as.numeric(cfg$study_domain$latitude_max)
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

species <- do.call(rbind, lapply(cfg$species, function(x) {
  data.frame(
    scientific_name = as.character(x$scientific_name),
    short = as.character(x$short),
    ecological_group = as.character(x$ecological_group),
    stringsAsFactors = FALSE
  )
}))
if (nrow(species) != 5L || anyDuplicated(species$short)) {
  stop("Expected five uniquely named focal Bombus species.", call. = FALSE)
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

env_all <- terra::rast(unname(predictor_paths))
names(env_all) <- names(predictor_paths)
if (!all(vapply(seq_len(terra::nlyr(env_all)), function(i) {
  terra::compareGeom(env_all[[1]], env_all[[i]], stopOnError = FALSE)
}, logical(1)))) {
  stop("Prepared Bombus SDM rasters do not share one grid.", call. = FALSE)
}
if (!terra::is.lonlat(env_all[[1]])) {
  stop("Bombus predictor grid must use longitude/latitude coordinates.", call. = FALSE)
}

study_extent <- terra::ext(
  as.numeric(cfg$study_domain$longitude_min),
  as.numeric(cfg$study_domain$longitude_max),
  as.numeric(cfg$study_domain$latitude_min),
  as.numeric(cfg$study_domain$latitude_max)
)
env <- terra::crop(env_all, study_extent, snap = "out")
dem <- env[["elevation"]]
slope <- terra::terrain(dem, v = "slope", unit = "degrees")
roughness <- terra::terrain(dem, v = "roughness")
names(slope) <- "slope"
names(roughness) <- "roughness"
env <- c(env, slope, roughness)

# Focal occurrences: exact GBIF snapshot, then one presence per predictor cell.
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
  x <- x[inside_domain(x$decimalLongitude, x$decimalLatitude), , drop = FALSE]
  x <- x[order(as.character(x$key)), , drop = FALSE]
  x <- x[!duplicated(as.character(x$key)), , drop = FALSE]
  cells <- terra::cellFromXY(
    env[[1]], cbind(x$decimalLongitude, x$decimalLatitude)
  )
  keep <- !is.na(cells)
  x <- x[keep, , drop = FALSE]
  cells <- cells[keep]
  x <- x[!duplicated(cells), , drop = FALSE]
  occurrence_list[[sh]] <- x
  occurrence_manifest[[i]] <- data.frame(
    species = species$scientific_name[[i]],
    short = sh,
    ecological_group = species$ecological_group[[i]],
    n_records_in_domain = nrow(x),
    occurrence_csv = normalizePath(path, winslash = "/", mustWork = TRUE),
    occurrence_sha256 = sha256_file(path),
    stringsAsFactors = FALSE
  )
}
write_csv(do.call(rbind, occurrence_manifest), file.path(output_dir, "occurrence_input_manifest.csv"))

# Genus-wide target-group background.  Using all Bombus occurrence cells makes
# the background a proxy for where bumblebees have been recorded under the same
# heterogeneous GBIF observation process.  It is not interpreted as absence.
tg_path <- file.path(occurrence_dir, "bombus_target_group_gbif.csv")
if (!file.exists(tg_path)) stop("Missing genus-wide Bombus target-group snapshot: ", tg_path, call. = FALSE)
tg <- readr::read_csv(tg_path, show_col_types = FALSE)
for (nm in c("decimalLongitude", "decimalLatitude")) {
  if (!(nm %in% names(tg))) stop("Target-group CSV lacks ", nm, call. = FALSE)
  tg[[nm]] <- suppressWarnings(as.numeric(tg[[nm]]))
}
tg <- tg[inside_domain(tg$decimalLongitude, tg$decimalLatitude), , drop = FALSE]
tg <- tg[order(as.character(tg$key)), , drop = FALSE]
tg_cells <- terra::cellFromXY(env[[1]], cbind(tg$decimalLongitude, tg$decimalLatitude))
keep_tg <- !is.na(tg_cells)
tg <- tg[keep_tg, , drop = FALSE]
tg_cells <- tg_cells[keep_tg]
tg <- tg[!duplicated(tg_cells), , drop = FALSE]
tg_cells <- terra::cellFromXY(env[[1]], cbind(tg$decimalLongitude, tg$decimalLatitude))
tg_xy <- data.frame(lon = tg$decimalLongitude, lat = tg$decimalLatitude, cell = tg_cells)
write_csv(data.frame(
  file = normalizePath(tg_path, winslash = "/", mustWork = TRUE),
  sha256 = sha256_file(tg_path),
  n_unique_target_group_cells = nrow(tg_xy),
  background_role = "genus-wide Bombus observation-effort proxy",
  stringsAsFactors = FALSE
), file.path(output_dir, "target_group_background_manifest.csv"))
if (nrow(tg_xy) < as.integer(cfg$background$minimum_n)) {
  stop("Too few genus-wide target-group cells: ", nrow(tg_xy), call. = FALSE)
}

# A single response-blind predictor screen is shared by all five species.  This
# keeps broad and alpine taxa on the same environmental feature basis rather
# than allowing species-specific variable screens to create artificial model
# differences.
vif_seed <- as.integer(base_seed + 101L)
seed_rows[[length(seed_rows) + 1L]] <- data.frame(
  species = "shared", stage = "vif_domain_sample", seed = vif_seed,
  stringsAsFactors = FALSE
)
set.seed(vif_seed)
domain_sample <- terra::spatSample(
  env,
  size = min(as.integer(cfg$predictor_screening$domain_sample_n), terra::ncell(env)),
  method = "random", na.rm = TRUE, values = TRUE, as.df = TRUE
)
domain_sample <- domain_sample[stats::complete.cases(domain_sample), , drop = FALSE]
variable_sd <- vapply(domain_sample, stats::sd, numeric(1), na.rm = TRUE)
domain_sample <- domain_sample[, is.finite(variable_sd) & variable_sd > 0, drop = FALSE]
if (nrow(domain_sample) < as.integer(cfg$predictor_screening$minimum_complete_cells)) {
  stop("Too few complete environmental cells for shared VIF screening.", call. = FALSE)
}
vif_fit <- usdm::vifstep(domain_sample, th = as.numeric(cfg$predictor_screening$vif_threshold))
kept_vars <- as.character(vif_fit@results$Variables)
if (!length(kept_vars)) stop("No predictor survived shared VIF screening.", call. = FALSE)
vif_table <- as.data.frame(vif_fit@results)
write_csv(vif_table, file.path(evaluation_dir, "shared_sdm_predictor_vif.csv"))
env_kept <- env[[kept_vars]]

predict_cloglog <- function(env_rast, model) {
  terra::predict(
    env_rast, model,
    fun = function(m, newdata, ...) {
      as.numeric(stats::predict(
        m, newdata = as.data.frame(newdata), type = "cloglog",
        clamp = isTRUE(cfg$model$clamp)
      ))
    },
    na.rm = TRUE
  )
}

metrics <- list()
warning_rows <- list()
selected_rows <- list()
for (i in seq_len(nrow(species))) {
  sp <- species$scientific_name[[i]]
  sh <- species$short[[i]]
  message("=== ", sp, " (", species$ecological_group[[i]], ") ===")

  occ <- occurrence_list[[sh]]
  occ_i <- data.frame(
    lon = occ$decimalLongitude,
    lat = occ$decimalLatitude,
    key = as.character(occ$key),
    stringsAsFactors = FALSE
  )
  if (nrow(occ_i) < as.integer(cfg$model$minimum_occurrence_cells)) {
    stop("Too few one-per-cell records for ", sp, ": ", nrow(occ_i), call. = FALSE)
  }
  occ_cells_i <- terra::cellFromXY(env_kept[[1]], as_xy(occ_i))

  # Same target-group sampling universe for every species, excluding only the
  # focal species' occupied predictor cells.  A deterministic sample is taken
  # only when the target-group pool exceeds the configured cap.
  bg_pool <- tg_xy[!tg_xy$cell %in% occ_cells_i, c("lon", "lat", "cell"), drop = FALSE]
  bg_pool <- bg_pool[order(bg_pool$cell, bg_pool$lon, bg_pool$lat), , drop = FALSE]
  bg_seed <- stage_seed(i, 21L, sh, "target_group_background")
  if (nrow(bg_pool) > as.integer(cfg$background$target_group_n)) {
    set.seed(bg_seed)
    idx <- sort(sample.int(
      nrow(bg_pool), as.integer(cfg$background$target_group_n), replace = FALSE
    ))
    bg_pool <- bg_pool[idx, , drop = FALSE]
  }
  if (nrow(bg_pool) < as.integer(cfg$background$minimum_n)) {
    stop("Too few target-group background cells for ", sp, ": ", nrow(bg_pool), call. = FALSE)
  }

  enmeval_seed <- stage_seed(i, 41L, sh, "ENMevaluate")
  set.seed(enmeval_seed)
  eval_capture <- capture_warnings(ENMeval::ENMevaluate(
    occs = as.data.frame(as_xy(occ_i)),
    envs = env_kept,
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

  pred <- predict_cloglog(env_kept, best_model)
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
  selected_row$ecological_group <- species$ecological_group[[i]]
  selected_row$selected_row <- selected
  selected_row$selection_rule <- as.character(cfg$model$selection_rule)
  selected_row$kept_predictors <- paste(kept_vars, collapse = ";")
  selected_row$n_occurrence_cells <- nrow(occ_i)
  selected_row$n_background <- nrow(bg_pool)
  selected_row$prediction_sha256 <- sha256_file(pred_file)
  selected_rows[[i]] <- selected_row
  write_csv(selected_row, file.path(evaluation_dir, paste0(sh, "_selected_model.csv")))

  pts <- sf::st_as_sf(occ_i, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  pts_ea <- sf::st_transform(pts, 6933)
  hull_area_km2 <- as.numeric(sf::st_area(sf::st_convex_hull(sf::st_union(pts_ea)))) / 1e6

  metrics[[i]] <- data.frame(
    species = sp,
    short = sh,
    ecological_group = species$ecological_group[[i]],
    n_occurrence_cells = nrow(occ_i),
    occurrence_hull_km2 = hull_area_km2,
    n_background = nrow(bg_pool),
    n_predictors = length(kept_vars),
    selected_row = selected,
    AICc = as.numeric(results$AICc[[selected]]),
    auc_val_avg = if ("auc.val.avg" %in% names(results)) as.numeric(results$auc.val.avg[[selected]]) else NA_real_,
    or10_avg = if ("or.10p.avg" %in% names(results)) as.numeric(results$or.10p.avg[[selected]]) else NA_real_,
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

  rm(eval_obj, best_model, pred)
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

# AUC is reported as a model diagnostic, not as a cross-species biological
# weight.  With only five focal species the relationship with range breadth is
# descriptive; it is useful for recognizing that broad environmental niches are
# intrinsically harder to discriminate from a common background.
auc_diag <- metrics_table[
  is.finite(metrics_table$auc_val_avg) &
    is.finite(metrics_table$occurrence_hull_km2) &
    metrics_table$occurrence_hull_km2 > 0,
  , drop = FALSE
]
if (nrow(auc_diag) >= 3L) {
  rho <- suppressWarnings(stats::cor(
    log(auc_diag$occurrence_hull_km2), auc_diag$auc_val_avg,
    method = "spearman", use = "complete.obs"
  ))
} else rho <- NA_real_
write_csv(data.frame(
  n_species = nrow(auc_diag),
  spearman_auc_vs_log_occurrence_hull = rho,
  interpretation = paste(
    "descriptive only; AUC is not used to weight species because niche breadth,",
    "background contrast and sampling structure affect cross-species AUC"
  ),
  stringsAsFactors = FALSE
), file.path(output_dir, "auc_range_breadth_diagnostic.csv"))

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
      metric = c(paste0("finite_", species$short), "all_five_finite", "flower_rows"),
      n = c(
        vapply(species$short, function(sh) sum(is.finite(coverage[[sh]])), integer(1)),
        sum(coverage$all_five_finite),
        nrow(flowers)
      ),
      stringsAsFactors = FALSE
    ), file.path(output_dir, "flower_prediction_coverage_summary.csv"))
  }
}

all_files <- list.files(output_dir, recursive = TRUE, full.names = TRUE)
all_files <- all_files[file.info(all_files)$isdir %in% FALSE]
all_files <- all_files[basename(all_files) != "file_manifest.csv"]
root_norm <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
file_manifest <- data.frame(
  path = substring(normalizePath(all_files, winslash = "/", mustWork = TRUE), nchar(root_norm) + 2L),
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
    thread_variables = as.list(stats::setNames(
      rep(as.character(cfg$reproducibility$threads), length(thread_vars)), thread_vars
    )),
    study_domain = cfg$study_domain,
    background_source = cfg$background$source,
    target_group_snapshot_sha256 = sha256_file(tg_path),
    shared_predictors = kept_vars,
    algorithm = cfg$model$algorithm,
    partitions = cfg$model$partitions,
    feature_classes = cfg$model$feature_classes,
    regularization_multipliers = cfg$model$regularization_multipliers,
    selection_rule = cfg$model$selection_rule,
    prediction_scale = cfg$model$prediction_scale,
    clamp = cfg$model$clamp,
    availability_use = cfg$availability_exposure
  ),
  file.path(output_dir, "source_build_metadata.json"),
  pretty = TRUE, auto_unbox = TRUE
)

cat("Completed ecology-aligned seeded Bombus SDM build at ", output_dir, "\n", sep = "")
