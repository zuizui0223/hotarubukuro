reanalysis_script_environment <- new.env(parent = globalenv())
local({
  previous_working_directory <- getwd()
  on.exit(setwd(previous_working_directory), add = TRUE)
  setwd(test_project_root)
  sys.source(
    file.path(test_project_root, "scripts", "run_reanalysis.R"),
    envir = reanalysis_script_environment
  )
})

testthat::test_that("synthetic reviewed reanalysis is end-to-end and deterministic", {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("digest")
  testthat::skip_if_not_installed("yaml")

  pipeline <- reanalysis_script_environment
  testthat::expect_identical(
    normalizePath(pipeline$repo_root, winslash = "/"),
    normalizePath(test_project_root, winslash = "/")
  )

  root <- tempfile("reanalysis-e2e-")
  dir.create(root)
  raster_dir <- file.path(root, "public-rasters")
  sdm_dir <- file.path(root, "sdm")
  dir.create(raster_dir)
  dir.create(sdm_dir)

  resolution <- 30 / 3600
  template <- terra::rast(
    nrows = 4,
    ncols = 6,
    xmin = 135,
    xmax = 135 + 6 * resolution,
    ymin = 35,
    ymax = 35 + 4 * resolution,
    crs = "EPSG:4326"
  )
  cells <- seq_len(terra::ncell(template))
  cell_xy <- terra::xyFromCell(template, cells)

  # Thirty photographs occupy 24 exact sites; six sites each have two photos.
  site_cell <- c(cells, cells[seq_len(6L)])
  photograph_index <- seq_along(site_cell)
  clamp_rgb <- function(value) pmin(250, pmax(5, value))
  primary_r <- 65 + (photograph_index * 37) %% 165
  primary_g <- 50 + (photograph_index * 53) %% 175
  primary_b <- 55 + (photograph_index * 29) %% 170
  synthetic_ids <- sprintf("photo-%03d", photograph_index)
  colour_data <- data.frame(
    observation_id = synthetic_ids,
    image_sha256 = vapply(
      synthetic_ids,
      digest::digest,
      character(1),
      algo = "sha256",
      serialize = FALSE
    ),
    duplicate_image_sha256 = FALSE,
    source_row = photograph_index + 1L,
    date = as.character(as.Date("2024-06-01") + photograph_index),
    latitude = cell_xy[site_cell, 2L],
    longitude = cell_xy[site_cell, 1L],
    coordinate_source = "source_workbook",
    coordinate_crs_assumed = "EPSG:4326",
    coordinate_recomputed = FALSE,
    coordinate_qc_status = "source_value_not_independently_recomputed",
    exact_site_id = sprintf("site-%03d", site_cell),
    photo_coordinate_qc_status = "mapped_by_workbook_cell_and_image_hash",
    colour_measurement_scope = "uncalibrated_display_referred_sRGB",
    neutral_reference_available = FALSE,
    illumination_correction_status = "not_identifiable",
    R = primary_r,
    G = primary_g,
    B = primary_b,
    median_R = primary_r,
    median_G = primary_g,
    median_B = primary_b,
    mean_R = clamp_rgb(primary_r + ((photograph_index %% 5L) - 2L) * 2),
    mean_G = clamp_rgb(primary_g + ((photograph_index %% 7L) - 3L) * 2),
    mean_B = clamp_rgb(primary_b + ((photograph_index %% 3L) - 1L) * 3),
    legacy_R = clamp_rgb(primary_r + ((photograph_index %% 4L) - 2L) * 4),
    legacy_G = clamp_rgb(primary_g - ((photograph_index %% 6L) - 3L) * 3),
    legacy_B = clamp_rgb(primary_b + ((photograph_index %% 5L) - 2L) * 5),
    hsv_peak_R = clamp_rgb(primary_r + photograph_index %% 3L),
    hsv_peak_G = clamp_rgb(primary_g - photograph_index %% 4L),
    hsv_peak_B = clamp_rgb(primary_b + photograph_index %% 5L),
    hsv_exposure_filtered_peak_R = clamp_rgb(primary_r - photograph_index %% 5L),
    hsv_exposure_filtered_peak_G = clamp_rgb(primary_g + photograph_index %% 3L),
    hsv_exposure_filtered_peak_B = clamp_rgb(primary_b - photograph_index %% 4L),
    alpha_peak_R = clamp_rgb(primary_r + photograph_index %% 7L),
    alpha_peak_G = clamp_rgb(primary_g + photograph_index %% 5L),
    alpha_peak_B = clamp_rgb(primary_b - photograph_index %% 6L),
    white_balance_applied = FALSE,
    white_balance_method = "none",
    white_balance_reliability = "unavailable",
    color_statistic = "median",
    primary_colour_method = "median_hsv_mask_v2_1_compatible",
    qc_status = ifelse(photograph_index %% 10L == 0L, "manual_review_required", "ok"),
    extraction_version = "2.2.2",
    processed_at = "2026-07-19T00:00:00Z",
    stringsAsFactors = FALSE
  )
  input_path <- file.path(root, "Data_S1_synthetic.csv")
  utils::write.csv(colour_data, input_path, row.names = FALSE)
  legacy_input_path <- file.path(root, "Data_S1_legacy_synthetic.csv")
  utils::write.csv(
    data.frame(
      date = colour_data$date,
      latitude = colour_data$latitude,
      longitude = colour_data$longitude,
      R = colour_data$legacy_R,
      G = colour_data$legacy_G,
      B = colour_data$legacy_B
    ),
    legacy_input_path,
    row.names = FALSE
  )

  source_registry_path <- file.path(test_project_root, "config", "raster_sources.csv")
  registry <- pipeline$read_raster_sources(source_registry_path)
  registry <- registry[registry$enabled, , drop = FALSE]
  registry$native_crs <- "EPSG:4326"
  registry$native_resolution_arcsec <- 30
  registry_path <- file.path(root, "raster_sources.csv")
  testthat::expect_equal(nrow(registry), 19L)

  set.seed(20260719)
  public_paths <- setNames(
    file.path(raster_dir, paste0(registry$source_id, ".tif")),
    registry$source_id
  )
  public_values <- lapply(seq_len(nrow(registry)), function(index) {
    1 + stats::runif(terra::ncell(template), min = index / 20, max = index + 4)
  })
  names(public_values) <- registry$source_id
  public_values$soilgrids_bdod_0_5cm <- 0.7 + stats::runif(terra::ncell(template), 0, 0.7)
  public_values$soilgrids_phh2o_0_5cm <- 4.5 + stats::runif(terra::ncell(template), 0, 3.5)
  public_values$worldpop_2020_count <- 2 + stats::runif(terra::ncell(template), 0, 50)

  count_raster <- template
  terra::values(count_raster) <- public_values$worldpop_2020_count
  density_raster <- count_raster / terra::cellSize(count_raster, unit = "km")
  public_values$worldpop_2020_density <- terra::values(density_raster, mat = FALSE)

  for (source_id in registry$source_id) {
    raster <- template
    terra::values(raster) <- public_values[[source_id]]
    terra::writeRaster(
      raster,
      public_paths[[source_id]],
      overwrite = TRUE,
      datatype = if (grepl("^worldpop_", source_id)) "FLT4S" else "FLT8S"
    )
  }
  processed_hashes <- vapply(
    unname(public_paths[registry$source_id]),
    pipeline$sha256_file,
    character(1)
  )
  cache_hashes <- processed_hashes
  cache_hashes[registry$source_id == "worldpop_2020_density"] <-
    cache_hashes[registry$source_id == "worldpop_2020_count"]
  registry$expected_sha256 <- unname(cache_hashes)
  utils::write.csv(
    registry,
    registry_path,
    row.names = FALSE,
    na = ""
  )

  target_extent <- paste(
    format(as.vector(terra::ext(template)), digits = 17L, scientific = FALSE),
    collapse = ";"
  )
  public_manifest <- data.frame(
    source_id = registry$source_id,
    provider = registry$provider,
    dataset_version = registry$dataset_version,
    source_url = registry$url,
    source_page = registry$source_page,
    license = registry$license,
    cache_path = unname(public_paths[registry$source_id]),
    cache_bytes = unname(file.info(public_paths[registry$source_id])$size),
    native_crs = "EPSG:4326",
    native_res_x = resolution,
    native_res_y = resolution,
    native_extent = target_extent,
    provider_native_resolution = ifelse(
      registry$access == "wcs",
      "250_m",
      "30_arcsec"
    ),
    processed_path = unname(public_paths[registry$source_id]),
    cache_sha256 = unname(cache_hashes),
    processed_sha256 = unname(processed_hashes),
    target_crs = "EPSG:4326",
    target_res_x = resolution,
    target_res_y = resolution,
    target_extent = target_extent,
    output_value_semantics = vapply(
      seq_len(nrow(registry)),
      function(index) pipeline$output_semantics(registry[index, , drop = FALSE]),
      character(1)
    ),
    source_value_semantics = registry$value_semantics,
    resample_method = registry$resample_method,
    scale_factor = registry$scale_factor,
    unit = registry$unit,
    processed_min = vapply(
      unname(public_paths[registry$source_id]),
      function(path) as.numeric(terra::minmax(terra::rast(path))[[1L]]),
      numeric(1)
    ),
    processed_max = vapply(
      unname(public_paths[registry$source_id]),
      function(path) as.numeric(terra::minmax(terra::rast(path))[[2L]]),
      numeric(1)
    ),
    valid_cells = terra::ncell(template),
    missing_cells = 0,
    processing_fingerprint = paste(rep("f", 64L), collapse = ""),
    validity_mask_rule = ifelse(
      grepl("^soilgrids_", registry$source_id),
      "soilgrids_bdod_positive_before_reprojection",
      NA_character_
    ),
    validity_mask_sha256 = ifelse(
      grepl("^soilgrids_", registry$source_id),
      cache_hashes[registry$source_id == "soilgrids_bdod_0_5cm"],
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
  public_manifest_path <- file.path(root, "public_manifest.csv")
  utils::write.csv(public_manifest, public_manifest_path, row.names = FALSE, na = "")

  species <- pipeline$expected_bombus_species()
  set.seed(20260720)
  sdm_rows <- lapply(seq_along(species), function(index) {
    raster <- template
    terra::values(raster) <- stats::runif(
      terra::ncell(template),
      min = index / 100,
      max = 0.7 + index / 25
    )
    names(raster) <- species[[index]]
    path <- file.path(sdm_dir, paste0(species[[index]], ".tif"))
    terra::writeRaster(raster, path, overwrite = TRUE, datatype = "FLT8S")
    pipeline$inspect_sdm_raster(path, species[[index]])
  })
  sdm_manifest <- do.call(rbind, sdm_rows)
  sdm_manifest$value_semantics <- "continuous_habitat_suitability"
  sdm_manifest$model_output <- "maxnet_cloglog"
  sdm_manifest$provenance_status <- "legacy_unverifiable"
  utils::write.csv(
    sdm_manifest,
    file.path(sdm_dir, "manifest.csv"),
    row.names = FALSE,
    na = "NaN"
  )

  pipeline_config <- yaml::read_yaml(
    file.path(test_project_root, "config", "pipeline.yml")
  )
  extent <- as.vector(terra::ext(template))
  pipeline_config$grid$extent <- list(
    xmin = extent[[1L]], xmax = extent[[2L]],
    ymin = extent[[3L]], ymax = extent[[4L]]
  )
  pipeline_path <- file.path(root, "pipeline.yml")
  yaml::write_yaml(pipeline_config, pipeline_path, precision = 17L)

  spatial_libraries <- terra::gdal(lib = "all")
  public_manifest$processing_algorithm_version <- 3L
  public_manifest$pipeline_version <- 3L
  public_manifest$pipeline_config_sha256 <- pipeline$sha256_file(pipeline_path)
  public_manifest$raster_registry_sha256 <- pipeline$sha256_file(registry_path)
  public_manifest$raster_processing_code_sha256 <- pipeline$sha256_file(
    file.path(test_project_root, "R", "raster_sources.R")
  )
  public_manifest$raster_preparation_script_sha256 <- pipeline$sha256_file(
    file.path(test_project_root, "scripts", "prepare_rasters.R")
  )
  public_manifest$terra_version <- as.character(utils::packageVersion("terra"))
  public_manifest$gdal_version <- unname(spatial_libraries[["gdal"]])
  public_manifest$proj_version <- unname(spatial_libraries[["proj"]])
  public_manifest$geos_version <- unname(spatial_libraries[["geos"]])
  utils::write.csv(
    public_manifest,
    public_manifest_path,
    row.names = FALSE,
    na = ""
  )

  # Keep the reviewed methods, predictors, variants and spatial CV, but limit
  # the synthetic test to the primary predictive outcome for speed. The global
  # reference PC1 remains a descriptive artifact and is never cross-validated.
  analysis_config <- yaml::read_yaml(
    file.path(test_project_root, "config", "reanalysis.yml")
  )
  analysis_config$outcomes <- "a"
  analysis_config_path <- file.path(root, "reanalysis.yml")
  yaml::write_yaml(analysis_config, analysis_config_path)

  make_options <- function(output_dir) {
    pipeline$parse_reanalysis_args(c(
      "--input", input_path,
      "--legacy-input", legacy_input_path,
      "--config", analysis_config_path,
      "--sdm-dir", sdm_dir,
      "--public-raster-manifest", public_manifest_path,
      "--raster-registry", registry_path,
      "--pipeline", pipeline_path,
      "--output-dir", output_dir
    ))
  }
  run_once <- function(output_dir) {
    output_sink_before <- sink.number()
    message_sink_before <- sink.number(type = "message")
    captured <- utils::capture.output(
      result <- suppressMessages(pipeline$run_reanalysis(make_options(output_dir)))
    )
    testthat::expect_equal(sink.number(), output_sink_before)
    testthat::expect_equal(sink.number(type = "message"), message_sink_before)
    testthat::expect_type(result, "list")
    testthat::expect_true(all(c(
      "row_flow", "sensitivity", "missingness", "model_performance",
      "public_validation", "sdm_validation"
    ) %in% names(result)))
    testthat::expect_true(length(captured) > 0L)
    result
  }

  output_a <- file.path(root, "run-a")
  output_b <- file.path(root, "run-b")
  result_a <- run_once(output_a)
  result_b <- run_once(output_b)

  row_flow <- utils::read.csv(
    file.path(output_a, "row_flow.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  testthat::expect_equal(nrow(row_flow), 13L)
  testthat::expect_true(all(row_flow$input_records == nrow(colour_data)))
  photo_flow <- row_flow[row_flow$grain == "photo", , drop = FALSE]
  site_flow <- row_flow[row_flow$grain == "site", , drop = FALSE]
  testthat::expect_equal(nrow(site_flow), 1L)
  testthat::expect_equal(site_flow$analysis_units_after_grain, 24L)
  testthat::expect_equal(
    photo_flow$analysis_units_after_grain[photo_flow$qc_scenario == "inclusive"],
    rep(nrow(colour_data), 6L)
  )
  testthat::expect_equal(
    photo_flow$analysis_units_after_grain[photo_flow$qc_scenario == "strict_ok"],
    rep(sum(colour_data$qc_status == "ok"), 6L)
  )
  site_membership <- utils::read.csv(
    file.path(output_a, "site_membership.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  testthat::expect_equal(nrow(site_membership), 24L)
  testthat::expect_equal(sum(site_membership$n_photos), nrow(colour_data))

  csv_paths <- list.files(
    output_a,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  duplicate_headers <- vapply(csv_paths, function(path) {
    value <- utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    anyDuplicated(names(value)) > 0L
  }, logical(1))
  testthat::expect_false(any(duplicate_headers), info = paste(
    csv_paths[duplicate_headers],
    collapse = ", "
  ))

  output_manifest <- utils::read.csv(
    file.path(output_a, "output_manifest.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  testthat::expect_false(anyDuplicated(output_manifest$artifact) > 0L)
  manifest_basenames <- basename(output_manifest$artifact)
  testthat::expect_equal(sum(manifest_basenames == "run.log"), 1L)
  testthat::expect_equal(sum(manifest_basenames == "run_manifest.yml"), 1L)
  testthat::expect_false("output_manifest.csv" %in% manifest_basenames)
  for (artifact in c("run.log", "run_manifest.yml")) {
    row <- output_manifest[manifest_basenames == artifact, , drop = FALSE]
    path <- file.path(output_a, artifact)
    testthat::expect_true(file.exists(path))
    testthat::expect_gt(file.info(path)$size, 0)
    testthat::expect_identical(row$sha256, pipeline$sha256_file(path))
  }
  testthat::expect_match(
    paste(readLines(file.path(output_a, "run.log"), warn = FALSE), collapse = "\n"),
    "Analysis tables complete; input snapshot verified; manifest generation pending"
  )

  essential_outputs <- c(
    "row_flow.csv",
    "reference_colour_pca.csv",
    "analysis_cohorts.csv",
    "colour_scores_long.csv",
    "colour_method_sensitivity.csv",
    "legacy_vs_v2_summary.csv",
    "lab_implementation_comparison.csv",
    "raster_join_audit.csv",
    "missingness_audit.csv",
    "predictor_screening.csv",
    "predictor_correlations.csv",
    "spatial_folds.csv",
    "site_membership.csv",
    "model_coefficients.csv",
    "model_performance.csv",
    "model_performance_by_fold.csv"
  )
  hashes_a <- vapply(
    file.path(output_a, essential_outputs),
    pipeline$sha256_file,
    character(1)
  )
  hashes_b <- vapply(
    file.path(output_b, essential_outputs),
    pipeline$sha256_file,
    character(1)
  )
  testthat::expect_identical(unname(hashes_a), unname(hashes_b))
  testthat::expect_identical(result_a$row_flow, result_b$row_flow)
  testthat::expect_identical(result_a$model_performance, result_b$model_performance)
})
