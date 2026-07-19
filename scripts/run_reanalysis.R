#!/usr/bin/env Rscript

# Reanalyse the reviewed colour table against the complete prepared public
# predictor stack and the structurally validated legacy Bombus rasters. The
# output is descriptive and sensitivity-focused; legacy CFA/INLA/qGAM models
# are not executed.

current_reanalysis_script_path <- function() {
  frame_files <- vapply(sys.frames(), function(frame) {
    value <- frame$ofile
    if (is.null(value)) "" else as.character(value)[1L]
  }, character(1))
  frame_files <- frame_files[nzchar(frame_files)]
  matching <- frame_files[basename(frame_files) == "run_reanalysis.R"]
  if (length(matching)) {
    return(normalizePath(
      matching[length(matching)], winslash = "/", mustWork = TRUE
    ))
  }
  script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(script_argument)) {
    candidate <- sub("^--file=", "", script_argument[[1L]])
    if (basename(candidate) == "run_reanalysis.R") {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  normalizePath("scripts/run_reanalysis.R", winslash = "/", mustWork = TRUE)
}

script_path <- current_reanalysis_script_path()
repo_root <- dirname(dirname(script_path))
source(file.path(repo_root, "scripts", "run_analysis.R"), local = TRUE)
source(file.path(repo_root, "R", "reanalysis.R"), local = TRUE)

reanalysis_usage <- function() {
  paste(
    "Usage:",
    "  Rscript scripts/run_reanalysis.R [options]",
    "",
    "Options:",
    "  --input PATH              Reviewed public colour table (default: Data_S1.csv)",
    "  --legacy-input PATH       Preserved previous six-column table",
    "  --config PATH             Reanalysis specification (default: config/reanalysis.yml)",
    "  --sdm-dir PATH            Validated legacy SDM directory (default: sdm)",
    "  --public-raster-manifest PATH  Complete prepared raster manifest",
    "  --raster-registry PATH    Public source registry",
    "  --pipeline PATH           Public raster grid configuration",
    "  --output-dir PATH         New output directory (must be empty/nonexistent)",
    "  --help                    Show this help",
    sep = "\n"
  )
}

parse_reanalysis_args <- function(args) {
  values <- list(
    input = file.path(repo_root, "Data_S1.csv"),
    legacy_input = file.path(repo_root, "data", "legacy", "Data_S1_legacy.csv"),
    config = file.path(repo_root, "config", "reanalysis.yml"),
    sdm_dir = file.path(repo_root, "sdm"),
    public_raster_manifest = file.path(
      repo_root, "data", "processed", "raster_manifest.csv"
    ),
    raster_registry = file.path(repo_root, "config", "raster_sources.csv"),
    pipeline = file.path(repo_root, "config", "pipeline.yml"),
    output_dir = file.path(repo_root, "results", "reanalysis", "v2_2_2"),
    help = FALSE
  )
  mapping <- c(
    "--input" = "input",
    "--legacy-input" = "legacy_input",
    "--config" = "config",
    "--sdm-dir" = "sdm_dir",
    "--public-raster-manifest" = "public_raster_manifest",
    "--env-manifest" = "public_raster_manifest",
    "--raster-registry" = "raster_registry",
    "--pipeline" = "pipeline",
    "--output-dir" = "output_dir"
  )
  index <- 1L
  while (index <= length(args)) {
    argument <- args[[index]]
    if (argument %in% names(mapping)) {
      if (index == length(args)) stop("Missing value after ", argument, call. = FALSE)
      values[[mapping[[argument]]]] <- args[[index + 1L]]
      index <- index + 2L
    } else if (argument %in% c("--help", "-h")) {
      values$help <- TRUE
      index <- index + 1L
    } else {
      stop("Unknown argument: ", argument, call. = FALSE)
    }
  }
  path_names <- setdiff(names(values), "help")
  for (name in path_names) values[[name]] <- canonical_cli_path(values[[name]])
  values
}

write_lines_portable <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(path), "."), dirname(path), ".part")
  on.exit(unlink(temporary), add = TRUE)
  writeLines(lines, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) stop("Could not atomically write: ", path, call. = FALSE)
  invisible(path)
}

prefix_columns <- function(data, values) {
  for (name in names(values)) data[[name]] <- values[[name]]
  data
}

safe_mean <- function(x, index) {
  values <- x[index & is.finite(x)]
  if (length(values)) mean(values) else NA_real_
}

audit_value <- function(audit, metric) {
  value <- audit$value[audit$metric == metric]
  if (length(value) != 1L || !is.finite(value)) {
    stop("Audit metric is missing or non-scalar: ", metric, call. = FALSE)
  }
  as.integer(value)
}

run_reanalysis <- function(options) {
  if (options$help) {
    cat(reanalysis_usage(), "\n")
    return(invisible(NULL))
  }
  inputs <- c(
    options$input,
    options$legacy_input,
    options$config,
    options$raster_registry,
    options$pipeline,
    options$public_raster_manifest,
    file.path(options$sdm_dir, "manifest.csv")
  )
  code_files <- file.path(repo_root, c(
    "scripts/run_reanalysis.R",
    "scripts/run_analysis.R",
    "scripts/prepare_rasters.R",
    "R/reanalysis.R",
    "R/analysis_core.R",
    "R/public_predictors.R",
    "R/raster_sources.R",
    "R/sdm.R"
  ))
  dependency_files <- file.path(repo_root, "renv.lock")
  snapshot_paths <- unique(c(inputs, code_files, dependency_files))
  missing_inputs <- snapshot_paths[!file.exists(snapshot_paths)]
  if (length(missing_inputs)) {
    stop("Required reanalysis inputs are missing: ", paste(missing_inputs, collapse = ", "), call. = FALSE)
  }
  snapshot_paths <- normalizePath(snapshot_paths, winslash = "/", mustWork = TRUE)
  initial_hashes <- setNames(
    vapply(snapshot_paths, sha256_file, character(1)),
    snapshot_paths
  )
  snapshot_hash <- function(path) {
    normalized <- normalizePath(path, winslash = "/", mustWork = TRUE)
    value <- initial_hashes[[normalized]]
    if (is.null(value)) stop("Path is outside the reanalysis snapshot: ", path, call. = FALSE)
    value
  }
  if (file.exists(options$output_dir) && length(list.files(options$output_dir, all.files = TRUE, no.. = TRUE))) {
    stop("Refusing to overwrite non-empty output directory: ", options$output_dir, call. = FALSE)
  }
  started_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)

  git_head <- tryCatch(
    trimws(system2("git", c("-C", repo_root, "rev-parse", "HEAD"), stdout = TRUE)),
    error = function(error) NA_character_
  )
  git_status <- tryCatch(
    system2("git", c("-C", repo_root, "status", "--porcelain"), stdout = TRUE),
    error = function(error) "<unavailable>"
  )
  git_dirty <- length(git_status) > 0L && any(nzchar(git_status))

  dir.create(options$output_dir, recursive = TRUE, showWarnings = FALSE)
  input_dir <- file.path(options$output_dir, "inputs")
  dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(options$output_dir, "run.log")
  output_sink_depth <- sink.number()
  message_sink_connection <- sink.number(type = "message")
  log_connection <- file(log_path, open = "wt", encoding = "UTF-8")
  log_closed <- FALSE
  close_run_log <- function() {
    if (log_closed) return(invisible(NULL))
    if (sink.number(type = "message") != message_sink_connection) {
      sink(type = "message")
    }
    while (sink.number() > output_sink_depth) sink()
    close(log_connection)
    log_closed <<- TRUE
    invisible(NULL)
  }
  sink(log_connection, split = TRUE)
  sink(log_connection, type = "message")
  on.exit(close_run_log(), add = TRUE)

  config <- read_reanalysis_config(options$config)
  data_s1_all <- read_colour_data(options$input, allow_missing_rgb = TRUE)
  data_contract_audit <- validate_reviewed_data_s1(data_s1_all)
  legacy_source <- utils::read.csv(
    options$legacy_input,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
  required_legacy <- c("date", "latitude", "longitude", "R", "G", "B")
  if (length(setdiff(required_legacy, names(legacy_source)))) {
    stop("Preserved legacy input has an unexpected schema.", call. = FALSE)
  }
  legacy_available <- finite_complete_rows(
    data_s1_all,
    c("legacy_R", "legacy_G", "legacy_B")
  )
  data_s1_legacy <- data_s1_all[legacy_available, , drop = FALSE]
  if (nrow(legacy_source) != nrow(data_s1_legacy)) {
    stop("Legacy input and Data_S1 legacy crosswalk row counts differ.", call. = FALSE)
  }
  legacy_dates <- as.Date(as.character(legacy_source$date))
  if (anyNA(legacy_dates) ||
      !identical(legacy_dates, as.Date(data_s1_legacy$date))) {
    stop("Legacy input dates no longer match the Data_S1 legacy rows.", call. = FALSE)
  }
  legacy_differences <- cbind(
    as.matrix(legacy_source[c("latitude", "longitude")]) -
      as.matrix(data_s1_legacy[c("latitude", "longitude")]),
    as.matrix(legacy_source[c("R", "G", "B")]) -
      as.matrix(data_s1_legacy[c("legacy_R", "legacy_G", "legacy_B")])
  )
  if (any(!is.finite(legacy_differences)) || max(abs(legacy_differences)) > 1e-5) {
    stop("Legacy input no longer matches the Data_S1 legacy columns in row order.", call. = FALSE)
  }
  registry <- read_raster_sources(options$raster_registry)
  raster_pipeline <- read_pipeline_config(
    options$pipeline
  )
  expected_public_grid <- canonical_grid(
    pipeline_bbox(raster_pipeline),
    resolution_arcsec = raster_pipeline$grid$resolution_arcsec,
    crs = raster_pipeline$grid$crs,
    origin = c(
      raster_pipeline$grid$origin_x,
      raster_pipeline$grid$origin_y
    )
  )
  expected_source_ids <- registry$source_id[registry$enabled]
  public_validation <- validate_public_raster_collection(
    options$public_raster_manifest,
    project_root = repo_root,
    expected_source_ids = expected_source_ids,
    expected_extent = as.vector(terra::ext(expected_public_grid)),
    registry = registry,
    verify_hashes = TRUE
  )
  provenance_hashes <- list(
    pipeline_config_sha256 = snapshot_hash(options$pipeline),
    raster_registry_sha256 = snapshot_hash(options$raster_registry),
    raster_processing_code_sha256 = snapshot_hash(
      file.path(repo_root, "R", "raster_sources.R")
    ),
    raster_preparation_script_sha256 = snapshot_hash(
      file.path(repo_root, "scripts", "prepare_rasters.R")
    )
  )
  for (field in names(provenance_hashes)) {
    values <- unique(public_validation$manifest[[field]])
    if (length(values) != 1L ||
        !identical(tolower(values), tolower(provenance_hashes[[field]]))) {
      stop("Public raster manifest has stale provenance field: ", field, call. = FALSE)
    }
  }
  sdm_validation <- validate_sdm_collection(options$sdm_dir)
  if (!isTRUE(sdm_validation$structural_ok)) {
    stop("SDM structural validation failed: ", paste(sdm_validation$issues, collapse = "; "), call. = FALSE)
  }

  cat("Reanalysis version:", config$analysis_version, "\n")
  cat("Git HEAD:", git_head, "| dirty before output:", git_dirty, "\n")
  cat("Public predictors:", nrow(public_validation$manifest), "\n")
  cat("SDM model-generation reproducible:", sdm_validation$model_reproducible, "\n")

  run_results <- list()
  row_flow <- list()
  score_rows <- list()
  scenarios <- names(config$qc_scenarios)
  for (scenario in scenarios) {
    accepted <- paste(unlist(config$qc_scenarios[[scenario]]), collapse = ",")
    for (method in unlist(config$colour_methods)) {
      run_name <- paste(scenario, method, sep = "__")
      output <- file.path(input_dir, paste0(run_name, ".csv"))
      loadings <- file.path(input_dir, paste0(run_name, "__colour_pca.csv"))
      result <- run_analysis_cli(
        c(
          "--input", options$input,
          "--sdm-dir", options$sdm_dir,
          "--public-raster-manifest", options$public_raster_manifest,
          "--raster-registry", options$raster_registry,
          "--accepted-qc-status", accepted,
          "--colour-method", method,
          "--grain", "photo",
          "--output", output,
          "--loadings-output", loadings
        ),
        project_root = repo_root,
        public_raster_cache = public_validation,
        sdm_raster_cache = sdm_validation,
        input_data_cache = data_s1_all
      )
      run_results[[run_name]] <- result
      bombus_complete <- if ("Bombus_suitability_sum" %in% names(result$data)) {
        sum(is.finite(result$data$Bombus_suitability_sum))
      } else 0L
      public_complete <- sum(finite_complete_rows(result$data, expected_source_ids))
      row_flow[[run_name]] <- data.frame(
        run = run_name,
        qc_scenario = scenario,
        colour_method = method,
        grain = "photo",
        input_records = audit_value(
          result$input_grain_audit,
          "photograph_records"
        ),
        after_qc = sum(result$qc_audit$records[result$qc_audit$analysis_included]),
        after_photo_coordinate_identity = sum(
          result$photo_coordinate_audit$records[
            result$photo_coordinate_audit$analysis_included
          ]
        ),
        after_colour_method = as.integer(
          result$colour_method_audit$selected_records[[1L]]
        ),
        analysis_units_after_grain = nrow(result$data),
        public_predictors_complete = public_complete,
        bombus_complete = bombus_complete,
        bombus_missing = nrow(result$data) - bombus_complete,
        stringsAsFactors = FALSE
      )
    }
  }

  site_output <- file.path(input_dir, "inclusive__primary__site.csv")
  site_loadings <- file.path(input_dir, "inclusive__primary__site__colour_pca.csv")
  site_result <- run_analysis_cli(
    c(
      "--input", options$input,
      "--sdm-dir", options$sdm_dir,
      "--public-raster-manifest", options$public_raster_manifest,
      "--raster-registry", options$raster_registry,
      "--accepted-qc-status", paste(unlist(config$qc_scenarios$inclusive), collapse = ","),
      "--colour-method", "primary",
      "--grain", "site",
      "--site-id", "exact_site_id",
      "--output", site_output,
      "--loadings-output", site_loadings
    ),
    project_root = repo_root,
    public_raster_cache = public_validation,
    sdm_raster_cache = sdm_validation,
    input_data_cache = data_s1_all
  )
  run_results[["inclusive__primary__site"]] <- site_result
  row_flow[["inclusive__primary__site"]] <- data.frame(
    run = "inclusive__primary__site",
    qc_scenario = "inclusive",
    colour_method = "primary",
    grain = "site",
    input_records = audit_value(
      site_result$input_grain_audit,
      "photograph_records"
    ),
    after_qc = sum(site_result$qc_audit$records[site_result$qc_audit$analysis_included]),
    after_photo_coordinate_identity = sum(
      site_result$photo_coordinate_audit$records[
        site_result$photo_coordinate_audit$analysis_included
      ]
    ),
    after_colour_method = as.integer(
      site_result$colour_method_audit$selected_records[[1L]]
    ),
    analysis_units_after_grain = nrow(site_result$data),
    public_predictors_complete = sum(finite_complete_rows(site_result$data, expected_source_ids)),
    bombus_complete = sum(is.finite(site_result$data$Bombus_suitability_sum)),
    bombus_missing = sum(!is.finite(site_result$data$Bombus_suitability_sum)),
    stringsAsFactors = FALSE
  )
  row_flow <- do.call(rbind, row_flow)
  write_csv_portable(row_flow, file.path(options$output_dir, "row_flow.csv"))

  reference_pca <- run_results[["inclusive__primary"]]$colour_pca
  reference_pca_table <- data.frame(
    variable = names(reference_pca$loadings),
    PC1_loading = unname(reference_pca$loadings),
    center = unname(reference_pca$model$center[names(reference_pca$loadings)]),
    scale = unname(reference_pca$model$scale[names(reference_pca$loadings)]),
    explained_variance_PC1 = reference_pca$explained_variance,
    anchor = reference_pca$anchor,
    stringsAsFactors = FALSE
  )
  write_csv_portable(
    reference_pca_table,
    file.path(options$output_dir, "reference_colour_pca.csv")
  )
  for (run_name in names(run_results)) {
    result <- run_results[[run_name]]
    data <- result$data
    data$primary_reference_PC1 <- predict(reference_pca, data)
    parts <- strsplit(run_name, "__", fixed = TRUE)[[1L]]
    score_rows[[run_name]] <- data.frame(
      analysis_id = data$analysis_id,
      qc_scenario = parts[[1L]],
      colour_method = parts[[2L]],
      grain = if (length(parts) >= 3L) parts[[3L]] else "photo",
      L = data$L,
      a = data$a,
      b = data$b,
      C = data$C,
      method_specific_PC1 = data$colour_PC1,
      primary_reference_PC1 = data$primary_reference_PC1,
      stringsAsFactors = FALSE
    )
    run_results[[run_name]]$data <- data
  }
  colour_scores <- do.call(rbind, score_rows)
  rownames(colour_scores) <- NULL
  write_csv_portable(colour_scores, file.path(options$output_dir, "colour_scores_long.csv"))

  cohort_rows <- list()
  common_method_ids <- list()
  for (scenario in scenarios) {
    method_ids <- lapply(unlist(config$colour_methods), function(method) {
      run_results[[paste(scenario, method, sep = "__")]]$data$analysis_id
    })
    names(method_ids) <- unlist(config$colour_methods)
    common_all <- Reduce(intersect, method_ids)
    common_method_ids[[scenario]] <- common_all
    for (method in names(method_ids)) {
      cohort_rows[[paste(scenario, method, sep = "__")]] <- data.frame(
        qc_scenario = scenario,
        colour_method = method,
        method_specific_records = length(method_ids[[method]]),
        all_methods_common_records = length(common_all),
        stringsAsFactors = FALSE
      )
    }
  }
  analysis_cohorts <- do.call(rbind, cohort_rows)
  write_csv_portable(
    analysis_cohorts,
    file.path(options$output_dir, "analysis_cohorts.csv")
  )

  sensitivity <- list()
  for (scenario in scenarios) {
    primary <- run_results[[paste(scenario, "primary", sep = "__")]]$data
    for (method in setdiff(unlist(config$colour_methods), "primary")) {
      candidate <- run_results[[paste(scenario, method, sep = "__")]]$data
      summary <- pairwise_colour_method_summary(primary, candidate, method)
      summary$qc_scenario <- scenario
      sensitivity[[paste(scenario, method, sep = "__")]] <- summary
    }
  }
  sensitivity <- do.call(rbind, sensitivity)
  sensitivity <- sensitivity[c("qc_scenario", setdiff(names(sensitivity), "qc_scenario"))]
  write_csv_portable(
    sensitivity,
    file.path(options$output_dir, "colour_method_sensitivity.csv")
  )

  primary <- run_results[["inclusive__primary"]]$data
  legacy <- run_results[["inclusive__legacy"]]$data
  common <- intersect(primary$analysis_id, legacy$analysis_id)
  primary_common <- primary[match(common, primary$analysis_id), , drop = FALSE]
  legacy_common <- legacy[match(common, legacy$analysis_id), , drop = FALSE]
  legacy_comparison <- data.frame(
    observation_id = common,
    source_row = primary_common$source_row,
    primary_L = primary_common$L,
    primary_a = primary_common$a,
    primary_b = primary_common$b,
    legacy_L = legacy_common$L,
    legacy_a = legacy_common$a,
    legacy_b = legacy_common$b,
    delta_L_legacy_minus_primary = legacy_common$L - primary_common$L,
    delta_a_legacy_minus_primary = legacy_common$a - primary_common$a,
    delta_b_legacy_minus_primary = legacy_common$b - primary_common$b,
    stringsAsFactors = FALSE
  )
  legacy_comparison$deltaE76 <- with(
    legacy_comparison,
    sqrt(
      delta_L_legacy_minus_primary^2 +
      delta_a_legacy_minus_primary^2 +
      delta_b_legacy_minus_primary^2
    )
  )
  write_csv_portable(
    legacy_comparison,
    file.path(options$output_dir, "legacy_vs_v2_record_level.csv")
  )
  legacy_crosswalk <- data.frame(
    legacy_row = seq_len(nrow(data_s1_legacy)),
    observation_id = data_s1_legacy$observation_id,
    source_row = data_s1_legacy$source_row,
    image_sha256 = data_s1_legacy$image_sha256,
    legacy_R = data_s1_legacy$legacy_R,
    legacy_G = data_s1_legacy$legacy_G,
    legacy_B = data_s1_legacy$legacy_B,
    primary_R = data_s1_legacy$R,
    primary_G = data_s1_legacy$G,
    primary_B = data_s1_legacy$B,
    included_in_inclusive_legacy_analysis =
      data_s1_legacy$observation_id %in% common,
    exclusion_reason = ifelse(
      data_s1_legacy$observation_id %in% common,
      "",
      "unresolved_duplicate_photo_multiple_coordinates"
    ),
    stringsAsFactors = FALSE
  )
  write_csv_portable(
    legacy_crosswalk,
    file.path(options$output_dir, "legacy_input_crosswalk.csv")
  )
  legacy_summary <- sensitivity[
    sensitivity$qc_scenario == "inclusive" & sensitivity$method == "legacy",
    ,
    drop = FALSE
  ]
  write_csv_portable(
    legacy_summary,
    file.path(options$output_dir, "legacy_vs_v2_summary.csv")
  )

  old_lab <- grDevices::convertColor(
    as.matrix(primary[c("R", "G", "B")]) / 255,
    from = "sRGB",
    to = "Lab"
  )
  new_lab <- as.matrix(primary[c("L", "a", "b")])
  lab_delta <- old_lab - new_lab
  lab_delta_e <- sqrt(rowSums(lab_delta^2))
  lab_implementation_comparison <- data.frame(
    conversion = "grDevices_convertColor_minus_explicit_IEC_sRGB_D65",
    records = nrow(primary),
    delta_L_mean = mean(lab_delta[, 1L]),
    delta_a_mean = mean(lab_delta[, 2L]),
    delta_b_mean = mean(lab_delta[, 3L]),
    deltaE76_median = stats::median(lab_delta_e),
    deltaE76_p95 = unname(stats::quantile(lab_delta_e, 0.95)),
    deltaE76_max = max(lab_delta_e),
    stringsAsFactors = FALSE
  )
  write_csv_portable(
    lab_implementation_comparison,
    file.path(options$output_dir, "lab_implementation_comparison.csv")
  )

  primary <- add_reanalysis_transforms(primary, config)
  public_audit <- public_validation$manifest[c(
    "source_id", "provider", "dataset_version", "unit",
    "output_value_semantics", "processed_sha256", "processed_min", "processed_max"
  )]
  public_audit$extracted_observed_n <- vapply(
    public_audit$source_id,
    function(column) sum(is.finite(primary[[column]])),
    integer(1)
  )
  public_audit$extracted_missing_n <- nrow(primary) - public_audit$extracted_observed_n
  public_audit$extracted_missing_fraction <- public_audit$extracted_missing_n / nrow(primary)
  write_csv_portable(public_audit, file.path(options$output_dir, "raster_join_audit.csv"))

  candidate_predictors <- unlist(config$candidate_environment_predictors)
  model_predictors <- unlist(config$environment_predictors)
  missingness <- summarize_predictor_missingness(
    primary,
    c(candidate_predictors, as.character(config$bombus_predictor)),
    unlist(config$outcomes)
  )
  coverage_groups <- list(
    all_candidates = candidate_predictors,
    all_environment = model_predictors,
    environment_plus_bombus = c(
      model_predictors,
      as.character(config$bombus_predictor)
    )
  )
  for (coverage_name in names(coverage_groups)) {
    observed <- finite_complete_rows(primary, coverage_groups[[coverage_name]])
    row <- data.frame(
      predictor = paste0("<", coverage_name, ">"),
      records = nrow(primary),
      observed_n = sum(observed),
      missing_n = sum(!observed),
      missing_fraction = mean(!observed),
      stringsAsFactors = FALSE
    )
    for (outcome in unlist(config$outcomes)) {
      row[[paste0(outcome, "_mean_observed")]] <- safe_mean(
        primary[[outcome]], observed
      )
      row[[paste0(outcome, "_mean_missing")]] <- safe_mean(
        primary[[outcome]], !observed
      )
    }
    missingness <- rbind(missingness, row)
  }
  write_csv_portable(missingness, file.path(options$output_dir, "missingness_audit.csv"))

  screen_predictor_set <- function(set_name, predictors) {
    complete <- finite_complete_rows(primary, predictors)
    matrix <- as.matrix(primary[complete, predictors, drop = FALSE])
    standardized <- standardize_training_matrix(matrix)$train
    correlation <- stats::cor(matrix)
    maximum_pairwise <- vapply(seq_along(predictors), function(index) {
      values <- abs(correlation[index, -index, drop = TRUE])
      if (length(values)) max(values) else NA_real_
    }, numeric(1))
    reasons <- config$excluded_candidate_predictors
    data.frame(
      predictor_set = set_name,
      predictor = predictors,
      records_complete = nrow(matrix),
      vif = unname(variance_inflation_factors(standardized)[predictors]),
      maximum_absolute_pairwise_correlation = maximum_pairwise,
      model_matrix_condition_number = kappa(cbind(1, standardized)),
      selection_status = ifelse(
        predictors %in% model_predictors,
        "retained",
        "excluded_collinearity"
      ),
      selection_reason = vapply(predictors, function(predictor) {
        if (predictor %in% model_predictors) {
          "retained as a distinct reviewed ecological or observation-process axis"
        } else {
          as.character(reasons[[predictor]])
        }
      }, character(1)),
      stringsAsFactors = FALSE
    )
  }
  predictor_screening <- rbind(
    screen_predictor_set("candidate_nine", candidate_predictors),
    screen_predictor_set("selected_six", model_predictors)
  )
  write_csv_portable(
    predictor_screening,
    file.path(options$output_dir, "predictor_screening.csv")
  )

  correlations <- stats::cor(
    primary[c(candidate_predictors, as.character(config$bombus_predictor))],
    use = "pairwise.complete.obs"
  )
  correlation_long <- as.data.frame(as.table(correlations), stringsAsFactors = FALSE)
  names(correlation_long) <- c("predictor_1", "predictor_2", "pearson_correlation")
  correlation_long <- correlation_long[
    as.character(correlation_long$predictor_1) < as.character(correlation_long$predictor_2),
    ,
    drop = FALSE
  ]
  write_csv_portable(
    correlation_long,
    file.path(options$output_dir, "predictor_correlations.csv")
  )

  photo_folds <- make_spatial_block_folds(
    primary,
    folds = config$spatial_block_cv$folds
  )
  photo_folds$grain <- "photo"
  site_data <- add_reanalysis_transforms(site_result$data, config)
  site_folds <- assign_spatial_folds(
    site_data,
    unique(photo_folds[c("longitude", "latitude", "spatial_fold")])
  )
  site_folds$grain <- "site"
  write_csv_portable(
    rbind(photo_folds, site_folds),
    file.path(options$output_dir, "spatial_folds.csv")
  )
  site_membership <- site_data[c("analysis_id", "n_photos", "record_ids")]
  write_csv_portable(site_membership, file.path(options$output_dir, "site_membership.csv"))

  model_coefficients <- list()
  model_performance <- list()
  model_performance_by_fold <- list()
  cv_predictions <- list()
  model_runs <- names(run_results)
  for (run_name in model_runs) {
    data <- add_reanalysis_transforms(run_results[[run_name]]$data, config)
    is_site <- identical(run_name, "inclusive__primary__site")
    fold_table <- if (is_site) site_folds else photo_folds
    fold_index <- match(data$analysis_id, fold_table$analysis_id)
    if (anyNA(fold_index)) stop("Spatial fold join lost analysis IDs.", call. = FALSE)
    data$spatial_fold <- fold_table$spatial_fold[fold_index]
    parts <- strsplit(run_name, "__", fixed = TRUE)[[1L]]
    method <- parts[[2L]]
    outcomes <- if (method == "primary") {
      unlist(config$outcomes)
    } else {
      as.character(config$primary_outcome)
    }
    cohort_data <- list(method_specific = data)
    if (!is_site) {
      common_ids <- common_method_ids[[parts[[1L]]]]
      cohort_data$all_methods_common <- data[
        data$analysis_id %in% common_ids,
        ,
        drop = FALSE
      ]
    }
    all_variants <- list(
      environment_only_full = list(
        predictors = model_predictors,
        require_bombus_complete = FALSE
      ),
      environment_only_bombus_common = list(
        predictors = model_predictors,
        require_bombus_complete = TRUE
      ),
      environment_plus_bombus = list(
        predictors = c(
          model_predictors,
          as.character(config$bombus_predictor)
        ),
        require_bombus_complete = TRUE
      )
    )
    enabled_variants <- names(Filter(isTRUE, config$model_variants))
    variants <- all_variants[enabled_variants]
    for (cohort_basis in names(cohort_data)) {
      analysis_subset <- cohort_data[[cohort_basis]]
      for (outcome in outcomes) {
        for (variant in names(variants)) {
          variant_spec <- variants[[variant]]
          predictors <- variant_spec$predictors
          model_data <- analysis_subset
          if (isTRUE(variant_spec$require_bombus_complete)) {
            model_data <- model_data[
              is.finite(model_data[[as.character(config$bombus_predictor)]]),
              ,
              drop = FALSE
            ]
          }
          key <- paste(
            run_name, cohort_basis, outcome, variant,
            sep = "__"
          )
          fit <- fit_standardized_association(model_data, outcome, predictors)
          cv <- blocked_cv_association(
            model_data,
            outcome,
            predictors,
            configured_folds = seq_len(
              as.integer(config$spatial_block_cv$folds)
            )
          )
          if (!identical(
            as.integer(fit$performance$records_complete),
            as.integer(cv$performance$records_complete)
          )) {
            stop("Model and CV complete-case cohorts differ.", call. = FALSE)
          }
          common_meta <- list(
            run = run_name,
            qc_scenario = parts[[1L]],
            colour_method = method,
            grain = if (is_site) "site" else "photo",
            cohort_basis = cohort_basis,
            outcome = outcome,
            model_variant = variant
          )
          model_coefficients[[key]] <- prefix_columns(
            fit$coefficients,
            common_meta
          )
          cv_performance <- cv$performance
          names(cv_performance)[names(cv_performance) == "records_complete"] <-
            "records_complete_cv"
          performance <- cbind(fit$performance, cv_performance)
          model_performance[[key]] <- prefix_columns(performance, common_meta)
          model_performance_by_fold[[key]] <- prefix_columns(
            cv$fold_performance,
            common_meta
          )
          cv_predictions[[key]] <- prefix_columns(cv$predictions, common_meta)
        }
      }
    }
  }
  model_coefficients <- do.call(rbind, model_coefficients)
  model_performance <- do.call(rbind, model_performance)
  model_performance_by_fold <- do.call(rbind, model_performance_by_fold)
  cv_predictions <- do.call(rbind, cv_predictions)
  rownames(model_coefficients) <- NULL
  rownames(model_performance) <- NULL
  rownames(model_performance_by_fold) <- NULL
  rownames(cv_predictions) <- NULL
  write_csv_portable(
    model_coefficients,
    file.path(options$output_dir, "model_coefficients.csv")
  )
  write_csv_portable(
    model_performance,
    file.path(options$output_dir, "model_performance.csv")
  )
  write_csv_portable(
    model_performance_by_fold,
    file.path(options$output_dir, "model_performance_by_fold.csv")
  )
  write_csv_portable(
    cv_predictions,
    file.path(options$output_dir, "model_predictions.csv")
  )

  session_lines <- capture.output(sessionInfo())
  write_lines_portable(session_lines, file.path(options$output_dir, "sessionInfo.txt"))

  primary_performance <- model_performance[
    model_performance$run == "inclusive__primary" &
      model_performance$outcome == as.character(config$primary_outcome) &
      model_performance$cohort_basis == "method_specific",
    ,
    drop = FALSE
  ]
  performance_value <- function(variant, column = "q_squared_cv") {
    value <- primary_performance[
      primary_performance$model_variant == variant,
      column,
      drop = TRUE
    ]
    if (length(value) != 1L) {
      stop("Expected exactly one primary performance value for ", variant, call. = FALSE)
    }
    as.numeric(value)
  }
  bombus_missing <- missingness[
    missingness$predictor == as.character(config$bombus_predictor),
    ,
    drop = FALSE
  ]
  report_input <- data_s1_all
  extraction_versions <- if ("extraction_version" %in% names(report_input)) {
    paste(sort(unique(report_input$extraction_version)), collapse = ", ")
  } else {
    "not recorded"
  }
  manual_review_count <- if ("qc_status" %in% names(report_input)) {
    sum(report_input$qc_status == "manual_review_required", na.rm = TRUE)
  } else {
    NA_integer_
  }
  identity_conflicts <- nrow(
    run_results[["inclusive__primary"]]$excluded_photo_coordinate_conflicts
  )
  unique_fold_sizes <- table(unique(
    photo_folds[c("longitude", "latitude", "spatial_fold")]
  )$spatial_fold)
  report <- c(
    "# Reviewed public-data reanalysis",
    "",
    paste0("Analysis version: `", config$analysis_version, "`."),
    "",
    "## Scope",
    "",
    paste0(
      "The analysis starts from Data_S1 extraction version ", extraction_versions,
      ", hard-excludes ", identity_conflicts, " unresolved ",
      "duplicate-photo/different-coordinate record(s), verifies the complete 30 arc-second ",
      "public raster manifest, and attaches every raster by `analysis_id`."
    ),
    "",
    "The primary response is display-referred CIELAB a* (red-green). L* and b* are illumination/exposure diagnostics. The global-reference colour PC1 is retained only for descriptive method comparison and is not cross-validated, because fitting it on all records would leak test-fold outcome information. No result is a calibrated pigment, reflectance, pollinator-vision, adaptation, or causal estimate.",
    "",
    "## Row flow",
    "",
    paste0("- Primary inclusive photograph records: ", nrow(primary), "."),
    paste0("- Primary strict-OK photograph records: ", nrow(run_results[["strict_ok__primary"]]$data), "."),
    paste0("- Exact-site sensitivity records: ", nrow(site_data), "."),
    paste0("- Public predictor layers validated: ", nrow(public_validation$manifest), "."),
    paste0(
      "- Candidate predictor maximum VIF: ",
      format(max(predictor_screening$vif[
        predictor_screening$predictor_set == "candidate_nine"
      ]), digits = 4),
      "; retained six-predictor maximum VIF: ",
      format(max(predictor_screening$vif[
        predictor_screening$predictor_set == "selected_six"
      ]), digits = 4), "."
    ),
    paste0("- Bombus-complete primary records: ", bombus_missing$observed_n, "; missing: ", bombus_missing$missing_n, "."),
    paste0("- All-colour-method common inclusive records: ", length(common_method_ids$inclusive), "."),
    paste0("- Unique-coordinate fold sizes: ", paste(unique_fold_sizes, collapse = ", "), "."),
    "",
    "## Descriptive model status",
    "",
    paste0(
      "Full-cohort environment-only blocked Q2 for primary ",
      config$primary_outcome, ": ",
      format(performance_value("environment_only_full"), digits = 4),
      ". On the identical Bombus-complete cohort, environment-only Q2 is ",
      format(performance_value("environment_only_bombus_common"), digits = 4),
      " and environment-plus-Bombus Q2 is ",
      format(performance_value("environment_plus_bombus"), digits = 4),
      "."
    ),
    "",
    paste0(
      "These are descriptive standardized linear models. Naive standard errors are included only as diagnostics and do not correct spatial dependence. ",
      config$spatial_block_cv$folds,
      " equal-site, principal-geographic-axis bands assess prediction sensitivity; no buffer is imposed and the result is not causal. Pooled Q2 uses each fold's training mean as its baseline, and fold-level/macro metrics are retained."
    ),
    "",
    "## Required caveats",
    "",
    "- White balance is not identifiable from the post-processed cut-outs and was not applied.",
    paste0("- Manual adjudication of ", manual_review_count, " automated review flags is incomplete; inclusive and strict-OK results are both retained."),
    "- Workbook coordinates were remapped to images but not independently recomputed from GPX.",
    "- The five Bombus rasters are structurally valid but their model-generation provenance is legacy_unverifiable; their sum is suitability, not richness.",
    "- Bombus suitability reuses environmental information and is therefore shown only as an additional sensitivity predictor.",
    "- WorldPop 2020 is treated mainly as a human access/sampling-intensity proxy, not a causal ecological driver.",
    "- CHELSA 1981–2010 climatology and WorldPop 2020 do not match individual photograph dates; season, camera, illumination, and non-random citizen-photo sampling remain uncontrolled.",
    "- SoilGrids uses a mutable `latest` endpoint; this run is pinned by the retrieved-file hashes in the raster manifest."
  )
  write_lines_portable(report, file.path(options$output_dir, "report.md"))
  final_snapshot_hashes <- setNames(
    vapply(snapshot_paths, sha256_file, character(1)),
    snapshot_paths
  )
  changed_snapshot <- names(initial_hashes)[
    tolower(initial_hashes) != tolower(final_snapshot_hashes[names(initial_hashes)])
  ]
  if (length(changed_snapshot)) {
    stop(
      "Reanalysis inputs or code changed during execution: ",
      paste(vapply(changed_snapshot, repo_relative_path, character(1), root = repo_root),
            collapse = ", "),
      call. = FALSE
    )
  }
  cat("Analysis tables complete; input snapshot verified; manifest generation pending.\n")
  close_run_log()
  completed_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  reproducible_command <- paste(
    "Rscript scripts/run_reanalysis.R",
    "--input", shQuote(repo_relative_path(options$input, repo_root)),
    "--legacy-input", shQuote(repo_relative_path(options$legacy_input, repo_root)),
    "--config", shQuote(repo_relative_path(options$config, repo_root)),
    "--sdm-dir", shQuote(repo_relative_path(options$sdm_dir, repo_root)),
    "--public-raster-manifest",
    shQuote(repo_relative_path(options$public_raster_manifest, repo_root)),
    "--raster-registry",
    shQuote(repo_relative_path(options$raster_registry, repo_root)),
    "--pipeline", shQuote(repo_relative_path(options$pipeline, repo_root)),
    "--output-dir", shQuote(repo_relative_path(options$output_dir, repo_root))
  )
  run_manifest <- list(
    analysis_version = config$analysis_version,
    execution = list(
      started_at_utc = started_at_utc,
      completed_at_utc = completed_at_utc,
      command = reproducible_command,
      r_version = R.version.string,
      platform = R.version$platform
    ),
    git = list(head = git_head, dirty_before_output = git_dirty),
    inputs = list(
      data_s1 = list(
        path = repo_relative_path(options$input, repo_root),
        sha256 = snapshot_hash(options$input),
        contract = as.list(data_contract_audit[1L, , drop = FALSE])
      ),
      legacy_input = list(
        path = repo_relative_path(options$legacy_input, repo_root),
        sha256 = snapshot_hash(options$legacy_input),
        rows_crosswalked = nrow(legacy_crosswalk),
        maximum_absolute_rounding_difference = max(abs(legacy_differences))
      ),
      reanalysis_config = list(
        path = repo_relative_path(options$config, repo_root),
        sha256 = snapshot_hash(options$config)
      ),
      raster_registry = list(
        path = repo_relative_path(options$raster_registry, repo_root),
        sha256 = snapshot_hash(options$raster_registry)
      ),
      raster_pipeline = list(
        path = repo_relative_path(options$pipeline, repo_root),
        sha256 = snapshot_hash(options$pipeline)
      ),
      public_raster_manifest = list(
        path = repo_relative_path(options$public_raster_manifest, repo_root),
        sha256 = snapshot_hash(options$public_raster_manifest),
        layers = nrow(public_validation$manifest)
      ),
      sdm_manifest = list(
        path = repo_relative_path(file.path(options$sdm_dir, "manifest.csv"), repo_root),
        sha256 = snapshot_hash(file.path(options$sdm_dir, "manifest.csv")),
        structural_ok = sdm_validation$structural_ok,
        model_reproducible = sdm_validation$model_reproducible
      ),
      code = lapply(code_files, function(path) {
        list(
          path = repo_relative_path(path, repo_root),
          sha256 = snapshot_hash(path)
        )
      }),
      dependencies = list(
        path = repo_relative_path(dependency_files, repo_root),
        sha256 = snapshot_hash(dependency_files)
      )
    ),
    method = list(
      primary_outcome = config$primary_outcome,
      secondary_composite = config$secondary_composite,
      secondary_composite_use = "descriptive_method_comparison_only_not_cross_validated",
      colour_methods = unlist(config$colour_methods),
      qc_scenarios = config$qc_scenarios,
      candidate_environment_predictors = candidate_predictors,
      environment_predictors = model_predictors,
      excluded_candidate_predictors = config$excluded_candidate_predictors,
      bombus_predictor = config$bombus_predictor,
      model_variants = config$model_variants,
      model_cohorts = c("method_specific", "all_methods_common"),
      spatial_block_cv = config$spatial_block_cv,
      public_grid = "EPSG:4326, 30 arc-seconds",
      white_balance_applied = FALSE,
      coordinates_independently_recomputed = FALSE,
      manual_qc_complete = FALSE,
      heavy_models_run = FALSE
    ),
    outputs = list(
      manifest = repo_relative_path(
        file.path(options$output_dir, "output_manifest.csv"), repo_root
      ),
      reference_pca = repo_relative_path(
        file.path(options$output_dir, "reference_colour_pca.csv"), repo_root
      ),
      row_flow = repo_relative_path(
        file.path(options$output_dir, "row_flow.csv"), repo_root
      )
    )
  )
  write_lines_portable(
    yaml::as.yaml(run_manifest),
    file.path(options$output_dir, "run_manifest.yml")
  )

  artifact_paths <- list.files(
    options$output_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  )
  artifact_paths <- artifact_paths[file.info(artifact_paths)$isdir %in% FALSE]
  artifact_paths <- setdiff(
    artifact_paths,
    file.path(options$output_dir, "output_manifest.csv")
  )
  artifact_manifest <- do.call(rbind, lapply(artifact_paths, function(path) {
    extension <- tolower(tools::file_ext(path))
    relative_output_path <- substring(
      normalizePath(path, winslash = "/", mustWork = TRUE),
      nchar(normalizePath(options$output_dir, winslash = "/", mustWork = TRUE)) + 2L
    )
    distribution_status <- if (
      grepl("^inputs/", relative_output_path) ||
      basename(path) %in% c("model_predictions.csv", "run.log")
    ) {
      "local_ignored"
    } else {
      "tracked_summary"
    }
    dimensions <- if (extension == "csv") {
      value <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
      if (anyDuplicated(names(value))) {
        stop("Artifact has duplicate CSV headers: ", path, call. = FALSE)
      }
      c(nrow(value), ncol(value))
    } else c(NA_integer_, NA_integer_)
    data.frame(
      artifact = repo_relative_path(path, repo_root),
      sha256 = sha256_file(path),
      bytes = unname(file.info(path)$size),
      rows = dimensions[[1L]],
      columns = dimensions[[2L]],
      distribution_status = distribution_status,
      stringsAsFactors = FALSE
    )
  }))
  write_csv_portable(
    artifact_manifest,
    file.path(options$output_dir, "output_manifest.csv")
  )

  message("Reanalysis complete: ", options$output_dir)
  invisible(list(
    row_flow = row_flow,
    sensitivity = sensitivity,
    missingness = missingness,
    model_performance = model_performance,
    public_validation = public_validation,
    sdm_validation = sdm_validation
  ))
}

if (sys.nframe() == 0L) {
  options <- parse_reanalysis_args(commandArgs(trailingOnly = TRUE))
  run_reanalysis(options)
}
