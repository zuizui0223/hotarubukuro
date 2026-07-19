#!/usr/bin/env Rscript

# Lightweight, reproducible entry point.  This script prepares validated
# photograph-level colour and Bombus SDM data.  It intentionally does not fit
# the previously used INLA/qGAM models: those models require a separate,
# reviewed specification for projection, priors, joint uncertainty, and
# spatially blocked validation.

current_script_path <- function() {
  frame_files <- vapply(
    sys.frames(),
    function(frame) {
      value <- frame$ofile
      if (is.null(value)) "" else as.character(value)[1L]
    },
    character(1)
  )
  frame_files <- frame_files[nzchar(frame_files)]
  matching_script <- frame_files[basename(frame_files) == "run_analysis.R"]
  if (length(matching_script)) {
    return(normalizePath(
      matching_script[length(matching_script)],
      winslash = "/",
      mustWork = TRUE
    ))
  }
  if (length(frame_files)) {
    return(normalizePath(frame_files[length(frame_files)], winslash = "/", mustWork = TRUE))
  }

  file_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_argument)) {
    return(normalizePath(sub("^--file=", "", file_argument[1L]), winslash = "/", mustWork = TRUE))
  }
  NA_character_
}

analysis_script <- current_script_path()
project_root_default <- if (!is.na(analysis_script)) {
  candidate <- dirname(dirname(analysis_script))
  if (file.exists(file.path(candidate, "R", "analysis_core.R"))) candidate else getwd()
} else {
  getwd()
}

source(file.path(project_root_default, "R", "analysis_core.R"), local = TRUE)
source(file.path(project_root_default, "R", "sdm.R"), local = TRUE)

analysis_usage <- function() {
  paste(
    "Usage:",
    "  Rscript scripts/run_analysis.R [options]",
    "",
    "Options:",
    "  --input PATH              Standard colour CSV (default: Data_S1.csv)",
    "  --sdm-dir PATH            Directory containing the five SDM TIFFs (default: sdm)",
    "  --no-sdm                  Skip SDM extraction",
    "  --output PATH             Prepared analysis CSV (default: results/analysis_input_photo.csv)",
    "  --loadings-output PATH    Colour PCA loadings CSV (default: results/colour_pca_loadings.csv)",
    "  --accepted-qc-status TEXT Comma-separated statuses (default: ok,manual_review_required)",
    "  --colour-method METHOD    primary (default), hsv_peak, hsv_exposure_filtered_peak, or alpha_peak",
    "  --grain photo|site        Analysis grain (default: photo)",
    "  --site-id COLUMN          Required explicit site ID when --grain site",
    "  --run-heavy               Refuse with an explanation; heavy models are not approved",
    "  --help                    Show this help",
    sep = "\n"
  )
}

parse_analysis_args <- function(args, project_root) {
  defaults <- list(
    input = file.path(project_root, "Data_S1.csv"),
    sdm_dir = file.path(project_root, "sdm"),
    use_sdm = TRUE,
    output = file.path(project_root, "results", "analysis_input_photo.csv"),
    loadings_output = file.path(project_root, "results", "colour_pca_loadings.csv"),
    accepted_qc_status = "ok,manual_review_required",
    colour_method = "primary",
    grain = "photo",
    site_id = NULL,
    run_heavy = FALSE,
    help = FALSE
  )

  value_options <- c(
    "--input" = "input",
    "--sdm-dir" = "sdm_dir",
    "--output" = "output",
    "--loadings-output" = "loadings_output",
    "--accepted-qc-status" = "accepted_qc_status",
    "--colour-method" = "colour_method",
    "--color-method" = "colour_method",
    "--grain" = "grain",
    "--site-id" = "site_id"
  )
  index <- 1L
  while (index <= length(args)) {
    argument <- args[index]
    if (argument %in% names(value_options)) {
      if (index == length(args)) {
        stop("Missing value after ", argument, ".", call. = FALSE)
      }
      defaults[[value_options[[argument]]]] <- args[index + 1L]
      index <- index + 2L
    } else if (argument == "--no-sdm") {
      defaults$use_sdm <- FALSE
      index <- index + 1L
    } else if (argument == "--run-heavy") {
      defaults$run_heavy <- TRUE
      index <- index + 1L
    } else if (argument %in% c("--help", "-h")) {
      defaults$help <- TRUE
      index <- index + 1L
    } else {
      stop("Unknown argument: ", argument, call. = FALSE)
    }
  }

  if (!(defaults$grain %in% c("photo", "site"))) {
    stop("--grain must be 'photo' or 'site'.", call. = FALSE)
  }
  defaults
}

write_csv_portable <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = paste0(".", basename(path), "."),
    tmpdir = dirname(path),
    fileext = ".part"
  )
  on.exit(unlink(temporary), add = TRUE)
  utils::write.csv(data, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    stop("Could not atomically write output: ", path, call. = FALSE)
  }
  invisible(path)
}

canonical_cli_path <- function(path) {
  path <- path.expand(as.character(path)[1L])
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", path)) {
    path <- file.path(getwd(), path)
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

validate_analysis_output_paths <- function(input, output, loadings_output) {
  paths <- vapply(
    c(input = input, output = output, loadings_output = loadings_output),
    canonical_cli_path,
    character(1)
  )
  if (anyDuplicated(unname(paths))) {
    duplicated_paths <- unique(paths[duplicated(paths) | duplicated(paths, fromLast = TRUE)])
    stop(
      "Input and output paths must be distinct: ",
      paste(duplicated_paths, collapse = ", "),
      call. = FALSE
    )
  }
  existing_outputs <- paths[c("output", "loadings_output")]
  existing_outputs <- existing_outputs[file.exists(existing_outputs)]
  if (length(existing_outputs)) {
    stop(
      "Refusing to overwrite existing output: ",
      paste(existing_outputs, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(paths)
}

run_analysis_cli <- function(args = commandArgs(trailingOnly = TRUE),
                             project_root = project_root_default) {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  options <- parse_analysis_args(args, project_root)
  if (options$help) {
    cat(analysis_usage(), "\n")
    return(invisible(NULL))
  }
  if (options$run_heavy) {
    stop(
      paste(
        "Heavy INLA/qGAM execution is intentionally disabled.",
        "A future joint model must first define a Japan-appropriate metric CRS,",
        "PC priors, full predictor standardization, spatially blocked validation,",
        "and uncertainty-aware variance decomposition."
      ),
      call. = FALSE
    )
  }
  validate_analysis_output_paths(
    options$input,
    options$output,
    options$loadings_output
  )

  data <- read_colour_data(options$input, allow_missing_rgb = TRUE)
  input_grain_audit <- audit_analysis_grain(data)
  accepted_qc_status <- trimws(strsplit(
    options$accepted_qc_status,
    ",",
    fixed = TRUE
  )[[1L]])
  qc_filter <- filter_analysis_qc(data, accepted_status = accepted_qc_status)
  data <- qc_filter$data
  if (qc_filter$status_available) {
    message(
      "Colour QC retained ", nrow(data), " record(s) and excluded ",
      nrow(qc_filter$excluded), "; accepted status: ",
      paste(qc_filter$accepted_status, collapse = ", "), "."
    )
  } else {
    message(
      "Column 'qc_status' is unavailable in this legacy input; all ",
      nrow(data), " record(s) were retained."
    )
  }
  if (!nrow(data)) {
    stop("No records remain after colour QC filtering.", call. = FALSE)
  }
  identity_filter <- filter_photo_coordinate_conflicts(data)
  data <- identity_filter$data
  if (nrow(identity_filter$excluded)) {
    message(
      "Excluded ", nrow(identity_filter$excluded),
      " unresolved duplicate-photo/multiple-coordinate record(s)."
    )
  }
  if (!nrow(data)) {
    stop("No records remain after photo-coordinate conflict filtering.", call. = FALSE)
  }
  colour_selection <- select_analysis_colour_method(
    data,
    method = options$colour_method
  )
  data <- colour_selection$data
  message(
    "Colour method '", colour_selection$method, "' retained ", nrow(data),
    " record(s) and rejected ", nrow(colour_selection$excluded),
    " record(s) with missing or non-finite selected RGB values."
  )
  if (!nrow(data)) {
    stop("No records remain after colour-method selection.", call. = FALSE)
  }
  grain_audit <- audit_analysis_grain(data)
  data <- add_colour_features(data)

  site_values <- intersect(
    c(
      "R", "G", "B", "L", "a", "b", "C", "darkness",
      "primary_R", "primary_G", "primary_B"
    ),
    names(data)
  )
  analysis_data <- prepare_analysis_grain(
    data,
    grain = options$grain,
    site_id_col = options$site_id,
    value_cols = if (options$grain == "site") site_values else character()
  )

  # Fit PCA only after the requested analysis grain has been established.
  # Repeated photographs therefore cannot silently reweight a site-grain PCA.
  colour_pca <- fit_oriented_pca(
    analysis_data,
    variables = c("L", "a", "b"),
    anchor = "a",
    anchor_direction = 1
  )
  analysis_data$colour_PC1 <- colour_pca$scores

  sdm_validation <- NULL
  if (options$use_sdm) {
    sdm_validation <- validate_sdm_collection(options$sdm_dir)
    if (!isTRUE(sdm_validation$structural_ok)) {
      stop(
        "SDM structural validation failed: ",
        paste(sdm_validation$issues, collapse = "; "),
        call. = FALSE
      )
    }
    if (!isTRUE(sdm_validation$model_reproducible)) {
      message(
        "SDM rasters passed structural validation, but their model-generation ",
        "provenance is legacy_unverifiable."
      )
    }
    extracted <- extract_sdm_by_record_id(
      analysis_data,
      options$sdm_dir,
      id_col = "analysis_id"
    )
    analysis_data <- attach_raster_values(
      analysis_data,
      extracted,
      id_col = "analysis_id"
    )
    analysis_data <- add_bombus_suitability_sum(
      analysis_data,
      require_all = TRUE
    )
  }

  analysis_data$analysis_colour_method <- colour_selection$method
  analysis_data$analysis_colour_source_columns <- paste(
    colour_selection$source_columns,
    collapse = ";"
  )

  loadings <- data.frame(
    variable = names(colour_pca$loadings),
    PC1_loading = unname(colour_pca$loadings),
    center = unname(colour_pca$model$center[names(colour_pca$loadings)]),
    scale = unname(colour_pca$model$scale[names(colour_pca$loadings)]),
    explained_variance_PC1 = colour_pca$explained_variance,
    anchor = colour_pca$anchor,
    stringsAsFactors = FALSE
  )

  write_csv_portable(analysis_data, options$output)
  write_csv_portable(loadings, options$loadings_output)

  cat("Validated input:", normalizePath(options$input, winslash = "/"), "\n")
  cat("Input grain audit (before QC):\n")
  print(input_grain_audit, row.names = FALSE)
  cat("Colour QC audit:\n")
  print(qc_filter$audit, row.names = FALSE)
  cat("Photo-coordinate identity audit:\n")
  print(identity_filter$audit, row.names = FALSE)
  cat("Colour-method audit:\n")
  print(colour_selection$audit, row.names = FALSE)
  cat("Analysis grain audit (after QC):\n")
  print(grain_audit, row.names = FALSE)
  cat("Analysis grain:", options$grain, "| rows:", nrow(analysis_data), "\n")
  if ("Bombus_suitability_sum" %in% names(analysis_data)) {
    cat(
      "Bombus suitability complete:",
      sum(is.finite(analysis_data$Bombus_suitability_sum)),
      "| missing:", sum(!is.finite(analysis_data$Bombus_suitability_sum)), "\n"
    )
  }
  cat("Prepared data:", normalizePath(options$output, winslash = "/", mustWork = TRUE), "\n")
  cat("PCA loadings:", normalizePath(options$loadings_output, winslash = "/", mustWork = TRUE), "\n")
  message(
    "INLA and qGAM were not run; this command prepares only reviewed deterministic inputs."
  )
  invisible(list(
    data = analysis_data,
    colour_pca = colour_pca,
    input_grain_audit = input_grain_audit,
    grain_audit = grain_audit,
    qc_audit = qc_filter$audit,
    excluded_qc = qc_filter$excluded,
    photo_coordinate_audit = identity_filter$audit,
    excluded_photo_coordinate_conflicts = identity_filter$excluded,
    colour_method_audit = colour_selection$audit,
    excluded_colour_method = colour_selection$excluded,
    sdm_validation = sdm_validation
  ))
}

if (sys.nframe() == 0L) {
  run_analysis_cli()
}
