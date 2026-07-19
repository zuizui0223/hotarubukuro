# Core, lightweight analysis helpers for the hotarubukuro data pipeline.
#
# These functions deliberately avoid fitting INLA or qGAM models.  They define
# the data contract and the deterministic transformations that must be tested
# before any expensive model is considered.

required_colour_columns <- function() {
  c("date", "latitude", "longitude", "R", "G", "B")
}

bombus_species_columns <- function() {
  c("ardens", "beaticola", "consobrinus", "diversus", "honshuensis")
}

locate_project_root <- function(start = getwd(), marker = "Data_S1.csv") {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  if (!dir.exists(path)) {
    path <- dirname(path)
  }

  repeat {
    if (file.exists(file.path(path, marker)) &&
        file.exists(file.path(path, "R", "analysis_core.R"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not locate the project root from: ", start, call. = FALSE)
    }
    path <- parent
  }
}

strip_utf8_bom <- function(x) {
  sub("^\ufeff", "", x, perl = TRUE)
}

coerce_numeric_strict <- function(x, column) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  raw <- trimws(as.character(x))
  missing <- is.na(raw) | raw == ""
  value <- suppressWarnings(as.numeric(raw))
  bad <- !missing & is.na(value)
  if (any(bad)) {
    examples <- unique(raw[bad])
    stop(
      sprintf(
        "Column '%s' contains non-numeric values (for example: %s).",
        column,
        paste(utils::head(examples, 3L), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  value
}

parse_collection_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  raw <- trimws(as.character(x))
  raw[is.na(x) | raw == ""] <- NA_character_
  parsed <- as.Date(rep(NA_character_, length(raw)))
  formats <- c(
    "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d",
    "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
    "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M"
  )

  for (format in formats) {
    take <- is.na(parsed) & !is.na(raw)
    if (!any(take)) {
      break
    }
    candidate <- suppressWarnings(as.Date(raw[take], format = format))
    parsed[take] <- candidate
  }
  parsed
}

validate_colour_data <- function(data,
                                 allow_missing_rgb = TRUE,
                                 allow_missing_coordinates = FALSE,
                                 allow_missing_date = FALSE) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }

  missing_columns <- setdiff(required_colour_columns(), names(data))
  if (length(missing_columns)) {
    stop(
      "Missing required columns: ", paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(names(data))) {
    stop("Column names must be unique.", call. = FALSE)
  }

  for (column in c("latitude", "longitude", "R", "G", "B")) {
    if (!is.numeric(data[[column]])) {
      stop("Column '", column, "' must be numeric.", call. = FALSE)
    }
  }

  if (!inherits(data$date, "Date")) {
    stop("Column 'date' must be parsed as Date.", call. = FALSE)
  }
  if (!allow_missing_date && anyNA(data$date)) {
    stop("Column 'date' contains missing or unparseable values.", call. = FALSE)
  }

  longitude_ok <- is.finite(data$longitude)
  latitude_ok <- is.finite(data$latitude)
  coordinate_ok <- longitude_ok & latitude_ok
  if (!allow_missing_coordinates && any(!coordinate_ok)) {
    stop("Coordinates contain missing or non-finite values.", call. = FALSE)
  }
  if (any(longitude_ok & (data$longitude < -180 | data$longitude > 180))) {
    stop("Longitude must be in [-180, 180].", call. = FALSE)
  }
  if (any(latitude_ok & (data$latitude < -90 | data$latitude > 90))) {
    stop("Latitude must be in [-90, 90].", call. = FALSE)
  }

  rgb <- as.matrix(data[c("R", "G", "B")])
  if (any(!is.na(rgb) & !is.finite(rgb))) {
    stop("RGB values must be finite or NA.", call. = FALSE)
  }
  observed_channels <- rowSums(!is.na(rgb))
  partially_missing <- observed_channels > 0L & observed_channels < 3L
  if (any(partially_missing)) {
    stop("RGB channels must be either all observed or all missing per record.", call. = FALSE)
  }
  if (!allow_missing_rgb && any(observed_channels == 0L)) {
    stop("RGB values contain missing records.", call. = FALSE)
  }
  observed <- !is.na(rgb)
  if (any(rgb[observed] < 0 | rgb[observed] > 255)) {
    stop("RGB values must use the documented 0-255 sRGB scale.", call. = FALSE)
  }

  if ("record_id" %in% names(data)) {
    ids <- as.character(data$record_id)
    if (anyNA(ids) || any(trimws(ids) == "") || anyDuplicated(ids)) {
      stop("record_id must be non-missing and unique.", call. = FALSE)
    }
  }
  if ("source_row" %in% names(data)) {
    if (anyNA(data$source_row) || anyDuplicated(data$source_row)) {
      stop("source_row must be non-missing and unique.", call. = FALSE)
    }
  }

  invisible(data)
}

read_colour_data <- function(path = "Data_S1.csv",
                             allow_missing_rgb = TRUE,
                             allow_missing_coordinates = FALSE) {
  if (!file.exists(path)) {
    stop("Input CSV does not exist: ", path, call. = FALSE)
  }

  # Use one parser in every environment. Optional-package dispatch previously
  # made column classes and downstream artifact bytes depend on whether readr
  # happened to be installed on the host.
  data <- suppressWarnings(utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM"
  ))

  names(data) <- strip_utf8_bom(names(data))
  if (anyDuplicated(names(data))) {
    stop("Input CSV column names must be unique.", call. = FALSE)
  }
  missing_columns <- setdiff(required_colour_columns(), names(data))
  if (length(missing_columns)) {
    stop(
      "Missing required columns: ", paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  for (column in c("latitude", "longitude", "R", "G", "B")) {
    data[[column]] <- coerce_numeric_strict(data[[column]], column)
  }
  data$date <- parse_collection_date(data$date)

  if (!("source_row" %in% names(data))) {
    data$source_row <- seq_len(nrow(data))
  }
  if (!("record_id" %in% names(data))) {
    if ("observation_id" %in% names(data)) {
      data$record_id <- as.character(data$observation_id)
    } else {
      data$record_id <- sprintf("row-%06d", data$source_row)
    }
  } else {
    data$record_id <- as.character(data$record_id)
  }

  first_columns <- c("record_id", "source_row", required_colour_columns())
  data <- data[c(first_columns, setdiff(names(data), first_columns))]
  rownames(data) <- NULL

  validate_colour_data(
    data,
    allow_missing_rgb = allow_missing_rgb,
    allow_missing_coordinates = allow_missing_coordinates
  )
  data
}

filter_analysis_qc <- function(data,
                               accepted_status = "ok",
                               status_col = "qc_status") {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }
  accepted_status <- unique(tolower(trimws(as.character(accepted_status))))
  if (!length(accepted_status) || anyNA(accepted_status) ||
      any(!nzchar(accepted_status))) {
    stop("accepted_status must contain one or more non-empty values.", call. = FALSE)
  }

  if (!(status_col %in% names(data))) {
    audit <- data.frame(
      qc_status = "<not_available>",
      records = nrow(data),
      analysis_included = TRUE,
      stringsAsFactors = FALSE
    )
    return(structure(
      list(
        data = data,
        excluded = data[FALSE, , drop = FALSE],
        audit = audit,
        status_available = FALSE,
        accepted_status = accepted_status
      ),
      class = "analysis_qc_filter"
    ))
  }

  raw_status <- as.character(data[[status_col]])
  normalised_status <- tolower(trimws(raw_status))
  normalised_status[is.na(raw_status) | !nzchar(normalised_status)] <- NA_character_
  included <- !is.na(normalised_status) & normalised_status %in% accepted_status
  displayed_status <- ifelse(is.na(normalised_status), "<missing>", normalised_status)
  status_levels <- unique(displayed_status)
  audit <- do.call(rbind, lapply(status_levels, function(status) {
    index <- displayed_status == status
    data.frame(
      qc_status = status,
      records = sum(index),
      analysis_included = all(included[index]),
      stringsAsFactors = FALSE
    )
  }))
  rownames(audit) <- NULL

  structure(
    list(
      data = data[included, , drop = FALSE],
      excluded = data[!included, , drop = FALSE],
      audit = audit,
      status_available = TRUE,
      accepted_status = accepted_status
    ),
    class = "analysis_qc_filter"
  )
}

filter_photo_coordinate_conflicts <- function(
    data,
    status_col = "photo_coordinate_qc_status",
    conflict_status = "manual_review_required_duplicate_photo_at_multiple_coordinates") {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }
  if (!(status_col %in% names(data))) {
    return(structure(
      list(
        data = data,
        excluded = data[FALSE, , drop = FALSE],
        audit = data.frame(
          photo_coordinate_qc_status = "<not_available>",
          records = nrow(data),
          analysis_included = TRUE,
          stringsAsFactors = FALSE
        ),
        status_available = FALSE
      ),
      class = "photo_coordinate_conflict_filter"
    ))
  }

  raw <- as.character(data[[status_col]])
  status <- tolower(trimws(raw))
  status[is.na(raw) | !nzchar(status)] <- "<missing>"
  conflict <- status == tolower(conflict_status)
  levels <- unique(status)
  audit <- do.call(rbind, lapply(levels, function(level) {
    index <- status == level
    data.frame(
      photo_coordinate_qc_status = level,
      records = sum(index),
      analysis_included = !any(conflict[index]),
      stringsAsFactors = FALSE
    )
  }))
  rownames(audit) <- NULL

  structure(
    list(
      data = data[!conflict, , drop = FALSE],
      excluded = data[conflict, , drop = FALSE],
      audit = audit,
      status_available = TRUE
    ),
    class = "photo_coordinate_conflict_filter"
  )
}

analysis_colour_methods <- function() {
  c(
    "primary", "mean", "legacy", "hsv_peak",
    "hsv_exposure_filtered_peak", "alpha_peak"
  )
}

select_analysis_colour_method <- function(data, method = "primary") {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }
  method <- as.character(method)
  if (length(method) != 1L || is.na(method) ||
      !(method %in% analysis_colour_methods())) {
    stop(
      "colour method must be exactly one of: ",
      paste(analysis_colour_methods(), collapse = ", "),
      call. = FALSE
    )
  }

  rgb_channels <- c("R", "G", "B")
  missing_primary <- setdiff(rgb_channels, names(data))
  if (length(missing_primary)) {
    stop(
      "Missing primary RGB columns: ",
      paste(missing_primary, collapse = ", "),
      call. = FALSE
    )
  }
  primary_columns <- paste0("primary_", rgb_channels)
  primary_present <- primary_columns %in% names(data)
  if (any(primary_present) && !all(primary_present)) {
    stop("primary_R, primary_G and primary_B must be present together.", call. = FALSE)
  }
  if (!all(primary_present)) {
    for (index in seq_along(rgb_channels)) {
      data[[primary_columns[[index]]]] <- data[[rgb_channels[[index]]]]
    }
  }

  source_columns <- switch(
    method,
    primary = primary_columns,
    mean = paste0("mean_", rgb_channels),
    legacy = paste0("legacy_", rgb_channels),
    paste0(method, "_", rgb_channels)
  )
  missing_source <- setdiff(source_columns, names(data))
  if (length(missing_source)) {
    stop(
      "Selected colour method is missing columns: ",
      paste(missing_source, collapse = ", "),
      call. = FALSE
    )
  }

  for (column in unique(c(primary_columns, source_columns))) {
    data[[column]] <- coerce_numeric_strict(data[[column]], column)
  }
  selected_rgb <- as.matrix(data[source_columns])
  finite_values <- is.finite(selected_rgb)
  observed_finite <- !is.na(selected_rgb) & finite_values
  if (any(observed_finite & (selected_rgb < 0 | selected_rgb > 255))) {
    stop(
      "Selected colour method contains RGB values outside the 0-255 scale.",
      call. = FALSE
    )
  }
  available <- rowSums(finite_values) == length(rgb_channels)

  selected <- data
  selected$analysis_colour_method <- method
  selected$analysis_colour_source_columns <- paste(source_columns, collapse = ";")
  for (index in seq_along(rgb_channels)) {
    selected[[rgb_channels[[index]]]] <- selected_rgb[, index]
  }

  audit <- data.frame(
    analysis_colour_method = method,
    source_columns = paste(source_columns, collapse = ";"),
    input_records = nrow(selected),
    selected_records = sum(available),
    rejected_missing_or_nonfinite = sum(!available),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      data = selected[available, , drop = FALSE],
      excluded = selected[!available, , drop = FALSE],
      audit = audit,
      method = method,
      source_columns = source_columns
    ),
    class = "analysis_colour_selection"
  )
}

srgb_to_cielab_d65 <- function(rgb) {
  rgb <- as.matrix(rgb)
  if (ncol(rgb) != 3L) {
    stop("RGB input must have exactly three columns.", call. = FALSE)
  }
  if (!is.numeric(rgb) || any(!is.finite(rgb)) || any(rgb < 0 | rgb > 255)) {
    stop("RGB input must contain finite values on the 0-255 scale.", call. = FALSE)
  }

  encoded <- rgb / 255
  linear <- ifelse(
    encoded <= 0.04045,
    encoded / 12.92,
    ((encoded + 0.055) / 1.055)^2.4
  )
  srgb_to_xyz <- matrix(
    c(
      0.4124564, 0.3575761, 0.1804375,
      0.2126729, 0.7151522, 0.0721750,
      0.0193339, 0.1191920, 0.9503041
    ),
    nrow = 3L,
    byrow = TRUE
  )
  xyz <- linear %*% t(srgb_to_xyz)
  xyz <- sweep(xyz, 2L, c(0.95047, 1, 1.08883), "/")
  delta <- 6 / 29
  transformed <- ifelse(
    xyz > delta^3,
    xyz^(1 / 3),
    xyz / (3 * delta^2) + 4 / 29
  )
  lab <- cbind(
    L = 116 * transformed[, 2L] - 16,
    a = 500 * (transformed[, 1L] - transformed[, 2L]),
    b = 200 * (transformed[, 2L] - transformed[, 3L])
  )
  rownames(lab) <- rownames(rgb)
  lab
}

add_colour_features <- function(data) {
  missing_columns <- setdiff(c("R", "G", "B"), names(data))
  if (length(missing_columns)) {
    stop("Missing RGB columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  for (column in c("R", "G", "B")) {
    if (!is.numeric(data[[column]])) {
      stop("Column '", column, "' must be numeric.", call. = FALSE)
    }
  }

  rgb <- as.matrix(data[c("R", "G", "B")])
  if (any(!is.na(rgb) & !is.finite(rgb))) {
    stop("RGB values must be finite or NA.", call. = FALSE)
  }
  observed_channels <- rowSums(!is.na(rgb))
  if (any(observed_channels > 0L & observed_channels < 3L)) {
    stop("RGB channels must be either all observed or all missing per record.", call. = FALSE)
  }
  observed <- !is.na(rgb)
  if (any(rgb[observed] < 0 | rgb[observed] > 255)) {
    stop("RGB values must use the documented 0-255 sRGB scale.", call. = FALSE)
  }

  complete <- observed_channels == 3L
  lab <- matrix(NA_real_, nrow(data), 3L, dimnames = list(NULL, c("L", "a", "b")))
  hsv <- matrix(NA_real_, nrow(data), 3L, dimnames = list(NULL, c("H", "S", "V")))
  colour_hex <- rep(NA_character_, nrow(data))

  if (any(complete)) {
    rgb_complete <- rgb[complete, , drop = FALSE]
    lab[complete, ] <- srgb_to_cielab_d65(rgb_complete)
    hsv[complete, ] <- t(grDevices::rgb2hsv(
      rgb_complete[, 1L],
      rgb_complete[, 2L],
      rgb_complete[, 3L],
      maxColorValue = 255
    ))
    colour_hex[complete] <- grDevices::rgb(
      rgb_complete[, 1L],
      rgb_complete[, 2L],
      rgb_complete[, 3L],
      maxColorValue = 255
    )
  }

  data$L <- lab[, "L"]
  data$a <- lab[, "a"]
  data$b <- lab[, "b"]
  data$C <- sqrt(data$a^2 + data$b^2)
  data$darkness <- -data$L
  data$H <- hsv[, "H"]
  data$S <- hsv[, "S"]
  data$V <- hsv[, "V"]
  data$colour_hex <- colour_hex
  data
}

fit_oriented_pca <- function(data,
                             variables,
                             anchor,
                             anchor_direction = 1,
                             min_complete = 3L) {
  variables <- unique(as.character(variables))
  if (length(variables) < 2L) {
    stop("PCA requires at least two variables.", call. = FALSE)
  }
  if (!(anchor %in% variables)) {
    stop("The PCA anchor must be one of 'variables'.", call. = FALSE)
  }
  if (!(anchor_direction %in% c(-1, 1))) {
    stop("anchor_direction must be -1 or 1.", call. = FALSE)
  }
  missing_columns <- setdiff(variables, names(data))
  if (length(missing_columns)) {
    stop("Missing PCA columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  x <- data[variables]
  if (!all(vapply(x, is.numeric, logical(1)))) {
    stop("Every PCA variable must be numeric.", call. = FALSE)
  }
  x_matrix <- as.matrix(x)
  complete <- stats::complete.cases(x_matrix) &
    apply(x_matrix, 1L, function(row) all(is.finite(row)))
  if (sum(complete) < max(as.integer(min_complete), length(variables) + 1L)) {
    stop("Too few complete records to fit PCA.", call. = FALSE)
  }

  standard_deviations <- apply(x_matrix[complete, , drop = FALSE], 2L, stats::sd)
  if (any(!is.finite(standard_deviations) | standard_deviations <= 0)) {
    stop("PCA variables must have finite, non-zero variance.", call. = FALSE)
  }

  model <- stats::prcomp(
    x_matrix[complete, , drop = FALSE],
    center = TRUE,
    scale. = TRUE
  )
  anchor_loading <- model$rotation[anchor, 1L]
  if (!is.finite(anchor_loading) || abs(anchor_loading) < sqrt(.Machine$double.eps)) {
    stop("The PC1 anchor loading is zero; its direction cannot be fixed.", call. = FALSE)
  }
  orientation <- if (sign(anchor_loading) == anchor_direction) 1 else -1

  scores <- rep(NA_real_, nrow(data))
  scores[complete] <- model$x[, 1L] * orientation
  loadings <- model$rotation[, 1L] * orientation
  explained_variance <- model$sdev[1L]^2 / sum(model$sdev^2)

  structure(
    list(
      model = model,
      variables = variables,
      anchor = anchor,
      anchor_direction = anchor_direction,
      orientation = orientation,
      complete = complete,
      scores = scores,
      loadings = loadings,
      explained_variance = explained_variance
    ),
    class = "oriented_pca"
  )
}

predict.oriented_pca <- function(object, newdata, ...) {
  missing_columns <- setdiff(object$variables, names(newdata))
  if (length(missing_columns)) {
    stop("Missing PCA columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  x <- as.matrix(newdata[object$variables])
  complete <- stats::complete.cases(x) & apply(x, 1L, function(row) all(is.finite(row)))
  scores <- rep(NA_real_, nrow(newdata))
  if (any(complete)) {
    predicted <- stats::predict(object$model, newdata = x[complete, , drop = FALSE])
    scores[complete] <- predicted[, 1L] * object$orientation
  }
  scores
}

audit_analysis_grain <- function(data,
                                 id_col = "record_id",
                                 longitude_col = "longitude",
                                 latitude_col = "latitude") {
  required <- c(id_col, longitude_col, latitude_col)
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns)) {
    stop("Missing grain-audit columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(data[[id_col]])) {
    stop(id_col, " must be unique at photograph grain.", call. = FALSE)
  }

  valid <- is.finite(data[[longitude_col]]) & is.finite(data[[latitude_col]])
  key <- rep(NA_character_, nrow(data))
  key[valid] <- paste(
    format(data[[longitude_col]][valid], digits = 16L, scientific = FALSE, trim = TRUE),
    format(data[[latitude_col]][valid], digits = 16L, scientific = FALSE, trim = TRUE),
    sep = "|"
  )
  counts <- table(key[valid])

  data.frame(
    metric = c(
      "photograph_records", "valid_coordinate_records", "exact_coordinate_sites",
      "repeated_exact_sites", "extra_records_at_exact_sites"
    ),
    value = c(
      nrow(data), sum(valid), length(counts), sum(counts > 1L), sum(pmax(counts - 1L, 0L))
    ),
    row.names = NULL
  )
}

mean_or_na <- function(x) {
  if (!length(x) || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

prepare_analysis_grain <- function(data,
                                   grain = c("photo", "site"),
                                   site_id_col = NULL,
                                   value_cols = character(),
                                   record_id_col = "record_id",
                                   coordinate_tolerance = 1e-10) {
  grain <- match.arg(grain)
  if (!(record_id_col %in% names(data))) {
    stop("Missing record identifier column: ", record_id_col, call. = FALSE)
  }
  if (anyDuplicated(data[[record_id_col]])) {
    stop(record_id_col, " must be unique.", call. = FALSE)
  }

  if (grain == "photo") {
    result <- data
    result$analysis_id <- as.character(result[[record_id_col]])
    result$n_photos <- 1L
    return(result)
  }

  if (is.null(site_id_col) || !(site_id_col %in% names(data))) {
    stop(
      "Site-grain analysis requires an explicit site_id column; coordinates are not silently deduplicated.",
      call. = FALSE
    )
  }
  site_id <- as.character(data[[site_id_col]])
  if (anyNA(site_id) || any(trimws(site_id) == "")) {
    stop("Site identifiers must be non-missing.", call. = FALSE)
  }
  missing_values <- setdiff(value_cols, names(data))
  if (length(missing_values)) {
    stop("Missing site summary columns: ", paste(missing_values, collapse = ", "), call. = FALSE)
  }
  if (length(value_cols) && !all(vapply(data[value_cols], is.numeric, logical(1)))) {
    stop("Site summary columns must be numeric.", call. = FALSE)
  }
  if (!is.numeric(coordinate_tolerance) || length(coordinate_tolerance) != 1L ||
      !is.finite(coordinate_tolerance) || coordinate_tolerance < 0) {
    stop("coordinate_tolerance must be one non-negative finite number.", call. = FALSE)
  }

  ordered_sites <- unique(site_id)
  groups <- split(seq_len(nrow(data)), factor(site_id, levels = ordered_sites))
  rows <- lapply(names(groups), function(site) {
    index <- groups[[site]]
    site_longitude <- data$longitude[index]
    site_latitude <- data$latitude[index]
    if (any(!is.finite(site_longitude)) || any(!is.finite(site_latitude))) {
      stop("Site '", site, "' contains missing or non-finite coordinates.", call. = FALSE)
    }
    if (diff(range(site_longitude)) > coordinate_tolerance ||
        diff(range(site_latitude)) > coordinate_tolerance) {
      stop(
        "Site '", site, "' spans multiple coordinates; define an explicit spatial unit before aggregation.",
        call. = FALSE
      )
    }
    row <- data.frame(
      analysis_id = site,
      site_id = site,
      n_photos = length(index),
      record_ids = paste(data[[record_id_col]][index], collapse = ";"),
      longitude = site_longitude[[1L]],
      latitude = site_latitude[[1L]],
      stringsAsFactors = FALSE
    )
    if ("date" %in% names(data) && inherits(data$date, "Date")) {
      row$date_start <- if (all(is.na(data$date[index]))) as.Date(NA) else min(data$date[index], na.rm = TRUE)
      row$date_end <- if (all(is.na(data$date[index]))) as.Date(NA) else max(data$date[index], na.rm = TRUE)
    }
    if ("qc_status" %in% names(data)) {
      status <- tolower(trimws(as.character(data$qc_status[index])))
      status[is.na(status) | !nzchar(status)] <- "<missing>"
      row$qc_statuses <- paste(sort(unique(status)), collapse = ";")
      row$qc_ok_n <- sum(status == "ok")
      row$qc_manual_review_required_n <- sum(status == "manual_review_required")
    }
    for (column in value_cols) {
      row[[column]] <- mean_or_na(data[[column]][index])
    }
    row
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

extract_raster_values <- function(data,
                                  rasters,
                                  id_col = "record_id",
                                  longitude_col = "longitude",
                                  latitude_col = "latitude",
                                  input_crs = "EPSG:4326",
                                  prefix = NULL) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required for raster extraction.", call. = FALSE)
  }
  required <- c(id_col, longitude_col, latitude_col)
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns)) {
    stop("Missing raster-extraction columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(data[[id_col]])) {
    stop(id_col, " must be unique before raster extraction.", call. = FALSE)
  }

  raster_paths <- NULL
  if (inherits(rasters, "SpatRaster")) {
    raster <- rasters
  } else if (is.character(rasters) && length(rasters)) {
    if (any(!file.exists(rasters))) {
      stop("Raster files do not exist: ", paste(rasters[!file.exists(rasters)], collapse = ", "), call. = FALSE)
    }
    raster_paths <- rasters
    raster <- terra::rast(rasters)
  } else {
    stop("'rasters' must be a SpatRaster or a non-empty vector of file paths.", call. = FALSE)
  }
  if (!nzchar(terra::crs(raster))) {
    stop("Raster CRS is missing.", call. = FALSE)
  }

  layer_names <- names(raster)
  if (!is.null(raster_paths) && terra::nlyr(raster) == length(raster_paths)) {
    layer_names <- tools::file_path_sans_ext(basename(raster_paths))
  }
  if (anyNA(layer_names) || any(layer_names == "")) {
    layer_names <- paste0("layer_", seq_len(terra::nlyr(raster)))
  }
  layer_names <- make.unique(layer_names, sep = "_")
  if (!is.null(prefix)) {
    layer_names <- paste0(prefix, layer_names)
  }
  names(raster) <- layer_names

  longitude <- coerce_numeric_strict(data[[longitude_col]], longitude_col)
  latitude <- coerce_numeric_strict(data[[latitude_col]], latitude_col)
  valid <- is.finite(longitude) & is.finite(latitude)
  value_matrix <- matrix(
    NA_real_,
    nrow = nrow(data),
    ncol = terra::nlyr(raster),
    dimnames = list(NULL, layer_names)
  )

  if (any(valid)) {
    points <- terra::vect(
      data.frame(longitude = longitude[valid], latitude = latitude[valid]),
      geom = c("longitude", "latitude"),
      crs = input_crs
    )
    if (!terra::same.crs(points, raster)) {
      points <- terra::project(points, terra::crs(raster))
    }
    extracted <- terra::extract(raster, points, ID = FALSE)
    if (nrow(extracted) != sum(valid)) {
      stop("Raster extraction changed the number or order of point rows.", call. = FALSE)
    }
    if (!all(vapply(extracted, is.numeric, logical(1)))) {
      stop("Only numeric raster layers are supported by this analysis core.", call. = FALSE)
    }
    value_matrix[valid, ] <- as.matrix(extracted)
  }

  result <- data[id_col]
  result <- cbind(result, as.data.frame(value_matrix, check.names = FALSE))
  rownames(result) <- NULL
  attr(result, "invalid_coordinate_rows") <- which(!valid)
  result
}

attach_raster_values <- function(data,
                                 extracted,
                                 id_col = "record_id",
                                 allow_overwrite = FALSE) {
  if (!(id_col %in% names(data)) || !(id_col %in% names(extracted))) {
    stop("Both tables must contain '", id_col, "'.", call. = FALSE)
  }
  if (anyDuplicated(data[[id_col]]) || anyDuplicated(extracted[[id_col]])) {
    stop("Raster joins require unique identifiers.", call. = FALSE)
  }
  index <- match(data[[id_col]], extracted[[id_col]])
  if (anyNA(index)) {
    stop("Raster extraction is missing one or more input identifiers.", call. = FALSE)
  }

  value_columns <- setdiff(names(extracted), id_col)
  conflicts <- intersect(value_columns, names(data))
  if (length(conflicts) && !allow_overwrite) {
    stop("Raster columns already exist: ", paste(conflicts, collapse = ", "), call. = FALSE)
  }
  for (column in value_columns) {
    data[[column]] <- extracted[[column]][index]
  }
  data
}

add_bombus_suitability_sum <- function(data,
                                       species_cols = bombus_species_columns(),
                                       output_col = "Bombus_suitability_sum",
                                       coverage_col = "Bombus_predictions_n",
                                       require_all = TRUE) {
  if (length(species_cols) != 5L || anyDuplicated(species_cols)) {
    stop("Exactly five unique Bombus species columns are required.", call. = FALSE)
  }
  missing_columns <- setdiff(species_cols, names(data))
  if (length(missing_columns)) {
    stop("Missing Bombus prediction columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (!all(vapply(data[species_cols], is.numeric, logical(1)))) {
    stop("Bombus prediction columns must be numeric.", call. = FALSE)
  }

  prediction <- as.matrix(data[species_cols])
  if (any(!is.na(prediction) & !is.finite(prediction))) {
    stop("Bombus predictions must be finite or NA.", call. = FALSE)
  }
  finite <- is.finite(prediction)
  if (any(prediction[finite] < 0 | prediction[finite] > 1)) {
    stop("Bombus SDM predictions must be on the documented 0-1 scale.", call. = FALSE)
  }

  coverage <- rowSums(finite)
  suitability <- rowSums(prediction, na.rm = TRUE)
  if (require_all) {
    suitability[coverage != length(species_cols)] <- NA_real_
  } else {
    suitability[coverage == 0L] <- NA_real_
  }
  data[[coverage_col]] <- coverage
  data[[output_col]] <- suitability
  data
}

as_draw_matrix <- function(x, name) {
  if (is.null(dim(x))) {
    matrix(
      as.numeric(x),
      ncol = 1L,
      dimnames = list(names(x), "draw_1")
    )
  } else {
    if (length(dim(x)) != 2L) {
      stop(name, " must be a vector or two-dimensional matrix.", call. = FALSE)
    }
    value <- as.matrix(x)
    storage.mode(value) <- "double"
    value
  }
}

has_complete_unique_names <- function(value) {
  !is.null(value) && length(value) > 0L && !anyNA(value) &&
    all(nzchar(value)) && !anyDuplicated(value)
}

variance_decomposition_with_covariance <- function(fixed,
                                                   spatial,
                                                   residual_variance,
                                                   alignment = c(
                                                     "names",
                                                     "position"
                                                   )) {
  alignment <- match.arg(alignment)
  fixed_was_vector <- is.null(dim(fixed))
  spatial_was_vector <- is.null(dim(spatial))
  fixed_input_names <- if (fixed_was_vector) {
    list(rows = names(fixed), draws = NULL)
  } else {
    list(rows = rownames(fixed), draws = colnames(fixed))
  }
  spatial_input_names <- if (spatial_was_vector) {
    list(rows = names(spatial), draws = NULL)
  } else {
    list(rows = rownames(spatial), draws = colnames(spatial))
  }
  fixed <- as_draw_matrix(fixed, "fixed")
  spatial <- as_draw_matrix(spatial, "spatial")
  if (!identical(dim(fixed), dim(spatial))) {
    stop("fixed and spatial must have identical observation-by-draw dimensions.", call. = FALSE)
  }
  if (nrow(fixed) < 2L || ncol(fixed) < 1L) {
    stop("At least two observations are required for variance decomposition.", call. = FALSE)
  }

  if (identical(alignment, "names")) {
    if (!has_complete_unique_names(rownames(fixed)) ||
        !has_complete_unique_names(colnames(fixed)) ||
        !has_complete_unique_names(rownames(spatial)) ||
        !has_complete_unique_names(colnames(spatial))) {
      stop(
        paste(
          "Name alignment requires complete unique observation row names and",
          "draw column names for fixed and spatial; use alignment = 'position'",
          "only for genuinely unnamed, already-aligned inputs."
        ),
        call. = FALSE
      )
    }
    if (!setequal(rownames(fixed), rownames(spatial)) ||
        !setequal(colnames(fixed), colnames(spatial))) {
      stop("fixed and spatial dimname sets must match exactly.", call. = FALSE)
    }
    spatial <- spatial[
      rownames(fixed),
      colnames(fixed),
      drop = FALSE
    ]
    draw_names <- colnames(fixed)
  } else {
    supplied_names <- c(
      fixed_input_names$rows,
      fixed_input_names$draws,
      spatial_input_names$rows,
      spatial_input_names$draws
    )
    if (length(supplied_names)) {
      stop(
        "Positional alignment is allowed only when fixed and spatial are unnamed.",
        call. = FALSE
      )
    }
    draw_names <- paste0("draw_", seq_len(ncol(fixed)))
    rownames(fixed) <- NULL
    rownames(spatial) <- NULL
    colnames(fixed) <- draw_names
    colnames(spatial) <- draw_names
  }
  if (any(!is.finite(fixed)) || any(!is.finite(spatial))) {
    stop("fixed and spatial values must be finite.", call. = FALSE)
  }

  residual_names <- names(residual_variance)
  residual_variance <- as.numeric(residual_variance)
  if (length(residual_variance) == 1L) {
    residual_variance <- rep(residual_variance, ncol(fixed))
  } else if (identical(alignment, "names")) {
    if (!has_complete_unique_names(residual_names) ||
        !setequal(residual_names, draw_names)) {
      stop(
        "Multi-draw residual_variance must have unique names matching fixed draw names.",
        call. = FALSE
      )
    }
    residual_variance <- residual_variance[match(draw_names, residual_names)]
  } else if (!is.null(residual_names)) {
    stop("Positional residual_variance must be unnamed.", call. = FALSE)
  }
  if (length(residual_variance) != ncol(fixed) ||
      any(!is.finite(residual_variance) | residual_variance < 0)) {
    stop("residual_variance must be non-negative and have one value per draw.", call. = FALSE)
  }

  per_draw <- lapply(seq_len(ncol(fixed)), function(draw) {
    fixed_variance <- stats::var(fixed[, draw])
    spatial_variance <- stats::var(spatial[, draw])
    covariance_contribution <- 2 * stats::cov(fixed[, draw], spatial[, draw])
    fitted_variance <- stats::var(fixed[, draw] + spatial[, draw])
    total_variance <- fitted_variance + residual_variance[draw]

    reconstructed <- fixed_variance + spatial_variance + covariance_contribution
    if (!isTRUE(all.equal(fitted_variance, reconstructed, tolerance = 1e-10))) {
      stop("Variance identity failed for draw ", draw_names[[draw]], ".", call. = FALSE)
    }
    if (!is.finite(total_variance) || total_variance <= 0) {
      stop("Total variance must be positive for every draw.", call. = FALSE)
    }

    contribution <- c(
      fixed = fixed_variance,
      spatial = spatial_variance,
      fixed_spatial_covariance = covariance_contribution,
      residual = residual_variance[draw],
      total = total_variance
    )
    data.frame(
      draw = draw,
      draw_name = draw_names[[draw]],
      component = names(contribution),
      contribution = unname(contribution),
      proportion = c(contribution[1:4] / total_variance, 1),
      row.names = NULL
    )
  })
  per_draw <- do.call(rbind, per_draw)

  components <- unique(per_draw$component)
  summary <- do.call(rbind, lapply(components, function(component) {
    rows <- per_draw[per_draw$component == component, , drop = FALSE]
    data.frame(
      component = component,
      contribution_median = stats::median(rows$contribution),
      contribution_lower95 = unname(stats::quantile(rows$contribution, 0.025)),
      contribution_upper95 = unname(stats::quantile(rows$contribution, 0.975)),
      proportion_median = stats::median(rows$proportion),
      proportion_lower95 = unname(stats::quantile(rows$proportion, 0.025)),
      proportion_upper95 = unname(stats::quantile(rows$proportion, 0.975)),
      row.names = NULL
    )
  }))

  structure(
    list(draws = per_draw, summary = summary),
    class = "variance_decomposition"
  )
}
