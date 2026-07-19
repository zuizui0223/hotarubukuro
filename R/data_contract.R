# Validation and profiling for the repository's published Data_S1.csv.

validate_data_s1 <- function(data) {
  required <- c("date", "latitude", "longitude", "R", "G", "B")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Data_S1.csv is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!nrow(data)) stop("Data_S1.csv has no rows", call. = FALSE)

  for (column in c("latitude", "longitude", "R", "G", "B")) {
    parsed <- suppressWarnings(as.numeric(data[[column]]))
    if (any(!is.finite(parsed))) stop("Non-finite values in ", column, call. = FALSE)
    data[[column]] <- parsed
  }

  if (any(data$latitude < -90 | data$latitude > 90)) stop("Latitude outside [-90, 90]", call. = FALSE)
  if (any(data$longitude < -180 | data$longitude > 180)) stop("Longitude outside [-180, 180]", call. = FALSE)
  for (channel in c("R", "G", "B")) {
    if (any(data[[channel]] < 0 | data[[channel]] > 255)) stop(channel, " outside [0, 255]", call. = FALSE)
  }

  parsed_date <- as.Date(data$date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d"))
  if (anyNA(parsed_date)) stop("Unparseable dates in Data_S1.csv", call. = FALSE)
  data$date <- parsed_date
  if (!("sample_id" %in% names(data))) data$sample_id <- seq_len(nrow(data))
  data
}

profile_data_s1 <- function(data, coordinate_digits = 6L) {
  data <- validate_data_s1(data)
  coord_key <- paste(round(data$longitude, coordinate_digits), round(data$latitude, coordinate_digits), sep = ":")
  exact_key <- paste(data$date, data$latitude, data$longitude, data$R, data$G, data$B, sep = "|")
  data.frame(
    metric = c("rows", "unique_dates", "unique_coordinates_rounded", "duplicate_coordinate_rows",
               "exact_duplicate_rows", "min_date", "max_date", "min_latitude", "max_latitude",
               "min_longitude", "max_longitude", "min_R", "max_R", "min_G", "max_G", "min_B", "max_B"),
    value = as.character(c(nrow(data), length(unique(data$date)), length(unique(coord_key)), sum(duplicated(coord_key)),
                           sum(duplicated(exact_key)), min(data$date), max(data$date), min(data$latitude), max(data$latitude),
                           min(data$longitude), max(data$longitude), min(data$R), max(data$R), min(data$G), max(data$G),
                           min(data$B), max(data$B))),
    stringsAsFactors = FALSE
  )
}

# Never silently drop repeated coordinates. Aggregation is opt-in and preserves sample size.
aggregate_by_location <- function(data, digits = 6L, response_columns = c("R", "G", "B")) {
  data <- validate_data_s1(data)
  missing <- setdiff(response_columns, names(data))
  if (length(missing)) stop("Missing response columns: ", paste(missing, collapse = ", "), call. = FALSE)
  data$.lon_key <- round(data$longitude, digits)
  data$.lat_key <- round(data$latitude, digits)
  groups <- interaction(data$.lon_key, data$.lat_key, drop = TRUE)
  rows <- lapply(split(seq_len(nrow(data)), groups), function(index) {
    values <- data[index, , drop = FALSE]
    out <- data.frame(longitude = values$.lon_key[[1]], latitude = values$.lat_key[[1]], n_photos = nrow(values))
    for (column in response_columns) out[[column]] <- mean(values[[column]], na.rm = TRUE)
    out
  })
  do.call(rbind, rows)
}
