# Minimal spatial-context helper retained by the active human-context stage.
#
# The original multiscale-hotspot implementation is archived under legacy/.
# This function is the only part of that implementation required by the 1,909
# pipeline: summing or averaging raster values within fixed kilometre radii.

multiscale_point_context <- function(raster, sites, radii_km,
                                     summary_function = c("mean", "sum")) {
  summary_function <- match.arg(summary_function)
  raster_values <- as.matrix(raster, wide = TRUE)
  x_centres <- terra::xFromCol(raster, seq_len(terra::ncol(raster)))
  y_centres <- terra::yFromRow(raster, seq_len(terra::nrow(raster)))
  result <- matrix(
    NA_real_, nrow = nrow(sites), ncol = length(radii_km),
    dimnames = list(NULL, as.character(radii_km))
  )
  maximum_radius <- max(radii_km)
  for (index in seq_len(nrow(sites))) {
    longitude <- sites$longitude[index]
    latitude <- sites$latitude[index]
    longitude_km <- 111.32 * cos(latitude * pi / 180)
    rows <- which(abs(y_centres - latitude) * 110.57 <= maximum_radius)
    columns <- which(abs(x_centres - longitude) * longitude_km <= maximum_radius)
    if (!length(rows) || !length(columns)) next
    y_distance <- (y_centres[rows] - latitude) * 110.57
    x_distance <- (x_centres[columns] - longitude) * longitude_km
    distance <- sqrt(outer(y_distance^2, x_distance^2, "+"))
    values <- raster_values[rows, columns, drop = FALSE]
    for (radius_index in seq_along(radii_km)) {
      selected <- values[
        distance <= radii_km[radius_index] & is.finite(values)
      ]
      if (!length(selected)) next
      result[index, radius_index] <- if (summary_function == "mean") {
        mean(selected)
      } else {
        sum(selected)
      }
    }
  }
  result
}
