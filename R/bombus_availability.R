# Sensitivity summaries for continuous legacy Bombus SDM outputs.
#
# These indices are not calibrated occurrence probabilities. They compare
# alternative ways to summarize potential availability from the same five
# maxnet cloglog suitability surfaces. The widespread/montane split is retained
# only as a diagnostic of spatial and elevational structure, not as an
# alternative biological availability definition.

bombus_spatial_distribution_groups <- function() {
  list(
    widespread = c("ardens", "diversus"),
    montane = c("beaticola", "consobrinus", "honshuensis")
  )
}

validate_bombus_groups <- function(
    groups = bombus_spatial_distribution_groups(),
    expected_species = expected_bombus_species()) {
  if (!is.list(groups) || !length(groups) || is.null(names(groups)) ||
      any(!nzchar(names(groups)))) {
    stop("Bombus spatial groups must be a named, non-empty list.", call. = FALSE)
  }
  members <- unlist(groups, use.names = FALSE)
  if (anyDuplicated(members) || !setequal(members, expected_species)) {
    stop(
      "Bombus spatial groups must partition the five expected species exactly once.",
      call. = FALSE
    )
  }
  invisible(groups)
}

any_species_availability <- function(x, species = names(x)) {
  x <- as.data.frame(x, check.names = FALSE)
  if (!length(species) || any(!species %in% names(x))) {
    stop("Requested Bombus species columns are missing.", call. = FALSE)
  }
  values <- x[, species, drop = FALSE]
  if (!all(vapply(values, is.numeric, logical(1)))) {
    stop("Bombus suitability columns must be numeric.", call. = FALSE)
  }
  raw <- unlist(values, use.names = FALSE)
  finite <- raw[is.finite(raw)]
  if (length(finite) && any(finite < 0 | finite > 1)) {
    stop("Bombus suitability values must lie in [0, 1].", call. = FALSE)
  }
  complete <- stats::complete.cases(values)
  result <- rep(NA_real_, nrow(values))
  if (any(complete)) {
    matrix <- as.matrix(values[complete, , drop = FALSE])
    result[complete] <- 1 - apply(1 - matrix, 1L, prod)
  }
  result
}

maximum_species_availability <- function(x, species = names(x)) {
  x <- as.data.frame(x, check.names = FALSE)
  if (!length(species) || any(!species %in% names(x))) {
    stop("Requested Bombus species columns are missing.", call. = FALSE)
  }
  values <- x[, species, drop = FALSE]
  if (!all(vapply(values, is.numeric, logical(1)))) {
    stop("Bombus suitability columns must be numeric.", call. = FALSE)
  }
  raw <- unlist(values, use.names = FALSE)
  finite <- raw[is.finite(raw)]
  if (length(finite) && any(finite < 0 | finite > 1)) {
    stop("Bombus suitability values must lie in [0, 1].", call. = FALSE)
  }
  complete <- stats::complete.cases(values)
  result <- rep(NA_real_, nrow(values))
  if (any(complete)) {
    result[complete] <- apply(
      as.matrix(values[complete, , drop = FALSE]), 1L, max
    )
  }
  result
}

add_bombus_availability_indices <- function(
    data,
    groups = bombus_spatial_distribution_groups()) {
  validate_bombus_groups(groups)
  species <- expected_bombus_species()
  suitability <- validate_suitability_matrix(data[species], species)

  # Biological availability summaries used in the sensitivity comparison.
  data$Bombus_suitability_sum <- summed_suitability(suitability, species)
  data$Bombus_any_availability <- any_species_availability(suitability, species)
  data$Bombus_max_availability <- maximum_species_availability(suitability, species)

  # Spatial-distribution diagnostics only. These should not be interpreted as
  # independent pollinator functional groups or primary availability metrics.
  for (group in names(groups)) {
    members <- groups[[group]]
    data[[paste0("Bombus_spatial_", group, "_any")]] <-
      any_species_availability(suitability, members)
    data[[paste0("Bombus_spatial_", group, "_max")]] <-
      maximum_species_availability(suitability, members)
  }
  data
}

bombus_availability_predictors <- function() {
  c(
    "Bombus_suitability_sum",
    "Bombus_any_availability",
    "Bombus_max_availability"
  )
}

bombus_spatial_diagnostic_predictors <- function() {
  c(
    "Bombus_spatial_widespread_any",
    "Bombus_spatial_montane_any",
    "Bombus_spatial_widespread_max",
    "Bombus_spatial_montane_max"
  )
}
