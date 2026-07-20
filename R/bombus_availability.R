# Sensitivity summaries for continuous legacy Bombus SDM outputs.
#
# These indices are not calibrated occurrence probabilities. They compare
# alternative ways to summarize potential availability from the same five
# maxnet cloglog suitability surfaces.

bombus_ecological_groups <- function() {
  list(
    widespread = c("ardens", "diversus"),
    montane = c("beaticola", "consobrinus", "honshuensis")
  )
}

validate_bombus_groups <- function(groups = bombus_ecological_groups(),
                                   expected_species = expected_bombus_species()) {
  if (!is.list(groups) || !length(groups) || is.null(names(groups)) ||
      any(!nzchar(names(groups)))) {
    stop("Bombus ecological groups must be a named, non-empty list.", call. = FALSE)
  }
  members <- unlist(groups, use.names = FALSE)
  if (anyDuplicated(members) || !setequal(members, expected_species)) {
    stop(
      "Bombus ecological groups must partition the five expected species exactly once.",
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

add_bombus_availability_indices <- function(data,
                                            groups = bombus_ecological_groups()) {
  validate_bombus_groups(groups)
  species <- expected_bombus_species()
  suitability <- validate_suitability_matrix(data[species], species)

  data$Bombus_suitability_sum <- summed_suitability(suitability, species)
  data$Bombus_any_availability <- any_species_availability(suitability, species)
  data$Bombus_max_availability <- maximum_species_availability(suitability, species)

  for (group in names(groups)) {
    members <- groups[[group]]
    data[[paste0("Bombus_", group, "_any_availability")]] <-
      any_species_availability(suitability, members)
    data[[paste0("Bombus_", group, "_max_availability")]] <-
      maximum_species_availability(suitability, members)
  }
  data
}

bombus_availability_predictors <- function() {
  c(
    "Bombus_suitability_sum",
    "Bombus_any_availability",
    "Bombus_max_availability",
    "Bombus_widespread_any_availability",
    "Bombus_montane_any_availability",
    "Bombus_widespread_max_availability",
    "Bombus_montane_max_availability"
  )
}
