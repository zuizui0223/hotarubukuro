args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = "") {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1L]
}

as_bool <- function(x) tolower(x) %in% c("1", "true", "yes", "y")
`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
required <- c("rgbif", "dplyr", "readr", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install required packages first: ", paste(missing, collapse = ", "), call. = FALSE)
}

output_dir <- arg_value(
  "--output-dir", "results/bombus_occurrence_phenology_cache"
)
refresh <- as_bool(arg_value("--refresh", "false"))
page_size <- as.integer(arg_value("--page-size", "300"))
max_uncertainty_m <- as.numeric(arg_value("--max-coordinate-uncertainty-m", "10000"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

species <- data.frame(
  scientific_name = c(
    "Bombus ardens", "Bombus diversus", "Bombus beaticola",
    "Bombus consobrinus", "Bombus honshuensis"
  ),
  short = c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis"),
  distribution_block = c("widespread", "widespread", "alpine", "alpine", "alpine"),
  stringsAsFactors = FALSE
)

extract_key <- function(x) {
  candidates <- lapply(c("usageKey", "taxonKey", "key"), function(nm) {
    if (nm %in% names(x)) x[[nm]] else NULL
  })
  for (candidate in candidates) {
    if (!is.null(candidate) && length(candidate) == 1L && !is.na(candidate)) {
      return(as.character(candidate))
    }
  }
  NA_character_
}

fetch_one <- function(scientific_name, short) {
  path <- file.path(output_dir, paste0(short, "_gbif.csv"))
  if (file.exists(path) && !refresh) {
    return(readr::read_csv(path, show_col_types = FALSE))
  }
  backbone <- rgbif::name_backbone(
    name = scientific_name, kingdom = "Animalia", rank = "species", strict = FALSE
  )
  taxon_key <- extract_key(backbone)
  if (is.na(taxon_key)) stop("GBIF taxon key unresolved for ", scientific_name)

  pages <- list()
  start <- 0L
  repeat {
    message("[GBIF] ", short, " start=", start)
    ans <- rgbif::occ_search(
      taxonKey = taxon_key, country = "JP", hasCoordinate = TRUE,
      occurrenceStatus = "present", limit = page_size, start = start,
      fields = c(
        "key", "scientificName", "decimalLongitude", "decimalLatitude",
        "coordinateUncertaintyInMeters", "basisOfRecord", "year", "month",
        "day", "eventDate", "datasetKey", "publishingOrgKey", "issues"
      )
    )
    if (!nrow(ans$data)) break
    pages[[length(pages) + 1L]] <- ans$data
    start <- start + nrow(ans$data)
    if (start >= ans$meta$count || nrow(ans$data) < page_size) break
  }
  x <- dplyr::bind_rows(pages)
  if (!nrow(x)) stop("No GBIF records returned for ", scientific_name)
  accepted_basis <- c(
    "HUMAN_OBSERVATION", "OBSERVATION", "PRESERVED_SPECIMEN",
    "MATERIAL_SAMPLE", "MACHINE_OBSERVATION"
  )
  x <- x |>
    dplyr::mutate(
      decimalLongitude = suppressWarnings(as.numeric(decimalLongitude)),
      decimalLatitude = suppressWarnings(as.numeric(decimalLatitude)),
      coordinateUncertaintyInMeters = suppressWarnings(as.numeric(coordinateUncertaintyInMeters)),
      year = suppressWarnings(as.integer(year)),
      month = suppressWarnings(as.integer(month)),
      day = suppressWarnings(as.integer(day))
    ) |>
    dplyr::filter(
      is.finite(decimalLongitude), is.finite(decimalLatitude),
      decimalLongitude >= 120, decimalLongitude <= 150,
      decimalLatitude >= 20, decimalLatitude <= 50,
      is.na(coordinateUncertaintyInMeters) |
        coordinateUncertaintyInMeters <= max_uncertainty_m,
      is.na(basisOfRecord) | basisOfRecord %in% accepted_basis
    ) |>
    dplyr::distinct(key, .keep_all = TRUE)
  attr(x, "taxon_key") <- taxon_key
  readr::write_csv(x, path)
  x
}

manifest <- vector("list", nrow(species))
for (i in seq_len(nrow(species))) {
  x <- fetch_one(species$scientific_name[i], species$short[i])
  path <- file.path(output_dir, paste0(species$short[i], "_gbif.csv"))
  valid_month_day <- is.finite(x$month) & x$month >= 1 & x$month <= 12 &
    is.finite(x$day) & x$day >= 1 & x$day <= 31
  manifest[[i]] <- data.frame(
    scientific_name = species$scientific_name[i],
    short = species$short[i],
    distribution_block = species$distribution_block[i],
    taxon_key = attr(x, "taxon_key") %||% NA_character_,
    n_filtered_records = nrow(x),
    n_with_month_day = sum(valid_month_day),
    month_day_fraction = mean(valid_month_day),
    file = normalizePath(path, winslash = "/", mustWork = TRUE),
    file_md5 = unname(tools::md5sum(path)),
    stringsAsFactors = FALSE
  )
}
manifest <- do.call(rbind, manifest)
readr::write_csv(manifest, file.path(output_dir, "manifest.csv"))
jsonlite::write_json(
  list(
    accessed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    gbif_query = list(
      country = "JP", hasCoordinate = TRUE, occurrenceStatus = "present",
      maximum_coordinate_uncertainty_m = max_uncertainty_m
    ),
    temporal_use = paste(
      "month/day is used only for response-blind seasonal availability;",
      "it is not treated as visitation or abundance"
    )
  ),
  file.path(output_dir, "query_manifest.json"), pretty = TRUE, auto_unbox = TRUE
)
cat("Wrote frozen Bombus occurrence-date cache to ", output_dir, "\n", sep = "")
