args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(flag, default = "") hb_arg_value(args, flag, default)
as_bool <- hb_as_bool
`%||%` <- hb_or_else
hb_require_stage_packages("bombus_occurrences")

output_dir <- arg_value(
  "--output-dir", "results/bombus_occurrence_phenology_cache"
)
refresh <- as_bool(arg_value("--refresh", "false"))
page_size <- as.integer(arg_value("--page-size", "300"))
max_uncertainty_m <- as.numeric(arg_value("--max-coordinate-uncertainty-m", "10000"))
gbif_max_attempts <- as.integer(arg_value("--gbif-max-attempts", "8"))
gbif_initial_backoff_s <- as.numeric(arg_value("--gbif-initial-backoff-s", "2"))
gbif_max_backoff_s <- as.numeric(arg_value("--gbif-max-backoff-s", "60"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!is.finite(gbif_max_attempts) || gbif_max_attempts < 1L) {
  stop("--gbif-max-attempts must be an integer >= 1")
}
if (!is.finite(gbif_initial_backoff_s) || gbif_initial_backoff_s < 0) {
  stop("--gbif-initial-backoff-s must be >= 0")
}
if (!is.finite(gbif_max_backoff_s) || gbif_max_backoff_s < gbif_initial_backoff_s) {
  stop("--gbif-max-backoff-s must be >= --gbif-initial-backoff-s")
}

species <- data.frame(
  scientific_name = c(
    "Bombus ardens", "Bombus diversus", "Bombus beaticola",
    "Bombus consobrinus", "Bombus honshuensis"
  ),
  short = c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis"),
  ecological_group = c("widespread", "widespread", "alpine", "high_montane", "montane"),
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

gbif_retry <- function(request, label) {
  last_error <- NULL
  for (attempt in seq_len(gbif_max_attempts)) {
    value <- tryCatch(
      request(),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    if (!is.null(value)) return(value)
    if (attempt >= gbif_max_attempts) break

    wait_s <- min(
      gbif_max_backoff_s,
      gbif_initial_backoff_s * (2 ^ (attempt - 1L))
    )
    message(
      "[GBIF] request failed for ", label,
      " (attempt ", attempt, "/", gbif_max_attempts, "): ",
      conditionMessage(last_error),
      "; retrying in ", format(wait_s, trim = TRUE), " s"
    )
    if (wait_s > 0) Sys.sleep(wait_s)
  }

  stop(
    "GBIF request failed after ", gbif_max_attempts,
    " attempts for ", label, ": ", conditionMessage(last_error),
    call. = FALSE
  )
}

accepted_basis <- c(
  "HUMAN_OBSERVATION", "OBSERVATION", "PRESERVED_SPECIMEN",
  "MATERIAL_SAMPLE", "MACHINE_OBSERVATION"
)

fetch_taxon <- function(scientific_name, rank, file_stem) {
  path <- file.path(output_dir, paste0(file_stem, "_gbif.csv"))
  if (file.exists(path) && !refresh) {
    return(readr::read_csv(path, show_col_types = FALSE))
  }

  backbone <- gbif_retry(
    function() rgbif::name_backbone(
      name = scientific_name, kingdom = "Animalia", rank = rank, strict = FALSE
    ),
    paste0(file_stem, " taxon backbone")
  )
  taxon_key <- extract_key(backbone)
  if (is.na(taxon_key)) stop("GBIF taxon key unresolved for ", scientific_name)

  pages <- list()
  start <- 0L
  repeat {
    message("[GBIF] ", file_stem, " start=", start)
    ans <- gbif_retry(
      function() rgbif::occ_search(
        taxonKey = taxon_key, country = "JP", hasCoordinate = TRUE,
        occurrenceStatus = "present", limit = page_size, start = start,
        fields = c(
          "key", "scientificName", "acceptedScientificName", "taxonRank",
          "decimalLongitude", "decimalLatitude",
          "coordinateUncertaintyInMeters", "basisOfRecord", "year", "month",
          "day", "eventDate", "datasetKey", "publishingOrgKey", "issues"
        )
      ),
      paste0(file_stem, " occurrence page start=", start)
    )
    if (!nrow(ans$data)) break
    pages[[length(pages) + 1L]] <- ans$data
    start <- start + nrow(ans$data)
    if (start >= ans$meta$count || nrow(ans$data) < page_size) break
  }

  x <- dplyr::bind_rows(pages)
  if (!nrow(x)) stop("No GBIF records returned for ", scientific_name)
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

manifest <- list()
for (i in seq_len(nrow(species))) {
  sh <- species$short[i]
  x <- fetch_taxon(species$scientific_name[i], "species", sh)
  path <- file.path(output_dir, paste0(sh, "_gbif.csv"))
  valid_month_day <- is.finite(x$month) & x$month >= 1 & x$month <= 12 &
    is.finite(x$day) & x$day >= 1 & x$day <= 31
  manifest[[length(manifest) + 1L]] <- data.frame(
    scientific_name = species$scientific_name[i],
    short = sh,
    ecological_group = species$ecological_group[i],
    role = "focal_species",
    taxon_key = attr(x, "taxon_key") %||% NA_character_,
    n_filtered_records = nrow(x),
    n_with_month_day = sum(valid_month_day),
    month_day_fraction = mean(valid_month_day),
    file = normalizePath(path, winslash = "/", mustWork = TRUE),
    file_md5 = unname(tools::md5sum(path)),
    stringsAsFactors = FALSE
  )
}

# Survey-effort proxy for presence-background modelling.  We use the complete
# Japanese Bombus genus occurrence pool, rather than only the five focal taxa,
# so that background cells represent where bumblebees have been recorded by the
# same heterogeneous GBIF observation process.  This is a sampling-bias control,
# not a statement that all Bombus species are ecologically interchangeable.
tg <- fetch_taxon("Bombus", "genus", "bombus_target_group")
tg_path <- file.path(output_dir, "bombus_target_group_gbif.csv")
tg_valid_month_day <- is.finite(tg$month) & tg$month >= 1 & tg$month <= 12 &
  is.finite(tg$day) & tg$day >= 1 & tg$day <= 31
manifest[[length(manifest) + 1L]] <- data.frame(
  scientific_name = "Bombus",
  short = "bombus_target_group",
  ecological_group = "all_bombus",
  role = "target_group_background",
  taxon_key = attr(tg, "taxon_key") %||% NA_character_,
  n_filtered_records = nrow(tg),
  n_with_month_day = sum(tg_valid_month_day),
  month_day_fraction = mean(tg_valid_month_day),
  file = normalizePath(tg_path, winslash = "/", mustWork = TRUE),
  file_md5 = unname(tools::md5sum(tg_path)),
  stringsAsFactors = FALSE
)

manifest <- do.call(rbind, manifest)
readr::write_csv(manifest, file.path(output_dir, "manifest.csv"))
jsonlite::write_json(
  list(
    accessed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    gbif_query = list(
      country = "JP", hasCoordinate = TRUE, occurrenceStatus = "present",
      maximum_coordinate_uncertainty_m = max_uncertainty_m,
      page_size = page_size
    ),
    target_group_background = paste(
      "all filtered Japanese Bombus genus occurrence cells; used only to",
      "represent spatial observation effort for presence-background SDMs"
    ),
    retry_policy = list(
      maximum_attempts_per_request = gbif_max_attempts,
      initial_backoff_seconds = gbif_initial_backoff_s,
      maximum_backoff_seconds = gbif_max_backoff_s,
      schedule = "deterministic exponential backoff; no random jitter"
    ),
    temporal_use = paste(
      "month/day is retained for response-blind seasonal diagnostics;",
      "it is not treated as visitation or abundance"
    )
  ),
  file.path(output_dir, "query_manifest.json"), pretty = TRUE, auto_unbox = TRUE
)
cat("Wrote focal Bombus occurrences and genus-wide target-group background to ", output_dir, "\n", sep = "")
