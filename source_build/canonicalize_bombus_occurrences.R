#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) return(args[[i + 1L]])
  prefix <- paste0(flag, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

required <- c("readr", "digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "), call. = FALSE)

input_dir <- arg_value("--input-dir", "results/bombus_occurrence_live")
output_dir <- arg_value("--output-dir", "results/bombus_occurrence_snapshot")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

members <- c(
  "ardens", "diversus", "beaticola", "consobrinus", "honshuensis",
  "bombus_target_group"
)
roles <- c(rep("focal_species", 5L), "target_group_background")
rows <- list()
for (i in seq_along(members)) {
  sh <- members[[i]]
  src <- file.path(input_dir, paste0(sh, "_gbif.csv"))
  if (!file.exists(src)) stop("Missing occurrence source: ", src, call. = FALSE)
  x <- readr::read_csv(src, show_col_types = FALSE)
  if (!("key" %in% names(x))) stop("Occurrence CSV has no GBIF key: ", src, call. = FALSE)
  key <- as.character(x$key)
  if (anyNA(key) || any(!nzchar(key))) stop("Occurrence CSV has missing GBIF keys: ", src, call. = FALSE)
  x <- x[order(key), , drop = FALSE]
  x <- x[!duplicated(as.character(x$key)), , drop = FALSE]
  dst <- file.path(output_dir, paste0(sh, "_gbif.csv"))
  readr::write_csv(x, dst, na = "")
  rows[[sh]] <- data.frame(
    short = sh,
    role = roles[[i]],
    n_records = nrow(x),
    first_key = as.character(x$key[[1]]),
    last_key = as.character(x$key[[nrow(x)]]),
    sha256 = unname(digest::digest(file = dst, algo = "sha256")),
    stringsAsFactors = FALSE
  )
}
manifest <- do.call(rbind, rows)
readr::write_csv(manifest, file.path(output_dir, "occurrence_snapshot_manifest.csv"), na = "")

for (name in c("manifest.csv", "query_manifest.json")) {
  src <- file.path(input_dir, name)
  if (file.exists(src)) file.copy(src, file.path(output_dir, name), overwrite = TRUE)
}

jsonlite::write_json(
  list(
    canonicalization = "sort by GBIF occurrence key; deduplicate exact occurrence key",
    members = members,
    roles = stats::setNames(as.list(roles), members),
    target_group_background = paste(
      "bombus_target_group is a frozen genus-wide Japanese Bombus occurrence",
      "snapshot used as a spatial observation-effort background"
    ),
    manifest_sha256 = unname(digest::digest(
      file = file.path(output_dir, "occurrence_snapshot_manifest.csv"), algo = "sha256"
    ))
  ),
  file.path(output_dir, "canonicalization.json"),
  pretty = TRUE, auto_unbox = TRUE
)
cat("Canonical Bombus occurrence snapshot written to ", output_dir, "\n", sep = "")
