args <- commandArgs(trailingOnly = TRUE)

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[1L])
} else "scripts/build_human_raster.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "pipeline_support.R"))
arg_value <- function(flag, default = "") hb_arg_value(args, flag, default)
hb_require_stage_packages("human_raster")
hb_load_modules("human_raster", root = repo_root)

# MLIT occasionally closes large archive transfers early. A partial non-empty ZIP
# must never be accepted as cache, otherwise the next run fails at unzip instead
# of retrying the acquisition. Keep this resilience at the public-source build
# boundary so it does not alter any downstream analysis estimand.
mlit_archive_is_valid <- function(path) {
  if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size <= 0) {
    return(FALSE)
  }
  listing <- tryCatch(
    suppressWarnings(utils::unzip(path, list = TRUE)),
    error = function(e) NULL
  )
  !is.null(listing) &&
    nrow(listing) > 0L &&
    sum(grepl("\\.dbf$", listing$Name, ignore.case = TRUE)) == 1L
}

download_mlit_archive <- function(primary_mesh, cache_dir, retries = 3L) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(
    cache_dir, sprintf("L03-b-21_%s-jgd2011_GML.zip", primary_mesh)
  )
  if (mlit_archive_is_valid(archive)) return(archive)
  if (file.exists(archive)) unlink(archive, force = TRUE)

  url <- mlit_archive_url(primary_mesh)
  old_timeout <- getOption("timeout")
  options(timeout = max(1800, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  last_error <- NULL
  for (attempt in seq_len(as.integer(retries))) {
    if (file.exists(archive)) unlink(archive, force = TRUE)
    message(sprintf(
      "MLIT download %s attempt %d/%d", primary_mesh, attempt, retries
    ))
    ok <- tryCatch({
      status <- utils::download.file(
        url, archive, mode = "wb", method = "libcurl", quiet = TRUE
      )
      identical(status, 0L) && mlit_archive_is_valid(archive)
    }, warning = function(w) {
      last_error <<- conditionMessage(w)
      FALSE
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      FALSE
    })
    if (isTRUE(ok)) return(archive)

    if (file.exists(archive)) unlink(archive, force = TRUE)
    if (attempt < retries) Sys.sleep(3 * attempt)
  }

  stop(
    "Failed to download a complete MLIT archive for primary mesh ", primary_mesh,
    " after ", retries, " attempts",
    if (!is.null(last_error)) paste0(": ", last_error) else "",
    call. = FALSE
  )
}

observation_csv <- arg_value(
  "--observation-csv",
  file.path(repo_root, "Data_S1.csv")
)
output_dir <- arg_value(
  "--output-dir",
  file.path(repo_root, "results", "public_rasters", "mlit_human_forest_edge_2021")
)
cache_dir <- arg_value(
  "--cache-dir",
  Sys.getenv(
    "HOTARUBUKURO_MLIT_CACHE",
    file.path(path.expand("~"), ".cache", "hotarubukuro", "mlit_l03_2021")
  )
)

build_mlit_human_forest_edge(observation_csv, output_dir, cache_dir)
cat(
  "Completed MLIT human-forest edge layer: ",
  normalizePath(output_dir, winslash = "/"), "\n", sep = ""
)
