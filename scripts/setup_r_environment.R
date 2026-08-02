# Restore the declared R environment on a clean runner.
#
# One strategy, applied everywhere: every CRAN package resolves from the single
# pinned snapshot in dependencies/cran-snapshot.txt, and INLA resolves to the
# exact version pinned in dependencies/inla.csv. Nothing installs from an
# unpinned mirror, and nothing is installed that is not declared in
# dependencies/r-packages.csv.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
report_dir <- arg_value("--report-dir", "reproducibility")
# Which declared scopes to install. A workflow that never fetches raw external
# data does not need the acquisition stack, and a workflow that never draws a
# figure does not need the figure stack. The scopes are named here rather than
# listing packages, so the declaration in dependencies/r-packages.csv stays the
# single source of truth.
scopes_argument <- arg_value("--scopes", "all")
# INLA is required by every modelling stage and by nothing else. A diagnostic
# that only inspects the analysis-population filter runs entirely upstream of
# any model, so it can be provisioned without INLA — which matters because the
# INLA download host is a mutable third-party service that is periodically
# unavailable, and an outage there should not block a diagnostic that does not
# use it. Any workflow that fits a model must leave this at its default.
skip_inla <- hb_as_bool(arg_value("--skip-inla", "false"))
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

repository <- rp_cran_repository()
options(
  repos = c(CRAN = repository),
  warn = 1,
  timeout = max(getOption("timeout"), 1800)
)
message("[setup] pinned CRAN snapshot: ", repository)

all_declared <- rp_declared_packages()
declared <- if (identical(tolower(scopes_argument), "all")) {
  all_declared
} else {
  requested <- trimws(strsplit(scopes_argument, ",", fixed = TRUE)[[1L]])
  unknown <- setdiff(requested, unique(all_declared$scope))
  if (length(unknown)) {
    stop(
      "Unknown dependency scopes: ", paste(unknown, collapse = ", "),
      ". Declared scopes: ", paste(unique(all_declared$scope), collapse = ", "),
      call. = FALSE
    )
  }
  all_declared[all_declared$scope %in% requested, , drop = FALSE]
}
message("[setup] scopes: ", paste(unique(declared$scope), collapse = ", "))
cran_packages <- declared$package[declared$source == "cran"]

missing <- cran_packages[
  !vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  message("[setup] installing ", length(missing), " declared CRAN packages")
  utils::install.packages(missing, Ncpus = max(1L, parallel::detectCores()))
}
still_missing <- cran_packages[
  !vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(still_missing)) {
  stop(
    "Declared CRAN packages could not be installed from the pinned snapshot: ",
    paste(still_missing, collapse = ", "),
    call. = FALSE
  )
}

# ---------------------------------------------------------------------------
# INLA
#
# INLA is not on CRAN and its download host serves the current release from a
# mutable path that has been observed to return an HTML error page instead of a
# tarball. Every candidate location is therefore validated as a real gzip
# archive carrying the pinned version before it is installed, and the resolved
# URL, retrieval time, and SHA-256 are recorded.
# ---------------------------------------------------------------------------
if (!"INLA" %in% declared$package) {
  message("[setup] INLA is outside the requested scopes; skipping it")
  rp_write_csv_atomic(
    rp_environment_record(), file.path(report_dir, "package_versions.csv")
  )
  rp_write_session_record(report_dir)
  quit(save = "no", status = 0)
}

inla <- rp_read_key_value("dependencies/inla.csv")
target_version <- inla[["version"]]
if (!nzchar(target_version)) {
  stop("dependencies/inla.csv must pin an INLA version.", call. = FALSE)
}

candidate_urls <- unname(inla[grepl("^source_url_", names(inla))])
candidate_urls <- candidate_urls[nzchar(candidate_urls)]
if (!length(candidate_urls)) {
  stop("dependencies/inla.csv must declare at least one INLA source URL.",
       call. = FALSE)
}

is_gzip_archive <- function(path) {
  if (!file.exists(path) || file.info(path)$size < 1024) return(FALSE)
  magic <- readBin(path, what = "raw", n = 2L)
  length(magic) == 2L && magic[[1L]] == as.raw(0x1f) && magic[[2L]] == as.raw(0x8b)
}

archive_declares_version <- function(path, version) {
  entries <- tryCatch(utils::untar(path, list = TRUE), error = function(e) character())
  description <- entries[basename(entries) == "DESCRIPTION" &
                           startsWith(entries, "INLA/")]
  if (!length(description)) return(FALSE)
  extract_dir <- tempfile("inla-description-")
  dir.create(extract_dir, recursive = TRUE)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  ok <- tryCatch({
    utils::untar(path, files = description[[1L]], exdir = extract_dir)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return(FALSE)
  fields <- read.dcf(file.path(extract_dir, description[[1L]]))
  "Version" %in% colnames(fields) &&
    identical(as.character(fields[1L, "Version"]), version)
}

download_candidate <- function(url, destination, attempts = 4L) {
  for (attempt in seq_len(attempts)) {
    message("[setup] INLA download attempt ", attempt, ": ", url)
    status <- tryCatch(
      utils::download.file(url, destination, mode = "wb", quiet = TRUE),
      error = function(error) {
        message("[setup] download error: ", conditionMessage(error))
        1L
      },
      warning = function(warning) {
        message("[setup] download warning: ", conditionMessage(warning))
        1L
      }
    )
    if (identical(as.integer(status), 0L) && is_gzip_archive(destination)) {
      if (archive_declares_version(destination, target_version)) return(TRUE)
      message("[setup] archive does not declare INLA ", target_version)
      return(FALSE)
    }
    if (file.exists(destination)) unlink(destination)
    Sys.sleep(5 * attempt)
  }
  FALSE
}

installed_version <- if (requireNamespace("INLA", quietly = TRUE)) {
  as.character(utils::packageVersion("INLA"))
} else {
  ""
}

resolved_url <- NA_character_
archive_sha256 <- NA_character_
retrieved_utc <- NA_character_

if (skip_inla) {
  message(
    "[setup] --skip-inla: INLA is not being installed. This environment can ",
    "only run stages that fit no model."
  )
  resolved_url <- "skipped"
} else if (identical(installed_version, target_version)) {
  message("[setup] INLA ", target_version, " already present; reusing it")
  resolved_url <- "already-installed"
} else {
  archive <- file.path(tempdir(), paste0("INLA_", target_version, ".tar.gz"))
  for (url in candidate_urls) {
    if (download_candidate(url, archive)) {
      resolved_url <- url
      break
    }
  }
  if (is.na(resolved_url)) {
    message("[setup] none of the declared INLA locations served version ",
            target_version)
    available <- tryCatch({
      index <- utils::available.packages(
        repos = "https://inla.r-inla-download.org/R/stable"
      )
      if ("INLA" %in% rownames(index)) index["INLA", "Version"] else NA_character_
    }, error = function(error) NA_character_)
    stop(
      "Pinned INLA ", target_version, " is not obtainable from any declared ",
      "source. The current INLA stable index advertises version '",
      available, "'. Update dependencies/inla.csv deliberately rather than ",
      "letting the version drift.",
      call. = FALSE
    )
  }

  archive_sha256 <- rp_sha256(archive)
  retrieved_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  expected <- inla[["expected_sha256"]]
  if (nzchar(expected) && !identical(expected, archive_sha256)) {
    stop(
      "INLA archive checksum does not match the recorded pin.\n",
      "  expected: ", expected, "\n  observed: ", archive_sha256,
      call. = FALSE
    )
  }
  utils::install.packages(archive, repos = NULL, type = "source")
}

if (!skip_inla) {
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("INLA is not loadable after installation.", call. = FALSE)
  }
  if (!identical(as.character(utils::packageVersion("INLA")), target_version)) {
    stop(
      "Installed INLA ", as.character(utils::packageVersion("INLA")),
      " does not match the pinned version ", target_version, ".",
      call. = FALSE
    )
  }
}

record <- data.frame(
  field = c(
    "package", "pinned_version", "installed_version", "resolved_url",
    "archive_sha256", "retrieved_utc", "cran_snapshot", "r_version"
  ),
  value = c(
    "INLA", target_version, as.character(utils::packageVersion("INLA")),
    resolved_url, archive_sha256, retrieved_utc, repository,
    paste(R.version$major, R.version$minor, sep = ".")
  ),
  stringsAsFactors = FALSE
)
rp_write_csv_atomic(record, file.path(report_dir, "inla_resolution.csv"))
# Printed as well as recorded: the source URL is mutable, so the retrieval time
# and checksum that a given run actually used must be visible in its log.
print(record, row.names = FALSE)
rp_write_csv_atomic(
  rp_environment_record(), file.path(report_dir, "package_versions.csv")
)
rp_write_session_record(report_dir)

message("[setup] environment restored; INLA ", target_version,
        " from ", resolved_url)
