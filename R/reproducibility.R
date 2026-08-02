# Shared reproducibility utilities.
#
# Every workflow in this repository records what it consumed and what it
# produced through these helpers so that an input manifest, an output manifest,
# and a checkpoint written by one stage mean the same thing everywhere.

rp_sha256 <- function(path) {
  if (!file.exists(path)) {
    stop("Cannot hash missing file: ", path, call. = FALSE)
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required for checksum recording.", call. = FALSE)
  }
  digest::digest(path, algo = "sha256", file = TRUE)
}

rp_read_key_value <- function(path) {
  table <- utils::read.csv(
    path, check.names = FALSE, stringsAsFactors = FALSE,
    colClasses = "character"
  )
  if (!all(c("field", "value") %in% names(table))) {
    stop(path, " must contain 'field' and 'value' columns.", call. = FALSE)
  }
  stats::setNames(table$value, table$field)
}

rp_read_lines_config <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  lines[nzchar(lines) & !startsWith(lines, "#")]
}

rp_cran_repository <- function(root = ".") {
  values <- rp_read_lines_config(file.path(root, "dependencies", "cran-snapshot.txt"))
  if (length(values) != 1L) {
    stop("dependencies/cran-snapshot.txt must declare exactly one repository URL.",
         call. = FALSE)
  }
  values[[1L]]
}

rp_declared_packages <- function(root = ".", scopes = NULL) {
  table <- utils::read.csv(
    file.path(root, "dependencies", "r-packages.csv"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  if (!is.null(scopes)) {
    table <- table[table$scope %in% scopes, , drop = FALSE]
  }
  table
}

# Atomic write: build the file beside its destination and rename it into place
# so an interrupted run can never leave a half-written checkpoint that a resumed
# run would treat as valid.
rp_write_atomic <- function(path, writer) {
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = paste0(".", basename(path), "."), tmpdir = directory
  )
  on.exit({
    if (file.exists(temporary)) unlink(temporary)
  }, add = TRUE)
  writer(temporary)
  if (!file.exists(temporary)) {
    stop("Writer produced no file for ", path, call. = FALSE)
  }
  if (!file.rename(temporary, path)) {
    stop("Could not move temporary file into place: ", path, call. = FALSE)
  }
  invisible(path)
}

rp_write_csv_atomic <- function(data, path) {
  rp_write_atomic(path, function(temporary) {
    utils::write.csv(data, temporary, row.names = FALSE)
  })
}

rp_write_lines_atomic <- function(lines, path) {
  rp_write_atomic(path, function(temporary) {
    writeLines(lines, temporary, useBytes = TRUE)
  })
}

rp_save_rds_atomic <- function(object, path, compress = "gzip") {
  rp_write_atomic(path, function(temporary) {
    saveRDS(object, temporary, compress = compress)
  })
}

rp_manifest_rows <- function(paths, role = NA_character_, note = NA_character_,
                             root = ".") {
  paths <- unique(paths[file.exists(paths)])
  if (!length(paths)) {
    return(data.frame(
      path = character(), bytes = numeric(), sha256 = character(),
      role = character(), note = character(), stringsAsFactors = FALSE
    ))
  }
  normalized_root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  relative <- vapply(paths, function(path) {
    absolute <- normalizePath(path, winslash = "/", mustWork = FALSE)
    sub(paste0("^", normalized_root, "/"), "", absolute)
  }, character(1), USE.NAMES = FALSE)
  data.frame(
    path = relative,
    bytes = as.numeric(file.info(paths)$size),
    sha256 = vapply(paths, rp_sha256, character(1), USE.NAMES = FALSE),
    role = rep(role, length(paths)),
    note = rep(note, length(paths)),
    stringsAsFactors = FALSE
  )
}

rp_write_manifest <- function(rows, path) {
  rows <- rows[order(rows$path), , drop = FALSE]
  rownames(rows) <- NULL
  rp_write_csv_atomic(rows, path)
  invisible(rows)
}

rp_verify_manifest <- function(manifest, root = ".", label = "manifest") {
  failures <- character()
  for (index in seq_len(nrow(manifest))) {
    path <- file.path(root, manifest$path[[index]])
    if (!file.exists(path)) {
      failures <- c(failures, paste0("missing: ", manifest$path[[index]]))
      next
    }
    observed <- rp_sha256(path)
    expected <- manifest$sha256[[index]]
    if (!is.na(expected) && nzchar(expected) && !identical(observed, expected)) {
      failures <- c(failures, sprintf(
        "checksum mismatch: %s (expected %s, observed %s)",
        manifest$path[[index]], expected, observed
      ))
    }
  }
  if (length(failures)) {
    stop(label, " verification failed:\n  ", paste(failures, collapse = "\n  "),
         call. = FALSE)
  }
  invisible(TRUE)
}

rp_git_commit <- function(root = ".") {
  value <- Sys.getenv("GITHUB_SHA", "")
  if (nzchar(value)) return(value)
  result <- tryCatch(
    system2("git", c("-C", root, "rev-parse", "HEAD"),
            stdout = TRUE, stderr = FALSE),
    error = function(error) character()
  )
  if (length(result) == 1L && nzchar(result)) result else NA_character_
}

rp_environment_record <- function(root = ".") {
  packages <- rp_declared_packages(root)$package
  installed <- vapply(packages, function(package) {
    if (requireNamespace(package, quietly = TRUE)) {
      as.character(utils::packageVersion(package))
    } else {
      NA_character_
    }
  }, character(1))
  data.frame(
    package = packages,
    version = unname(installed),
    available = !is.na(unname(installed)),
    stringsAsFactors = FALSE
  )
}

rp_write_session_record <- function(directory, root = ".") {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  rp_write_lines_atomic(
    c(
      utils::capture.output(utils::sessionInfo()),
      "",
      "# Declared package versions",
      utils::capture.output(print(rp_environment_record(root), row.names = FALSE))
    ),
    file.path(directory, "sessionInfo.txt")
  )
  invisible(file.path(directory, "sessionInfo.txt"))
}

# ---------------------------------------------------------------------------
# Run records: seeds, checkpoints, and the machine the run happened on.
# ---------------------------------------------------------------------------

# Every random seed the analysis code sets, read out of the source rather than
# copied by hand, so the registry cannot drift away from the scripts. Seeds that
# are passed in rather than written as literals are recorded as derived, with the
# argument expression, because that is the honest description of them.
rp_seed_registry <- function(root = ".",
                             directories = c("R", "scripts", "validation")) {
  present <- directories[dir.exists(file.path(root, directories))]
  files <- list.files(
    file.path(root, present), pattern = "\\.R$", recursive = TRUE,
    full.names = TRUE
  )
  rows <- list()
  for (file in sort(files)) {
    relative <- sub(
      paste0("^", normalizePath(root, winslash = "/", mustWork = FALSE), "/"),
      "", normalizePath(file, winslash = "/", mustWork = FALSE)
    )
    lines <- readLines(file, warn = FALSE)
    for (index in seq_along(lines)) {
      line <- lines[[index]]
      seed_call <- regmatches(
        line, regexpr("set\\.seed\\([^)]*\\)", line, perl = TRUE)
      )
      if (length(seed_call)) {
        expression <- sub("^set\\.seed\\(", "", seed_call[[1L]])
        expression <- sub("\\)$", "", expression)
        literal <- grepl("^[0-9]+L?$", trimws(expression))
        rows[[length(rows) + 1L]] <- data.frame(
          script = relative, line = index, kind = "set_seed",
          expression = trimws(expression),
          value = if (literal) {
            sub("L$", "", trimws(expression))
          } else {
            NA_character_
          },
          resolution = if (literal) "literal" else "derived",
          stringsAsFactors = FALSE
        )
      }
      default_call <- regmatches(
        line, regexpr('"--seed",\\s*"[^"]*"', line, perl = TRUE)
      )
      if (length(default_call)) {
        value <- sub('^"--seed",\\s*"', "", default_call[[1L]])
        value <- sub('"$', "", value)
        rows[[length(rows) + 1L]] <- data.frame(
          script = relative, line = index, kind = "seed_argument_default",
          expression = default_call[[1L]], value = value,
          resolution = "literal", stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) {
    return(data.frame(
      script = character(), line = integer(), kind = character(),
      expression = character(), value = character(), resolution = character(),
      stringsAsFactors = FALSE
    ))
  }
  registry <- do.call(rbind, rows)
  rownames(registry) <- NULL
  registry
}

# Checkpoints are what make a long run restartable, so a completed run has to be
# able to say which ones existed, what they hash to, and which specification
# they were written under. A checkpoint whose recorded specification no longer
# matches the code is reported here as stale rather than being trusted.
rp_checkpoint_manifest <- function(roots = "results", inspect = TRUE) {
  directories <- unlist(lapply(roots, function(root) {
    if (!dir.exists(root)) return(character())
    list.dirs(root, recursive = TRUE, full.names = TRUE)
  }), use.names = FALSE)
  directories <- directories[basename(directories) == "checkpoints"]
  files <- unlist(lapply(directories, function(directory) {
    list.files(directory, pattern = "\\.rds$", full.names = TRUE)
  }), use.names = FALSE)
  if (!length(files)) {
    return(data.frame(
      path = character(), bytes = numeric(), sha256 = character(),
      stage_output = character(), model = character(),
      analysis_spec_version = character(), n_draws = integer(),
      modified_utc = character(), stringsAsFactors = FALSE
    ))
  }
  files <- sort(files)
  describe <- function(path) {
    fields <- list(
      model = NA_character_, analysis_spec_version = NA_character_,
      n_draws = NA_integer_
    )
    if (!inspect) return(fields)
    object <- tryCatch(readRDS(path), error = function(error) NULL)
    if (is.null(object) || !is.list(object)) return(fields)
    if (!is.null(object$model)) fields$model <- as.character(object$model)[[1L]]
    if (!is.null(object$analysis_spec_version)) {
      fields$analysis_spec_version <-
        as.character(object$analysis_spec_version)[[1L]]
    }
    if (!is.null(object$draws) && !is.null(ncol(object$draws))) {
      fields$n_draws <- ncol(object$draws)
    }
    fields
  }
  described <- lapply(files, describe)
  data.frame(
    path = files,
    bytes = as.numeric(file.info(files)$size),
    sha256 = vapply(files, rp_sha256, character(1), USE.NAMES = FALSE),
    stage_output = basename(dirname(dirname(files))),
    model = vapply(described, function(x) x$model, character(1)),
    analysis_spec_version = vapply(
      described, function(x) x$analysis_spec_version, character(1)
    ),
    n_draws = vapply(described, function(x) as.integer(x$n_draws), integer(1)),
    modified_utc = format(
      file.info(files)$mtime, tz = "UTC", usetz = TRUE
    ),
    stringsAsFactors = FALSE
  )
}

# The machine and toolchain the run actually happened on. Recorded verbatim so a
# later run that produces different numbers can be compared against it rather
# than argued about.
rp_workflow_environment_lines <- function(root = ".") {
  shell <- function(command, arguments) {
    output <- tryCatch(
      suppressWarnings(system2(command, arguments, stdout = TRUE, stderr = FALSE)),
      error = function(error) character()
    )
    if (!length(output)) "unavailable" else output
  }
  blas <- tryCatch(sessionInfo()$BLAS, error = function(error) NULL)
  lapack <- tryCatch(sessionInfo()$LAPACK, error = function(error) NULL)
  github <- Sys.getenv(c(
    "GITHUB_REPOSITORY", "GITHUB_REF_NAME", "GITHUB_SHA", "GITHUB_RUN_ID",
    "GITHUB_RUN_ATTEMPT", "GITHUB_WORKFLOW", "RUNNER_OS", "RUNNER_ARCH",
    "RUNNER_NAME", "ImageOS", "ImageVersion"
  ))
  c(
    "# Workflow environment",
    paste0("recorded_utc: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    "",
    "## Continuous integration",
    if (any(nzchar(github))) {
      paste0(names(github), ": ", ifelse(nzchar(github), github, "unset"))
    } else {
      "not running under GitHub Actions"
    },
    "",
    "## Operating system",
    shell("uname", c("-a")),
    shell("lsb_release", c("-ds")),
    "",
    "## Processors and memory",
    shell("nproc", character()),
    shell("free", c("-h")),
    "",
    "## Disk",
    shell("df", c("-h", ".")),
    "",
    "## R toolchain",
    paste0("R: ", R.version.string),
    paste0("platform: ", R.version$platform),
    paste0("locale: ", Sys.getlocale()),
    paste0("timezone: ", Sys.timezone()),
    paste0("BLAS: ", if (is.null(blas)) "unknown" else blas),
    paste0("LAPACK: ", if (is.null(lapack)) "unknown" else lapack),
    paste0("CRAN snapshot: ", tryCatch(
      rp_cran_repository(root), error = function(error) "undeclared"
    )),
    paste0("INLA: ", if (requireNamespace("INLA", quietly = TRUE)) {
      as.character(utils::packageVersion("INLA"))
    } else {
      "not installed"
    }),
    "",
    "## Declared system libraries",
    tryCatch(
      rp_read_lines_config(file.path(root, "dependencies", "apt-packages.txt")),
      error = function(error) "undeclared"
    )
  )
}
