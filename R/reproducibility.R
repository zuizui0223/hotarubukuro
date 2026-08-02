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
