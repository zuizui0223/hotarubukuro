# Preflight checks run before any expensive analysis stage.
#
# Two classes of failure used to surface hours into a run: a package that is
# only loaded indirectly (for example the skew-normal support that
# INLA::inla.posterior.sample pulls in), and an input file that is present on a
# development machine but not on a clean runner. Both are checked here, in
# seconds, before anything expensive starts.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
scope <- arg_value("--scope", "canonical")
report_dir <- arg_value("--report-dir", "reproducibility")
inputs_argument <- arg_value("--inputs", "")
manifest_path <- arg_value("--input-manifest", "")
skip_inla <- hb_as_bool(arg_value("--skip-inla-smoke", "false"))

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

scope_choices <- c("canonical", "raw", "tests")
if (!scope %in% scope_choices) {
  stop("--scope must be one of: ", paste(scope_choices, collapse = ", "),
       call. = FALSE)
}

required_scopes <- switch(
  scope,
  canonical = c("analysis", "reproducibility", "testing"),
  raw = c("analysis", "reproducibility", "acquisition", "testing"),
  tests = c("reproducibility", "testing")
)

audit_lines <- c(
  "# Dependency audit",
  "",
  paste0("scope: ", scope),
  paste0("generated_utc: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("commit: ", rp_git_commit()),
  paste0("r_version: ", paste(R.version$major, R.version$minor, sep = ".")),
  paste0("cran_snapshot: ", rp_cran_repository()),
  ""
)

declared <- rp_declared_packages(scopes = required_scopes)
failures <- character()
package_rows <- list()

for (index in seq_len(nrow(declared))) {
  package <- declared$package[[index]]
  loaded <- tryCatch({
    loadNamespace(package)
    TRUE
  }, error = function(error) {
    failures <<- c(failures, sprintf(
      "%s could not be loaded: %s", package, conditionMessage(error)
    ))
    FALSE
  })
  package_rows[[length(package_rows) + 1L]] <- data.frame(
    package = package,
    scope = declared$scope[[index]],
    loaded = loaded,
    version = if (loaded) as.character(utils::packageVersion(package)) else NA_character_,
    stringsAsFactors = FALSE
  )
}
package_status <- do.call(rbind, package_rows)
audit_lines <- c(
  audit_lines,
  "## Declared namespaces",
  "",
  utils::capture.output(print(package_status, row.names = FALSE)),
  ""
)

# ---------------------------------------------------------------------------
# INLA end-to-end smoke test.
#
# Loading the INLA namespace is not enough: posterior sampling dispatches into
# packages that are only required at call time. Running one tiny model through
# the same call path the analysis uses is what actually proves the environment
# is complete.
# ---------------------------------------------------------------------------
if (!skip_inla && "analysis" %in% required_scopes) {
  message("[preflight] exercising INLA posterior sampling")
  smoke <- tryCatch({
    set.seed(20260725L)
    n <- 40L
    data <- data.frame(
      y = stats::rbinom(n, size = 4L, prob = 0.4),
      Ntrials = rep(4L, n),
      x = stats::rnorm(n),
      idx = seq_len(n)
    )
    fit <- INLA::inla(
      y ~ x + f(idx, model = "iid"),
      family = "binomial", Ntrials = data$Ntrials, data = data,
      control.compute = list(config = TRUE), verbose = FALSE
    )
    samples <- INLA::inla.posterior.sample(
      n = 3L, result = fit, seed = 1L, num.threads = 1,
      parallel.configs = FALSE, add.names = TRUE
    )
    stopifnot(length(samples) == 3L, all(is.finite(samples[[1]]$latent[, 1])))
    "PASS"
  }, error = function(error) {
    failures <<- c(failures, paste0(
      "INLA posterior-sampling smoke test failed: ", conditionMessage(error)
    ))
    "FAIL"
  })
  audit_lines <- c(
    audit_lines,
    "## INLA posterior-sampling smoke test",
    "",
    paste0("status: ", smoke),
    paste0("inla_version: ", as.character(utils::packageVersion("INLA"))),
    ""
  )
}

# ---------------------------------------------------------------------------
# Input presence and checksums.
# ---------------------------------------------------------------------------
input_paths <- character()
if (nzchar(inputs_argument)) {
  input_paths <- trimws(strsplit(inputs_argument, ",", fixed = TRUE)[[1L]])
  input_paths <- input_paths[nzchar(input_paths)]
}

missing_inputs <- input_paths[!file.exists(input_paths)]
if (length(missing_inputs)) {
  failures <- c(failures, vapply(missing_inputs, function(path) sprintf(
    paste0(
      "required input '%s' is absent. It is produced upstream in the pipeline ",
      "or restored from the canonical snapshot; see reproducibility/pipeline_dag.md ",
      "for the stage that generates it. It must never be supplied by hand."
    ), path
  ), character(1), USE.NAMES = FALSE))
}

present_inputs <- input_paths[file.exists(input_paths)]
input_manifest <- rp_manifest_rows(present_inputs, role = "stage_input")
if (nrow(input_manifest)) {
  rp_write_manifest(input_manifest, file.path(report_dir, "input_manifest.csv"))
  audit_lines <- c(
    audit_lines,
    "## Input checksums",
    "",
    utils::capture.output(
      print(input_manifest[c("path", "bytes", "sha256")], row.names = FALSE)
    ),
    ""
  )
}

if (nzchar(manifest_path)) {
  if (!file.exists(manifest_path)) {
    failures <- c(failures, paste0("input manifest not found: ", manifest_path))
  } else {
    expected <- utils::read.csv(
      manifest_path, check.names = FALSE, stringsAsFactors = FALSE
    )
    verified <- tryCatch({
      rp_verify_manifest(expected, label = manifest_path)
      "PASS"
    }, error = function(error) {
      failures <<- c(failures, conditionMessage(error))
      "FAIL"
    })
    audit_lines <- c(
      audit_lines,
      "## Declared input manifest",
      "",
      paste0("manifest: ", manifest_path),
      paste0("entries: ", nrow(expected)),
      paste0("status: ", verified),
      ""
    )
  }
}

audit_lines <- c(
  audit_lines,
  "## Result",
  "",
  if (length(failures)) {
    c("status: FAIL", "", paste0("- ", failures))
  } else {
    "status: PASS"
  }
)
rp_write_lines_atomic(audit_lines, file.path(report_dir, "dependency_audit.txt"))
rp_write_session_record(report_dir)

if (length(failures)) {
  stop(
    "Preflight failed:\n  ", paste(failures, collapse = "\n  "),
    call. = FALSE
  )
}
cat("[preflight] PASS (", nrow(package_status), " namespaces, ",
    length(present_inputs), " inputs)\n", sep = "")
