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
  canonical = c("analysis", "reproducibility", "testing", "figures", "reporting"),
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
# Geospatial and figure-device smoke tests.
#
# sf and terra are the two packages whose system-library coupling actually
# breaks on a runner, and the figure devices fail only when a font or raster
# backend is missing. Exercising them here costs seconds.
# ---------------------------------------------------------------------------
if ("analysis" %in% required_scopes) {
  geo <- tryCatch({
    points <- sf::st_as_sf(
      data.frame(longitude = c(137, 138), latitude = c(35, 36)),
      coords = c("longitude", "latitude"), crs = 4326
    )
    projected <- sf::st_transform(
      points,
      "+proj=laea +lat_0=36 +lon_0=137 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
    )
    stopifnot(all(is.finite(sf::st_coordinates(projected))))
    raster <- terra::rast(
      nrows = 10, ncols = 10, xmin = 137, xmax = 138, ymin = 35, ymax = 36,
      crs = "EPSG:4326"
    )
    terra::values(raster) <- seq_len(100)
    extracted <- terra::extract(raster, terra::vect(points), method = "bilinear")
    stopifnot(nrow(extracted) == 2L)
    "PASS"
  }, error = function(error) {
    failures <<- c(failures, paste0(
      "sf/terra smoke test failed: ", conditionMessage(error)
    ))
    "FAIL"
  })
  audit_lines <- c(
    audit_lines, "## Geospatial smoke test", "", paste0("status: ", geo),
    paste0("gdal: ", terra::gdal()), paste0("proj: ", sf::sf_extSoftVersion()[["PROJ"]]),
    ""
  )
}

if ("figures" %in% required_scopes) {
  figures <- tryCatch({
    plot <- ggplot2::ggplot(
      data.frame(x = 1:3, y = 1:3), ggplot2::aes(x = x, y = y)
    ) + ggplot2::geom_point()
    target <- tempfile(fileext = ".png")
    ggplot2::ggsave(target, plot, width = 2, height = 2, dpi = 72)
    stopifnot(file.exists(target), file.info(target)$size > 0)
    pdf_target <- tempfile(fileext = ".pdf")
    ggplot2::ggsave(
      pdf_target, plot, width = 2, height = 2, device = grDevices::cairo_pdf
    )
    stopifnot(file.exists(pdf_target), file.info(pdf_target)$size > 0)
    # rnaturalearth resolves medium-scale geometry from the rnaturalearthdata
    # package rather than the network. If that package were missing the figure
    # stage would try to download at run time.
    japan <- rnaturalearth::ne_countries(
      scale = "medium", country = "Japan", returnclass = "sf"
    )
    stopifnot(nrow(japan) >= 1L)
    "PASS"
  }, error = function(error) {
    failures <<- c(failures, paste0(
      "figure device smoke test failed: ", conditionMessage(error)
    ))
    "FAIL"
  })
  audit_lines <- c(
    audit_lines, "## Figure device smoke test", "",
    paste0("status: ", figures), ""
  )
}

# ---------------------------------------------------------------------------
# Runner resources. Recorded rather than enforced: the canonical pipeline holds
# several 1000-draw matrices in memory and writes large checkpoints, and a
# resource ceiling is a far more legible failure than an OOM kill hours later.
# ---------------------------------------------------------------------------
resource_lines <- tryCatch({
  c(
    utils::capture.output(
      cat(system2("df", c("-h", "."), stdout = TRUE), sep = "\n")
    ),
    utils::capture.output(
      cat(system2("free", "-h", stdout = TRUE), sep = "\n")
    )
  )
}, error = function(error) paste("resource probe unavailable:", conditionMessage(error)))
audit_lines <- c(audit_lines, "## Runner resources", "", resource_lines, "")

# ---------------------------------------------------------------------------
# Stage-registry preflight: every registered generating, validation, and audit
# script must exist before the pipeline starts.
# ---------------------------------------------------------------------------
registry_path <- file.path("reproducibility", "pipeline_stage_registry.csv")
if (file.exists(registry_path)) {
  registry <- utils::read.csv(
    registry_path, check.names = FALSE, stringsAsFactors = FALSE
  )
  scripts <- unique(unlist(
    registry[c("generating_script", "validation_script", "audit_script")],
    use.names = FALSE
  ))
  scripts <- scripts[!is.na(scripts) & nzchar(scripts)]
  absent <- scripts[!file.exists(scripts)]
  if (length(absent)) {
    failures <- c(failures, paste0(
      "stage registry references missing scripts: ",
      paste(absent, collapse = ", ")
    ))
  }
  audit_lines <- c(
    audit_lines, "## Stage registry", "",
    paste0("stages: ", nrow(registry)),
    paste0("referenced scripts present: ", length(scripts) - length(absent),
           "/", length(scripts)),
    ""
  )
} else if (scope == "canonical") {
  failures <- c(failures, paste0("stage registry not found: ", registry_path))
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
