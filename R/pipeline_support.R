# Shared interface for the publication analysis.
# Superseded hotspot/candidate/DID modules are intentionally not registered.

hb_package_groups <- list(
  phenotype = c(
    "INLA", "jsonlite", "mclust", "mgcv", "qgam", "quantreg", "sf", "terra"
  ),
  analysis_cells = "terra",
  natural_predictive_model = c("INLA", "Matrix", "sf", "terra"),
  bombus_sdm = c("ENMeval", "FNN", "maxnet", "sf", "terra"),
  bombus_occurrences = c("dplyr", "jsonlite", "readr", "rgbif"),
  environment_input = c("jsonlite", "terra"),
  human_raster = c("foreign", "terra"),
  testing = "testthat"
)

hb_stage_packages <- list(
  phenotype = "phenotype",
  analysis_cells = "analysis_cells",
  natural_predictive_model = "natural_predictive_model",
  bombus_occurrences = "bombus_occurrences",
  bombus_sdm = "bombus_sdm",
  environment_input = "environment_input",
  human_raster = "human_raster",
  testing = "testing",
  full = names(hb_package_groups)
)

hb_module_files <- c(
  environment_spatial = "R/environment_spatial.R",
  natural_biotic_covariates = "R/natural_biotic_covariates.R",
  phenotype_hurdle = "R/phenotype_hurdle.R",
  human_raster_features = "R/human_raster_features.R"
)

hb_stage_modules <- list(
  environment_input = "environment_spatial",
  bombus_sdm = "environment_spatial",
  human_raster = "human_raster_features",
  phenotype = c(
    "environment_spatial", "natural_biotic_covariates", "phenotype_hurdle"
  )
)

hb_publication_stage_registry <- function() {
  data.frame(
    stage_id = c(
      "01_phenotype", "02_broad_geography", "03_local_bombus",
      "04_continuous_isolation", "05_result_lock"
    ),
    manuscript_role = c(
      "measurement_model",
      "confirmatory_environment_plus_space_and_spatial_null",
      "scale_matched_local_biotic_test",
      "exploratory_all_cell_human_context",
      "claim_and_artifact_lock"
    ),
    response = c(
      "pigmentation state and pigmented-only conditional intensity",
      "same two-part response",
      "white-pigmented local boundaries and focal Bombus support",
      "same-colour nearest-neighbour isolation versus population exposure",
      "registered results and claim ceilings"
    ),
    stringsAsFactors = FALSE
  )
}

hb_arg_value <- function(args, name, default = NULL) {
  index <- match(name, args)
  if (!is.na(index) && index < length(args)) return(args[[index + 1L]])
  prefix <- paste0(name, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  sub(prefix, "", hit[[1L]], fixed = TRUE)
}

hb_as_bool <- function(value) {
  tolower(as.character(value)) %in% c("1", "true", "yes", "y")
}

hb_or_else <- function(value, default) {
  if (is.null(value) || !length(value)) default else value
}

hb_require_packages <- function(packages) {
  packages <- unique(packages[nzchar(packages)])
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop("Missing R packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(packages)
}

hb_require_stage_packages <- function(stage) {
  groups <- hb_stage_packages[[stage]]
  if (is.null(groups)) stop("Unknown publication package stage: ", stage, call. = FALSE)
  hb_require_packages(unique(unlist(hb_package_groups[groups], use.names = FALSE)))
}

hb_load_modules <- function(stage, envir = parent.frame(), root = ".") {
  modules <- hb_stage_modules[[stage]]
  if (is.null(modules)) stop("Unknown publication module stage: ", stage, call. = FALSE)
  paths <- file.path(root, unname(hb_module_files[modules]))
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop("Missing publication modules: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  for (path in paths) sys.source(path, envir = envir)
  invisible(paths)
}

hb_read_csv <- function(path) {
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

hb_close_enough <- function(x, y, tolerance = 1e-9) {
  length(x) == length(y) &&
    all(
      (is.na(x) & is.na(y)) |
        (is.finite(x) & is.finite(y) & abs(x - y) <= tolerance)
    )
}
