# Shared interface for the publication analysis.
#
# Analysis functions live in stage-specific files under R/. Executable scripts
# source this file and request a paper stage through hb_load_modules(). This
# keeps package declarations, argument parsing, and module ordering in one place.

hb_package_groups <- list(
  phenotype = c(
    "INLA", "jsonlite", "mclust", "mgcv", "qgam", "quantreg", "sf", "terra"
  ),
  multiscale_environment = c("INLA", "mgcv", "sf", "terra"),
  natural_predictive_model = c("INLA", "Matrix", "sf", "terra"),
  bombus_sdm = c("ENMeval", "FNN", "maxnet", "rgbif", "sf", "terra"),
  bombus_occurrences = c("dplyr", "jsonlite", "readr", "rgbif"),
  environment_input = c("jsonlite", "terra"),
  human_context = c("foreign", "ranger", "terra"),
  reporting = c("knitr", "rmarkdown"),
  testing = "testthat"
)

hb_stage_packages <- list(
  phenotype = c("phenotype"),
  multiscale_hotspots = c("multiscale_environment"),
  natural_predictive_model = c("natural_predictive_model"),
  local_bombus_turnover = character(),
  human_landscape_features = c("human_context"),
  local_pigmented_isolates = c("human_context"),
  local_human_context = c("human_context", "multiscale_environment"),
  did_sensitivity = c("human_context"),
  bombus_occurrences = c("bombus_occurrences"),
  bombus_sdm = c("bombus_sdm"),
  environment_input = c("environment_input"),
  human_raster = c("human_context"),
  reporting = c("reporting"),
  full = names(hb_package_groups)
)

hb_module_files <- c(
  environment_spatial = "R/environment_spatial.R",
  natural_biotic_covariates = "R/natural_biotic_covariates.R",
  phenotype_hurdle = "R/phenotype_hurdle.R",
  local_transition_pairs = "R/local_transition_pairs.R",
  hotspot_candidates = "R/hotspot_candidates.R",
  bombus_community_fingerprint = "R/bombus_community_fingerprint.R",
  multiscale_hotspots = "R/multiscale_hotspots.R",
  natural_predictive_model = "R/natural_predictive_model.R",
  local_bombus_turnover = "R/local_bombus_turnover.R",
  candidate_null_tools = "R/candidate_null_tools.R",
  human_landscape_features = "R/human_landscape_features.R",
  local_pigmented_isolates = "R/local_pigmented_isolates.R",
  human_raster_features = "R/human_raster_features.R",
  local_human_context = "R/local_human_context.R",
  did_sensitivity = "R/did_sensitivity.R",
  final_registry = "R/final_registry.R"
)

hb_stage_modules <- list(
  environment_input = "environment_spatial",
  bombus_sdm = "environment_spatial",
  human_raster = "human_raster_features",
  phenotype = c(
    "environment_spatial", "natural_biotic_covariates",
    "phenotype_hurdle"
  ),
  multiscale_hotspots = c(
    "environment_spatial", "local_transition_pairs",
    "hotspot_candidates", "bombus_community_fingerprint",
    "multiscale_hotspots"
  ),
  natural_predictive_model = "natural_predictive_model",
  local_bombus_turnover = "local_bombus_turnover",
  human_landscape_features = c(
    "candidate_null_tools", "human_landscape_features"
  ),
  local_pigmented_isolates = c(
    "candidate_null_tools", "human_landscape_features",
    "local_pigmented_isolates"
  ),
  local_human_context = c(
    "human_raster_features", "candidate_null_tools",
    "human_landscape_features", "local_pigmented_isolates",
    "local_human_context", "multiscale_hotspots"
  ),
  did_sensitivity = c(
    "human_raster_features", "candidate_null_tools",
    "human_landscape_features", "local_pigmented_isolates",
    "local_human_context", "did_sensitivity"
  ),
  final_registry = "final_registry"
)

hb_publication_stage_registry <- function() {
  data.frame(
    stage_id = c(
      "01_phenotype", "02_natural_model", "03_local_bombus",
      "04_candidate_definition", "05_human_context", "06_final_lock"
    ),
    manuscript_role = c(
      "measurement_model", "confirmatory_natural_baseline",
      "planned_local_biotic_test", "candidate_definition",
      "exploratory_human_context", "claim_and_artifact_lock"
    ),
    response = c(
      "pigmentation presence and pigmented-only intensity",
      "same two-part response", "local turnover of both response stages",
      "pigmented isolates among environment-similar white neighbours",
      "population and DID contrasts", "registered results and claim ceilings"
    ),
    stringsAsFactors = FALSE
  )
}

hb_arg_value <- function(args, name, default = NULL) {
  index <- match(name, args)
  if (!is.na(index) && index < length(args)) {
    return(args[[index + 1L]])
  }
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
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(packages)
}

hb_require_stage_packages <- function(stage) {
  groups <- hb_stage_packages[[stage]]
  if (is.null(groups)) {
    stop("Unknown publication package stage: ", stage, call. = FALSE)
  }
  hb_require_packages(unique(unlist(hb_package_groups[groups], use.names = FALSE)))
}

hb_load_modules <- function(stage, envir = parent.frame(), root = ".") {
  modules <- hb_stage_modules[[stage]]
  if (is.null(modules)) {
    stop("Unknown publication module stage: ", stage, call. = FALSE)
  }
  paths <- file.path(root, unname(hb_module_files[modules]))
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "Missing publication modules: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  for (path in paths) {
    sys.source(path, envir = envir)
  }
  invisible(paths)
}

hb_read_csv <- function(path) {
  utils::read.csv(
    path, check.names = FALSE, stringsAsFactors = FALSE
  )
}

hb_close_enough <- function(x, y, tolerance = 1e-9) {
  length(x) == length(y) &&
    all(
      (is.na(x) & is.na(y)) |
        (is.finite(x) & is.finite(y) & abs(x - y) <= tolerance)
    )
}
