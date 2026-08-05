# Shared interface for the active 1,909-observation analysis.
#
# Only modules named in hb_module_files are loadable by the active pipeline.
# Historical implementations live under legacy/ and are deliberately absent
# from this registry.

hb_package_groups <- list(
  natural_predictive_model = c("INLA", "Matrix", "sf", "terra"),
  bombus_occurrences = c("dplyr", "jsonlite", "readr", "rgbif"),
  human_context = c("foreign", "ranger", "terra"),
  publication_figures = c(
    "cowplot", "ggplot2", "patchwork", "rnaturalearth", "scales", "sf"
  ),
  reporting = c("knitr", "rmarkdown"),
  testing = "testthat"
)

hb_stage_packages <- list(
  natural_predictive_model = "natural_predictive_model",
  local_bombus_turnover = character(),
  human_landscape_features = "human_context",
  local_pigmented_isolates = "human_context",
  local_human_context = "human_context",
  did_sensitivity = "human_context",
  bombus_occurrences = "bombus_occurrences",
  human_raster = "human_context",
  reporting = "reporting",
  publication_figures = "publication_figures",
  full = names(hb_package_groups)
)

hb_module_files <- c(
  natural_predictive_model = "R/natural_predictive_model.R",
  local_bombus_turnover = "R/local_bombus_turnover.R",
  candidate_null_tools = "R/candidate_null_tools.R",
  human_landscape_features = "R/human_landscape_features.R",
  local_pigmented_isolates = "R/local_pigmented_isolates.R",
  human_raster_features = "R/human_raster_features.R",
  local_human_context = "R/local_human_context.R",
  spatial_context = "R/spatial_context.R",
  did_sensitivity = "R/did_sensitivity.R",
  final_registry = "R/final_registry.R"
)

hb_stage_modules <- list(
  human_raster = "human_raster_features",
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
    "local_human_context", "spatial_context"
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
    stop("Unknown active package stage: ", stage, call. = FALSE)
  }
  hb_require_packages(unique(unlist(hb_package_groups[groups], use.names = FALSE)))
}

hb_load_modules <- function(stage, envir = parent.frame(), root = ".") {
  modules <- hb_stage_modules[[stage]]
  if (is.null(modules)) {
    stop("Unknown active module stage: ", stage, call. = FALSE)
  }
  paths <- file.path(root, unname(hb_module_files[modules]))
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "Missing active modules: ", paste(missing, collapse = ", "),
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
