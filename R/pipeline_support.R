# Shared interface for the current JBI paper analysis.
#
# Only modules named in hb_module_files are loadable through the current paper
# pipeline. Historical implementations live under legacy/ and are deliberately
# absent from this registry. The current source boundary is the curated
# 1,965-row Data_S1 table; downstream sample sizes are determined by current
# QC/support rules rather than a fixed historical n.

hb_configure_deterministic_compute <- function() {
  thread_variables <- c(
    "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS", "GOTO_NUM_THREADS",
    "BLIS_NUM_THREADS", "INLA_NUM_THREADS"
  )
  values <- as.list(rep("1", length(thread_variables)))
  names(values) <- thread_variables
  do.call(Sys.setenv, values)
  Sys.setenv(OMP_DYNAMIC = "FALSE")
  options(mc.cores = 1L)
  RNGkind(
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  r_seed <- suppressWarnings(as.integer(Sys.getenv(
    "HOTARUBUKURO_R_SEED", "20260725"
  )))
  if (length(r_seed) != 1L || !is.finite(r_seed) || r_seed <= 0L) {
    stop("HOTARUBUKURO_R_SEED must be one positive integer.", call. = FALSE)
  }
  set.seed(r_seed)
  if (requireNamespace("INLA", quietly = TRUE)) {
    INLA::inla.setOption(num.threads = "1:1")
  }
  invisible(data.frame(
    field = c(thread_variables, "OMP_DYNAMIC", "R_seed", "RNGkind"),
    value = c(
      rep("1", length(thread_variables)), "FALSE", as.character(r_seed),
      paste(RNGkind(), collapse = ";")
    ),
    stringsAsFactors = FALSE
  ))
}

hb_deterministic_compute <- hb_configure_deterministic_compute()

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
  candidate_null_tools = "R/candidate_null_tools.R",
  human_landscape_features = "R/human_landscape_features.R",
  local_pigmented_isolates = "R/local_pigmented_isolates.R",
  human_raster_features = "R/human_raster_features.R",
  local_human_context = "R/local_human_context.R",
  spatial_context = "R/spatial_context.R",
  did_sensitivity = "R/did_sensitivity.R"
)

hb_stage_modules <- list(
  human_raster = "human_raster_features",
  natural_predictive_model = "natural_predictive_model",
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
  )
)

hb_publication_stage_registry <- function() {
  data.frame(
    stage_id = c(
      "01_phenotype", "02_natural_model", "03_local_bombus",
      "04_candidate_definition", "05_human_context", "06_submission_lock"
    ),
    manuscript_role = c(
      "measurement_model", "broad_natural_template",
      "local_focal_pollinator_transition_test", "candidate_definition",
      "post_selection_human_context", "claim_and_artifact_lock"
    ),
    response = c(
      "pigmentation presence and pigmented-only intensity",
      "same two-part response",
      "white-to-pigmented direction across Bombus-blind 5-km sharp local transitions",
      "repeatable local pigmentation-state events calibrated against natural predictive maps",
      "population, landscape and DID context after candidate fixation",
      "current manuscript claims and reproducibility references"
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
    stop("Unknown current package stage: ", stage, call. = FALSE)
  }
  hb_require_packages(unique(unlist(hb_package_groups[groups], use.names = FALSE)))
}

hb_load_modules <- function(stage, envir = parent.frame(), root = ".") {
  modules <- hb_stage_modules[[stage]]
  if (is.null(modules)) {
    stop("Unknown current module stage: ", stage, call. = FALSE)
  }
  paths <- file.path(root, unname(hb_module_files[modules]))
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "Missing current modules: ", paste(missing, collapse = ", "),
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
