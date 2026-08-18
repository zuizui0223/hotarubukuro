#!/usr/bin/env Rscript

# The analysis is split into three reviewable stages. They intentionally share
# one execution environment so the frozen inputs and derived geometry are read
# once and reused without serialization or hidden state.
#
# Stage 01 sources the public helper module itself. Intercept that one source
# call so the runtime-equivalent high-repetition rho helpers are installed
# immediately afterwards, before any permutation loop begins.
base_source <- base::source
source <- function(file, ...) {
  result <- base_source(file, ...)
  if (identical(file, "R/continuous_colour_isolation.R")) {
    base_source(
      "analysis_sensitivity/continuous_colour_isolation/00_fast_overrides.R",
      local = .GlobalEnv
    )
  }
  invisible(result)
}
base_source(
  "analysis_sensitivity/continuous_colour_isolation/01_observed_geometry.R",
  local = .GlobalEnv
)
rm(source, envir = .GlobalEnv)
base_source(
  "analysis_sensitivity/continuous_colour_isolation/02_natural_map_guardrail.R",
  local = .GlobalEnv
)
base_source(
  "analysis_sensitivity/continuous_colour_isolation/03_reporting.R",
  local = .GlobalEnv
)
