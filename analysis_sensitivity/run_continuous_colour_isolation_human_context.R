#!/usr/bin/env Rscript

# The analysis is split into three reviewable stages. They intentionally share
# one execution environment so the frozen inputs and derived geometry are read
# once and reused without serialization or hidden state.
source(
  "analysis_sensitivity/continuous_colour_isolation/01_observed_geometry.R",
  local = .GlobalEnv
)
source(
  "analysis_sensitivity/continuous_colour_isolation/02_natural_map_guardrail.R",
  local = .GlobalEnv
)
source(
  "analysis_sensitivity/continuous_colour_isolation/03_reporting.R",
  local = .GlobalEnv
)
