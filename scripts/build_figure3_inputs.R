# Minimal path through the same QC, spatial extraction, mixture and cell builders
# used by the publication pipeline. Recompute every flower response from RGB.
args <- commandArgs(TRUE)
stopifnot(length(args) == 4L)
source("R/environment_spatial.R")
source("R/phenotype_hurdle.R")
source("R/analysis_cells.R")
raw <- read.csv(args[1], stringsAsFactors = FALSE)
environment <- read.csv(args[2], stringsAsFactors = FALSE)
match_row <- match(environment$observation_id, raw$observation_id)
stopifnot(!anyNA(match_row), !anyDuplicated(environment$observation_id),
          all(abs(environment$latitude - raw$latitude[match_row]) < 1e-8),
          all(abs(environment$longitude - raw$longitude[match_row]) < 1e-8))
data <- make_analysis_data(environment, raw, args[3])$data
measurement <- fit_pigmentation_measurement(data$colour_a, data$colour_L, data$colour_C)
data <- cbind(data, measurement$observations)
cells <- ac_population_table(data)
dir.create(args[4], recursive = TRUE, showWarnings = FALSE)
write.csv(data, file.path(args[4], "observations.csv"), row.names = FALSE)
write.csv(cells, file.path(args[4], "cells.csv"), row.names = FALSE)
