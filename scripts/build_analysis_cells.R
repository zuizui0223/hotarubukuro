#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/analysis_cells.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

input_path <- arg_value(
  "--input",
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
)
cache_root <- arg_value("--environment-cache", Sys.getenv("HOTARUBUKURO_PUBLIC_CACHE", ""))
output_dir <- arg_value("--output", "results/analysis_cells")

if (!file.exists(input_path)) stop("Missing phenotype input: ", input_path, call. = FALSE)
if (!dir.exists(cache_root)) stop("Missing environment cache: ", cache_root, call. = FALSE)
hb_require_stage_packages("analysis_cells")

observations <- utils::read.csv(input_path, check.names = FALSE, stringsAsFactors = FALSE)
cells <- ac_population_table(observations, cell_km = 1)
cells <- ac_add_bombus_rank_support(cells)
phenotype <- ac_cell_phenotypes(observations, cells)
context <- ac_add_local_environment_context(
  phenotype$cells, cache_root = cache_root, radius_km = 50
)
cells <- context$cells

n_pigmented_cells <- sum(as.numeric(cells$n_pigmented) > 0)
n_white_cells <- sum(as.numeric(cells$n_pigmented) == 0)
if (nrow(cells) != 1305L || n_pigmented_cells != 674L || n_white_cells != 631L) {
  stop(
    "Frozen analysis-cell geometry changed: cells=", nrow(cells),
    ", pigmented=", n_pigmented_cells, ", white=", n_white_cells,
    call. = FALSE
  )
}

required <- c(
  "exact_site_id", "longitude", "latitude", "x_km", "y_km", "spatial_fold",
  "n_observations", "n_pigmented", "n_white", "pigment_share",
  "conditional_intensity_median", "n_independent_sites",
  "env_Temperature_PC1", "env_precip_PC1", "env_TemperatureSeasonality",
  "env_PrecipSeasonality", "env_topo_PC1", "env_soil_PC1", "env_soil_PC2",
  "env_RSDS", "broad50km_pc1", "broad50km_pc2", "within50km_pc1",
  "within50km_pc2", "bombus_fingerprint_common_support"
)
ac_require_columns(cells, required, "final analysis cells")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  cells, file.path(output_dir, "analysis_cells_1km.csv"), row.names = FALSE
)
utils::write.csv(
  phenotype$exact_sites,
  file.path(output_dir, "analysis_exact_site_traits.csv"), row.names = FALSE
)
utils::write.csv(
  context$context,
  file.path(output_dir, "local_environment_context_50km.csv"), row.names = FALSE
)
utils::write.csv(
  context$provenance,
  file.path(output_dir, "local_environment_provenance.csv"), row.names = FALSE
)
utils::write.csv(
  context$loadings,
  file.path(output_dir, "local_environment_pca_loadings.csv"), row.names = FALSE
)
utils::write.csv(
  data.frame(
    field = c(
      "analysis_unit", "cell_size_km", "cells", "pigmented_cells", "white_cells",
      "purpose", "excluded_legacy_products"
    ),
    value = c(
      "fixed 1-km flower cells", 1, nrow(cells), n_pigmented_cells, n_white_cells,
      "shared input for Broad spatial-null, local Bombus and continuous isolation analyses",
      "hotspot CV; candidate ranking; horticulture facets; community-turnover candidate machinery"
    ),
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "analysis_cell_contract.csv"), row.names = FALSE
)
cat("Built frozen final analysis-cell table: ", nrow(cells), " cells.\n", sep = "")
