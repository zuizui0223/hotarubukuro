#!/usr/bin/env Rscript

# Run the standard JBI figure validation, then assert that Figure 2's
# observation-level panels use the finalized response-specific Broad models.

source("validation/validate_jbi_figure_bundle.R")

fixed_path <- "reproducibility/broad_environment_spatial_final_fixed_effects_2026-08-11.csv"
hyper_path <- "reproducibility/broad_environment_spatial_final_hyperparameters_2026-08-11.csv"
used_fixed_path <- file.path(output_dir, "figure_data", "figure2_final_observation_fixed_effects.csv")
used_range_path <- file.path(output_dir, "figure_data", "figure2_final_observation_spatial_ranges.csv")
for (path in c(fixed_path, hyper_path, used_fixed_path, used_range_path)) {
  if (!file.exists(path) || file.info(path)$size <= 0) stop("Missing final Broad Figure 2 lock: ", path, call. = FALSE)
}

fixed_lock <- hb_read_csv(fixed_path)
hyper_lock <- hb_read_csv(hyper_path)
fixed_used <- hb_read_csv(used_fixed_path)
range_used <- hb_read_csv(used_range_path)

lookup_fixed <- function(model, term, field = "mean") {
  hit <- fixed_lock$model == model & fixed_lock$term == term
  if (sum(hit) != 1L) stop("Expected one fixed-effect lock row for ", model, " / ", term, call. = FALSE)
  as.numeric(fixed_lock[[field]][hit])
}
lookup_range <- function(model, field = "mean") {
  hit <- hyper_lock$model == model & hyper_lock$hyperparameter == "Range for spatial"
  if (sum(hit) != 1L) stop("Expected one spatial-range lock row for ", model, call. = FALSE)
  as.numeric(hyper_lock[[field]][hit])
}

expected <- c(
  state_temperature = -0.5418496090784,
  intensity_temperature = -0.083741191947274,
  intensity_precipitation = -0.174117451265804,
  intensity_topography = -0.133731126333632,
  intensity_temperature_x_seasonality = -0.204233616042013,
  state_range_km = 132.755723,
  intensity_range_km = 65.717848
)
observed <- c(
  state_temperature = lookup_fixed("presence_N_environment_space", "env_Temperature_PC1"),
  intensity_temperature = lookup_fixed("intensity_N_environment_space", "env_Temperature_PC1"),
  intensity_precipitation = lookup_fixed("intensity_N_environment_space", "env_precip_PC1"),
  intensity_topography = lookup_fixed("intensity_N_environment_space", "env_topo_PC1"),
  intensity_temperature_x_seasonality = lookup_fixed(
    "intensity_N_environment_space",
    "env_Temperature_PC1_x_TemperatureSeasonality"
  ),
  state_range_km = lookup_range("presence_N_environment_space"),
  intensity_range_km = lookup_range("intensity_N_environment_space")
)
tolerance <- c(rep(1e-10, 5), 1e-6, 1e-6)
if (any(abs(observed - expected) > tolerance)) {
  stop(
    "Final Broad numerical lock mismatch: ",
    paste(names(expected), observed, expected, sep = "=", collapse = "; "),
    call. = FALSE
  )
}

# Confirm the panel data written after adapter execution include the retained
# interaction only for intensity and the final response-specific ranges.
used_interaction <- fixed_used$response == "Pigmented-only intensity" &
  as.character(fixed_used$term) == "env_Temperature_PC1_x_TemperatureSeasonality"
if (sum(used_interaction) != 1L) {
  stop("Figure 2A does not contain exactly one retained thermal interaction row.", call. = FALSE)
}
if (abs(as.numeric(fixed_used$mean[used_interaction]) - expected[["intensity_temperature_x_seasonality"]]) > 1e-10) {
  stop("Figure 2A thermal interaction value does not match the final lock.", call. = FALSE)
}

range_check <- setNames(as.numeric(range_used$mean), as.character(range_used$response))
if (!all(c("Pigmentation state", "Pigmented-only intensity") %in% names(range_check))) {
  stop("Figure 2D lacks one or both finalized response ranges.", call. = FALSE)
}
if (abs(range_check[["Pigmentation state"]] - expected[["state_range_km"]]) > 1e-6 ||
    abs(range_check[["Pigmented-only intensity"]] - expected[["intensity_range_km"]]) > 1e-6) {
  stop("Figure 2D range values do not match the final lock.", call. = FALSE)
}

sources <- hb_read_csv(source_path)
needed_sources <- c(fixed_path, hyper_path)
if (!all(needed_sources %in% as.character(sources$path))) {
  stop("Figure source manifest does not include both final Broad lock files.", call. = FALSE)
}

extra <- data.frame(
  check = c("figure2_final_broad_fixed_effects", "figure2_final_broad_spatial_ranges"),
  status = c("PASS", "PASS"),
  detail = c(
    sprintf("thermal interaction=%.6f; precipitation=%.6f; topography=%.6f", observed[["intensity_temperature_x_seasonality"]], observed[["intensity_precipitation"]], observed[["intensity_topography"]]),
    sprintf("state=%.3f km; intensity=%.3f km", observed[["state_range_km"]], observed[["intensity_range_km"]])
  ),
  stringsAsFactors = FALSE
)
validation_path <- file.path(output_dir, "figure_bundle_validation.csv")
validation_current <- hb_read_csv(validation_path)
utils::write.csv(rbind(validation_current, extra), validation_path, row.names = FALSE)
cat("Final Broad Figure 2 lock passed.\n")
