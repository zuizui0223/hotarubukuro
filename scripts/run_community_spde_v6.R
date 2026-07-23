args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1L) args[1] else
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
output_dir <- if (length(args) >= 2L) args[2] else
  "results/ecological_v13_community_threshold"

source("scripts/ecological_analysis_v2.R")
source("scripts/transition_zone_design_v5.R")
source("scripts/community_threshold_v6.R")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
data <- utils::read.csv(input, check.names = FALSE, stringsAsFactors = FALSE)
env_terms <- community_environment_terms(data)
result <- community_fit_spde_foundation(data, env_terms)

write.csv(
  result$metrics, file.path(output_dir, "community_spde_model_comparison.csv"),
  row.names = FALSE
)
write.csv(
  result$fixed, file.path(output_dir, "community_spde_fixed_effects.csv"),
  row.names = FALSE
)
write.csv(
  result$hyper, file.path(output_dir, "community_spde_hyperparameters.csv"),
  row.names = FALSE
)
writeLines(
  c(
    paste0("status=", result$status),
    paste0("environment_terms=", paste(env_terms, collapse = ",")),
    "region_fixed_effect=false",
    "Bombus_fixed_effect=false",
    "white_a_star_used_for_intensity=false"
  ),
  file.path(output_dir, "community_spde_run_status.txt")
)

metadata_path <- file.path(output_dir, "community_analysis_metadata.csv")
if (file.exists(metadata_path)) {
  metadata <- utils::read.csv(
    metadata_path, check.names = FALSE, stringsAsFactors = FALSE
  )
  row <- match("spde_status", metadata$field)
  if (is.na(row)) {
    metadata <- rbind(
      metadata,
      data.frame(field = "spde_status", value = result$status)
    )
  } else {
    metadata$value[row] <- result$status
  }
  write.csv(metadata, metadata_path, row.names = FALSE)
}
print(result$metrics)
