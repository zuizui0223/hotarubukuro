args <- commandArgs(trailingOnly = TRUE)
source("R/broad_spatial_inertia_environment_tracking.R")

arg_value <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", name, "="), "", hit[[1]])
}

scores_path <- arg_value(
  "--scores",
  "results/broad_spatial_inertia_environment_tracking/fold_model_scores.csv"
)
output_dir <- arg_value(
  "--output", "results/broad_spatial_inertia_environment_tracking"
)
if (!file.exists(scores_path)) {
  stop(
    "Missing fold-model score table: ", scores_path,
    ". Generate geographically blocked held-out scores for the four predefined ",
    "models (null, environment, space, environment_plus_space) first.",
    call. = FALSE
  )
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
scores <- utils::read.csv(scores_path, stringsAsFactors = FALSE)
summary_table <- broad_tracking_summarise(scores)
interpretation <- broad_tracking_interpretation(summary_table)
utils::write.csv(
  summary_table, file.path(output_dir, "shapley_predictive_decomposition.csv"),
  row.names = FALSE
)
utils::write.csv(
  interpretation, file.path(output_dir, "component_interpretation.csv"),
  row.names = FALSE
)

cat("Broad spatial-inertia/environment-tracking decomposition complete.\n")
print(summary_table)
print(interpretation)
