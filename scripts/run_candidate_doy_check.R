# Supplementary: local flowering-date difference for the fixed candidate set.
#
# Runs after the candidates and their matched controls exist, and changes
# neither. See R/candidate_doy_check.R for what this does and does not claim.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/candidate_null_tools.R")
source("R/local_pigmented_isolates.R")
source("R/candidate_doy_check.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
observations_path <- arg_value(
  "--observations",
  paste0(
    "results/ecological_v11_pigmentation_hurdle/",
    "analysis_data_pigmentation_hurdle.csv"
  )
)
isolate_dir <- arg_value(
  "--isolates", "results/ecological_v20_local_white_isolates"
)
output_dir <- arg_value(
  "--output", "results/ecological_v24_candidate_doy_check"
)

candidates_path <- file.path(isolate_dir, "local_isolate_candidates.csv")
pairs_path <- file.path(isolate_dir, "local_isolate_observed_pairs.csv")
for (path in c(
    cells_path, observations_path, candidates_path, pairs_path
  )) {
  if (!file.exists(path)) {
    stop(
      "Missing required input: ", path,
      ". The candidate set is produced by the local-isolate stage; this ",
      "supplementary check never regenerates it.", call. = FALSE
    )
  }
}

cells <- utils::read.csv(
  cells_path, check.names = FALSE, stringsAsFactors = FALSE
)
observations <- utils::read.csv(
  observations_path, check.names = FALSE, stringsAsFactors = FALSE
)
candidates <- utils::read.csv(
  candidates_path, check.names = FALSE, stringsAsFactors = FALSE
)
pairs <- utils::read.csv(
  pairs_path, check.names = FALSE, stringsAsFactors = FALSE
)
cell_year <- v24_cell_year_table(observations)

# The same neighbourhood definition the candidate stage locked: 10 km, an
# environmental caliper of 1, at least three neighbours. Rebuilt here rather
# than passed in, so this stage cannot silently drift to a different graph.
configurations <- v20_configuration_table()
primary <- configurations[configurations$role == "primary", , drop = FALSE]
if (nrow(primary) != 1L) {
  stop("Expected exactly one primary local-isolate configuration.", call. = FALSE)
}
message("[v24] rebuilding the locked primary neighbourhood graph")
graph <- v20_neighbour_graph(
  cells,
  radius_km = primary$radius_km,
  environment_caliper = primary$environment_caliper,
  minimum_neighbours = primary$minimum_neighbours,
  same_fold_only = primary$same_fold_only
)

ids <- as.character(cells$exact_site_id)
candidate_index <- match(as.character(candidates$exact_site_id), ids)
if (anyNA(candidate_index)) {
  stop("Candidate identifiers are absent from the cell table.", call. = FALSE)
}
control_index <- match(as.character(pairs$control_id), ids)
control_index <- unique(control_index[!is.na(control_index)])

message(
  "[v24] flowering-date difference for ", length(candidate_index),
  " candidates and ", length(control_index), " matched controls"
)
candidate_result <- v24_doy_difference_table(
  cells, graph, candidate_index, "local_isolate_candidate", cell_year
)
control_result <- v24_doy_difference_table(
  cells, graph, control_index, "matched_control", cell_year
)
differences <- rbind(
  candidate_result$differences, control_result$differences
)
year_detail <- rbind(
  candidate_result$year_detail, control_result$year_detail
)
summary <- v24_doy_summary(differences)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  differences, file.path(output_dir, "candidate_doy_differences.csv"),
  row.names = FALSE
)
utils::write.csv(
  year_detail, file.path(output_dir, "candidate_doy_year_differences.csv"),
  row.names = FALSE
)
utils::write.csv(
  cell_year, file.path(output_dir, "candidate_doy_cell_year_table.csv"),
  row.names = FALSE
)
utils::write.csv(
  summary, file.path(output_dir, "candidate_doy_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  data.frame(
    field = c(
      "analysis_spec_version", "generated_at", "model_fitted",
      "significance_test", "used_for_candidate_selection",
      "used_for_candidate_ranking", "supports_main_claim",
      "neighbourhood_definition", "unrestricted_summary",
      "year_restriction"
    ),
    value = c(
      v24_analysis_spec_version, as.character(Sys.time()), "none", "none",
      "false", "false", "false",
      paste(
        "locked primary local-isolate graph:",
        primary$radius_km, "km,", "environment caliper",
        primary$environment_caliper, ", at least",
        primary$minimum_neighbours, "neighbours"
      ),
      paste(
        "focal cell median DOY minus the median of neighbour-cell median DOYs"
      ),
      paste(
        "exact observation-year overlap: cell-year median DOYs are compared",
        "within each literal shared year and the focal-cell statistic is the",
        "median of those year-specific differences"
      )
    ),
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "candidate_doy_metadata.csv"),
  row.names = FALSE
)

writeLines(
  c(
    "# v24 supplementary local flowering-date check",
    "",
    paste(
      "For each fixed local-isolate candidate, the unrestricted description is",
      "its cell-level median day-of-year minus the median of its locked",
      "environment-similar neighbours' cell-level medians. Negative means the",
      "candidate was observed flowering earlier. The same description is",
      "reported for matched non-isolated pigmented controls."
    ),
    "",
    paste(
      "The primary temporal safeguard uses exact calendar years from the",
      "observation table. Within every year shared by the focal cell and at",
      "least one locked neighbour, the focal cell-year median DOY is compared",
      "with the median of neighbour-cell DOYs in that same year. The focal-cell",
      "supplementary value is the median of these year-specific differences.",
      "No shared year is reported as missing; it is never replaced by the",
      "unrestricted value."
    ),
    "",
    paste(
      "No model or significance test is fitted. The withdrawn national",
      "phenology component is not restored. Nothing here selects candidates,",
      "ranks them, or supports a main claim; this is a supplementary",
      "description of a candidate set fixed upstream."
    )
  ),
  file.path(output_dir, "README.md"), useBytes = TRUE
)

cat("v24 supplementary DOY check written to ", normalizePath(output_dir), "\n")
print(summary)
