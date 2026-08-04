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
isolate_dir <- arg_value(
  "--isolates", "results/ecological_v20_local_white_isolates"
)
output_dir <- arg_value(
  "--output", "results/ecological_v24_candidate_doy_check"
)

candidates_path <- file.path(isolate_dir, "local_isolate_candidates.csv")
pairs_path <- file.path(isolate_dir, "local_isolate_observed_pairs.csv")
for (path in c(cells_path, candidates_path, pairs_path)) {
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
candidates <- utils::read.csv(
  candidates_path, check.names = FALSE, stringsAsFactors = FALSE
)
pairs <- utils::read.csv(
  pairs_path, check.names = FALSE, stringsAsFactors = FALSE
)

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
control_index <- control_index[!is.na(control_index)]

message(
  "[v24] flowering-date difference for ", length(candidate_index),
  " candidates and ", length(control_index), " matched controls"
)
differences <- rbind(
  v24_doy_difference_table(cells, graph, candidate_index, "local_isolate_candidate"),
  v24_doy_difference_table(cells, graph, control_index, "matched_control")
)
summary <- v24_doy_summary(differences)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  differences, file.path(output_dir, "candidate_doy_differences.csv"),
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
      "used_for_candidate_selection", "used_for_candidate_ranking",
      "supports_main_claim", "neighbourhood_definition", "year_restriction"
    ),
    value = c(
      v24_analysis_spec_version, as.character(Sys.time()), "none",
      "false", "false", "false",
      paste(
        "locked primary local-isolate graph:",
        primary$radius_km, "km,", "environment caliper",
        primary$environment_caliper, ", at least",
        primary$minimum_neighbours, "neighbours"
      ),
      paste(
        "reported twice: all environment-similar neighbours, and the subset",
        "whose median observation year equals the focal cell's"
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
      "For each fixed local-isolate candidate, the difference between its",
      "median observation day-of-year and the mean of its environment-similar",
      "neighbours under the locked primary graph. Negative means the candidate",
      "flowers earlier than its neighbours. The same statistic is reported for",
      "the matched non-isolated pigmented controls, so the candidate values",
      "have something to be read against."
    ),
    "",
    paste(
      "Two neighbour sets are reported and never merged: every",
      "environment-similar neighbour, and the subset sharing the focal cell's",
      "median observation year. A candidate with no same-year neighbour is",
      "reported as NA with a neighbour count of zero."
    ),
    "",
    paste(
      "No model is fitted. This is arithmetic on the frozen cell table and the",
      "locked neighbourhood graph. The withdrawn national phenology component",
      "is not restored. Nothing here selects candidates, ranks them, or",
      "supports a claim in the manuscript; it is a supplementary description",
      "of a candidate set that was already fixed upstream."
    )
  ),
  file.path(output_dir, "README.md"), useBytes = TRUE
)

cat("v24 supplementary DOY check written to ", normalizePath(output_dir), "\n")
print(summary)
