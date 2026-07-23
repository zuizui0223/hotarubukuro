args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[1] else "results/ecological_v12_transition_zones"

read_output <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing transition output: ", path, call. = FALSE)
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

checks <- list()
add_check <- function(name, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = name, passed = isTRUE(passed), detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

sites <- read_output("transition_population_cells_1km.csv")
hotspots <- read_output("transition_hotspot_pairs_25km.csv")
pairs <- read_output("transition_matched_pairs_25km.csv")
cv <- read_output("transition_edge_spatial_cv_25km.csv")
cv_summary <- read_output("transition_edge_spatial_cv_summary_25km.csv")
candidates <- read_output("transition_enclave_candidates_25km.csv")
sensitivity <- read_output("transition_population_unit_sensitivity.csv")

add_check(
  "population observations conserved", sum(sites$n_observations) == 1923,
  paste("n=", sum(sites$n_observations))
)
add_check(
  "primary spatial unit is 1 km",
  all(sites$spatial_unit_km == 1), paste(unique(sites$spatial_unit_km), collapse = ",")
)
add_check(
  "hotspots are short discordant edges",
  all(hotspots$flower_discordant == 1L) &&
    all(hotspots$geographic_distance_km <= 25),
  paste("n=", nrow(hotspots))
)
selected <- hotspots[hotspots$independent_pair, , drop = FALSE]
add_check(
  "independent hotspots do not reuse endpoints",
  anyDuplicated(c(selected$site_i, selected$site_j)) == 0L,
  paste("n=", nrow(selected))
)
add_check(
  "matched pairs meet distance calipers",
  all(pairs$geographic_distance_km <= 25) &&
    all(pairs$environmental_distance <= 0.75),
  paste("n=", nrow(pairs))
)
add_check(
  "matched pairs do not reuse cells",
  anyDuplicated(pairs$pigmented_site) == 0L &&
    anyDuplicated(pairs$white_site) == 0L,
  "both endpoint sets unique"
)
add_check(
  "edge CV is node-disjoint and background excludes tested causes",
  all(cv$strict_node_disjoint_split) &&
    all(cv$background_excludes_bombus_region_human) &&
    !any(grepl("Bombus|region|z_H|z_R|z_A", cv$background_formula)) &&
    !any(nzchar(ifelse(is.na(cv$background_warnings), "", cv$background_warnings))),
  paste("rows=", nrow(cv))
)
required_models <- c(
  "enmeval_composition", "enmeval_availability_level",
  "enmeval_availability_gradient", "enmeval_availability", "enmeval_full",
  "occurrence_full_50km", "occurrence_full_100km"
)
add_check(
  "all prespecified Bombus comparisons reported",
  all(required_models %in% cv_summary$model),
  paste(cv_summary$model, collapse = " | ")
)
directions <- table(candidates$candidate_direction)
add_check(
  "horticultural anomaly has reciprocal control direction",
  all(c("pigmented_in_white_background", "white_in_pigmented_background") %in%
      names(directions)),
  paste(names(directions), directions, collapse = " | ")
)
add_check(
  "spatial-unit sensitivity is complete",
  identical(as.numeric(sensitivity$cell_km), c(0.5, 1, 2, 5)),
  paste(sensitivity$cell_km, collapse = ",")
)

result <- do.call(rbind, checks)
write.csv(result, file.path(output_dir, "transition_v12_audit.csv"), row.names = FALSE)
print(result, row.names = FALSE)
if (!all(result$passed)) {
  stop("Transition v12 audit failed.", call. = FALSE)
}
cat("All", nrow(result), "transition v12 audit checks passed.\n")
