# Independent validation of the v24 supplementary flowering-date check.
#
# The checks here recompute the reported differences from the cell table rather
# than trusting the runner, and confirm the two properties that make this
# supplementary rather than confirmatory: no model was fitted, and the reported
# values reach nothing downstream.

args <- commandArgs(trailingOnly = TRUE)
positional <- args[!startsWith(args, "--")]
output_dir <- if (length(positional)) {
  positional[[1L]]
} else {
  "results/ecological_v24_candidate_doy_check"
}
cells_path <- "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"

read_output <- function(name) {
  utils::read.csv(
    file.path(output_dir, name), check.names = FALSE, stringsAsFactors = FALSE
  )
}

differences <- read_output("candidate_doy_differences.csv")
summary <- read_output("candidate_doy_summary.csv")
metadata <- read_output("candidate_doy_metadata.csv")
metadata_value <- setNames(metadata$value, metadata$field)
cells <- utils::read.csv(
  cells_path, check.names = FALSE, stringsAsFactors = FALSE
)

checks <- list()
add_check <- function(check, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check,
    status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}
close_enough <- function(x, y, tolerance = 1e-9) {
  finite <- is.finite(x) & is.finite(y)
  all((is.na(x) & is.na(y)) | (!finite) | abs(x - y) <= tolerance)
}

# The focal cell's own median DOY must match the frozen cell table. If this
# fails the stage is describing something other than the cells it names.
index <- match(differences$exact_site_id, as.character(cells$exact_site_id))
add_check(
  "focal_cells_resolve",
  !anyNA(index),
  paste("rows=", nrow(differences), "unresolved=", sum(is.na(index)))
)
add_check(
  "focal_doy_matches_cell_table",
  !anyNA(index) &&
    close_enough(differences$median_DOY, as.numeric(cells$median_DOY)[index]),
  "reported median_DOY equals the frozen cell table"
)

# The difference must be exactly focal minus neighbour mean. This is the whole
# statistic, so it is recomputed rather than assumed.
add_check(
  "difference_is_focal_minus_neighbour",
  close_enough(
    differences$all_neighbour_difference,
    differences$median_DOY - differences$all_neighbour_doy
  ) &&
    close_enough(
      differences$same_year_difference,
      differences$median_DOY - differences$same_year_neighbour_doy
    ),
  "both neighbour sets"
)

# The same-year set is a subset of the unrestricted set, always.
add_check(
  "same_year_is_a_subset",
  all(differences$same_year_n_neighbours <= differences$all_n_neighbours),
  paste(
    "maximum same-year minus all =",
    max(differences$same_year_n_neighbours - differences$all_n_neighbours)
  )
)

# A candidate with no same-year neighbour must be NA, never back-filled from
# the unrestricted set. This is the check that stops the year restriction from
# quietly becoming decorative.
empty_same_year <- differences$same_year_n_neighbours == 0L
add_check(
  "empty_same_year_reported_as_na",
  !any(empty_same_year) ||
    all(is.na(differences$same_year_difference[empty_same_year])),
  paste("focal cells with no same-year neighbour=", sum(empty_same_year))
)

# Both roles present, so the candidate values can be read against a reference.
add_check(
  "both_roles_reported",
  all(c("local_isolate_candidate", "matched_control") %in% differences$role),
  paste("roles=", paste(sort(unique(differences$role)), collapse = ", "))
)

recomputed <- lapply(split(differences, differences$role), function(block) {
  vapply(c("all", "same_year"), function(set) {
    value <- if (identical(set, "all")) {
      block$all_neighbour_difference
    } else block$same_year_difference
    usable <- is.finite(value)
    if (any(usable)) mean(value[usable]) else NA_real_
  }, numeric(1))
})
summary_ok <- all(vapply(seq_len(nrow(summary)), function(i) {
  expected <- recomputed[[summary$role[i]]][[summary$neighbour_set[i]]]
  observed <- summary$mean_difference_days[i]
  (is.na(expected) && is.na(observed)) ||
    (is.finite(expected) && is.finite(observed) &&
       abs(expected - observed) <= 1e-9)
}, logical(1)))
add_check(
  "summary_recomputes",
  summary_ok,
  paste("summary rows=", nrow(summary))
)

# The supplementary contract, asserted from the stage's own metadata.
add_check(
  "no_model_fitted",
  identical(metadata_value[["model_fitted"]], "none"),
  paste("model_fitted=", metadata_value[["model_fitted"]])
)
add_check(
  "not_used_for_selection_ranking_or_claims",
  identical(metadata_value[["used_for_candidate_selection"]], "false") &&
    identical(metadata_value[["used_for_candidate_ranking"]], "false") &&
    identical(metadata_value[["supports_main_claim"]], "false"),
  "flowering date reaches no selection, ranking, or main claim"
)

validation <- do.call(rbind, checks)
utils::write.csv(
  validation, file.path(output_dir, "VALIDATION.csv"), row.names = FALSE
)
writeLines(
  c(
    "# v24 supplementary flowering-date check validation",
    "",
    paste(sum(validation$status == "PASS"), "of", nrow(validation), "checks passed."),
    "",
    paste0("- ", validation$check, ": ", validation$status,
           " (", validation$detail, ")")
  ),
  file.path(output_dir, "VALIDATION.md"), useBytes = TRUE
)
if (any(validation$status == "FAIL")) {
  print(validation[validation$status == "FAIL", , drop = FALSE])
  stop("v24 supplementary validation failed.", call. = FALSE)
}
cat("v24 supplementary validation passed: ", nrow(validation), " checks\n")
