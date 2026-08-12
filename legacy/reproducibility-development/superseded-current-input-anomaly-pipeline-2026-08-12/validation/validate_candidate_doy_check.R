# Independent validation of the v24 supplementary flowering-date check.
#
# Recomputes the cell-year table and the reported arithmetic from source
# observations. It also confirms that this remains a model-free supplement that
# reaches neither candidate selection nor the main claims.

args <- commandArgs(trailingOnly = TRUE)
positional <- args[!startsWith(args, "--")]
output_dir <- if (length(positional)) {
  positional[[1L]]
} else {
  "results/ecological_v24_candidate_doy_check"
}
cells_path <- "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
observations_path <- paste0(
  "results/ecological_v11_pigmentation_hurdle/",
  "analysis_data_pigmentation_hurdle.csv"
)

read_output <- function(name) {
  utils::read.csv(
    file.path(output_dir, name), check.names = FALSE, stringsAsFactors = FALSE
  )
}

differences <- read_output("candidate_doy_differences.csv")
year_detail <- read_output("candidate_doy_year_differences.csv")
cell_year <- read_output("candidate_doy_cell_year_table.csv")
summary <- read_output("candidate_doy_summary.csv")
metadata <- read_output("candidate_doy_metadata.csv")
metadata_value <- setNames(metadata$value, metadata$field)
cells <- utils::read.csv(
  cells_path, check.names = FALSE, stringsAsFactors = FALSE
)
observations <- utils::read.csv(
  observations_path, check.names = FALSE, stringsAsFactors = FALSE
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
  length(x) == length(y) && all(
    (is.na(x) & is.na(y)) |
      (is.finite(x) & is.finite(y) & abs(x - y) <= tolerance)
  )
}

# Rebuild the cell-year table independently from the observation rows.
required <- c("x_km", "y_km", "DOY", "year")
add_check(
  "observation_columns",
  all(required %in% names(observations)),
  paste("required=", paste(required, collapse = ","))
)
observations$cell_id_v24_validation <- paste0(
  "cell-1km-", floor(as.numeric(observations$x_km)), "_",
  floor(as.numeric(observations$y_km))
)
observations$DOY <- as.numeric(observations$DOY)
observations$year <- as.integer(round(as.numeric(observations$year)))
usable_observations <- observations[
  is.finite(observations$DOY) & is.finite(observations$year), , drop = FALSE
]
groups <- split(
  seq_len(nrow(usable_observations)),
  paste(
    usable_observations$cell_id_v24_validation,
    usable_observations$year, sep = "::"
  )
)
recomputed_cell_year <- do.call(rbind, lapply(groups, function(index) {
  block <- usable_observations[index, , drop = FALSE]
  data.frame(
    exact_site_id = as.character(block$cell_id_v24_validation[[1L]]),
    year = as.integer(block$year[[1L]]),
    median_DOY = stats::median(block$DOY),
    n_observations = nrow(block), stringsAsFactors = FALSE
  )
}))
recomputed_cell_year <- recomputed_cell_year[
  order(recomputed_cell_year$exact_site_id, recomputed_cell_year$year),
  , drop = FALSE
]
cell_year <- cell_year[order(cell_year$exact_site_id, cell_year$year), , drop = FALSE]
add_check(
  "cell_year_table_recomputes",
  identical(
    paste(cell_year$exact_site_id, cell_year$year),
    paste(recomputed_cell_year$exact_site_id, recomputed_cell_year$year)
  ) &&
    close_enough(cell_year$median_DOY, recomputed_cell_year$median_DOY) &&
    identical(
      as.integer(cell_year$n_observations),
      as.integer(recomputed_cell_year$n_observations)
    ),
  paste("cell-years=", nrow(cell_year))
)

# Focal-cell DOY and unrestricted arithmetic.
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
add_check(
  "unrestricted_difference_is_focal_minus_neighbour_median",
  close_enough(
    differences$all_neighbour_difference,
    differences$median_DOY - differences$all_neighbour_doy
  ),
  paste("focal cells=", nrow(differences))
)

# Every exact-year row must be literal same-year arithmetic, not equality of a
# cell-level median-year proxy.
detail_arithmetic <- close_enough(
  year_detail$difference_days,
  year_detail$focal_year_median_DOY - year_detail$neighbour_year_median_DOY
)
focal_keys <- paste(year_detail$exact_site_id, year_detail$year)
source_keys <- paste(cell_year$exact_site_id, cell_year$year)
focal_rows_exist <- all(focal_keys %in% source_keys)
neighbour_years_exist <- all(vapply(seq_len(nrow(year_detail)), function(i) {
  ids <- strsplit(
    as.character(year_detail$neighbour_cell_ids[[i]]), "|", fixed = TRUE
  )[[1L]]
  all(paste(ids, year_detail$year[[i]]) %in% source_keys)
}, logical(1)))
add_check(
  "exact_year_rows_are_literal_shared_years",
  detail_arithmetic && focal_rows_exist && neighbour_years_exist &&
    all(year_detail$n_neighbour_cells >= 1L),
  paste("year-specific contrasts=", nrow(year_detail))
)

# Recompute focal-cell same-year summaries from the detailed rows.
recomputed_same_year <- lapply(seq_len(nrow(differences)), function(i) {
  block <- year_detail[
    year_detail$role == differences$role[[i]] &
      year_detail$exact_site_id == differences$exact_site_id[[i]],
    , drop = FALSE
  ]
  c(
    median_difference = if (nrow(block)) {
      stats::median(block$difference_days)
    } else NA_real_,
    mean_difference = if (nrow(block)) mean(block$difference_days) else NA_real_,
    n_years = nrow(block),
    mean_neighbours = if (nrow(block)) mean(block$n_neighbour_cells) else NA_real_,
    min_neighbours = if (nrow(block)) min(block$n_neighbour_cells) else 0
  )
})
recomputed_same_year <- do.call(rbind, recomputed_same_year)
add_check(
  "same_year_focal_summaries_recompute",
  close_enough(
    differences$same_year_median_difference,
    recomputed_same_year[, "median_difference"]
  ) &&
    close_enough(
      differences$same_year_mean_difference,
      recomputed_same_year[, "mean_difference"]
    ) &&
    identical(
      as.integer(differences$same_year_n_years),
      as.integer(recomputed_same_year[, "n_years"])
    ) &&
    close_enough(
      differences$same_year_mean_neighbour_cells,
      recomputed_same_year[, "mean_neighbours"]
    ) &&
    identical(
      as.integer(differences$same_year_min_neighbour_cells),
      as.integer(recomputed_same_year[, "min_neighbours"])
    ),
  paste("focal cells=", nrow(differences))
)
empty_same_year <- differences$same_year_n_years == 0L
add_check(
  "no_shared_year_reported_as_na",
  !any(empty_same_year) || all(
    is.na(differences$same_year_median_difference[empty_same_year]) &
      is.na(differences$same_year_mean_difference[empty_same_year])
  ),
  paste("focal cells with no shared year=", sum(empty_same_year))
)

add_check(
  "both_roles_reported",
  all(c("local_isolate_candidate", "matched_control") %in% differences$role),
  paste("roles=", paste(sort(unique(differences$role)), collapse = ", "))
)

# Recompute the two summary series.
summary_ok <- all(vapply(seq_len(nrow(summary)), function(i) {
  block <- differences[differences$role == summary$role[[i]], , drop = FALSE]
  value <- if (summary$neighbour_set[[i]] == "all") {
    block$all_neighbour_difference
  } else {
    block$same_year_median_difference
  }
  usable <- is.finite(value)
  expected_mean <- if (any(usable)) mean(value[usable]) else NA_real_
  expected_median <- if (any(usable)) stats::median(value[usable]) else NA_real_
  ((is.na(expected_mean) && is.na(summary$mean_difference_days[[i]])) ||
     abs(expected_mean - summary$mean_difference_days[[i]]) <= 1e-9) &&
    ((is.na(expected_median) && is.na(summary$median_difference_days[[i]])) ||
       abs(expected_median - summary$median_difference_days[[i]]) <= 1e-9) &&
    summary$n_with_usable_neighbours[[i]] == sum(usable) &&
    summary$n_earlier_than_neighbours[[i]] == sum(usable & value < 0) &&
    summary$n_later_than_neighbours[[i]] == sum(usable & value > 0)
}, logical(1)))
add_check(
  "summary_recomputes",
  summary_ok,
  paste("summary rows=", nrow(summary))
)

add_check(
  "no_model_or_significance_test",
  identical(metadata_value[["model_fitted"]], "none") &&
    identical(metadata_value[["significance_test"]], "none"),
  paste(
    "model=", metadata_value[["model_fitted"]],
    "test=", metadata_value[["significance_test"]]
  )
)
add_check(
  "not_used_for_selection_ranking_or_claims",
  identical(metadata_value[["used_for_candidate_selection"]], "false") &&
    identical(metadata_value[["used_for_candidate_ranking"]], "false") &&
    identical(metadata_value[["supports_main_claim"]], "false"),
  "flowering date reaches no selection, ranking, or main claim"
)
add_check(
  "exact_year_method_declared",
  grepl("exact observation-year overlap", metadata_value[["year_restriction"]],
        fixed = TRUE),
  metadata_value[["year_restriction"]]
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
