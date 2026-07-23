args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) {
  args[[1L]]
} else {
  "results/ecological_v22_did_human_context"
}

read_output <- function(name) {
  utils::read.csv(
    file.path(output_dir, name), check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
close_enough <- function(x, y, tolerance = 1e-9) {
  length(x) == length(y) &&
    all((is.na(x) & is.na(y)) |
          (is.finite(x) & is.finite(y) & abs(x - y) <= tolerance))
}
finite_max <- function(value) {
  value <- value[is.finite(value)]
  if (length(value)) max(value) else NA_real_
}
checks <- list()
add_check <- function(check, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check,
    status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = detail,
    stringsAsFactors = FALSE
  )
}

context <- read_output("did_cell_context.csv")
definitions <- read_output("did_feature_definitions.csv")
summary <- read_output("did_contrast_summary.csv")
null <- read_output("did_contrast_null.csv")
convergence <- read_output("did_convergence_summary.csv")
convergence_null <- read_output("did_convergence_null.csv")
composition <- read_output("did_context_composition_summary.csv")
composition_null <- read_output("did_context_composition_null.csv")
candidates <- read_output("did_candidate_details.csv")
collinearity <- read_output("did_collinearity.csv")
provenance <- read_output("did_provenance.csv")
metadata <- read_output("did_metadata.csv")
metadata_value <- setNames(metadata$value, metadata$field)

add_check(
  "did_source_archive",
  file.exists(provenance$archive_path[1L]) &&
    identical(
      unname(tools::md5sum(provenance$archive_path[1L])),
      provenance$archive_md5[1L]
    ),
  paste("md5=", provenance$archive_md5[1L])
)
presence_path <- file.path(
  output_dir, "mlit_did_presence_2015_1km.tif"
)
distance_path <- file.path(
  output_dir, "mlit_did_distance_2015_1km.tif"
)
presence <- terra::rast(presence_path)
distance <- terra::rast(distance_path)
add_check(
  "did_raster_alignment",
  terra::compareGeom(presence, distance, stopOnError = FALSE) &&
    terra::ncell(distance) == 1040L * 1010L &&
    grepl("6668", terra::crs(distance)),
  paste(
    "dimensions=", terra::nrow(distance), "x", terra::ncol(distance)
  )
)
distance_range <- unlist(terra::global(
  distance, c("min", "max"), na.rm = TRUE
))
add_check(
  "did_distance_values",
  all(is.finite(distance_range)) &&
    distance_range[1L] == 0 && distance_range[2L] > 500000,
  paste("range_m=", paste(round(distance_range), collapse = " to "))
)
add_check(
  "cell_context_completeness",
  nrow(context) == 1307L &&
    !anyDuplicated(context$exact_site_id) &&
    all(stats::complete.cases(context)) &&
    all(context$did_distance_km >= 0),
  paste("cells=", nrow(context))
)
add_check(
  "context_class_partition",
  sum(table(context$human_context_class)) == nrow(context) &&
    length(unique(context$human_context_class)) == 4L,
  paste(
    names(table(context$human_context_class)),
    as.integer(table(context$human_context_class)),
    collapse = "; "
  )
)

v21_details <- utils::read.csv(
  paste0(
    "results/ecological_v21_local_human_neighbourhood/",
    "human_neighbourhood_observed_details.csv"
  ),
  check.names = FALSE, stringsAsFactors = FALSE
)
v21_primary <- v21_details[
  v21_details$configuration == "primary_10km_env1_all_white",
  , drop = FALSE
]
add_check(
  "candidate_selector_unchanged",
  nrow(candidates) == 16L &&
    setequal(candidates$exact_site_id, v21_primary$exact_site_id),
  paste("v22=", nrow(candidates), "v21=", nrow(v21_primary))
)
add_check(
  "candidate_local_support",
  all(candidates$n_white_neighbours >= 3L),
  paste(
    "white-neighbour range=",
    paste(range(candidates$n_white_neighbours), collapse = " to ")
  )
)

observed <- vapply(summary$feature, function(feature) {
  mean(
    candidates[[paste0(feature, "_focal_minus_white")]],
    na.rm = TRUE
  )
}, numeric(1))
raw_p <- vapply(seq_len(nrow(summary)), function(index) {
  simulated <- null[[summary$feature[index]]]
  (1 + sum(simulated >= observed[index])) / (length(simulated) + 1)
}, numeric(1))
simulated_matrix <- as.matrix(
  null[, summary$feature, drop = FALSE]
)
center <- colMeans(simulated_matrix)
spread <- apply(simulated_matrix, 2, stats::sd)
spread[!is.finite(spread) | spread <= 1e-12] <- 1
null_z <- sweep(
  sweep(simulated_matrix, 2, center, "-"), 2, spread, "/"
)
observed_z <- (observed - center) / spread
null_max <- apply(null_z, 1, finite_max)
fwer <- vapply(seq_len(nrow(summary)), function(index) {
  (1 + sum(null_max >= observed_z[index])) /
    (length(null_max) + 1)
}, numeric(1))
add_check(
  "contrast_statistics",
  close_enough(
    observed, summary$observed_focal_minus_white_neighbour
  ) &&
    close_enough(raw_p, summary$directional_or_two_sided_p),
  paste("features=", nrow(summary))
)
add_check(
  "contrast_maxT_statistics",
  close_enough(fwer, summary$maxT_FWER_p),
  paste("features=", nrow(summary))
)
add_check(
  "natural_map_replication",
  nrow(null) == 1000L &&
    all(summary$n_null_draws == 1000L) &&
    all(convergence$n_null_draws == 1000L),
  paste("contrast draws=", nrow(null))
)

convergence_p <- vapply(seq_len(nrow(convergence)), function(index) {
  block <- convergence_null[
    convergence_null$spike_feature ==
      convergence$spike_feature[index],
    , drop = FALSE
  ]
  simulated <- block[[convergence$metric[index]]]
  (1 + sum(simulated >= convergence$observed_value[index])) /
    (length(simulated) + 1)
}, numeric(1))
add_check(
  "convergence_statistics",
  close_enough(convergence_p, convergence$empirical_p),
  paste("tests=", nrow(convergence))
)
add_check(
  "composition_partition",
  sum(composition$observed_candidate_count) == nrow(candidates) &&
    close_enough(sum(composition$observed_candidate_fraction), 1) &&
    nrow(composition_null) == 1000L,
  paste(
    "observed count=", sum(composition$observed_candidate_count)
  )
)
add_check(
  "single_joint_q10_followup",
  sum(candidates$joint_q10_did_proximity_spike) == 1L,
  paste(
    "joint candidates=",
    sum(candidates$joint_q10_did_proximity_spike)
  )
)
add_check(
  "collinearity_disclosed",
  any(
    collinearity$feature_1 == "did_aligned_population_score" &
      collinearity$feature_2 == "population_5km_rank" &
      abs(collinearity$spearman_rho) > 0.95
  ) &&
    any(
      collinearity$feature_1 == "did_proximity_rank" &
        collinearity$feature_2 == "did_aligned_population_score" &
        abs(collinearity$spearman_rho) > 0.95
    ),
  paste(
    "aligned score maximum input rho=",
    round(max(abs(collinearity$spearman_rho[
      collinearity$feature_1 == "did_aligned_population_score" |
        collinearity$feature_2 == "did_aligned_population_score"
    ])), 3)
  )
)
add_check(
  "residual_not_used",
  identical(
    metadata_value[["residual_used_as_response"]], "false"
  ),
  paste(
    "residual_used_as_response=",
    metadata_value[["residual_used_as_response"]]
  )
)
add_check(
  "claim_ceiling",
  grepl(
    "not planting", metadata_value[["causal_claim_ceiling"]],
    fixed = TRUE
  ) &&
    grepl(
      "not planting", metadata_value[["causal_claim_ceiling"]],
      fixed = TRUE
    ),
  metadata_value[["causal_claim_ceiling"]]
)
current_final_md5 <- if (file.exists("final.Rmd")) {
  unname(tools::md5sum("final.Rmd"))
} else {
  NA_character_
}
add_check(
  "final_rmd_untouched",
  identical(current_final_md5, metadata_value[["final_Rmd_md5"]]) &&
    identical(
      current_final_md5, "0173d76bef175c6319d43196b1406cc8"
    ),
  paste("md5=", current_final_md5)
)

validation <- do.call(rbind, checks)
utils::write.csv(
  validation,
  file.path(output_dir, "did_independent_validation.csv"),
  row.names = FALSE
)
writeLines(
  c(
    "# v22 independent validation",
    "",
    paste(
      sum(validation$status == "PASS"), "of", nrow(validation),
      "checks passed."
    ),
    "",
    paste0(
      "- ", validation$check, ": ", validation$status,
      " (", validation$detail, ")"
    )
  ),
  file.path(output_dir, "VALIDATION.md"),
  useBytes = TRUE
)
if (any(validation$status != "PASS")) {
  print(validation[validation$status != "PASS", , drop = FALSE])
  stop("v22 independent validation failed.", call. = FALSE)
}
cat("v22 independent validation passed ",
    nrow(validation), " checks.\n")
