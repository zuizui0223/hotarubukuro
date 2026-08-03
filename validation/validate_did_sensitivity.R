args <- commandArgs(trailingOnly = TRUE)
positional <- args[!startsWith(args, "--")]
output_dir <- if (length(positional)) {
  positional[[1L]]
} else {
  "results/ecological_v22_did_human_context"
}

# See validation/audit_phenotype.R and validation/validate_local_human_context.R.
# Two published-run constants appear here: 1,307 cells and 16 candidates. Under
# --baseline reconstruction they are reported as not_applicable with the
# observed value; under --baseline published they are enforced unchanged.
#
# The structural checks conjoined with them stay enforced in both modes. In
# particular `candidate_selector_unchanged` keeps its `setequal` assertion that
# v22's candidate set is exactly v21's — that is the check that actually
# guarantees the DID sensitivity analysis did not reselect candidates, and it
# is independent of how many there are.
baseline_argument <- grep("^--baseline=", args, value = TRUE)
baseline <- if (length(baseline_argument)) {
  sub("^--baseline=", "", baseline_argument[[1L]])
} else {
  "published"
}
if (!baseline %in% c("published", "reconstruction")) {
  stop(
    "--baseline must be 'published' or 'reconstruction'; got '", baseline, "'.",
    call. = FALSE
  )
}
published_cell_count <- 1307L
published_candidate_count <- 16L

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
add_not_applicable <- function(check, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = check, status = "not_applicable",
    detail = as.character(detail), stringsAsFactors = FALSE
  )
}
add_published_count <- function(check, observed, published) {
  if (identical(baseline, "published")) {
    add_check(check, observed == published, paste("observed=", observed))
  } else {
    add_not_applicable(check, paste0(
      "observed=", observed, ";published=", published,
      ";difference=", observed - published,
      ";reason=the reconstruction defines its own analysis population"
    ))
  }
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
  !anyDuplicated(context$exact_site_id) &&
    all(stats::complete.cases(context)) &&
    all(context$did_distance_km >= 0),
  paste("cells=", nrow(context))
)
add_published_count(
  "cell_context_population", nrow(context), published_cell_count
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
  setequal(candidates$exact_site_id, v21_primary$exact_site_id),
  paste("v22=", nrow(candidates), "v21=", nrow(v21_primary))
)
add_published_count(
  "candidate_count", nrow(candidates), published_candidate_count
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
failed <- validation[validation$status == "FAIL", , drop = FALSE]
skipped <- validation[validation$status == "not_applicable", , drop = FALSE]
if (nrow(failed)) {
  print(failed)
  stop("v22 independent validation failed.", call. = FALSE)
}
if (nrow(skipped)) {
  cat("v22 checks not applicable under --baseline ", baseline, ":\n", sep = "")
  print(skipped)
}
cat(
  "v22 independent validation passed ", sum(validation$status == "PASS"),
  " checks (", nrow(skipped), " not applicable).\n", sep = ""
)
