args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
candidate_path <- arg_value(
  "--candidates",
  "results/ecological_v20_local_white_isolates/local_isolate_candidates.csv"
)
presence_checkpoint <- arg_value(
  "--presence-checkpoint",
  paste0(
    "results/ecological_v25_submission_presence/checkpoints/",
    "national_environment_spde_presence_draws10000.rds"
  )
)
active_output_dir <- arg_value(
  "--active-output", "results/ecological_v20_local_white_isolates"
)
output_dir <- arg_value(
  "--output", "results/ecological_v25_submission_isolate_null"
)
expected_draws <- as.integer(arg_value(
  "--draws", Sys.getenv("HOTARUBUKURO_SUBMISSION_DRAWS", "10000")
))
submission_seed <- as.integer(arg_value("--seed", "20260725"))

if (!is.finite(expected_draws) || expected_draws < 5000L) {
  stop("Submission null refinement requires at least 5,000 maps.", call. = FALSE)
}
if (!is.finite(submission_seed)) {
  stop("--seed must be a finite integer.", call. = FALSE)
}

hb_require_stage_packages("local_pigmented_isolates")
hb_load_modules("local_pigmented_isolates")

cells <- hb_read_csv(cells_path)
candidates <- hb_read_csv(candidate_path)
presence_raw <- readRDS(presence_checkpoint)
presence <- v18_align_result(presence_raw, cells, "presence")
counts <- presence$draws
if (!is.matrix(counts) || ncol(counts) != expected_draws) {
  stop(
    "Submission checkpoint has ", ncol(counts),
    " draws; expected ", expected_draws, ".", call. = FALSE
  )
}

observed_q <- v18_predictive_tail_q(
  as.numeric(cells$n_pigmented), counts, "upper"
)
simulated_q <- v18_simulation_tail_q(counts, "upper")
configurations <- v20_configuration_table()
summary_rows <- list()
null_rows <- list()
primary_candidate_ids <- character()

for (row in seq_len(nrow(configurations))) {
  configuration <- configurations[row, ]
  graph <- v20_neighbour_graph(
    cells,
    radius_km = configuration$radius_km,
    environment_caliper = configuration$environment_caliper,
    minimum_neighbours = configuration$minimum_neighbours,
    same_fold_only = configuration$same_fold_only
  )
  observed_profile <- v20_local_profile(
    as.numeric(cells$n_pigmented), graph,
    configuration$maximum_neighbour_pigment_share
  )
  simulated_profile <- v20_local_profile(
    counts, graph,
    configuration$maximum_neighbour_pigment_share
  )
  metric <- v20_metric_rows(
    observed_profile, simulated_profile,
    observed_q, simulated_q, configuration$configuration
  )
  summary_rows[[length(summary_rows) + 1L]] <- metric$summary
  null_rows[[length(null_rows) + 1L]] <- metric$null
  if (identical(configuration$role, "primary")) {
    primary_candidate_ids <- as.character(
      cells$exact_site_id[observed_profile$candidate[, 1L]]
    )
  }
}

submission_summary <- do.call(rbind, summary_rows)
submission_null <- do.call(rbind, null_rows)
rownames(submission_summary) <- NULL
rownames(submission_null) <- NULL

expected_candidate_ids <- sort(as.character(candidates$exact_site_id))
observed_candidate_ids <- sort(primary_candidate_ids)
if (!identical(expected_candidate_ids, observed_candidate_ids)) {
  stop(
    "The submission refinement changed observed candidate identity. ",
    "Observed candidates must be fixed before the natural-map comparison.",
    call. = FALSE
  )
}

primary_summary <- submission_summary[
  submission_summary$configuration == "primary_10km_env1_all_white",
  , drop = FALSE
]
if (!identical(sort(primary_summary$metric), c("candidate_count", "candidate_fraction"))) {
  stop("Primary submission summary is incomplete.", call. = FALSE)
}

thread_fields <- c(
  "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS", "GOTO_NUM_THREADS",
  "BLIS_NUM_THREADS", "INLA_NUM_THREADS", "OMP_DYNAMIC"
)
metadata <- data.frame(
  field = c(
    "analysis_spec_version", "generated_utc", "n_natural_maps",
    "presence_checkpoint", "presence_checkpoint_sha256", "candidate_count",
    "candidate_ids_sha256", "random_seed", "RNGkind", thread_fields,
    "commit", "role"
  ),
  value = c(
    "submission_isolate_null_1.0",
    format(Sys.time(), tz = "UTC", usetz = TRUE),
    as.character(expected_draws),
    presence_checkpoint,
    rp_sha256(presence_checkpoint),
    as.character(length(observed_candidate_ids)),
    digest::digest(observed_candidate_ids, algo = "sha256"),
    as.character(submission_seed),
    paste(RNGkind(), collapse = ";"),
    unname(Sys.getenv(thread_fields, unset = "unset")),
    rp_git_commit(),
    paste(
      "submission-scale natural-map reference for the pre-fixed local",
      "pigmented-isolate event; no human variable enters candidate selection"
    )
  ),
  stringsAsFactors = FALSE
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
rp_write_csv_atomic(
  submission_summary,
  file.path(output_dir, "submission_isolate_natural_null_summary.csv")
)
rp_write_csv_atomic(
  submission_null,
  file.path(output_dir, "submission_isolate_natural_null.csv")
)
rp_write_csv_atomic(
  data.frame(
    exact_site_id = observed_candidate_ids,
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "submission_candidate_ids.csv")
)
rp_write_csv_atomic(
  metadata,
  file.path(output_dir, "submission_isolate_null_metadata.csv")
)

manifest_paths <- c(
  presence_checkpoint,
  file.path(output_dir, "submission_isolate_natural_null_summary.csv"),
  file.path(output_dir, "submission_isolate_natural_null.csv"),
  file.path(output_dir, "submission_candidate_ids.csv"),
  file.path(output_dir, "submission_isolate_null_metadata.csv")
)
rp_write_manifest(
  rp_manifest_rows(
    manifest_paths,
    role = "submission_reference_candidate",
    note = "freeze after multi-run stability verification"
  ),
  file.path(output_dir, "submission_reference_manifest.csv")
)

# The final registry and manuscript figures already consume the v20 paths.
# Replace only the natural-map count/fraction tables after all 1,000-map human
# diagnostics have completed; candidate, matching, landscape and DID outputs are
# untouched.
rp_write_csv_atomic(
  submission_summary,
  file.path(active_output_dir, "local_isolate_natural_null_summary.csv")
)
rp_write_csv_atomic(
  submission_null,
  file.path(active_output_dir, "local_isolate_natural_null.csv")
)
rp_write_csv_atomic(
  metadata,
  file.path(active_output_dir, "submission_null_override_metadata.csv")
)

cat(
  "Submission isolate null refined with ", expected_draws,
  " natural maps.\n", sep = ""
)
print(primary_summary)
