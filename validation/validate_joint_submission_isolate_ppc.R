args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
output_dir <- arg_value(
  "--output", "results/ecological_v26_joint_submission_isolate_ppc"
)
candidate_path <- arg_value(
  "--candidates",
  "results/ecological_v20_local_white_isolates/local_isolate_candidates.csv"
)

summary_path <- file.path(output_dir, "joint_isolate_natural_null_summary.csv")
null_path <- file.path(output_dir, "joint_isolate_natural_null.csv")
ids_path <- file.path(output_dir, "joint_candidate_ids.csv")
boundary_path <- file.path(output_dir, "crossfit_boundary_audit.csv")
shard_path <- file.path(output_dir, "joint_ppc_shard_stability.csv")
metadata_path <- file.path(output_dir, "joint_isolate_ppc_metadata.csv")
manifest_path <- file.path(output_dir, "joint_submission_ppc_manifest.csv")
required <- c(
  summary_path, null_path, ids_path, boundary_path, shard_path,
  metadata_path, manifest_path
)
if (!all(file.exists(required))) {
  stop(
    "Missing joint PPC artifacts: ",
    paste(required[!file.exists(required)], collapse = ", "),
    call. = FALSE
  )
}

summary_table <- hb_read_csv(summary_path)
null_table <- hb_read_csv(null_path)
ids <- hb_read_csv(ids_path)
expected_ids <- hb_read_csv(candidate_path)
boundary <- hb_read_csv(boundary_path)
shards <- hb_read_csv(shard_path)
metadata <- hb_read_csv(metadata_path)
manifest <- hb_read_csv(manifest_path)
meta <- setNames(as.character(metadata$value), metadata$field)

checkpoint <- file.path(
  output_dir,
  paste0("joint_presence_probability_draws", meta[["n_latent_draws"]], ".rds")
)
if (!file.exists(checkpoint)) {
  stop("Missing joint probability checkpoint: ", checkpoint, call. = FALSE)
}
checkpoint_object <- readRDS(checkpoint)

checks <- list()
add_check <- function(name, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = name,
    status = if (isTRUE(passed)) "PASS" else "FAIL",
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

add_check(
  "candidate_identity_fixed",
  identical(
    sort(as.character(ids$exact_site_id)),
    sort(as.character(expected_ids$exact_site_id))
  ) && nrow(ids) == 18L,
  paste("candidates=", nrow(ids))
)

n_latent <- as.integer(meta[["n_latent_draws"]])
n_replicates <- as.integer(meta[["observation_replicates_per_latent"]])
n_maps <- as.integer(meta[["n_natural_maps"]])
add_check(
  "nested_draw_counts",
  is.finite(n_latent) && n_latent >= 5000L &&
    is.finite(n_replicates) && n_replicates >= 5L &&
    n_maps == n_latent * n_replicates && nrow(null_table) == n_maps,
  paste("latent=", n_latent, "replicates=", n_replicates, "maps=", n_maps)
)

add_check(
  "probability_checkpoint_dimensions",
  is.matrix(checkpoint_object$probability) &&
    ncol(checkpoint_object$probability) == n_latent &&
    nrow(checkpoint_object$probability) == length(checkpoint_object$cell_id) &&
    all(is.finite(checkpoint_object$probability)) &&
    all(checkpoint_object$probability >= 0 & checkpoint_object$probability <= 1),
  paste(dim(checkpoint_object$probability), collapse = "x")
)

add_check(
  "probability_checkpoint_hash",
  identical(
    unname(rp_sha256(checkpoint)),
    unname(meta[["probability_checkpoint_sha256"]])
  ),
  meta[["probability_checkpoint_sha256"]]
)

thread_fields <- c(
  "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS", "GOTO_NUM_THREADS",
  "BLIS_NUM_THREADS", "INLA_NUM_THREADS"
)
add_check(
  "single_thread_lock",
  all(meta[thread_fields] == "1") && identical(meta[["OMP_DYNAMIC"]], "FALSE"),
  paste(meta[thread_fields], collapse = ",")
)

primary <- summary_table[
  summary_table$configuration == "primary_10km_env1_all_white" &
    summary_table$metric %in% c("candidate_count", "candidate_fraction"),
  , drop = FALSE
]
add_check(
  "primary_summary_complete",
  nrow(primary) == 2L &&
    all(primary$n_null_draws == n_maps) &&
    all(is.finite(primary$null_mean)) &&
    all(primary$empirical_p >= 0 & primary$empirical_p <= 1) &&
    all(primary$monte_carlo_se >= 0) &&
    all(primary$mc_lower_95 <= primary$empirical_p) &&
    all(primary$mc_upper_95 >= primary$empirical_p),
  paste(primary$metric, round(primary$empirical_p, 6), collapse = "; ")
)

count_row <- primary[primary$metric == "candidate_count", , drop = FALSE]
fraction_row <- primary[primary$metric == "candidate_fraction", , drop = FALSE]
recalc_count_mean <- mean(null_table$candidate_count)
recalc_fraction_mean <- mean(null_table$candidate_fraction)
recalc_count_p <- mean(null_table$candidate_count >= count_row$observed_value)
recalc_fraction_p <- mean(
  null_table$candidate_fraction >= fraction_row$observed_value
)
add_check(
  "raw_null_recalculation",
  hb_close_enough(recalc_count_mean, count_row$null_mean) &&
    hb_close_enough(recalc_fraction_mean, fraction_row$null_mean) &&
    hb_close_enough(recalc_count_p, count_row$empirical_p) &&
    hb_close_enough(recalc_fraction_p, fraction_row$empirical_p),
  paste(
    "count_p=", round(recalc_count_p, 6),
    "fraction_p=", round(recalc_fraction_p, 6)
  )
)

boundary_value <- setNames(as.numeric(boundary$value), boundary$metric)
add_check(
  "crossfit_boundary_is_audited",
  boundary_value[["observed_candidates"]] == 18 &&
    boundary_value[["observed_candidates_crossing_fold"]] > 0 &&
    boundary_value[["supported_cells_crossing_fold"]] > 0,
  paste(
    "candidate_crossings=",
    boundary_value[["observed_candidates_crossing_fold"]],
    "supported_crossings=",
    boundary_value[["supported_cells_crossing_fold"]]
  )
)

add_check(
  "latent_shard_stability_present",
  nrow(shards) == 5L &&
    all(is.finite(shards$fraction_tail_probability)) &&
    all(shards$fraction_tail_probability >= 0 &
          shards$fraction_tail_probability <= 1),
  paste(
    "fraction tail range=",
    paste(round(range(shards$fraction_tail_probability), 4), collapse = "-")
  )
)

manifest_hash_ok <- all(vapply(seq_len(nrow(manifest)), function(index) {
  path <- as.character(manifest$path[index])
  if (!file.exists(path)) return(FALSE)
  identical(unname(rp_sha256(path)), as.character(manifest$sha256[index]))
}, logical(1)))
add_check(
  "manifest_hashes",
  manifest_hash_ok,
  paste("files=", nrow(manifest))
)

validation <- do.call(rbind, checks)
rp_write_csv_atomic(
  validation,
  file.path(output_dir, "joint_submission_ppc_validation.csv")
)
writeLines(
  c(
    "# Joint submission isolate PPC validation",
    "",
    paste(sum(validation$status == "PASS"), "of", nrow(validation), "checks passed."),
    "",
    paste0("- ", validation$check, ": ", validation$status, " (", validation$detail, ")")
  ),
  file.path(output_dir, "VALIDATION.md"),
  useBytes = TRUE
)

if (any(validation$status != "PASS")) {
  stop("Joint submission isolate PPC validation failed.", call. = FALSE)
}
cat("Joint submission isolate PPC validation passed.\n")
