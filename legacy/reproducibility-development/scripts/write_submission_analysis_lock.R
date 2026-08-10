args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

crossfit_path <- arg_value(
  "--crossfit",
  paste0(
    "results/ecological_v25_submission_isolate_null/",
    "submission_isolate_natural_null_summary.csv"
  )
)
joint_path <- arg_value(
  "--joint",
  paste0(
    "results/ecological_v26_joint_submission_isolate_ppc/",
    "joint_isolate_natural_null_summary.csv"
  )
)
boundary_path <- arg_value(
  "--boundary",
  paste0(
    "results/ecological_v26_joint_submission_isolate_ppc/",
    "crossfit_boundary_audit.csv"
  )
)
final_results_path <- arg_value(
  "--final-results",
  "results/final_analysis_pipeline/final_result_registry.csv"
)
final_claims_path <- arg_value(
  "--final-claims",
  "results/final_analysis_pipeline/final_claim_registry.csv"
)
candidate_ids_path <- arg_value(
  "--candidate-ids",
  paste0(
    "results/ecological_v25_submission_isolate_null/",
    "submission_candidate_ids.csv"
  )
)
joint_ids_path <- arg_value(
  "--joint-ids",
  paste0(
    "results/ecological_v26_joint_submission_isolate_ppc/",
    "joint_candidate_ids.csv"
  )
)
output_dir <- arg_value(
  "--output", "results/submission_analysis_lock"
)

required <- c(
  crossfit_path, joint_path, boundary_path, final_results_path,
  final_claims_path, candidate_ids_path, joint_ids_path
)
if (!all(file.exists(required))) {
  stop(
    "Missing submission-lock inputs: ",
    paste(required[!file.exists(required)], collapse = ", "),
    call. = FALSE
  )
}

crossfit <- hb_read_csv(crossfit_path)
joint <- hb_read_csv(joint_path)
boundary <- hb_read_csv(boundary_path)
final_results <- hb_read_csv(final_results_path)
claims <- hb_read_csv(final_claims_path)
ids <- hb_read_csv(candidate_ids_path)
joint_ids <- hb_read_csv(joint_ids_path)

select_one <- function(data, configuration, metric, label) {
  out <- data[
    data$configuration == configuration & data$metric == metric,
    , drop = FALSE
  ]
  if (nrow(out) != 1L) {
    stop("Expected exactly one row for ", label, call. = FALSE)
  }
  out
}

primary_count <- select_one(
  crossfit, "primary_10km_env1_all_white", "candidate_count",
  "primary cross-fitted count"
)
primary_fraction <- select_one(
  crossfit, "primary_10km_env1_all_white", "candidate_fraction",
  "primary cross-fitted fraction"
)
fold_fraction <- select_one(
  crossfit, "fold_restricted_10km_env1_all_white", "candidate_fraction",
  "same-fold cross-fitted fraction"
)
joint_count <- select_one(
  joint, "primary_10km_env1_all_white", "candidate_count",
  "joint posterior-predictive count"
)
joint_fraction <- select_one(
  joint, "primary_10km_env1_all_white", "candidate_fraction",
  "joint posterior-predictive fraction"
)

if (!identical(
  sort(as.character(ids$exact_site_id)),
  sort(as.character(joint_ids$exact_site_id))
)) {
  stop("Cross-fitted and joint PPC candidate IDs differ.", call. = FALSE)
}
if (nrow(ids) != 18L) {
  stop("Submission lock expects the pre-fixed 18 candidate IDs.", call. = FALSE)
}

mc_interval <- function(p, se) {
  z <- stats::qnorm(0.975)
  c(max(0, p - z * se), min(1, p + z * se))
}
primary_count_ci <- mc_interval(
  primary_count$empirical_p, primary_count$monte_carlo_se
)
primary_fraction_ci <- mc_interval(
  primary_fraction$empirical_p, primary_fraction$monte_carlo_se
)
fold_fraction_ci <- mc_interval(
  fold_fraction$empirical_p, fold_fraction$monte_carlo_se
)

row <- function(
    reference, metric, observed, null_mean, p, se,
    lower, upper, maps, role) {
  data.frame(
    reference = reference,
    metric = metric,
    observed_value = as.numeric(observed),
    null_mean = as.numeric(null_mean),
    tail_probability = as.numeric(p),
    monte_carlo_se = as.numeric(se),
    mc_lower_95 = as.numeric(lower),
    mc_upper_95 = as.numeric(upper),
    n_natural_maps = as.integer(maps),
    role = role,
    stringsAsFactors = FALSE
  )
}

evidence <- rbind(
  row(
    "crossfit_primary", "candidate_count",
    primary_count$observed_value, primary_count$null_mean,
    primary_count$empirical_p, primary_count$monte_carlo_se,
    primary_count_ci[1], primary_count_ci[2], primary_count$n_null_draws,
    "held-out predictive reference"
  ),
  row(
    "crossfit_primary", "candidate_fraction",
    primary_fraction$observed_value, primary_fraction$null_mean,
    primary_fraction$empirical_p, primary_fraction$monte_carlo_se,
    primary_fraction_ci[1], primary_fraction_ci[2],
    primary_fraction$n_null_draws,
    "held-out predictive reference"
  ),
  row(
    "crossfit_same_fold", "candidate_fraction",
    fold_fraction$observed_value, fold_fraction$null_mean,
    fold_fraction$empirical_p, fold_fraction$monte_carlo_se,
    fold_fraction_ci[1], fold_fraction_ci[2], fold_fraction$n_null_draws,
    "jointly coherent held-out sensitivity within spatial folds"
  ),
  row(
    "joint_full_data_ppc", "candidate_count",
    joint_count$observed_value, joint_count$null_mean,
    joint_count$empirical_p, joint_count$monte_carlo_se,
    joint_count$mc_lower_95, joint_count$mc_upper_95,
    joint_count$n_null_draws,
    "full-data joint spatial posterior-predictive coherence sensitivity"
  ),
  row(
    "joint_full_data_ppc", "candidate_fraction",
    joint_fraction$observed_value, joint_fraction$null_mean,
    joint_fraction$empirical_p, joint_fraction$monte_carlo_se,
    joint_fraction$mc_lower_95, joint_fraction$mc_upper_95,
    joint_fraction$n_null_draws,
    "full-data joint spatial posterior-predictive coherence sensitivity"
  )
)

# Robustness is deliberately stricter than whether one Monte Carlo point
# estimate falls below 0.05. An upper-tail claim must clear the threshold after
# Monte Carlo uncertainty in both the held-out and joint predictive references.
robust_fraction_upper_tail <-
  primary_fraction_ci[2] < 0.05 &&
  fold_fraction_ci[2] < 0.05 &&
  joint_fraction$mc_upper_95 < 0.05

near_upper_crossfit <- primary_fraction$empirical_p <= 0.10
joint_compatible <- joint_fraction$empirical_p > 0.05
submission_status <- if (robust_fraction_upper_tail) {
  "robust_upper_tail_across_predictive_references"
} else if (near_upper_crossfit && joint_compatible) {
  "crossfit_upper_tail_not_jointly_robust"
} else if (near_upper_crossfit) {
  "threshold_adjacent_upper_tail"
} else {
  "compatible_with_natural_model"
}

boundary_value <- setNames(as.numeric(boundary$value), boundary$metric)
if (is.null(boundary_value[["observed_candidates_crossing_fold"]])) {
  stop("Boundary audit is incomplete.", call. = FALSE)
}

claim_text <- sprintf(
  paste(
    "Eighteen pre-fixed local pigmented isolates were identified",
    "(fraction %.4f). The held-out cross-fitted reference gave count",
    "p=%.3f and fraction p=%.3f (Monte Carlo 95%% interval %.3f-%.3f);",
    "the same-fold sensitivity gave fraction p=%.3f. A full-data joint",
    "spatial posterior-predictive check gave count p=%.3f and fraction",
    "p=%.3f (Monte Carlo 95%% interval %.3f-%.3f). Because the fraction",
    "did not show a robust upper-tail departure across predictive",
    "references, the isolates are retained as follow-up candidates rather",
    "than evidence of excess occurrence or horticultural provenance."
  ),
  primary_fraction$observed_value,
  primary_count$empirical_p,
  primary_fraction$empirical_p,
  primary_fraction_ci[1], primary_fraction_ci[2],
  fold_fraction$empirical_p,
  joint_count$empirical_p,
  joint_fraction$empirical_p,
  joint_fraction$mc_lower_95, joint_fraction$mc_upper_95
)

claim_row <- match("C5_local_isolates", claims$claim_id)
if (is.na(claim_row)) {
  stop("C5_local_isolates is missing from the final claim registry.", call. = FALSE)
}
claims$status[claim_row] <- submission_status
claims$claim[claim_row] <- claim_text
claims$claim_ceiling[claim_row] <- paste(
  "candidate definition and predictive-model checking;",
  "not evidence of introduction, planting, or horticultural origin"
)

summary <- data.frame(
  field = c(
    "submission_status", "candidate_count", "candidate_fraction",
    "crossfit_count_p", "crossfit_fraction_p",
    "crossfit_fraction_mc_lower_95", "crossfit_fraction_mc_upper_95",
    "same_fold_fraction_p", "joint_count_p", "joint_fraction_p",
    "joint_fraction_mc_lower_95", "joint_fraction_mc_upper_95",
    "candidate_fold_boundary_crossings", "claim"
  ),
  value = c(
    submission_status, nrow(ids), primary_fraction$observed_value,
    primary_count$empirical_p, primary_fraction$empirical_p,
    primary_fraction_ci[1], primary_fraction_ci[2],
    fold_fraction$empirical_p,
    joint_count$empirical_p, joint_fraction$empirical_p,
    joint_fraction$mc_lower_95, joint_fraction$mc_upper_95,
    boundary_value[["observed_candidates_crossing_fold"]], claim_text
  ),
  stringsAsFactors = FALSE
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
rp_write_csv_atomic(
  evidence,
  file.path(output_dir, "submission_isolate_evidence.csv")
)
rp_write_csv_atomic(
  crossfit,
  file.path(output_dir, "submission_isolate_crossfit_sensitivity.csv")
)
rp_write_csv_atomic(
  claims,
  file.path(output_dir, "submission_claim_registry.csv")
)
rp_write_csv_atomic(
  summary,
  file.path(output_dir, "submission_inference_summary.csv")
)
writeLines(
  c(
    "# Submission inference lock",
    "",
    paste0("Status: **", submission_status, "**"),
    "",
    claim_text,
    "",
    paste0(
      "Six of the 18 observed candidates had at least one eligible",
      " neighbour across a spatial-fold boundary; the same-fold",
      " cross-fitted sensitivity and the full-data joint PPC are therefore",
      " reported alongside the primary held-out reference."
    )
  ),
  file.path(output_dir, "SUBMISSION_CLAIM.md"),
  useBytes = TRUE
)

manifest_paths <- c(
  crossfit_path, joint_path, boundary_path,
  candidate_ids_path, joint_ids_path,
  file.path(output_dir, "submission_isolate_evidence.csv"),
  file.path(output_dir, "submission_isolate_crossfit_sensitivity.csv"),
  file.path(output_dir, "submission_claim_registry.csv"),
  file.path(output_dir, "submission_inference_summary.csv"),
  file.path(output_dir, "SUBMISSION_CLAIM.md")
)
rp_write_manifest(
  rp_manifest_rows(
    manifest_paths,
    role = "submission_inference_lock",
    note = paste(
      "cross-fitted and joint posterior-predictive evidence are",
      "locked before manuscript reporting"
    )
  ),
  file.path(output_dir, "submission_analysis_manifest.csv")
)

cat("Submission inference lock written: ", submission_status, "\n", sep = "")
cat(claim_text, "\n")
