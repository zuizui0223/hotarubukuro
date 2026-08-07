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
output_dir <- arg_value(
  "--output", "results/ecological_v26_joint_submission_isolate_ppc"
)
n_latent <- as.integer(arg_value(
  "--latent-draws", Sys.getenv("HOTARUBUKURO_JOINT_LATENT_DRAWS", "10000")
))
n_replicates <- as.integer(arg_value(
  "--observation-replicates", Sys.getenv("HOTARUBUKURO_JOINT_OBS_REPS", "20")
))
posterior_seed <- as.integer(arg_value("--posterior-seed", "20260725"))
observation_seed <- as.integer(arg_value("--observation-seed", "20261725"))

if (!is.finite(n_latent) || n_latent < 5000L) {
  stop("--latent-draws must be at least 5,000.", call. = FALSE)
}
if (!is.finite(n_replicates) || n_replicates < 5L) {
  stop("--observation-replicates must be at least 5.", call. = FALSE)
}
if (!is.finite(posterior_seed) || posterior_seed <= 0L ||
    !is.finite(observation_seed) || observation_seed <= 0L) {
  stop("Seeds must be positive integers.", call. = FALSE)
}

hb_require_stage_packages("natural_predictive_model")
hb_require_stage_packages("local_pigmented_isolates")
hb_load_modules("natural_predictive_model")
hb_load_modules("local_pigmented_isolates")

cells <- hb_read_csv(cells_path)
candidates <- hb_read_csv(candidate_path)
environment_terms <- v16_environment_terms(50)

if (!all(stats::complete.cases(cells[c(
  "x_km", "y_km", "n_pigmented", "n_observations", environment_terms
)]))) {
  stop("Joint PPC cells contain incomplete model inputs.", call. = FALSE)
}

# A local isolate can span a spatial-fold boundary. Cross-fitted maps are
# excellent for held-out prediction but stitch together posterior samples from
# different fitted models. For a nonlinear neighbourhood event, this script
# therefore uses one full-data fit solely as a posterior-predictive model check,
# preserving the joint spatial field across the whole 10-km neighbourhood.
basis <- v16_fold_predictors(
  cells, cells, environment_terms = environment_terms
)
mesh <- v16_make_mesh(cells)$mesh
train_X <- cbind(Intercept = 1, basis$train)
test_X <- cbind(Intercept = 1, basis$test)
A_train <- INLA::inla.spde.make.A(
  mesh, loc = as.matrix(cells[c("x_km", "y_km")])
)
A_test <- A_train
spde <- v16_make_spde(mesh, A_train)

stack_estimation <- INLA::inla.stack(
  data = list(
    y = as.integer(cells$n_pigmented),
    Ntrials = as.integer(cells$n_observations)
  ),
  A = list(A_train, 1),
  effects = list(spatial = seq_len(spde$n.spde), X = train_X),
  tag = "est", compress = FALSE, remove.unused = FALSE
)
stack_prediction <- INLA::inla.stack(
  data = list(
    y = rep(NA_real_, nrow(cells)),
    Ntrials = as.integer(cells$n_observations)
  ),
  A = list(A_test, 1),
  effects = list(spatial = seq_len(spde$n.spde), X = test_X),
  tag = "pred", compress = FALSE, remove.unused = FALSE
)
stack <- INLA::inla.stack(stack_estimation, stack_prediction)
stack_data <- INLA::inla.stack.data(stack)
fixed_terms <- colnames(train_X)
formula <- stats::as.formula(
  paste(
    "y ~ -1 +", paste(fixed_terms, collapse = " + "),
    "+ f(spatial, model = spde)"
  ),
  env = environment()
)

fit_time <- system.time({
  fit <- INLA::inla(
    formula,
    data = stack_data,
    family = "binomial",
    Ntrials = stack_data$Ntrials,
    control.predictor = list(
      A = INLA::inla.stack.A(stack), compute = TRUE, link = 1
    ),
    control.compute = list(config = TRUE),
    num.threads = 1L,
    blas.num.threads = 1L,
    verbose = FALSE
  )
})[["elapsed"]]

prediction_index <- INLA::inla.stack.index(stack, "pred")$data

# R-INLA documents that reproducible inla.posterior.sample() calls require both
# a positive GMRFLib seed and a controlled R .Random.seed. Set it immediately
# before this call, rather than relying on earlier process history.
set.seed(posterior_seed)
sample_time <- system.time({
  samples <- INLA::inla.posterior.sample(
    n = n_latent,
    result = fit,
    selection = list(APredictor = prediction_index),
    seed = posterior_seed,
    num.threads = 1L,
    parallel.configs = FALSE,
    add.names = TRUE
  )
})[["elapsed"]]

predictor_order <- v16_predictor_row_order(
  samples[[1L]], prediction_index, section = "APredictor"
)
eta <- vapply(
  samples,
  function(sample) as.numeric(sample$latent[predictor_order, 1L]),
  numeric(nrow(cells))
)
if (!is.matrix(eta)) eta <- matrix(eta, nrow = nrow(cells))
probability <- stats::plogis(eta)
rm(samples, eta, fit)
invisible(gc())

primary <- v20_configuration_table()
primary <- primary[primary$role == "primary", , drop = FALSE]
if (nrow(primary) != 1L) {
  stop("Expected exactly one primary isolate configuration.", call. = FALSE)
}
graph <- v20_neighbour_graph(
  cells,
  radius_km = primary$radius_km,
  environment_caliper = primary$environment_caliper,
  minimum_neighbours = primary$minimum_neighbours,
  same_fold_only = primary$same_fold_only
)
observed_profile <- v20_local_profile(
  as.integer(cells$n_pigmented), graph,
  primary$maximum_neighbour_pigment_share
)
observed_candidates <- which(observed_profile$candidate[, 1L])
observed_candidate_ids <- sort(as.character(cells$exact_site_id[observed_candidates]))
expected_candidate_ids <- sort(as.character(candidates$exact_site_id))
if (!identical(observed_candidate_ids, expected_candidate_ids)) {
  stop(
    "Joint PPC changed the pre-fixed observed candidate identity.",
    call. = FALSE
  )
}
observed_supported_present <- observed_profile$present[, 1L] &
  observed_profile$supported
observed_count <- sum(observed_profile$candidate[, 1L])
observed_fraction <- observed_count / max(sum(observed_supported_present), 1L)

# Audit how often the original local event crosses a fold boundary. This is the
# reason the joint full-data PPC is reported as a coherence sensitivity rather
# than pretending a stitched cross-fit mosaic is one joint posterior draw.
fold <- as.integer(cells$spatial_fold)
crosses_fold <- vapply(seq_len(nrow(cells)), function(index) {
  neighbour <- graph$neighbours[[index]]
  length(neighbour) > 0L && any(fold[neighbour] != fold[index])
}, logical(1))
boundary_audit <- data.frame(
  metric = c(
    "supported_cells", "supported_cells_crossing_fold",
    "supported_pigmented_cells", "supported_pigmented_cells_crossing_fold",
    "observed_candidates", "observed_candidates_crossing_fold"
  ),
  value = c(
    sum(graph$support$supported),
    sum(graph$support$supported & crosses_fold),
    sum(graph$support$supported & cells$n_pigmented > 0),
    sum(graph$support$supported & cells$n_pigmented > 0 & crosses_fold),
    length(observed_candidates),
    sum(crosses_fold[observed_candidates])
  ),
  stringsAsFactors = FALSE
)

trials <- as.integer(cells$n_observations)
count_by_latent <- matrix(NA_integer_, nrow = n_latent, ncol = n_replicates)
fraction_by_latent <- matrix(NA_real_, nrow = n_latent, ncol = n_replicates)

for (replicate_index in seq_len(n_replicates)) {
  message(
    "[joint-ppc] observation replicate ", replicate_index, "/", n_replicates
  )
  set.seed(observation_seed + replicate_index)
  counts <- matrix(
    stats::rbinom(
      length(probability),
      size = rep(trials, n_latent),
      prob = as.vector(probability)
    ),
    nrow = nrow(cells), ncol = n_latent
  )
  profile <- v20_local_profile(
    counts, graph, primary$maximum_neighbour_pigment_share
  )
  candidate_count <- colSums(profile$candidate)
  supported_present <- profile$present & matrix(
    profile$supported, nrow(profile$present), ncol(profile$present)
  )
  denominator <- pmax(colSums(supported_present), 1L)
  count_by_latent[, replicate_index] <- candidate_count
  fraction_by_latent[, replicate_index] <- candidate_count / denominator
  rm(counts, profile, supported_present)
  invisible(gc())
}

summarise_metric <- function(values, observed, metric) {
  flat <- as.numeric(values)
  upper_cluster <- rowMeans(values >= observed)
  lower_cluster <- rowMeans(values <= observed)
  p_upper <- mean(upper_cluster)
  p_lower <- mean(lower_cluster)
  mcse <- stats::sd(upper_cluster) / sqrt(nrow(values))
  z <- stats::qnorm(0.975)
  data.frame(
    configuration = primary$configuration,
    metric = metric,
    observed_value = observed,
    n_latent_draws = nrow(values),
    observation_replicates_per_latent = ncol(values),
    n_null_draws = length(flat),
    null_mean = mean(flat),
    null_sd = stats::sd(flat),
    lower_95 = unname(stats::quantile(flat, 0.025)),
    upper_95 = unname(stats::quantile(flat, 0.975)),
    empirical_p = p_upper,
    empirical_two_sided_p = min(1, 2 * min(p_upper, p_lower)),
    percentile = mean(flat <= observed),
    monte_carlo_se = mcse,
    mc_lower_95 = max(0, p_upper - z * mcse),
    mc_upper_95 = min(1, p_upper + z * mcse),
    stringsAsFactors = FALSE
  )
}

summary_table <- rbind(
  summarise_metric(count_by_latent, observed_count, "candidate_count"),
  summarise_metric(
    fraction_by_latent, observed_fraction, "candidate_fraction"
  )
)

null_table <- data.frame(
  latent_draw = rep(seq_len(n_latent), times = n_replicates),
  observation_replicate = rep(seq_len(n_replicates), each = n_latent),
  candidate_count = as.vector(count_by_latent),
  candidate_fraction = as.vector(fraction_by_latent),
  stringsAsFactors = FALSE
)

# Pre-declared five equal latent-draw shards show whether the estimated tail is
# driven by one portion of the posterior sample. These are diagnostics, not
# additional tests.
shard <- cut(
  seq_len(n_latent), breaks = 5L, labels = FALSE, include.lowest = TRUE
)
shard_rows <- lapply(sort(unique(shard)), function(group) {
  keep <- shard == group
  data.frame(
    shard = group,
    n_latent_draws = sum(keep),
    count_null_mean = mean(count_by_latent[keep, , drop = FALSE]),
    count_tail_probability = mean(
      count_by_latent[keep, , drop = FALSE] >= observed_count
    ),
    fraction_null_mean = mean(fraction_by_latent[keep, , drop = FALSE]),
    fraction_tail_probability = mean(
      fraction_by_latent[keep, , drop = FALSE] >= observed_fraction
    ),
    stringsAsFactors = FALSE
  )
})
shard_table <- do.call(rbind, shard_rows)

thread_fields <- c(
  "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS", "GOTO_NUM_THREADS",
  "BLIS_NUM_THREADS", "INLA_NUM_THREADS", "OMP_DYNAMIC"
)
metadata <- data.frame(
  field = c(
    "analysis_spec_version", "generated_utc", "model_role",
    "n_latent_draws", "observation_replicates_per_latent", "n_natural_maps",
    "posterior_seed", "observation_seed", "RNGkind", "candidate_count",
    "candidate_ids_sha256", "fit_elapsed_seconds", "sample_elapsed_seconds",
    thread_fields, "commit"
  ),
  value = c(
    "joint_submission_isolate_ppc_1.0",
    format(Sys.time(), tz = "UTC", usetz = TRUE),
    paste(
      "full-data joint posterior predictive coherence sensitivity;",
      "cross-fitted model remains the held-out predictive benchmark"
    ),
    n_latent, n_replicates, n_latent * n_replicates,
    posterior_seed, observation_seed,
    paste(RNGkind(), collapse = ";"),
    observed_count,
    digest::digest(observed_candidate_ids, algo = "sha256"),
    fit_time, sample_time,
    unname(Sys.getenv(thread_fields, unset = "unset")),
    rp_git_commit()
  ),
  stringsAsFactors = FALSE
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
checkpoint <- file.path(
  output_dir,
  paste0("joint_presence_probability_draws", n_latent, ".rds")
)
rp_save_rds_atomic(
  list(
    analysis_spec_version = "joint_submission_isolate_ppc_1.0",
    cell_id = as.character(cells$exact_site_id),
    trials = trials,
    probability = probability,
    posterior_seed = posterior_seed,
    environment_terms = environment_terms,
    formula = paste(deparse(formula), collapse = " ")
  ),
  checkpoint
)
rp_write_csv_atomic(
  summary_table,
  file.path(output_dir, "joint_isolate_natural_null_summary.csv")
)
rp_write_csv_atomic(
  null_table,
  file.path(output_dir, "joint_isolate_natural_null.csv")
)
rp_write_csv_atomic(
  data.frame(exact_site_id = observed_candidate_ids, stringsAsFactors = FALSE),
  file.path(output_dir, "joint_candidate_ids.csv")
)
rp_write_csv_atomic(
  boundary_audit,
  file.path(output_dir, "crossfit_boundary_audit.csv")
)
rp_write_csv_atomic(
  shard_table,
  file.path(output_dir, "joint_ppc_shard_stability.csv")
)
metadata <- rbind(
  metadata,
  data.frame(
    field = "probability_checkpoint_sha256",
    value = rp_sha256(checkpoint),
    stringsAsFactors = FALSE
  )
)
rp_write_csv_atomic(
  metadata,
  file.path(output_dir, "joint_isolate_ppc_metadata.csv")
)
manifest_paths <- c(
  checkpoint,
  file.path(output_dir, "joint_isolate_natural_null_summary.csv"),
  file.path(output_dir, "joint_isolate_natural_null.csv"),
  file.path(output_dir, "joint_candidate_ids.csv"),
  file.path(output_dir, "crossfit_boundary_audit.csv"),
  file.path(output_dir, "joint_ppc_shard_stability.csv"),
  file.path(output_dir, "joint_isolate_ppc_metadata.csv")
)
rp_write_manifest(
  rp_manifest_rows(
    manifest_paths,
    role = "joint_submission_ppc_sensitivity",
    note = "full-data joint spatial posterior; no human variable enters selection"
  ),
  file.path(output_dir, "joint_submission_ppc_manifest.csv")
)

cat(
  "Joint submission isolate PPC completed with ", n_latent,
  " latent draws x ", n_replicates, " observation replicates (",
  n_latent * n_replicates, " natural maps).\n", sep = ""
)
print(summary_table)
print(boundary_audit)
