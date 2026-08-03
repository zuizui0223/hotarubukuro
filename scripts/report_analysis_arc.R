# Report the reconstruction as the paper's narrative arc.
#
# The pipeline reports itself as a list of stages, which is the right shape for
# debugging and the wrong shape for reading. This walks the five steps of the
# manuscript's argument in order and prints what the run actually produced at
# each one, so the arc can be checked end to end rather than reassembled from
# stage logs.
#
# It computes nothing. Every number here is read from a file another stage
# wrote; if a stage did not run, its step is reported as absent rather than
# filled in from anywhere else.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

report_dir <- arg_value("--report-dir", "reproducibility")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

read_if <- function(path) {
  if (!file.exists(path)) return(NULL)
  out <- try(
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    silent = TRUE
  )
  if (inherits(out, "try-error")) NULL else out
}
fmt <- function(x, digits = 3) {
  if (is.null(x) || !length(x) || !is.finite(suppressWarnings(as.numeric(x[[1L]])))) {
    return("not available")
  }
  format(round(as.numeric(x[[1L]]), digits), nsmall = 0L)
}

lines <- c(
  "# The reconstruction, read as the paper's argument",
  "",
  paste(
    "Five steps, in the order the manuscript makes them. Every value below was",
    "read from a file this run produced. A step whose stage did not run is",
    "reported as absent; nothing is carried over from a previous run or from",
    "the published outputs."
  ),
  ""
)

step <- function(number, title, body) {
  c(paste0("## ", number, ". ", title), "", body, "")
}

# --- 1. Broad environmental and spatial structure ---------------------------
performance <- read_if(
  "results/ecological_v16_predictive_replication/predictive_replication_model_performance.csv"
)
if (is.null(performance)) {
  lines <- c(lines, step(1, "Broad environmental and spatial structure",
    "Absent: the natural predictive model stage produced no performance table."))
} else {
  presence <- performance[
    performance$model == "national_environment_spde_presence", , drop = FALSE
  ]
  intensity <- performance[
    performance$model == "national_environment_spde_intensity", , drop = FALSE
  ]
  lines <- c(lines, step(1, "Broad environmental and spatial structure", c(
    paste0(
      "Cross-fitted over five response-blind 100-km folds, 1,000 predictive ",
      "draws per held-out cell."
    ),
    "",
    paste0("- pigmentation presence AUC: ", fmt(presence$AUC)),
    paste0("- conditional intensity RMSE: ", fmt(intensity$RMSE)),
    paste0("- components fitted: ", nrow(performance))
  )))
}

# --- 2. Correspondence with the local bumblebee community -------------------
turnover <- read_if(
  "results/ecological_v17_local_pair_turnover/local_pair_predictive_summary.csv"
)
if (is.null(turnover)) {
  lines <- c(lines, step(2, "Correspondence with the local bumblebee community",
    "Absent: the local turnover stage produced no summary."))
} else {
  primary <- turnover[turnover$radius_km %in% 25, , drop = FALSE]
  if (!nrow(primary)) primary <- turnover
  rows <- vapply(seq_len(nrow(primary)), function(i) {
    paste0(
      "- ", primary$response[i], ": partial beta=",
      fmt(primary$observed_partial_beta[i]),
      ", p=", fmt(primary$beta_empirical_p[i], 4),
      ", BH q=", fmt(primary$BH_q_primary_25km[i], 4)
    )
  }, character(1))
  lines <- c(lines, step(2, "Correspondence with the local bumblebee community",
    c("At the pre-specified 25-km scale:", "", rows)))
}

# --- 3. Local departure from the natural baseline ---------------------------
isolate_summary <- read_if(
  "results/ecological_v20_local_white_isolates/local_isolate_natural_null_summary.csv"
)
candidates <- read_if(
  "results/ecological_v20_local_white_isolates/local_isolate_candidates.csv"
)
directional <- if (is.null(candidates)) {
  "Absent: the local-isolate stage produced no candidate table."
} else {
  c(
    paste0(
      "Directional event, as in the manuscript: a pigmented 1-km cell whose ",
      "environment-similar neighbours within 10 km are all observed white."
    ),
    "",
    paste0("- candidates: ", nrow(candidates)),
    paste0(
      "- candidate cells: ",
      paste(utils::head(candidates$exact_site_id, 40), collapse = ", ")
    )
  )
}
lines <- c(lines, step(3, "Local departure from the natural baseline", directional))

# --- 3b. The same departure without assuming a direction --------------------
asymmetry <- read_if(
  "results/ecological_v23_local_state_asymmetry/local_state_asymmetry_summary.csv"
)
agnostic <- if (is.null(asymmetry)) {
  paste(
    "Absent: the symmetry diagnostic did not run. The directional result above",
    "therefore stands without a direction-agnostic comparison."
  )
} else {
  rows <- vapply(seq_len(nrow(asymmetry)), function(i) {
    paste0(
      "- ", asymmetry$metric[i], ": observed=",
      fmt(asymmetry$observed_value[i]),
      ", null mean=", fmt(asymmetry$null_mean[i]),
      ", two-sided p=", fmt(asymmetry$two_sided_p[i], 4)
    )
  }, character(1))
  c(
    paste(
      "The step above assumes pigmented-among-white is the signal. This",
      "replays the same neighbourhood graph and the same natural null without",
      "that assumption, counting a white cell among pigmented neighbours as",
      "the same kind of departure. Post hoc symmetry diagnostic; it selects no",
      "candidates and changes nothing downstream."
    ),
    "",
    rows
  )
}
lines <- c(
  lines,
  step("3b", "The same departure without assuming a direction", agnostic)
)

# --- 4. Human-landscape characterisation of the candidates ------------------
human <- read_if(
  paste0(
    "results/ecological_v21_local_human_neighbourhood/",
    "human_neighbourhood_contrast_summary.csv"
  )
)
if (is.null(human)) {
  lines <- c(lines, step(4, "Human-landscape characterisation of the candidates",
    "Absent: the human-context stage produced no contrast summary."))
} else {
  passed <- sum(human$maxT_FWER_p < 0.05, na.rm = TRUE)
  lines <- c(lines, step(4, "Human-landscape characterisation of the candidates", c(
    paste(
      "Computed only after the candidates and their white-neighbour",
      "comparison sets were fixed."
    ),
    "",
    paste0("- contrasts tested: ", nrow(human)),
    paste0("- passing familywise maxT at 0.05: ", passed),
    paste0(
      "- smallest maxT p: ",
      fmt(min(human$maxT_FWER_p, na.rm = TRUE), 4)
    )
  )))
}

# --- 5. The light DOY check on the horticultural hypothesis -----------------
lines <- c(lines, step(5, "The light DOY check on the horticultural hypothesis", c(
  paste(
    "Withdrawn. The v16 phenology component that carried this step aborts",
    "inside INLA's sampler and fold 4 never completed in any measured",
    "configuration, so `early_predictive_q` and the Figure 4d early-flowering",
    "series no longer exist. See docs/public-reconstruction.md."
  ),
  "",
  paste(
    "This step of the argument is therefore currently unsupported by the",
    "reconstruction. It is reported here rather than quietly dropped, because",
    "the arc is stated in the manuscript with five steps and this run",
    "delivers four."
  )
)))

writeLines(lines, file.path(report_dir, "analysis_arc.md"), useBytes = TRUE)
cat(paste(lines, collapse = "\n"), "\n")
