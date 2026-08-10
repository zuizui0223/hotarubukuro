# Report the active 1,909 reconstruction as the paper's narrative arc.
args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
report_dir <- arg_value("--report-dir", "reproducibility")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

read_if <- function(path) {
  if (!file.exists(path)) return(NULL)
  out <- try(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE), silent = TRUE)
  if (inherits(out, "try-error")) NULL else out
}
fmt <- function(x, digits = 3) {
  if (is.null(x) || !length(x) || !is.finite(suppressWarnings(as.numeric(x[[1L]])))) {
    return("not available")
  }
  format(round(as.numeric(x[[1L]]), digits), nsmall = 0L)
}
step <- function(number, title, body) c(paste0("## ", number, ". ", title), "", body, "")

lines <- c(
  "# The reconstruction, read as the paper's argument", "",
  paste(
    "The paper uses one measurement framework, one national natural baseline,",
    "a directional local Bombus-limitation hypothesis, and a separate",
    "post-selection human-context extension. Every number below is read from",
    "fresh outputs from this run."
  ), ""
)

performance <- read_if(
  "results/ecological_v16_predictive_replication/predictive_replication_model_performance.csv"
)
if (is.null(performance)) {
  lines <- c(lines, step(1, "Broad environmental and spatial structure",
    "Absent: the natural predictive model stage produced no performance table."))
} else {
  presence <- performance[performance$model == "national_environment_spde_presence", , drop = FALSE]
  intensity <- performance[performance$model == "national_environment_spde_intensity", , drop = FALSE]
  lines <- c(lines, step(1, "Broad environmental and spatial structure", c(
    "Five response-blind 100-km folds with 1,000 predictive draws per held-out cell.", "",
    paste0("- pigmentation presence AUC: ", fmt(presence$AUC)),
    paste0("- conditional intensity RMSE: ", fmt(intensity$RMSE)),
    paste0("- components fitted: ", nrow(performance))
  )))
}

gate <- read_if(
  "results/ecological_v17_bombus_limitation_gate/bombus_limitation_gate_summary.csv"
)
meta <- read_if(
  "results/ecological_v17_bombus_limitation_gate/bombus_limitation_gate_metadata.csv"
)
if (is.null(gate)) {
  lines <- c(lines, step(2, "Local Bombus limitation and pigmentation benefit",
    "Absent: the Bombus limitation-gate stage produced no summary."))
} else {
  primary <- gate[gate$is_primary_gate, , drop = FALSE]
  p <- primary[primary$response == "pigmentation_share", , drop = FALSE]
  i <- primary[primary$response == "pigmented_only_intensity", , drop = FALSE]
  lines <- c(lines, step(2, "Local Bombus limitation and pigmentation benefit", c(
    paste(
      "Nearby cells were matched before reading flower colour. The active",
      "lower-third gate contrasts cells where all five focal Bombus species",
      "have low within-species predicted support against cells where at least",
      "one species is at or above median support."
    ), "",
    paste0("- matched pigmentation pairs: ", p$n_pairs),
    paste0("- pigmentation difference, available - limited: ", fmt(p$observed_directed_difference)),
    paste0("- natural-map upper-tail p: ", fmt(p$upper_tail_p, 4)),
    paste0("- across-grid BH q: ", fmt(p$BH_q_all_gate_tests, 4)),
    paste0("- pigmented-only intensity difference: ", fmt(i$observed_directed_difference)),
    paste0("- intensity upper-tail p: ", fmt(i$upper_tail_p, 4)),
    "",
    paste(
      "The gate was adopted after exploratory design development for biological",
      "interpretability; the full threshold grid and its multiplicity remain",
      "visible. SDM support is predicted availability, not visitation pressure."
    )
  )))
}

candidates <- read_if(
  "results/ecological_v20_local_white_isolates/local_isolate_candidates.csv"
)
if (is.null(candidates)) {
  lines <- c(lines, step(3, "Local departure from the natural baseline",
    "Absent: the local-isolate stage produced no candidate table."))
} else {
  lines <- c(lines, step(3, "Local departure from the natural baseline", c(
    paste(
      "A separate event asks where a pigmented 1-km cell occurs among",
      "environment-similar white neighbours. This does not use the Bombus gate."
    ), "",
    paste0("- candidates: ", nrow(candidates)),
    paste0("- candidate cells: ", paste(utils::head(candidates$exact_site_id, 40), collapse = ", "))
  )))
}

human <- read_if(
  "results/ecological_v21_local_human_neighbourhood/human_neighbourhood_contrast_summary.csv"
)
if (is.null(human)) {
  lines <- c(lines, step(4, "Human-landscape characterisation of candidates",
    "Absent: the human-context stage produced no contrast summary."))
} else {
  lines <- c(lines, step(4, "Human-landscape characterisation of candidates", c(
    "Computed only after candidates and comparison neighbourhoods were fixed.", "",
    paste0("- contrasts tested: ", nrow(human)),
    paste0("- passing familywise maxT at 0.05: ", sum(human$maxT_FWER_p < 0.05, na.rm = TRUE)),
    paste0("- smallest maxT p: ", fmt(min(human$maxT_FWER_p, na.rm = TRUE), 4))
  )))
}

doy <- read_if("results/ecological_v24_candidate_doy_check/candidate_doy_summary.csv")
if (is.null(doy)) {
  lines <- c(lines, step(5, "Held-out flowering-date description",
    "Absent: the supplementary flowering-date stage produced no summary."))
} else {
  rows <- vapply(seq_len(nrow(doy)), function(i) paste0(
    "- ", doy$role[i], " (", doy$neighbour_set[i], " neighbours): mean ",
    fmt(doy$mean_difference_days[i], 2), " days; ",
    doy$n_with_usable_neighbours[i], " usable cells"
  ), character(1))
  lines <- c(lines, step(5, "Held-out flowering-date description", c(
    "Supplementary only; no model, selection, ranking, or main claim.", "", rows
  )))
}

writeLines(lines, file.path(report_dir, "analysis_arc.md"), useBytes = TRUE)
cat(paste(lines, collapse = "\n"), "\n")
