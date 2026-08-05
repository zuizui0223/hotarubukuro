# Is the snapshot we are about to analyse the established analysis input?
#
# The frozen upstream audits in validation/ already assert this, but they assert
# it one stage into the run and they report a failed check rather than a
# quantity. This runs immediately after snapshot verification and states the
# comparison directly, because "the inputs are not the published inputs" is a
# completely different problem from "a stage crashed" and the two should not
# look alike in a workflow log.
#
# It never modifies anything and never substitutes a value. It reports.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
expectations_path <- arg_value(
  "--expectations", "inputs/established_input_expectations.csv"
)
report_dir <- arg_value("--report-dir", "reproducibility")
strict <- hb_as_bool(arg_value("--strict", "false"))

expectations <- utils::read.csv(
  expectations_path, check.names = FALSE, stringsAsFactors = FALSE
)

observe <- function(quantity) {
  observations_path <- paste0(
    "results/ecological_v11_pigmentation_hurdle/",
    "analysis_data_pigmentation_hurdle.csv"
  )
  summary_path <- paste0(
    "results/ecological_v11_pigmentation_hurdle/",
    "pigmentation_measurement_summary.csv"
  )
  if (quantity == "analysis_observations") {
    if (!file.exists(observations_path)) return(NA_real_)
    return(nrow(utils::read.csv(
      observations_path, check.names = FALSE, stringsAsFactors = FALSE
    )))
  }
  if (!file.exists(summary_path)) return(NA_real_)
  measurement <- utils::read.csv(
    summary_path, check.names = FALSE, stringsAsFactors = FALSE
  )
  column <- switch(
    quantity,
    white_observations = "n_white",
    pigmented_observations = "n_pigmented",
    NA_character_
  )
  if (is.na(column) || !column %in% names(measurement)) return(NA_real_)
  as.numeric(measurement[[column]][[1L]])
}

rows <- list()
for (index in seq_len(nrow(expectations))) {
  entry <- expectations[index, , drop = FALSE]
  observed <- observe(entry$quantity)
  expected <- as.numeric(entry$expectation)
  rows[[length(rows) + 1L]] <- data.frame(
    quantity = entry$quantity,
    expected = expected,
    observed = observed,
    difference = observed - expected,
    status = if (!is.na(observed) && observed == expected) "PASS" else "FAIL",
    source_of_expectation = entry$source,
    note = entry$note,
    stringsAsFactors = FALSE
  )
}
report <- do.call(rbind, rows)

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
rp_write_csv_atomic(report, file.path(report_dir, "input_fidelity_report.csv"))
print(
  report[c("quantity", "expected", "observed", "difference", "status")],
  row.names = FALSE
)

failures <- sum(report$status != "PASS")
if (failures > 0L) {
  message(
    "\n", failures, " of ", nrow(report),
    " established-input expectations are not met.\n",
    "  meaning: the analysis inputs in this snapshot are not the tables the\n",
    "    published results were computed from, so the frozen upstream audits\n",
    "    in validation/ will refuse the run at stage 01.\n",
    "  cause: the published analysis-input tables no longer exist. They were\n",
    "    never committed, they are absent from the recorded publication commit,\n",
    "    from both published snapshots, and from every retained artifact, so the\n",
    "    snapshot is rebuilt from Data_S1.csv and the pinned public rasters. Row\n",
    "    inclusion is decided by complete.cases over the extracted environmental\n",
    "    covariates, so any coverage difference in a public raster changes the\n",
    "    analysis population.\n",
    "  status: this is a recorded, searched-for loss, not a configuration\n",
    "    mistake. docs/established-inputs.md sets out what was searched and what\n",
    "    survives. The published quantities have deliberately not been\n",
    "    re-baselined onto the reconstruction, so this check is expected to fail\n",
    "    until the original tables resurface.\n"
  )
} else {
  cat("\nThe snapshot matches the established analysis inputs.\n")
}

if (strict && failures > 0L) {
  stop("Established-input fidelity check failed.", call. = FALSE)
}
