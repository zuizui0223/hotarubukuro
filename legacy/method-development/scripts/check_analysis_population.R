args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
expectations_path <- arg_value(
  "--expectations", "inputs/analysis_1909_expectations.csv"
)
report_dir <- arg_value("--report-dir", "reproducibility")
strict <- hb_as_bool(arg_value("--strict", "true"))

expectations <- utils::read.csv(
  expectations_path, check.names = FALSE, stringsAsFactors = FALSE
)
required <- c("quantity", "expectation", "source", "note")
if (!all(required %in% names(expectations))) {
  stop("Expectation file is missing: ",
       paste(setdiff(required, names(expectations)), collapse = ", "),
       call. = FALSE)
}

observations_path <- file.path(
  "results", "ecological_v11_pigmentation_hurdle",
  "analysis_data_pigmentation_hurdle.csv"
)
summary_path <- file.path(
  "results", "ecological_v11_pigmentation_hurdle",
  "pigmentation_measurement_summary.csv"
)

observe <- function(quantity) {
  if (identical(quantity, "analysis_observations")) {
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

report <- do.call(rbind, lapply(seq_len(nrow(expectations)), function(index) {
  entry <- expectations[index, , drop = FALSE]
  observed <- observe(entry$quantity)
  expected <- as.numeric(entry$expectation)
  data.frame(
    quantity = entry$quantity,
    expected = expected,
    observed = observed,
    difference = observed - expected,
    status = if (!is.na(observed) && observed == expected) "PASS" else "FAIL",
    source = entry$source,
    note = entry$note,
    stringsAsFactors = FALSE
  )
}))

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
rp_write_csv_atomic(
  report, file.path(report_dir, "analysis_population_check.csv")
)
print(report[c("quantity", "expected", "observed", "difference", "status")],
      row.names = FALSE)

failures <- sum(report$status != "PASS")
if (failures) {
  message(
    failures, " active 1,909 population invariants failed. ",
    "Do not fit models on this snapshot until the input mismatch is resolved."
  )
}
if (strict && failures) {
  stop("Active analysis-population check failed.", call. = FALSE)
}
cat("Active analysis population verified.\n")
