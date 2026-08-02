# Numerical regression check against the locked publication values.
#
# Reproducing an analysis is not the same as re-running it. This compares the
# result registry a fresh run produced against the values recorded when the
# manuscript analysis was locked, using a tolerance per quantity rather than one
# global epsilon, because the quantities are not all of the same kind.
#
# Three tolerance kinds are used:
#   exact     - the quantity is a pure function of the immutable inputs, so any
#               difference is a defect. Tolerance zero.
#   absolute  - the quantity is a draw or permutation summary; the tolerance is
#               stated on its own scale.
#   relative  - as above, but the tolerance is a fraction of the reference.
#
# Every row also carries an invariant: the qualitative property the manuscript
# actually relies on (a sign, a threshold, an exact equality). A value can drift
# within tolerance, but an invariant that breaks is a changed conclusion, and
# both are reported separately so the difference stays visible.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
reference_path <- arg_value("--reference", "inputs/numerical_reference.csv")
registry_path <- arg_value(
  "--registry", "results/final_analysis_pipeline/final_result_registry.csv"
)
report_dir <- arg_value("--report-dir", "reproducibility")
strict <- hb_as_bool(arg_value("--strict", "true"))
not_run_reason <- arg_value("--not-run", "")

# A comparison that did not happen still has to leave a record, and that record
# has to say so. Writing nothing would leave the previous run's report in place;
# writing a passing report would claim a verification that never ran.
if (nzchar(not_run_reason)) {
  reference <- utils::read.csv(
    reference_path, check.names = FALSE, stringsAsFactors = FALSE
  )
  report <- data.frame(
    result_id = reference$result_id,
    field = reference$field,
    reference_value = as.numeric(reference$reference_value),
    observed_value = NA_real_,
    difference = NA_real_,
    tolerance_kind = reference$tolerance_kind,
    tolerance = NA_real_,
    status = "NOT_RUN",
    detail = not_run_reason,
    invariant = reference$invariant,
    invariant_status = "NONE",
    invariant_detail = "",
    justification = reference$justification,
    stringsAsFactors = FALSE
  )
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  rp_write_csv_atomic(
    report, file.path(report_dir, "numerical_regression_report.csv")
  )
  cat("Numerical regression not run: ", not_run_reason, "\n", sep = "")
  cat(nrow(report), " comparisons recorded as NOT_RUN.\n", sep = "")
  quit(save = "no", status = 0)
}

if (!file.exists(reference_path)) {
  stop(
    "Numerical reference not found: ", reference_path, ".\n",
    "  expected: the committed table of locked publication values.\n",
    "  remediation: restore inputs/numerical_reference.csv from version control.",
    call. = FALSE
  )
}
if (!file.exists(registry_path)) {
  stop(
    "Result registry not found: ", registry_path, ".\n",
    "  expected: the registry written by the publication lock stage.\n",
    "  remediation: run scripts/run_publication_pipeline.R before this check.",
    call. = FALSE
  )
}

reference <- utils::read.csv(
  reference_path, check.names = FALSE, stringsAsFactors = FALSE
)
registry <- utils::read.csv(
  registry_path, check.names = FALSE, stringsAsFactors = FALSE
)

check_invariant <- function(expression, value) {
  if (is.na(expression) || !nzchar(expression)) {
    return(list(status = "NONE", detail = ""))
  }
  parts <- strsplit(expression, ":", fixed = TRUE)[[1L]]
  operator <- parts[[1L]]
  bound <- if (length(parts) > 1L) as.numeric(parts[[2L]]) else NA_real_
  if (is.na(value)) {
    return(list(status = "FAIL", detail = "observed value is missing"))
  }
  held <- switch(
    operator,
    eq = TRUE,
    gt = value > bound,
    lt = value < bound,
    abs_lt = abs(value) < bound,
    abs_gt = abs(value) > bound,
    NA
  )
  if (is.na(held)) {
    return(list(
      status = "FAIL", detail = paste0("unknown invariant: ", expression)
    ))
  }
  list(
    status = if (held) "PASS" else "FAIL",
    detail = sprintf("%s against %.6g", expression, value)
  )
}

rows <- list()
for (index in seq_len(nrow(reference))) {
  entry <- reference[index, , drop = FALSE]
  match <- registry[registry$result_id == entry$result_id, , drop = FALSE]
  observed <- NA_real_
  status <- "PASS"
  detail <- ""
  if (!nrow(match)) {
    status <- "FAIL"
    detail <- "result_id absent from the regenerated registry"
  } else if (!entry$field %in% names(match)) {
    status <- "FAIL"
    detail <- paste0("field absent from the registry: ", entry$field)
  } else {
    observed <- suppressWarnings(as.numeric(match[[entry$field]][[1L]]))
  }

  expected <- as.numeric(entry$reference_value)
  allowed <- switch(
    entry$tolerance_kind,
    exact = 0,
    absolute = as.numeric(entry$tolerance),
    relative = abs(expected) * as.numeric(entry$tolerance),
    NA_real_
  )
  difference <- if (is.na(observed)) NA_real_ else observed - expected

  if (status == "PASS") {
    if (is.na(allowed)) {
      status <- "FAIL"
      detail <- paste0("unknown tolerance_kind: ", entry$tolerance_kind)
    } else if (is.na(observed)) {
      status <- "FAIL"
      detail <- "regenerated value is not numeric"
    } else if (abs(difference) > allowed) {
      status <- "FAIL"
      detail <- sprintf(
        "|%.10g - %.10g| = %.3g exceeds %s tolerance %.3g",
        observed, expected, abs(difference), entry$tolerance_kind, allowed
      )
    } else {
      detail <- sprintf(
        "within %s tolerance %.3g", entry$tolerance_kind, allowed
      )
    }
  }

  invariant <- check_invariant(entry$invariant, observed)
  rows[[length(rows) + 1L]] <- data.frame(
    result_id = entry$result_id,
    field = entry$field,
    reference_value = expected,
    observed_value = observed,
    difference = difference,
    tolerance_kind = entry$tolerance_kind,
    tolerance = allowed,
    status = status,
    detail = detail,
    invariant = entry$invariant,
    invariant_status = invariant$status,
    invariant_detail = invariant$detail,
    justification = entry$justification,
    stringsAsFactors = FALSE
  )
}

report <- do.call(rbind, rows)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
rp_write_csv_atomic(
  report, file.path(report_dir, "numerical_regression_report.csv")
)

print(
  report[c("result_id", "field", "reference_value", "observed_value",
           "difference", "tolerance", "status", "invariant_status")],
  row.names = FALSE
)

unchecked <- setdiff(registry$result_id, reference$result_id)
if (length(unchecked)) {
  cat(
    "\nRegistry results with no committed reference value: ",
    paste(unchecked, collapse = ", "), "\n", sep = ""
  )
}

tolerance_failures <- sum(report$status != "PASS")
invariant_failures <- sum(report$invariant_status == "FAIL")
cat(sprintf(
  "\n%d comparisons; %d outside tolerance; %d broken invariants.\n",
  nrow(report), tolerance_failures, invariant_failures
))

if (strict && (tolerance_failures > 0L || invariant_failures > 0L)) {
  stop(
    "Numerical regression check failed. See ",
    file.path(report_dir, "numerical_regression_report.csv"),
    " for the per-quantity comparison and the tolerance justifications.",
    call. = FALSE
  )
}
