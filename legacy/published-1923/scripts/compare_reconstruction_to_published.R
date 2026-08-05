# Describe the public reconstruction alongside the published analysis.
#
# NOT a reproduction check and NOT a robustness test. The published analysis is
# computed on 1,923 observations; the public reconstruction is computed on 1,909
# because the published analysis-input tables are lost and the population is
# rebuilt from public sources (see docs/established-inputs.md). The two are
# therefore different analysis populations by construction.
#
# This script reports, per quantity: the published number, the reconstructed
# number, their difference, and the relative difference. It issues no verdict —
# no robust/differs, no pass/fail, no threshold. Whether a difference matters is
# a scientific judgement for the reader, not something a script can settle from
# two numbers computed on different data.
#
# A quantity present in only one of the two is reported with the other side
# blank rather than being dropped or scored.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
published_dir <- arg_value("--published", "inputs/published_reference")
lock_dir <- arg_value("--lock", "results/final_analysis_pipeline")
report_dir <- arg_value("--report-dir", "reproducibility")

# A comparison is only a comparison if both sides came from different places.
#
# When the pipeline stops early it leaves the repository's committed publication
# outputs sitting on disk, and those are the very files inputs/published_reference
# was copied from. Reading them as "reconstructed" compares a file with itself
# and reports every difference as exactly zero, which renders as total
# robustness. That is the most misleading output this script could produce, so
# the reconstructed side is only accepted when this run actually wrote it.
run_started <- suppressWarnings(
  as.numeric(Sys.getenv("HOTARUBUKURO_RUN_STARTED", ""))
)
regenerated <- function(path) {
  if (!file.exists(path)) return(FALSE)
  if (!isTRUE(is.finite(run_started))) return(NA)
  as.numeric(file.info(path)$mtime) >= run_started
}

not_regenerated <- character()
read_optional <- function(path) {
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}
# Every read of a reconstructed artifact goes through this, so a stale file can
# never reach a verdict.
read_reconstructed <- function(path) {
  if (!file.exists(path)) return(NULL)
  fresh <- regenerated(path)
  if (isTRUE(fresh) || is.na(fresh)) return(read_optional(path))
  not_regenerated <<- unique(c(not_regenerated, path))
  NULL
}
published <- function(name) read_optional(file.path(published_dir, name))

rows <- list()
# Descriptive only. This script reports the published number, the reconstructed
# number and the difference between them. It does not decide whether a
# difference matters: no robust/differs verdict, no pass/fail. Interpretation is
# the reader's, and the two analyses are computed on different observation sets,
# so a difference here is a description of that, not a test of anything.
add <- function(section, quantity, published_value, reconstructed_value,
                note = "") {
  numeric_pair <- is.numeric(published_value) && is.numeric(reconstructed_value)
  rows[[length(rows) + 1L]] <<- data.frame(
    section = section,
    quantity = quantity,
    published = published_value,
    reconstructed = reconstructed_value,
    difference = if (numeric_pair) reconstructed_value - published_value else NA_real_,
    relative_difference = if (numeric_pair && is.finite(published_value) &&
                              abs(published_value) > 0) {
      (reconstructed_value - published_value) / abs(published_value)
    } else NA_real_,
    note = note,
    stringsAsFactors = FALSE
  )
}


# ---------------------------------------------------------------------------
# 1. Sample size
# ---------------------------------------------------------------------------
published_measure <- published("pigmentation_measurement_summary.csv")
reconstructed_measure <- read_reconstructed(paste0(
  "results/ecological_v11_pigmentation_hurdle/",
  "pigmentation_measurement_summary.csv"
))
if (!is.null(published_measure) && !is.null(reconstructed_measure)) {
  for (field in c("n", "n_white", "n_pigmented")) {
    if (!field %in% names(published_measure)) next
    add(
      "sample size", field,
      as.numeric(published_measure[[field]][[1L]]),
      as.numeric(reconstructed_measure[[field]][[1L]]),
      note = "observation counts differ by construction; recorded for context"
    )
  }
}

published_performance <- published("predictive_replication_model_performance.csv")
reconstructed_performance <- read_reconstructed(paste0(
  "results/ecological_v16_predictive_replication/",
  "predictive_replication_model_performance.csv"
))
if (!is.null(published_performance) && !is.null(reconstructed_performance)) {
  merged <- merge(
    published_performance[c("model", "n", "n_observations")],
    reconstructed_performance[c("model", "n", "n_observations")],
    by = "model", suffixes = c("_published", "_reconstructed")
  )
  for (index in seq_len(nrow(merged))) {
    add(
      "sample size", paste0(merged$model[[index]], ": 1-km cells"),
      merged$n_published[[index]], merged$n_reconstructed[[index]],
      note = "analysis population of the cross-fitted model"
    )
  }
}

# ---------------------------------------------------------------------------
# 2. Environmental model
# ---------------------------------------------------------------------------
# The published reference also carries a `national_environment_year_spde_phenology`
# row. That component has been withdrawn, so the reconstruction produces no
# counterpart and the loop below skips the model outright; the withdrawal is
# described in docs/public-reconstruction.md rather than shown as a difference.
if (!is.null(published_performance) && !is.null(reconstructed_performance)) {
  metrics <- list(
    list(model = "national_environment_spde_presence", field = "AUC",
         tolerance = 0.05, kind = "discrimination"),
    list(model = "national_environment_spde_intensity", field = "RMSE",
         tolerance = 0.10, kind = "error"),
    list(model = "common_support_environment_spde_presence", field = "AUC",
         tolerance = 0.05, kind = "discrimination"),
    list(model = "common_support_environment_spde_bombus_presence",
         field = "AUC", tolerance = 0.05, kind = "discrimination")
  )
  for (metric in metrics) {
    published_row <- published_performance[
      published_performance$model == metric$model, , drop = FALSE
    ]
    reconstructed_row <- reconstructed_performance[
      reconstructed_performance$model == metric$model, , drop = FALSE
    ]
    if (!nrow(published_row) || !nrow(reconstructed_row)) next
    published_value <- as.numeric(published_row[[metric$field]][[1L]])
    reconstructed_value <- as.numeric(reconstructed_row[[metric$field]][[1L]])
    add(
      "environmental model",
      paste0(metric$model, ": ", metric$field),
      published_value, reconstructed_value,
      note = if (identical(metric$kind, "discrimination")) {
        "cross-fitted discrimination; compare the values, not a threshold"
      } else {
        "error scale; no threshold claim in the manuscript"
      }
    )
  }
}

published_registry <- published("final_result_registry.csv")
reconstructed_registry <- read_reconstructed(
  file.path(lock_dir, "final_result_registry.csv")
)

registry_value <- function(table, id, field) {
  if (is.null(table)) return(NA_real_)
  row <- table[table$result_id == id, , drop = FALSE]
  if (!nrow(row) || !field %in% names(row)) return(NA_real_)
  as.numeric(row[[field]][[1L]])
}

if (!is.null(published_registry) && !is.null(reconstructed_registry)) {
  add(
    "environmental model", "national_bombus_auc_gain",
    registry_value(published_registry, "national_bombus_auc_gain", "estimate"),
    registry_value(reconstructed_registry, "national_bombus_auc_gain", "estimate"),
    note = "national Bombus AUC gain"
  )
}

# ---------------------------------------------------------------------------
# 3. Local Bombus turnover
# ---------------------------------------------------------------------------
if (!is.null(published_registry) && !is.null(reconstructed_registry)) {
  for (id in c("local_bombus_presence", "local_bombus_intensity")) {
    published_beta <- registry_value(published_registry, id, "estimate")
    reconstructed_beta <- registry_value(reconstructed_registry, id, "estimate")
    add(
      "local Bombus", paste0(id, ": partial beta"),
      published_beta, reconstructed_beta,
      note = "25-km partial turnover slope"
    )
    published_q <- registry_value(published_registry, id, "corrected_p")
    reconstructed_q <- registry_value(reconstructed_registry, id, "corrected_p")
    add(
      "local Bombus", paste0(id, ": corrected p"),
      published_q, reconstructed_q,
      note = "corrected p-value; reported as a number, not scored"
    )
  }
}

# ---------------------------------------------------------------------------
# 4. Local colour-state discordance
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# 5. Human context
# ---------------------------------------------------------------------------
if (!is.null(published_registry) && !is.null(reconstructed_registry)) {
  add(
    "local isolates", "local_isolate_count",
    registry_value(published_registry, "local_isolate_count", "estimate"),
    registry_value(reconstructed_registry, "local_isolate_count", "estimate"),
    note = "candidate definition; depends on the analysis population"
  )
  # The count, the fraction, each one's 1,000-map null mean, and each one's
  # natural-null p. The fraction is the scale-free form of the same event, so
  # reporting only the count would hide a difference that arises because the
  # two runs have different numbers of eligible cells.
  add(
    "local isolates", "local_isolate_count: natural-null mean",
    registry_value(published_registry, "local_isolate_count", "null_reference"),
    registry_value(
      reconstructed_registry, "local_isolate_count", "null_reference"
    ),
    note = "mean candidate count across 1,000 replicated natural maps"
  )
  add(
    "local isolates", "local_isolate_count: natural-null p",
    registry_value(published_registry, "local_isolate_count", "raw_p"),
    registry_value(reconstructed_registry, "local_isolate_count", "raw_p"),
    note = paste(
      "isolate count against the natural null",
      "rather than becoming a positive finding"
    )
  )
  add(
    "local isolates", "local_isolate_fraction",
    registry_value(published_registry, "local_isolate_fraction", "estimate"),
    registry_value(
      reconstructed_registry, "local_isolate_fraction", "estimate"
    ),
    note = "candidates as a fraction of eligible pigmented cells"
  )
  add(
    "local isolates", "local_isolate_fraction: natural-null mean",
    registry_value(
      published_registry, "local_isolate_fraction", "null_reference"
    ),
    registry_value(
      reconstructed_registry, "local_isolate_fraction", "null_reference"
    ),
    note = "mean candidate fraction across 1,000 replicated natural maps"
  )
  add(
    "local isolates", "local_isolate_fraction: natural-null p",
    registry_value(published_registry, "local_isolate_fraction", "raw_p"),
    registry_value(reconstructed_registry, "local_isolate_fraction", "raw_p"),
    note = paste(
      "the fraction is the scale-free form of the same event;",
      "read it beside the count rather than instead of it"
    )
  )
  for (id in c("local_population_5km", "local_population_did_alignment")) {
    published_estimate <- registry_value(published_registry, id, "estimate")
    reconstructed_estimate <- registry_value(reconstructed_registry, id, "estimate")
    add(
      "human context", paste0(id, ": estimate"),
      published_estimate, reconstructed_estimate,
      note = "exploratory contrast estimate"
    )
    published_q <- registry_value(published_registry, id, "corrected_p")
    reconstructed_q <- registry_value(reconstructed_registry, id, "corrected_p")
    add(
      "human context", paste0(id, ": corrected p"),
      published_q, reconstructed_q,
      note = paste(
        "corrected p-value for the exploratory contrast",
        "significance"
      )
    )
  }
  add(
    "human context", "did_proximate_candidate_fraction",
    registry_value(
      published_registry, "did_proximate_candidate_fraction", "estimate"
    ),
    registry_value(
      reconstructed_registry, "did_proximate_candidate_fraction", "estimate"
    ),
    note = "DID-proximate isolate fraction"
  )
}

comparison <- do.call(rbind, rows)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

# If the run did not regenerate the artifacts, there is no comparison to report.
# Say so, loudly, instead of publishing verdicts drawn from stale files.
if (length(not_regenerated)) {
  reason <- paste0(
    "The pipeline did not regenerate these artifacts during this run:\n  ",
    paste(not_regenerated, collapse = "\n  "),
    "\nThey are the repository's committed publication outputs, left on disk by ",
    "a run that stopped early. Comparing them against inputs/published_reference ",
    "would compare a file with itself and report every difference as zero, which ",
    "reads as perfect agreement. Nothing has been reported."
  )
  if (!is.null(comparison) && nrow(comparison)) {
    comparison$note <- "NOT_COMPARABLE: see reason above"
  } else {
    comparison <- data.frame(
      section = "comparison", quantity = "all",
      published = NA_real_, reconstructed = NA_real_, difference = NA_real_,
      note = "the pipeline produced no reconstructed artifacts",
      stringsAsFactors = FALSE
    )
  }
  rp_write_csv_atomic(
    comparison, file.path(report_dir, "reconstruction_vs_published.csv")
  )
  rp_write_lines_atomic(
    c(
      "# The public reconstruction versus the published analysis",
      "",
      "## No comparison was possible",
      "",
      strsplit(reason, "\n", fixed = TRUE)[[1L]]
    ),
    file.path(report_dir, "reconstruction_vs_published.md")
  )
  message(reason)
  stop(
    "Refusing to report a comparison against artifacts this run did not ",
    "produce.", call. = FALSE
  )
}
rp_write_csv_atomic(
  comparison, file.path(report_dir, "reconstruction_vs_published.csv")
)

# ---------------------------------------------------------------------------
# Human-readable report.
# ---------------------------------------------------------------------------
format_value <- function(x) {
  if (!is.finite(x)) return("—")
  if (abs(x) >= 1000 || (abs(x) < 0.001 && x != 0)) {
    formatC(x, format = "g", digits = 4)
  } else {
    formatC(x, format = "f", digits = 4)
  }
}

lines <- c(
  "# The published analysis and the public reconstruction, side by side",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  "",
  "**This is a description, not a test.** The published analysis is computed on",
  "1,923 observations. The public reconstruction is computed on 1,909, because",
  "the published analysis-input tables are lost and the population is rebuilt",
  "from `Data_S1.csv` and the pinned public environmental sources; see",
  "`docs/established-inputs.md`. Both run the same locked pipeline, but on",
  "different observation sets, so the numbers are not expected to match.",
  "",
  "No verdict is issued here. Each row gives the published value, the",
  "reconstructed value and the difference between them. Whether a difference",
  "matters is a scientific judgement about two analyses of different data, and",
  "is left to the reader.",
  ""
)

for (section in unique(comparison$section)) {
  block <- comparison[comparison$section == section, , drop = FALSE]
  lines <- c(
    lines,
    paste0("## ", toupper(substring(section, 1, 1)), substring(section, 2)),
    "",
    "| quantity | published (1,923) | reconstruction (1,909) | difference | relative | note |",
    "|---|---:|---:|---:|---:|---|",
    paste0(
      "| ", block$quantity,
      " | ", vapply(block$published, format_value, character(1)),
      " | ", vapply(block$reconstructed, format_value, character(1)),
      " | ", vapply(block$difference, format_value, character(1)),
      " | ", ifelse(
        is.na(block$relative_difference), "\u2014",
        paste0(format(round(100 * block$relative_difference, 1), trim = TRUE), "%")
      ),
      " | ", block$note, " |"
    ),
    ""
  )
}

rp_write_lines_atomic(
  lines, file.path(report_dir, "reconstruction_vs_published.md")
)

print(
  comparison[c("section", "quantity", "published", "reconstructed",
               "difference")],
  row.names = FALSE
)
cat(sprintf(
  "\n%d quantities described. No verdict issued.\n", nrow(comparison)
))
