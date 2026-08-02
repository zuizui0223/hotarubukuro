# Compare the public reconstruction against the published analysis.
#
# This is not a reproduction check. The published analysis inputs are lost, so
# the two analyses are computed from different observation sets and their
# numbers are not expected to match. The question is narrower and more useful:
# do the qualitative conclusions survive when the whole pipeline is rebuilt from
# nothing but declared, reproducible public sources?
#
# Each comparison therefore carries two verdicts:
#
#   agreement  - whether the reconstructed value is close to the published one,
#                on a scale stated per quantity. Informative, not decisive.
#   conclusion - whether the thing the manuscript actually claims still holds:
#                the sign of an effect, which side of a significance threshold a
#                test falls on, whether a discrimination measure stays useful.
#                This is what "robust" and "differs" refer to.
#
# A quantity with no published counterpart is reported as `new_analysis` rather
# than being silently dropped or scored.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/reproducibility.R")

arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)
published_dir <- arg_value("--published", "inputs/published_reference")
lock_dir <- arg_value("--lock", "results/final_analysis_pipeline")
report_dir <- arg_value("--report-dir", "reproducibility")
discordance_dir <- arg_value(
  "--discordance", "results/ecological_v23_local_state_asymmetry"
)

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
add <- function(section, quantity, published_value, reconstructed_value,
                agreement = NA_character_, conclusion = NA_character_,
                note = "") {
  rows[[length(rows) + 1L]] <<- data.frame(
    section = section,
    quantity = quantity,
    published = published_value,
    reconstructed = reconstructed_value,
    difference = if (is.numeric(published_value) &&
                     is.numeric(reconstructed_value)) {
      reconstructed_value - published_value
    } else {
      NA_real_
    },
    agreement = agreement,
    conclusion = conclusion,
    note = note,
    stringsAsFactors = FALSE
  )
}

# A published effect and a reconstructed effect agree in conclusion when they
# point the same way. A null result agrees when both stay null.
same_sign <- function(a, b) {
  if (!is.finite(a) || !is.finite(b)) return(NA)
  sign(a) == sign(b)
}
same_side <- function(a, b, threshold = 0.05) {
  if (!is.finite(a) || !is.finite(b)) return(NA)
  (a < threshold) == (b < threshold)
}
verdict <- function(holds) {
  if (is.na(holds)) "indeterminate" else if (holds) "robust" else "differs"
}
band <- function(a, b, tolerance) {
  if (!is.finite(a) || !is.finite(b)) return(NA_character_)
  if (abs(b - a) <= tolerance) "close" else "apart"
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
      agreement = NA_character_,
      conclusion = "not a conclusion",
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
      conclusion = "not a conclusion",
      note = "analysis population of the cross-fitted model"
    )
  }
}

# ---------------------------------------------------------------------------
# 2. Environmental model
# ---------------------------------------------------------------------------
if (!is.null(published_performance) && !is.null(reconstructed_performance)) {
  metrics <- list(
    list(model = "national_environment_spde_presence", field = "AUC",
         tolerance = 0.05, kind = "discrimination"),
    list(model = "national_environment_spde_intensity", field = "RMSE",
         tolerance = 0.10, kind = "error"),
    list(model = "national_environment_year_spde_phenology", field = "RMSE",
         tolerance = 2.0, kind = "error"),
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
    # The conclusion an AUC carries is that the environmental baseline
    # discriminates usefully. 0.75 is the floor below which the manuscript's
    # "the natural model predicts the pigmentation surface" reading would no
    # longer hold.
    holds <- if (identical(metric$kind, "discrimination")) {
      (published_value >= 0.75) == (reconstructed_value >= 0.75)
    } else {
      NA
    }
    add(
      "environmental model",
      paste0(metric$model, ": ", metric$field),
      published_value, reconstructed_value,
      agreement = band(published_value, reconstructed_value, metric$tolerance),
      conclusion = if (identical(metric$kind, "discrimination")) {
        verdict(holds)
      } else {
        "magnitude only"
      },
      note = if (identical(metric$kind, "discrimination")) {
        "conclusion: discrimination stays above 0.75"
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
    agreement = band(
      registry_value(published_registry, "national_bombus_auc_gain", "estimate"),
      registry_value(reconstructed_registry, "national_bombus_auc_gain", "estimate"),
      0.01
    ),
    conclusion = verdict({
      a <- registry_value(published_registry, "national_bombus_auc_gain", "estimate")
      b <- registry_value(reconstructed_registry, "national_bombus_auc_gain", "estimate")
      if (!is.finite(a) || !is.finite(b)) NA else (abs(a) < 0.02) == (abs(b) < 0.02)
    }),
    note = "conclusion: the national Bombus gain stays small (|gain| < 0.02)"
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
      agreement = band(published_beta, reconstructed_beta, 0.05),
      conclusion = verdict(same_sign(published_beta, reconstructed_beta)),
      note = "conclusion: the turnover association points the same way"
    )
    published_q <- registry_value(published_registry, id, "corrected_p")
    reconstructed_q <- registry_value(reconstructed_registry, id, "corrected_p")
    add(
      "local Bombus", paste0(id, ": corrected p"),
      published_q, reconstructed_q,
      agreement = band(published_q, reconstructed_q, 0.05),
      conclusion = verdict(same_side(published_q, reconstructed_q)),
      note = "conclusion: same side of the 0.05 correction threshold"
    )
  }
}

# ---------------------------------------------------------------------------
# 4. Local colour-state discordance
# ---------------------------------------------------------------------------
discordance <- read_reconstructed(
  file.path(discordance_dir, "local_state_asymmetry_summary.csv")
)
if (is.null(discordance)) {
  add(
    "local discordance", "bidirectional discordance diagnostic",
    NA_real_, NA_real_, conclusion = "not run",
    note = "no discordance output was found"
  )
} else {
  primary <- discordance[
    discordance$state_rule == "legacy_any_pigmented", , drop = FALSE
  ]
  if (!nrow(primary)) primary <- discordance
  for (metric in c("count_difference", "rate_difference", "log_rate_ratio")) {
    row <- primary[primary$metric == metric, , drop = FALSE]
    if (!nrow(row)) next
    observed <- as.numeric(row$observed_value[[1L]])
    two_sided <- as.numeric(row$two_sided_p[[1L]])
    add(
      "local discordance", paste0(metric, " (observed)"),
      NA_real_, observed,
      conclusion = "new_analysis",
      note = "no published counterpart; the diagnostic is new in this analysis"
    )
    add(
      "local discordance", paste0(metric, " (two-sided p vs natural maps)"),
      NA_real_, two_sided,
      conclusion = if (!is.finite(two_sided)) {
        "indeterminate"
      } else if (two_sided < 0.05) {
        "discordance exceeds the natural baseline"
      } else {
        "compatible with the natural baseline"
      },
      note = "post hoc diagnostic against the cross-fitted natural maps"
    )
  }
}

# ---------------------------------------------------------------------------
# 5. Human context
# ---------------------------------------------------------------------------
if (!is.null(published_registry) && !is.null(reconstructed_registry)) {
  add(
    "human context", "local_isolate_count",
    registry_value(published_registry, "local_isolate_count", "estimate"),
    registry_value(reconstructed_registry, "local_isolate_count", "estimate"),
    conclusion = "not a conclusion",
    note = "candidate definition; depends on the analysis population"
  )
  published_p <- registry_value(published_registry, "local_isolate_count", "raw_p")
  reconstructed_p <- registry_value(
    reconstructed_registry, "local_isolate_count", "raw_p"
  )
  add(
    "human context", "local_isolate_count: natural-null p",
    published_p, reconstructed_p,
    agreement = band(published_p, reconstructed_p, 0.10),
    conclusion = verdict(same_side(published_p, reconstructed_p)),
    note = paste(
      "conclusion: the isolate count stays compatible with the natural model",
      "rather than becoming a positive finding"
    )
  )
  for (id in c("local_population_5km", "local_population_did_alignment")) {
    published_estimate <- registry_value(published_registry, id, "estimate")
    reconstructed_estimate <- registry_value(reconstructed_registry, id, "estimate")
    add(
      "human context", paste0(id, ": estimate"),
      published_estimate, reconstructed_estimate,
      agreement = band(published_estimate, reconstructed_estimate, 0.05),
      conclusion = verdict(same_sign(published_estimate, reconstructed_estimate)),
      note = "conclusion: the exploratory contrast points the same way"
    )
    published_q <- registry_value(published_registry, id, "corrected_p")
    reconstructed_q <- registry_value(reconstructed_registry, id, "corrected_p")
    add(
      "human context", paste0(id, ": corrected p"),
      published_q, reconstructed_q,
      agreement = band(published_q, reconstructed_q, 0.05),
      conclusion = verdict(same_side(published_q, reconstructed_q)),
      note = paste(
        "conclusion: stays in the exploratory tier rather than crossing into",
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
    agreement = band(
      registry_value(
        published_registry, "did_proximate_candidate_fraction", "estimate"
      ),
      registry_value(
        reconstructed_registry, "did_proximate_candidate_fraction", "estimate"
      ),
      0.15
    ),
    conclusion = verdict({
      a <- registry_value(
        published_registry, "did_proximate_candidate_fraction", "estimate"
      )
      b <- registry_value(
        reconstructed_registry, "did_proximate_candidate_fraction", "estimate"
      )
      if (!is.finite(a) || !is.finite(b)) NA else (a > 0.5) == (b > 0.5)
    }),
    note = "conclusion: most isolates still fall in DID-proximate context"
  )
}

comparison <- do.call(rbind, rows)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

# A survey run is not a reference reconstruction, and no verdict may be drawn
# from one. The freshness gate below catches the case where a stage produced
# nothing, but a survey run can fail a late stage while every artifact this
# script reads was regenerated — so freshness alone would let it through. The
# run mode is checked directly for that reason.
run_mode_path <- file.path(lock_dir, "run_mode.txt")
if (file.exists(run_mode_path)) {
  run_mode_lines <- readLines(run_mode_path, warn = FALSE)
  if (any(grepl("^run_mode=survey$", run_mode_lines))) {
    stop(
      "Refusing to compare: ", lock_dir, " was produced by a survey run ",
      "(--continue-on-failure). Survey runs exist to enumerate downstream ",
      "failures during development and may contain outputs from stages whose ",
      "upstream dependencies failed. Rerun the pipeline in canonical mode and ",
      "compare that.",
      call. = FALSE
    )
  }
}

# If the run did not regenerate the artifacts, there is no comparison to report.
# Say so, loudly, instead of publishing verdicts drawn from stale files.
if (length(not_regenerated)) {
  reason <- paste0(
    "The pipeline did not regenerate these artifacts during this run:\n  ",
    paste(not_regenerated, collapse = "\n  "),
    "\nThey are the repository's committed publication outputs, left on disk by ",
    "a run that stopped early. Comparing them against inputs/published_reference ",
    "would compare a file with itself and report every difference as zero, which ",
    "reads as complete robustness. No verdicts have been issued."
  )
  if (!is.null(comparison) && nrow(comparison)) {
    comparison$conclusion <- "NOT_COMPARABLE"
    comparison$agreement <- NA_character_
  } else {
    comparison <- data.frame(
      section = "comparison", quantity = "all",
      published = NA_real_, reconstructed = NA_real_, difference = NA_real_,
      agreement = NA_character_, conclusion = "NOT_COMPARABLE",
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
    "Refusing to report a robustness verdict against artifacts this run did ",
    "not produce.", call. = FALSE
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
  "# The public reconstruction versus the published analysis",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  "",
  "The published analysis inputs are lost, so this is not a reproduction. Both",
  "analyses run the same locked pipeline; they differ in their observation set,",
  "because the reconstruction is built entirely from `Data_S1.csv` and the",
  "pinned public environmental sources. The numbers are not expected to match.",
  "What is being asked is whether the conclusions survive.",
  "",
  "`conclusion` is the column that matters: `robust` means the claim the",
  "manuscript makes still holds, `differs` means it does not.",
  ""
)

for (section in unique(comparison$section)) {
  block <- comparison[comparison$section == section, , drop = FALSE]
  lines <- c(
    lines,
    paste0("## ", toupper(substring(section, 1, 1)), substring(section, 2)),
    "",
    "| quantity | published | reconstructed | difference | agreement | conclusion |",
    "|---|---:|---:|---:|---|---|",
    paste0(
      "| ", block$quantity,
      " | ", vapply(block$published, format_value, character(1)),
      " | ", vapply(block$reconstructed, format_value, character(1)),
      " | ", vapply(block$difference, format_value, character(1)),
      " | ", ifelse(is.na(block$agreement), "—", block$agreement),
      " | ", block$conclusion, " |"
    ),
    ""
  )
}

scored <- comparison[comparison$conclusion %in% c("robust", "differs"), , drop = FALSE]
robust_count <- sum(scored$conclusion == "robust")
lines <- c(
  lines,
  "## Verdict",
  "",
  paste0(
    robust_count, " of ", nrow(scored),
    " testable conclusions are reproduced by the public reconstruction."
  ),
  ""
)
if (nrow(scored) && robust_count < nrow(scored)) {
  lines <- c(
    lines,
    "Conclusions that do not survive:",
    "",
    paste0(
      "- **", scored$quantity[scored$conclusion == "differs"], "** — ",
      scored$note[scored$conclusion == "differs"]
    ),
    ""
  )
}

rp_write_lines_atomic(
  lines, file.path(report_dir, "reconstruction_vs_published.md")
)

print(
  comparison[c("section", "quantity", "published", "reconstructed",
               "conclusion")],
  row.names = FALSE
)
cat(sprintf(
  "\n%d comparisons; %d of %d testable conclusions reproduced.\n",
  nrow(comparison), robust_count, nrow(scored)
))
