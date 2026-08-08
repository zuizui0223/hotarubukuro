#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) return(args[[i + 1L]])
  prefix <- paste0(flag, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

output_dir <- arg_value("--output", "results/bombus_ecology_sensitivity")
summary_path <- file.path(output_dir, "bombus_ecology_sensitivity_summary.csv")
cell_path <- file.path(output_dir, "cell_bombus_support.csv")
calibration_path <- file.path(output_dir, "occurrence_support_calibration.csv")
design_path <- file.path(output_dir, "bombus_ecology_sensitivity_design.csv")
required <- c(summary_path, cell_path, calibration_path, design_path)
if (!all(file.exists(required))) {
  stop("Missing Bombus ecology sensitivity output(s): ", paste(required[!file.exists(required)], collapse = ", "), call. = FALSE)
}

summary <- utils::read.csv(summary_path, check.names = FALSE, stringsAsFactors = FALSE)
cells <- utils::read.csv(cell_path, check.names = FALSE, stringsAsFactors = FALSE)
calibration <- utils::read.csv(calibration_path, check.names = FALSE, stringsAsFactors = FALSE)
design <- utils::read.csv(design_path, check.names = FALSE, stringsAsFactors = FALSE)

species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
groups <- c(
  "all_five", "widespread", "montane_alpine",
  paste0("leave_out_", species)
)
normalizations <- c("flower_cell_rank", "occurrence_site_ecdf")
thresholds <- c(0.10, 0.20, 0.25, 0.33)
responses <- c("pigmentation_share", "pigmented_only_intensity")
expected_n <- length(groups) * length(normalizations) * length(thresholds) * length(responses)

if (nrow(summary) != expected_n) {
  stop("Expected ", expected_n, " sensitivity summary rows; found ", nrow(summary), call. = FALSE)
}
if (!setequal(unique(summary$normalization), normalizations)) {
  stop("Sensitivity output has unexpected normalization set.", call. = FALSE)
}
if (!setequal(unique(summary$exposure_group), groups)) {
  stop("Sensitivity output has unexpected exposure-group set.", call. = FALSE)
}
if (!setequal(unique(summary$response), responses)) {
  stop("Sensitivity output has unexpected response set.", call. = FALSE)
}
if (!setequal(round(unique(summary$low_threshold), 8), thresholds)) {
  stop("Sensitivity output has unexpected low-threshold grid.", call. = FALSE)
}
key <- paste(
  summary$normalization, summary$exposure_group,
  summary$low_threshold, summary$response, sep = "::"
)
if (anyDuplicated(key)) stop("Sensitivity summary keys are duplicated.", call. = FALSE)
if (any(!is.finite(summary$n_pairs)) || any(summary$n_pairs < 0)) {
  stop("Sensitivity summary has invalid pair counts.", call. = FALSE)
}

positive_pair <- summary$n_pairs > 0
if (any(positive_pair & !is.finite(summary$observed_directed_difference))) {
  stop("Positive-pair sensitivity rows must have finite observed differences.", call. = FALSE)
}
if (any(positive_pair & !is.finite(summary$upper_tail_p))) {
  stop("Positive-pair sensitivity rows must have finite predictive p-values.", call. = FALSE)
}
if (any(is.finite(summary$upper_tail_p) & (summary$upper_tail_p <= 0 | summary$upper_tail_p > 1))) {
  stop("Predictive p-values must lie in (0, 1].", call. = FALSE)
}

raw_columns <- paste0(species, "_suitability")
rank_columns <- paste0(species, "_flower_cell_rank")
ecdf_columns <- paste0(species, "_occurrence_site_ecdf")
missing_cell_columns <- setdiff(
  c("exact_site_id", "common_rebuilt_sdm_support", raw_columns, rank_columns, ecdf_columns),
  names(cells)
)
if (length(missing_cell_columns)) {
  stop("Cell-support output lacks: ", paste(missing_cell_columns, collapse = ", "), call. = FALSE)
}
if (sum(as.logical(cells$common_rebuilt_sdm_support)) < 1000L) {
  stop("Unexpectedly small rebuilt-SDM common-support cell set.", call. = FALSE)
}
for (nm in c(rank_columns, ecdf_columns)) {
  x <- as.numeric(cells[[nm]])
  if (any(is.finite(x) & (x < 0 | x > 1))) {
    stop(nm, " contains support values outside [0,1].", call. = FALSE)
  }
}

if (!setequal(calibration$species, species) || nrow(calibration) != length(species)) {
  stop("Occurrence calibration must contain exactly the five focal species.", call. = FALSE)
}
if (any(calibration$n_unique_occurrence_cells_with_prediction < 20)) {
  stop("Occurrence calibration has too few unique occurrence cells.", call. = FALSE)
}
quantile_columns <- c(
  "suitability_q10", "suitability_q20", "suitability_q25",
  "suitability_q33", "suitability_q50"
)
for (r in seq_len(nrow(calibration))) {
  q <- as.numeric(calibration[r, quantile_columns])
  if (any(!is.finite(q)) || any(diff(q) < -1e-12)) {
    stop("Occurrence suitability quantiles are invalid for ", calibration$species[[r]], call. = FALSE)
  }
}

auc_weight <- design$value[design$metric == "AUC_weighting_used"]
if (length(auc_weight) != 1L || toupper(as.character(auc_weight)) != "FALSE") {
  stop("AUC weighting must remain disabled.", call. = FALSE)
}

focus <- summary[
  summary$low_threshold == 0.33 &
    summary$response == "pigmentation_share" &
    summary$exposure_group %in% c("all_five", "widespread", "montane_alpine"),
  c(
    "normalization", "exposure_group", "n_pairs",
    "observed_directed_difference", "upper_tail_p", "BH_q_within_group_grid"
  ),
  drop = FALSE
]
print(focus, row.names = FALSE)
cat("Bombus ecology sensitivity validation passed.\n")
