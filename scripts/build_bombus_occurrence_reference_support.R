#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
sdm_root <- arg_value(
  "--sdm-root", "source_bombus/results/bombus_sdm_rebuild_A"
)
output_dir <- arg_value(
  "--output", "results/bombus_occurrence_reference_support"
)

required_packages <- c("ENMeval", "maxnet")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}
if (!file.exists(cells_path)) stop("Missing flower-cell table: ", cells_path, call. = FALSE)

cells <- utils::read.csv(cells_path, check.names = FALSE, stringsAsFactors = FALSE)
species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
focal_species <- c("ardens", "diversus")
raw_columns <- paste0("bee_", species)
required_columns <- c("exact_site_id", raw_columns)
missing_columns <- setdiff(required_columns, names(cells))
if (length(missing_columns)) {
  stop("Flower-cell table is missing: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

seed_paths <- function(short) {
  c(
    model = file.path(sdm_root, "models", paste0(short, "_ENMeval.rds")),
    selected = file.path(sdm_root, "evaluation", paste0(short, "_selected_model.csv"))
  )
}
seeded <- vapply(species, function(short) all(file.exists(seed_paths(short))), logical(1))
missing_focal <- focal_species[!seeded[match(focal_species, species)]]
if (length(missing_focal)) {
  stop(
    "Missing seeded SDM model/evaluation for focal species: ",
    paste(missing_focal, collapse = ", "),
    call. = FALSE
  )
}
seeded_species <- species[seeded]

# Each materialized species is put on an occurrence-referenced support scale.
# The exact-reproduction contract guarantees the two focal species used by the
# manuscript Main analysis (ardens and diversus). Auxiliary montane species are
# calibrated when their seeds are present, but their absence must not block
# reconstruction of the locked focal exposure.
occurrence_reference <- function(short) {
  paths <- seed_paths(short)
  evaluation <- readRDS(paths[["model"]])
  selected_table <- utils::read.csv(paths[["selected"]], check.names = FALSE, stringsAsFactors = FALSE)
  selected <- as.integer(selected_table$selected_row[[1]])
  models <- ENMeval::eval.models(evaluation)
  if (!is.finite(selected) || selected < 1L || selected > length(models)) {
    stop("Invalid selected ENMeval row for ", short, call. = FALSE)
  }

  model <- models[[selected]]
  occ <- as.data.frame(ENMeval::eval.occs(evaluation))
  occ_prediction <- suppressWarnings(as.numeric(stats::predict(
    model, newdata = occ, type = "cloglog", clamp = TRUE
  )))
  occ_prediction <- occ_prediction[is.finite(occ_prediction)]
  if (length(occ_prediction) < 20L) {
    stop("Too few occurrence-reference predictions for ", short, call. = FALSE)
  }

  ref_ecdf <- stats::ecdf(occ_prediction)
  raw <- suppressWarnings(as.numeric(cells[[paste0("bee_", short)]]))
  score <- rep(NA_real_, length(raw))
  keep <- is.finite(raw)
  score[keep] <- as.numeric(ref_ecdf(raw[keep]))

  list(
    score = score,
    summary = data.frame(
      species = short,
      selected_row = selected,
      n_occurrence_reference = length(occ_prediction),
      occurrence_prediction_q10 = unname(stats::quantile(occ_prediction, 0.10)),
      occurrence_prediction_q50 = unname(stats::quantile(occ_prediction, 0.50)),
      occurrence_prediction_q90 = unname(stats::quantile(occ_prediction, 0.90)),
      flower_score_min = min(score, na.rm = TRUE),
      flower_score_median = stats::median(score, na.rm = TRUE),
      flower_score_max = max(score, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  )
}

reference <- lapply(seeded_species, occurrence_reference)
names(reference) <- seeded_species
score_matrix <- matrix(
  NA_real_, nrow = nrow(cells), ncol = length(species),
  dimnames = list(NULL, paste0(species, "_occurrence_reference"))
)
for (short in seeded_species) {
  score_matrix[, paste0(short, "_occurrence_reference")] <- reference[[short]]$score
}
calibration <- do.call(rbind, lapply(reference, `[[`, "summary"))

raw_matrix <- as.matrix(cells[, raw_columns, drop = FALSE])
storage.mode(raw_matrix) <- "double"

effective_occmax <- pmax(
  score_matrix[, "ardens_occurrence_reference"],
  score_matrix[, "diversus_occurrence_reference"]
)
effective_occmean <- rowMeans(score_matrix[, c(
  "ardens_occurrence_reference", "diversus_occurrence_reference"
), drop = FALSE])
effective_rawmax <- pmax(raw_matrix[, "bee_ardens"], raw_matrix[, "bee_diversus"])

all_five_seeded <- all(seeded)
if (all_five_seeded) {
  all5_occmax <- apply(score_matrix, 1, max)
  all5_occmean <- rowMeans(score_matrix)
} else {
  all5_occmax <- rep(NA_real_, nrow(cells))
  all5_occmean <- rep(NA_real_, nrow(cells))
}

cell_support <- data.frame(
  exact_site_id = cells$exact_site_id,
  score_matrix,
  effective_occmax = effective_occmax,
  effective_occmean = effective_occmean,
  effective_rawmax = effective_rawmax,
  all5_occmax = all5_occmax,
  all5_occmean = all5_occmean,
  stringsAsFactors = FALSE
)

if (any(!is.finite(cell_support$effective_occmax))) {
  stop("Non-finite primary focal-pollinator support detected.", call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  cell_support,
  file.path(output_dir, "cell_effective_bombus_support.csv"),
  row.names = FALSE
)
utils::write.csv(
  calibration,
  file.path(output_dir, "occurrence_reference_calibration.csv"),
  row.names = FALSE
)

writeLines(c(
  "# Occurrence-referenced Bombus support",
  "",
  "Primary manuscript exposure: max(A_ardens, A_diversus).",
  "A_k is an empirical-CDF support score relative to predictions at that species' occurrence cells.",
  "It is not occurrence probability, abundance, visitation, pollen transfer, or selection pressure.",
  if (all_five_seeded) {
    "All-five summaries are retained for Supporting Information sensitivity/biogeography."
  } else {
    paste0(
      "Exact reproduction materialized focal seeds only; all-five summaries are NA. ",
      "Auxiliary seeds present: ", paste(setdiff(seeded_species, focal_species), collapse = ", ")
    )
  }
), file.path(output_dir, "README.md"))

cat(
  "Wrote occurrence-referenced Bombus support for ", nrow(cell_support),
  " flower cells using seeded species: ", paste(seeded_species, collapse = ", "), ".\n",
  sep = ""
)
