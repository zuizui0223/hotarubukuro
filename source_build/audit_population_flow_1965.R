#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) return(args[[i + 1L]])
  prefix <- paste0(flag, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

input <- arg_value("--input", "Data_S1.csv")
coverage_path <- arg_value(
  "--coverage",
  "source-artifact/results/bombus_sdm_rebuild_A/flower_prediction_coverage.csv"
)
output_dir <- arg_value("--output", "results/population_flow_1965")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

x <- utils::read.csv(input, check.names = FALSE, stringsAsFactors = FALSE)
coverage <- utils::read.csv(coverage_path, check.names = FALSE, stringsAsFactors = FALSE)
if (nrow(x) != 1965L) {
  stop("Expected the curated source table to contain 1,965 rows; got ", nrow(x), call. = FALSE)
}
required <- c(
  "observation_id", "source_row", "image_sha256", "duplicate_image_sha256",
  "date", "latitude", "longitude", "R", "G", "B"
)
missing <- setdiff(required, names(x))
if (length(missing)) stop("Data_S1 missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
sp <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
if (!all(c("observation_index", sp, "all_five_finite") %in% names(coverage))) {
  stop("Coverage table lacks required columns.", call. = FALSE)
}
if (nrow(coverage) != nrow(x) || !identical(as.integer(coverage$observation_index), seq_len(nrow(x)))) {
  stop("Bombus flower coverage does not align one-to-one with Data_S1 row order.", call. = FALSE)
}

as_bool <- function(z) tolower(trimws(as.character(z))) %in% c("true", "t", "1", "yes")
source_order <- order(suppressWarnings(as.numeric(x$source_row)), seq_len(nrow(x)), na.last = TRUE)
canonical_duplicate <- logical(nrow(x))
canonical_duplicate[source_order] <- duplicated(as.character(x$image_sha256[source_order]))
flag_duplicate <- as_bool(x$duplicate_image_sha256)
colour_formable <- is.finite(suppressWarnings(as.numeric(x$R))) &
  is.finite(suppressWarnings(as.numeric(x$G))) &
  is.finite(suppressWarnings(as.numeric(x$B)))
coords_formable <- is.finite(suppressWarnings(as.numeric(x$longitude))) &
  is.finite(suppressWarnings(as.numeric(x$latitude)))
inside_mainland_bbox <- coords_formable &
  as.numeric(x$longitude) >= 128 & as.numeric(x$longitude) <= 144 &
  as.numeric(x$latitude) >= 30 & as.numeric(x$latitude) <= 41.5
bombus_all_finite <- as_bool(coverage$all_five_finite)

# Do not use warning flags to remove observations. The source-table policy is
# measurement formability first, then one retained representative per exact
# image hash, followed by analysis-specific data availability.
stage0 <- rep(TRUE, nrow(x))
stage1 <- stage0 & colour_formable
stage2 <- stage1 & !canonical_duplicate
stage3 <- stage2 & coords_formable
stage4 <- stage3 & bombus_all_finite

flow <- data.frame(
  stage = c(
    "00_curated_source", "01_formable_primary_colour", "02_exact_image_dedup",
    "03_formable_coordinates", "04_new_bombus_all_five_finite"
  ),
  rule = c(
    "all curated Data_S1 rows",
    "finite primary R/G/B; QC warnings retained",
    "stable source-row order; keep first occurrence of each exact image_sha256",
    "finite longitude and latitude",
    "all five predictions finite from seeded Bombus rebuild A"
  ),
  n_included = c(sum(stage0), sum(stage1), sum(stage2), sum(stage3), sum(stage4)),
  dropped_this_stage = c(
    0L, sum(stage0 & !stage1), sum(stage1 & !stage2),
    sum(stage2 & !stage3), sum(stage3 & !stage4)
  ),
  stringsAsFactors = FALSE
)

rows <- data.frame(
  observation_index = seq_len(nrow(x)),
  observation_id = x$observation_id,
  source_row = x$source_row,
  date = x$date,
  latitude = suppressWarnings(as.numeric(x$latitude)),
  longitude = suppressWarnings(as.numeric(x$longitude)),
  colour_formable = colour_formable,
  exact_hash_extra_row = canonical_duplicate,
  duplicate_flag = flag_duplicate,
  duplicate_flag_agrees_with_extra_row = flag_duplicate == canonical_duplicate,
  coords_formable = coords_formable,
  inside_mainland_bbox = inside_mainland_bbox,
  bombus_all_five_finite = bombus_all_finite,
  retained_after_measurement_and_dedup = stage2,
  retained_after_new_bombus = stage4,
  stringsAsFactors = FALSE
)
for (s in sp) rows[[paste0("bombus_", s, "_finite")]] <- is.finite(coverage[[s]])
rows$first_exclusion <- "retained_through_new_bombus"
rows$first_exclusion[!colour_formable] <- "nonformable_primary_colour"
rows$first_exclusion[colour_formable & canonical_duplicate] <- "exact_duplicate_extra_row"
rows$first_exclusion[stage2 & !coords_formable] <- "nonformable_coordinates"
rows$first_exclusion[stage3 & !bombus_all_finite] <- "new_bombus_not_all_five_finite"

cross <- data.frame(
  metric = c(
    "duplicate_flag_true", "exact_hash_extra_rows", "duplicate_flag_disagreement_rows",
    "new_bombus_missing_all_five", "new_bombus_missing_inside_bbox",
    "new_bombus_missing_outside_bbox", "duplicate_extra_and_bombus_missing",
    "retained_after_dedup_and_new_bombus"
  ),
  n = c(
    sum(flag_duplicate), sum(canonical_duplicate), sum(flag_duplicate != canonical_duplicate),
    sum(!bombus_all_finite), sum(!bombus_all_finite & inside_mainland_bbox),
    sum(!bombus_all_finite & !inside_mainland_bbox),
    sum(canonical_duplicate & !bombus_all_finite), sum(stage4)
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(flow, file.path(output_dir, "population_flow_summary.csv"), row.names = FALSE)
utils::write.csv(rows, file.path(output_dir, "population_flow_rows.csv"), row.names = FALSE)
utils::write.csv(cross, file.path(output_dir, "population_flow_diagnostics.csv"), row.names = FALSE)
utils::write.csv(
  rows[!rows$retained_after_new_bombus, , drop = FALSE],
  file.path(output_dir, "excluded_before_downstream_analysis.csv"), row.names = FALSE
)

cat("Population flow from curated 1,965 rows:\n")
print(flow, row.names = FALSE)
cat("\nDiagnostics:\n")
print(cross, row.names = FALSE)
