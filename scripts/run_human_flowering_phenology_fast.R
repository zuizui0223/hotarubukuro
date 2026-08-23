#!/usr/bin/env Rscript

# Fast PR-only reconstruction of the primary phenology hypotheses from the
# immutable raw-colour table plus the already-frozen continuous-isolation cell
# artifact. This result is accepted only if reconstructed cell-level phenotype
# counts match that artifact exactly; otherwise the script stops and the full
# public reconstruction remains the only valid route.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  if (hit[[1L]] == length(args)) stop("Missing value for ", flag, call. = FALSE)
  args[[hit[[1L]] + 1L]]
}

source("R/environment_spatial.R")
source("R/phenotype_hurdle.R")
source("R/flowering_phenology.R")

raw_path <- arg_value("--raw", "Data_S1.csv")
isolation_path <- arg_value("--isolation", "continuous_isolation_cell_metrics.csv")
output_dir <- arg_value("--output", "results/human_flowering_phenology_fast")
permutations <- as.integer(arg_value("--permutations", "9999"))
seed <- as.integer(arg_value("--seed", "20260823"))

if (!file.exists(raw_path)) stop("Missing raw input: ", raw_path, call. = FALSE)
if (!file.exists(isolation_path)) stop("Missing frozen isolation artifact: ", isolation_path, call. = FALSE)
if (!is.finite(permutations) || permutations < 99L) stop("--permutations must be >= 99", call. = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

raw <- utils::read.csv(raw_path, check.names = FALSE, stringsAsFactors = FALSE)
qc <- audit_colour_qc(raw, author_review_confirmed = TRUE)
d <- qc$data[qc$data$colour_qc_primary, , drop = FALSE]
if (!nrow(d)) stop("No primary colour-QC observations", call. = FALSE)

measurement <- fit_pigmentation_measurement(
  d$colour_a, colour_L = d$colour_L, colour_C = d$colour_C
)
d <- cbind(d, measurement$observations)
photo_date <- as.Date(d$date)
d$year <- as.integer(format(photo_date, "%Y"))
d$DOY <- as.integer(format(photo_date, "%j"))

isolation <- utils::read.csv(isolation_path, check.names = FALSE, stringsAsFactors = FALSE)
fp_require_columns(isolation,
  c("exact_site_id", "longitude", "latitude", "x_km", "y_km", "spatial_fold", "colour",
    "n_observations", "n_pigmented", "n_white", "same_colour_nn_km", "relative_isolation_nn",
    "local_population_rank", "population_5km_rank", "population_10km_rank",
    "population_25km_rank", "population_50km_rank"),
  "frozen isolation artifact")

# Recover the final 1-km cell for each raw observation from frozen cell centres.
# This avoids environmental re-fitting in the fast PR route. Exact cell-level
# count equality below is the acceptance gate for this assignment.
obs_lon <- as.numeric(d$longitude)
obs_lat <- as.numeric(d$latitude)
cell_lon <- as.numeric(isolation$longitude)
cell_lat <- as.numeric(isolation$latitude)
nearest <- integer(nrow(d))
nearest_distance_km <- numeric(nrow(d))
for (i in seq_len(nrow(d))) {
  dx <- (cell_lon - obs_lon[[i]]) * 111.320 * cos(obs_lat[[i]] * pi / 180)
  dy <- (cell_lat - obs_lat[[i]]) * 110.574
  distance <- sqrt(dx^2 + dy^2)
  nearest[[i]] <- which.min(distance)
  nearest_distance_km[[i]] <- distance[[nearest[[i]]]]
}
d$x_km <- isolation$x_km[nearest]
d$y_km <- isolation$y_km[nearest]
d$recovered_cell_id <- isolation$exact_site_id[nearest]
d$recovered_cell_distance_km <- nearest_distance_km

cell_groups <- split(seq_len(nrow(d)), d$recovered_cell_id)
reconstructed <- do.call(rbind, lapply(cell_groups, function(index) {
  block <- d[index, , drop = FALSE]
  data.frame(
    exact_site_id = block$recovered_cell_id[[1L]],
    n_observations = nrow(block),
    n_pigmented = sum(block$pigmented_mixture50 == 1L, na.rm = TRUE),
    n_white = sum(block$pigmented_mixture50 == 0L, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
rownames(reconstructed) <- NULL
validation <- merge(
  isolation[, c("exact_site_id", "n_observations", "n_pigmented", "n_white")],
  reconstructed, by = "exact_site_id", all = TRUE, suffixes = c("_frozen", "_fast"), sort = FALSE
)
count_columns <- c("n_observations", "n_pigmented", "n_white")
for (column in count_columns) {
  validation[[paste0(column, "_match")]] <-
    validation[[paste0(column, "_frozen")]] == validation[[paste0(column, "_fast")]]
}
validation$all_counts_match <- apply(
  validation[paste0(count_columns, "_match")], 1L,
  function(x) all(!is.na(x) & x)
)
utils::write.csv(validation, file.path(output_dir, "fast_cell_count_validation.csv"), row.names = FALSE)

exact_cells <- nrow(validation) == nrow(isolation) && all(validation$all_counts_match)
if (!exact_cells) {
  mismatch <- sum(!validation$all_counts_match | is.na(validation$all_counts_match))
  stop(
    "FAST ROUTE REJECTED: reconstructed phenotype counts do not exactly match frozen isolation artifact; mismatched cells=",
    mismatch, call. = FALSE
  )
}
if (max(nearest_distance_km, na.rm = TRUE) > 1.0) {
  stop("FAST ROUTE REJECTED: at least one accepted observation is >1 km from recovered cell centre", call. = FALSE)
}

# Use the original exact-site identifier for site weighting, while x/y are the
# validated frozen 1-km cell coordinates.
phenology <- data.frame(
  exact_site_id = as.character(d$exact_site_id),
  x_km = as.numeric(d$x_km), y_km = as.numeric(d$y_km),
  year = d$year, DOY = d$DOY,
  pigmented_mixture50 = as.integer(d$pigmented_mixture50),
  stringsAsFactors = FALSE
)
cell_year_colour <- fp_build_cell_year_colour(phenology, cell_km = 1)
utils::write.csv(cell_year_colour, file.path(output_dir, "phenology_cell_year_colour.csv"), row.names = FALSE)

radii <- c(5, 10, 20)
summary_rows <- list()
collapsed_sets <- list()
for (j in seq_along(radii)) {
  radius <- radii[[j]]
  pairs <- fp_mutual_nearest_pairs(cell_year_colour, max_distance_km = radius)
  collapsed <- fp_collapse_geometric_pairs(pairs)
  collapsed_sets[[as.character(radius)]] <- collapsed
  summary_rows[[j]] <- fp_pair_summary(
    pairs, max_distance_km = radius, permutations = permutations, seed = seed + 100L * j
  )
  utils::write.csv(pairs, file.path(output_dir, paste0("matched_pair_years_", radius, "km.csv")), row.names = FALSE)
  utils::write.csv(collapsed, file.path(output_dir, paste0("matched_unique_pairs_", radius, "km.csv")), row.names = FALSE)
}
pair_summary <- do.call(rbind, summary_rows)
utils::write.csv(pair_summary, file.path(output_dir, "matched_pair_summary.csv"), row.names = FALSE)

primary_pairs <- collapsed_sets[["5"]]
human_pairs <- fp_human_pair_table(primary_pairs, isolation)
pigmented_cells <- fp_collapse_pigmented_cells(human_pairs)
correlations <- fp_human_correlations(pigmented_cells, permutations = permutations, seed = seed + 1000L)
leave_one_fold <- fp_leave_one_fold_out(pigmented_cells)
utils::write.csv(human_pairs, file.path(output_dir, "primary_5km_human_pairs.csv"), row.names = FALSE)
utils::write.csv(pigmented_cells, file.path(output_dir, "primary_5km_pigmented_cells.csv"), row.names = FALSE)
utils::write.csv(correlations, file.path(output_dir, "primary_human_correlations.csv"), row.names = FALSE)
utils::write.csv(leave_one_fold, file.path(output_dir, "primary_human_leave_one_fold_out.csv"), row.names = FALSE)

primary <- pair_summary[pair_summary$max_distance_km == 5, , drop = FALSE]
primary_human <- correlations[correlations$role %in% c("primary_isolation", "primary_human_exposure"), , drop = FALSE]
lines <- c(
  "PASS fast phenology reconstruction: frozen cell phenotype counts matched exactly",
  sprintf("primary colour-QC observations: %d", nrow(d)),
  sprintf("frozen/reconstructed cells: %d", nrow(isolation)),
  sprintf("max raw-observation distance to recovered cell centre: %.6f km", max(nearest_distance_km)),
  sprintf("mixture boundary a*: %.8f", measurement$summary$decision_boundary_a),
  sprintf("mixture n white/pigmented: %d/%d", measurement$summary$n_white, measurement$summary$n_pigmented),
  sprintf("primary 5-km unique pairs: %d", primary$n_unique_geometric_pairs),
  sprintf("H1 mean delta DOY pigmented-white: %.6f", primary$mean_delta_doy_pigmented_minus_white),
  sprintf("H1 median delta DOY pigmented-white: %.6f", primary$median_delta_doy_pigmented_minus_white),
  sprintf("H1 proportion pigmented earlier: %.6f", primary$proportion_pigmented_earlier),
  sprintf("H1 one-sided sign-flip p: %.8g", primary$one_sided_signflip_p_pigmented_earlier),
  sprintf("H1 two-sided sign-flip p: %.8g", primary$two_sided_signflip_p)
)
for (i in seq_len(nrow(primary_human))) {
  lines <- c(lines, sprintf(
    "%s: rho=%.6f, one-sided within-fold permutation p=%.8g, Holm p=%.8g, n=%d",
    primary_human$feature[[i]], primary_human$spearman_rho[[i]],
    primary_human$within_spatial_fold_permutation_p_greater[[i]],
    primary_human$holm_p_two_primary[[i]], primary_human$n[[i]]
  ))
}
writeLines(lines, file.path(output_dir, "fast_validation.txt"))
cat(paste(lines, collapse = "\n"), "\n")
