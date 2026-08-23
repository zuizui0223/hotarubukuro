#!/usr/bin/env Rscript

# Exploratory flowering-date diagnostic for the human/provenance section.
# Photo DOY is an observation-date phenology proxy, not flowering onset.

args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
source("R/flowering_phenology.R")
source("R/flowering_phenology_elevation.R")
arg_value <- function(name, default = NULL) hb_arg_value(args, name, default)

phenotype_path <- arg_value("--phenotype",
  "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv")
isolation_path <- arg_value("--isolation",
  "results/continuous_colour_isolation/isolation_cell_metrics.csv")
output_dir <- arg_value("--output", "results/human_flowering_phenology")
permutations <- as.integer(arg_value("--permutations", "9999"))
seed <- as.integer(arg_value("--seed", "20260823"))
radii <- as.numeric(strsplit(arg_value("--radii", "5,10,20"), ",", fixed = TRUE)[[1L]])

for (path in c(phenotype_path, isolation_path)) {
  if (!file.exists(path)) stop("Missing required input: ", path, call. = FALSE)
}
if (!is.finite(permutations) || permutations < 99L) stop("--permutations must be >= 99", call. = FALSE)
if (!length(radii) || any(!is.finite(radii)) || any(radii < 0)) stop("Invalid --radii", call. = FALSE)
if (!any(abs(radii - 5) < 1e-9)) stop("Primary 5-km radius must be present", call. = FALSE)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
observations <- utils::read.csv(phenotype_path, check.names = FALSE, stringsAsFactors = FALSE)
isolation <- utils::read.csv(isolation_path, check.names = FALSE, stringsAsFactors = FALSE)
fp_require_columns(observations,
  c("exact_site_id", "x_km", "y_km", "year", "DOY", "pigmented_mixture50", "elevation"),
  "phenotype observations")

cell_year_colour <- fp_build_cell_year_colour(observations, cell_km = 1)
utils::write.csv(cell_year_colour, file.path(output_dir, "phenology_cell_year_colour.csv"), row.names = FALSE)

descriptive_groups <- split(seq_len(nrow(cell_year_colour)),
  paste(cell_year_colour$year, cell_year_colour$colour, sep = "::"))
descriptive <- do.call(rbind, lapply(descriptive_groups, function(index) {
  block <- cell_year_colour[index, , drop = FALSE]
  data.frame(
    year = as.integer(block$year[[1L]]), colour = as.character(block$colour[[1L]]),
    n_cell_years = nrow(block), median_doy = stats::median(block$median_doy),
    mean_doy = mean(block$median_doy), q25_doy = unname(stats::quantile(block$median_doy, 0.25)),
    q75_doy = unname(stats::quantile(block$median_doy, 0.75)), stringsAsFactors = FALSE)
}))
descriptive <- descriptive[order(descriptive$year, descriptive$colour), , drop = FALSE]
utils::write.csv(descriptive, file.path(output_dir, "national_descriptive.csv"), row.names = FALSE)

summary_rows <- list()
collapsed_sets <- list()
pair_year_sets <- list()
for (i in seq_along(radii)) {
  radius <- radii[[i]]
  pairs <- fp_mutual_nearest_pairs(cell_year_colour, max_distance_km = radius)
  collapsed <- fp_collapse_geometric_pairs(pairs)
  pair_year_sets[[as.character(radius)]] <- pairs
  collapsed_sets[[as.character(radius)]] <- collapsed
  summary_rows[[i]] <- fp_pair_summary(pairs, max_distance_km = radius,
    permutations = permutations, seed = seed + 100L * i)
  utils::write.csv(pairs, file.path(output_dir, paste0("matched_pair_years_", radius, "km.csv")), row.names = FALSE)
  utils::write.csv(collapsed, file.path(output_dir, paste0("matched_unique_pairs_", radius, "km.csv")), row.names = FALSE)
}
pair_summary <- do.call(rbind, summary_rows)
utils::write.csv(pair_summary, file.path(output_dir, "matched_pair_summary.csv"), row.names = FALSE)

# Elevation guardrails were frozen in PR #67 before inspecting the phenology result.
cell_elevation <- fp_build_cell_year_colour_elevation(observations, cell_km = 1)
primary_pair_years_elevation <- fp_add_pair_elevation(pair_year_sets[["5"]], cell_elevation)
elevation_guardrails <- fp_elevation_guardrails(
  primary_pair_years_elevation, permutations = permutations, seed = seed + 500L)
utils::write.csv(cell_elevation,
  file.path(output_dir, "phenology_cell_year_colour_elevation.csv"), row.names = FALSE)
utils::write.csv(primary_pair_years_elevation,
  file.path(output_dir, "matched_pair_years_5km_with_elevation.csv"), row.names = FALSE)
utils::write.csv(elevation_guardrails$summary,
  file.path(output_dir, "elevation_guardrail_summary.csv"), row.names = FALSE)
utils::write.csv(elevation_guardrails$correlation,
  file.path(output_dir, "elevation_guardrail_correlation.csv"), row.names = FALSE)

# Human/isolation context is joined only after the phenotype-only 5-km pairs are fixed.
primary_pairs <- collapsed_sets[["5"]]
human_pairs <- fp_human_pair_table(primary_pairs, isolation)
pigmented_cells <- fp_collapse_pigmented_cells(human_pairs)
correlations <- fp_human_correlations(pigmented_cells, permutations = permutations, seed = seed + 1000L)
leave_one_fold <- fp_leave_one_fold_out(pigmented_cells)

utils::write.csv(human_pairs, file.path(output_dir, "primary_5km_human_pairs.csv"), row.names = FALSE)
utils::write.csv(pigmented_cells, file.path(output_dir, "primary_5km_pigmented_cells.csv"), row.names = FALSE)
utils::write.csv(correlations, file.path(output_dir, "primary_human_correlations.csv"), row.names = FALSE)
utils::write.csv(leave_one_fold, file.path(output_dir, "primary_human_leave_one_fold_out.csv"), row.names = FALSE)

primary_summary <- pair_summary[abs(pair_summary$max_distance_km - 5) < 1e-9, , drop = FALSE]
if (nrow(primary_summary) != 1L) stop("Primary 5-km summary not uniquely defined", call. = FALSE)
if (primary_summary$n_unique_geometric_pairs < 5L) stop("Too few primary matched pairs for a useful diagnostic", call. = FALSE)
join_rate <- if (nrow(primary_pairs)) mean(primary_pairs$pigmented_cell_id %in% isolation$exact_site_id) else 0
if (!is.finite(join_rate) || join_rate < 0.95) stop("Human/isolation join rate below 95%: ", signif(join_rate, 4), call. = FALSE)

primary_human <- correlations[correlations$role %in% c("primary_isolation", "primary_human_exposure"), , drop = FALSE]
validation_lines <- c(
  "PASS human flowering-phenology exploratory diagnostic",
  "Interpretation boundary: YAMAP photo DOY is an observation-date proxy, not flowering onset or provenance.",
  "Elevation guardrails were preregistered in PR #67 before the first phenology result was inspected.",
  sprintf("phenotype rows: %d", nrow(observations)),
  sprintf("cell-year-colour rows: %d", nrow(cell_year_colour)),
  sprintf("primary 5-km unique pairs: %d", primary_summary$n_unique_geometric_pairs),
  sprintf("primary 5-km mean delta DOY (pigmented-white): %.4f", primary_summary$mean_delta_doy_pigmented_minus_white),
  sprintf("primary 5-km median delta DOY (pigmented-white): %.4f", primary_summary$median_delta_doy_pigmented_minus_white),
  sprintf("primary H1 one-sided sign-flip p (pigmented earlier): %.6g", primary_summary$one_sided_signflip_p_pigmented_earlier),
  sprintf("human/isolation join rate: %.4f", join_rate))
if (nrow(primary_human)) {
  for (i in seq_len(nrow(primary_human))) {
    validation_lines <- c(validation_lines, sprintf(
      "%s: rho(early_days)=%.4f, one-sided within-fold permutation p=%.6g, Holm p=%.6g",
      primary_human$feature[[i]], primary_human$spearman_rho[[i]],
      primary_human$within_spatial_fold_permutation_p_greater[[i]], primary_human$holm_p_two_primary[[i]]))
  }
}
if (nrow(elevation_guardrails$summary)) {
  for (i in seq_len(nrow(elevation_guardrails$summary))) {
    row <- elevation_guardrails$summary[i, , drop = FALSE]
    validation_lines <- c(validation_lines, sprintf(
      "elevation guardrail %s: n=%d, mean delta DOY=%.4f, one-sided p=%.6g",
      row$scope, row$n_unique_geometric_pairs,
      row$mean_delta_doy_pigmented_minus_white,
      row$one_sided_signflip_p_pigmented_earlier))
  }
}
if (nrow(elevation_guardrails$correlation)) {
  validation_lines <- c(validation_lines, sprintf(
    "elevation-delta association: rho=%.4f, p=%.6g, n=%d",
    elevation_guardrails$correlation$spearman_rho,
    elevation_guardrails$correlation$asymptotic_p,
    elevation_guardrails$correlation$n_unique_geometric_pairs))
}
writeLines(validation_lines, file.path(output_dir, "validation.txt"))
cat(paste(validation_lines, collapse = "\n"), "\n")
