args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = NULL) {
  prefix <- paste0(name, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  sub(prefix, "", hit[1], fixed = TRUE)
}

cells_path <- arg_value(
  "--cells",
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)
checkpoint_root <- arg_value(
  "--checkpoint-root",
  "results/ecological_v16_predictive_replication/checkpoints"
)
output_dir <- arg_value(
  "--output", "results/ecological_v18_extreme_human_diagnostic"
)
control_min_q <- as.numeric(arg_value("--control-min-q", "0.25"))
fractions <- as.numeric(strsplit(
  arg_value("--fractions", "0.01,0.025,0.05,0.10"), ","
)[[1]])

source("scripts/extreme_human_diagnostic_v11.R")

cells <- utils::read.csv(
  cells_path, check.names = FALSE, stringsAsFactors = FALSE
)
presence <- v18_align_result(
  readRDS(file.path(
    checkpoint_root,
    "national_environment_spde_presence_draws1000.rds"
  )),
  cells, "presence"
)
intensity <- v18_align_result(
  readRDS(file.path(
    checkpoint_root,
    "national_environment_spde_intensity_draws1000.rds"
  )),
  cells, "intensity"
)
phenology <- v18_align_result(
  readRDS(file.path(
    checkpoint_root,
    "national_environment_year_spde_phenology_draws1000.rds"
  )),
  cells, "phenology"
)
if (length(unique(c(
  ncol(presence$draws), ncol(intensity$draws), ncol(phenology$draws)
))) != 1L) {
  stop("Checkpoint draw counts differ.", call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
final_md5 <- if (file.exists("final.Rmd")) {
  unname(tools::md5sum("final.Rmd"))
} else NA_character_

profile <- v18_profile(cells, presence, phenology, intensity)
counts <- presence$draws
trials <- pmax(as.numeric(cells$n_observations), 1)
shares <- sweep(counts, 1, trials, "/")
q_sim <- v18_simulation_tail_q(counts, "upper")
z_sim <- v18_z_matrix(shares)
simulated_eligible <- counts > 0
observed_eligible <- as.numeric(cells$n_pigmented) > 0
ids <- as.character(cells$exact_site_id)

message("[v18] building response-blind natural-control options")
match_options <- v18_match_options(cells, presence$latent_mean)

message("[v18] evaluating global extreme envelope")
envelope <- v18_extreme_envelope(
  profile, q_sim, z_sim, simulated_eligible, observed_eligible,
  fractions, ids
)

message("[v18] repeatedly rematching predictive extreme sets")
matched <- v18_matched_extreme_analysis(
  cells, profile, q_sim, z_sim, counts, match_options,
  fractions = fractions, control_min_q = control_min_q
)
stability <- v18_simulation_stability(matched$null)

for (fraction in fractions) {
  label <- paste0("top_", sprintf("%04g", 100 * fraction))
  selected <- v18_top_indices(
    profile$unexpected_pigmented_q,
    profile$unexpected_pigmented_z,
    observed_eligible, fraction, ids
  )
  profile[[label]] <- seq_len(nrow(profile)) %in% selected
}

write.csv(
  profile, file.path(output_dir, "extreme_cell_tail_profiles.csv"),
  row.names = FALSE
)
write.csv(
  envelope, file.path(output_dir, "extreme_global_envelope.csv"),
  row.names = FALSE
)
write.csv(
  matched$summary,
  file.path(output_dir, "extreme_rematched_summary.csv"),
  row.names = FALSE
)
write.csv(
  matched$null,
  file.path(output_dir, "extreme_rematched_null.csv"),
  row.names = FALSE
)
write.csv(
  matched$observed_pairs,
  file.path(output_dir, "extreme_observed_matched_pairs.csv"),
  row.names = FALSE
)
write.csv(
  stability,
  file.path(output_dir, "extreme_simulation_stability.csv"),
  row.names = FALSE
)
write.csv(
  match_options$settings,
  file.path(output_dir, "extreme_matching_settings.csv"),
  row.names = FALSE
)

metadata <- data.frame(
  field = c(
    "analysis_spec_version", "generated_at", "n_predictive_draws",
    "extreme_selector", "primary_extreme_fraction",
    "sensitivity_extreme_fractions", "control_definition",
    "matching_variables", "population_role", "phenology_role",
    "intensity_role", "joint_posterior_claim", "residual_used_as_response",
    "horticultural_claim_ceiling", "final_Rmd_modified_by_v18",
    "final_Rmd_md5"
  ),
  value = c(
    v18_analysis_spec_version, as.character(Sys.time()),
    as.character(ncol(counts)),
    "one-sided cross-fitted posterior predictive pigmentation tail",
    "0.05", paste(setdiff(fractions, 0.05), collapse = ","),
    paste0(
      "pigmented cell in the same analysis support with colour-tail q >= ",
      control_min_q
    ),
    paste0(
      "same spatial fold; geography; 50-km environment; natural pigmentation ",
      "logit; observation effort"
    ),
    "fixed held-out human-context facet; never used for case selection or matching",
    "lower posterior predictive tail; no phenology residual",
    "upper posterior predictive tail among pigmented cells; no intensity residual",
    "separate facet models are not treated as a joint posterior",
    "false",
    "human-consistent follow-up priority, not horticultural origin probability",
    "false", final_md5
  ),
  stringsAsFactors = FALSE
)
write.csv(
  metadata, file.path(output_dir, "extreme_analysis_metadata.csv"),
  row.names = FALSE
)

readme <- c(
  "# v18 posterior-predictive extreme and human-context diagnostic",
  "",
  paste0(
    "This analysis replaces residual magnitude with one-sided cross-fitted ",
    "posterior predictive tail probabilities for pigmentation, early ",
    "phenology, and conditional dark intensity."
  ),
  "",
  paste0(
    "First, a global extreme envelope compares the observed maximum and ",
    "upper-tail burden with the same statistics from 1000 natural predictive ",
    "maps. Second, extreme unexpected-pigmented cells are matched to ",
    "natural-typical pigmented controls on spatial fold, geography, ",
    "environment, natural pigmentation expectation, and observation effort."
  ),
  "",
  paste0(
    "Case selection and control matching are repeated independently for every ",
    "predictive map. Population, early-flowering tail, and dark-intensity tail ",
    "are fixed held-out facets and never select or match cases."
  ),
  "",
  paste0(
    "The analysis tests whether extreme natural-model departures repeatedly ",
    "coincide with human-consistent facets more than expected under the fitted ",
    "natural model. It does not estimate horticultural origin, escape, ",
    "introgression, or genetic pollution."
  ),
  "",
  "`final.Rmd` is intentionally not modified."
)
writeLines(readme, file.path(output_dir, "README.md"), useBytes = TRUE)

cat("v18 extreme diagnostic written to ", normalizePath(output_dir), "\n")
print(envelope)
print(matched$summary[matched$summary$fraction == 0.05, ])

