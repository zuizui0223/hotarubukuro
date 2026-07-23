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
presence_path <- arg_value(
  "--presence",
  paste0(
    "results/ecological_v16_predictive_replication/checkpoints/",
    "national_environment_spde_presence_draws1000.rds"
  )
)
intensity_path <- arg_value(
  "--intensity",
  paste0(
    "results/ecological_v16_predictive_replication/checkpoints/",
    "national_environment_spde_intensity_draws1000.rds"
  )
)
output_dir <- arg_value(
  "--output", "results/ecological_v17_local_pair_turnover"
)
k <- as.integer(arg_value("--k", "5"))
radii <- as.numeric(strsplit(arg_value("--radii", "10,25,50"), ",")[[1]])

source("scripts/local_pair_predictive_v10.R")

cells <- utils::read.csv(
  cells_path, check.names = FALSE, stringsAsFactors = FALSE
)
presence_raw <- readRDS(presence_path)
intensity_raw <- readRDS(intensity_path)
presence <- v17_align_result(presence_raw, cells, "presence checkpoint")
intensity <- v17_align_result(intensity_raw, cells, "intensity checkpoint")

if (ncol(presence$draws) != ncol(intensity$draws)) {
  stop("Presence and intensity checkpoints have different draw counts.",
       call. = FALSE)
}
if (ncol(presence$draws) < 1000L) {
  stop("At least 1000 predictive draws are required.", call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
final_md5 <- if (file.exists("final.Rmd")) {
  unname(tools::md5sum("final.Rmd"))
} else NA_character_

edge_tables <- list()
summaries <- list()
null_tables <- list()
for (radius in radii) {
  message("[v17] building response-blind pair graph at ", radius, " km")
  edges <- v17_pair_graph(
    cells, radius_km = radius, k = k,
    same_fold_only = TRUE, common_support_only = TRUE
  )
  edges <- v17_add_pair_features(edges, cells, presence, intensity)
  edge_tables[[as.character(radius)]] <- edges

  presence_test <- v17_pair_predictive_test(
    edges, cells, presence, "presence"
  )
  intensity_test <- v17_pair_predictive_test(
    edges, cells, intensity, "intensity"
  )
  summaries[[paste0(radius, "_presence")]] <- presence_test$summary
  summaries[[paste0(radius, "_intensity")]] <- intensity_test$summary
  null_tables[[paste0(radius, "_presence")]] <- presence_test$null
  null_tables[[paste0(radius, "_intensity")]] <- intensity_test$null
}

edges <- do.call(rbind, edge_tables)
summary <- v17_adjust_multiplicity(do.call(rbind, summaries))
null <- do.call(rbind, null_tables)
stability <- v17_simulation_stability(null)

write.csv(
  edges, file.path(output_dir, "local_pair_edges.csv"), row.names = FALSE
)
write.csv(
  summary, file.path(output_dir, "local_pair_predictive_summary.csv"),
  row.names = FALSE
)
write.csv(
  null, file.path(output_dir, "local_pair_predictive_null.csv"),
  row.names = FALSE
)
write.csv(
  stability, file.path(output_dir, "local_pair_simulation_stability.csv"),
  row.names = FALSE
)

metadata <- data.frame(
  field = c(
    "analysis_spec_version", "generated_at", "candidate_unit",
    "pair_construction", "primary_radius_km", "sensitivity_radii_km",
    "neighbours_per_cell", "pair_joint_support", "presence_response",
    "intensity_response", "pollinator_primary_estimand",
    "pollinator_interpretation", "natural_null",
    "residual_as_pair_response", "observation_year_role",
    "n_predictive_draws", "final_Rmd_modified_by_v17", "final_Rmd_md5"
  ),
  value = c(
    v17_analysis_spec_version, as.character(Sys.time()),
    "1-km cell endpoint",
    "response-blind undirected k-nearest graph on five-species common support",
    "25", paste(setdiff(radii, 25), collapse = ","),
    as.character(k),
    "same heldout spatial fold only",
    "absolute endpoint difference in observed pigmented share",
    "absolute endpoint difference in conditional intensity among pigmented endpoints",
    "distance in total-support plus composition-PC1/PC2 fingerprint",
    "predicted habitat community fingerprint, not abundance, visitation, or pollination effectiveness",
    "1000 v16 environment-plus-SPDE cross-fitted predictive maps",
    "false",
    "nuisance adjustment only; no three-year temporal inference",
    as.character(ncol(presence$draws)), "false", final_md5
  ),
  stringsAsFactors = FALSE
)
write.csv(
  metadata, file.path(output_dir, "local_pair_metadata.csv"),
  row.names = FALSE
)

readme <- c(
  "# v17 local flower-colour / Bombus-fingerprint pair analysis",
  "",
  paste0(
    "Pairs are constructed without flower-colour information among 1-km cells ",
    "on the five-species ENMeval common support. The primary graph uses up to ",
    k, " nearest neighbours within 25 km; 10 and 50 km are fixed sensitivities."
  ),
  "",
  paste0(
    "The primary response is the absolute endpoint difference in pigmented ",
    "share. The hierarchical second response is the absolute endpoint ",
    "difference in optical intensity only when both endpoints contain ",
    "pigmented observations."
  ),
  "",
  paste0(
    "Bombus is represented as a community fingerprint: total habitat support ",
    "plus two composition axes. Its endpoint distance is not abundance, ",
    "visitation, pollination effectiveness, or a direct selection-pressure ",
    "measurement."
  ),
  "",
  paste0(
    "For each of 1000 v16 environment-plus-SPDE predictive maps, the same ",
    "pair response and partial association are recomputed. Shared endpoints ",
    "and the pair graph are therefore present in both observed and null ",
    "statistics. Edges are restricted to endpoints in the same heldout fold ",
    "because v16 posterior draws are joint within, but not across, folds."
  ),
  "",
  paste0(
    "The result tests whether observed local colour turnover aligns with ",
    "predicted community turnover more strongly than expected from the fitted ",
    "natural model and observation design. It does not estimate a causal ",
    "pollinator effect."
  ),
  "",
  "`final.Rmd` is intentionally not modified."
)
writeLines(
  readme, file.path(output_dir, "README.md"), useBytes = TRUE
)

cat("v17 local-pair analysis written to ", normalizePath(output_dir), "\n")
print(summary)

