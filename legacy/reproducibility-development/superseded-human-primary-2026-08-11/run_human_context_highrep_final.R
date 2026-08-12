#!/usr/bin/env Rscript

# Final 10,000-map adjudication of the Broad -> local departure -> human-context chain.
# Uses the frozen submission-scale current natural reference and refits the final-eight-axis
# presence reference with identical folds/SPDE machinery. The final-eight-axis matching
# caliper is calibrated response-blind to the current four-PC neighbour-support distribution.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  if (hit[length(hit)] == length(args)) stop("Missing value after ", flag, call. = FALSE)
  args[hit[length(hit)] + 1L]
}
reference_root <- arg_value("--reference-root", "reference-artifact")
output_dir <- arg_value("--output", "results/human_context_highrep_final")
n_draws <- as.integer(arg_value("--draws", "10000"))
seed <- as.integer(arg_value("--seed", "20260725"))
if (!is.finite(n_draws) || n_draws < 5000L) stop("Final adjudication requires >=5000 maps", call. = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_packages <- c("INLA", "Matrix", "jsonlite")
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse=", "), call. = FALSE)
source("R/natural_predictive_model.R")
source("R/candidate_null_tools.R")
source("R/local_pigmented_isolates.R")
source("R/local_human_context.R")
Sys.setenv(
  OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1",
  VECLIB_MAXIMUM_THREADS="1", NUMEXPR_NUM_THREADS="1", INLA_NUM_THREADS="1",
  OMP_DYNAMIC="FALSE"
)
set.seed(seed)

cells_path <- file.path(reference_root, "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv")
human_path <- file.path(reference_root, "results/ecological_v21_local_human_neighbourhood/human_neighbourhood_cell_features.csv")
did_path <- file.path(reference_root, "results/ecological_v22_did_human_context/did_cell_context.csv")
current_checkpoint <- file.path(
  reference_root, "results/ecological_v25_submission_presence/checkpoints",
  paste0("national_environment_spde_presence_draws", n_draws, ".rds")
)
for (p in c(cells_path, human_path, did_path, current_checkpoint)) {
  if (!file.exists(p) || file.info(p)$size <= 0) stop("Missing input: ", p, call. = FALSE)
}
cells <- utils::read.csv(cells_path, check.names=FALSE, stringsAsFactors=FALSE)
human <- utils::read.csv(human_path, check.names=FALSE, stringsAsFactors=FALSE)
did <- utils::read.csv(did_path, check.names=FALSE, stringsAsFactors=FALSE)
current_model <- readRDS(current_checkpoint)

current4 <- c("broad50km_pc1", "broad50km_pc2", "within50km_pc1", "within50km_pc2")
final8 <- c(
  "env_Temperature_PC1", "env_precip_PC1", "env_TemperatureSeasonality",
  "env_PrecipSeasonality", "env_topo_PC1", "env_soil_PC1", "env_soil_PC2", "env_RSDS"
)
v20_require_columns(cells, unique(c(
  "exact_site_id", "x_km", "y_km", "spatial_fold", "n_independent_sites",
  "n_observations", "n_pigmented", current4, final8
)), "cells")

message("[highrep] fitting final-eight-axis natural reference with ", n_draws, " draws")
final8_model <- v16_crossfit_spde(
  cells, response="n_pigmented", family="binomial", environment_terms=final8,
  trials="n_observations", model="final8_environment_spde_presence_highrep",
  n_draws=n_draws, seed=seed
)
saveRDS(final8_model, file.path(output_dir, paste0("final8_presence_draws", n_draws, ".rds")))

current_perf <- v16_model_performance(current_model, cells, "n_pigmented", "binomial", "n_observations")
final8_perf <- v16_model_performance(final8_model, cells, "n_pigmented", "binomial", "n_observations")
utils::write.csv(rbind(current_perf, final8_perf), file.path(output_dir, "natural_reference_predictive_performance.csv"), row.names=FALSE)
utils::write.csv(rbind(current_model$log, final8_model$log), file.path(output_dir, "natural_reference_fold_log.csv"), row.names=FALSE)

scale_matrix <- function(data, terms) {
  X <- as.matrix(data[, terms, drop=FALSE]); storage.mode(X) <- "double"
  X <- apply(X, 2, function(x) {
    sx <- stats::sd(x, na.rm=TRUE)
    if (!is.finite(sx) || sx <= 1e-12) rep(0, length(x)) else (x-mean(x,na.rm=TRUE))/sx
  })
  if (!is.matrix(X)) X <- matrix(X, ncol=length(terms))
  X
}
coordinates <- as.matrix(cells[, c("x_km","y_km"), drop=FALSE])
geographic <- as.matrix(stats::dist(coordinates))
env_dist <- function(terms) {
  X <- scale_matrix(cells, terms)
  as.matrix(stats::dist(X)) / sqrt(ncol(X))
}
current_env <- env_dist(current4)
final_env <- env_dist(final8)
support_vector <- function(env, caliper) vapply(seq_len(nrow(cells)), function(i) {
  sum(seq_len(nrow(cells)) != i & geographic[i,] <= 10 & env[i,] <= caliper)
}, integer(1))
current_support <- support_vector(current_env, 1)
target_mean <- mean(current_support); target_supported <- sum(current_support >= 3L)
caliper_grid <- seq(0.50, 1.20, by=0.01)
calibration <- do.call(rbind, lapply(caliper_grid, function(caliper) {
  s <- support_vector(final_env, caliper)
  data.frame(
    caliper=caliper, mean_neighbours=mean(s), supported_cells=sum(s>=3L),
    score=abs(mean(s)-target_mean)/target_mean + abs(sum(s>=3L)-target_supported)/nrow(cells),
    stringsAsFactors=FALSE
  )
}))
best <- which(calibration$score == min(calibration$score))
if (length(best)>1L) best <- best[which.min(abs(calibration$caliper[best]-1))]
best <- best[1L]; matched_caliper <- calibration$caliper[best]
calibration$selected <- seq_len(nrow(calibration)) == best
utils::write.csv(calibration, file.path(output_dir, "response_blind_caliper_calibration.csv"), row.names=FALSE)

make_graph <- function(env, caliper, label) {
  neighbours <- vector("list", nrow(cells)); weights <- vector("list", nrow(cells)); rows <- vector("list", nrow(cells))
  for (i in seq_len(nrow(cells))) {
    adjacent <- which(seq_len(nrow(cells)) != i & geographic[i,] <= 10 & env[i,] <= caliper)
    if (length(adjacent)) {
      adjacent <- adjacent[order(geographic[i,adjacent], as.character(cells$exact_site_id[adjacent]))]
      w <- 1/pmax(geographic[i,adjacent],0.5); w <- w/sum(w)
    } else w <- numeric()
    neighbours[[i]] <- adjacent; weights[[i]] <- w
    rows[[i]] <- data.frame(
      exact_site_id=as.character(cells$exact_site_id[i]), n_neighbours=length(adjacent),
      n_neighbour_independent_sites=sum(as.numeric(cells$n_independent_sites[adjacent]),na.rm=TRUE),
      mean_neighbour_distance_km=if(length(adjacent)) mean(geographic[i,adjacent]) else NA_real_,
      maximum_neighbour_distance_km=if(length(adjacent)) max(geographic[i,adjacent]) else NA_real_,
      mean_environmental_distance=if(length(adjacent)) mean(env[i,adjacent]) else NA_real_,
      maximum_environmental_distance=if(length(adjacent)) max(env[i,adjacent]) else NA_real_,
      supported=length(adjacent)>=3L, stringsAsFactors=FALSE
    )
  }
  list(
    neighbours=neighbours, weights=weights, geographic_distance=geographic,
    environmental_distance=env, support=do.call(rbind,rows),
    settings=data.frame(representation=label,radius_km=10,environment_caliper=caliper,minimum_neighbours=3L,stringsAsFactors=FALSE)
  )
}
current_graph <- make_graph(current_env,1,"current_4pc_caliper1")
matched_graph <- make_graph(final_env,matched_caliper,"final8_support_calibrated")
utils::write.csv(rbind(current_graph$settings,matched_graph$settings),file.path(output_dir,"graph_registry.csv"),row.names=FALSE)

hidx <- match(cells$exact_site_id,human$exact_site_id); didx <- match(cells$exact_site_id,did$exact_site_id)
if(anyNA(hidx)||anyNA(didx)) stop("Human features do not align",call.=FALSE)
features <- data.frame(
  exact_site_id=as.character(cells$exact_site_id),
  local_population_rank=as.numeric(human$local_population_rank[hidx]),
  population_5km_rank=as.numeric(human$population_5km_rank[hidx]),
  population_10km_rank=as.numeric(human$population_10km_rank[hidx]),
  population_25km_rank=as.numeric(human$population_25km_rank[hidx]),
  population_50km_rank=as.numeric(human$population_50km_rank[hidx]),
  did_proximity_rank=as.numeric(did$did_proximity_rank[didx]),
  road_proximity_rank=as.numeric(human$road_proximity_rank[hidx]),
  built_up_fraction_rank=as.numeric(human$built_up_fraction_rank[hidx]),
  forest_human_edge_rank=as.numeric(human$forest_human_edge_rank[hidx]),
  forest_cover_rank=as.numeric(human$forest_cover_rank[hidx]),
  mountainness_rank=as.numeric(human$mountainness_rank[hidx]), stringsAsFactors=FALSE
)
definitions <- data.frame(
  feature=c("local_population_rank","population_5km_rank","population_10km_rank","population_25km_rank","population_50km_rank","did_proximity_rank","road_proximity_rank","built_up_fraction_rank","forest_human_edge_rank","forest_cover_rank","mountainness_rank"),
  role=c("population_focal","population_5km","population_10km","population_25km","population_50km","dense_settlement","transport_access","built_context","managed_natural_interface","natural_forest_alternative","natural_mountain_alternative"),
  hypothesis_direction=c(rep("greater",9),"two_sided","two_sided"), stringsAsFactors=FALSE
)
effort <- data.frame(
  exact_site_id=as.character(cells$exact_site_id),
  observation_effort_rank=as.numeric(human$observation_effort_rank[hidx]),
  independent_site_support_rank=as.numeric(human$independent_site_support_rank[hidx]), stringsAsFactors=FALSE
)
effort_def <- data.frame(
  feature=c("observation_effort_rank","independent_site_support_rank"),
  role=c("observation_effort_alternative","independent_site_support_alternative"),
  hypothesis_direction=c("two_sided","two_sided"),stringsAsFactors=FALSE
)
utils::write.csv(definitions,file.path(output_dir,"global_mechanism_family_registry.csv"),row.names=FALSE)

run_one <- function(model_result,graph,scenario) {
  aligned <- v18_align_result(model_result,cells,scenario)
  observed_q <- v18_predictive_tail_q(aligned$observed,aligned$draws,"upper")
  simulated_q <- v18_simulation_tail_q(aligned$draws,"upper")
  obs_profile <- v20_local_profile(matrix(aligned$observed,ncol=1L),graph)
  sim_profile <- v20_local_profile(aligned$draws,graph)
  event <- v20_metric_rows(obs_profile,sim_profile,observed_q,simulated_q,scenario)$summary
  event$scenario <- scenario
  obs_h <- v21_local_contrasts(obs_profile$present,obs_profile$candidate,graph,features,definitions$feature)
  sim_h <- v21_local_contrasts(sim_profile$present,sim_profile$candidate,graph,features,definitions$feature)
  ov <- as.numeric(obs_h$contrast[1L,]); names(ov)<-colnames(obs_h$contrast)
  human_summary <- v21_contrast_summary(ov,as.data.frame(sim_h$contrast),definitions,scenario)
  human_summary$observed_n_candidates <- sum(obs_profile$candidate[,1L])
  obs_e <- v21_local_contrasts(obs_profile$present,obs_profile$candidate,graph,effort,effort_def$feature)
  sim_e <- v21_local_contrasts(sim_profile$present,sim_profile$candidate,graph,effort,effort_def$feature)
  ev <- as.numeric(obs_e$contrast[1L,]); names(ev)<-colnames(obs_e$contrast)
  effort_summary <- v21_contrast_summary(ev,as.data.frame(sim_e$contrast),effort_def,scenario)
  candidates <- data.frame(scenario=scenario,exact_site_id=as.character(cells$exact_site_id[obs_profile$candidate[,1L]]),stringsAsFactors=FALSE)
  list(event=event,human=human_summary,effort=effort_summary,candidates=candidates)
}

scenarios <- list(
  current_model_current_graph=list(model=current_model,graph=current_graph),
  current_model_final8_support_matched=list(model=current_model,graph=matched_graph),
  final8_model_current_graph=list(model=final8_model,graph=current_graph),
  final8_model_final8_support_matched=list(model=final8_model,graph=matched_graph)
)
results <- lapply(names(scenarios),function(name){
  message("[highrep human audit] ",name)
  run_one(scenarios[[name]]$model,scenarios[[name]]$graph,name)
}); names(results)<-names(scenarios)

event <- do.call(rbind,lapply(results,`[[`,"event")); human_summary <- do.call(rbind,lapply(results,`[[`,"human"))
effort_summary <- do.call(rbind,lapply(results,`[[`,"effort")); candidate_membership <- do.call(rbind,lapply(results,`[[`,"candidates"))
utils::write.csv(event,file.path(output_dir,"candidate_null_highrep.csv"),row.names=FALSE)
utils::write.csv(human_summary,file.path(output_dir,"global_human_mechanism_maxT_highrep.csv"),row.names=FALSE)
utils::write.csv(effort_summary,file.path(output_dir,"observation_bias_alternative_highrep.csv"),row.names=FALSE)
utils::write.csv(candidate_membership,file.path(output_dir,"candidate_membership_highrep.csv"),row.names=FALSE)

decision <- do.call(rbind,lapply(split(human_summary,human_summary$configuration),function(block){
  best <- block[which.min(block$maxT_FWER_p),,drop=FALSE]
  pop <- block[block$feature %in% definitions$feature[1:5],,drop=FALSE]
  bestp <- pop[which.min(pop$maxT_FWER_p),,drop=FALSE]
  data.frame(
    scenario=block$configuration[1],candidate_count=unique(block$observed_n_candidates)[1],
    best_population_scale=bestp$feature,best_population_global_FWER=bestp$maxT_FWER_p,
    best_global_feature=best$feature,best_global_FWER=best$maxT_FWER_p,
    any_human_global_FWER_below_0_05=any(block$maxT_FWER_p[block$feature %in% definitions$feature[1:9]]<0.05),
    stringsAsFactors=FALSE
  )
}))
utils::write.csv(decision,file.path(output_dir,"global_human_decision_highrep.csv"),row.names=FALSE)

jsonlite::write_json(list(
  status="PASS",n_maps=n_draws,seed=seed,selected_final8_caliper=matched_caliper,
  current_mean_neighbours=target_mean,current_supported_cells=target_supported,
  matched_final8_mean_neighbours=calibration$mean_neighbours[best],matched_final8_supported_cells=calibration$supported_cells[best],
  current_checkpoint=current_checkpoint,
  interpretation_rule="Robust human-context support requires familywise support that is not an artifact of environmental matching geometry and does not depend materially on which defensible natural reference is used.",
  claim_ceiling="Association with anthropogenic exposure/managed context only; no direct planting, escape, cultivar ancestry or introgression claim."
),file.path(output_dir,"validation.json"),pretty=TRUE,auto_unbox=TRUE)
cat("High-rep human-context adjudication complete; final8 caliper=",matched_caliper,"\n",sep="")
