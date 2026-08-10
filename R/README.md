# R modules

`R/pipeline_support.R` is the shared loader for the current JBI analysis path. Its `hb_module_files` and `hb_stage_modules` registries define only modules that are still used by the current broad-natural-template and local-departure/human-context pipeline.

## Current scientific modules

- `natural_predictive_model.R` — broad environment + SPDE natural template and predictive-map calibration;
- `candidate_null_tools.R` — common event/null utilities for the departure layer;
- `human_landscape_features.R` — landscape/context feature construction;
- `local_pigmented_isolates.R` — repeatable local pigmentation-state event definition;
- `human_raster_features.R` — raster-derived human-context support;
- `local_human_context.R` — post-selection candidate context analysis;
- `spatial_context.R` — spatial support utilities for the human-context layer;
- `did_sensitivity.R` — DID/context sensitivity analysis.

`candidate_doy_check.R` is an active Supporting Information check run after candidates are fixed. `reproducibility.R` provides provenance helpers. `raster_sources.R` supports current source-build utilities but is not loaded as a scientific analysis module.

Main-2 sharp-transition code is intentionally self-contained in `scripts/run_bombus_local_sharp_transition.R` plus `R/local_pair_graph.R`; five-species community-boundary and montane guardrail code is intentionally self-contained in `scripts/run_bombus_spatial_replication_test.R`. They are not routed through the broad/anomaly module loader.

Superseded `local_bombus_turnover`, `final_registry`, fixed-n, limitation-gate and other historical modules live under `legacy/`. Active code must not import them.
