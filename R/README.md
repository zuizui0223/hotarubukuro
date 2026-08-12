# R modules

This directory contains reusable R modules required by the adopted manuscript workflows. `R/pipeline_support.R` is the shared loader and its registry is checked by CI against `paper/active-file-map.csv`.

## Local-departure and human-context modules

- `natural_predictive_model.R` — environment + SPDE predictive-reference utilities;
- `candidate_null_tools.R` — event/null utilities for predictive-map replay;
- `human_landscape_features.R` — landscape/context feature construction;
- `local_pigmented_isolates.R` — final-eight-axis local pigmentation-event utilities;
- `human_raster_features.R` — raster-derived human-context support;
- `local_human_context.R` — post-selection candidate-context contrasts;
- `spatial_context.R` — spatial support utilities;
- `did_sensitivity.R` — DID/context feature utilities used in the global human family.

Current execution route:

- `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`;
- `.github/workflows/human-context-highrep-final.yml`.

## Local focal-Bombus module

- `local_pair_graph.R` — deterministic local graph and pair construction for the sharp white-pigmented boundary test.

Primary inference is executed by `scripts/run_bombus_local_sharp_transition.R`; community/elevation guardrails are executed by `scripts/run_bombus_spatial_replication_test.R`.

## Infrastructure modules

- `reproducibility.R` — provenance/hash helpers;
- `raster_sources.R` — environmental source-build support;
- `pipeline_support.R` — current module registry, argument and dependency helpers.

## Validation boundary

Reusable modules are unit tested under `tests/testthat/`, parsed by the repository CI and must be reachable from a current workflow or registered infrastructure path. Scientific definitions and expected numerical identities are documented in `paper/analysis-map.md` and `docs/reproduction-guide.md`.
