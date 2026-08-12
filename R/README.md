# R modules

`R/pipeline_support.R` is the shared loader for reusable modules still needed by the current JBI analysis and checksum-locked current-Broad human replay. Its registry must not point into `legacy/`.

## Current scientific modules

- `natural_predictive_model.R` — environment + SPDE predictive-reference utilities used to construct the locked natural-map framework;
- `candidate_null_tools.R` — common event/null utilities for local-departure replay;
- `human_landscape_features.R` — landscape/context feature construction used in the frozen human-support source;
- `local_pigmented_isolates.R` — local pigmentation-state event utilities used by the current final-eight-axis detector;
- `human_raster_features.R` — raster-derived human-context support;
- `local_human_context.R` — post-selection candidate-context utilities;
- `spatial_context.R` — spatial support utilities for the human-context layer;
- `did_sensitivity.R` — DID/context feature utilities retained because DID remains in the current global human family;
- `local_pair_graph.R` — deterministic local graph/pair utilities for the focal-Bombus boundary test.

`reproducibility.R` provides provenance helpers. `raster_sources.R` supports current environmental source-build workflows.

The old candidate-DOY helper is no longer a current Supporting analysis: it fed no candidate selection or Main claim and was tied to the superseded downstream candidate definition. It is preserved under `legacy/reproducibility-development/superseded-current-input-anomaly-pipeline-2026-08-12/R/`.

Main-2 sharp-transition inference remains in `scripts/run_bombus_local_sharp_transition.R` plus `R/local_pair_graph.R`; five-species community-boundary and montane guardrails remain in `scripts/run_bombus_spatial_replication_test.R`.

Superseded local-Bombus-turnover, final-registry, fixed-n, limitation-gate, four-PC/17-candidate downstream and other historical modules belong under `legacy/`. Current code must not import them.
