# R modules

`R/pipeline_support.R` is the only active module loader. Its `hb_module_files` registry is authoritative.

## Active scientific modules

- `natural_predictive_model.R`
- `local_bombus_turnover.R`
- `candidate_null_tools.R`
- `human_landscape_features.R`
- `local_pigmented_isolates.R`
- `human_raster_features.R`
- `local_human_context.R`
- `spatial_context.R`
- `did_sensitivity.R`
- `final_registry.R`

`candidate_doy_check.R` is an active supplementary module used only after candidates are fixed. `reproducibility.R` provides provenance helpers. `raster_sources.R` supports optional utilities under `source_build/` and is not loaded by the canonical pipeline.

Superseded v11/v15 modules and the post-hoc asymmetry module are under `legacy/`; active code may not import them.
