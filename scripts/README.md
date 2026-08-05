# Active scripts

## Supported entry points

- `run_analysis_1909.sh`: restores inputs, verifies the 1,909 population, runs all stages, builds figures and writes the run summary.
- `run_publication_pipeline.R`: ordered numerical-stage and validation orchestrator.

## Numerical stages

- `run_natural_predictive_model.R`
- `run_local_bombus_turnover.R`
- `run_human_landscape_features.R`
- `run_local_pigmented_isolates.R`
- `run_local_human_context.R`
- `run_did_sensitivity.R`

`run_candidate_doy_check.R` is supplementary and cannot select candidates.

## Reproducibility support

Snapshot, environment, preflight, population-check, reporting and figure scripts remain here because they are called by the supported complete runner.

Raw/public-data construction utilities are under `source_build/`. Superseded runners, reconstruction experiments and the post-hoc asymmetry runner are under `legacy/`.
