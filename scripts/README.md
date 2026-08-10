# Current manuscript-facing scripts

Start with [`../paper/README.md`](../paper/README.md). This directory contains only current reusable infrastructure plus scripts feeding the active broad/anomaly or local-pollinator analyses. Superseded runners and estimands are under `legacy/method-development/`.

## Broad natural template + event-based departures

- `run_reanalysis_current_inputs.sh` — rebuild the fresh 1,965-row upstream environment/phenotype boundary.
- `run_downstream_current_inputs.sh` — natural predictive reference, event-based departure calibration and post-selection human context.
- `report_reanalysis_current_inputs.R` — fresh 1,922-observation manuscript summary.
- `run_natural_predictive_model.R`
- `run_local_pigmented_isolates.R`
- `refine_submission_isolate_null.R`
- `run_joint_submission_isolate_ppc.R`
- `run_human_landscape_features.R`
- `run_local_human_context.R`
- `run_did_sensitivity.R`
- `run_candidate_doy_check.R`

## Local pollinator analysis

- `build_bombus_occurrence_reference_support.R` — convert each selected fresh Bombus SDM to an occurrence-referenced support scale; the Main exposure uses *B. ardens* + *B. diversus*.
- `run_bombus_local_sharp_transition.R` — manuscript-facing 5-km white-pigmented boundary test.
- `run_bombus_spatial_replication_test.R` — Supporting Information five-species community-boundary and montane/elevation guardrails.

The current biological interpretation and claim ceiling are in `../docs/bombus-inference-current.md`.

## Infrastructure

Snapshot/environment/setup helpers remain outside `legacy/` only when a current workflow calls them. The authoritative list of manuscript-facing files is `../paper/active-file-map.csv`; files are not current merely because they remain executable.
