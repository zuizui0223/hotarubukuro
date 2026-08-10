# Current manuscript-facing scripts

Start with [`../paper/README.md`](../paper/README.md). This directory contains only current reusable infrastructure plus scripts feeding the active broad/anomaly or local-pollinator analyses. Superseded runners and estimands are under `legacy/`.

## Broad natural template + event-based departures

- `run_reanalysis_current_inputs.sh` — rebuild the fresh 1,965-row upstream environment/phenotype boundary.
- `run_downstream_current_inputs.sh` — run the natural predictive reference, event-based departure calibration and post-selection human context, with each current independent validator and reconstruction-aware claim audit executed immediately after the stage it checks.
- `report_reanalysis_current_inputs.R` — write the fresh manuscript summary and collect the actual current validation/audit outputs rather than historical generic validation paths.
- `run_natural_predictive_model.R`
- `run_local_pigmented_isolates.R`
- `refine_submission_isolate_null.R`
- `run_joint_submission_isolate_ppc.R`
- `run_human_landscape_features.R`
- `run_local_human_context.R`
- `run_did_sensitivity.R`
- `run_candidate_doy_check.R`

The validation routing and the distinction between structural PASS/FAIL checks and reconstruction-specific `RESULT` findings are documented in [`../validation/README.md`](../validation/README.md).

## Local pollinator analysis

- `build_bombus_occurrence_reference_support.R` — convert each selected fresh Bombus SDM to an occurrence-referenced support scale; the Main exposure uses *B. ardens* + *B. diversus*.
- `run_bombus_local_sharp_transition.R` — manuscript-facing 5-km white-pigmented boundary test.
- `run_bombus_spatial_replication_test.R` — Supporting Information five-species community-boundary and montane/elevation guardrails.

The current biological interpretation and claim ceiling are in `../docs/bombus-inference-current.md`.

## JBI figure production

- `build_jbi_figure_bundle.R` — generate the four current Main figures from the checksum-locked broad/anomaly and local-transition artifacts. It writes 600-dpi PNG and vector PDF copies, figure-data indices, source hashes and a numerical lock under `results/jbi_figure_bundle/`.

The corresponding execution route is `.github/workflows/jbi-main-figures.yml`; `validation/validate_jbi_figure_bundle.R` verifies all eight figure files, their signatures and hashes, and the manuscript-facing numerical values before the bundle is uploaded as an Actions artifact.

## Current infrastructure

- `setup_r_environment.R` — restore the pinned R/INLA environment used by current workflows.
- `canonical_snapshot.sh` — restore the checksum-locked static human-context support bundle required downstream.

The old full-canonical snapshot publisher/verifier, generic preflight/reproduction reporter and fixed-result submission-lock writer are archived under `legacy/reproducibility-development/scripts/`. Historical result-identity audits are under `legacy/reproducibility-development/validation/`; neither location supplies current JBI acceptance criteria.

The authoritative list of manuscript-facing files is `../paper/active-file-map.csv`; files are not current merely because they remain executable.
