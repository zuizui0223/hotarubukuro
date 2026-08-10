# Current validation boundary

This directory contains only checks that are reachable from a current workflow or current source build. A file is not current merely because it can still be executed.

## Broad natural template and event-based departures

`scripts/run_downstream_current_inputs.sh` runs the current checks immediately after the stage they inspect:

- `validate_natural_predictive_model.R` and `audit_natural_predictive_model.R` — independent arithmetic/model-structure validation and the environment-only national claim ceiling;
- `validate_human_landscape_features.R --baseline=reconstruction` — independent reconstruction-aware validation of post-selection landscape features;
- `validate_local_pigmented_isolates.R` and `audit_local_pigmented_isolates.R --baseline=reconstruction` — candidate-definition reproduction plus reconstruction-aware claim reporting;
- `validate_candidate_doy_check.R` — model-free Supporting Information flowering-date arithmetic;
- `validate_local_human_context.R` and `audit_local_human_context.R`, both with `--baseline=reconstruction`;
- `validate_did_sensitivity.R` and `audit_did_sensitivity.R`, both with `--baseline=reconstruction`;
- `validate_joint_submission_isolate_ppc.R` — dynamic candidate identity, nested draw counts, boundary audit and manifest hashes.

Under a reconstruction baseline, numerical findings that belong to a particular observation set are reported as `RESULT` or `not_applicable`; they are not relabelled as passes and are not forced to equal a historical run. Structural invariants, independent recalculations, selection independence and claim ceilings remain PASS/FAIL checks.

## Bombus source build

`compare_bombus_sdm_rebuilds.R` is called by `.github/workflows/rebuild-bombus-sdm.yml` to compare two seeded mainland SDM rebuilds.

## Historical audits

Audits tied to the old v11/v15 publication boundary or to fixed historical inferential outcomes are preserved under `legacy/reproducibility-development/validation/`. They must not be invoked from current workflows.
