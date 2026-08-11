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

## Environmental interaction sensitivity

- `run_environment_interaction_inla_screen.R` — compare the frozen additive observation-level INLA-SPDE models with ten predeclared ecologically motivated interactions, four mechanism bundles and one global interaction set for both pigmentation state and pigmented-only intensity.

The corresponding workflow is `.github/workflows/environment-interaction-inla-screen.yml`. It restores the checksum-locked broad-analysis artifact, verifies that the independently reconstructed additive model matches the manuscript lock, then compares candidate models by WAIC/CPO, the same five geographical folds, spatial-block bootstrap loss gain, VIF, coefficient stability and SPDE-range stability. No interaction replaces the additive Main model automatically; see `../reproducibility/environment_interaction_inla_screen_spec_2026-08-11.md`.

## Broad environmental and spatial finalization

- `run_broad_environment_spatial_audit.R` — compare the current eight-axis observation-level model with omitted public hydroclimate, climate-extreme, habitat, coastality, temporal and image-QC sensitivities; adjudicate the seasonality interactions; and compare stationary, no-region, repeated-site and ocean-barrier SPDE structures using the same five geographical folds.

The corresponding workflow is `.github/workflows/broad-environment-spatial-audit.yml`. It restores the checksum-locked broad artifact, prepares CHELSA VPD, site water balance, BIO6, BIO13 and the elevation land mask, joins the current 1-km forest fraction, and writes response-specific decision tables. It never silently replaces the separate cell-level natural predictive reference or the 17 departure targets. See `../reproducibility/broad_environment_spatial_audit_spec_2026-08-11.md` and `../reproducibility/broad_environment_variable_evidence_registry_2026-08-11.csv`.

## Local pollinator analysis

- `build_bombus_occurrence_reference_support.R` — convert each selected fresh Bombus SDM to an occurrence-referenced support scale; the Main exposure uses *B. ardens* + *B. diversus*.
- `run_bombus_local_sharp_transition.R` — manuscript-facing 5-km white-pigmented boundary test.
- `run_bombus_spatial_replication_test.R` — Supporting Information five-species community-boundary and montane/elevation guardrails.

The current biological interpretation and claim ceiling are in `../docs/bombus-inference-current.md`.

## JBI figure production

- `build_jbi_figure_bundle.R` — assemble the four current Main plots from the checksum-locked broad/anomaly and local-transition artifacts, together with figure-data indices, source hashes and the numerical lock.
- `render_jbi_main_figures.R` — apply the current two-column JBI layout, shorten display-only labels, prevent clipping and write the final 600-dpi PNG and vector PDF copies under `results/jbi_figure_bundle/` without changing plotted values.

The corresponding execution route is `.github/workflows/jbi-main-figures.yml`; `validation/validate_jbi_figure_bundle.R` verifies all eight final figure files, their signatures and hashes, and the manuscript-facing numerical values before the bundle is uploaded as an Actions artifact.

## JBI submission delivery

- `build_jbi_submission_bundle.py` — convert the current Markdown sources into an anonymized Main DOCX with embedded Figures 1–4, one combined Supporting Information DOCX, identifying templates, a translated abstract, an SDM checklist, separate figures, readiness records, hashes and a ZIP archive.
- `validate_jbi_submission_bundle.py` — independently verify DOCX structure, anonymity, four embedded figures, Appendices S1–S6, separate PNG/PDF signatures, file hashes, known portal blockers and archive contents.

`.github/workflows/jbi-submission-bundle.yml` restores the fixed Main-figure artifact, builds the editable package and converts all generated DOCX files through LibreOffice. The builder never fills author-controlled names, affiliations, ORCIDs, funding, conflicts, contributions, private repository links, disclosures or taxon-image information.

## Current infrastructure

- `setup_r_environment.R` — restore the pinned R/INLA environment used by current workflows.
- `canonical_snapshot.sh` — restore the checksum-locked static human-context support bundle required downstream.

The old full-canonical snapshot publisher/verifier, generic preflight/reproduction reporter and fixed-result submission-lock writer are archived under `legacy/reproducibility-development/scripts/`. Historical result-identity audits are under `legacy/reproducibility-development/validation/`; neither location supplies current JBI acceptance criteria.

The authoritative list of manuscript-facing files is `../paper/active-file-map.csv`; files are not current merely because they remain executable.
