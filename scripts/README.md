# Manuscript-facing scripts

Start with [`../paper/README.md`](../paper/README.md). This directory indexes executable scripts used by the adopted analysis, figure generation and submission package.

## Broad environment + space

- `run_environment_interaction_inla_screen.R` — evaluate the declared ecological interaction family and exhaustive pairwise guardrail under the common Broad model.
- `run_broad_environment_spatial_audit.R` — evaluate environmental completeness and alternative spatial specifications.

Execution:

- `.github/workflows/environment-interaction-inla-screen.yml`;
- `.github/workflows/broad-environment-spatial-audit.yml`.

Final decisions are in `../reproducibility/broad_environment_spatial_final_model_2026-08-11.md` and Appendix S3.

## Local departures and human context

- `../analysis_sensitivity/run_human_context_current_broad_primary_fast.R` — rebuild the final-eight-axis local graph, replay the event detector on 10,000 locked predictive maps and evaluate the global maxT context family.

Execution:

- `.github/workflows/human-context-highrep-final.yml`.

Expected identity: 16 candidates; candidate-count P=0.27897; 5-km population contrast +0.06744; global maxT FWER P=0.05479.

## Local focal Bombus

- `build_bombus_occurrence_reference_support.R` — convert selected Bombus SDMs to species-specific occurrence-referenced support.
- `run_bombus_local_sharp_transition.R` — fixed-pair local white-pigmented directional test.
- `run_bombus_spatial_replication_test.R` — Supporting five-species community-boundary and elevation guardrails.

Environmental pair-balance audit:

- `../analysis_sensitivity/audit_bombus_final8_environment_distance.R`;
- `.github/workflows/bombus-final8-environment-audit.yml`.

## JBI figures

- `build_jbi_figure_bundle.R` — shared figure-data/panel builder;
- `build_jbi_figure_bundle_final_broad.R` — adapter that binds the finalized Broad, Bombus-balance and local-departure evidence to Figures 2–4;
- `render_jbi_main_figures.R` — journal-width renderer and map-scale layer;
- `render_jbi_main_figures_final_broad.R` — current renderer entry point.

Execution:

- `.github/workflows/jbi-main-figures.yml`.

## JBI delivery package

- `build_jbi_submission_bundle.py` — assemble the editable review package;
- `jbi_submission_bundle_core.py` — shared document-generation implementation;
- `validate_jbi_submission_bundle.py` — validate DOCX structure, anonymity, figures, SI, hashes and archive contents.

Execution:

- `.github/workflows/jbi-submission-bundle.yml`.

## Environment and infrastructure

- `setup_r_environment.R` — restore the pinned R/INLA environment used by current workflows.

Python dependencies are declared in `../pyproject.toml`; R/system dependencies are declared under `../dependencies/`.

The authoritative script/workflow interface is `../paper/active-file-map.csv`. Exact scientific evidence identities are in `../paper/analysis-map.md`.
