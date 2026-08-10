# Current paper — start here

This directory is the **single entry point for the manuscript-facing analysis**. Files not listed here are either implementation dependencies needed to reproduce one of the stages below or historical material archived under `legacy/`.

## Target paper

Current first-choice submission target: **Journal of Biogeography**.

Current submission package:

- manuscript: `submission/jbi/JBI_main_manuscript_anonymized.md`
- title page: `submission/jbi/JBI_title_page_template.md`
- cover letter: `submission/jbi/JBI_cover_letter.md`
- four-main-figure plan: `submission/jbi/JBI_main_figure_plan.md`
- Supporting Information outline: `submission/jbi/JBI_supporting_information_outline.md`
- manuscript-ready YAMAP benchmark: `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md`
- Japanese translated abstract: `submission/jbi/JBI_translated_abstract_ja.md`
- submission-format validator: `submission/jbi/validate_jbi_submission.py`

The paper is intentionally written as **one dependent ecological argument**, not as an inventory of every analysis that was tried during development.

## The paper in one line

`YAMAP/iEcology data layer -> two-part flower-colour phenotype -> broad environment + space -> local focal-Bombus boundary test -> repeated ecological departures -> cautious human-context follow-up`

Each component is independently substantial, but the logic is sequential: the image stream makes the national quantitative phenotype possible; the broad template reveals why national pollinator-map overlap is ambiguous; that ambiguity motivates a local boundary test; and the broad natural reference is required before local departures can be calibrated and human context examined.

## Main text

### Data layer — YAMAP / iEcology

Purpose: build a recent, georeferenced, quantitative flower-colour dataset from recreational hiking photographs rather than a purpose-built biodiversity database.

Current evidence:

- eligible source-table records: 1,965;
- YAMAP activity-photo rows: 1,964;
- unique image hashes: 1,964 across the full source table and 1,963 within the YAMAP subset;
- final phenotype observations: 1,922;
- matched iNaturalist photo+geo observations: 516;
- matched GBIF records: 393, of which 389 were syndicated from iNaturalist.

The 1,965/1,964 distinction is intentional: the analysis source table contains one eligible record beyond the 1,964-row YAMAP benchmark subset. Manuscript and benchmark statements should therefore label the population being counted rather than use “source rows” without qualification.

Manuscript role: methodological and sampling-frame innovation. The mountain-route frame is useful for natural/semi-natural mountain trait geography but does not remove observer bias or guarantee wild provenance.

Primary files:

- `Data_S1.csv`
- `Code_S1.py`
- `source_build/build_data_s1.py`
- `source_build/extract_color.py`
- `reproducibility/yamap_public_database_benchmark_spec_2026-08-09.md`
- `reproducibility/yamap_public_database_benchmark_results_2026-08-09.md`

### Main 1 — Broad natural template

Question: what environmental and continuous spatial structure organizes (i) pigmentation state and (ii) visible intensity among pigmented flowers across Japan?

Active result population: 1,922 observations in 1,305 1-km cells; white=966; pigmented=956.

Primary reconstruction:

- `.github/workflows/reanalysis-current-inputs.yml`
- `scripts/run_reanalysis_current_inputs.sh`
- `scripts/run_downstream_current_inputs.sh`
- `scripts/report_reanalysis_current_inputs.R`
- `reproducibility/current_broad_anomaly_reference_2026-08-09.md`

Canonical manuscript artifact: workflow run `31258851297`, artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

### Main 2 — Local focal-pollinator availability

Question: within the broad template, do abrupt nearby white-pigmented transitions align directionally with predicted availability of the documented broad focal pollinators *Bombus ardens* and *B. diversus*?

Primary design: pure non-overlapping transitions within 5 km, selected without Bombus values or transition direction; occurrence-referenced focal-pollinator support is inspected only after the transition set is fixed.

Primary result: 67 pairs, median separation 2.0 km, mean pigmented-minus-white support +0.0359; one-sided sign-flip P=0.0272, but median/sign proportion/raw-SDM and broader-scale sensitivities make the result weak and local rather than robust evidence of selection.

Primary files:

- `scripts/build_bombus_occurrence_reference_support.R`
- `scripts/run_bombus_local_sharp_transition.R`
- `.github/workflows/bombus-occurrence-reference-support.yml`
- `.github/workflows/bombus-local-sharp-transition.yml`
- `reproducibility/bombus_occurrence_reference_support_2026-08-09.md`
- `reproducibility/bombus_local_sharp_transition_current_spec_2026-08-09.md`
- `reproducibility/bombus_local_sharp_transition_current_results_2026-08-09.md`

The frozen support artifact used by the manuscript was originally produced during a broader exploratory refinement; only its occurrence-reference support table is retained as current evidence. The superseded environment+SPDE directional refinement is under `legacy/`.

### Main 3 — Event-based departures and human context

Question: which local colour-state configurations depart from the broad natural reference, how often does the fitted natural model itself generate the same event, and what human context characterizes the independently defined candidates?

Primary event: a pigmented cell embedded among geographically close, environmentally similar white neighbours. Human variables are not used to define candidates.

Primary result: 17 candidates; their count/fraction are compatible with repeated natural predictive maps. Population/DID contrasts are suggestive but familywise inconclusive. These are provenance/field targets, not demonstrated anthropogenic anomalies.

Primary current scripts:

- `scripts/run_natural_predictive_model.R`
- `scripts/run_local_pigmented_isolates.R`
- `scripts/refine_submission_isolate_null.R`
- `scripts/run_joint_submission_isolate_ppc.R`
- `scripts/run_human_landscape_features.R`
- `scripts/run_local_human_context.R`
- `scripts/run_did_sensitivity.R`
- `scripts/run_candidate_doy_check.R`

These are orchestrated by `scripts/run_downstream_current_inputs.sh` after Main 1's fresh broad boundary is rebuilt.

## Supporting Information

Supporting analyses are important evidence but are **not additional Main stories**:

- YAMAP vs iNaturalist/GBIF benchmark and full quality matrix;
- complete broad-model coefficients and diagnostics;
- five-species Bombus community-turnover boundary correspondence;
- montane/alpine equal-elevation guardrail;
- all-five/raw-SDM/scale/threshold availability sensitivities;
- anomaly calibration and full human-context/maxT families.

Primary supplementary pollinator files:

- `scripts/run_bombus_spatial_replication_test.R`
- `.github/workflows/bombus-spatial-replication-test.yml`
- `reproducibility/bombus_spatial_replication_test_spec_2026-08-09.md`
- `reproducibility/bombus_spatial_replication_test_results_2026-08-09.md`

## Infrastructure that remains active but is not a paper result

`inputs/canonical_snapshot.json` and `scripts/canonical_snapshot.sh` are retained only to restore static WorldPop/MLIT/DID support files for the current human-context stage. The old 1,909 flower-population identity associated with that snapshot is not a current manuscript constraint.

## Final integration

The manuscript-facing integration workflow is:

- `.github/workflows/final-paper-analysis.yml`

The scientific hierarchy and pinned artifacts are recorded in:

- `reproducibility/final_paper_pipeline_2026-08-09.md`
- `paper/active-file-map.csv`
- `paper/analysis-map.md`

## Legacy rule

`legacy/` contains superseded drafts, abandoned estimands, old gates, development diagnostics and previous publication architectures. They remain available for provenance, but **nothing under `legacy/` should be cited as current manuscript evidence unless the active paper map explicitly labels it as historical provenance**.
