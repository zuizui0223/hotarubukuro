# Current paper — start here

This directory is the **single entry point for the manuscript-facing analysis**. Files not represented by the current analysis map are either implementation dependencies with an explicit current workflow route or historical material archived under `legacy/`.

## Target paper

Current first-choice submission target: **Journal of Biogeography**.

Current submission package:

- manuscript: `submission/jbi/JBI_main_manuscript_anonymized.md`
- title page: `submission/jbi/JBI_title_page_template.md`
- cover letter: `submission/jbi/JBI_cover_letter.md`
- four-main-figure plan: `submission/jbi/JBI_main_figure_plan.md`
- Supporting Information outline: `submission/jbi/JBI_supporting_information_outline.md`
- Appendices S1–S6: `submission/jbi/supporting/`
- Japanese translated abstract: `submission/jbi/JBI_translated_abstract_ja.md`
- submission-format validator: `submission/jbi/validate_jbi_submission.py`

The paper is intentionally one dependent ecological argument rather than an inventory of analyses tried during development.

## The paper in one line

`YAMAP/iEcology -> two-part flower-colour phenotype -> final Broad environment + space -> local focal-Bombus boundary test -> final-eight-axis natural-map departures -> post-selection human context`

## Data layer — YAMAP / iEcology

The current environment-complete integrated analysis contains 1,922 observations derived from the predefined 2023–2025 source frame. The image phenotype is constructed before ecological predictors enter. The YAMAP benchmark is a sampling-frame contribution, not a claim of unbiased occurrence sampling.

Primary files:

- `Data_S1.csv`
- `Code_S1.py`
- `source_build/build_data_s1.py`
- `source_build/extract_color.py`
- `reproducibility/yamap_public_database_benchmark_spec_2026-08-09.md`
- `reproducibility/yamap_public_database_benchmark_results_2026-08-09.md`

## Main 1 — finalized Broad natural template

Question: what environmental and continuous spatial structure organizes (i) pigmentation state and (ii) visible intensity among pigmented flowers across Japan?

Current population: **1,922 observations in 1,305 1-km cells; white-like=966; pigmented=956**.

Current execution/evidence:

- `scripts/run_environment_interaction_inla_screen.R`
- `.github/workflows/environment-interaction-inla-screen.yml`
- `scripts/run_broad_environment_spatial_audit.R`
- `analysis_sensitivity/run_broad_environment_spatial_audit_wrapper.R`
- `.github/workflows/broad-environment-spatial-audit.yml`
- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`

Final state model: eight measured abiotic axes + East/West + stationary SPDE, additive environmental structure. Final conditional-intensity model: the same framework plus Temperature PC1 × temperature-seasonality. VIF is treated as a graded stability diagnostic, not a hard deletion threshold.

The checksum-locked current-input/cell artifact (`9022276431`) remains a source evidence object used by later audits. Its historical generator also contained the superseded four-PC/17-candidate downstream branch, so that generator has been moved to `legacy/` and is not a current manuscript workflow.

## Main 2 — local focal-pollinator availability

Question: within the broad geographical template, do abrupt nearby white-pigmented transitions align directionally with predicted habitat opportunity for the documented broad focal pollinators *Bombus ardens* and *B. diversus*?

Primary design: Bombus/environment-blind pure non-overlapping transitions within 5 km; orientation occurs after pair identities are frozen.

Primary result: **67 pairs**, mean pigmented-minus-white occurrence-referenced support **+0.03590**, one-sided sign-flip **P=0.02716**, three-scale BH **q=0.08148**, median contrast -0.00277 and 49.3% positive. This is a weak, local, magnitude-driven correspondence, not evidence of pollinator-mediated selection.

Current files:

- `scripts/build_bombus_occurrence_reference_support.R`
- `scripts/run_bombus_local_sharp_transition.R`
- `.github/workflows/bombus-occurrence-reference-support.yml`
- `.github/workflows/bombus-local-sharp-transition.yml`
- `analysis_sensitivity/audit_bombus_final8_environment_distance.R`
- `.github/workflows/bombus-final8-environment-audit.yml`
- `submission/jbi/supporting/Appendix_S5_local_pollinator_robustness.md`

The final-eight-axis environmental-distance audit is a balance diagnostic only; it does not select or weight the 67 pairs.

## Main 3 — current-Broad local departures and human context

Question: how often does the finalized natural state model generate locally discordant pigmented cells, and what human context characterizes the independently selected observed candidates?

Primary event: pigmented focal cell, at least three neighbours within 10 km, final-eight-axis standardized RMS distance <=1, all eligible neighbours observed white. Human variables do not enter candidate selection.

Current result:

- **16 candidates**;
- candidate count natural-map P=0.27897;
- candidate fraction upper-tail P=0.12609;
- population exposure within 5 km: contrast +0.06744, directional P=0.00800, global maxT FWER P=0.05479;
- observation-effort and independent-source-support alternatives are null after maxT.

These are local-departure/provenance field targets, not demonstrated anthropogenic anomalies.

Current implementation:

- `R/natural_predictive_model.R`
- `R/candidate_null_tools.R`
- `R/local_pigmented_isolates.R`
- `R/local_human_context.R`
- `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`
- `.github/workflows/human-context-highrep-final.yml`
- `reproducibility/current_broad_human_primary_2026-08-12.md`
- `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`

The historical four-PC matching, 17-candidate output, older high-rep wrapper, candidate-DOY helper and former downstream validators/runners are preserved only under `legacy/`.

## Supporting Information hierarchy

- **S1:** YAMAP/public-database benchmark and observation-process framing.
- **S2:** deterministic image phenotyping and measurement/QC.
- **S3:** final Broad environment + spatial model, interaction/variable/spatial guardrails, and current Main-3 handoff.
- **S4:** fresh Bombus SDM source/calibration and occurrence-reference support.
- **S5:** local focal-Bombus robustness, final8 balance diagnostic, community-turnover Supporting analysis, montane/elevation negative guardrail.
- **S6:** final-eight-axis local departures, 10,000-map natural calibration, global-maxT human context and observation-process alternatives.

Supporting analyses constrain Main claims; they are not extra Main stories.

## Final integration and submission lock

Current integration:

- `.github/workflows/final-paper-analysis.yml`
- `reproducibility/final_integrated_pipeline_2026-08-12.md`
- `FINAL_PIPELINE_SUPP_MANUSCRIPT_AUDIT_2026-08-12.md`
- `paper/active-file-map.csv`
- `paper/analysis-map.md`

The integration workflow validates the frozen Broad/Bombus/human evidence and manuscript/Supp numerical hierarchy. It does not regenerate historical downstream candidates.

## Legacy rule

`legacy/` contains superseded drafts, abandoned estimands, previous publication architectures, the former current-input/four-PC downstream orchestration and historical validation/results. They remain auditable for provenance, but **nothing under `legacy/` is current manuscript evidence** unless a current reproducibility file explicitly cites it as historical provenance.
