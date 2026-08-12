# Current paper — scientific and reproducibility entry point

This page is the **single entry point** for the adopted analysis. A reader should be able to move from the biological question to the exact code, evidence and validation route without reconstructing project development history.

## Paper in one line

`YAMAP/iEcology -> two-part flower-colour phenotype -> broad environment + space -> local focal-Bombus boundary test -> final-eight-axis natural-map departures -> post-selection human context`

## Submission sources

- manuscript: `submission/jbi/JBI_main_manuscript_anonymized.md`
- title page: `submission/jbi/JBI_title_page_template.md`
- cover letter: `submission/jbi/JBI_cover_letter.md`
- Figure 1–4 captions/plan: `submission/jbi/JBI_main_figure_captions.md`, `submission/jbi/JBI_main_figure_plan.md`
- Supporting Information: `submission/jbi/supporting/`
- Japanese translated abstract: `submission/jbi/JBI_translated_abstract_ja.md`
- submission validator: `submission/jbi/validate_jbi_submission.py`

## 1. YAMAP / iEcology phenotype layer

**Question:** can a recreational, GPS-linked image stream support a quantitative national trait dataset after explicit taxon, flower and image-quality review?

The environment-complete integrated analysis contains **1,922 observations**. The phenotype is constructed before ecological predictors enter and has two responses: white/pigmented state and visible intensity conditional on pigmentation.

Key files:

- `Data_S1.csv`
- `Code_S1.py`
- `source_build/build_data_s1.py`
- `source_build/extract_color.py`
- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md`
- `submission/jbi/supporting/Appendix_S2_image_phenotyping.md`

The public-database benchmark characterizes the sampling frame and observation process; it is not a claim that YAMAP is unbiased occurrence sampling.

## 2. Broad environment + spatial template

**Question:** what measured environmental and continuous spatial structure organizes pigmentation state and visible intensity among pigmented flowers across Japan?

Analysis population: **1,922 observations in 1,305 1-km cells; white-like=966; pigmented=956**.

Adopted models:

- state: eight measured abiotic axes + East/West + stationary SPDE, additive environmental structure;
- conditional intensity: the same framework + Temperature PC1 × temperature-seasonality.

Key execution/evidence:

- `scripts/run_environment_interaction_inla_screen.R`
- `.github/workflows/environment-interaction-inla-screen.yml`
- `scripts/run_broad_environment_spatial_audit.R`
- `analysis_sensitivity/run_broad_environment_spatial_audit_wrapper.R`
- `.github/workflows/broad-environment-spatial-audit.yml`
- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`

VIF is used as a graded stability diagnostic. Model promotion also requires geographical-transfer evidence rather than fit statistics alone.

## 3. Local focal-Bombus boundary test

**Question:** do abrupt nearby white-pigmented transitions align directionally with predicted habitat opportunity for the broad focal pollinators *Bombus ardens* and *B. diversus*?

Pair identities are selected without Bombus information. The primary design contains **67 pure non-overlapping transitions within 5 km**. The occurrence-referenced pigmented-minus-white support contrast is +0.03590 on average; one-sided sign-flip P=0.02716, three-scale BH q=0.08148, median=-0.00277 and 49.3% of pairs are positive. The manuscript therefore treats this as weak local correspondence, not evidence of pollinator-mediated selection.

Key files:

- `scripts/build_bombus_occurrence_reference_support.R`
- `scripts/run_bombus_local_sharp_transition.R`
- `.github/workflows/bombus-occurrence-reference-support.yml`
- `.github/workflows/bombus-local-sharp-transition.yml`
- `analysis_sensitivity/audit_bombus_final8_environment_distance.R`
- `.github/workflows/bombus-final8-environment-audit.yml`
- `submission/jbi/supporting/Appendix_S4_bombus_sdm_occurrence_support.md`
- `submission/jbi/supporting/Appendix_S5_local_pollinator_robustness.md`

The final-eight-axis environmental-distance analysis is a diagnostic of the already fixed pairs; it does not select or weight them.

## 4. Calibrated local departures and human context

**Question:** how often does the finalized natural state reference generate locally discordant pigmented cells, and what human context characterizes independently selected observed events?

Primary event definition:

- pigmented focal cell;
- at least three neighbours within 10 km;
- standardized RMS distance <=1 across the finalized eight abiotic axes;
- all eligible observed neighbours are white.

Human variables are absent from event selection.

Current evidence:

- **16 observed candidates**;
- candidate-count natural-map P=0.27897;
- candidate-fraction upper-tail P=0.12609;
- 5-km population exposure contrast +0.06744, directional P=0.00800, global maxT FWER P=0.05479;
- observation-effort and independent-source-support alternatives are null after maxT.

These events are field/provenance targets, not demonstrated anthropogenic anomalies.

Key files:

- `R/natural_predictive_model.R`
- `R/candidate_null_tools.R`
- `R/local_pigmented_isolates.R`
- `R/local_human_context.R`
- `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`
- `.github/workflows/human-context-highrep-final.yml`
- `reproducibility/current_broad_human_primary_2026-08-12.md`
- `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`

## 5. Supporting evidence hierarchy

- **S1:** YAMAP/public-database benchmark and observation-process framing
- **S2:** deterministic image phenotyping and measurement QC
- **S3:** broad environmental/spatial model and guardrails
- **S4:** Bombus SDM source, calibration and occurrence-reference transformation
- **S5:** local Bombus robustness, environmental-balance diagnostic, community turnover and elevation guardrails
- **S6:** local departures, 10,000-map calibration, global-maxT human context and observation-process alternatives

Supporting analyses constrain the claim ceiling; they are not additional Main mechanisms.

## 6. Reproduction and evidence locks

Use these in order:

1. `paper/analysis-map.md` — maps each scientific claim to the evidence/workflow used by the manuscript.
2. `docs/reproduction-guide.md` — explains how to restore or rerun each stage.
3. `paper/active-file-map.csv` — machine-readable registry of the current public interface.
4. `reproducibility/final_integrated_pipeline_2026-08-12.md` — canonical integrated numerical/evidence lock.
5. `.github/workflows/final-paper-analysis.yml` — checks that Broad, Bombus, local-departure and manuscript/SI evidence agree.

Large raster-derived and predictive-draw inputs are restored by checksum rather than silently reacquired from mutable external services. A refreshed external source is treated as a new analysis.

## 7. Data availability boundary

- `Data_S1.csv` is the distributable derived trait/source table.
- Original YAMAP photographs are third-party content and are not redistributed.
- Source-construction utilities and declared external inputs live under `source_build/`, `config/` and the reproduction guide.
- Random seeds, model/event definitions, artifact identities and claim ceilings are versioned.
