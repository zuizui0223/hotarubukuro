# Manuscript-facing analysis map

This file maps each manuscript claim to the adopted implementation, checksum-locked evidence and claim ceiling. It is the compact scientific index for reproducing the paper.

## Evidence hierarchy

| Layer | Ecological question | Role | Adopted analysis | Claim ceiling |
|---|---|---|---|---|
| YAMAP/iEcology | Can a recent national quantitative flower-colour dataset be built from recreational imagery after explicit review? | cross-cutting data/method | YAMAP benchmark + author-screened `Data_S1.csv` + deterministic colour extraction | complementary observation process; not unbiased occurrence sampling |
| Broad environment + space | What broad measured environment and unresolved geography organize pigmentation state and pigmented-only intensity? | Main | response-specific INLA-SPDE models using eight abiotic axes; state additive; intensity includes Temperature PC1 × temperature-seasonality | geographical association/residual structure; not direct causation |
| Local focal Bombus | Do sharp nearby white-pigmented boundaries align directionally with habitat opportunity for documented broad focal bumblebees? | Main | five-species SDM source -> occurrence-reference calibration -> 67 fixed 5-km pure non-overlap transitions -> signed *B. ardens*/*B. diversus* contrast | weak/local correspondence; not pollinator-mediated selection |
| Local departures + human context | How often do locally discordant pigmented configurations arise under the finalized natural state geography, and what human context characterizes observed events? | Main | final-eight-axis cross-fitted state maps + final-eight-axis RMS matching + 10,000-map replay + post-selection global-maxT family | field/provenance targets; near-threshold settlement exposure; no anthropogenic-origin claim |
| Bombus community turnover | Do colour boundaries also coincide with broader predicted Bombus-community boundaries? | Supporting | five-species matched Hellinger turnover | unsigned biogeographic correspondence |
| Montane/elevation guardrail | Does high-elevation Bombus overlap provide an independent directional mechanism? | Supporting/negative guardrail | near-equal-elevation transition comparison | no independent effect beyond shared elevational geography |

## Dependency and anti-circularity

1. The flower-colour phenotype is constructed without environmental, Bombus or human predictors.
2. Broad environment + space establishes the national natural template.
3. Bombus SDMs are built from Bombus occurrence and environmental data, not flower colour.
4. Local Bombus pair identities are fixed without Bombus values and oriented only after selection.
5. Final-eight-axis environmental distance is a balance diagnostic of the fixed Bombus pairs and does not select or weight them.
6. Local-departure candidates are defined from the finalized natural state space before human variables are read.
7. The same event detector is replayed on 10,000 predictive maps.
8. Human-context variables are then evaluated in one global maxT family.

The paper therefore connects processes across scales rather than estimating one omnibus regression and interpreting its coefficients as independent mechanisms.

## YAMAP / phenotype layer

### Implementation

- `Data_S1.csv`
- `Code_S1.py`
- `source_build/build_data_s1.py`
- `source_build/extract_color.py`
- `.github/workflows/yamap-public-database-benchmark.yml`
- `.github/workflows/yamap-public-database-overlap-audit.yml`

### Documentation

- `docs/data-s1-dictionary.md`
- `docs/yamap-iecology-rationale.md`
- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md`
- `submission/jbi/supporting/Appendix_S2_image_phenotyping.md`

### Locked population

- 1,922 environment-complete observations;
- 966 white-like;
- 956 pigmented;
- 1,305 1-km cells.

## Broad environment + space

### Implementation

- `scripts/run_environment_interaction_inla_screen.R`
- `.github/workflows/environment-interaction-inla-screen.yml`
- `scripts/run_broad_environment_spatial_audit.R`
- `analysis_sensitivity/run_broad_environment_spatial_audit_wrapper.R`
- `.github/workflows/broad-environment-spatial-audit.yml`

### Evidence/decision files

- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `reproducibility/broad_environment_spatial_final_fixed_effects_2026-08-11.csv`
- `reproducibility/broad_environment_spatial_final_hyperparameters_2026-08-11.csv`
- `reproducibility/broad_environment_variable_evidence_registry_2026-08-11.csv`
- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`

### Adopted models

Pigmentation state:

`state ~ eight abiotic axes + East/West + stationary SPDE`

Conditional visible intensity:

`intensity ~ eight abiotic axes + East/West + Temperature PC1:temperature seasonality + stationary SPDE`

Key diagnostics:

- state maximum VIF=4.430;
- intensity maximum VIF=6.340;
- retained interaction VIF=1.664;
- state spatial range=132.76 km;
- intensity spatial range=65.72 km.

### Source evidence artifact

- run `31258851297`
- artifact `9022276431`
- SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`

## Local focal Bombus

### Source/SDM implementation

- `config/bombus_sdm.yml`
- `source_build/fetch_bombus_occurrences.R`
- `source_build/canonicalize_bombus_occurrences.R`
- `source_build/build_bombus_sdm_mainland.R`
- `.github/workflows/rebuild-bombus-sdm.yml`

### Occurrence-reference and local test

- `scripts/build_bombus_occurrence_reference_support.R`
- `.github/workflows/bombus-occurrence-reference-support.yml`
- `R/local_pair_graph.R`
- `scripts/run_bombus_local_sharp_transition.R`
- `.github/workflows/bombus-local-sharp-transition.yml`
- `analysis_sensitivity/audit_bombus_final8_environment_distance.R`
- `.github/workflows/bombus-final8-environment-audit.yml`

### Supporting guardrails

- `scripts/run_bombus_spatial_replication_test.R`
- `.github/workflows/bombus-spatial-replication-test.yml`
- `submission/jbi/supporting/Appendix_S4_bombus_sdm_occurrence_support.md`
- `submission/jbi/supporting/Appendix_S5_local_pollinator_robustness.md`

### Primary numerical identity

- 67 pure non-overlapping 5-km pairs;
- mean focal contrast +0.03590;
- median -0.00277;
- 49.3% positive;
- one-sided P=0.02716;
- three-scale BH q=0.08148;
- raw-cloglog 5-km P=0.26715.

Final-eight-axis balance at 5 km:

- selected median RMS=0.24408;
- all-local-edge median RMS=0.31752.

### Evidence artifacts

Bombus source build:

- run `31249841493`
- artifact `9020226937`
- SHA-256 `d5d639e8e00d1ccc2f887c53fa8041465905b29f6bca1127f816e8c7a649d708`

Occurrence-reference support:

- run `31262211605`
- artifact `9023137743`
- SHA-256 `d04c561b09b652591b9b479f6e26a779bb562c7c1b5f9b14e61d5e7ca8e2794b`

Local sharp transitions:

- run `31263324505`
- artifact `9023416810`
- SHA-256 `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`

Final-eight-axis pair-balance audit:

- run `31538548679`
- artifact `9119773035`
- SHA-256 `51fc846d2f3d815d8bbf3c0b95647eabeb0acf731f1e6dd164c11a0dfe1b896f`

Community/elevation guardrails:

- run `31285234317`
- artifact `9029595037`
- SHA-256 `067dd3408b2a7f046ba263732ffa4cefa2f54a7f1fb672478be46bca425f6bf0`

## Calibrated local departures + human context

### Implementation

- `R/natural_predictive_model.R`
- `R/candidate_null_tools.R`
- `R/local_pigmented_isolates.R`
- `R/human_landscape_features.R`
- `R/human_raster_features.R`
- `R/local_human_context.R`
- `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`
- `.github/workflows/human-context-highrep-final.yml`

### Event definition

- focal cell contains pigmentation;
- neighbours within 10 km;
- standardized RMS distance <=1 across the finalized eight abiotic axes;
- at least three eligible neighbours;
- all eligible observed neighbours are white;
- human variables absent from event selection.

### Primary numerical identity

- 16 observed candidates;
- 10,000 predictive maps;
- candidate-count P=0.27897;
- candidate-fraction upper-tail P=0.12609;
- 5-km population contrast +0.06744;
- directional P=0.00800;
- global maxT FWER P=0.05479;
- observation-effort maxT P=0.96320;
- independent-site-support maxT P=0.75642.

### Evidence

- `reproducibility/current_broad_human_primary_2026-08-12.md`
- `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`
- replay run `31537102360`
- artifact `9119306089`
- SHA-256 `f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4`
- predictive-draw artifact `9094339466`
- SHA-256 `413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`

## Figures and submission

Current figure generation:

- `scripts/build_jbi_figure_bundle.R`
- `scripts/build_jbi_figure_bundle_final_broad.R`
- `scripts/render_jbi_main_figures.R`
- `scripts/render_jbi_main_figures_final_broad.R`
- `.github/workflows/jbi-main-figures.yml`

Current corrected figure evidence:

- run `31559274663`
- artifact `9127198711`
- SHA-256 `ff5d43e8f71224261b8b74ddb2d6e24a66a4f2349ad53fb72032118492bca924`

Submission generation/validation:

- `.github/workflows/jbi-submission-bundle.yml`
- `scripts/build_jbi_submission_bundle.py`
- `scripts/validate_jbi_submission_bundle.py`
- `submission/jbi/validate_jbi_submission.py`

## Integrated acceptance check

- `.github/workflows/final-paper-analysis.yml`
- `reproducibility/final_integrated_pipeline_2026-08-12.md`
- `FINAL_PIPELINE_SUPP_MANUSCRIPT_AUDIT_2026-08-12.md`

The integration workflow restores the locked Broad, Bombus and local-departure evidence and verifies that manuscript, Supporting Information and figures use the same current numerical identities and claim ceilings.
