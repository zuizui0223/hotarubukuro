# Manuscript-facing analysis map

This file maps the current paper from biological question to implementation, evidence and inference ceiling. The analysis is one dependency chain: a national flower-colour polymorphism is measured, its broad environmental and residual geography is established, a local pollinator-maintenance hypothesis is tested at sharp state boundaries, and local departures are calibrated before contemporary human context is examined.

## Evidence hierarchy

| Layer | Ecological question | Current implementation | Main evidence | Inference ceiling |
|---|---|---|---|---|
| YAMAP / iEcology | Can a contemporary national flower-colour polymorphism be measured quantitatively from an alternative digital observation stream? | author-screened YAMAP photographs -> deterministic visible-colour extraction -> pigmentation state + pigmented-only intensity | 1,922 environment-complete observations; 966 white-like, 956 pigmented | quantitative visible phenotype and complementary observation process; not calibrated spectroscopy, pigment chemistry or unbiased occurrence sampling |
| Broad environment + space | Do pigmentation state and intensity occupy the same macroecological landscape, and what regional structure remains after measured environment? | response-specific INLA-SPDE models using eight abiotic axes, East/West structural geography and stationary Matérn fields | state: cool-climate association, additive; intensity: Temperature PC1 × temperature-seasonality plus moisture/terrain context; residual ranges 132.76 and 65.72 km | candidate adaptive/developmental landscape plus unresolved geography; not direct local adaptation, physiology, dispersal or population history |
| Local focal Bombus | Does predicted focal-pollinator opportunity align directionally with abrupt local pigmentation-state boundaries? | five-species SDMs -> occurrence-reference calibration -> Bombus/environment-blind 5-km pure non-overlap transitions -> signed *B. ardens*/*B. diversus* contrast | 67 pairs; mean +0.03590; median -0.00277; 49.3% positive; P=.02716; q=.08148; scale/raw-support fragility | weak, local, magnitude-driven opportunity correspondence; if biological, compatible with maintenance/loss of pigmented state; not realized visitation or pollinator-mediated selection |
| Bombus biogeographic guardrails | Could broader Bombus community or montane overlap be mistaken for the same directional mechanism? | five-species Hellinger turnover and near-equal-elevation montane comparisons | community-boundary correspondence is unsigned; montane signed contrast disappears after elevation control | supporting biogeography and negative confounding guardrail; not a second positive pollinator mechanism |
| Local departures | Are locally discordant pigmented configurations excessive relative to the fitted natural geography? | final-eight-axis ecological event detector replayed on 10,000 cross-fitted natural maps | 16 observed candidates; count P=.27897; candidate-fraction P=.12609 | reproducible field targets that are not collectively anomalous under the natural model |
| Contemporary human context | After natural calibration and candidate freezing, do local departures occupy distinctive human context? | one global maxT family across population, DID, road, built-land, forest-interface and natural-alternative features | population within 5 km contrast +.06744; directional P=.00800; global maxT FWER=.05479; effort/source alternatives null | near-threshold contemporary settlement/provenance hypothesis; not anthropogenic origin or horticultural provenance |

## Dependency and anti-circularity

1. **Phenotype first.** The white/pigmented boundary and conditional intensity are constructed without geography, environment, Bombus or human variables.
2. **Broad geography next.** The national analysis establishes measured environmental associations and unresolved continuous geography before biotic or human interpretation.
3. **Scale changes for the pollinator hypothesis.** Bombus SDMs are independent of flower colour. Species-specific occurrence-reference calibration is colour-blind, and local transition pairs are selected without Bombus values, environmental values or eventual sign.
4. **Environmental balance does not select pairs.** Final-eight-axis distance is calculated only after the Bombus pair set is fixed; it cannot create the signed contrast.
5. **Natural departures precede human context.** The local event uses only flower state, geographic neighbourhood and finalized abiotic similarity. The same detector is replayed on 10,000 natural maps before any human feature is examined.
6. **Human variables enter last.** All human-context features are assessed in one global maxT family after candidate identities are fixed.

The paper therefore does not claim to partition independent causal contributions of climate, population history, pollinators and humans. It asks increasingly resolved ecological questions at the spatial scale where each is interpretable.

## Current implementation

### YAMAP / phenotype

- `Data_S1.csv`
- `Code_S1.py`
- `source_build/build_data_s1.py`
- `source_build/extract_color.py`
- `.github/workflows/yamap-public-database-benchmark.yml`
- `.github/workflows/yamap-public-database-overlap-audit.yml`
- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md`
- `submission/jbi/supporting/Appendix_S2_image_phenotyping.md`

### Broad environment + space

- `scripts/run_environment_interaction_inla_screen.R`
- `.github/workflows/environment-interaction-inla-screen.yml`
- `scripts/run_broad_environment_spatial_audit.R`
- `analysis_sensitivity/run_broad_environment_spatial_audit_wrapper.R`
- `.github/workflows/broad-environment-spatial-audit.yml`
- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `reproducibility/broad_environment_spatial_final_fixed_effects_2026-08-11.csv`
- `reproducibility/broad_environment_spatial_final_hyperparameters_2026-08-11.csv`
- `reproducibility/broad_environment_variable_evidence_registry_2026-08-11.csv`
- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`

### Local focal Bombus and guardrails

- `config/bombus_sdm.yml`
- `source_build/build_bombus_sdm_mainland.R`
- `scripts/build_bombus_occurrence_reference_support.R`
- `.github/workflows/bombus-occurrence-reference-support.yml`
- `scripts/run_bombus_local_sharp_transition.R`
- `.github/workflows/bombus-local-sharp-transition.yml`
- `analysis_sensitivity/audit_bombus_final8_environment_distance.R`
- `.github/workflows/bombus-final8-environment-audit.yml`
- `scripts/run_bombus_spatial_replication_test.R`
- `.github/workflows/bombus-spatial-replication-test.yml`
- `submission/jbi/supporting/Appendix_S4_bombus_sdm_occurrence_support.md`
- `submission/jbi/supporting/Appendix_S5_local_pollinator_robustness.md`

### Local departures and contemporary context

- `R/natural_predictive_model.R`
- `R/candidate_null_tools.R`
- `R/local_pigmented_isolates.R`
- `R/local_human_context.R`
- `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`
- `.github/workflows/human-context-highrep-final.yml`
- `reproducibility/current_broad_human_primary_2026-08-12.md`
- `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`

### Integrated validation

- `.github/workflows/final-paper-analysis.yml`
- `reproducibility/final_integrated_pipeline_2026-08-12.md`
- `FINAL_PIPELINE_SUPP_MANUSCRIPT_AUDIT_2026-08-12.md`

## Frozen evidence identities

### Broad/current cells
- run `31258851297`
- artifact `9022276431`
- SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`

### Bombus source build
- run `31249841493`
- artifact `9020226937`
- SHA-256 `d5d639e8e00d1ccc2f887c53fa8041465905b29f6bca1127f816e8c7a649d708`

### Occurrence-referenced focal support
- run `31262211605`
- artifact `9023137743`
- SHA-256 `d04c561b09b652591b9b479f6e26a779bb562c7c1b5f9b14e61d5e7ca8e2794b`

### Local sharp-transition test
- run `31263324505`
- artifact `9023416810`
- SHA-256 `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`

### Final-eight-axis Bombus balance audit
- run `31538548679`
- artifact `9119773035`
- SHA-256 `51fc846d2f3d815d8bbf3c0b95647eabeb0acf731f1e6dd164c11a0dfe1b896f`

### Bombus community/elevation guardrails
- run `31285234317`
- artifact `9029595037`
- SHA-256 `067dd3408b2a7f046ba263732ffa4cefa2f54a7f1fb672478be46bca425f6bf0`

### Local-departure/human replay
- run `31537102360`
- artifact `9119306089`
- SHA-256 `f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4`
- predictive-draw artifact `9094339466`
- SHA-256 `413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`

### YAMAP public-source benchmark
- retrieval run/artifact `31289927019` / `9031041034`
- retrieval SHA-256 `3e53669395cfd926a0942b3488f844720dca2cb97b9ea210627262691e69f31a`
- provider-overlap audit run/artifact `31290095532` / `9031085975`

## Ecological synthesis and next resolution

The paper's biological synthesis is a **spatial mosaic of processes acting on different components and scales of one polymorphism**. Pigmentation state and conditional intensity are not ecologically interchangeable. Broad geography identifies environmental contrasts for common-garden, reciprocal-transplant, physiological and fitness tests. Residual continuous geography identifies regions for ancestry, isolation-by-distance and admixture analyses. The focal-Bombus layer identifies local boundaries for direct visitation, stigma-contact, pollen-deposition and reproductive-success measurements. The 16 local departures identify populations for field provenance, planting-history and genomic-assignment work.

Macroecological pattern is therefore used to locate the next mechanistic tests, not to substitute for them.
