# Current analysis map

| Layer | Question | Comparison unit | Final evidence |
|---|---|---|---|
| Trait construction | Can hiking photographs recover quantitative intraspecific colour geography? | 1,922 observations | state and conditional intensity |
| Broad full model | Which named environmental gradients remain after continuous space? | observations | state Temperature PC1; conditional precipitation, seasonality, terrain and interaction; response-specific ranges |
| Supported-term fixed null | Do supported environmental distances organize extra held-out divergence? | fixed cell pairs in 25 fold-distance strata | state excess +0.052133, P=0.00998; intensity P=0.26347 |
| Local Bombus | Does focal support change across fixed local colour boundaries? | 67 pairs | subset-driven mean +0.03590; median -0.00277; 49.3% positive; q=0.0815 |
| Continuous human context | Does same-colour isolation track population exposure beyond natural geography and sampling density? | all 1,305 cells | pigmented raw P=0.000200; relative P=0.000900 |
| Event calibration | Are restrictive local configurations excessive? | 16 cells | count/fraction null; supplementary targets |

## Active implementations

- Broad full model: `analysis_sensitivity/run_broad_environment_spatial_sensitivity.R`
- Supported-term fixed-null check: `scripts/fit_broad_supported_term_distance_space_null.R`
- Continuous isolation: `analysis_sensitivity/run_continuous_colour_isolation_human_context.R`
- Final pipeline contract: `config/paper_pipeline.lock.json`
- Final integration record: `reproducibility/final_analysis_pipeline_integration_2026-08-19.md`

The supported-term comparison asks directly whether environmental separation orders phenotype divergence beyond fitted spatial continuity. The continuous-isolation analysis is an explicitly post hoc human-context test. Neither establishes adaptation, realized pollination or provenance.

## Accepted artifact locks

- Broad baseline: artifact `9022276431`; SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.
- Supported environmental-term distance: artifact `9350975649`; SHA-256 `4d5a1d28b8313cc0fb6c85484d21c6d94535ac7cc0881e83dc7ed02678854f03`.
- Local Bombus boundary result: artifact `9023416810`; SHA-256 `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`.
- Bombus final-eight environmental audit: artifact `9119773035`; SHA-256 `51fc846d2f3d815d8bbf3c0b95647eabeb0acf731f1e6dd164c11a0dfe1b896f`.
- Supplementary event/human-context result: artifact `9119306089`; SHA-256 `f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4`.
- Continuous colour isolation: artifact `9317087893`; SHA-256 `6fd26d9a938b68d3f0c56512cd1620597c740d44ba91ab5a7ccbb9daa99d5386`.