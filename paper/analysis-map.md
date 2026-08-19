# Current analysis map

The JBI paper is frozen to the scientific hierarchy in `reproducibility/FINAL_RESULTS_2026-08-19.md`.

| Layer | Question | Comparison unit | Final evidence |
|---|---|---|---|
| Trait construction | Can hiking photographs recover quantitative intraspecific colour geography? | 1,922 observations | pigmentation state + pigmented-only intensity |
| Broad full model | Which named environmental gradients remain after continuous space? | observations | state Temperature PC1; conditional precipitation, seasonality, terrain and interaction; response-specific residual ranges |
| Supported-term fixed null | Do final-model-supported environmental distances organize extra held-out divergence? | fixed pairs in 25 fold-distance strata | state excess +0.052133, P=0.00998; intensity P=0.26347 |
| Local Bombus | Does focal support change across fixed local colour boundaries? | 67 pairs | heterogeneous mean +0.03590; median -0.00277; 49.3% positive; q=0.08148 |
| Continuous human context | Does same-colour isolation track population exposure beyond natural geography and sampling density? | all 1,305 cells | pigmented raw P=0.000200; relative P=0.000900 |
| Event calibration | Are restrictive local configurations excessive? | 16 cells | count/fraction null; Supporting Information only |

## Canonical execution

- `python run_pipeline.py audit` — fast manuscript/repository contract validation.
- `python run_pipeline.py reproduce` — checksum-locked accepted-evidence reproduction and submission rebuild.
- Shared reusable R functions live under `R/`; analysis entry scripts live under `analysis_sensitivity/`; submission builders/renderers live under `scripts/`.

## Accepted artifact locks

- Broad baseline: artifact `9022276431`; SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.
- Seeded Bombus SDMs: artifact `9020226937`; SHA-256 `d5d639e8e00d1ccc2f887c53fa8041465905b29f6bca1127f816e8c7a649d708`.
- Final-eight predictive draws: artifact `9094339466`; SHA-256 `413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`.
- Local Bombus boundary result: artifact `9023416810`; SHA-256 `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`.
- Bombus final-eight environment audit: artifact `9119773035`; SHA-256 `51fc846d2f3d815d8bbf3c0b95647eabeb0acf731f1e6dd164c11a0dfe1b896f`.
- Supplementary event/human result: artifact `9119306089`; SHA-256 `f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4`.
- Accepted figure bundle provenance: artifact `9291438085`; SHA-256 `51dde9026c4348205c494db3594414c0f099166f8878dc1c82edbb173f9e2848`.
- Continuous colour isolation: artifact `9317087893`; SHA-256 `6fd26d9a938b68d3f0c56512cd1620597c740d44ba91ab5a7ccbb9daa99d5386`.
- Supported environmental-term distance: artifact `9350975649`; SHA-256 `4d5a1d28b8313cc0fb6c85484d21c6d94535ac7cc0881e83dc7ed02678854f03`.

The supported-term and continuous-isolation analyses are model-informed corroborations. They do not convert spatial association into adaptation or provenance.
