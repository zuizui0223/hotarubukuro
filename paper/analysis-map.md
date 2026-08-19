# Current analysis map

| Layer | Question | Comparison unit | Final evidence |
|---|---|---|---|
| Trait construction | Can hiking photographs recover quantitative intraspecific colour geography? | 1,922 observations | state and conditional intensity |
| Broad full model | Which named environmental gradients remain after continuous space? | observations | state Temperature PC1; conditional precipitation, seasonality, terrain and interaction; response-specific ranges |
| Supported-term fixed null | Do supported environmental distances organize extra held-out divergence? | fixed cell pairs in 25 fold-distance strata | state excess +0.052133, P=0.00998; intensity P=0.26347 |
| Local Bombus | Does focal support change across fixed local colour boundaries? | 67 pairs | subset-driven mean +0.03590; median -0.00277; 49.3% positive; q=0.08148 |
| Continuous human context | Does same-colour isolation track population exposure beyond natural geography and sampling density? | all 1,305 cells | pigmented raw P=0.000200; relative P=0.000900 |
| Event calibration | Are restrictive local configurations excessive? | 16 cells | count/fraction null; supplementary targets |

## Active implementations

- Broad full model: `analysis_sensitivity/run_broad_environment_spatial_sensitivity.R`
- Supported-term fixed-null check: `scripts/fit_broad_supported_term_distance_space_null.R`
- Continuous isolation: `analysis_sensitivity/run_continuous_colour_isolation_human_context.R`
- Final pipeline contract: `config/paper_pipeline.lock.json`
- Final integration record: `reproducibility/final_analysis_pipeline_integration_2026-08-19.md`

The supported-term and continuous-isolation analyses are model-informed corroborations. They do not convert spatial association into adaptation or provenance.
