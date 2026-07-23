# Results directory status

## Current manuscript results

- `final_analysis_pipeline/` is the locked final handoff. It joins the v11/v16
  hierarchical national natural baseline, the v17 local Bombus-fingerprint
  pair test, the v20 local-isolate definition, and the exploratory v21-v22
  human-context characterization. Its result, claim, exclusion, checksum, and
  stage registries define which outputs are retained for the manuscript.

- `ecological_v11_pigmentation_hurdle/` is the current response-corrected
  manuscript analysis. It separates optical pigmentation presence across all
  flowers from visible colour intensity among pigmented flowers, and contains
  threshold sensitivity, Bombus evidence ladders, binomial/Gaussian SPDE-INLA,
  response-specific collinearity audits, direct H-by-R tests, and cross-fitted
  residual-tail candidate analyses.

- `ecological_v9_final_public_HRNA_50km/` is the locked base analysis. It uses
  1,923 author-confirmed flower-colour regions, standardized CIELAB a*, joint
  environmental axes, AICc-selected ENMeval surfaces, 50 km spatial folds, and
  PC-prior INLA-SPDE models. H, R, N, and A are present, but the natural SPDE
  base remains the primary ecological model.
- `ecological_v10_final_mechanism_HRNA/` is the mechanism extension. It contains
  the Bombus evidence ladder and the response-independent horticultural H-by-R
  convergence tests. Its `bombus_alpine_species_*` tables retain the separate
  beaticola, consobrinus, and honshuensis gradients instead of treating their
  combined A index as three independent replications.
- `public_rasters/mlit_human_forest_edge_2021/` contains compact MLIT-derived R
  and A products and provenance manifests.
- `ecological_v21_local_human_neighbourhood/` is the current local
  human-context analysis. It compares each fixed pigmented isolate with its
  own environment-similar observed-white neighbours and repeats the contrast
  in 1,000 natural predictive maps.
- `ecological_v22_did_human_context/` is a sensitivity analysis using 2015
  Densely Inhabited District distance. It contains the source audit,
  multiple-testing correction, candidate table, and independent validation.

Use `final_v11.Rmd`, `ANALYSIS_REVIEW_V11.md`, and
`MANUSCRIPT_STORY_V11.md` for the current confirmatory pigmentation analysis.
Use `ANALYSIS_REVIEW_V22.md` for the exploratory human-settlement continuation.
The v9 and v10 directories remain provenance inputs; their all-flower
continuous-a* inferential results are not the v11 pigmentation claim.

## Supporting frozen inputs

- `environment_v3/ecological_input_v3.csv`
- `enmeval_aicc_reselected/predictions/`
- `enmeval_aicc_reselected/ENMeval_AICc_selection.csv`
- `bombus_occurrence_phenology_cache/`

Directories named `ecological_v2_*` through `ecological_v8_*`, v5/v6 outputs,
and no-warning/block-size sensitivities are development or audit runs. They are
retained for provenance but are not the current manuscript result.
