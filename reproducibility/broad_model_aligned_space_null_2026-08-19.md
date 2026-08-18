# Model-aligned Broad environmental score versus spatial continuity

Date: 2026-08-19
Status: fixed design; result pending checksum-locked execution

## Question

For each final response-specific environment + SPDE model, do held-out locations that differ more in the model-supported environmental fixed-effect component also differ more phenotypically than an intercept + continuous Matérn SPDE predicts at comparable geographical separation?

This is a supporting corroboration of the final observation-level model. It replaces neither that model nor the accepted main Broad analysis.

## Frozen evidence

- Broad artifact ID: `9022276431`
- Artifact ZIP SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`
- Observation table: `results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv`
- Cell table: `results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv`
- Geographical folds: the existing five response-blind folds
- Held-out pairs: maximum 15,000 per response and fold
- Geographical strata: five equal-count bins per fold
- Space-only null: intercept + stationary Matérn SPDE
- Posterior-predictive realizations: 500
- Seed: `20260725`
- Test: predeclared one-sided upper-tail posterior-predictive probability

## Final observation-level models retained

Both final models retain the eight response-blind environmental axes, East/West adjustment and stationary Matérn field. Conditional intensity additionally retains the standardized Temperature PC1 × temperature-seasonality interaction.

The full model remains the source of directional partial coefficients. The present test uses only its environmental fixed-effect component as a response-specific ordering score.

## Fixed score definitions

### Pigmentation state

`environment score = beta_temperature * Temperature PC1`

Temperature PC1 is the only final environmental term whose 95% credible interval excludes zero.

### Conditional intensity

`environment score = beta_temperature * Temperature PC1 + beta_precipitation * precipitation PC1 + beta_seasonality * temperature seasonality + beta_topography * topography PC1 + beta_interaction * standardized(Temperature PC1 × temperature seasonality)`

Precipitation PC1, temperature seasonality, topography PC1 and the interaction have 95% credible intervals excluding zero. Temperature PC1 is retained by the hierarchy principle because it participates in the interaction.

No other term is added to either score and no term is removed after seeing the result.

## Cross-fitting and estimand

For each response and held-out geographical fold:

1. fit the exact final observation-level environment + SPDE model on the other four folds;
2. use the training-fold posterior mean fixed coefficients to score held-out 1-km cells;
3. calculate absolute score difference and observed phenotype divergence for held-out cell pairs;
4. within each geographical-distance stratum, contrast the upper and lower score-distance quartiles;
5. calculate the identical contrast on 500 predictive maps from a separately fitted space-only SPDE;
6. average across the 25 fold-by-distance strata.

The primary estimand is:

`observed high-score-distance minus low-score-distance phenotype divergence - median of the same contrast under the space-only posterior-predictive distribution`.

Exactly two primary tests are produced: pigmentation state and conditional intensity.

## Interpretation rules

- Positive excess: the response-specific fitted environmental surface orders held-out phenotype divergence beyond fitted spatial continuity.
- Null result: the full model can retain directional partial coefficients without producing this stronger pairwise-divergence signature.
- The test is unsigned; ecological direction comes from the final full-model coefficients.
- Because score terms were selected from the final full-data model, this is model-aligned corroboration rather than independent validation.
- No outcome demonstrates selection, local adaptation, plasticity, genetic differentiation or direct anthocyanin physiology.

## Implementation

- Script: `scripts/fit_broad_model_aligned_space_null.R`
- Workflow: `.github/workflows/broad-model-aligned-space-null.yml`
- Output directory: `results/broad_model_aligned_space_null/`
