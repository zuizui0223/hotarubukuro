# Model-aligned Broad environmental score versus spatial continuity

Date: 2026-08-19
Status: checksum-locked execution complete

## Question

For each final response-specific environment + SPDE model, do held-out locations that differ more in the model-supported environmental fixed-effect component also differ more phenotypically than an intercept + continuous Matérn SPDE predicts at comparable geographical separation?

This is a supporting corroboration of the final observation-level model. It replaces neither that model nor the accepted main Broad analysis.

## Frozen evidence and execution

- Broad input artifact ID: `9022276431`
- Input artifact ZIP SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`
- Observation table: `results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv`
- Cell table: `results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv`
- Geographical folds: the existing five response-blind folds
- Held-out pairs: maximum 15,000 per response and fold
- Geographical strata: five equal-count bins per fold
- Space-only null: intercept + stationary Matérn SPDE
- Posterior-predictive realizations: 500
- Seed: `20260725`
- Test: predeclared one-sided upper-tail posterior-predictive probability

Successful workflow run: `32185651732`

Result artifact:

- artifact ID: `9342715733`
- artifact digest: `sha256:11f3592e9da389d4510aaedc90c8107e0d8151bfc30b00c0fd77e04d3282504e`

The workflow completed all full-model coefficient locks, 1,305 state-cell and 674 intensity-cell score checks, 10 space-only held-out fits and 500-draw output checks.

## Final observation-level model identity

Both final models retain the eight response-blind environmental axes, East/West adjustment and stationary Matérn field. Conditional intensity additionally retains the standardized Temperature PC1 × temperature-seasonality interaction.

The full-data refit reproduced the accepted directional coefficients:

| Response | Environmental term | Posterior mean | 95% credible interval |
|---|---|---:|---:|
| Pigmentation state | Temperature PC1 | -0.541850 | -1.032936 to -0.048589 |
| Conditional intensity | Temperature PC1 | -0.083741 | -0.274485 to +0.105571 |
| Conditional intensity | precipitation PC1 | -0.174117 | -0.322874 to -0.024427 |
| Conditional intensity | temperature seasonality | +0.207410 | +0.043681 to +0.368626 |
| Conditional intensity | topography PC1 | -0.133731 | -0.224362 to -0.042896 |
| Conditional intensity | Temperature PC1 × temperature seasonality | -0.204234 | -0.301869 to -0.106561 |

The full model remains the source of ecological direction. The distance test below is unsigned.

## Fixed score definitions

### Pigmentation state

`environment score = beta_temperature * Temperature PC1`

Temperature PC1 is the only final environmental term whose 95% credible interval excludes zero. Its cross-fitted coefficient remained negative in all five held-out-fold fits.

### Conditional intensity

`environment score = beta_temperature * Temperature PC1 + beta_precipitation * precipitation PC1 + beta_seasonality * temperature seasonality + beta_topography * topography PC1 + beta_interaction * standardized(Temperature PC1 × temperature seasonality)`

Precipitation PC1, temperature seasonality, topography PC1 and the interaction have full-data 95% credible intervals excluding zero. Temperature PC1 is retained by the hierarchy principle because it participates in the interaction. Every score coefficient retained the same sign in all five cross-fitted models.

No other term was added and no term was removed after seeing the result.

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

Exactly two primary tests were produced.

## Primary result

| Response | Observed high-score minus low-score divergence | Space-only median | Excess beyond space | One-sided P |
|---|---:|---:|---:|---:|
| **Pigmentation state** | **0.100608** | **0.050192** | **+0.050416** | **0.01996** |
| Conditional intensity | 0.056681 | 0.008532 | +0.048149 | 0.10579 |

### Pigmentation state

The state result is positive in every geographical fold. Nineteen of the 25 fold-by-distance strata have positive excess over their space-only median.

Because the score contains Temperature PC1 only, the result joins two non-equivalent observations:

1. the final environment + SPDE model identifies a negative Temperature PC1 coefficient, so pigmentation is more likely toward the cooler end of the measured gradient;
2. among held-out locations separated by comparable geographical distances, pairs separated more strongly along that fitted temperature response are more different in pigmentation state than the space-only model predicts.

The defensible conclusion is:

> The directional cool-climate association of pigmentation state in the final full model is corroborated by held-out pigmentation-state divergence beyond fitted spatial continuity.

### Conditional intensity

The combined fitted intensity surface gives a positive observed excess, but the one-sided posterior-predictive probability is `0.10579`; the observed statistic remains inside the space-only central 95% distribution. Seventeen of 25 strata are positive, but fold-level excess is positive in three folds and negative in two.

The defensible conclusion is:

> Conditional intensity retains directional partial associations with moisture, temperature seasonality, terrain and their thermal interaction, but the combined fitted environmental response is not confirmed as pairwise intensity divergence beyond spatial continuity.

This null does not invalidate the full-model coefficients. The full model estimates smooth directional partial associations, whereas the supporting test asks for the stronger signature that locations separated on the fitted response surface are more phenotypically divergent than a space-only predictive distribution.

## Simple Broad hierarchy

The Broad evidence can now be reported in two steps.

1. **Final environment + SPDE models:** identify which environmental gradients have directional partial associations after continuous spatial structure is represented.
2. **Model-aligned spatial-null check:** ask whether the fitted response-specific environmental component also organizes held-out phenotype divergence beyond spatial continuity.

The result is response specific:

- pigmentation state passes both steps, with Temperature PC1 providing the directional ecological interpretation;
- conditional intensity passes the coefficient step but not the stronger divergence-beyond-space step.

No additional geographic-bin grid, nearest-distance deletion series or item-by-item axis search is needed for this model-aligned conclusion.

## Interpretation boundary

- The score terms were fixed from the final full-data model, so this is model-aligned corroboration rather than an independent discovery or validation analysis.
- Absolute score distance and phenotype divergence are unsigned; ecological direction comes from the full-model coefficient signs.
- The spatial field is unresolved geography and is not neutral genetic divergence or drift.
- A positive state result does not distinguish environmental selection from plasticity or omitted processes, and does not establish local adaptation, genetic differentiation or direct anthocyanin physiology.
- The conditional-intensity response is observed only among pigmented flowers and remains a conditional geographical response.

## Implementation

- Script: `scripts/fit_broad_model_aligned_space_null.R`
- Workflow: `.github/workflows/broad-model-aligned-space-null.yml`
- Output directory: `results/broad_model_aligned_space_null/`
