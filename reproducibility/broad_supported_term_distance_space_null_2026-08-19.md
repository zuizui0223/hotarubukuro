# Supported environmental-term distance versus spatial continuity

Date: 2026-08-19  
Status: fixed-null reaggregation complete

## Question

Do locations separated more strongly along the environmental terms supported by the final response-specific model also differ more in flower colour than continuous spatial similarity alone predicts at comparable geographical separation?

The final observation-level environment + SPDE models remain primary. This supporting check does **not** rebuild those models and does **not** coefficient-weight their predictors. It only changes the environmental distance used to organise the already fixed held-out pairs.

## What was reused

No environment or spatial model was refitted.

The analysis reuses:

- the frozen Broad 1-km cell table from artifact `9022276431`;
- the fixed response-specific held-out pairs;
- the exact 500 cross-fitted intercept + Matérn SPDE posterior-predictive maps stored by successful workflow `32118428188`;
- cached spatial-null artifact `9317764270`, digest `sha256:3062452997c0717946ddaaa938835ff3fd8bf90d7be3031b60b09c93e5cc9234`;
- the existing five geographical folds, five geographical-distance strata and seed-defined pair identities.

Thus the only new operation is recalculating environmental distance from the terms supported by the final response-specific model and reapplying the same high-versus-low-distance contrast to the fixed spatial-null maps.

## Environmental distances

### Pigmentation state

The final state model has one measured environmental term whose 95% credible interval excludes zero:

- Temperature PC1.

The environmental distance is the absolute difference in training-fold-standardised Temperature PC1.

### Conditional intensity

The final conditional-intensity model has four supported measured terms:

- precipitation PC1;
- temperature seasonality;
- topography PC1;
- Temperature PC1 × temperature-seasonality interaction.

The environmental distance is the Euclidean distance across those four terms after standardisation on the relevant training folds. The interaction is represented by the product `Temperature PC1 × temperature seasonality`, then standardised on the training fold.

No posterior coefficient is used as a distance weight. Direction remains a property of the accepted full-model coefficients; this pairwise distance test is unsigned.

## Fixed comparison

For each response and each of the five geographical folds:

1. use the already fixed held-out cell pairs;
2. divide them into the existing five geographical-distance strata;
3. within each stratum, contrast phenotype divergence between the upper and lower quartiles of the response-specific environmental distance;
4. calculate the identical contrast on each of the 500 cached space-only predictive maps;
5. average across the 25 fold-by-distance strata.

## Result

| Response | Supported environmental terms | Observed high-distance − low-distance phenotype divergence | Space-only median | Excess beyond space | One-sided P |
|---|---|---:|---:|---:|---:|
| **Pigmentation state** | Temperature PC1 | **0.100608** | **0.048475** | **+0.052133** | **0.00998** |
| Conditional intensity | precipitation, temperature seasonality, topography, Temperature × seasonality | 0.047416 | 0.020897 | +0.026519 | 0.26347 |

### Pigmentation state

The excess is positive in all five geographical folds and in 19 of 25 fold-by-distance strata.

The final full model supplies the directional interpretation: its Temperature PC1 coefficient is negative, so pigmentation is more likely toward the cooler end of the measured gradient. The present comparison adds that held-out locations separated more strongly along that temperature gradient are more different in pigmentation state than the fixed spatial-continuity expectation predicts.

### Conditional intensity

The combined distance across the four supported intensity terms gives a positive observed excess, but it remains well inside the space-only posterior-predictive distribution. Only 13 of 25 strata are positive, and fold-mean excess is positive in two folds and negative in three.

The final model therefore retains directional conditional associations with moisture, temperature seasonality, terrain and the thermal interaction, but those terms do not jointly produce the stronger signature of pairwise intensity divergence beyond spatial continuity.

## Simple Broad hierarchy

The Broad result can now be stated in two steps.

1. **Final environment + SPDE model:** identifies which environmental gradients have directional partial associations after continuous spatial structure is represented.
2. **Fixed-null environmental-distance check:** asks whether distance along those supported gradients also orders held-out phenotype divergence beyond the already fitted spatial-continuity expectation.

Pigmentation state passes both steps, with Temperature PC1 supplying the ecological direction. Conditional intensity passes the coefficient step but not the stronger divergence-beyond-space step.

## Interpretation ceiling

- This is model-informed corroboration, not independent variable discovery.
- The spatial null is unresolved geography, not neutral genetic differentiation or drift.
- The positive state result does not distinguish plasticity, selection, population differentiation or omitted environmental processes.
- The result does not demonstrate local adaptation or direct anthocyanin physiology.
- Conditional intensity remains a response defined only among pigmented flowers.

## Implementation

- script: `scripts/fit_broad_supported_term_distance_space_null.R`;
- workflow: `.github/workflows/broad-supported-term-distance-space-null.yml`;
- output directory: `results/broad_supported_term_distance_space_null/`.
