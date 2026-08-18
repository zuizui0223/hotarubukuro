# Broad full model versus environmental divergence beyond a spatial null

## Why two analyses are needed

The observation-level full model and the cross-fitted spatial-null comparison do not estimate the same quantity.

### 1. Observation-level environment + SPDE model

The full INLA-SPDE model asks:

> Holding the other measured axes and a continuous spatial field in the model, in which direction and by how much is one environmental axis associated with the response?

It is directional and variable specific. Its inferential outputs are posterior means, credible intervals, response-specific interactions, spatial hyperparameters and geographically blocked predictive performance.

The current final models are:

- pigmentation state: Bernoulli-logit model for 1,922 observations with East/West, the final eight measured abiotic axes and a stationary Matérn SPDE field;
- conditional visible intensity: Gaussian model for the 956 pigmented observations with the same terms, the retained Temperature PC1 × temperature-seasonality interaction and a stationary Matérn SPDE field.

Posterior intervals excluding zero among the final measured environmental terms are:

| Response | Term | Posterior mean | 95% credible interval | Directional reading |
|---|---|---:|---:|---|
| Pigmentation state | Temperature PC1 | -0.54185 | -1.03294 to -0.04859 | pigmentation is less likely toward warmer warm-season climate |
| Conditional intensity | Precipitation PC1 | -0.17412 | -0.32287 to -0.02443 | already-pigmented flowers are less intense toward wetter/moister climate |
| Conditional intensity | Temperature seasonality | +0.20741 | +0.04368 to +0.36863 | the main effect at mean Temperature PC1 is positive, but must be read with the interaction |
| Conditional intensity | Topography PC1 | -0.13373 | -0.22436 to -0.04290 | intensity is lower toward steeper, greater-relief terrain |
| Conditional intensity | Temperature PC1 × temperature seasonality | -0.20423 | -0.30187 to -0.10656 | the warm-climate slope becomes more negative as temperature seasonality increases |

The structural East/West adjustment for conditional intensity also has a credible interval narrowly excluding zero, but it is retained as geographical adjustment rather than interpreted as an environmental mechanism.

These coefficients do not establish adaptation, developmental plasticity or direct anthocyanin physiology. They identify directional, partial geographical associations after measured environment and continuous space are represented.

### 2. Cross-fitted space-only divergence test

The spatial-null analysis asks:

> Among pairs separated by comparable geographical distances, are pairs that differ more environmentally also more phenotypically different than an intercept + Matérn SPDE model predicts?

It is a divergence-of-pairs test. Environmental distance and phenotype divergence are absolute, so the test is unsigned. A positive result says that environmental separation orders phenotypic separation beyond fitted spatial continuity; it does not say whether warmer, wetter or more rugged locations have the higher trait value.

The previously accepted calculation reported:

| Response | Observed high-environment minus low-environment divergence | Spatial-null median | Excess | One-sided posterior-predictive P |
|---|---:|---:|---:|---:|
| Pigmentation state | 0.106802 | 0.058240 | +0.048562 | 0.03393 |
| Conditional intensity | -0.047179 | -0.001287 | -0.045891 | 0.87226 |

## Basis correction discovered during attribution

The accepted numerical result above is reproducible, but its environmental-basis description had drifted from its implementation.

The fitter actually calls `v16_environment_terms(50)`, which resolves to four legacy multiscale components:

- `broad50km_pc1`;
- `broad50km_pc2`;
- `within50km_pc1`;
- `within50km_pc2`.

A later wrapper and workflow wrote six climate/aridity/topography labels into metadata even though those six named columns were not the variables used by the fitter. Therefore:

- P=0.03393 remains a valid result for the frozen **legacy four-PC composite environmental distance**;
- it must not be described as a final-eight-axis test;
- it cannot identify Temperature PC1 or any other final-model axis as the source of the excess.

This is a provenance/interpretation defect, not evidence that the stored numerical calculation changed.

## New final-eight-axis attribution

`fit_broad_final8_axis_space_null_attribution.R` reruns the same cross-fitted space-only design with the exact final eight measured abiotic axes used by the observation-level Broad model:

1. Temperature PC1;
2. precipitation PC1;
3. temperature seasonality;
4. precipitation seasonality;
5. topography PC1;
6. soil PC1;
7. soil PC2;
8. RSDS.

For each response it produces two levels of inference.

### Omnibus final-eight-axis distance

Euclidean distance across all eight training-fold-standardized axes tests whether final-model environmental separation as a whole orders phenotype divergence beyond space.

### Axis-specific attribution

For each axis separately, the same held-out pairs, geographical-distance strata and shared posterior-predictive draws test whether large absolute difference on that axis corresponds to excess phenotype divergence.

The output reports:

- raw directional posterior-predictive P;
- Benjamini-Hochberg q across the eight axes within a response;
- shared-draw, single-step maxT familywise P across the eight axes within a response.

The maxT result is the claim-governing axis-specific test. Raw P values remain diagnostic and cannot be used to select a causal axis after inspection.

## How the two analyses should be combined

A defensible environmental claim needs both pieces, but they perform different jobs:

1. **Full model:** direction and partial association of each axis.
2. **Final-eight-axis spatial-null test:** whether environmental separation, jointly or on a named axis, orders held-out phenotype divergence beyond spatial continuity.

Possible combinations are interpreted as follows:

| Full-model coefficient | Axis-specific spatial-null excess | Interpretation |
|---|---|---|
| credibly non-zero | survives maxT | strongest triangulation: directional partial association plus held-out divergence beyond space |
| credibly non-zero | does not survive maxT | directional association is supported, but the axis is not shown to organize pairwise divergence beyond space |
| crosses zero | survives maxT | the axis may organize nonlinear, thresholded or context-dependent divergence not captured by one constant coefficient |
| crosses zero | does not survive maxT | no promoted axis-specific environmental result |

Neither combination alone proves selection, local adaptation or direct physiological causation.

## Current execution

The final-eight-axis omnibus and eight-axis maxT calculation is executed by `.github/workflows/broad-final8-axis-space-null.yml` from the checksum-locked Broad cell table, using five held-out geographical folds, five geographical-distance strata per fold, 500 posterior-predictive realizations and seed 20260725.
