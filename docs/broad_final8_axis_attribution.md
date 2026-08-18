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

The analyses also use different data units:

- full model: observation-level state (`n=1,922`) and intensity among pigmented observations (`n=956`);
- spatial-null test: held-out pairs among 1-km cells (1,305 state cells; 674 cells with conditional intensity).

## Basis correction discovered during attribution

The previously accepted P=0.03393 result is reproducible, but its environmental-basis description had drifted from its implementation.

The fitter actually calls `v16_environment_terms(50)`, which resolves to four legacy multiscale components:

- `broad50km_pc1`;
- `broad50km_pc2`;
- `within50km_pc1`;
- `within50km_pc2`.

A later wrapper and workflow wrote six climate/aridity/topography labels into metadata even though those six named columns were not used by the fitter. Therefore:

- P=0.03393 remains a valid result for the frozen **legacy four-PC composite environmental distance**;
- it must not be described as a final-eight-axis test;
- it cannot identify Temperature PC1 or another final-model axis as the source of the excess.

The wrapper, workflow and documentation now record the implemented legacy basis explicitly.

## Final-eight-axis attribution design

`fit_broad_final8_axis_space_null_attribution.R` reruns the same cross-fitted space-only design with the exact final eight measured abiotic axes used by the observation-level Broad model:

1. Temperature PC1;
2. precipitation PC1;
3. temperature seasonality;
4. precipitation seasonality;
5. topography PC1;
6. soil PC1;
7. soil PC2;
8. RSDS.

For each response it produces:

- an omnibus Euclidean distance across all eight training-fold-standardized axes;
- eight single-axis absolute-difference tests using the same pairs and posterior draws;
- raw one-sided posterior-predictive P;
- Benjamini-Hochberg q across the eight axes within a response;
- shared-draw, single-step maxT familywise P across the eight axes within a response.

The maxT result is the claim-governing axis-specific test. Single-axis tests are marginal with respect to correlations among environmental differences; combining them with the partial full-model coefficients is therefore essential.

## Executed final-eight-axis result

The checksum-locked calculation completed successfully through 10 space-only fits (two responses × five folds).

### Omnibus distance across all eight axes

| Response | Observed high-env − low-env divergence | Space-null median | Excess | One-sided P |
|---|---:|---:|---:|---:|
| Pigmentation state | 0.026205 | 0.026663 | -0.000458 | 0.51497 |
| Conditional intensity | 0.021725 | 0.024745 | -0.003019 | 0.53493 |

The final-eight-axis omnibus does not exceed spatial continuity for either response.

### Pigmentation-state axes

| Axis | Excess over null median | Raw P | BH q | maxT FWER P | Status |
|---|---:|---:|---:|---:|---|
| Temperature PC1 | **+0.052133** | **0.00998** | 0.07984 | 0.07784 | strongest candidate; familywise inconclusive |
| Temperature seasonality | +0.020641 | 0.10579 | 0.34597 | 0.46906 | not supported |
| RSDS | +0.012151 | 0.12974 | 0.34597 | 0.59681 | not supported |
| Topography PC1 | +0.000623 | 0.48703 | 0.97405 | 0.98403 | not supported |
| Precipitation PC1 | -0.007179 | 0.73852 | 0.98802 | 1.00000 | not supported |
| Soil PC1 | -0.013830 | 0.96607 | 0.98802 | 1.00000 | not supported |
| Soil PC2 | -0.019417 | 0.98802 | 0.98802 | 1.00000 | not supported |
| Precipitation seasonality | -0.020881 | 0.89820 | 0.98802 | 1.00000 | not supported |

Temperature PC1 has positive mean excess in all five geographical folds and in 19 of 25 fold-by-distance strata. Thus its raw pattern is geographically repeated rather than carried by one fold, but neither BH nor maxT crosses 0.05.

### Conditional-intensity axes

No axis is supported. The largest positive candidates are:

- Temperature PC1: excess +0.038442; raw P=0.15369; BH q=0.61477; maxT P=0.58283;
- Soil PC1: excess +0.033157; raw P=0.14371; BH q=0.61477; maxT P=0.63673.

All other raw P values are at least 0.445.

Successful workflow: `32111354890`. Artifact: `9315132730`; digest `sha256:56cb9d0da2a04f583ae97f495d6a2fd58a91602c111374a5bebf3f38925e4a1e`.

## How the two analyses combine

| Full-model coefficient | Axis-specific spatial-null excess | Interpretation |
|---|---|---|
| credibly non-zero | survives maxT | strongest triangulation: directional partial association plus held-out divergence beyond space |
| credibly non-zero | does not survive maxT | directional association is supported, but the axis is not formally shown to organize pairwise divergence beyond space |
| crosses zero | survives maxT | possible nonlinear, thresholded or context-dependent divergence not captured by one constant coefficient |
| crosses zero | does not survive maxT | no promoted axis-specific environmental result |

Current conclusions are:

1. **Pigmentation state / Temperature PC1** — strongest triangulation available, but not complete confirmation. The full model supports a cool-climate association and the held-out axis test gives a repeated raw excess, yet maxT FWER P=0.07784 remains above 0.05.
2. **Conditional intensity** — full-model coefficients describe directional geography, but none is corroborated as held-out pairwise divergence beyond spatial continuity.
3. **Environment as one eight-dimensional distance** — not supported for either response. Adding weak or irrelevant axes dilutes the focused Temperature PC1 state pattern.
4. **Legacy four-PC result** — remains a basis-specific multiscale topoclimate/radiation sensitivity, not a final-eight-axis headline.

## Manuscript-safe wording

> Pigmentation state retained a directional cool-climate association in the full environment-plus-SPDE model. In held-out pairwise tests, the exact eight-axis environmental distance did not produce excess divergence beyond spatial continuity. Temperature PC1 was the strongest named axis and showed a raw repeated excess, but eight-axis BH and maxT correction remained above 0.05. Conditional intensity showed directional full-model associations without corresponding held-out divergence beyond space.

Neither analysis proves selection, local adaptation, plasticity or direct physiological causation.
