# Supported environmental distance versus spatial continuity

Date: 2026-08-19
Status: fixed design; result pending checksum-locked execution

## Question

Without refitting the final environment + SPDE models, use those models only to identify the environmental terms whose 95% credible intervals exclude zero. Do held-out locations farther apart in that supported environmental space also show more phenotype divergence than continuous spatial structure predicts at comparable geographical separation?

## Supported environmental distances

### Pigmentation state

The final model supports Temperature PC1 only.

`D_env(i,j) = |TemperaturePC1_i - TemperaturePC1_j|`

### Conditional intensity

The final model supports precipitation PC1, temperature seasonality, topography PC1 and the standardized Temperature PC1 × temperature-seasonality interaction.

`D_env(i,j) = sqrt(sum_k (x_ik - x_jk)^2)`

for those four standardized supported terms. Every dimension receives weight 1. Estimated coefficients are not used to weight the distance.

## Spatial-continuity comparison

For each response and each of the existing five held-out geographical folds:

1. fit only an intercept + stationary Matérn SPDE to the other four folds;
2. generate 500 posterior-predictive phenotype maps for the held-out fold;
3. form up to 15,000 held-out cell pairs;
4. divide pairs into five equal-count geographical-distance strata;
5. within each stratum, compare phenotype divergence for the upper versus lower quartile of supported environmental distance;
6. compare the observed 25-stratum mean contrast with the identical contrast from the space-only maps.

The final environment + SPDE model is not refitted. It supplies only the frozen term identities and the ecological direction reported elsewhere.

## Outputs and claim ceiling

Exactly two primary tests are produced: pigmentation state and conditional intensity.

A positive result means that divergence along the supported environmental variables exceeds the fitted continuous-spatial expectation. It does not demonstrate selection, local adaptation, plasticity, genetic differentiation, drift or direct anthocyanin physiology.

Frozen input artifact: `9022276431`  
ZIP SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`  
Posterior draws: `500`  
Seed: `20260725`
