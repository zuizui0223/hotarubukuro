# Final Broad environmental and spatial model decision

Date: 2026-08-11

## Final observation-level models

The current JBI Broad act uses two response-specific INLA-SPDE models on the same environment-complete observation set and the same stationary Matérn mesh/prior specification.

### Pigmentation state

Bernoulli-logit response, n = 1,922:

`state ~ East/West + Temperature_PC1 + precipitation_PC1 + temperature_seasonality + precipitation_seasonality + Topography_PC1 + Soil_PC1 + Soil_PC2 + RSDS + stationary_SPDE`

No interaction satisfied the full posterior + geographical-transfer promotion rule. The dryness × RSDS term remains a suggestive Supporting Information sensitivity only.

### Conditional visible intensity

Gaussian response among pigmented observations, n = 956:

`intensity ~ East/West + Temperature_PC1 + precipitation_PC1 + temperature_seasonality + precipitation_seasonality + Topography_PC1 + Soil_PC1 + Soil_PC2 + RSDS + Temperature_PC1:temperature_seasonality + stationary_SPDE`

The retained interaction has posterior mean -0.204234 and 95% CrI -0.301869 to -0.106561. The warm-climate decline in visible intensity becomes stronger as long-term temperature seasonality increases.

A narrow joint adjudication showed that the exhaustive-only `precipitation_PC1 × temperature_seasonality` signal collapses after the predeclared thermal interaction is fitted: thermal interaction -0.196437 (CrI -0.319443 to -0.075305), moisture interaction +0.015180 (CrI -0.124599 to +0.153172).

## Hydroclimate completeness

CHELSA VPD and site water balance were tested because the primary moisture PC is more supply-focused than atmospheric-demand focused.

### State

- +VPD: no WAIC gain, held-out log loss worsened, max VIF 25.91.
- +SWB: WAIC and held-out prediction worsened.
- +VPD+SWB: prediction worsened, max VIF 26.96.
- hydroclimate replacement: prediction worsened.

### Intensity

- +VPD improved WAIC by 6.79 but held-out mean squared error did not improve materially, only 2/5 folds improved, the spatial-block bootstrap interval crossed zero and max VIF was 25.81.
- +SWB worsened WAIC and held-out prediction.
- +VPD+SWB retained severe collinearity and no held-out gain.
- hydroclimate replacement worsened transfer.

Decision: retain the original precipitation/moisture PC in both models. VPD is biologically plausible but not independently identifiable enough in the sampled Japanese environmental geography to justify model expansion.

## Spatial-structure audit

The reference stationary Matérn field was compared with removal of the structural East/West factor and with coastline-barrier SPDEs.

### State

- stationary + East/West: WAIC 1577.23; held-out log loss 0.46449.
- stationary without East/West: small held-out gain 0.00035, but bootstrap 95% interval -0.00009 to +0.00086 and ΔWAIC only +0.73.
- coastline barrier + East/West: WAIC worsened by 4.33 and held-out log loss worsened by 0.00486; only 1/5 folds improved.
- barrier without East/West: likewise worse.

### Intensity with Temperature PC1 × temperature seasonality

- stationary + East/West: WAIC 2567.93; held-out MSE 0.90646.
- stationary without East/West: WAIC worsened by 5.93; held-out gain small and bootstrap crossed zero.
- coastline barrier + East/West: WAIC improved by 2.49 but held-out MSE improved only 0.00065; bootstrap crossed zero and 3/5 folds improved.
- barrier without East/West: no transferable improvement.

Decision: retain stationary Matérn + East/West for both response parts. The barrier results are a negative guardrail: documented island population history does not justify a coastline barrier for the current photographic response data.

## Final spatial hyperparameters

- state range: 132.76 km, 95% CrI 88.78–195.68; spatial SD 2.105, CrI 1.629–2.696.
- intensity range: 65.72 km, 95% CrI 31.05–132.63; spatial SD 0.357, CrI 0.236–0.501.

The range contrast is descriptive only because the two response models use different likelihoods and analysis subsets. Neither range is interpreted as a seed-, pollen- or colonization-distance estimate.

## Environmental completeness decision

Literature and public-data review classifies the current predictor basis as broadly complete for long-term national geography: warm-season thermal regime, climatic water supply, temperature/precipitation variability, terrain relief, soil resource/texture context and total shortwave radiation are represented. Major remaining gaps are classified rather than silently ignored:

- direct UV-B: biologically relevant but available global climatology is too coarse for 1-km inference;
- observation-year pre-anthesis weather: important for plasticity but belongs in a separately specified dated-weather analysis;
- deeper root-zone soil/CEC: potential refinement but highly correlated and not required by the current national screening model;
- flower-level canopy/light/hydrology: not measured by terrain or RSDS;
- variety/genetic identity: not defensibly inferable from geography and requires morphology/genomics;
- direct dispersal kernels: unavailable for the current populations.

## Species-specific spatial rationale

Allozyme evidence shows clear mainland–Izu differentiation, stronger among-population structure on islands, substantial outcrossing in mainland/Oshima populations and progressive southward island colonization. This supports retaining an explicit residual biogeographic component after measured environment, but not assigning it to one process. The coastline-barrier sensitivity was therefore biologically motivated and empirically rejected as a necessary improvement.

## Evidence provenance

- frozen additive Broad reference: workflow `31258851297`, artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`;
- mechanism-prioritized interaction screen: workflow `31435734122`, artifact `9081794678`, digest `sha256:bfd8ff0585265394522d874e1cd5b27fbd19046f1214c3443a2efbc64393f283`;
- complete 28-pair interaction audit: workflow `31437458302`, artifact `9082475952`, digest `sha256:1ed71cf1c875d2a088e5172f9be4a755547071730f96380508e5a418650bc153`;
- joint seasonality adjudication: workflow `31458262192`, artifact `9089131067`, digest `sha256:408ae23c0a15a17e4b57e6dfc9f3df585c90ae670bcef2191d8e9def96250411`;
- state hydroclimate/spatial audit: workflow `31458262192`, artifact `9089376082`, digest `sha256:d9d1b1ebba9d7916a6ab1d177ab157d9868164bee36ab70394cadeb53ad22b12`;
- intensity hydroclimate/spatial audit: workflow `31458262192`, artifact `9089411334`, digest `sha256:0fe1869ed3335f749a461e4a7a5c11ed34a2483db2e2c9c3af1b101eb47787d6`.

The separate 1-km-cell cross-fitted natural predictive reference used for the 17 local-departure candidates is unchanged by this observation-level model decision.
