# Appendix S3. Broad environmental and spatial flower-colour model

## Purpose and model boundary

This Appendix documents the broad geographical reference used by Main 1 and, through its cross-fitted predictive maps, by the local-departure analysis in Main 3. The broad stage contains **environment and continuous space only**. Bombus SDMs are not fitted as national independent predictors, and human variables do not enter this stage.

Two related but distinct model layers are retained:

1. **observation-level INLA-SPDE models**, used to summarize environmental associations and residual spatial range for pigmentation state and conditional visible intensity; and

2. **cell-level cross-fitted predictive reference models**, used to evaluate transfer to withheld geography and to generate repeated natural maps at the observed 1-km sampling cells.

The spatial field represents unresolved geography after the measured environmental terms. It can absorb unmeasured environment, dispersal/population history and other spatially structured processes; it is not interpreted as a single historical mechanism. Likewise, fixed environmental associations are geographical covariation after conditioning on the fitted spatial field, not proof of abiotic causation.

## Analysis population and responses

The current source reconstruction began with 1,965 author-screened source records. One later exact-image hash copy was removed under stable source-row ordering, 40 records lacked the derived topographic composite and two lacked the required soil variables. The final environment-complete observation-level analysis therefore contained:

- 1,922 flower photographs;

- 966 white-like observations;

- 956 pigmented observations;

- 1,305 unique 1-km cells;

- 674 cells with at least one observed pigmented flower and therefore a finite cell-level conditional-intensity response.

The two responses were:

- **pigmentation state:** Bernoulli white-like/pigmented classification for all 1,922 observations;

- **conditional visible intensity:** standardized `max(a* - 4.968780, 0)` among the 956 pigmented observations only.

White-like photographs do not receive a conditional-intensity value.

## Environmental source and response-blind compression

Climate variables were extracted from the frozen public raster registry, soil variables from SoilGrids, and elevation from the frozen WorldClim-derived elevation layer; slope, terrain ruggedness index (TRI) and roughness were derived from elevation. The complete source manifest records 18 raster inputs and their checksums.

Environmental compression preceded the flower-colour models. Component signs are arbitrary but fixed by the frozen fitted rotations. All resulting model terms were standardized over the analysis population.

## A priori ecological hypotheses and predictor roles

The environmental terms were chosen to represent distinct biological pathways rather than to ask whether an undifferentiated environment block was “significant.”

1. **Thermal regulation.** Temperature PC1 represents warm-season maximum/mean conditions and growing-degree accumulation, while BIO4 represents annual thermal variability. Because petal anthocyanin accumulation and pathway-gene expression can be reduced by sustained high temperature, the directional prediction was lower pigmentation probability and lower pigmented-only intensity toward the warmer end of Temperature PC1 (Lu et al., 2009; Naing et al., 2018; Zhou et al., 2021).

2. **Water stress and seasonality.** Precipitation PC1 increases toward wetter/moister conditions and BIO15 represents precipitation variability. A stress-allocation hypothesis predicted stronger pigmentation toward drier or more seasonal conditions, while recognizing that water limitation can also constrain carbon gain and floral investment.

3. **Irradiance.** RSDS represents incident shortwave radiation. If visible anthocyanin contributes to light/oxidative-stress buffering, higher radiation should favour pigmentation or greater intensity. The analysis does not assume that visible anthocyanins are the only floral UV protectants (Mori et al., 2005; Koski & Ashman, 2015).

4. **Terrain and soil context.** Topography PC1 combines slope, roughness and TRI rather than elevation. It represents terrain-mediated exposure, microclimatic heterogeneity and potential habitat/population isolation. Soil PC1 and PC2 represent nutrient/organic-matter–bulk-density/pH and silt–sand texture gradients. Because these are composite resource-context axes, no universal directional sign was imposed.

5. **Residual regional organization.** The SPDE field tests whether coherent geography remains after the measured abiotic terms. If threshold-like expression state is more strongly organized by lineage, dispersal or population structure than environmentally labile variation among pigmented flowers, the pigmentation-state field should have a longer residual range than the conditional-intensity field. This is a scale prediction, not a claim that the spatial field uniquely measures genetic history.

**Table S3.1. Observation-level environmental terms.** Loadings are from the frozen response-blind PCA objects.

| Model term | Source variables and frozen PC1 loadings | Interpretation of increasing score |
|---|---|---|
| Temperature PC1 | BIO5 0.574; BIO10 0.583; GDD5 0.575 | warmer maximum/growing-season conditions and greater growing-degree accumulation |
| Precipitation PC1 | climatic moisture index 0.593; BIO12 0.594; BIO14 0.544 | wetter/moister conditions across annual and dry-period summaries |
| Temperature seasonality | standardized BIO4 | greater annual temperature variability |
| Precipitation seasonality | standardized BIO15 | greater annual precipitation variability |
| Topography PC1 | roughness 0.590; slope 0.571; TRI 0.571 | steeper, rougher and more rugged terrain |
| Soil PC1 | bulk density -0.456; coarse fragments 0.154; sand 0.007; silt 0.024; nitrogen 0.419; organic carbon density 0.439; soil organic carbon 0.486; pH -0.404 | joint soil gradient with higher organic/nutrient values and lower bulk density/pH at its positive end |
| Soil PC2 | bulk density 0.107; coarse fragments -0.276; sand -0.603; silt 0.711; nitrogen 0.167; organic carbon density -0.075; soil organic carbon 0.096; pH 0.013 | mainly a silt-positive, sand-negative texture gradient |
| RSDS | standardized surface downwelling shortwave radiation | higher incident shortwave radiation |

A structural East/West factor was also retained. The frozen implementation assigned observations at longitude >=136.5 degrees E to `East` and the remainder to `West`. This factor is a broad geographical adjustment, not an ecological mechanism.

## Observation-level INLA-SPDE specification

Pigmentation state used a binomial likelihood with logit link. Conditional intensity used a Gaussian likelihood. Each model included the East/West factor, all eight standardized environmental terms and a Matérn spatial field.

Coordinates were transformed to a Japan-centred Lambert azimuthal equal-area projection and expressed in kilometres. For each response subset, the mesh was created from observation coordinates with:

- inner/outer maximum triangle edge lengths: 20/100 km;

- minimum point separation (`cutoff`): 5 km;

- Matérn smoothness parameterization: alpha=2;

- PC prior for range: `P(range < 100 km)=0.05`;

- PC prior for spatial standard deviation: `P(sigma > 1)=0.05`.

The pigmentation-state mesh contained 5,753 vertices; the pigmented-only intensity mesh contained 5,144 vertices. The frozen environment-plus-space models had no non-finite conditional predictive ordinate values.

**Table S3.2. Fixed effects for pigmentation state.** Values are posterior mean and 95% credible interval on the logit scale.

| Term | Mean | 95% CrI |
|---|---|---|
| Intercept | -0.597 | -1.943 to 0.728 |
| East versus West | -0.020 | -1.423 to 1.336 |
| Temperature PC1 | -0.542 | -1.033 to -0.049 |
| Precipitation PC1 | -0.409 | -0.848 to 0.026 |
| Temperature seasonality | 0.409 | -0.071 to 0.893 |
| Precipitation seasonality | 0.158 | -0.368 to 0.675 |
| Topography PC1 | -0.188 | -0.417 to 0.042 |
| Soil PC1 | -0.181 | -0.630 to 0.270 |
| Soil PC2 | 0.120 | -0.103 to 0.345 |
| RSDS | 0.004 | -0.209 to 0.218 |

The thermal prediction was supported for pigmentation state. A one-standard-deviation shift toward warmer maximum/warm-quarter conditions and greater growing-degree accumulation corresponded to posterior mean odds ratio 0.58 (95% CrI 0.36–0.95), or about 42% lower odds of a pigmented classification. The precipitation coefficient pointed toward lower pigmentation in wetter conditions and temperature seasonality pointed positive, but their intervals included zero. Soil, terrain and radiation intervals likewise included zero after conditioning on the spatial field.

**Table S3.3. Fixed effects for visible intensity conditional on pigmentation.** Values are posterior mean and 95% credible interval in standardized intensity units.

| Term | Mean | 95% CrI |
|---|---|---|
| Intercept | 0.330 | 0.035 to 0.601 |
| East versus West | -0.435 | -0.789 to -0.019 |
| Temperature PC1 | -0.319 | -0.484 to -0.156 |
| Precipitation PC1 | -0.125 | -0.282 to 0.031 |
| Temperature seasonality | 0.080 | -0.090 to 0.241 |
| Precipitation seasonality | -0.126 | -0.273 to 0.027 |
| Topography PC1 | -0.138 | -0.231 to -0.045 |
| Soil PC1 | -0.060 | -0.235 to 0.115 |
| Soil PC2 | -0.010 | -0.106 to 0.086 |
| RSDS | 0.030 | -0.045 to 0.105 |

The thermal prediction was also supported within the pigmented subset: conditional intensity declined by 0.319 standardized units per SD of Temperature PC1. Intensity additionally declined by 0.138 SD per SD of Topography PC1, meaning that already-pigmented flowers were paler toward steeper, rougher terrain after the other terms and spatial field were fitted. Because this axis excludes elevation and combines several terrain properties, the coefficient is not evidence that high-elevation flowers are paler. The East/West term also differed after conditioning on environment and the spatial field, but it remains an unresolved regional adjustment rather than a process attribution. Precipitation, seasonality, soil and RSDS intervals included zero, providing no clear independent support for the broad stress–photoprotection alternatives.

## Residual spatial structure

**Table S3.4. Spatial hyperparameters for the active observation-level models.**

| Response | Posterior mean range, km | 95% CrI, km | Posterior mean spatial SD | 95% CrI |
|---|---|---|---|---|
| Pigmentation state | 132.75 | 88.70 to 195.60 | 2.106 | 1.629 to 2.697 |
| Pigmented-only intensity | 60.89 | 31.02 to 115.78 | 0.421 | 0.303 to 0.559 |

The posterior mean pigmentation-state range was 2.18 times the conditional-intensity range (132.75 versus 60.89 km). Their 95% intervals overlapped, so this is not a formal between-response difference, but the fitted scales were consistent with the a priori prediction of broader regional organization for threshold-like state and shorter-scale variation within pigmented flowers. These are model-based correlation ranges, not dispersal distances or dated historical boundaries, and either field may contain omitted environmental structure.

For internal fit diagnostics, the active environment-plus-space models had WAIC 1,577.233 for pigmentation state and 2,573.818 for conditional intensity. These values are retained for reproducibility, not used as measures of explained variance.

## Cross-fitted 1-km-cell natural reference

The predictive reference aggregated the observations to the same 1-km cells used by the local analyses. Its presence likelihood was binomial `n_pigmented` out of the observed `n_observations` in each cell. Conditional intensity was Gaussian and was defined only for cells with at least one observed pigmented flower.

The cell-level environmental basis deliberately represented broad context and local departure from that context. For elevation, temperature, precipitation and radiation, response-blind raster summaries produced:

- broad 50-km means, compressed to Broad PC1 and PC2; and

- cell-minus-broad-mean deviations, compressed to Within-50-km PC1 and PC2.

**Table S3.5. Frozen 50-km predictive environmental basis.**

| Axis | PC | Variance proportion | Elevation | Temperature | Precipitation | Radiation |
|---|---|---|---|---|---|---|
| Broad 50 km | PC1 | 0.462 | 0.709 | -0.701 | 0.076 | 0.009 |
| Broad 50 km | PC2 | 0.300 | 0.165 | 0.237 | 0.557 | 0.779 |
| Within 50 km | PC1 | 0.697 | -0.571 | 0.572 | -0.508 | 0.296 |
| Within 50 km | PC2 | 0.207 | 0.201 | -0.199 | 0.105 | 0.953 |

Prediction used five response-blind approximately 100-km geographical folds. Within every fold, predictor centring/scaling was learned from training cells only. The SPDE field was constrained to zero mean at the training locations. The final mosaic consisted only of predictions for cells withheld from their corresponding fit. One thousand predictive draws were retained under seed 20260725 for the main cross-fitted natural reference.

**Table S3.6. Geographically blocked predictive performance.**

| Response / metric | Value |
|---|---|
| Pigmentation state: trial-weighted image-level AUC | 0.8635 |
| Pigmentation state: cell any-pigmented AUC | 0.8580 |
| Pigmentation state: cell majority-pigmented AUC | 0.8707 |
| Pigmentation state: Brier score | 0.1504 |
| Pigmentation state: observed prevalence | 0.4974 |
| Pigmentation state: predicted prevalence | 0.5213 |
| Pigmentation state: calibration intercept | -0.1495 |
| Pigmentation state: calibration slope | 1.1505 |
| Conditional intensity: evaluated cells | 674 |
| Conditional intensity: RMSE | 0.9192 |
| Conditional intensity: MAE | 0.7147 |
| Conditional intensity: 95% predictive coverage | 0.9436 |

Fold-specific pigmentation AUC ranged from 0.795 to 0.923, and fold-specific intensity RMSE from 0.886 to 0.962. Thus predictive skill varied geographically even though the pooled blocked metrics were useful for constructing a natural reference.

## Use in the later analyses

The broad stage has two later roles:

1. it establishes why national flower-colour/Bombus map overlap would be difficult to interpret as an independent biotic mechanism, motivating the separate local transition design; and

2. its cross-fitted predictive draws provide the reference on which the identical local-departure event detector is replayed.

The predictive reference is not used to classify a residual tail as a cause. Human variables are examined only after local event identities have been fixed independently.

## Reproducibility resources

Primary current files:

- `.github/workflows/reanalysis-current-inputs.yml`;

- `scripts/run_reanalysis_current_inputs.sh`;

- `scripts/run_downstream_current_inputs.sh`;

- `scripts/report_reanalysis_current_inputs.R`;

- `R/natural_predictive_model.R`;

- `reproducibility/current_broad_anomaly_reference_2026-08-09.md`.

Frozen Main 1/Main 3 reference before the current clean re-execution:

- workflow run: `31258851297`;

- artifact: `9022276431`;

- artifact SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

The broad-model claim is specific but limited: warmer maximum/growing-season climates were associated with both lower pigmentation odds and lower intensity among pigmented flowers; rugged terrain was associated with lower intensity; other moisture, seasonality, soil and radiation terms lacked clear conditional effects; and broader residual geography remained for state than for intensity. These patterns motivate thermal-expression and regional-history follow-up, but the model is not a variance partition, causal environmental decomposition, pollinator-selection estimate or population-history reconstruction.
