# Environmental interaction INLA-SPDE screen specification

Date: 2026-08-11

## Purpose

The current JBI broad analysis uses two response-specific additive INLA-SPDE models:

1. Bernoulli pigmentation state for all 1,922 observations; and
2. Gaussian visible intensity conditional on the 956 observations classified as pigmented.

This sensitivity asks whether biologically motivated combinations of environmental stressors or buffering contexts improve the additive model. It does **not** replace the frozen manuscript model merely because an interaction coefficient or in-sample information criterion is favourable.

The frozen comparison input is workflow `31258851297`, artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

## Additive reference

Both responses retain the same fixed-effect structure used in the current observation-level broad analysis:

- East/West structural adjustment;
- Temperature PC1;
- Precipitation/moisture PC1;
- temperature seasonality;
- precipitation seasonality;
- Topography PC1;
- Soil PC1 and Soil PC2;
- surface downwelling shortwave radiation (RSDS);
- one Matérn SPDE field with the frozen mesh and PC-prior specification.

`Dryness` is defined only for interaction interpretation as `-Precipitation PC1`; the additive main effect remains the frozen precipitation PC.

## Complete predeclared mechanism set

This set contains interactions with a distinct ecological mechanism rather than generic “environment modifies environment” reasoning.

### Climate-stress co-exposure

1. **Temperature × radiation** — light can induce floral anthocyanin whereas warm conditions can suppress it; tests whether the radiation association changes across the thermal axis.
2. **Temperature × dryness** — temperature and water limitation jointly determine developmental and oxidative stress, although heat can also suppress pigment biosynthesis.
3. **Dryness × radiation** — combined water limitation and solar load can increase radiative, thermal and oxidative stress and the possible benefit of flavonoid investment.

### Stress magnitude × variability

4. **Temperature × temperature seasonality** — the meaning of average warm-season conditions may change where annual thermal variability alters exposure to cold or heat extremes.
5. **Dryness × precipitation seasonality** — low mean moisture may have a stronger biological effect where precipitation is more intermittent.

### Terrain-mediated context

6. **Temperature × Topography PC1** — slope and local relief can generate microclimatic heterogeneity within a broad thermal regime.
7. **Dryness × Topography PC1** — slope and relief can alter drainage, exposure and refugial moisture.
8. **Radiation × Topography PC1** — terrain relief can modify local exposure and heat load relative to broad RSDS.

Topography PC1 contains slope, TRI and roughness. It does not directly measure aspect, canopy, hydrology or flower-level illumination.

### Substrate buffering of climatic dryness

9. **Dryness × Soil PC1** — the organic/nutrient/bulk-density/pH composite may buffer or constrain physiological responses to climatic dryness; direction is not treated as universal because the axis is composite.
10. **Dryness × Soil PC2** — the silt-positive, sand-negative texture axis can modify water retention and drainage, so the effect of climatic dryness may depend on substrate texture.

## Literature basis

The interaction families are motivated by direct evidence that floral anthocyanin accumulation or pathway expression responds to temperature, light/UV and drought, and by established ecological roles of terrain and soil in modifying microclimate and water availability. Key examples include:

- Shvarts, Borochov & Weiss (1997), *Physiologia Plantarum* 99: 67–72, DOI `10.1111/j.1399-3054.1997.tb03432.x`;
- Stiles et al. (2007), *Physiologia Plantarum* 129: 756–765, DOI `10.1111/j.1399-3054.2007.00855.x`;
- Lu et al. (2009), *Molecular Ecology* 18: 3857–3871, DOI `10.1111/j.1365-294X.2009.04288.x`;
- Hennayake et al. (2006), *Environment Control in Biology* 44: 103–110, DOI `10.2525/ecb.44.103`;
- Zhang et al. (2023), *Physiologia Plantarum* 175: e13859, DOI `10.1111/ppl.13859`;
- Zhou et al. (2025), *Plant, Cell & Environment* 48: 3750–3765, DOI `10.1111/pce.15390`.

These sources justify testing interactions; they do not establish a universal sign across taxa. RSDS is total shortwave radiation rather than UV-B, and the present response is display-referred CIELAB colour rather than pigment chemistry.

## Mechanism model set

For each response, the primary mechanism screen fits:

- the additive reference;
- ten single-interaction extensions;
- four mechanism bundles: climate stress, stress variability, terrain context and substrate buffering;
- one global model containing all ten interactions.

This gives 16 models per response.

## Complete pairwise search-space audit

A second, explicitly exploratory layer adds every one of the `choose(8, 2) = 28` pairwise products among the eight current environmental axes to the additive model one at a time. This layer exists because a finite ten-interaction mechanism set can still omit a combination that improves geographic transfer.

The complete pairwise grid is not granted the same biological status as the ten mechanism hypotheses. It serves three purposes:

1. document improvement or deterioration for every possible two-axis product;
2. reveal whether a mechanism-prioritized interaction is genuinely competitive within the full search space;
3. detect omitted combinations that require a new ecological explanation rather than silently selecting them after seeing the result.

For each response the exhaustive audit fits the additive reference plus 28 one-interaction extensions, giving 29 models. No all-28 model is fitted because that would combine a large, highly correlated interaction basis without a hierarchical shrinkage prior and would answer a different question.

BH adjustment is applied across all 28 exhaustive single-interaction coefficient screens within each response. Predictive comparisons use the same five folds and spatial-block bootstrap as the mechanism screen. An exhaustive-grid winner is treated as hypothesis-generating unless its mechanism is independently defensible and its improvement also survives the stricter joint decision rule.

## Comparison criteria

Every candidate model is assessed with:

1. full-data WAIC, DIC and conditional predictive ordinate log score;
2. interaction posterior mean, 95% credible interval and posterior sign probability;
3. BH adjustment within the declared coefficient family—ten tests for the mechanism screen or 28 for the exhaustive audit;
4. the same five response-blind geographical folds used by the current analysis;
5. paired held-out loss relative to the additive model;
6. a spatial-block cluster bootstrap of the paired held-out loss difference;
7. fold consistency;
8. fixed-effect VIF, base-coefficient stability and SPDE hyperparameter stability;
9. observed support in all four low/high combinations of the interacting variables.

Primary held-out loss is log loss for pigmentation state and squared error for conditional intensity. A model cannot be promoted by WAIC or coefficient concentration alone.

## Decision rule

A single interaction receives strong support only when all of the following hold:

- its 95% posterior interval excludes zero and its within-family BH screening value is below 0.05;
- geographically blocked predictive loss improves relative to the additive model;
- the spatial-block bootstrap 95% interval for paired loss gain is above zero;
- at least four of five geographical folds improve;
- maximum fixed-effect VIF remains below 10;
- no major destabilization of the additive environmental coefficients or spatial range is evident.

Mechanism bundles are judged on predictive support and stability; they are not used to claim that every included interaction is supported. A pair found only in the exhaustive audit is not moved into Main automatically.

## Claim boundary

This is a post-lock sensitivity designed to sharpen ecological interpretation. The additive model remains the manuscript reference unless an interaction satisfies the joint coefficient-plus-geographical-prediction rule and has a defensible ecological mechanism. Even then, the result describes an interacting geographical context, not direct proof of molecular anthocyanin regulation, UV protection, water stress or local microclimate.
