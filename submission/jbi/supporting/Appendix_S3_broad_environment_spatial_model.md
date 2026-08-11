# Appendix S3. Broad environmental and spatial flower-colour model

## Purpose and inferential boundary

This Appendix documents the broad geographical component of the current paper and the model-selection audit used to decide what belongs in that component. The broad stage asks whether long-term abiotic geography and unresolved spatial structure organize two related but biologically distinct visible-colour responses in *Campanula punctata*:

1. **pigmentation state** — whether a flower lies in the white-like or visibly pigmented regime; and
2. **conditional visible intensity** — how strong the visible red–green signal is after the flower has entered the pigmented regime.

The broad stage contains environment and continuous space only. Bombus SDMs are not treated as independent national environmental predictors, and human-context variables do not enter this stage.

Two model layers must be kept separate.

- The **observation-level INLA-SPDE models** are used to interpret environmental coefficients, environmental interactions and residual spatial correlation.
- A separately specified **1-km-cell cross-fitted natural reference** is used later to generate predictive maps for the local-departure analysis.

Updating the observation-level coefficient model does not automatically change the cell-level predictive reference or the 17 fixed local-departure candidates. The latter remain tied to their separately validated cross-fitted model unless that predictive model is explicitly respecified and rebuilt.

The image response is a reproducible display-referred CIELAB phenotype. Neither pigmentation state nor conditional a* intensity is interpreted as a direct assay of anthocyanin concentration, anthocyanin identity, vacuolar pH, spectral reflectance, ultraviolet contrast, petal temperature or Bombus colour contrast.

## Analysis population and record flow

All records recovered within the predefined 2023–2025 YAMAP keyword frame were taken through author visual screening rather than subsampled. Taxonomic misidentifications and non-focal campanuloid subjects were excluded before the screened source table was frozen, the focal flower and usable petal region were confirmed, and identical-coordinate records were checked against photographs, dates and activity provenance. Coordinate identity alone was not an exclusion criterion. Exact-image duplication was audited separately by SHA-256.

The current record flow was:

- 1,965 author-screened eligible source records;
- minus one later exact-image duplicate;
- minus 40 records without complete topographic raster support;
- minus two records without complete soil support;
- **1,922 observations in the environment-complete integrated analysis**.

The final population comprised 966 white-like and 956 pigmented observations in 1,305 unique 1-km cells. Among these, 674 cells contained at least one pigmented observation and therefore had a finite cell-level conditional-intensity response.

The two observation-level responses were:

- **pigmentation state:** Bernoulli white-like/pigmented classification for all 1,922 observations;
- **conditional visible intensity:** standardized `max(a* - 4.968780, 0)` among the 956 pigmented observations only.

White-like observations do not receive a conditional-intensity value.

## Ecological hypotheses behind the broad predictor set

The predictor set was not interpreted as an undirected search for whichever raster became significant. We organized it around environmental processes that can plausibly affect anthocyanin-associated floral colour, while recognizing that a long-term geographical association can combine developmental plasticity, evolved population differentiation and historical population sorting.

### Thermal regulation

Temperature was the strongest a priori directional abiotic hypothesis. Experimental work in other flowers shows that moderate low temperature can increase corolla anthocyanin accumulation and expression of anthocyanin-pathway genes, while geographical studies of floral colour and reflectance often find stronger pigmentation or lower reflectance in cooler climates (Shvarts et al., 1997; Koski & Galloway, 2020). We therefore expected warmer growing-season conditions to reduce the probability of a visible pigmented state and/or conditional visible intensity.

For *C. punctata* specifically, geographical variation in thermal germination responses has also been documented (Inoue & Washitani, 1989). This does not provide a floral-colour mechanism, but it supports the broader premise that thermal environment has contributed to geographically differentiated life-history responses in the species.

The primary thermal axis combines maximum temperature of the warmest month, mean temperature of the warmest quarter and growing degree-days above 5 °C. It therefore describes long-term warm-season climate and thermal accumulation, not the weather experienced during formation of an individual photographed corolla.

### Temperature variability

Mean temperature need not have the same biological meaning in weakly and strongly seasonal climates. Because floral pigment expression can respond to temperature during development, recurrent annual thermal variability can modify the association between long-term warm-season climate and the realized visible phenotype. Temperature seasonality was therefore retained as a main effect and its interaction with Temperature PC1 was included in the mechanism-based interaction audit.

BIO4 is a long-term annual temperature-variability proxy. It should not be interpreted as frost frequency or direct pre-anthesis cold exposure.

### Water supply and atmospheric demand

Anthocyanin polymorphisms can be associated with drought and atmospheric water demand. Comparative floral studies have explicitly predicted greater anthocyanin pigmentation under declining precipitation or increasing vapour-pressure deficit, and drought can alter petal anthocyanin in experimental systems (Sullivan & Koski, 2021). The primary moisture axis therefore combines climatic moisture index, annual precipitation and precipitation of the driest month.

This axis primarily represents climatic water supply. It is not a direct measurement of plant water potential, soil moisture or atmospheric drying demand. To test whether that omission changed the broad result, we later added CHELSA mean vapour-pressure deficit (VPD) and site water balance (SWB) as same-resolution sensitivity variables.

### Radiation and flavonoid photoprotection

Light and ultraviolet exposure can regulate flavonoid and anthocyanin production. This is especially relevant to *C. punctata*: a UV-B exclusion experiment on coastal and inland populations showed decreasing leaf flavonoid accumulation as UV-B was excluded, although natural-population correlations were weak and micro-environmental context was important (Hashiba et al., 2006). That study concerns **leaf flavonoids**, not petal anthocyanin, so it establishes species-level flavonoid responsiveness rather than a direct flower-colour mechanism.

The present model uses CHELSA surface downwelling shortwave radiation (RSDS). RSDS is total shortwave radiation, not UV-B. A global UV-B product such as glUV is biologically closer to photoprotection, but its approximately 15-arc-minute resolution and historical period are much coarser than the approximately 1-km flower analysis. We therefore did not interpolate it to apparent 1-km precision. The absence of direct UV-B remains an explicit measurement limitation.

### Terrain relief as micro-environmental context

Topography PC1 summarizes slope, terrain ruggedness index (TRI) and roughness. Increasing values mean steeper terrain with larger local elevation relief. Elevation itself is **not** part of this PC.

Terrain relief can create heterogeneous local radiation, drainage, wind exposure, snow persistence and near-surface microclimates, but the present axis does not directly measure aspect, canopy, hydrology or flower-level illumination. We therefore treat Topography PC1 as a terrain-context variable rather than a unidirectional stress index.

Elevation was deliberately not added as another fixed effect in the process-decomposition model because it jointly proxies thermal, hydrological, radiative and regional gradients that the model attempts to distinguish. It remains useful descriptively, but adding it to the same regression would make the conditional meanings of the more mechanistic axes less clear.

### Soil resource and texture context

Soil PC1 combines topsoil organic/carbon and nitrogen status, bulk density, coarse fragments and pH; Soil PC2 is primarily a silt-positive, sand-negative texture axis. These variables can alter nutrient supply, drainage and water holding, thereby modifying environmental stress and pigment investment. They are contextual predictors without a universal directional expectation.

SoilGrids also provides CEC and deeper soil layers. Those are real measurement gaps in the current model, especially for a perennial root system, but adding multiple highly correlated depth-specific layers would not automatically make the ecological model better. We therefore use the current 0–5-cm resource/texture axes as a national screening layer and retain deeper root-zone chemistry and CEC as residual environmental uncertainty.

## Public-data completeness audit

We reviewed major public environmental domains that could plausibly influence floral anthocyanin-associated colour and classified each as primary, exact sensitivity, guardrail or unresolved measurement gap.

**Table S3.1. Environmental-process and public-data audit.**

| Process | Current proxy | Public alternative considered | Decision for final broad model |
|---|---|---|---|
| Warm-season temperature | Temperature PC1: BIO5, BIO10, GDD5 | BIO6; dated daily temperature | **Retain primary Temperature PC1.** BIO6 is a cold-season/germination guardrail rather than a direct summer-corolla variable; dated weather is a separate plasticity question |
| Annual thermal variability | BIO4 | frost-change or snow indices | **Retain BIO4** and evaluate mechanistic interaction with Temperature PC1 |
| Climatic water supply | CMI, BIO12, BIO14 PC1 | VPD, SWB, PET | **Retain moisture PC1.** VPD/SWB tested exactly as same-resolution sensitivities and did not justify expansion |
| Water intermittency | BIO15 | daily/monthly concentration indices | **Retain BIO15**; interactions evaluated in declared model screen |
| Shortwave/light environment | RSDS | UV-B climatology | **Retain RSDS with claim ceiling.** Do not upsample coarse UV-B into false local precision |
| Terrain relief | slope, TRI, roughness PC1 | aspect/northness, TPI, heat-load/TWI | **Retain relief PC** as terrain context; no direct microclimate claim |
| Soil resource/chemistry | Soil PC1 | CEC; deeper SoilGrids depths | **Retain broad PC1.** CEC/deeper root-zone properties remain unmeasured uncertainty |
| Soil texture/water retention | Soil PC2 | clay, depth-specific texture | **Retain PC2** |
| Forest/canopy context | none in broad model | forest fraction/canopy products | Supporting habitat/light guardrail only; forest fraction is not flower-level light and also relates to access/land use |
| Elevation | deliberately omitted as fixed effect | WorldClim elevation | Descriptive composite gradient, not added to the mechanistic fixed-effect set |
| Coastality | implicit in climate + space | distance to coast | Context/history guardrail only; no unique petal-anthocyanin mechanism |
| Observation-year weather | not in climatological model | daily 2023–2025 temperature/precipitation/radiation | Important future plasticity analysis; not mixed post hoc with long-term geographical inference |
| Taxonomic/lineage identity | not recorded | image-based varietal audit/genetics | Important possible regional axis; **not inferred from geography** because that would be circular |

CHELSA v2.1/BIOCLIM+ provides approximately 1-km temperature, precipitation, radiation, VPD, wind, growing-degree, climate-moisture and water-balance variables, making it suitable for this national geographical question (Brun et al., 2022). SoilGrids 2.0 provides 250-m global predictions of major soil properties and quantified uncertainty (Poggio et al., 2021). Thus the principal abiotic domains are represented, but the model should not be described as environmentally exhaustive at flower scale.

## Response-blind environmental compression

Environmental compression preceded the flower-colour models. Component signs are arbitrary but fixed by the frozen rotations. All model terms were standardized over the corresponding analysis population.

**Table S3.2. Observation-level environmental terms.**

| Model term | Source variables and frozen loadings | Interpretation of increasing score |
|---|---|---|
| Temperature PC1 | BIO5 0.574; BIO10 0.583; GDD5 0.575 | warmer maximum/growing-season conditions and greater growing-degree accumulation |
| Precipitation PC1 | climatic moisture index 0.593; BIO12 0.594; BIO14 0.544 | wetter/moister annual and dry-period conditions |
| Temperature seasonality | standardized BIO4 | greater annual temperature variability |
| Precipitation seasonality | standardized BIO15 | greater annual precipitation variability |
| Topography PC1 | roughness 0.590; slope 0.571; TRI 0.571 | steeper terrain with greater local elevation relief |
| Soil PC1 | bulk density -0.456; coarse fragments 0.154; sand 0.007; silt 0.024; nitrogen 0.419; organic-carbon density 0.439; soil organic carbon 0.486; pH -0.404 | higher organic/nutrient values and lower bulk density/pH at the positive end |
| Soil PC2 | bulk density 0.107; coarse fragments -0.276; sand -0.603; silt 0.711; nitrogen 0.167; organic-carbon density -0.075; soil organic carbon 0.096; pH 0.013 | mainly silt-positive, sand-negative texture |
| RSDS | standardized surface downwelling shortwave radiation | higher incident shortwave radiation |

A structural East/West factor was also retained, with observations at longitude >=136.5° E assigned to `East`. It is a geographical adjustment, not an ecological mechanism or an inferred genetic boundary.

## Observation-level INLA-SPDE specification

Pigmentation state used a Bernoulli likelihood with logit link. Conditional intensity used a Gaussian likelihood. Coordinates were transformed to a Japan-centred Lambert azimuthal equal-area projection and expressed in kilometres.

The reference stationary SPDE used:

- inner/outer mesh maximum edge lengths: 20/100 km;
- point cutoff: 5 km;
- Matérn alpha = 2;
- PC prior `P(range < 100 km)=0.05`;
- PC prior `P(spatial SD > 1)=0.05`.

The state mesh contained 5,753 vertices and the intensity mesh 5,144 vertices.

Model extension was deliberately conservative. A new environmental term or interaction was eligible for promotion only when it had a defensible ecological interpretation, acceptable collinearity, improved geographically blocked predictive loss, a positive spatial-block bootstrap interval and improvement in at least four of five response-blind geographical folds. WAIC or a concentrated posterior alone was insufficient. VIF was treated as a graded diagnostic rather than a universal deletion rule: values <5 were preferred; values of 5–10 required explicit stability of the focal coefficient, blocked geographic transfer and spatial hyperparameters; values >10 prevented promotion without exceptional mechanistic and predictive evidence. This final adjudication is intentionally stricter than treating the historical VIF=10 screen as a sufficient condition, while preserving the original screen specification as provenance.

## Environmental-interaction audit

We used two complementary screens.

1. A **mechanism-prioritized screen** fitted ten interactions motivated by thermal regulation, water/radiation co-stress, climatic variability, terrain context and substrate buffering.
2. An **exhaustive search-space audit** fitted all `choose(8,2)=28` pairwise products among the eight environmental axes, one at a time. This second layer was a guardrail against selectively omitting an inconvenient interaction; it did not give every pair equal biological status.

Both screens retained the same response-specific likelihood, fixed main effects, SPDE mesh/priors and five geographical folds. Interaction evidence was judged from posterior concentration, within-family BH adjustment, WAIC/CPO, held-out loss, spatial-block bootstrap gain, fold consistency, VIF and spatial-hyperparameter stability.

### Pigmentation state

No interaction satisfied the complete promotion rule. The strongest mechanistic candidate was climatic dryness × RSDS: its posterior was positive (mean 0.317; 95% CrI 0.115–0.519) and held-out log loss improved in four of five folds, but the spatial-block bootstrap interval for predictive gain crossed zero. The all-28 audit did not produce a state interaction that clearly justified replacing the additive model.

We therefore retain the **additive state model** and report dryness × radiation only as a suggestive Supporting Information co-stress sensitivity. RSDS is not UV-B, and long-term climatic dryness is not flower-level water stress.

### Conditional visible intensity

The mechanism screen identified a transferable Temperature PC1 × temperature-seasonality interaction. The interaction posterior was negative (mean -0.204; 95% CrI -0.302 to -0.107; mechanism-screen BH = 0.00043), WAIC improved by approximately 5.9 units relative to the additive intensity model, held-out squared error improved in four of five folds, and the spatial-block bootstrap interval remained above zero. Maximum model VIF was approximately 6.34, placing the model in the moderate 5–10 diagnostic band rather than the preferred <5 band. Crucially, the retained interaction itself had VIF 1.66; the larger values belonged to Temperature PC1 (6.34) and Soil PC1 (5.23), reflecting shared national geography among additive axes. Removing the East/West structural adjustment lowered the maximum VIF below 5 but worsened WAIC by about 5.9 units and did not provide robust transfer gain, so terms were not deleted solely to cross an arbitrary VIF=5 threshold.

The exhaustive 28-pair audit also identified precipitation PC1 × temperature seasonality. Because this interaction was not in the mechanism-prioritized set and shared the same seasonality axis, we fitted a narrow joint adjudication rather than narrating two independent mechanisms. In the model containing both interactions:

- Temperature PC1 × temperature seasonality remained negative: **-0.196** (95% CrI **-0.319 to -0.075**);
- precipitation PC1 × temperature seasonality collapsed to **+0.015** (95% CrI **-0.125 to +0.153**).

The exhaustive-only moisture interaction therefore did not carry independent information once the predeclared thermal interaction was present. The final intensity extension contains **Temperature PC1 × temperature seasonality only**.

The negative interaction means that the association between warmer Temperature PC1 and weaker visible intensity becomes more negative as long-term temperature seasonality increases. Equivalently, the cool-climate intensity advantage is strongest in more thermally seasonal regions. This describes an interacting geographical climate context, not direct evidence that fluctuating temperature caused anthocyanin expression in the photographed flowers.

Temperature PC1 × Topography PC1 was also attractive in the full-data fit but did not improve prediction to withheld geography, so it was rejected as an interaction extension. Cooler thermal geography and greater terrain relief therefore remain **countervailing additive associations**, not an inferred temperature-by-ruggedness mechanism.

## Hydroclimate-completeness sensitivity

VPD and SWB were the highest-priority same-resolution omissions identified by the literature/public-data audit because the primary precipitation PC represents supply more directly than atmospheric demand.

### Pigmentation state

Adding VPD did not improve the state model: WAIC was essentially unchanged/worse, held-out log loss worsened, and the maximum VIF increased to approximately **25.9**. Adding SWB, VPD+SWB, or replacing the moisture PC with a composite hydroclimate axis likewise failed to improve geographical transfer. The additive state model therefore retains the original precipitation/moisture PC.

### Conditional intensity

Adding VPD to the **additive** intensity model improved in-sample WAIC but did not improve held-out prediction and increased maximum VIF to approximately **25.8**. SWB worsened both WAIC and held-out prediction; VPD+SWB retained severe collinearity; replacing the moisture PC with a hydroclimate composite also worsened transfer.

This pattern is important biologically. VPD is a plausible floral-anthocyanin correlate in the literature, but within the Japanese sampling geography it is too strongly entangled with the existing thermal/moisture axes to supply stable independent information. A positive VPD coefficient in the expanded model is therefore not promoted as an additional water-stress mechanism.

## Final observation-level fixed effects

### Pigmentation state: final additive model

**Table S3.3. Final pigmentation-state fixed effects.** Posterior mean and 95% CrI on the logit scale.

| Term | Mean | 95% CrI |
|---|---:|---:|
| Intercept | -0.597 | -1.942 to 0.727 |
| East versus West | -0.020 | -1.422 to 1.336 |
| Temperature PC1 | **-0.542** | **-1.033 to -0.049** |
| Precipitation PC1 | -0.409 | -0.848 to 0.026 |
| Temperature seasonality | 0.409 | -0.070 to 0.893 |
| Precipitation seasonality | 0.158 | -0.368 to 0.675 |
| Topography PC1 | -0.188 | -0.417 to 0.042 |
| Soil PC1 | -0.181 | -0.630 to 0.270 |
| Soil PC2 | 0.120 | -0.103 to 0.345 |
| RSDS | 0.004 | -0.209 to 0.218 |

The clearest measured state association remains thermal: a one-SD shift toward warmer Temperature PC1 corresponds to an odds ratio of approximately `exp(-0.542)=0.58`. The precipitation coefficient is directionally consistent with greater pigmentation toward the drier end but remains uncertain after continuous space is included.

### Conditional visible intensity: final thermal-interaction model

**Table S3.4. Final conditional-intensity fixed effects.** Posterior mean and 95% CrI in standardized visible-intensity units. Main effects are conditional on the interacting variables being at their standardized reference values.

| Term | Mean | 95% CrI |
|---|---:|---:|
| Intercept | 0.257 | -0.017 to 0.513 |
| East versus West | **-0.379** | **-0.709 to -0.003** |
| Temperature PC1 | -0.084 | -0.274 to 0.106 |
| Precipitation PC1 | **-0.174** | **-0.323 to -0.024** |
| Temperature seasonality | **0.207** | **0.044 to 0.369** |
| Precipitation seasonality | -0.057 | -0.197 to 0.089 |
| Topography PC1 | **-0.134** | **-0.224 to -0.043** |
| Soil PC1 | 0.063 | -0.114 to 0.239 |
| Soil PC2 | 0.012 | -0.081 to 0.104 |
| RSDS | 0.026 | -0.048 to 0.099 |
| Temperature PC1 × temperature seasonality | **-0.204** | **-0.302 to -0.107** |

The final intensity model changes the ecological interpretation of the former additive temperature coefficient. There is not one constant national temperature slope. The warm-climate decline becomes stronger as temperature seasonality increases. Conditional intensity also remains lower toward wetter/moister Precipitation PC1 and toward steeper, greater-relief terrain after the other terms and continuous space are included.

These coefficients do **not** imply that precipitation or terrain directly controls anthocyanin concentration. They identify environmental contexts associated with the image-derived visible phenotype after spatial adjustment.

## Why the spatial field is biologically necessary

Residual spatial structure is not treated as disposable autocorrelation. *C. punctata* has a biological history capable of generating persistent regional covariance that is not reducible to present climate.

Allozyme work across seven Honshu and ten Izu populations found clear mainland-versus-island differentiation: genetic identity was high within mainland and island groups but substantially lower between them, gene diversity was more strongly partitioned among island populations, and island diversity declined with distance from the mainland (Inoue & Kawahara, 1990). Geological and genetic evidence supported older establishment in northern Izu islands followed by progressive southward dispersal. Mainland and Oshima populations were predominantly outcrossing, so pollen-mediated gene flow is also biologically important.

These results demonstrate that regional history, mating system and dispersal can structure the species independently of contemporary raster environment. They do **not** justify assigning a nationwide photographic record to a particular genetic lineage, nor do they provide a seed-dispersal kernel for the present observations.

Direct quantitative nationwide seed-dispersal distances for Japanese *C. punctata* were not located in the literature audit. Seed studies show small seeds and strong temperature/stratification dependence of germination, but germination biology is not a dispersal-distance estimate. The SPDE range must therefore not be interpreted as seed or pollen dispersal distance.

## Spatial-model audit

The reference model uses a stationary approximately isotropic Matérn field in projected kilometres. This is a conservative default because it models continuously decaying residual covariance without imposing unobserved genetic clusters. Its main biological concern in Japan is that Euclidean covariance can smooth across sea gaps.

We therefore compared:

1. stationary SPDE + East/West adjustment;
2. stationary SPDE without East/West;
3. coastline-barrier SPDE + East/West;
4. coastline-barrier SPDE without East/West.

The barrier sensitivity treats sea triangles as reduced-connectivity regions rather than absolute walls. It is motivated by the documented Izu differentiation, not by an assumption that all gene flow occurs over land.

### Pigmentation state

The stationary + East/West reference had WAIC 1577.23 and held-out log loss 0.46449. Removing East/West produced only a small WAIC change and a very small held-out improvement whose spatial-block bootstrap interval crossed zero. The coastline-barrier models worsened both WAIC and held-out prediction; the barrier + East/West model worsened held-out log loss by approximately 0.0049 and improved only one of five folds.

Thus neither removal of East/West nor coastline blocking supplied sufficiently stable predictive gain to replace the reference state spatial model.

### Conditional visible intensity

Spatial structure was compared using the retained Temperature PC1 × temperature-seasonality fixed-effect model. The stationary + East/West reference had WAIC 2567.93 and held-out mean squared error 0.90646. Removing East/West worsened WAIC by approximately 5.9 units. A coastline barrier improved full-data WAIC by approximately 2.5 units but improved held-out squared error by only 0.00065; its spatial-block bootstrap interval crossed zero and only three of five folds improved. Barrier/no-region and stationary/no-region variants likewise failed the transfer rule.

The final intensity model therefore also retains the **stationary Matérn field + East/West structural adjustment**. The barrier result is useful as a negative guardrail: documented island genetic history does not by itself justify replacing the stationary national covariance model for these observations.

Anisotropy and fully nonstationary SPDEs were not added. Although Japan is elongated and regional mating systems differ, no independent nationwide genetic or dispersal data linked to the 1,922 observations support estimating a free directional or region-specific covariance process from colour data alone. Such flexibility would be difficult to distinguish from unmeasured environment and YAMAP sampling geography.

## Final residual spatial scales

**Table S3.5. Spatial hyperparameters for the final observation-level models.**

| Response | Final fixed-effect structure | Mean range, km | 95% CrI, km | Mean spatial SD | 95% CrI |
|---|---|---:|---:|---:|---:|
| Pigmentation state | additive environmental model | 132.76 | 88.78–195.68 | 2.105 | 1.629–2.696 |
| Conditional visible intensity | additive environment + Temperature PC1 × temperature seasonality | 65.72 | 31.05–132.63 | 0.357 | 0.236–0.501 |

The posterior-mean residual range is broader for state than for conditional intensity, but the intervals overlap and the two responses use different likelihoods and analysis subsets. This is a **descriptive scale contrast**, not a formal cross-response test. It is compatible with broader regional organization of the threshold-like state and more local modulation of intensity after pigmentation is present, but it does not identify a genetic mechanism or colonization boundary.

## Ecological interpretation of the final broad model

The final Broad result is more specific than “environment and space mattered.”

For pigmentation state, the strongest measured environmental signal is a broad cool-climate association: warmer warm-season conditions reduce the probability of a visible pigmented state. A dryness × radiation co-stress pattern is biologically plausible but did not transfer robustly enough to enter the primary model.

For pigmented-only intensity, the thermal pattern is context dependent. The cool-climate intensity advantage is strongest in more thermally seasonal regions. Wetter/moister climatic geography is associated with weaker intensity in the final interaction model, while the VPD/SWB audit shows that this should not be re-labelled as an independently resolved atmospheric-demand mechanism. Terrain relief carries a separate negative association: after climatic geography and space are conditioned upon, already-pigmented flowers are less intense toward steeper terrain with greater local elevation relief.

This means that mountain geography is not one unitary “high-elevation darkening” process. High elevation commonly combines cooler climate with greater relief; in the fitted model these environmental components carry different information and can act in opposing directions for conditional intensity.

Substantial residual geography remains after these measured terms. Based on the species’ documented mainland–island differentiation, outcrossing biology and island colonization history, that residual structure is biologically credible. It may combine unmeasured environment, genetic/population history, dispersal and sampling geography, and is not assigned to any one of them.

## Remaining biological gaps

The variable audit also identifies questions that the current Broad model cannot answer and should not pretend to answer.

1. **Petal anthocyanin chemistry:** spectroscopy, pigment quantification and gene-expression assays are required to connect image a* with anthocyanin amount, identity or pathway activity.
2. **Flower-level UV/light:** total shortwave radiation is not UV-B and neither RSDS nor terrain relief measures the illumination reaching an individual corolla.
3. **Developmental weather plasticity:** dated 2023–2025 photographs make pre-anthesis temperature, VPD, precipitation and radiation anomalies testable, but that is a separate within-year plasticity question and should use an independently specified weather window.
4. **Root-zone soil:** CEC and deeper SoilGrids layers could refine resource/water-holding context but would add many correlated predictors; field soil measurements would be preferable for mechanism.
5. **Taxonomic and genomic structure:** the current photographic data do not carry defensible variety or genotype assignments. Geographic proxying of `var. punctata` versus `var. hondoensis` would be circular. Population genomic sampling is needed to determine how much residual flower-colour geography follows lineage or isolation by distance.
6. **Dispersal:** direct species-specific seed and pollen dispersal kernels linked to current populations are unavailable; spatial ranges are not dispersal distances.

## Cross-fitted 1-km-cell natural reference used by Main 3

The local-departure analysis retains its separately specified cell-level predictive reference. That model aggregates the observations to the same 1-km cells used by the local event detector. Pigmentation uses binomial counts and conditional intensity a Gaussian cell response among cells containing pigmented observations.

For elevation, temperature, precipitation and radiation, response-blind summaries produce broad 50-km means and cell-minus-broad deviations, each compressed to two PCs.

**Table S3.6. Frozen 50-km predictive environmental basis.**

| Axis | PC | Variance proportion | Elevation | Temperature | Precipitation | Radiation |
|---|---|---:|---:|---:|---:|---:|
| Broad 50 km | PC1 | 0.462 | 0.709 | -0.701 | 0.076 | 0.009 |
| Broad 50 km | PC2 | 0.300 | 0.165 | 0.237 | 0.557 | 0.779 |
| Within 50 km | PC1 | 0.697 | -0.571 | 0.572 | -0.508 | 0.296 |
| Within 50 km | PC2 | 0.207 | 0.201 | -0.199 | 0.105 | 0.953 |

Prediction uses five response-blind approximately 100-km geographical folds. Within each fold, predictor centring/scaling is learned from training cells only and the SPDE field is constrained at the training locations. One thousand predictive draws under seed 20260725 form the locked natural reference.

**Table S3.7. Geographically blocked performance of the separate cell-level natural reference.**

| Response / metric | Value |
|---|---:|
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

This predictive reference supplies the repeated natural maps used to calibrate the local event definition in Appendix S6. The new observation-level interaction and spatial sensitivities do not retroactively redefine those candidate events.

## Reproducibility and model-selection evidence

The final observation-level adjudication is based on:

- frozen broad-analysis evidence: workflow `31258851297`, artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`;
- mechanism-prioritized interaction screen: workflow `31435734122`, artifact `9081794678`;
- complete 28-pair interaction audit: workflow `31437458302`, artifact `9082475952`;
- narrow joint seasonality adjudication: workflow `31458262192`, artifact `9089131067`;
- VPD/SWB and spatial-model sensitivities: workflow `31458262192`, state artifact `9089376082` and conditional-intensity artifact `9089411334`.

The broad-model claim is limited to long-term environmental associations, one transferable temperature-by-seasonality interaction for conditional intensity, and substantial unresolved spatial structure. It is not a variance partition, causal decomposition of anthocyanin physiology, direct reconstruction of population history, dispersal estimate, pollinator-selection estimate or horticultural-provenance analysis.
