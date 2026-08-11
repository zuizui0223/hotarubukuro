# Broad environmental-variable literature and public-data audit

Date: 2026-08-11

## Scope

This audit asks whether the current eight-axis observation-level environment + INLA-SPDE model omits a major environmental process that is both biologically relevant to visible floral anthocyanin-associated colour and defensibly measurable at the approximately 1-km national scale. It separates three questions that should not be collapsed:

1. **long-term geographical environment** that may reflect adaptation, persistent population sorting and/or stable developmental context;
2. **short-term weather during corolla development**, which is a plasticity question because photographs are dated 2023-2025;
3. **population history and dispersal**, which belong in the residual spatial structure rather than being labelled environmental effects.

CIELAB a* is an image-derived visible phenotype. The environmental model cannot establish anthocyanin concentration, anthocyanin identity, vacuolar pH, UV reflectance or molecular pathway expression.

## Biological process inventory

| Process domain | Floral-colour rationale | Current representation | Same-scale public alternative | Audit decision |
|---|---|---|---|---|
| Warm-season temperature / thermal accumulation | Moderate low temperature can induce floral anthocyanin and chalcone synthase expression; geographic floral reflectance can covary with temperature | Temperature PC1 = BIO5 + BIO10 + GDD5 | BIO6, daily tas/tasmin/tasmax | **Core retained.** BIO6/cold extremes are a completeness guardrail; dated weather is a separate plasticity analysis, not an omitted climatological main effect |
| Long-term temperature variability | Repeated thermal variability can alter the ecological meaning of mean thermal regime | BIO4 directly | frost-change frequency, snow-cover days | **Core retained.** Exact interaction audit supports Temperature PC1 × BIO4 for conditional intensity; snow/frost remain guardrails |
| Mean climatic water supply | Floral anthocyanin frequency can increase under drier conditions in some polymorphic taxa | CMI + BIO12 + BIO14 PC1 | SWB, PET | **Core retained, but water-demand completeness requires VPD/SWB sensitivity** |
| Atmospheric drying demand | VPD captures evaporative demand not contained in precipitation alone; floral pigmentation has shown positive geographic association with VPD across taxa | not in primary model | CHELSA VPD, 30 arcsec | **High-value omission audit.** Fit VPD and SWB sensitivities before finalizing the model |
| Precipitation intermittency | Same mean water supply can have different consequences under seasonal precipitation | BIO15 directly | monthly/daily precipitation concentration | **Core retained.** Dryness × precipitation seasonality was tested; no automatic promotion |
| Solar/light exposure | Anthocyanins and flavonoids can respond to light/UV; floral anthocyanin concentration and UV patterns can vary with solar/UV environments | CHELSA RSDS | glUV UV-B; CHELSA daily RSDS | **RSDS retained.** glUV is ~15 arc-min and temporally mismatched, so it is not upsampled as a 1-km predictor. Dryness × RSDS remains an SI co-stress sensitivity |
| UV-B specifically | Mechanistically closer to photoprotection than total shortwave radiation | not directly represented | glUV (2004-2013, 15 arc-min) | **Documented measurement gap.** Resolution mismatch is too large for the current local 1-km geography; no false-precision interpolation |
| Terrain relief / local microclimate capacity | Slope and local relief alter exposure, drainage and microclimatic heterogeneity | Topography PC1 = slope + TRI + roughness | aspect/northness, TPI, heat-load or topographic wetness indices | **Relief retained.** Northness/TPI are useful one-at-a-time guardrails; Topography PC1 is not labelled direct microclimate |
| Elevation | Integrates temperature, moisture, radiation, snow and geographical history | excluded by design | WorldClim elevation | **Benchmark only.** Do not add to the process-decomposition model because it strongly aliases several mechanistic axes |
| Snow / frost | Can affect overwintering, phenology and developmental exposure; C. punctata also shows temperature-sensitive germination biology | indirectly through temperature/seasonality | CHELSA snow-cover days, frost-change frequency, BIO6 | **Secondary completeness guardrail**, not a direct floral-pigment mechanism |
| Wind | May affect evapotranspiration, flower microclimate and dispersal | not represented | CHELSA sfcWind | **Low-priority guardrail.** No direct floral-anthocyanin prediction; more relevant to realized exposure/dispersal than a core colour driver |
| Soil fertility / carbon / bulk density / pH | Resource availability and water holding can modify stress physiology; substrate effects on floral morphs occur in some systems | Soil PC1 using bdod, cfvo, sand, silt, N, OCD, SOC, pH | CEC; deeper SoilGrids layers | **Core broad soil context retained.** CEC and depth are documented omissions; avoid treating soil pH as direct vacuolar pH |
| Soil texture / water retention | Sand/silt balance affects drainage and effective drought | Soil PC2 | clay, CEC, deeper profiles | **Core retained.** Dryness × texture interaction already screened |
| Vegetation / canopy | Can alter flower-level illumination and microclimate | not directly represented | MLIT forest fraction / remote-sensing canopy | **Do not treat forest fraction as direct irradiance.** If used, only an SI habitat/light-context guardrail because it also reflects access/sampling context |
| Distance from coast | Can proxy maritime climate, island history and sampling geography | not primary | DEM-derived coast distance | **Benchmark/guardrail only.** It has no unique anthocyanin mechanism once climate and space are included |
| Latitude | Proxy for temperature, photoperiod and colonization history | captured indirectly by climate + space | coordinate itself | **Benchmark only**, not an environmental mechanism |
| Photoperiod | Can affect phenology and flavonoid regulation, but is almost deterministic from latitude/day of year at this scale | not primary | astronomical day length | **Not added** because latitude/DOY would make it a structural/phenological proxy rather than an independent national environment |
| Current-year developmental weather | Directly relevant to plastic regulation of corolla pigmentation | not in long-term model | CHELSA-daily 1-km tas/tasmin/tasmax/pr/rsds for 2023-2025 | **Important separate SI question**, but should not be mixed into the final climatological model; it asks plasticity rather than long-term geographical organization |

## Key literature basis

### Temperature

Shvarts, M., Borochov, A. & Weiss, D. (1997). Low temperature enhances petunia flower pigmentation and induces chalcone synthase gene expression. *Physiologia Plantarum* 99:67-72. DOI: 10.1111/j.1399-3054.1997.tb03432.x.

Koski, M.H. & Galloway, L.F. (2020). Geographic variation in floral color and reflectance correlates with temperature and colonization history. *Frontiers in Plant Science* 11:991. DOI: 10.3389/fpls.2020.00991.

### Drought, atmospheric demand and radiation

Sullivan, C.N. & Koski, M.H. (2021). The effects of climate change on floral anthocyanin polymorphisms. *Proceedings of the Royal Society B* 288:20202693. DOI: 10.1098/rspb.2020.2693. This study found pigmentation negatively associated with temperature and positively associated with VPD across its multi-species herbarium data, while explicitly noting that VPD had not been functionally linked to floral pigmentation.

Peach, K., Liu, J.W., Klitgaard, K., Mazer, S.J. & Ehlers, B.K. (2020). Climate predicts UV floral pattern size, anthocyanin concentration, and pollen performance in *Clarkia unguiculata*. *Frontiers in Plant Science* 11:847. DOI: 10.3389/fpls.2020.00847.

Short, A.W. & coauthors (2021). Abiotic environment predicts micro- but not macroevolutionary patterns of flower color in monkeyflowers (Phrymaceae). *Frontiers in Plant Science* 12:636133. DOI: 10.3389/fpls.2021.636133.

### Colour chemistry and inference ceiling

Tatsuzawa, F. et al. (2023). Flower colors and anthocyanins in the cultivars of *Campanula medium*. *Phytochemistry Letters* 53:13-21. DOI: 10.1016/j.phytol.2022.10.011. Campanula visible colour can depend on anthocyanin identity/pattern and intramolecular co-pigmentation, reinforcing that image a* is not a pigment-concentration assay.

## Public-data suitability

### CHELSA

CHELSA V2.1/BIOCLIM+ provides 30-arcsec climatological predictors including VPD, RSDS, wind, PET, CMI, site water balance, snow-cover days, growing degree days and growing-season summaries. The current model already uses the principal temperature, moisture, seasonality, GDD and RSDS domains. The highest-value same-resolution omission for plant water stress is atmospheric demand/site water balance, so VPD and SWB are subjected to exact INLA-SPDE sensitivity rather than automatically expanding the primary model.

Reference: Brun, P. et al. (2022). Global climate-related predictors at kilometer resolution for the past and future. *Earth System Science Data* 14:5573-5603. DOI: 10.5194/essd-14-5573-2022.

### SoilGrids

SoilGrids 2.0 provides 250-m predictions for SOC, total nitrogen, coarse fragments, pH, CEC, bulk density and texture at six standard depth intervals to 200 cm. The current model uses a broad 0-5-cm resource/texture basis but not CEC or deeper root-zone layers. This is reasonably comprehensive for a national screening layer, but it is not a complete rhizosphere measurement. Deeper soil and CEC are therefore documented as residual uncertainty rather than used to proliferate correlated primary predictors.

Reference: Poggio, L. et al. (2021). SoilGrids 2.0. *SOIL* 7:217-240. DOI: 10.5194/soil-7-217-2021.

### UV-B

The global glUV product is based on Aura-OMI and has 15-arc-min resolution for 2004-2013. It contains biologically closer UV-B information than RSDS but is much coarser than the approximately 1-km flower analysis. It is not resampled into apparent 1-km precision. The absence of direct UV-B is retained as a measurement limitation and a reason not to describe RSDS as a UV-B test.

Reference: Beckmann, M. et al. (2014). glUV: a global UV-B radiation data set for macroecological studies. *Methods in Ecology and Evolution* 5:372-383. DOI: 10.1111/2041-210X.12168.

## Variable-selection rule

A new environmental variable should enter the final observation-level Broad model only if it satisfies all of the following:

1. a distinct process not already adequately represented by the existing axes;
2. spatial resolution and temporal support compatible with the 1-km long-term geographical question;
3. broad data coverage without response-conditioned exclusions;
4. acceptable collinearity (maximum VIF <10, with attention to coefficient stability before that ceiling);
5. improved geographically blocked predictive loss relative to the simpler model;
6. positive spatial-block bootstrap gain and improvement in at least four of five folds;
7. no major destabilization of the core temperature/terrain interpretation or spatial hyperparameters;
8. a biological interpretation no stronger than the proxy actually measured.

Variables failing this rule remain documented guardrails or measurement gaps in Appendix S3 rather than being silently omitted.

## Final conceptual separation

The broad model should ultimately distinguish:

- **measured long-term environmental filtering/context** — fixed effects;
- **environmental interactions with transferable evidence** — retained only after exact INLA-SPDE adjudication;
- **short-term developmental plasticity** — separate dated-weather sensitivity if pursued;
- **unmeasured environment and population/dispersal history** — residual spatial field;
- **local pollinator opportunity** — separate 5-km Main analysis;
- **human provenance/context** — only after local event definition.
