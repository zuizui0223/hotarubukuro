# Comprehensive broad environmental and spatial-model audit

Date: 2026-08-11

## Purpose

The current Journal of Biogeography broad analysis uses two observation-level models:

1. Bernoulli pigmentation state for all environment-complete photographs; and
2. Gaussian visible intensity conditional on the observations classified as pigmented.

Both use eight environmental terms, an East/West structural adjustment and a stationary isotropic Matérn SPDE field. The current model is a strong broad reference, but it was assembled before a complete evidence audit of omitted public environmental proxies, observation-process controls and spatial structures implied by the natural history of *Campanula punctata*.

This sensitivity is designed to establish the final observation-level broad model. It does not automatically replace the separate 1-km-cell cross-fitted natural predictive reference used to define the 17 local-departure targets.

Frozen comparison input:

- workflow: `31258851297`;
- artifact: `9022276431`;
- artifact SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

## Ecological response hierarchy

The two responses are nested but not interchangeable.

- **Pigmentation state** is a threshold-like visible white/pigmented classification and may reflect regional regulation, genetic differentiation, developmental plasticity or their combination.
- **Conditional visible intensity** is the standardized CIELAB a* excess among pigmented photographs. It can reflect anthocyanin amount and composition, vacuolar chemistry, co-pigmentation, petal optics and image formation.

CIELAB a* is not a direct anthocyanin, UV-reflectance or pollinator-vision measurement.

## Literature-based environmental audit

The evidence registry `reproducibility/broad_environment_variable_evidence_registry_2026-08-11.csv` distinguishes four classes.

### Retained primary axes

1. **Warm-season thermal regime** — CHELSA BIO5, BIO10 and GDD5. Floral anthocyanin production can increase under cool conditions (Shvarts, Borochov & Weiss 1997; Stiles et al. 2007), and floral reflectance in *Campanula americana* covaries with temperature and colonization history (Koski & Galloway 2020).
2. **Moisture supply** — CMI, annual precipitation and driest-month precipitation. Across floral polymorphisms, pigmentation has been positively associated with aridity/VPD (Sullivan & Koski 2021); experimental drought can deepen anthocyanin-associated petal colour (Zhang et al. 2023).
3. **Temperature and precipitation seasonality** — retained as climatic context, with final joint adjudication of the already-supported Temperature PC1 × temperature-seasonality term.
4. **Shortwave radiation** — retained as broad light exposure. Direct flower experiments show light/UV-B induction of anthocyanin, but CHELSA RSDS is total shortwave radiation rather than UV-B (Hennayake et al. 2006; Zhou et al. 2025).
5. **Terrain relief and SoilGrids axes** — retained as contextual variables for drainage, resources, texture and unresolved microhabitat. They are not assigned a universal direction.

### High-priority omitted public proxies tested here

1. **VPD** — atmospheric demand can be more physiologically informative than precipitation supply.
2. **Site water balance** — integrates moisture supply and demand.
3. **BIO6** — a cold-extreme sensitivity; not promoted by default because the coldest month is biologically remote from summer corolla development.
4. **BIO13** — a wet-extreme/oceanicity guardrail with weak direct floral-pigment mechanism.
5. **Forest fraction** — proxy for canopy/open-habitat context and flower-level light environment, derived from current MLIT 1-km landscape outputs.
6. **Distance to coast** — a composite coastality/island-context proxy, tested only as a geographical sensitivity.

### Observation-process sensitivities

- DOY and year indicators;
- mask fraction and possible-overexposure flag.

These controls ask whether the environmental coefficients survive broad phenology and image-formation diagnostics. They are not part of the primary ecological mechanism set.

### Important gaps not inserted into the final model

- **UV-B climatology:** available global products are much coarser and period-mismatched relative to the 1-km 2023–2025 design; RSDS remains the broad light proxy with an explicit claim ceiling.
- **Actual pre-anthesis weather:** a key future analysis for separating plasticity from long-term differentiation, but it requires a separately specified time-varying acquisition and developmental window.
- **Taxonomic identity:** the source table does not record var. *punctata* versus var. *hondoensis*. Geography cannot be used as a substitute; an image-level calyx or genetic audit is required.
- **Soil P/K and direct rhizosphere moisture:** absent from the current harmonized public source boundary.

## Environmental model grid

Every model retains the current eight additive environmental terms and the current stationary region-adjusted SPDE while one evidence-based extension is evaluated.

### Pigmentation state

- additive reference;
- dryness × RSDS sensitivity;
- VPD;
- site water balance;
- VPD + site water balance;
- BIO6;
- BIO13;
- BIO6 + BIO13;
- forest fraction;
- log distance to coast;
- forest + coastality;
- DOY/year controls;
- image-QC controls;
- all observation controls;
- all six omitted environmental proxies;
- all six proxies + dryness × RSDS.

### Conditional intensity

The same extensions are evaluated on top of the exact-screen candidate Temperature PC1 × temperature seasonality. The narrow interaction adjudication also includes:

- additive reference;
- Temperature PC1 × temperature seasonality;
- precipitation PC1 × temperature seasonality;
- both terms;
- the predeclared Temperature PC1 × temperature seasonality + dryness × precipitation seasonality bundle;
- all three seasonality terms.

All environmental candidates are compared on one common complete-case population so that model improvement cannot be caused by changing the observations.

## Species biology and spatial-model hypotheses

### Evidence for regional continuity and barriers

Direct species evidence supports neither a purely exchangeable regional factor nor unrestricted Euclidean continuity.

- Mainland *C. punctata* is a self-incompatible perennial pollinated primarily by bumblebees; local pollinator assemblages can generate fine-scale floral selection mosaics (Nagano et al. 2014).
- The species can spread clonally through rhizomatous/stoloniferous growth, increasing local persistence without implying rapid among-population movement.
- Exact seed-dispersal kernels for *C. punctata* are not available in the current evidence base and are not invented here.
- Allozyme data from 17 populations separated a mainland group from the Izu island group. Nei genetic identity was approximately 0.97 within mainland populations, 0.95 within islands and 0.84 between mainland and islands; island populations also showed stronger among-population differentiation and a progressive southward colonization signal (Inoue & Kawahara 1990).
- Pollinator and breeding systems also differ sharply across the Izu chain (Inoue & Amano 1986; Inoue 1988).

These findings justify a sea-barrier sensitivity and caution against interpreting the spatial field as one dispersal or historical process.

### Spatial specifications compared

1. current stationary SPDE + East/West factor;
2. stationary SPDE without the 136.5°E factor;
3. each stationary model with a 1-km exact-site IID effect;
4. ocean-barrier SPDE + East/West factor;
5. ocean-barrier SPDE without the factor;
6. each barrier model with the exact-site IID effect.

The barrier triangles are defined from the processed elevation land/sea mask. The barrier range fraction is 0.2. This reduces latent correlation through ocean triangles but does not assert zero rare long-distance dispersal.

The exact-site IID effect asks whether repeated photos/cells contain residual local heterogeneity beyond the continuous field. It is not interpreted as a genetic population effect.

### Spatial claim ceiling

The SPDE field can contain:

- unmeasured environment;
- taxonomic mixture;
- population history and dispersal limitation;
- island barriers;
- observation structure;
- other coherent geography.

A better barrier or site-IID model supports a more appropriate residual covariance structure; it does not directly estimate migration, seed dispersal or colonization dates.

## Model comparison

Every full model reports:

- WAIC, DIC and mean negative log CPO;
- fixed effects and SPDE/site hyperparameters;
- VIF;
- five response-blind geographical folds;
- held-out log loss for state or squared error for intensity;
- AUC/Brier or RMSE/MAE/R²;
- a spatial-block bootstrap of paired held-out loss gain;
- fold consistency.

A candidate receives strong support relative to the declared reference only when:

1. the model fits successfully;
2. WAIC improves by at least 2 or mean negative log CPO improves by at least 0.001;
3. the spatial-block bootstrap lower 95% limit for predictive gain is above zero;
4. at least four of five geographical folds improve; and
5. maximum VIF is below 10.

Parsimonious models are preferred when a larger bundle produces no clear additional transfer gain.

## Finalization rule

The audit will produce one response-specific final observation-level model.

- If no environmental extension clears the rule, retain the current environmental basis.
- For conditional intensity, retain the Temperature PC1 × temperature-seasonality term only if it remains preferable after joint seasonality adjudication and the broader variable audit.
- Select a non-current spatial structure only if its held-out and full-fit evidence is consistent and its interpretation is biologically defensible.
- The final observation-level model may update coefficient interpretation and Figure 2/Appendix S3.
- The separate cell-level natural predictive reference and the 17 local-departure identities remain unchanged unless a later, explicitly approved predictive-reference rebuild is undertaken.

## Key references

- Hennayake, C. K. et al. (2006). DOI `10.2525/ecb.44.103`.
- Inoue, K. & Amano, M. (1986). DOI `10.1111/j.1442-1984.1986.tb00018.x`.
- Inoue, K. (1988). DOI `10.1111/j.1442-1984.1988.tb00178.x`.
- Inoue, K. & Kawahara, T. (1990). DOI `10.1002/j.1537-2197.1990.tb12554.x`.
- Koski, M. H. & Galloway, L. F. (2020). DOI `10.3389/fpls.2020.00991`.
- Nagano, Y. et al. (2014). DOI `10.1002/ece3.1191`.
- Poggio, L. et al. (2021). DOI `10.5194/soil-7-217-240-2021`.
- Shvarts, M., Borochov, A. & Weiss, D. (1997). DOI `10.1111/j.1399-3054.1997.tb03432.x`.
- Stiles, E. A. et al. (2007). DOI `10.1111/j.1399-3054.2007.00855.x`.
- Sullivan, C. N. & Koski, M. H. (2021). DOI `10.1098/rspb.2020.2693`.
- Zhang, S. et al. (2023). DOI `10.1111/ppl.13859`.
- Zhou, L.-J. et al. (2025). DOI `10.1111/pce.15390`.
