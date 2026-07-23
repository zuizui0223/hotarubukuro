# Mechanism-oriented workflow v3

## 1. Research purpose

The base result is the nationwide quantification of author-confirmed petal colour and the separation of measured environmental gradients from residual spatial structure with an INLA-SPDE model. The mechanism extension asks two questions for which *Campanula punctata* is unusually informative:

1. Does the geographical colour pattern contain a Bombus-associated component that cannot be reduced to one naïve species-distribution correlation?
2. Do dark-pink, relatively early observations form a broad-scale, multi-axis signature that is difficult to explain without some human horticultural influence?

Neither question is treated as a direct causal or provenance test. The contribution is a falsifiable evidence workflow for opportunistic, spatially biased image data.

## 2. Causal logic without requiring an SEM

Environment and biogeographic history affect both Bombus distributions and flower colour. Bombus is itself an organismal response to environment and space, so high collinearity between Bombus suitability, environment, and geography is expected biology rather than an automatic exclusion criterion.

The workflow therefore reports three distinct estimands:

- **Landscape concordance:** the total broad-scale association between colour and a Bombus proxy.
- **Shared biogeographic structure:** the part of the Bombus gradient predicted out of fold by environment and broad space and its association with colour.
- **Conditionally identifiable association:** the association supported by Bombus variation remaining after environmental and spatial prediction, plus local matched contrasts.

The last quantity is not substituted for the first two. VIF is a diagnostic of identifiability and coefficient competition; it is not a variable-deletion rule for the biological predictor.

## 3. Locked hypothesis hierarchy

### 3.1 Bombus hypotheses

- **B1 — directional broad concordance:** the prespecified prediction is that flower pinkness increases with suitability or absolute local availability of the two widespread Bombus species. A same-sign result in the opposite direction is descriptive co-geography, not support for B1.
- **B2 — proxy triangulation:** the direction is consistent between ENMeval suitability and a response-blind GBIF occurrence-composition index.
- **B3 — local contrast:** within the same region, observations matched for measured environment and separated by at most 100 km differ in colour according to Bombus availability.
- **B4 — distribution-type contrast:** widespread and alpine species blocks are analysed separately and only inside their common evaluated support.

Evidence is graded as follows:

- `pattern_only`: B1 only;
- `triangulated`: B1 and B2 have the same direction and neither is dominated by one poor-support proxy;
- `locally_supported`: the preceding evidence and B3 have the same direction with acceptable matching balance;
- `not_identifiable`: independent Bombus variation or matched contrasts are insufficient;
- `not_supported`: well-supported proxies and contrasts consistently fail or oppose the prediction.

### 3.2 Horticultural-influence hypotheses

- **H1 — tail specificity:** human exposure is associated more clearly with the dark-pink upper tail than with the median colour.
- **H2 — joint phenotype:** human exposure is associated with the joint occurrence of colour above a natural-reference Q0.90 and observation date below a natural-reference Q0.10.
- **H3 — mechanism convergence:** H2 strengthens where horticultural opportunity (H), receiving human–forest interface (R), and access (A) converge, after natural seasonal progression (N) is considered.
- **H4 — broad-scale consistency:** the signature is not confined to one arbitrary east–west boundary, one spatial fold, or one image-quality artefact.

The supported claim is limited to a `human-associated phenotype signature consistent with horticultural influence`. It does not identify a cultivar, an individual escape event, introgression, or planting history.

## 4. Data-role separation and anti-circularity rules

1. Colour values, exclusion rules, spatial folds, Bombus species groups, radii (50 and 100 km), reference fraction (lowest 30% human exposure within region), and tail thresholds (colour Q0.90/Q0.95; date Q0.10) are fixed before inspecting new mechanism results.
2. Bombus occurrence indices use only public Bombus records, coordinates, species identity, and response-blind deduplication. Flower colour is never used to construct them.
3. Low-human reference membership uses exposure variables, not colour or residual rank.
4. Natural-reference predictions are returned only to held-out spatial folds. Calibration is assessed among held-out reference observations before exposed observations are interpreted.
5. Candidate ranking is downstream of inference. Candidates are not counted again as an independent hypothesis test.
6. Extreme colours remain in the analysis. Removing the target upper tail would be outcome-dependent selection.
7. Missing R or A rasters remain missing. They are never silently replaced by zero, population, elevation, or another convenient proxy.

## 5. Analysis modules

### Module A — natural base

Use the locked environmental axes and PC-prior INLA-SPDE model. Report environmental fixed effects, spatial range and variance, and out-of-block predictive calibration. Treat the spatial field as unresolved geography that can include missing environment, demographic history, and genetic structure.

### Module B — Bombus triangulation

1. Retain the AICc-selected ENMeval suitability indices.
2. Build 50 and 100 km Gaussian-kernel occurrence summaries from GBIF records after one-record-per-species-by-1-km-cell-by-year deduplication.
3. Use the two widespread versus three alpine study species as an explicitly limited target group. Report absolute kernel density for the widespread block, alpine block, and all five species; target-group composition; and local support. The all-five-species density is the closest occurrence-based sensitivity for general Bombus availability, whereas the widespread share tests distribution type. Do not call any of them visitation frequency. Target-group composition reduces common recording-effort variation but is not a complete observation-effort correction.
4. Fit a four-rung held-out model ladder: landscape, environment-adjusted, space-adjusted, and environment-plus-space-adjusted.
5. Decompose each Bombus proxy into an out-of-fold environment-plus-space prediction and a remaining local component.
6. Construct response-blind high-versus-low local matches within region, 100 km, and a prespecified environmental-distance caliper.
7. As a seasonally explicit sensitivity, freeze GBIF month/day records and combine a 100 km spatial kernel with a 30-day temporal kernel. Analyse seasonal total density separately from temporal overlap normalized by local all-season records. Because both indices use flower observation date, all adjusted models and matching must include nonlinear DOY, observation year, and N. An apparent signal before these controls is not admissible evidence.

### Module C — horticultural signature

1. Define a low-human reference from the lowest 30% of H within East and West separately.
2. Train colour Q0.50/Q0.90/Q0.95 qGAMs and a fixed-basis date Q0.10 quantile regression only on reference observations outside each held-out spatial fold. The fixed-basis date model is used because the smaller reference sample produced an unstable extreme-quantile qGAM fit; the change is a convergence safeguard, not a result-selection rule.
3. Flag extrapolation using fold-specific natural-covariate support. Evaluate calibration in held-out reference observations.
4. Test both the probability and magnitude of excess colour, and the joint dark-pink/relatively-early event.
5. Use one prespecified variable per mechanism when available: H (horticultural opportunity), R (receiving interface), N (natural seasonal clock), and A (access). Additional rasters replace these in sensitivity analyses rather than entering a predictor soup.

### Module D — falsification

- Treat CIELAB L*, b*, and C* as correlated dimensions of the same floral colour phenotype, not independent replications or image-quality negative controls. Use response-independent image properties (pixel area, mask fraction, component count, and exposure warnings) as imaging diagnostics.
- Repeat Bombus results by species, proxy, radius, spatial scale, and leave-one-species-out widespread index.
- Treat failure after DOY adjustment as phenological confounding, even when an unadjusted seasonal proxy or an imbalanced match is strongly associated with colour.
- Report common support, matched-pair count, balance, quantile calibration, separation warnings, and spatial-fold heterogeneity.
- Classify an unsupported or non-identifiable result explicitly instead of selecting the model that best confirms the hypothesis.

## 6. Reviewer-facing claim ceiling

The workflow can support:

- a reproducible nationwide continuous-colour pattern;
- measured environmental gradients plus residual spatial structure;
- broad shared biogeography between flower colour and Bombus availability;
- a local Bombus association if independent occurrence and matched evidence converge;
- a multi-axis human-associated tail signature consistent with horticultural influence.

It cannot establish pollinator-mediated selection, historical causation, garden origin of an individual, cultivar identity, or genetic introgression without field, behavioural, common-garden, provenance, or genomic data.

## 7. Primary methodological references

- Paciorek CJ. 2010. The importance of scale for spatial-confounding bias and precision of spatial regression estimators. *Statistical Science* 25:107–125.
- Hughes J, Haran M. 2013. Dimension reduction and alleviation of confounding for spatial generalized linear mixed models. *JRSS B* 75:139–159.
- Roberts DR et al. 2017. Cross-validation strategies for data with temporal, spatial, hierarchical, or phylogenetic structure. *Ecography* 40:913–929.
- Dupont E, Wood SN, Augustin NH. 2022. Spatial+: a novel approach to spatial confounding. *Biometrics* 78:1279–1290.

## 8. Locked v10 outcome audit

The completed run uses `ecological_v9_final_public_HRNA_50km/` for the natural
base and `ecological_v10_final_mechanism_HRNA/` for the mechanism tests.

- **Natural base:** 1,923 observations; the PC-prior SPDE range is about 96.4 km
  and spatial standard deviation is about 0.556. The spatial field remains an
  ecological result about unresolved geography, not a nuisance term assigned to
  a single cause.
- **Bombus:** the widespread W guild has a negative landscape gradient while
  the montane A guild, and all three component ENMeval surfaces, have positive
  gradients. Thus floral colour aligns with broad pollinator-guild turnover,
  not with a single nationwide Bombus-availability axis. Total density and
  composition differ. After environment and space, held-out gain is approximately
  zero and local matching lacks balance. The turnover pattern is shared
  biogeography; a local pollinator effect is not identifiable.
- **Horticulture:** in West, the raw-colour H-by-R estimate is 0.127 (95% CI
  0.044--0.210) with spatial-fold incremental R2 of 0.0166. It remains positive
  after date adjustment, access adjustment, and boundary-cell exclusion. The
  pooled East-minus-West contrast is negative, as predicted for a West-specific
  signal. R alone is not positive.
- **Failed predictions retained:** the West H-by-R effect on raw observation DOY
  is positive rather than earlier and has an interval spanning zero; the joint
  dark-and-early event count is only nine. Tail strengthening is descriptive
  because the Q0.90-minus-Q0.50 interaction difference lacks an independent
  block-bootstrap interval.
- **Claim:** the present evidence supports a West-specific human-pressure by
  receiving-interface colour signature. It does not establish a horticultural
  origin, and the early-flowering component is not supported.

The horticultural extension is therefore informative because it is partly
supported and partly falsified. Reporting both outcomes prevents a circular
story in which every dark flower is labelled horticultural after model fitting.
