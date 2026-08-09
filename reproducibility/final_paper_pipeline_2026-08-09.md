# Final paper pipeline lock - YAMAP data layer + broad -> fine -> anomaly

Date: 2026-08-09

This file defines the manuscript-facing scientific hierarchy. It supersedes the old 1,909-observation five-species limitation-gate narrative without deleting historical provenance.

## 0. Cross-cutting data layer - YAMAP as iEcology

The paper treats the use of YAMAP as a methodological contribution, not merely a source of more records.

**Observation process:** YAMAP is a hiking/navigation and activity-diary platform rather than a purpose-built biodiversity-recording database. Public activities can retain route-linked photograph positions. The study repurposes those recreational digital traces to recover incidental *C. punctata* photographs with spatial provenance.

**Human validation:** the author reviews taxon identity, focal flower/petal region, repeated photographs and image suitability before colour values enter the analysis.

**Claim ceiling:** this does not make YAMAP an unbiased or universally superior alternative to GBIF/iNaturalist. Route/access, recreational-use, subject-selection and flower-conspicuousness bias remain; users can hide locations and GPS can be imperfect. Author review reduces identification/ROI/duplicate error, not the original sampling bias.

**Relation to Main 3:** because YAMAP is concentrated on mountain-route geography, the full urban-rural gradient may be compressed, potentially reducing power for a broad human-context association. Trailheads, roads and accessible mountain margins can create observation-opportunity bias in the opposite direction. Both are discussed explicitly.

## 1. Main/Supp scientific hierarchy

### Main 1 - Broad quantitative natural template

**Question:** What broad environmental and continuous spatial structure organizes two distinct components of flower-colour geography?

**Responses:** pigmentation state; conditional visible intensity among pigmented flowers.

**Why this remains a substantive contribution:** many broad flower-colour studies use categorical morphs, which do not resolve variation among already pigmented individuals. Here image-derived quantitative phenotyping preserves both state and within-pigmented intensity, and the environmental analysis explicitly models residual spatial autocorrelation rather than treating observations as independent.

**Active fresh baseline:** source 1,965 rows -> phenotype n=1,922 -> 1-km cells n=1,305; white=966; pigmented=956; ambiguous=124; a* boundary=4.9687800109621.

**Canonical current-input rerun:**

- workflow run `31258851297`
- artifact `9022276431`
- SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`
- source branch/commit baseline `agent/reanalyse-from-1965-new-bombus` / `bfcde46e2d702dbfbfc11dd1121088d7383533d5`

Key performance: presence AUC=0.86348; cell any-pigmented AUC=0.85799; cell majority AUC=0.87066; Brier=0.15043. Conditional intensity RMSE=0.91924; MAE=0.71472; 95% coverage=0.94362.

### Main 2 - Fine-scale focal-pollinator availability

**Question:** Within the broad geographical template, do the sharpest nearby white/pigmented transitions point in the same direction as availability of the documented broad focal Bombus pollinators?

**Primary exposure:** occurrence-referenced `max(B. ardens, B. diversus)`.

**Why scale changes instead of adding SDMs to the broad model:** Bombus SDMs are generated from environmental geography. A national `colour ~ environment + space + Bombus SDM` coefficient would therefore be difficult to read as a separate biotic mechanism. The local design changes the comparison unit to abrupt nearby transitions at the scale where pollinator-mediated selection is biologically plausible. This reduces broad geographical confounding by design but does not eliminate unmeasured environment.

**Why not all five species:** the all-five maximum rank is structurally high everywhere under fresh SDMs (minimum=0.488889), because broad/lowland and montane/alpine taxa replace one another geographically. Adding all five changes the estimand from focal-pollinator availability to Bombus niche turnover. The high-elevation taxa also overlap the broad geography of pigmented flowers, and their apparent association disappears in near-equal-elevation comparisons. The montane analysis therefore functions as a guardrail against circular interpretation of shared high-elevation niche geography.

**Primary design:** five nearest neighbours within 5 km; pure white-vs-pigmented cell transitions (`abs(delta pigment_share)=1`); pair selection Bombus-blind and sign-blind; greedy non-overlap; orient only after selection white -> pigmented; 100,000 sign flips; environment is a diagnostic, not a pair filter or second local regression.

**Canonical occurrence-referenced support build:**

- workflow run `31262211605`
- artifact `9023137743`
- SHA-256 `d04c561b09b652591b9b479f6e26a779bb562c7c1b5f9b14e61d5e7ca8e2794b`

**Canonical local sharp-transition run:**

- workflow run `31263324505`
- artifact `9023416810`
- SHA-256 `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`
- computation commit `2ff2c3136d19f5294bab059fa5070ffa11b5fd4f`

Primary result: n=67; median pair distance=2.0 km; selected median environmental distance=0.180 versus 0.343 for all 5-km graph edges; mean pigmented-minus-white effective support=+0.035896; median=-0.002770; proportion positive=0.492537; one-sided sign-flip P=0.027160; BH q across 5/10/25-km pure effective tests=0.081479. Raw-cloglog 5-km sensitivity P=0.267147.

**Claim ceiling:** weak/local exploratory consistency with the pigmentation-benefit relaxation hypothesis. Not a robust general effect, not a causal selection estimate, and not evidence that all transitions follow the same direction.

### Main 3 - Event-based anomaly screen + post-selection human context

**Question:** Which local colour-state events are discordant with the broad natural reference, how often does the fitted natural model itself generate the same event, and do those pre-defined locations occupy distinctive human context?

**Why not raw residuals:** raw residual magnitude mixes ecological discrepancy with model uncertainty, leverage, sampling effort and local interpolation. The inferential object is instead a biologically interpretable event that can be applied identically to observed and simulated maps.

**Primary event:** a pigmented cell supported by >=3 neighbours within 10 km and environmental distance <=1, with zero pigmented neighbours. Human variables do not enter event definition.

**Calibration:** replay the identical event on 10,000 held-out cross-fitted natural maps and a 200,000-map joint posterior-predictive sensitivity.

Fresh result: 17 candidates. Cross-fit candidate count P=0.19958; fraction P=0.08739. Joint PPC count P=0.31446; fraction P=0.19618. The event is therefore **not a robust excess over the natural model**.

Post-selection context: population 5-km rank directional P=0.02697, maxT FWER=0.08991; population-DID alignment P=0.02298, maxT FWER=0.07592.

**Species-specific motivation:** *C. punctata* is cultivated ornamentally, so planting/escape/introgression is a plausible provenance hypothesis for individual locally discordant populations. It is not used to define candidates and is not supported as a general causal conclusion by the present data.

**Claim ceiling:** reproducible field/provenance targets with suggestive human context only; no anthropogenic-origin claim.

## 2. Supplementary Bombus biogeography

Five-species community turnover is supplementary because it is unsigned and does not specify which community should favour white or pigmented flowers.

**Spatially matched boundary diagnostic:**

- workflow run `31285234317`
- artifact `9029595037`
- SHA-256 `067dd3408b2a7f046ba263732ffa4cefa2f54a7f1fb672478be46bca425f6bf0`
- computation commit `a55f45aa3b5ef7836af6cc211b9cbc3c59058469`

Occurrence-referenced Hellinger mean matched excess: +0.03303 at 5 km (P=0.06283), +0.03268 at 10 km (P=0.01423), +0.04107 at 25 km (P=0.00010). Direction is stable across matching windows and spatial-block leave-one-out diagnostics.

**Interpretation ceiling:** flower-colour transition zones can coincide with boundaries in predicted Bombus-community geography. This is biogeographic correspondence, not evidence that species replacement causes colour change.

**Montane/alpine guardrail:** among pure transitions with endpoint elevation difference <=50 m, mean pigmented-minus-white montane support is -0.00333, -0.00196 and -0.00589 at 5/10/25 km (P=0.843/0.755/0.832). There is no evidence for an additional montane/alpine effect beyond shared elevational geography.

## 3. Design logic - each layer removes a different ambiguity

The paper should be read as a sequence of changing representations and comparison units:

1. **Data source:** repurpose a recreational GPS/photo stream and validate it ecologically;
2. **Phenotype:** replace one coarse colour class with a two-part quantitative trait;
3. **Broad scale:** estimate environmental associations while explicitly retaining residual spatial geography;
4. **Fine scale:** avoid treating environmentally generated SDMs as independent national predictors; test a directional pollinator hypothesis at abrupt nearby boundaries;
5. **Biogeographic guardrail:** show that visually striking high-elevation Bombus/pigmentation overlap does not persist beyond shared elevation;
6. **Anomaly object:** replace a raw residual tail with a repeatable local ecological event;
7. **Human context:** characterize pre-fixed departures rather than using human variables to create them.

The paper's novelty is therefore not the number of predictor families. It is the deliberate alignment of **data source, phenotype representation, spatial scale and inferential object** with the ecological question.

## 4. Manuscript-facing versus historical files

Manuscript-facing:

- `manuscript/ecology-and-evolution-manuscript-final.md`
- `manuscript/figure-map.md`
- `manuscript/supporting-information-plan.md`
- `docs/yamap-iecology-rationale.md`
- `scripts/run_bombus_local_sharp_transition.R`
- `scripts/run_bombus_spatial_replication_test.R` (Supplement)
- `.github/workflows/final-paper-analysis.yml`

Immediately preceding draft/provenance:

- `manuscript/ecology-and-evolution-manuscript.md`

Historical/provenance only:

- `.github/workflows/analysis-1909.yml`
- `scripts/run_analysis_1909.sh`
- earlier five-species lower-third Bombus limitation gate
- archived 1,923 publication materials
- earlier environment+SPDE Bombus null, except as Supplementary method-development provenance.

Do not merge this branch into `main` without explicit approval.