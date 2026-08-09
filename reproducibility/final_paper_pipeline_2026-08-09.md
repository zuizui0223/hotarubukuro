# Final paper pipeline lock - broad -> fine -> anomaly

Date: 2026-08-09

This file defines the manuscript-facing analysis hierarchy. It supersedes the old 1,909-observation five-species limitation-gate narrative without deleting historical provenance.

## 1. Main/Supp scientific hierarchy

### Main 1 - Broad natural template

**Question:** What broad environmental and continuous spatial structure organizes flower-colour geography?

**Responses:** pigmentation state; conditional visible intensity among pigmented flowers.

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

**Why not all five species:** the all-five maximum rank is structurally high everywhere under fresh SDMs (minimum=0.488889), because lowland/broad and montane/alpine taxa replace one another geographically. Adding all five changes the estimand from focal-pollinator availability to Bombus niche turnover. Montane/alpine associations also disappear in near-equal-elevation comparisons.

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

Primary result: n=67; median pair distance=2.0 km; selected median environmental distance=0.180 versus 0.343 for all 5-km graph edges; mean pigmented-minus-white effective support=+0.035896; median=-0.002770; proportion positive=0.492537; one-sided sign-flip P=0.027160; BH q across 5/10/25-km pure effective tests=0.081479. Raw-cloglog 5-km sensitivity P=0.267147. Claim ceiling: **weak/local exploratory consistency**, not a robust general effect or causal selection estimate.

### Main 3 - Event-based anomaly screen + post-selection human context

**Question:** Which local colour-state events remain geographically discordant with the broad natural reference, and do those pre-defined locations occupy unusual human context?

**Primary event:** a pigmented cell supported by >=3 neighbours within 10 km and environmental distance <=1, with zero pigmented neighbours. Human variables do not enter event definition.

**Calibration:** replay the identical event on 10,000 held-out cross-fitted natural maps and a 200,000-map joint posterior-predictive sensitivity. This is preferred to ranking cells by raw residual magnitude because the inferential object is a biologically interpretable local event under the same sampling geometry.

Fresh result: 17 candidates. Cross-fit candidate count P=0.19958; fraction P=0.08739. Joint PPC count P=0.31446; fraction P=0.19618. The event is therefore **not a robust excess over the natural model**.

Post-selection context: population 5-km rank directional P=0.02697, maxT FWER=0.08991; population-DID alignment P=0.02298, maxT FWER=0.07592. Claim ceiling: suggestive human context only; no horticultural provenance.

## 2. Supplementary Bombus biogeography

Five-species community turnover is supplementary because it is unsigned and does not specify which community should favor white or pigmented flowers.

**Spatially matched boundary diagnostic:**

- workflow run `31285234317`
- artifact `9029595037`
- SHA-256 `067dd3408b2a7f046ba263732ffa4cefa2f54a7f1fb672478be46bca425f6bf0`
- computation commit `a55f45aa3b5ef7836af6cc211b9cbc3c59058469`

Occurrence-referenced Hellinger mean matched excess: +0.03303 at 5 km (P=0.06283), +0.03268 at 10 km (P=0.01423), +0.04107 at 25 km (P=0.00010). Direction is stable across matching windows and spatial-block leave-one-out diagnostics.

**Interpretation ceiling:** flower-colour transition zones can coincide with boundaries in predicted Bombus-community geography. This is not evidence that species replacement causes colour change.

**Montane/alpine guardrail:** among pure transitions with endpoint elevation difference <=50 m, mean pigmented-minus-white montane support is -0.00333, -0.00196 and -0.00589 at 5/10/25 km (P=0.843/0.755/0.832). There is no evidence for an additional montane/alpine effect beyond shared elevational geography.

## 3. Why the scale changes are part of the design

The broad model and local tests do not estimate the same thing.

- Broad environment + space describes a national geographical template.
- Fine-scale sharp transitions intentionally reduce large-scale environmental/spatial covariation by restricting the estimand to abrupt nearby boundaries; no claim is made that this fully removes unmeasured confounding.
- Event-based anomaly inference asks whether the same local discordance occurs more often than on repeated natural maps, rather than treating residual magnitude as a biological mechanism.

This architecture is preferable to a single national model containing environment, space, five Bombus SDMs and human predictors, where environmentally derived pollinator surfaces and human geography would be difficult to interpret independently.

## 4. Manuscript-facing versus historical files

Manuscript-facing:

- `manuscript/ecology-and-evolution-manuscript.md`
- `manuscript/figure-map.md`
- `manuscript/supporting-information-plan.md`
- `scripts/run_bombus_local_sharp_transition.R`
- `scripts/run_bombus_spatial_replication_test.R` (Supplement)
- `.github/workflows/final-paper-analysis.yml`

Historical/provenance only:

- `.github/workflows/analysis-1909.yml`
- `scripts/run_analysis_1909.sh`
- earlier five-species lower-third Bombus limitation gate
- archived 1,923 publication materials
- earlier environment+SPDE Bombus null, except as Supplementary method-development provenance.

Do not merge this branch into `main` without explicit approval.
