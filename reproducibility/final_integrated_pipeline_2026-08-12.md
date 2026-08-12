# Final integrated manuscript pipeline — 2026-08-12

This document freezes the manuscript-facing dependency graph after final alignment of the Broad, focal-*Bombus*, and downstream human-context analyses. It supersedes the 2026-08-09 pipeline note for determining current primary analyses; older dated records remain provenance.

## 1. Core inferential architecture

The paper is not an omnibus regression of environment, pollinators and human variables. The final directed chain is:

`YAMAP images -> two-part flower-colour phenotype -> Broad environment + space -> local focal-Bombus boundary test -> current-Broad local-departure calibration -> post-selection human context`

The stages use different comparison units because their ecological questions operate at different scales.

### Anti-circularity rules

1. Flower-colour state/intensity is constructed without environmental, Bombus or human predictors.
2. Broad model selection is completed before the local biotic and human interpretations.
3. Bombus SDMs use Bombus occurrence plus environmental inputs, not flower colour.
4. Occurrence-reference calibration is species-specific and does not use flower-colour responses.
5. Sharp transition pairs are selected without Bombus or environmental values and before white-to-pigmented orientation.
6. Final-eight-axis environmental distance is a Bombus balance diagnostic only; it does not change pair identities, weights or sign-flip statistics.
7. Human variables do not enter the local-departure event definition, environmental matching, natural-map replay or candidate selection.
8. Human features are examined only after candidates are defined, in one global maxT family.

## 2. Phenotype and analysis population

- Screened source records: 1,965.
- YAMAP activity-photo rows: 1,964.
- Unique YAMAP image hashes: 1,963.
- Environment-complete integrated observations after exact-duplicate and raster-support exclusions: 1,922.
- White-like observations: 966.
- Pigmented observations: 956.
- Primary phenotypes:
  - pigmentation state across all 1,922 observations;
  - conditional visible intensity among pigmented observations only.

The white/pigmented mixture boundary is estimated from colour alone. CIELAB a* is treated as a reproducible visible-colour phenotype, not calibrated reflectance or anthocyanin concentration.

## 3. Main 1 — finalized Broad natural template

### Pigmentation state

Primary observation-level model:

`state ~ eight abiotic axes + East/West + stationary SPDE`

The eight measured abiotic axes are Temperature PC1, precipitation PC1, temperature seasonality, precipitation seasonality, topography PC1, soil PC1, soil PC2 and RSDS. No environmental interaction passed the full promotion rule for state.

Collinearity policy is graded rather than mechanical:

- VIF <5 preferred;
- VIF 5–10 requires explicit coefficient/spatial/blocked-transfer stability;
- VIF >10 prevents expansion absent exceptional support.

The state model maximum VIF is 4.430.

### Conditional intensity

Primary model retains Temperature PC1 × temperature-seasonality. Maximum VIF is 6.340 for Temperature PC1, Soil PC1 is 5.233 and the retained interaction itself is 1.664. VPD expansions generated VIF around 26 and failed geographical-transfer support, so they are not promoted.

### Claim ceiling

Broad results describe environmental and residual geographical organization. The SPDE field is not labelled population history, and coefficients are not proof of direct physiological causation.

## 4. Main 2 — focal Bombus local-boundary test

### SDM construction

Five species are rebuilt on a common mainland domain with shared predictors and spatially blocked ENMeval/maxnet tuning:

- focal broad pollinators: *B. ardens*, *B. diversus*;
- montane/alpine guardrails: *B. beaticola*, *B. consobrinus*, *B. honshuensis*.

The shared SDM predictor VIF screen uses VIF=10 as a predictive-design diagnostic; the largest retained VIF is 8.502 (CMI). Individual SDM coefficients are not interpreted as causal ecological effects.

Raw cloglog support is transformed within species to occurrence-referenced support:

`A_k(x) = F_occ,k(s_k(x))`.

The primary directional exposure is `max(A_ardens, A_diversus)`.

### Fixed local transition design

- 1-km cells.
- Up to five nearest eligible neighbours.
- Primary radius 5 km.
- Pure observed transition: absolute pigment-share difference = 1.
- Greedy non-overlapping pairs.
- Pair selection Bombus-blind, environment-blind and sign-blind.
- White-to-pigmented orientation only after pair set is frozen.
- Primary statistic: mean pigmented-minus-white focal support.
- 100,000 one-sided sign flips.

Primary result:

- 67 non-overlapping pairs;
- median separation 2.0 km;
- mean delta +0.03590;
- median delta -0.00277;
- proportion positive 0.493;
- one-sided P=0.02716;
- BH q across 5/10/25-km pure focal tests=0.08148.

The signal attenuates at 10 and 25 km, and raw cloglog support does not reproduce the 5-km nominal result (P=0.26715). It is therefore weak and magnitude-driven rather than a pervasive pairwise shift.

### Final-Broad environmental balance audit

The fixed pairs were re-audited in the same eight standardized abiotic axes used by the finalized Broad state analysis. This does not alter the pair set or Bombus statistic.

- 5 km pure transitions: selected median RMS distance 0.24408 vs all local edges 0.31752; ratio 0.76871.
- 10 km: 0.33706 vs 0.42911.
- 25 km: 0.43545 vs 0.53145.

Thus the local-comparison rationale does not depend on the historical four-PC environmental summary. Fine-scale environmental confounding remains possible because the Bombus surfaces themselves are environment-derived.

### Supporting Bombus results

Five-species community turnover is unsigned and remains Supporting Information. Occurrence-referenced Hellinger matched excess is positive at 5, 10 and 25 km, but the 5-km primary matching result has P=0.06283. Broader correspondence does not identify which side should be pigmented.

Montane/alpine support disappears under near-equal-elevation transition comparisons (<=50 m: all one-sided P>=0.755), so visually strong high-elevation overlap is treated as shared biogeography rather than a second pollinator mechanism.

### Claim ceiling

The Main result supports at most weak local correspondence between pigmentation state and predicted habitat opportunity for documented focal bumblebees. It does not measure realized presence, visitation, stigma contact, pollen transfer, selection or fitness.

## 5. Main 3 — current-Broad local departure and human context

### Primary natural reference and matching

The downstream primary is now explicitly aligned with the finalized Broad pigmentation-state environment.

- Natural predictive reference: final-eight-axis cross-fitted pigmentation-state model.
- Local matching: RMS distance <=1 across the same eight standardized abiotic axes.
- Radius: 10 km.
- Minimum environmental neighbours: 3.
- Event: focal cell contains pigmentation and all eligible local comparator cells contain no observed pigmented flower.
- Human variables absent from event selection.

East/West is a structural geographical adjustment in the observation-level Broad model and is not an environmental-distance dimension. SPDE geography belongs to the natural predictive reference rather than to the measured-environment matching metric.

The historical four-PC broad/within matching is sensitivity provenance only.

### 10,000-map natural calibration

- Observed candidates: 16.
- Null mean candidate count: 13.5908.
- 95% null interval: 7–21.
- Count P=0.27897.
- Observed candidate fraction: 0.04071.
- Null mean fraction: 0.03107.
- Upper-tail P=0.12609.

The local-departure frequency is therefore compatible with the finalized natural geography.

### Post-selection human family

One global maxT family contains population exposure at focal/5/10/25/50 km, DID proximity, road proximity, built-up fraction, forest-human edge, forest cover and mountainness.

Leading feature:

- population within 5 km: contrast +0.06744;
- directional P=0.00800;
- global maxT FWER P=0.05479.

Population at 10 km and DID point in the same direction but do not survive global correction. The effect largely disappears at 25–50 km.

Measured within-dataset observation alternatives are null after correction:

- observation effort maxT P=0.96320;
- independent-site-support maxT P=0.75642.

### Claim ceiling

The result is a near-threshold short-scale settlement-exposure signal, not a corrected-significant anthropogenic effect. Horticultural opportunity/propagule pressure, fine-scale environmental modification/plasticity and broader sampling-frame accessibility remain competing interpretations. The 16 sites are provenance/field targets, not anthropogenic anomalies.

## 6. Final numerical evidence locks

### Broad/current cells

- run `31258851297`
- artifact `9022276431`
- SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`

### Bombus source build

- run `31249841493`
- artifact `9020226937`
- SHA-256 `d5d639e8e00d1ccc2f887c53fa8041465905b29f6bca1127f816e8c7a649d708`

### Occurrence-reference support

- run `31262211605`
- artifact `9023137743`
- SHA-256 `d04c561b09b652591b9b479f6e26a779bb562c7c1b5f9b14e61d5e7ca8e2794b`

### Focal Bombus local transition

- run `31263324505`
- artifact `9023416810`
- SHA-256 `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`

### Bombus spatial/community guardrails

- run `31285234317`
- artifact `9029595037`
- SHA-256 `067dd3408b2a7f046ba263732ffa4cefa2f54a7f1fb672478be46bca425f6bf0`

### Final8 environmental audit of fixed Bombus pairs

- run `31538548679`
- artifact `9119773035`
- SHA-256 `51fc846d2f3d815d8bbf3c0b95647eabeb0acf731f1e6dd164c11a0dfe1b896f`

### Current-Broad human primary

- replay run `31537102360`
- artifact `9119306089`
- SHA-256 `f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4`
- locked final8 predictive-draw artifact `9094339466`
- SHA-256 `413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`

## 7. Manuscript interpretation in one paragraph

The final analysis supports a layered rather than single-driver view of flower-colour geography. Broad pigmentation state follows a cool-climate and unresolved regional template, whereas intensity among pigmented flowers depends on thermal context with additional moisture and terrain associations. Against that background, predicted opportunity for the documented broad focal bumblebees shows only weak, highly local and magnitude-driven alignment with the sharpest white-pigmented boundaries; broader scales and raw SDM support do not reproduce the signal, and montane overlap is explained by shared elevation. Finally, locally discordant pigmented configurations are not more frequent than expected under the finalized natural geography. Their strongest post-selection human feature is short-range population exposure, but familywise support remains just above 0.05. The current evidence therefore narrows physiological, pollination and provenance hypotheses without identifying a single causal driver.

## 8. Next empirical tests implied by the pipeline

- common-garden temperature/light/water manipulations and pigment/spectral measurements for Broad physiological hypotheses;
- direct species-resolved Bombus visitation, floral contact, pollen deposition and fitness for the local pollination hypothesis;
- local management/planting histories, vouchers and repeated population sampling for the 16 departure sites;
- population-genetic comparison among candidate, neighbouring white and relevant horticultural material for provenance;
- genomic/spatial sampling to test whether residual Broad geography follows lineage history or isolation by distance.
