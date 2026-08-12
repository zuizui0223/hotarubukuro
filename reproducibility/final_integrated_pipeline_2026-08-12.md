# Final integrated manuscript pipeline — 2026-08-12

This document freezes the adopted manuscript-facing dependency graph, estimands, claim ceilings and evidence identities.

## 1. Inferential architecture

`YAMAP images -> two-part flower-colour phenotype -> Broad environment + space -> local focal-Bombus boundary test -> final-eight-axis local-departure calibration -> post-selection human context`

The stages use different comparison units because their ecological questions operate at different spatial scales.

### Anti-circularity rules

1. Flower-colour state/intensity is constructed without environmental, Bombus or human predictors.
2. Broad environmental/spatial models are defined before local biotic and human interpretation.
3. Bombus SDMs use Bombus occurrences and environmental inputs, not flower colour.
4. Occurrence-reference calibration is species-specific and does not use flower-colour responses.
5. Sharp transition pairs are selected without Bombus values and before white-to-pigmented orientation.
6. Final-eight-axis environmental distance is a balance diagnostic of the fixed Bombus pair set; it does not alter pair identity, weight or sign-flip statistics.
7. Human variables do not enter local-event definition, environmental matching, predictive replay or candidate selection.
8. Human features are examined only after candidates are defined, in one global maxT family.

## 2. Phenotype and analysis population

- screened source records: 1,965;
- YAMAP activity-photo rows: 1,964;
- unique YAMAP image hashes: 1,963;
- environment-complete integrated observations: **1,922**;
- white-like observations: **966**;
- pigmented observations: **956**;
- primary phenotypes:
  - pigmentation state across all 1,922 observations;
  - visible intensity conditional on pigmentation.

The white/pigmented boundary is estimated from colour alone. CIELAB a* is a reproducible visible-colour phenotype, not calibrated reflectance or anthocyanin concentration.

## 3. Broad environment + space

### Pigmentation state

`state ~ eight abiotic axes + East/West + stationary SPDE`

Abiotic axes:

- Temperature PC1;
- precipitation PC1;
- temperature seasonality;
- precipitation seasonality;
- topography PC1;
- soil PC1;
- soil PC2;
- RSDS.

No environmental interaction passes the full promotion rule for state. Maximum VIF=4.430.

### Conditional intensity

`intensity ~ eight abiotic axes + East/West + Temperature PC1:temperature seasonality + stationary SPDE`

The retained interaction has posterior mean -0.204234 and 95% CrI -0.301869 to -0.106561. Maximum VIF=6.340 for Temperature PC1; Soil PC1=5.233; interaction VIF=1.664.

VPD expansions are not retained because they generate severe collinearity and do not show sufficient geographical-transfer support.

### Spatial structure

- pigmentation-state range: 132.76 km, 95% CrI 88.78–195.68;
- conditional-intensity range: 65.72 km, 95% CrI 31.05–132.63.

The spatial field represents unresolved coherent geography. It is not interpreted as a dispersal distance or a single historical mechanism.

### Claim ceiling

Broad coefficients and spatial structure describe geographical association and residual organization. They do not by themselves establish direct physiological causation or population history.

## 4. Focal Bombus local-boundary test

### SDM layer

Five Bombus species are modelled on a common mainland domain with shared predictor screening and spatially blocked tuning.

Directional focal species:

- *Bombus ardens*;
- *Bombus diversus*.

Montane/elevation guardrails:

- *B. beaticola*;
- *B. consobrinus*;
- *B. honshuensis*.

Raw cloglog SDM support is transformed within species to occurrence-referenced support:

`A_k(x) = F_occ,k(s_k(x))`.

Primary focal exposure is `max(A_ardens, A_diversus)`.

### Local transition design

- 1-km cells;
- primary radius 5 km;
- pure observed white-pigmented transition;
- greedy non-overlapping pairs;
- pair selection without Bombus values;
- white-to-pigmented orientation after pair identities are fixed;
- primary statistic: mean pigmented-minus-white focal support;
- 100,000 one-sided sign flips.

Primary result:

- **67 pairs**;
- median separation 2.0 km;
- mean contrast **+0.03590**;
- median contrast **-0.00277**;
- positive pairs **49.3%**;
- one-sided P=**0.02716**;
- BH q across 5/10/25-km pure focal tests=**0.08148**.

The signal attenuates at 10 and 25 km and is not reproduced by raw cloglog support at 5 km (P=0.26715).

### Final-eight-axis environmental balance

For the fixed pair set:

- 5 km: selected median RMS distance 0.24408 vs all local edges 0.31752;
- 10 km: 0.33706 vs 0.42911;
- 25 km: 0.43545 vs 0.53145.

This diagnostic supports environmental locality of the selected comparisons without treating measured environment as eliminated.

### Supporting Bombus evidence

Five-species community turnover is retained as unsigned biogeographic context. Near-equal-elevation comparisons of the montane/alpine group do not support an independent high-elevation Bombus mechanism.

### Claim ceiling

The focal result supports at most weak, highly local correspondence between pigmentation state and predicted habitat opportunity. SDM support does not measure realized presence, visitation, stigma contact, pollen transfer, selection or fitness.

## 5. Calibrated local departures

### Event definition

- final-eight-axis cross-fitted pigmentation-state natural reference;
- RMS environmental distance <=1 across the same eight standardized abiotic axes;
- radius 10 km;
- minimum 3 eligible neighbours;
- event: pigmented focal cell with all eligible observed neighbours white;
- human variables absent from selection.

East/West is a structural Broad adjustment and is not an environmental-distance dimension. SPDE geography enters through the predictive natural reference.

### 10,000-map calibration

- observed candidates: **16**;
- null mean candidate count: **13.5908**;
- 95% null interval: **7–21**;
- count P=**0.27897**;
- observed candidate fraction: **0.04071**;
- null mean fraction: **0.03107**;
- upper-tail P=**0.12609**.

The observed local-departure frequency is compatible with the finalized natural geography.

## 6. Post-selection human context

One global maxT family contains:

- population exposure at focal/5/10/25/50 km;
- DID proximity;
- road proximity;
- built-up fraction;
- forest-human edge;
- forest cover;
- mountainness.

Leading feature:

- population within 5 km: contrast **+0.06744**;
- directional P=**0.00800**;
- global maxT FWER P=**0.05479**.

Measured within-dataset observation alternatives:

- observation-effort maxT P=0.96320;
- independent-site-support maxT P=0.75642.

### Claim ceiling

The result is a near-threshold short-scale settlement-exposure signal, not a corrected-significant anthropogenic effect. Horticultural opportunity, fine-scale environmental modification/plasticity and broader sampling accessibility remain competing interpretations. The 16 sites are field/provenance targets.

## 7. Evidence locks

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

### Bombus community/elevation guardrails

- run `31285234317`
- artifact `9029595037`
- SHA-256 `067dd3408b2a7f046ba263732ffa4cefa2f54a7f1fb672478be46bca425f6bf0`

### Final-eight-axis Bombus environmental audit

- run `31538548679`
- artifact `9119773035`
- SHA-256 `51fc846d2f3d815d8bbf3c0b95647eabeb0acf731f1e6dd164c11a0dfe1b896f`

### Local-departure/human replay

- run `31537102360`
- artifact `9119306089`
- SHA-256 `f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4`
- predictive-draw artifact `9094339466`
- SHA-256 `413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`

### Current Main figures

- run `31559274663`
- artifact `9127198711`
- SHA-256 `ff5d43e8f71224261b8b74ddb2d6e24a66a4f2349ad53fb72032118492bca924`

## 8. Integrated interpretation

The evidence supports a layered rather than single-driver view of flower-colour geography. Broad pigmentation state and conditional intensity show distinct environmental/spatial organization. Against that background, predicted opportunity for the documented broad focal bumblebees shows only weak, highly local alignment with sharp white-pigmented boundaries. Locally discordant pigmented configurations are not more frequent than expected under the finalized natural geography. Their strongest post-selection human feature is short-range population exposure, but familywise support remains just above 0.05. The analysis therefore narrows physiological, pollination and provenance hypotheses without identifying a single causal driver.

## 9. Empirical tests implied by the analysis

- common-garden temperature/light/water manipulations and pigment/spectral measurements;
- direct species-resolved Bombus visitation, floral contact, pollen deposition and fitness;
- local management/planting histories, vouchers and repeated population sampling for the 16 departure sites;
- population-genetic comparison among candidate, neighbouring white and horticultural material;
- genomic/spatial sampling to test lineage history and isolation-by-distance contributions to residual geography.
