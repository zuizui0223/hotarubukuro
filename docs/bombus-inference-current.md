# Current Bombus inference for the manuscript

This file is the **current manuscript-facing Bombus interpretation**. Superseded all-five limitation-gate designs and earlier national environment+SPDE directional tests are archived under `legacy/method-development/`.

## Main question

After the national environment+space analysis establishes the broad flower-colour template, the pollinator question changes scale:

> Across abrupt nearby white-pigmented boundaries, does predicted availability of the documented broad focal bumblebee pollinators increase from the white side to the pigmented side?

The main test is intentionally local. Bombus SDMs are themselves environmentally structured, so another national regression of flower colour on environment, space and Bombus suitability would not cleanly identify a biotic mechanism. Changing the comparison unit to nearby sharp colour transitions reduces broad geographical confounding by design, while not claiming that all fine-scale environmental confounding has been removed.

## Primary focal pollinators

The main availability exposure uses only:

- *Bombus ardens*;
- *Bombus diversus*.

These are the broadly distributed, directly documented focal Bombus pollinators used for the directional availability hypothesis.

The three montane/alpine taxa—*B. beaticola*, *B. consobrinus* and *B. honshuensis*—are **not** added to the primary availability maximum. Their niches overlap the same elevational geography in which pigmented flowers are common, so pooling all five would change the estimand from local availability of the documented broad focal pollinators toward broad Bombus niche replacement.

Short rule:

> **2 species = directional local availability hypothesis.**  
> **5 species = supplementary community/biogeographic turnover.**

## Occurrence-referenced support

Raw SDM values are not treated as abundance, visitation or pollination pressure and are not assumed to be comparable among species.

For each species, the selected-model cloglog prediction at a flower cell is mapped to its empirical CDF relative to predictions at that species' occurrence cells. The resulting score is a **relative occurrence-referenced habitat-support scale**.

It is not:

- occurrence probability;
- abundance;
- visit rate to *Campanula punctata*;
- pollen transfer;
- pollination effectiveness;
- selection pressure.

The manuscript-facing exposure is the maximum occurrence-referenced support of *B. ardens* and *B. diversus*.

Current builder:

- `scripts/build_bombus_occurrence_reference_support.R`
- `.github/workflows/bombus-occurrence-reference-support.yml`
- `reproducibility/bombus_occurrence_reference_support_2026-08-09.md`

## Local sharp-transition design

Primary design:

- 1-km flower cells;
- up to five nearest neighbours;
- maximum radius 5 km;
- pure observed white-pigmented transitions (`abs(delta pigment_share)=1`);
- pair selection without Bombus values;
- pair selection without using transition direction;
- greedy non-overlap before orientation;
- orientation only after selection, white -> pigmented;
- primary statistic = mean pigmented-minus-white focal-pollinator support;
- 100,000 sign flips.

Environment is used as a diagnostic rather than as a second local regression or as a post-result pair filter.

Current files:

- `R/local_pair_graph.R`
- `scripts/run_bombus_local_sharp_transition.R`
- `.github/workflows/bombus-local-sharp-transition.yml`
- `reproducibility/bombus_local_sharp_transition_current_spec_2026-08-09.md`
- `reproducibility/bombus_local_sharp_transition_current_results_2026-08-09.md`

## Main result and claim ceiling

Primary 5-km pure-transition set:

- 67 non-overlapping pairs;
- median separation 2.0 km;
- mean pigmented-minus-white focal-pollinator support +0.0359;
- median contrast approximately zero/slightly negative;
- 49.3% of pairs positive;
- one-sided sign-flip P=0.0272;
- across 5/10/25-km pure-transition family, q=0.0815;
- raw-cloglog support does not reproduce the 5-km result;
- broader 10- and 25-km effects attenuate strongly.

Therefore the manuscript interpretation is deliberately limited:

> The result is weak, local consistency with a hypothesis that the benefit of maintaining a pigmented floral signal may be greater where important bumblebee pollinators are locally available. It is not evidence of pollinator-mediated selection and does not imply that most colour boundaries follow the same Bombus gradient.

No persuasive local Bombus association is retained for conditional intensity among already pigmented flowers. If the weak local signal is biological, it is therefore more consistent with maintenance/loss of a pigment state than with progressive darkening.

## Supplementary five-species community correspondence

Five-species Bombus community turnover is supplementary because Hellinger turnover is unsigned: it can show that a flower-colour boundary is also a pollinator-community boundary, but it does not specify which side should be white or pigmented.

The matched-background analysis asks whether sharp colour boundaries have greater predicted five-species community turnover than nearby edges of comparable geographic and elevational context.

Current files:

- `scripts/run_bombus_spatial_replication_test.R`
- `.github/workflows/bombus-spatial-replication-test.yml`
- `reproducibility/bombus_spatial_replication_test_spec_2026-08-09.md`
- `reproducibility/bombus_spatial_replication_test_results_2026-08-09.md`

Preferred interpretation:

> Sharp flower-colour boundaries also coincide with unusually large changes in predicted Bombus assemblage composition, indicating that colour-transition zones are often embedded in broader pollinator biogeographic transition zones.

Do not call this a directional flower-colour mechanism.

## Montane/alpine guardrail

The national map shows apparent positive overlap between pigmented flowers and montane/alpine Bombus support. That overlap is not promoted as a pollinator mechanism because the same taxa and pigmented flowers share high-elevation geography.

Among near-equal-elevation local transitions, the additional montane/alpine correspondence disappears. This is a negative guardrail demonstrating why visually compelling national map overlap is insufficient for causal interpretation.

Preferred statement:

> Montane/alpine Bombus covaried geographically with pigmented flowers, but the correspondence disappeared among near-equal-elevation local transitions, indicating that this component of the overlap largely reflects shared elevational geography.

## Causal ceiling

All Bombus surfaces are environment-based SDM predictions. The present study does not measure realized visitation, abundance, first approach, flower choice, pollen deposition, seed set or fitness.

The next causal tests should target the identified local transition zones with:

- species-resolved visitation/occupancy;
- standardized visible and UV reflectance;
- receptor-based colour contrasts;
- pollen removal/deposition;
- seed set or other fitness components;
- local abiotic measurements;
- experimental colour manipulation where feasible.

The current paper therefore uses Bombus SDMs to identify **predicted pollinator context**, not to label the observed flower-colour geography as pollinator-mediated selection.
