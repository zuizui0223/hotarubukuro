# Spatially matched Bombus-boundary replication test

Date: 2026-08-09

## Status and purpose

This is a post-hoc diagnostic requested after observing that white/pigmented flower-colour turnover covaries with predicted Bombus community turnover and that montane/alpine Bombus support also follows the high-elevation pigmented-flower geography. It is **not** an independent confirmatory analysis.

The purpose is narrower than causal pollinator inference: test whether repeated local coincidence between sharp flower-colour boundaries and predicted Bombus-community boundaries is stronger than expected from the **shared broad spatial/elevational template alone**. No environment+SPDE predictive null is used. Because the Bombus surfaces are themselves environmental predictions, even a positive result cannot identify a causal Bombus effect.

The result will determine presentation rather than rescue a preferred mechanism:

- if the spatially matched boundary excess is robust, community turnover may be retained as a corroborating main-text result;
- if it is not robust, community turnover is supplementary spatial co-structure only;
- montane/alpine Bombus will remain supplementary unless an effect survives a near-equal-elevation test.

## Fixed inputs

Use the checksum-locked fresh flower-cell artifact and the checksum-locked occurrence-referenced Bombus support artifact already used by the local sharp-transition analysis.

Flower response at cell x:

`pigmented_share = n_pigmented / n_observations`.

Five Bombus species:

- documented effective guild for the focal plant: *B. ardens*, *B. diversus*;
- montane/alpine diagnostic group: *B. beaticola*, *B. consobrinus*, *B. honshuensis*.

Occurrence-referenced species scores are the fixed scores from the previous refinement; no new calibration or threshold is fitted here.

## Local edge graph

Reconstruct the same local graph family used by the sharp-transition analysis:

- projected 1-km flower cells;
- up to five nearest neighbours per cell;
- no spatial-fold restriction;
- common five-species SDM support required;
- radii 5, 10 and 25 km;
- 5 km is focal because this analysis asks about local pollinator-mediated selection and was already the focal scale before this spatial-overlap diagnostic.

A **sharp flower-colour boundary** is a pure transition edge with

`abs(delta pigmented_share) = 1`.

The same deterministic greedy non-overlap rule as the established sharp-transition analysis is used so each flower cell contributes to at most one focal transition edge. Bombus values are not used to select or orient these transition edges.

## Primary community-turnover metric

For each cell, normalize the five occurrence-referenced Bombus support scores to relative composition and take square roots. Community turnover on an edge is the Hellinger distance

`sqrt(sum((sqrt(p_i)-sqrt(p_j))^2)) / sqrt(2)`.

This metric concerns predicted **community composition**, not total Bombus abundance or visitation.

A sensitivity repeats the analysis with the five pre-existing within-species-rank scores used by the original community-turnover analysis.

## Spatial/elevational matched background

The primary question is whether a sharp flower-colour boundary has more Bombus-community turnover than nearby edges with a comparable broad spatial and elevational setting.

For each non-overlapping sharp transition edge at each radius:

1. Candidate control edges are all local graph edges that are **not** pure flower-colour transitions.
2. Exclude any candidate sharing either endpoint with the focal transition edge.
3. Require the candidate edge midpoint to lie within 50 km of the focal transition midpoint. This is the fixed primary spatial-background window.
4. Within that local candidate pool, match on three response-independent geometric/topographic quantities:
   - edge geographic length;
   - midpoint elevation;
   - absolute elevation difference between endpoints.
5. Standardize those three quantities across the eligible local candidate pool and retain the 20 nearest controls by Euclidean matching distance, with deterministic tie-breaking by edge ID.
6. A transition is retained only if at least 10 eligible controls exist.

The matched-control construction does not use Bombus values.

Fixed matching sensitivities:

- midpoint background windows 25 and 100 km;
- 10 and 50 retained nearest controls instead of 20.

No matching choice will be selected by P value.

## Primary statistic and matched null

For each retained transition edge i, define

`excess_i = Hellinger_transition_i - median(Hellinger_matched_controls_i)`.

Primary descriptive evidence of repeated local alignment is:

- mean `excess_i`;
- median `excess_i`;
- proportion `excess_i > 0`.

For inference, generate 100,000 matched-background replicates. In each replicate choose one control at random from each focal transition's fixed matched set, calculate the mean selected-control Hellinger distance, and compare it with the observed mean Hellinger distance across the focal transition edges. The one-sided empirical P tests whether sharp flower-colour boundaries have greater predicted Bombus-community turnover than their matched local spatial/elevational background.

The case edges are fixed throughout; flower colour is never permuted to tune spatial structure.

## Repetition across geography

A national mean can be driven by one geographic region. Therefore the pair-specific `excess_i` values are also summarized in fixed 100-km projected spatial blocks using the unshifted grid and a 50-km x/y shifted grid as a sensitivity.

For blocks containing at least three focal transitions, report:

- number and fraction of blocks with positive mean excess;
- block-level mean excess;
- leave-one-block-out national mean excess.

This is a robustness diagnostic, not a second independent P value. A result will be described as geographically repeated only if the primary matched-background test is positive and the effect is not carried by a single block (leave-one-block-out means retain the positive sign and positive block means are not confined to one block).

## Montane/alpine guardrail: beyond elevation

The montane/alpine group is **not** assumed to be an effective pollinator guild for *Campanula punctata*. Its previous apparent association with pigmented high-elevation transitions may simply reflect shared elevation/spatial structure.

For the established non-overlapping pure transitions at 5, 10 and 25 km, test

`delta_montane = max(beaticola, consobrinus, honshuensis)_pigmented - max(...)_white`

only among near-equal-elevation pairs:

- primary restriction: absolute elevation difference <= 50 m;
- sensitivity: <= 100 m.

Use a 100,000-replicate one-sided sign-flip test of the mean delta. Also report median and proportion positive.

The previous conditional `documented-effective guild does not increase toward pigmented` subset is retained only as a supplementary diagnostic and is subjected to the same elevation restrictions.

**Interpretation rule:** unless the montane/alpine effect remains positive beyond these near-equal-elevation restrictions and is directionally consistent across scales, it will not be interpreted as substitution or an additional pollinator mechanism. It will be described only as spatial/elevational co-distribution, or omitted from the main Discussion.

## Main-text versus Supplement rule

The analysis is organized by biological interpretability and connection to the manuscript's central hypothesis, not by significance alone.

### Main-text priority

1. Broad-scale flower-colour geography versus abiotic environment/spatial structure (already established earlier in the pipeline).
2. Local sharp-transition test of the directly documented effective Bombus guild (*B. ardens* + *B. diversus*), because it directly tests the availability/relaxed-pigmentation-benefit hypothesis. Its uncertainty and lack of robustness must remain visible.
3. Five-species community turnover only if the spatially/elevationally matched boundary test shows an excess that is not carried by one geographic block. If retained, it is interpreted as corroborating evidence that local flower-colour boundaries coincide with predicted pollinator-community boundaries, not as evidence of species-specific colour preference.

### Supplement priority

- montane/alpine Bombus analyses unless they pass the near-equal-elevation guardrail;
- raw-SDM versus occurrence-referenced exposure sensitivities;
- broader 10/25-km local-scale sensitivities;
- earlier environment+SPDE Bombus-null analyses as conservative robustness checks rather than the primary Bombus estimand;
- detailed community-turnover metric variants.

## Claim ceiling

A spatially matched positive result can show that broad geographic location/elevation alone is insufficient to reproduce the observed coincidence of local flower-colour and predicted Bombus-community boundaries. It still cannot establish pollinator causation because the Bombus predictors are SDM-derived and may share unmeasured environmental structure with flower colour. Direct presence/absence, visitation, pollen transfer, reproductive success, or an independent accessibility contrast would be required to identify the mechanism.