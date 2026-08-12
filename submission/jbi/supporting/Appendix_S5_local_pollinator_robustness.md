# Appendix S5. Local focal-pollinator robustness and Bombus biogeographic guardrails

## Purpose and analysis hierarchy

This Appendix separates three questions that cannot be combined into one pollinator effect:

1. **Directional Main test:** are the pigmented sides of the sharpest nearby white-pigmented boundaries associated with greater predicted habitat opportunity for the documented broad focal pollinators *Bombus ardens* and *B. diversus*?
2. **Exposure and design robustness:** does that directional result persist when spatial scale, transition threshold, species set or SDM support scale changes?
3. **Biogeographic guardrails:** do five-species community boundaries coincide with flower-colour boundaries, and does apparent montane/alpine support survive comparison at nearly equal elevations?

Only the first question is a directionally signed test of the pigmentation-benefit relaxation hypothesis. Five-species Hellinger turnover is unsigned and is interpreted as community-boundary correspondence. The montane analysis is a negative guardrail against promoting shared high-elevation geography to an additional pollinator mechanism.

## Local graph and transition selection

The unit was a 1-km flower cell. For each radius (5, 10 and 25 km), each eligible cell was connected to its five nearest eligible neighbours; duplicate undirected edges were removed. A pure transition had

`abs(pigment_share_j - pigment_share_i) = 1`,

so one endpoint was entirely white and the other entirely pigmented in the available photographs. Sensitivities relaxed this threshold to >=0.75 and >=0.50. These contrasts describe observed cell samples rather than exact population morph frequencies.

A greedy non-overlapping pair set was selected by decreasing absolute colour difference, then shorter geographic distance, then stable cell identifier. Pair selection was blind to *Bombus* values, environmental values and white-to-pigmented orientation. Only after the set was fixed was each pair oriented white -> pigmented.

For exposure A, the signed contrast was

`dA = A_pigmented - A_white`.

The primary statistic was mean dA across non-overlapping pairs. Median dA and the proportion of positive pairs were required diagnostics because a positive mean can be produced by a minority of large contrasts. The one-sided null used 100,000 independent sign flips with seed 20260808.

Environment was not used to select, orient or weight pairs. As a confounding diagnostic, the fixed pairs were evaluated in the same eight standardized abiotic axes used by the finalized Broad pigmentation-state analysis: Temperature PC1, precipitation PC1, temperature seasonality, precipitation seasonality, topography PC1, soil PC1, soil PC2 and RSDS. Environmental distance is root-mean-square distance across these axes.

## Strict 5-km focal-pollinator result

The focal test used the occurrence-referenced exposure

`effective_occmax = max(A_ardens, A_diversus)`.

**Table S5.1. Primary 5-km pure-transition result.**

| Quantity | Value |
|---|---:|
| Non-overlapping pairs | 67 |
| Median geographic separation | 2.0 km |
| Median final-eight-axis environmental distance, selected transitions | **0.244** |
| Median final-eight-axis environmental distance, all local edges | **0.318** |
| Selected/all environmental-distance ratio | **0.769** |
| Mean pigmented-minus-white focal support | +0.03590 |
| Median contrast | -0.00277 |
| Proportion positive | 0.493 |
| One-sided sign-flip P | 0.02716 |
| BH q across 5/10/25-km pure focal tests | 0.08148 |

The mean was in the hypothesized direction, but the median was approximately zero/slightly negative and fewer than half of the pairs were positive. The result is therefore magnitude-driven by a subset of transitions rather than a pervasive pairwise shift. It is treated as weak local directional consistency, not evidence of pollinator-mediated selection.

The environmental diagnostic supports, but does not prove, the local-comparison rationale. The fixed 67 transitions are closer in the finalized eight-axis Broad state space than local graph edges overall. Because environmental variables did not determine which transitions entered the test, this diagnostic does not manufacture the directional *Bombus* contrast; nor does it demonstrate that all fine-scale environmental confounding has been eliminated.

## Scale and transition-threshold sensitivity

**Table S5.2. Occurrence-referenced focal exposure and final-eight-axis environmental diagnostic for pure transitions.**

| Radius | Pairs | Mean contrast | Median contrast | Proportion positive | One-sided P | Selected final8 env. median | All-edge final8 env. median |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 5 km | 67 | +0.03590 | -0.00277 | 0.493 | 0.02716 | 0.244 | 0.318 |
| 10 km | 109 | +0.00840 | +0.00538 | 0.514 | 0.32454 | 0.337 | 0.429 |
| 25 km | 171 | +0.00292 | -0.00179 | 0.497 | 0.43575 | 0.435 | 0.531 |

The directional mean attenuated rapidly beyond the strict local window. Selected pure transitions were less environmentally separated than the full local edge set at all three radii.

At 5 km, relaxing the colour threshold from a pure transition to >=0.75 produced a similar result (69 pairs, mean +0.03461, P=0.02781). The >=0.50 family was weaker (98 pairs, mean +0.01010, P=0.24727). At 10 and 25 km, threshold sensitivities were null. Any directional alignment is therefore concentrated in the nearest, sharpest observed state boundaries rather than a general association across local colour differences.

## Exposure-scale and species-set sensitivity

Raw cloglog support tests whether the result depends on species-specific occurrence-reference scaling. The raw maximum of *B. ardens* and *B. diversus* did not reproduce the focal nominal result.

**Table S5.3. Exposure sensitivities for 5-km pure transitions.**

| Exposure | Biological role | Mean contrast | Median contrast | Proportion positive | One-sided P |
|---|---|---:|---:|---:|---:|
| Occurrence-referenced max of *B. ardens* and *B. diversus* | primary directional exposure | +0.03590 | -0.00277 | 0.493 | 0.02716 |
| Raw-cloglog max of *B. ardens* and *B. diversus* | scale sensitivity | +0.00443 | +0.00281 | 0.522 | 0.26715 |
| Occurrence-referenced max across all five species | any-*Bombus* sensitivity | +0.02446 | -0.00554 | 0.463 | 0.08358 |
| Occurrence-referenced max of three montane/alpine species | substitution diagnostic | +0.01033 | 0.00000 | 0.373 | 0.22291 |

The fixed local inferential family does not contain separate species-by-species hypothesis tests. Individual occurrence-referenced scores remain available in the support table for transparent inspection, but the biological estimands are the documented two-species guild, the all-five maximum and the montane/alpine guardrail. This avoids selecting a species after inspecting the direction of its local association.

The raw-support failure, all-five P=0.0836, near-zero median, q=0.0815 across the three pure-transition scales and broader-scale null results are part of the Main claim ceiling.

## All-edge descriptive comparison

All local graph edges share endpoints and are not independent; they are therefore descriptive only.

**Table S5.4. All pure-transition edges before non-overlap selection.**

| Radius | Edges | Mean focal contrast | Median focal contrast | Proportion positive |
|---:|---:|---:|---:|---:|
| 5 km | 150 | +0.00862 | -0.00977 | 0.447 |
| 10 km | 345 | -0.00181 | -0.00139 | 0.499 |
| 25 km | 693 | -0.00325 | +0.00139 | 0.501 |

This reinforces the conclusion that the focal positive mean is not a ubiquitous transition-by-transition effect.

## Five-species community-boundary correspondence

A separate analysis asked whether sharp flower-colour boundaries coincide with larger changes in predicted *Bombus* community composition than nearby non-transition edges with comparable spatial and elevational context.

For each cell, the five occurrence-referenced species scores were normalized to relative composition and square-root transformed. Edge turnover was Hellinger distance:

`sqrt(sum((sqrt(p_i) - sqrt(p_j))^2)) / sqrt(2)`.

The fixed matched-background design used non-overlapping pure transition edges; non-transition controls sharing neither endpoint; control midpoint within 50 km of the transition midpoint; matching on edge length, midpoint elevation and absolute endpoint elevation difference; 20 nearest controls with at least 10 eligible controls; and 100,000 random matched-background replicates. Controls were selected without *Bombus* values.

**Table S5.5. Occurrence-referenced five-species Hellinger results.**

| Radius | Retained transitions | Mean matched excess | Median excess | Positive excess | One-sided P | BH q across scales |
|---:|---:|---:|---:|---:|---:|---:|
| 5 km | 55 | +0.03303 | +0.01125 | 0.600 | 0.06283 | 0.06283 |
| 10 km | 102 | +0.03268 | +0.00871 | 0.588 | 0.01423 | 0.02135 |
| 25 km | 165 | +0.04107 | +0.01946 | 0.606 | 0.00010 | 0.00030 |

For the 5-km specification, observed mean community turnover was 0.12720 compared with a matched-background mean of 0.11049. The P=0.06283 result is reported as such and is not relabelled as a significant confirmation. Directional consistency at 10 and 25 km indicates repeated landscape-scale correspondence, but those broader windows are not evidence for the highly local directional colour-maintenance mechanism tested in Main 2.

A within-species-rank Hellinger metric showed the same direction under the same matching design (mean excess +0.01645, +0.01907 and +0.03321 at 5, 10 and 25 km; P=0.03658, 0.00802 and 0.00001). Across the predefined combinations of 25/50/100-km background windows and 10/20/50 controls, mean excess remained positive for both community metrics at every scale. Statistical strength varied at 5 and 10 km, while the 25-km result was positive and nominally significant in every occurrence-referenced matching specification.

## Geographic repetition

Pair-specific matched excess was summarized in fixed 100-km projected blocks and in a grid shifted by 50 km in both axes. Blocks with at least three transitions were retained. Leave-one-block-out national means tested whether one region carried the result.

**Table S5.6. Spatial repetition under the matched-background specification.**

| Radius | Positive blocks, unshifted | Positive blocks, shifted | Minimum leave-one-block-out mean excess |
|---:|---:|---:|---:|
| 5 km | 6/7 | 7/9 | +0.02381 |
| 10 km | 8/11 | 12/14 | +0.02861 |
| 25 km | 12/18 | 15/19 | +0.03672 |

All leave-one-block-out means remained positive in both block grids. The community-boundary correspondence is therefore not attributable to one national transition zone. Nevertheless, the surfaces remain SDM-derived and may share finer unmeasured environmental structure with flower colour.

## Montane/alpine near-equal-elevation guardrail

The visually striking national overlap between pigmented highland flowers and *B. beaticola*, *B. consobrinus* and *B. honshuensis* was tested by restricting pure transitions to endpoints with nearly equal elevation. The signed statistic was the pigmented-minus-white difference in the maximum occurrence-referenced support across the three montane/alpine taxa.

**Table S5.7. All pure transitions with absolute endpoint elevation difference <=50 m.**

| Radius | Pairs | Mean montane contrast | One-sided P |
|---:|---:|---:|
| 5 km | 23 | -0.00333 | 0.84321 |
| 10 km | 29 | -0.00196 | 0.75510 |
| 25 km | 36 | -0.00589 | 0.83179 |

The BH q across scales was 0.84321. With the <=100-m restriction, means remained approximately zero or negative and every P was >=0.47. A conditional subset in which the documented focal guild did not increase toward the pigmented side was also non-significant at every scale and elevation restriction.

The appropriate interpretation is shared high-elevation/spatial co-distribution, not substitution by montane bumblebees and not an additional pollinator mechanism.

## Main-versus-Supporting interpretation

The manuscript keeps the following inferential separation:

- **Main 2:** weak, highly local, directionally signed correspondence between pigmentation state and predicted opportunity for *B. ardens* plus *B. diversus*;
- **environmental guardrail:** the fixed transition pairs are locally close in the finalized eight-axis abiotic state space, without eliminating residual fine-scale environmental confounding;
- **Supporting biogeography:** sharp colour boundaries also tend to coincide with predicted five-species community boundaries under matched local comparisons;
- **negative guardrail:** montane/alpine overlap vanishes when elevation is held approximately equal.

The community result does not strengthen the focal mean into causal evidence because it is unsigned and generated from environment-derived surfaces. The montane result prevents a visually attractive map overlap from being narrated as a second positive mechanism. The final-eight-axis distance audit supports the locality of the fixed comparison set while leaving pair identity and the directional test unchanged.

## Reproducibility resources

Current files:

- `R/local_pair_graph.R` — local graph and deterministic non-overlap utilities;
- `scripts/run_bombus_local_sharp_transition.R` — Main directional test and scale/exposure sensitivities;
- `scripts/run_bombus_spatial_replication_test.R` — matched community-turnover and montane/elevation guardrails;
- `analysis_sensitivity/audit_bombus_final8_environment_distance.R` — final-eight-axis environmental balance audit of the fixed transition pairs;
- `.github/workflows/bombus-final8-environment-audit.yml` — checksum-locked balance workflow;
- `reproducibility/bombus_local_sharp_transition_current_spec_2026-08-09.md`;
- `reproducibility/bombus_local_sharp_transition_current_results_2026-08-09.md`;
- `reproducibility/bombus_spatial_replication_test_spec_2026-08-09.md`;
- `reproducibility/bombus_spatial_replication_test_results_2026-08-09.md`.

Local-transition evidence:

- workflow `31263324505`;
- artifact `9023416810`;
- SHA-256 `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`.

Community/elevation evidence:

- workflow `31285234317`;
- artifact `9029595037`;
- SHA-256 `067dd3408b2a7f046ba263732ffa4cefa2f54a7f1fb672478be46bca425f6bf0`.

Final-eight-axis environmental-balance evidence:

- workflow `31538548679`;
- artifact `9119773035`;
- SHA-256 `51fc846d2f3d815d8bbf3c0b95647eabeb0acf731f1e6dd164c11a0dfe1b896f`.

None of these analyses measures realized pollinator activity or selection. Direct species-resolved visitation, flower contact, pollen transfer and fitness measurements remain necessary to test the proposed mechanism.
