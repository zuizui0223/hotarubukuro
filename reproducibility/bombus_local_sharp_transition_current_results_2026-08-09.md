# Main 2 — local sharp flower-colour transition × focal Bombus availability: current results

Date: 2026-08-09

Frozen workflow run: `31263324505`  
Frozen artifact: `9023416810`  
Artifact SHA-256: `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`

This note records only the results used by the current Main 2 analysis. Earlier montane-substitution and broad-null development interpretations are archived under `legacy/`.

## Focal 5-km pure-transition result

Design:

- 1-km cells;
- five nearest eligible neighbours within 5 km;
- pure white-pigmented observed transitions (`abs(d pigment_share)=1`);
- greedy non-overlapping pairs;
- pair selection Bombus-blind and sign-blind;
- orient white -> pigmented only after the pair set is frozen;
- primary exposure `effective_occmax=max(A_ardens,A_diversus)`;
- 100,000 one-sided sign flips.

Result:

- non-overlapping pairs: **67**;
- median geographic separation: **2.0 km**;
- selected-pair median environmental-PC distance: **0.180**;
- all local 5-km edges median environmental-PC distance: **0.343**;
- mean pigmented-minus-white focal-Bombus support: **+0.03590**;
- median difference: **-0.00277**;
- proportion with higher focal-Bombus support on the pigmented side: **0.493**;
- one-sided sign-flip P: **0.02716**;
- BH q across the 5/10/25-km pure-transition primary exposure family: **0.08148**.

The focal mean is in the hypothesized direction, but it is not a majority-of-pairs pattern. The median is approximately zero/slightly negative and only about half the transitions point in the positive direction. The result is therefore magnitude-driven by a subset of transitions and is treated as weak local consistency rather than robust support.

## Scale sensitivity

For the occurrence-referenced focal-pollinator exposure:

- 5 km: mean +0.03590, P=0.02716, n=67;
- 10 km: mean +0.00840, P=0.32454, n=109;
- 25 km: mean +0.00292, P=0.43575, n=171.

The direction remains positive but rapidly attenuates beyond the strict local window.

## Exposure-scale sensitivity

Raw cloglog max support of *B. ardens* + *B. diversus* does not reproduce the focal nominal signal:

- 5 km: mean +0.00443, P=0.26715;
- 10 km: mean -0.00063, P=0.53970;
- 25 km: mean +0.00033, P=0.48134.

This is an important limitation and is retained in the Main claim ceiling.

## Transition-threshold sensitivity

Occurrence-referenced focal-pollinator support at 5 km:

- pure `abs dY=1`: P=0.02716;
- `abs dY>=0.75`: P=0.02781;
- `abs dY>=0.50`: P=0.24727.

At 10 and 25 km the threshold families are null. Any directional alignment is therefore concentrated in the sharpest, nearest observed state transitions rather than a general relationship across all local colour differences.

## All-edge descriptive check

All local graph edges share endpoints and are therefore descriptive rather than inferential.

For pure transitions:

- 5 km: 150 edges, mean +0.00862, median -0.00977, proportion positive 0.447;
- 10 km: 345 edges, mean -0.00181, median -0.00139, proportion positive 0.499;
- 25 km: 693 edges, mean -0.00325, median +0.00139, proportion positive 0.501.

This further shows that the focal positive mean is not a pervasive transition-by-transition effect.

## Environmental-similarity diagnostic

Selected sharp transition pairs are not unusually environmentally divergent under the predefined PC summary:

- 5 km: selected 0.180 vs all local edges 0.343;
- 10 km: selected 0.265 vs all local edges 0.442;
- 25 km: selected 0.371 vs all local edges 0.520.

This supports the design intuition that the focal comparison is local while not claiming that environmental confounding has been eliminated.

## Biological interpretation

The current evidence is consistent, weakly and only at the strictest local scale, with the hypothesis that predicted opportunity for the documented focal Bombus pollinators is greater on some pigmented sides of abrupt colour boundaries. If this reflects pollination ecology, it is more naturally interpreted as a possible contribution to maintaining a visible pigment **state** than to progressively darkening flowers that are already pigmented; no persuasive Bombus relationship was found for conditional intensity.

The result does not demonstrate pollinator-mediated selection. The Bombus surfaces are environment-based SDMs, the raw support sensitivity is null, broader local windows are null and the mean is not supported by the median/sign proportion. Direct visitation, stigma contact, pollen transfer and fitness measurements remain necessary to test the mechanism.

Montane/alpine Bombus and five-species community turnover are intentionally handled in the separate Supporting Information guardrail analysis rather than used to strengthen this Main 2 result.
