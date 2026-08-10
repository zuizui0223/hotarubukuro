# Local sharp flower-colour transition × Bombus: results

Date: 2026-08-08

Workflow run: `31263324505`  
Computation commit: `2ff2c3136d19f5294bab059fa5070ffa11b5fd4f`  
Result artifact: `9023416810`  
Artifact SHA-256: `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`

This is a post-null exploratory local-scale refinement. It intentionally does not use an environment+SPDE predictive null. The analysis conditions on geographically local observed flower-colour transitions and asks whether the white-to-pigmented direction aligns with the predicted Bombus contrast. Environment is only a descriptive pair-similarity diagnostic.

## Focal strict-local result

Focal design: 5-km graph, five nearest neighbours, no spatial-fold restriction, no environmental caliper, pure observed white-versus-pigmented transition (`abs(d pigment_share)=1`), greedy non-overlapping pairs selected without Bombus and without using transition sign, occurrence-referenced max support of the documented effective guild (*B. ardens* + *B. diversus*).

- non-overlapping transition pairs: 67;
- median geographic separation: 2.0 km;
- median environmental-PC distance among selected transition pairs: 0.180;
- median environmental-PC distance among all local 5-km graph edges: 0.343;
- mean pigmented-minus-white effective-Bombus support difference: +0.03590;
- median difference: -0.00277;
- proportion of pairs with higher effective-Bombus support on the pigmented side: 0.493;
- one-sided 100,000-replicate sign-flip P = 0.02716;
- BH q across 5/10/25 km for the same pure-transition effective-guild test = 0.08148;
- all-tests BH q = 0.16686.

Thus the strictest 5-km non-overlapping transition set has a nominal positive mean Bombus contrast, but the median and sign proportion are not positive. The mean signal is therefore magnitude-driven rather than a majority-of-transitions pattern and is not treated as robust confirmatory support.

## Scale sensitivity for pure transitions

Occurrence-referenced effective-guild max:

- 5 km: mean difference +0.03590, P = 0.02716, n = 67;
- 10 km: mean difference +0.00840, P = 0.32454, n = 109;
- 25 km: mean difference +0.00292, P = 0.43575, n = 171.

The mean direction is positive at all three scales but rapidly weakens beyond the strict 5-km window.

The raw-cloglog effective-guild max does not reproduce the 5-km nominal signal:

- 5 km: mean difference +0.00443, P = 0.26715;
- 10 km: -0.00063, P = 0.53970;
- 25 km: +0.00033, P = 0.48134.

## Transition-threshold sensitivity

For occurrence-referenced effective-guild support:

- pure (`abs dY=1`) at 5 km: P = 0.02716;
- `abs dY>=0.75` at 5 km: P = 0.02781;
- `abs dY>=0.50` at 5 km: P = 0.24727.

At 10 and 25 km none of these threshold families is significant. Therefore any availability alignment is concentrated in the sharpest, nearest observed state transitions rather than general local colour differences.

## All-edge descriptive check

Because local graph edges share endpoints, all-edge summaries are descriptive rather than the sign-flip inferential unit. For pure transitions:

- 5 km: 150 edges, effective-guild mean aligned difference +0.00862, median -0.00977, proportion positive 0.447;
- 10 km: 345 edges, mean -0.00181, median -0.00139, proportion positive 0.499;
- 25 km: 693 edges, mean -0.00325, median +0.00139, proportion positive 0.501.

This reinforces that the nominal 5-km non-overlapping mean result is not a pervasive transition-by-transition directional effect.

## Environmental-similarity diagnostic

The selected pure transition pairs are not unusually environmentally divergent. Median environmental-PC distance is lower among selected sharp transitions than among all graph edges at each scale:

- 5 km: selected 0.180 vs all local edges 0.343;
- 10 km: selected 0.265 vs all local edges 0.442;
- 25 km: selected 0.371 vs all local edges 0.520.

This supports the design intuition that the selected transitions are geographically and environmentally local, while not claiming environment has been causally controlled.

## Potential montane/alpine substitution

The montane/alpine group is *B. beaticola* + *B. consobrinus* + *B. honshuensis* and is not assumed to be an empirically demonstrated effective guild for *C. punctata*.

For pure transitions, among pairs where documented effective-guild support does not increase toward the pigmented side:

- 5 km: n = 34, mean montane-support difference +0.02633, joint sign-flip P = 0.22713;
- 10 km: n = 53, mean +0.04805, P = 0.00708, BH q across scales = 0.02124;
- 25 km: n = 86, mean +0.03691, P = 0.02069, BH q = 0.03104.

However, the montane-support contrast is strongly associated with pigmented-minus-white elevation difference in this subset (Spearman rho approximately 0.67, 0.81, 0.86 at 5, 10, 25 km). The selected pigmented side is also higher in elevation on average. Therefore this pattern is best treated as an elevation-linked potential-substitution diagnostic, not evidence that montane Bombus causally maintain pigmentation.

## Current interpretation

The local-transition design is biologically closer to the hypothesis that pollinator-mediated selection acts locally, and the sharp selected pairs are indeed environmentally similar on the existing environmental-PC diagnostic. Nevertheless, the documented-effective-guild availability hypothesis is only weakly supported by the current SDM data: a nominal mean alignment appears at the strictest 5-km sharp-transition design, but it is not supported by the median, the sign proportion, the raw-SDM sensitivity, broader transition definitions, or 10/25-km scales.

Accordingly, the main-paper ecological hypothesis can remain `lower effective Bombus availability -> relaxed maintenance of pigmentation`, but the observational SDM result should be phrased as weak/local-scale consistency rather than a demonstrated directional effect. The stronger five-species community-turnover result remains supplementary evidence of pollinator-community correspondence, not a replacement causal mechanism.

Because the Bombus surfaces are generated from environmental predictors and are not realized visitation data, abiotic environment and pollinator availability cannot be causally separated by this analysis. Direct Bombus presence/absence, visitation, pollen-transfer effectiveness, or an island accessibility contrast remains the appropriate evidence for testing the mechanism itself.