# Local sharp flower-colour transition × Bombus analysis

Date: 2026-08-08

## Status

This is a post-null exploratory refinement motivated by the biological scale of pollinator-mediated selection. It does not erase or replace the previously reported nationwide environment/spatial analysis or the previous Bombus-SDM analyses.

## Why the scale changes here

The broad-scale part of the paper already asks how flower colour is structured across Japan by abiotic environment and spatial structure. The Bombus analysis is therefore assigned a different biological question: within local landscapes, where broad geography changes little, do abrupt observed transitions from white to pigmented flowers point in the same direction as predicted availability of effective bumblebee pollinators?

No environment+SPDE predictive null is used in this analysis. The Bombus surfaces are themselves environmental SDM predictions, so this analysis is not intended to identify an environment-independent causal Bombus effect. The manuscript must explicitly state that SDM-derived Bombus availability cannot be separated causally from its environmental basis.

Environment is used only as a descriptive diagnostic after pair construction (median environmental distance of selected pairs); it is not used to select pairs, adjust the response, or generate a null.

## Biological hypothesis

The directional hypothesis is:

higher potential availability of an effective Bombus pollinator -> greater pollination benefit of a coloured floral signal -> greater maintenance of the pigmented state.

The primary effective guild is *Bombus ardens* + *B. diversus*, because these are the Bombus taxa directly documented as predominant pollinators in the relevant *Campanula punctata* system. The primary availability score is the occurrence-referenced maximum of those two SDMs (`effective_occmax`): support by either documented effective species is sufficient to prevent a strict low-availability interpretation.

The montane/alpine taxa (*B. beaticola*, *B. consobrinus*, *B. honshuensis*) are not mixed into the primary effective guild. They are retained as a potential-substitution analysis: at local colour transitions where the documented effective guild does not increase toward the pigmented side, does predicted montane/alpine Bombus support increase instead? This is labelled potential substitution, not demonstrated pollination effectiveness.

## Local graph

Unit: 1-km flower cell.

Response-blind geographic graph:
- each cell is linked to its five nearest eligible flower cells within the specified radius;
- duplicate undirected edges are removed;
- common finite five-species SDM support is required;
- no spatial-fold restriction is used, because cross-fitted predictive maps are not part of this analysis;
- no environmental caliper is used.

Local radii: 5, 10, and 25 km. The 5-km analysis is the focal strict-local scale; 10 and 25 km show whether the result persists as the landscape window broadens. Because this scale refinement follows previous analyses, all three scales are retained and interpreted exploratorily.

## Sharp colour transition definition

For each geographic edge, let `dY = pigment_share_j - pigment_share_i`.

Primary sharp-transition set: `abs(dY) = 1`, i.e. one observed cell is entirely white and the other entirely pigmented in the available sample. This is the sharpest possible observed state contrast and avoids selecting an arbitrary percentile after viewing Bombus results.

Sensitivity transition sets:
- `abs(dY) >= 0.75`;
- `abs(dY) >= 0.50`.

Because most 1-km cells contain few images, these are described as observed local colour-state transitions, not exact estimates of population morph frequency.

## Non-overlapping transition pairs

Inference uses a greedy one-to-one transition set so no flower cell appears in more than one tested pair. Pair selection is Bombus-blind and sign-blind:
1. larger absolute colour difference first;
2. shorter geographic distance next;
3. stable site-id tie-break.

Thus flower-colour magnitude can define the transition zone, but the direction (which endpoint is pigmented) is not used until after the pair set is frozen.

All eligible sharp edges are retained as descriptive output; inferential randomisation is based on the non-overlapping set.

## Directional statistic and null

Each selected pair is oriented from the whiter endpoint to the more pigmented endpoint.

For a Bombus exposure `A`, define `dA = A_pigmented - A_white`.

Primary statistic: mean `dA` across the non-overlapping pure-transition pairs.

Also report median `dA` and the proportion of pairs with `dA > 0`; these are important robustness diagnostics because a positive mean can otherwise be driven by a small number of large contrasts.

The randomisation null conditions on the selected local transition pairs and on `abs(dA)`. Under no directional flower-colour/Bombus association, which endpoint of each non-overlapping transition is the pigmented endpoint is exchangeable. Therefore the null is generated by independently sign-flipping each pair's Bombus contrast. Use 100,000 Monte Carlo sign-flip replicates with fixed seed 20260808 and a one-sided alternative `mean(dA) > 0`.

This null does not remove environmental structure from the SDM; it tests only whether the observed white-to-pigmented orientation is systematically aligned with the local Bombus contrast.

## Bombus exposures

Primary:
- `effective_occmax`: max occurrence-referenced support of *B. ardens* and *B. diversus*.

Sensitivity:
- `effective_rawmax`: max raw cloglog support of *B. ardens* and *B. diversus*;
- `montane_occmax`: max occurrence-referenced support of *B. beaticola*, *B. consobrinus*, and *B. honshuensis*;
- `all5_occmax`: max occurrence-referenced support across all five species.

## Potential montane substitution diagnostic

Within the same non-overlapping transition pairs, define a subset where `d effective_occmax <= 0`, meaning the documented effective guild is not more supported on the pigmented side. In that subset report the signed difference in `montane_occmax`, its sign-flip P value, and the pigmented-minus-white elevation difference.

Because montane SDM support is expected to covary strongly with elevation, this analysis is explicitly diagnostic. A positive result means that high-elevation Bombus habitat support may compensate for the simple widespread-effective-guild gradient; it cannot establish that those species are effective pollinators of *C. punctata*.

## Environmental-similarity diagnostic

Using the already defined four-dimensional environmental PC representation (`broad50km_pc1`, `broad50km_pc2`, `within50km_pc1`, `within50km_pc2`), report the median environmental distance among selected sharp-transition pairs and compare it descriptively with the median among all local graph edges. This is not a filter or adjustment. It is only a check of the design intuition that the sharp transitions being studied are geographically and environmentally local.

## Claim ceiling

A positive result supports only: within observed local white-pigmented transitions, the pigmented side tends to coincide with higher predicted effective-Bombus habitat support.

It does not show actual Bombus presence, abundance, visitation, colour preference, pollen transfer, or causal selection. Because the SDMs are generated from environmental predictors, abiotic environment and pollinator availability remain inseparable in this observational analysis and this limitation must be stated in the Discussion.