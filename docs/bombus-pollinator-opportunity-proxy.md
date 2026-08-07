# Bombus pollinator-opportunity proxy: an eDNA-inspired directional design

## Biological hypothesis

The biological hypothesis is directional rather than merely correlational:

> Greater local exposure to flower-visiting *Bombus* should increase the opportunity for a shared attraction advantage of pigmented flowers to translate into differential visitation and, ultimately, pollinator-mediated selection favouring pigmentation.

The broad-scale data cannot observe visitation rate or selection directly. The statistical problem is therefore not to manufacture an abundance estimate from SDM suitability, but to define the strongest **pollinator-opportunity estimand** that occurrence data can actually support.

## Why the eDNA analogy is useful

The useful analogy is conceptual, not literal. eDNA studies often separate a latent ecological state (occupancy/presence) from the observation process (capture and molecular detection). Multiscale occupancy models estimate presence while explicitly acknowledging imperfect detection; they do not require eDNA concentration to equal organism abundance. Attempts to infer abundance from eDNA concentration can work in some systems, but require calibration because shedding, transport, degradation, sampling and amplification can all alter the concentration-abundance relationship.

Relevant examples are Schmidt et al. (2013, Methods in Ecology and Evolution, doi:10.1111/2041-210X.12052), Willoughby et al. (2016, Molecular Ecology Resources, doi:10.1111/1755-0998.12531), Dorazio & Erickson (2018, Molecular Ecology Resources, doi:10.1111/1755-0998.12735), and the biomass review of Rourke et al. (2022, Environmental DNA, doi:10.1002/edn3.185).

The corresponding lesson for this study is:

> Estimate **potential Bombus availability / encounter opportunity**, which the occurrence data can support, rather than labelling habitat suitability as abundance, visitation pressure, or selection pressure.

## Do not use `sum(MaxEnt suitability)` as "Bombus pressure"

A raw sum assumes all of the following without data:

1. suitability is proportional to local abundance;
2. the proportionality is the same among species;
3. local abundance is proportional to visitation of *Campanula punctata*;
4. each visit has the same selective effect; and
5. the SDM outputs are quantitatively comparable among species.

Those assumptions are too strong. Published pollination-service modelling has used SDM occurrence likelihood as a proxy for **potential** pollinator service, but explicitly notes that occurrence likelihood does not contain the number of individuals and that cross-species comparability is an additional assumption (Zulian et al. 2013, PLOS ONE, doi:10.1371/journal.pone.0076308).

Presence-only models are more naturally interpreted in terms of relative occurrence intensity or relative habitat support than absolute occupancy or abundance. Opportunistic records also mix ecological intensity with observer sampling bias; multi-species point-process models can partly separate these when a shared bias process is estimable (Fithian et al. 2015, Methods in Ecology and Evolution, doi:10.1111/2041-210X.12242).

## Recommended primary estimand: directional Bombus opportunity without a scalar pressure index

### Core idea

If all focal *Bombus* species are assumed a priori to exert selection in the **same sign** (pigmented flowers have an attraction advantage relative to white flowers), then the strongest SDM-only comparison does not require estimating how many visits each species makes.

For an environmentally matched local pair of flower cells A and B, compare the five species-specific, spatially cross-fitted *Bombus* support values.

Define A as **strictly Bombus-opportunity dominant** over B when:

```text
support_A,s > support_B,s   for every focal Bombus species s.
```

This is a Pareto-dominance comparison. If every species contributes a non-negative amount to Bombus-mediated visitation opportunity and support is monotone with that species' local availability, then A has greater potential Bombus encounter opportunity than B under **any positive species weighting**. No assumption about equal abundance, equal visitation rates, or quantitative comparability of SDM values among species is required for the direction of the contrast.

The flower prediction is then directional:

```text
pigmented_share_A - pigmented_share_B > 0
```

with conditional intensity as a secondary response.

### Why this is stronger than the present turnover test

The present turnover test asks whether bee-community change and flower-colour change occur together. The dominance design instead tests the biological prediction:

> When all predicted Bombus opportunities increase in the same direction, does pigmentation also increase in that direction?

This directly matches the attraction-selection hypothesis while still respecting the fact that SDMs do not provide visitation counts.

## Primary analysis specification

1. **Rebuild each Bombus SDM with spatial cross-fitting.** A flower cell receives a species prediction only from a model trained without records from its held-out spatial fold.
2. **Control opportunistic-record bias.** Prefer a presence-only point-process / target-group framework that includes observer-effort covariates or a shared multi-species sampling-bias component. The estimand remains relative occurrence intensity, not abundance.
3. **Use species-specific ranks or other monotone support scales only within species.** Dominance is invariant to monotone transformation within each species, so it does not require cross-species calibration.
4. **Construct response-blind local pairs** within the pre-specified 25-km radius and same held-out flower-model fold.
5. **Environmentally match pairs** using the raw abiotic predictors used in the bee SDMs. Use a pre-specified caliper before examining flower colour.
6. **Orient each pair using Bombus only.** If A is higher than B for all five species, orient B -> A. Flower colour must not determine orientation.
7. **Primary response:** directed difference in pigmented share, `share_highBombus - share_lowBombus`.
8. **Primary hypothesis:** the mean/partial directed contrast is positive relative to the 1,000 flower natural-model posterior-predictive maps.
9. **Secondary response:** directed difference in pigmented-only visible intensity.
10. **Do not use mixed-sign pairs in the primary directional test.** Those pairs have ambiguous total Bombus opportunity without species-specific visitation weights and belong in the existing community-composition analysis.

## Useful sensitivity levels

Strict five-species dominance may reduce sample size. Pre-specify a hierarchy rather than choosing whichever is significant:

- **Primary:** 5/5 species agree in direction.
- **Sensitivity 1:** at least 4/5 agree, with the dissenting species difference small relative to its SDM uncertainty.
- **Sensitivity 2:** posterior/ensemble probability of common-direction dominance >= 0.8.
- **Negative control:** mixed-sign pairs where some species increase and others decrease; no simple directional pigmentation prediction is made.

A result that strengthens with agreement among species is more consistent with the shared-attraction hypothesis than a result driven by one species or by arbitrary community weighting.

## Propagate SDM uncertainty as dominance probability

With an ensemble of valid SDM realizations `b = 1,...,B`, define for each pair:

```text
D_AB = proportion of SDM realizations in which A is higher than B for all species.
```

Then either:

- use only pairs with `D_AB >= 0.8` for the primary directional comparison; or
- carry `D_AB` as probabilistic pair orientation in a hierarchical model.

This is preferable to averaging the SDM surfaces first, because averaging can hide uncertainty in which site actually has greater pollinator opportunity.

## Secondary scalar index: Potential Encounter Opportunity (PEO), not pressure

A scalar can still be useful as a secondary analysis, but its name and assumptions must be explicit.

For species `s` at flower site `i`, let `R_is` be the spatially cross-fitted within-species percentile of relative occurrence support. Optionally replace the point value with a foraging-accessible landscape value by kernel-averaging support around the flower site.

A simple agnostic index is:

```text
PEO_i = sum_s w_s R_is
```

where all `w_s >= 0`.

Equal weights may be used only as a transparent reference case. Robustness should be checked over many positive weight vectors (for example Dirichlet draws) and by leave-one-species-out analyses. The scalar is **potential encounter opportunity**, not expected abundance or visitation rate.

If future data provide genuinely calibrated species occupancy probabilities, a saturating "at least one Bombus available" quantity such as `1 - product_s(1-p_is)` could be considered, but this should not be applied to uncalibrated MaxEnt/cloglog values as if they were occupancy probabilities.

## Foraging accessibility can make the proxy more mechanistic

The plant interacts with foraging bees, not with a raster cell. A stronger opportunity surface can therefore integrate surrounding species support with a species-specific or common foraging kernel:

```text
A_is = integral K_s(distance(i,x)) * R_s(x) dx
```

This represents the amount of predicted bee habitat accessible to a flower site under an assumed foraging scale. The same dominance logic can then be applied to `A_is` instead of point-cell support. If species-specific foraging kernels are poorly known, a common kernel plus fixed sensitivity radii is safer than pretending precise species-specific movement parameters are known.

## Why this is analogous to eDNA occupancy rather than eDNA concentration

The preferred interpretation hierarchy is:

```text
eDNA:     latent presence -> detection evidence -> (only with calibration) abundance
Bombus:   latent availability -> SDM support -> (only with calibration) visitation/pressure
```

The current broad-scale study can defend the middle step: relative evidence for local Bombus availability. It cannot identify the last step without direct visitation or abundance data.

Therefore the manuscript should use terms such as:

- `predicted Bombus availability`;
- `potential Bombus encounter opportunity`;
- `Bombus-opportunity contrast`;
- `pollinator-opportunity proxy`.

Avoid:

- `Bombus abundance`;
- `visitation pressure`;
- `selection pressure`;
- `pollination service`;

unless those quantities are independently calibrated.

## Ecological claim enabled by the directional design

If environmentally matched, cross-fitted, high-confidence dominance pairs show higher pigmentation on the Bombus-dominant endpoint, the strongest defensible statement becomes:

> Sites with consistently greater predicted availability across the focal Bombus assemblage also tended to have greater flower pigmentation, beyond the flower natural-model expectation. Because the exposure is occurrence-based rather than visitation-based, this is consistent with the directional prediction of a Bombus-mediated attraction hypothesis but does not itself estimate visitation rate or selection strength.

This is substantially more mechanistic than a turnover-turnover correspondence while remaining honest about what presence-only SDMs can and cannot measure.
