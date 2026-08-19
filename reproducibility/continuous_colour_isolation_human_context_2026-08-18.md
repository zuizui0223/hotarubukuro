# Continuous colour-isolation human-context analysis

**Status:** post hoc exploratory generalisation, frozen before the natural-map guardrail is executed in this branch.

This analysis was motivated after inspection of the earlier 16-event human-context results. The raw isolation–human correlations and the white/pigmented contrast had already been viewed before this file was written. It is therefore **not preregistered** and must not be described as confirmatory. The purpose of this document is to prevent further outcome-dependent changes while adding sampling-density and natural-geography checks.

## 1. Biological question

For every 1-km cell, define colour state as pigmented when `n_pigmented > 0` and white otherwise. For every cell, define raw same-colour isolation as the Euclidean distance in the frozen `x_km`, `y_km` coordinate system to the nearest other cell with the same colour state.

The main descriptive question is:

> Does a cell's human context change with its distance from the nearest occurrence of the same colour, and is that relationship different between pigmented and white states?

This uses all 1,305 cells and no event radius, environmental caliper, minimum-neighbour rule, candidate threshold or residual cutoff.

## 2. Fixed inputs

- accepted Broad/current cells: artifact `9022276431`, ZIP SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`;
- locked final-eight-axis posterior-predictive presence maps: artifact `9094339466`, ZIP SHA-256 `413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`;
- the existing human-neighbourhood and DID feature tables inside the accepted Broad artifact;
- seed `20260725`;
- 2,000 restricted permutations and all 10,000 natural maps.

## 3. Primary quantities

The primary human feature is rank-transformed population exposure within 5 km.

For the observed map, report:

1. Spearman rho between raw same-colour isolation and 5-km population within pigmented cells;
2. the corresponding rho within white cells;
3. their direct difference, `rho_pigmented - rho_white`.

The same quantities are reported across the focal, 5, 10, 25 and 50-km population ladder. The 5-km result remains primary regardless of which scale is largest.

## 4. Sampling-density sensitivity

Raw nearest-neighbour distance increases where the full flower-cell frame is sparse. Therefore compute:

- nearest distance to any other flower cell;
- fifth-nearest distance to any other flower cell;
- `log(same-colour nearest / any-colour nearest)`;
- `log(same-colour nearest / any-colour fifth-nearest)`.

The first ratio is the fixed primary sampling-density sensitivity. A raw white/pigmented sign reversal that disappears for relative isolation is reported as attenuation, not hidden.

Observation-effort and independent-site-support ranks are negative-control features. They do not replace the geometric density correction.

## 5. Restricted randomisation and fold stability

Two descriptive checks are fixed:

- feature values are permuted within colour-by-geographical-fold strata for within-colour rho tests;
- colour labels are shuffled within the five geographical folds, preserving fold-specific colour counts, and the full isolation geometry is recomputed.

These randomisations do not preserve fine-scale spatial autocorrelation and are not the final causal guardrail. Leave-one-fold-out estimates are reported for every population scale.

## 6. Natural-map guardrail

For each of the 10,000 locked final-eight-axis posterior-predictive maps:

1. convert simulated pigmented counts to the identical state rule, `count > 0`;
2. recompute nearest same-colour distance for every cell;
3. recompute pigmented rho, white rho and their difference for the population ladder;
4. repeat the 5-km statistic using relative isolation.

The primary natural guardrail compares the observed raw 5-km rho difference with the distribution across all nondegenerate natural maps. A count-conditioned sensitivity uses maps with exactly the observed pigmented-cell count when at least 200 exist; otherwise it deterministically retains up to 1,000 maps nearest to the observed count. Selection of this subset does not use any human-context statistic.

The natural maps preserve the measured environmental basis, unresolved spatial continuity, per-cell binomial effort and the fixed sampled-cell frame. They are used here without the former 16-event detector.

## 7. Secondary features

The following are descriptive and family-labelled rather than individually promoted:

- focal population, population at 10, 25 and 50 km;
- DID proximity;
- artificial land, built land, agriculture and forest-human edge;
- mountainness;
- observation effort and independent-site support.

Land-cover relationships that occur in both colours are not treated as colour-specific human evidence.

## 8. Interpretation ceiling

A stronger positive isolation–population relationship in pigmented than white cells indicates a colour-specific human-context overlay on spatial geometry. It does not establish horticultural origin, planting, escape, establishment, plasticity, pollen movement, gene flow or causation by people. The 16 earlier event cells remain field/provenance targets and may be described as extreme members of the continuous isolation distribution, not as the basis of this analysis.
