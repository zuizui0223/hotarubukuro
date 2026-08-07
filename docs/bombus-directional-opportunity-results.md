# Directional Bombus-opportunity sensitivity: result

## Question

Can the fixed 1,909 Bombus SDM surfaces support the directional attraction hypothesis without pretending that SDM suitability is abundance or visitation pressure?

The exploratory test used a conservative pairwise estimand. Among nearby, environmentally matched cells, a pair was oriented only when **all five focal Bombus species** had higher within-species predicted support at the same endpoint. Under the biological assumption that every focal Bombus taxon contributes a non-negative pigmentation-favouring attraction effect, that endpoint has greater potential Bombus encounter opportunity under any positive species weighting. Flower colour did not determine pair orientation.

The directional prediction was:

```text
pigmented share at the all-species-higher endpoint
    >
pigmented share at the all-species-lower endpoint
```

The identical directed statistic was recalculated on the 1,000 cross-fitted flower natural-model posterior-predictive maps.

## Reproducible run

- workflow: `Bombus directional opportunity`
- Actions run: `31166828465`
- source head: `3e3e63c2dce6ed09e0055b6f06ed0b137ad2cf14`
- artifact: `bombus-directional-opportunity-3e3e63c2dce6ed09e0055b6f06ed0b137ad2cf14`
- artifact SHA-256: `c928c7f13233fee2f334fee06f2581c07273060266c7a3a4c44d88f19542814c`
- frozen flower/SDM input: canonical 1,909 submission artifact
- flower null maps: 1,000

## Primary result

Primary pair definition: 25 km, same held-out fold, environmental-distance threshold <= 0.75, strict 5-of-5 Bombus dominance.

| response | edges | observed directed difference | natural-null mean | upper-tail p | BH q across two responses |
|---|---:|---:|---:|---:|---:|
| pigmentation share | 502 | +0.0406 | +0.0111 | 0.144 | 0.288 |
| pigmented-only intensity | 184 | +0.0004 | +0.0246 | 0.570 | 0.570 |

Thus the directional pigmentation prediction was **not supported** under the fixed archived SDM surfaces.

## Matching sensitivity

The pigmentation-share contrast remained positive at all three environmental matching thresholds, but none exceeded its flower natural-model reference:

| environment threshold | strict 5-of-5 edges | observed pigmentation difference | upper-tail p |
|---|---:|---:|---:|
| 0.50 | 353 | +0.0379 | 0.196 |
| 0.75 | 502 | +0.0406 | 0.144 |
| 1.00 | 582 | +0.0327 | 0.218 |

Relaxing the definition to a 4-of-5 majority also did not produce directional support at the primary threshold (pigmentation-share upper-tail p = 0.215).

## Interpretation

This result is informative because it separates two biological stories that should not be conflated.

1. The stricter community-turnover analysis showed that environmentally controlled **Bombus community-composition turnover** corresponds to flower-colour turnover.
2. The present directional analysis does **not** show that locations with uniformly greater predicted Bombus opportunity across all five species have more pigmented flowers than expected from the flower natural model.

Therefore the current SDM data do not support a broad claim of the form:

> more Bombus opportunity -> stronger attraction-mediated selection -> more pigmentation.

The positive observed pigmentation contrast (+0.041) is directionally compatible with that hypothesis, but the same environmental/spatial flower model frequently generates contrasts of this magnitude on the Bombus-oriented pairs. The signal is therefore insufficient to distinguish a directional Bombus-attraction effect from the fitted natural geography.

## What this says about the proxy problem

The failure is not evidence that actual Bombus visitation has no effect. The test uses occurrence-derived habitat support, not visit counts. A location can have high predicted occurrence support yet low local visitation to Campanula, and the conversion from occurrence support to visitation opportunity can differ among species, landscapes and floral-resource contexts.

This is analogous to the distinction emphasized in eDNA studies between a latent occurrence state and a quantitative abundance/biomass estimate: occurrence evidence can be modelled without assuming that measurement magnitude is proportional to abundance, whereas abundance inference requires additional calibration.

For this project, the current fixed SDMs are therefore more defensible as **predicted availability / community-context layers** than as a scalar visitation-pressure proxy.

## Consequence for the manuscript

The Bombus section should not be reframed as evidence that higher Bombus pressure favours pigmentation. The current strongest SDM-only result remains the local community-composition correspondence, with an explicit causal ceiling.

A manuscript-safe synthesis is:

> Local flower-colour turnover corresponded to predicted Bombus community-composition turnover after conservative environmental control, but a separate directional test did not show greater pigmentation at sites with uniformly higher predicted support across all five Bombus species. The SDM evidence therefore supports a pollinator-community context correspondence, not a demonstrated gradient in Bombus visitation or attraction-mediated selection.

## Better next proxy

If a directional attraction hypothesis remains biologically central, the next SDM-only attempt should improve the latent **availability / encounter** model rather than invent a weighted suitability sum:

1. rebuild species surfaces with spatial cross-fitting and explicit opportunistic-record bias correction;
2. preserve multiple SDM realizations;
3. optionally integrate surrounding habitat with a pre-specified foraging kernel;
4. use dominance probability across SDM realizations rather than one fixed surface; and
5. retain the directed pigmentation contrast against the flower posterior-predictive null.

Even then, the estimand is potential encounter opportunity. Direct visitation or reproductive-success data are required to calibrate it to actual visitation pressure or selection strength.
