# Directional all-species Bombus-opportunity sensitivity: historical design-development result

This analysis is retained as a sensitivity from the design-development sequence. It is **not** the active stage-03 estimand. The active paper now uses the threshold-like Bombus-limitation gate documented in `docs/bombus-limitation-gate-results.md`.

## Question

Could the fixed 1,909 Bombus SDM surfaces support a simple monotonic attraction hypothesis without pretending that SDM suitability is abundance or visitation pressure?

Among nearby environmentally matched cells, a pair was oriented only when **all five focal Bombus species** had higher within-species predicted support at the same endpoint. Under the assumption that every focal taxon contributes a non-negative pigmentation-favouring effect, that endpoint has greater potential Bombus opportunity under any positive species weighting.

The prediction was:

```text
pigmented share at the all-species-higher endpoint
    >
pigmented share at the all-species-lower endpoint
```

The identical directed statistic was recalculated on the 1,000 cross-fitted flower natural-model maps.

## Reproducible run

- workflow: `Bombus directional opportunity`
- Actions run: `31166828465`
- source head: `3e3e63c2dce6ed09e0055b6f06ed0b137ad2cf14`
- artifact SHA-256: `c928c7f13233fee2f334fee06f2581c07273060266c7a3a4c44d88f19542814c`
- flower null maps: 1,000

## Result

| response | edges | observed directed difference | natural-null mean | upper-tail p | BH q across two responses |
|---|---:|---:|---:|---:|---:|
| pigmentation share | 502 | +0.0406 | +0.0111 | 0.144 | 0.288 |
| pigmented-only intensity | 184 | +0.0004 | +0.0246 | 0.570 | 0.570 |

The pigmentation direction was positive but not supported relative to the natural-map reference. Matching sensitivities at environmental thresholds 0.50, 0.75 and 1.00 were also positive but non-significant; a 4-of-5 majority definition did not change that conclusion.

## Why this does not contradict the active limitation hypothesis

This test asks whether flower pigmentation increases monotonically when **all five species increase together**. The active limitation hypothesis asks a different question: whether the attraction benefit of pigmentation is relaxed when **all focal species are poorly available**, versus restored when **at least one plausible focal pollinator is moderately available**.

A threshold-like pollinator limitation process need not produce a linear dose response once bumblebees are already available. The negative result here was one reason to move away from an arbitrary scalar “Bombus pressure” story and toward the biologically interpretable limitation gate.

## Proxy lesson

The result reinforces the distinction between predicted availability and actual visitation. Occurrence-derived habitat support should be used to define defensible latent availability contrasts, not converted into pseudo-abundance. Direct visitation or reproductive-success data remain necessary for attraction-mediated selection.
