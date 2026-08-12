# Current-Broad-primary human-context result lock

Date: 2026-08-12

## Decision

The downstream local-departure/human-context analysis is now aligned directly to the finalized Broad pigmentation-state analysis.

Primary chain:

`final eight Broad abiotic axes -> final8 cross-fitted pigmentation-state model + SPDE -> final8 local RMS matching -> 10,000 natural-map event replay -> post-selection global-maxT human context`

The historical four-PC broad/within-neighbourhood matching is retained as a sensitivity only.

## Primary event definition

- geographical radius: 10 km;
- environmental axes: Temperature PC1, precipitation PC1, temperature seasonality, precipitation seasonality, topography PC1, soil PC1, soil PC2, RSDS;
- axes standardized response-blind over analysis cells;
- environmental distance: RMS Euclidean distance across the eight standardized axes;
- caliper: <=1.0;
- minimum eligible neighbours: 3;
- focal event: observed pigmented cell whose eligible neighbours contain no observed pigmented flowers;
- human variables are absent from graph construction and candidate selection;
- East/West is a structural Broad adjustment and is not used as an abiotic matching dimension.

## Natural-map result

The checksum-locked 10,000 predictive draws from the final-eight-axis cross-fitted state model were replayed on the new current-Broad local graph.

- observed candidates: **16**;
- supported cells: **706**;
- mean eligible neighbours: **3.81609**;
- candidate-count null mean: **13.5908**;
- candidate-count 95% null interval: **7–21**;
- candidate-count Monte Carlo P: **0.27897**;
- observed candidate fraction: **0.040712**;
- candidate-fraction null mean: **0.031068**;
- candidate-fraction upper-tail P: **0.12609**.

Decision: locally discordant pigmented events are not excessive under the finalized Broad natural geography.

## Human-context result

All eleven human/natural-context features were evaluated in one global maxT family.

| Feature | observed focal-minus-white contrast | directional/two-sided P | global maxT FWER P |
|---|---:|---:|---:|
| focal population | +0.06336 | 0.04070 | 0.28287 |
| **population 5 km** | **+0.06744** | **0.00800** | **0.05479** |
| population 10 km | +0.06009 | 0.01370 | 0.10809 |
| population 25 km | +0.00583 | 0.33717 | 0.96900 |
| population 50 km | +0.00811 | 0.19008 | 0.84242 |
| DID proximity | +0.05897 | 0.02200 | 0.15948 |
| road proximity | +0.03505 | 0.23818 | 0.89301 |
| built-up fraction | +0.12331 | 0.04580 | 0.30857 |
| forest-human edge | +0.09106 | 0.10139 | 0.56834 |
| forest cover | -0.08058 | 0.28257 | 0.90331 |
| mountainness | -0.05337 | 0.23538 | 0.83442 |

The leading feature is population exposure within 5 km. It is strongly directional before familywise correction but remains just above the confirmatory 0.05 maxT threshold.

Decision: **near-threshold short-scale settlement exposure; not a corrected-significant anthropogenic effect.**

## Observation-process alternatives

Measured candidate-specific effort alternatives were null under the same 10,000-map framework:

- observation-effort rank: two-sided P=0.92411; maxT P=0.96320;
- independent-site-support rank: two-sided P=0.71813; maxT P=0.75642.

This weakens the simple explanation that candidate cells differ because they contain more photographs or more independent YAMAP activities. It does not remove broader route-access/source-inclusion bias.

## Ecological claim ceiling

Supported:

- the local event frequency is compatible with current Broad natural geography;
- short-scale (especially 5-km) settlement exposure is the leading post-selection human-context signal;
- the scale pattern is local rather than a broad 25–50-km population gradient;
- candidate-specific measured observation effort does not explain the signal.

Not supported:

- human causation;
- horticultural provenance;
- planting or escape;
- introgression;
- a general urbanization syndrome.

The 16 cells are provenance/field targets. Horticultural opportunity, fine-scale environmental modification/plasticity and broader observation accessibility remain competing explanations.

## Reproducibility lock

- branch: `agent/human-align-current-broad`;
- PR: #43;
- current-Broad replay workflow run: `31537102360`;
- current-Broad output artifact: `9119306089`;
- artifact digest: `sha256:f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4`;
- final8 10,000-draw source artifact: `9094339466`;
- final8 source digest: `sha256:413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`;
- frozen cell/human input artifact: `9022276431`;
- frozen input SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

Primary implementation:

- `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`;
- `.github/workflows/human-context-highrep-final.yml`;
- `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`.
