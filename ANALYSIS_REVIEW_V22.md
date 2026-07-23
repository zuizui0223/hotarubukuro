# V22 review: local human-settlement context of pigmented isolates

## Reviewer-facing question

After nationwide environment and unresolved spatial structure define the
natural flower-colour baseline, are locally isolated pigmented cells placed
closer to human settlement than their own environmentally similar observed-white
neighbours?

This is a post-selection characterization of local discontinuities. It does not
test horticultural ancestry directly and does not use a fitted residual as a
response.

## Why this design is not circular

The 16 focal cells were fixed by the v20/v21 rule:

- the focal 1-km cell contains at least one pigmented observation;
- at least three sampled cells lie within 10 km;
- neighbours pass a caliper of RMS distance no greater than 1 on the four
  broad- and within-50-km natural-environment PCs;
- every eligible observed neighbour is white.

Population, land use, road access, DID distance, flowering date, and pigmented
intensity do not select focal cells or neighbours. Each human feature is added
only after selection. The same candidate rule and the same focal-minus-white
contrast are recalculated in 1,000 cross-fitted natural predictive maps.

This comparison changes the scale of the question. The national SPDE analysis
asks what forms the broad white/pigmented geography. V21/V22 ask whether a
local colour discontinuity, conditional on measured environmental similarity,
coincides with a local change in human context more often than expected from
the natural model. Local matching reduces broad geographic and environmental
confounding but does not eliminate unmeasured local environment or sampling
bias.

## Added public human-context layer

V22 adds the 2015 MLIT Densely Inhabited District (DID) polygons. Polygons are
rasterized to the same 1-km template as the flower-colour analysis, and distance
is measured to the nearest DID-occupied grid cell.

DID identifies dense settlements, not all villages or rural residential land.
It is therefore used as a sensitivity layer, not as a complete measure of human
activity. The prespecified human-context features are:

1. DID proximity rank;
2. indicators for DID distance no greater than 5 and 10 km;
3. a population-DID alignment score, the mean of 5-km WorldPop rank and DID
   proximity rank;
4. a populated-beyond-DID score, the product of 5-km population rank and DID
   distance rank.

The alignment score is a convergence summary, not an independent predictor.
Its Spearman correlation is 0.967 with 5-km population rank and 0.966 with DID
proximity rank. Individual effects must not be inferred from simultaneous
interpretation of these variables.

## Results

### Local population scale from V21

| Feature | Mean focal minus white neighbours | Raw one-sided p | Within-family maxT p |
|---|---:|---:|---:|
| 1-km-cell population rank | 0.052 | 0.086 | 0.276 |
| 5-km population rank | 0.059 | 0.022 | 0.076 |
| 10-km population rank | 0.048 | 0.044 | 0.163 |
| 25-km population rank | 0.008 | 0.291 | 0.685 |
| 50-km population rank | 0.009 | 0.105 | 0.349 |

The direction is localized to 5–10 km and is not a broad 25–50-km urban-region
gradient. The 5-km result is suggestive after scale-family correction but does
not cross 0.05.

### DID sensitivity from V22

| Feature | Mean focal minus white neighbours | Raw one-sided p | Within-family maxT p |
|---|---:|---:|---:|
| DID proximity rank | 0.049 | 0.056 | 0.161 |
| Within 5 km of DID | 0.055 | 0.233 | 0.635 |
| Within 10 km of DID | 0.096 | 0.087 | 0.332 |
| Population-DID alignment | 0.054 | 0.029 | 0.084 |
| Populated beyond DID | -0.003 | 0.609 | 0.980 |

Five observed candidates fell in the top 10% of response-blind local DID
proximity spikes, compared with a natural-map mean of 1.757 (raw p = 0.030,
maxT p = 0.178). Nine of 16 candidates (56.3%) were within 5 km of DID and in
the upper population quartile, compared with a mean candidate fraction of
31.3% in natural maps (two-sided raw p = 0.082, maxT p = 0.117).

Only one of all 1,307 supported cells met the response-blind
“upper-quartile population but more than 10 km from DID” category. Consequently,
the current occurrence support cannot compare urban-fringe and rural-settlement
routes. The null result for that category is a support limitation, not evidence
against rural human influence.

### Candidate-level convergence

One cell, `cell-1km--108_-147` (135.83186 E, 34.67766 N), jointly met:

- local pigmented-isolate status;
- upper 10% natural predictive tail for unexpected pigmentation
  (q = 0.069);
- top 10% local DID-proximity spike;
- high local population and built-up context from V21.

It was not in the early-flowering tail (q = 0.943) or dark-pigmented tail
(q = 0.194). Thus the cell is a priority for provenance follow-up, but the
available phenotype does not reproduce the proposed early/dark horticultural
syndrome.

## What the analysis supports

The human-activity result is best summarized as a weak but coherent local
settlement-proximity tendency:

- the signal is strongest at 5–10 km, not 25–50 km;
- the independent DID layer points in the same direction;
- the direction appears in focal-versus-own-white-neighbour contrasts and in
  response-blind local spikes;
- no result survives a strict 0.05 family-wise threshold;
- the population-DID composite is highly collinear with its inputs and is not
  independent replication.

The defensible claim is therefore:

> Locally isolated pigmented occurrences tended to occupy more populated,
> DID-proximate settings than their environmentally similar white neighbours,
> but the association was exploratory and did not remain significant after
> family-wise correction. These cells provide prioritized candidates for
> testing human-mediated movement, not evidence of horticultural origin.

## What remains unexplained

This workflow cannot distinguish:

- deliberate planting, garden escape, seed transport, or natural dispersal;
- a cultivar from naturally pigmented ancestry;
- genetic introgression from a phenotypic observation;
- urban influence from all rural human influence, because DID omits small
  settlements;
- human placement from an unmeasured fine-scale habitat feature;
- biological absence from SNS observation absence.

The DID year (2015), flower-observation period, and current land cover are also
not perfectly synchronized. The 1-km rasterization can place a cell near a DID
boundary without identifying the actual observation parcel.

## Manuscript placement

Keep the national hurdle/SPDE results as the confirmatory ecological core. Put
the V21/V22 workflow in a separate exploratory subsection entitled, for
example, “Natural-model deviations and local human-settlement context.”

Do not present each correlated feature as another positive result. Present the
5-km population contrast as the main human-context estimate, DID as an
independent data-layer sensitivity, and the alignment composite only as a
convergence summary. Report corrected p-values in the main text and raw values
in the same table.

The single convergent cell belongs in a candidate table or map for future
sampling. A field/genetic follow-up should record planting history, compare
nearby gardens and wild populations, quantify flowering phenology in a common
environment, and test ancestry or gene flow.

## Reproducible outputs

- `results/ecological_v21_local_human_neighbourhood/`
- `results/ecological_v22_did_human_context/`
- `scripts/run_local_human_neighbourhood_v14.R`
- `scripts/run_did_human_context_v15.R`
- `scripts/validate_did_human_context_v15.R`
- `scripts/audit_did_human_context_v15.R`

`final.Rmd` remains unchanged.
