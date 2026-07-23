# V20 review: environment-similar white-neighbourhood isolates

## Question

Does an observed pigmented flower-colour cell embedded in a locally white,
environmentally similar neighbourhood occur more often than expected under the
cross-fitted natural model? If so, do such local discontinuities share
human-landscape, early-flowering, or unusually dark-colour characteristics?

This is a local-discontinuity analysis. It is not a residual regression and it
does not estimate horticultural origin.

## Primary estimand

The primary event is defined before inspecting human variables:

- focal unit: a pigmented 1-km cell;
- neighbourhood: all sampled cells within 10 km, including cells across
  spatial-fold boundaries;
- environmental caliper: RMS distance no greater than 1 on the four
  standardized broad- and within-50-km natural-environment PCs;
- sampling support: at least three neighbouring cells;
- local event: every eligible neighbouring cell is observed white.

The same event definition is applied to each of 1,000 cross-fitted natural
predictive flower-colour maps. Observed trial counts are held fixed. Within-fold
draws are joint; cross-fold cells form an independent out-of-fold mosaic.
A same-fold-only definition is therefore reported as a fold-boundary
sensitivity analysis.

Population, land use, road access, mountain context, phenology, and pigmented
intensity do not select candidates.

## Natural-model comparison

| Definition | Observed | Null mean | One-sided p | Interpretation |
|---|---:|---:|---:|---|
| Primary 10 km candidate count | 16 | 13.507 | 0.273 | Not excessive |
| Primary 10 km candidate fraction | 0.0448 | 0.0341 | 0.125 | Not excessive |
| Same-fold 10 km candidate count | 16 | 12.813 | 0.203 | Count is stable, membership is not identical |
| 5 km candidate fraction | 0.0585 | 0.0359 | 0.056 | Borderline sensitivity only |
| 25 km, at least 90% white, candidate count | 25 | 12.375 | 0.004 | Scale-specific excess |
| 25 km, at least 90% white, candidate fraction | 0.0449 | 0.0206 | 0.003 | Scale-specific excess |

The primary 10-km event is visually and biologically useful for defining
follow-up sites, but it is not rarer than expected under the natural model.
The 25-km result shows broader pigmented enclaves within mainly white regions,
not proof of very local introduction. It must remain a sensitivity result
because the scale and 90%-white threshold differ from the prespecified primary
event.

Only 2 of the 16 primary candidates also fall in the upper 10% predictive tail
for unexpected pigmentation, and none falls in the upper 5% tail. The joint
10% count is also compatible with the natural maps (p = 0.303).

## Held-out human-landscape diagnosis

Primary candidates were matched to locally non-isolated pigmented controls
using geography, the natural environmental axes, natural pigmentation
expectation, observation effort, spatial fold, and local sampling support.
Human variables were excluded from selection and matching.

Only 8 of 16 candidates could be matched without relaxing these constraints.
This small matched sample is a limitation and is the reason no local random
forest is reported.

The regularized multivariate landscape departure is not supported
(Mahalanobis distance = 5.158, natural-map p = 0.570). The human-reach
composite difference is 0.002 (p = 0.515; maxT FWER p = 0.994). The satoyama
interface difference is -0.029 (p = 0.627; maxT FWER p = 0.999). No individual
or composite human-landscape feature survives the natural-map comparison and
family-wise correction.

## Held-out early and dark facets

Early flowering and pigmented-only colour darkness are expressed as
cross-fitted predictive-tail depths, not fitted residuals. They are evaluated
after local candidates and matched controls are fixed.

- early-tail difference: 0.008, p = 0.514, maxT FWER p = 0.700;
- dark-tail difference: -0.108, p = 0.699, maxT FWER p = 0.811;
- primary candidates in the early 10% tail: 0;
- primary candidates in the dark 10% tail: 0.

Thus the local candidates do not show the proposed convergence of local colour
discontinuity, early flowering, dark pigmentation, and human reach.

## Reviewer-facing inference

Supported:

1. A response-blind, environmentally constrained neighbourhood can identify
   literal pigmented-in-white local discontinuities without treating a fitted
   residual as biological evidence.
2. Applying the identical candidate rule to 1,000 natural predictive maps
   provides a calibrated test of whether the observed pattern exceeds
   environment-plus-space expectations.
3. Broader 25-km pigmented enclaves are more frequent than expected in this
   sensitivity analysis and merit spatially targeted follow-up.
4. The resulting candidate list is useful for field sampling and genetic
   provenance tests.

Not supported:

1. The primary 10-km local event is not globally excessive under the current
   natural model.
2. The candidates are not distinguished by the tested human-landscape
   composite, early flowering, or pigmented-only darkness.
3. These data do not identify horticultural escape, human introduction, or
   genetic pollution.

## Manuscript wording

Use:

> We identified environmentally comparable pigmented-in-white neighbourhoods
> as local discontinuities and evaluated their frequency against replicated
> cross-fitted natural predictive maps. The primary 10-km discontinuity was not
> more frequent than expected, whereas a broader 25-km enclave definition
> showed a scale-specific excess. Neither human-landscape context, early
> flowering, nor pigmented-only darkness consistently distinguished primary
> candidates from matched pigmented controls. These sites therefore represent
> priorities for provenance and genetic follow-up rather than evidence of
> horticultural introduction.

Avoid:

> Isolated pink flowers prove horticultural escape.

## Reproducibility

Primary outputs are under
`results/ecological_v20_local_white_isolates/`. Independent validation passed
15 checks and the claim audit passed 11 checks. `final.Rmd` was not modified
(MD5 `0173d76bef175c6319d43196b1406cc8`).
