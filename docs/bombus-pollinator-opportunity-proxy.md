# Bombus pollinator-opportunity proxy — current manuscript definition

## Biological question

The paper asks a directional local question, not whether a national SDM coefficient is significant:

> Within the broad environmental–spatial flower-colour template, do abrupt nearby white→pigmented transitions tend to point toward higher predicted availability of the documented broad focal bumblebee pollinators?

The ecological hypothesis is a **pollinator-opportunity / benefit-relaxation hypothesis**. If visible pigmentation contributes to attraction or detection by important bumblebee visitors, the reproductive benefit of maintaining a pigmented state can be greater where those pollinators are available and can relax where their local availability is low. Pigment-production cost is not measured and is not required for the prediction.

## What the SDM values mean

The five Bombus SDMs are environment-derived predictions of relative habitat support. They are not:

- abundance;
- realized visitation to *Campanula punctata*;
- pollen deposition or removal;
- pollination effectiveness;
- selection pressure.

Accordingly, the paper never converts SDM output into a pseudo-abundance or visitation-pressure index.

## Occurrence-referenced calibration

For each Bombus species `k`, let `s_k` be the cloglog support predicted at a flower cell. The manuscript-facing calibration compares that value with the distribution of support at the species' observed occurrence cells:

`A_k = F_occ,k(s_k)`

where `F_occ,k` is the empirical CDF of the selected SDM support evaluated at the exact occurrence-reference cells.

`A_k` therefore means **support relative to the species' own observed-occurrence support distribution**. It is not occurrence probability and does not make raw SDM magnitudes directly comparable as biological probabilities.

## Primary focal-pollinator exposure

The directional Main analysis is deliberately restricted to:

- *Bombus ardens*;
- *Bombus diversus*.

The primary exposure is:

`effective_occmax = max(A_ardens, A_diversus)`

These taxa define the documented broad focal-pollinator availability estimand for *C. punctata*. The point is not that they are the only effective bumblebees everywhere, but that they are the widespread focal pollinators for which direct natural-history support exists across the broad study system and whose use does not simply encode the high-elevation replacement of the Bombus assemblage.

## Why the primary exposure is not an all-five maximum

An earlier exploratory design used the maximum within-species rank across all five taxa and attempted to define low-Bombus cells. Under the fresh SDMs this quantity was structurally high almost everywhere because widespread/lowland and montane/alpine taxa replace one another geographically. The old low gate therefore became non-estimable.

More importantly, an all-five maximum answers a different biological question: whether **some Bombus niche** is well supported after species replacement. It no longer isolates local availability of the documented broad focal pollinators.

The high-elevation taxa *B. beaticola*, *B. consobrinus* and *B. honshuensis* strongly overlap the same elevational geography in which pigmented flowers are common. Their raw national association disappears in near-equal-elevation local comparisons. They are therefore retained as a **guardrail against shared high-elevation biogeography**, not added to the Main availability metric.

## Why the pollinator test changes scale

The national flower-colour model already establishes broad environment plus continuous spatial structure. Bombus SDMs are themselves generated from environmental geography. A same-scale national regression of colour on environment, space and Bombus would therefore give a coefficient whose ecological meaning is hard to separate from the geography that generated the SDM.

The Main test instead changes the comparison unit:

1. aggregate the flower data to 1-km cells;
2. identify pure nearby white–pigmented boundaries without using Bombus values;
3. select non-overlapping local pairs without using transition direction;
4. orient the fixed pair only afterwards from white to pigmented;
5. calculate pigmented-minus-white `effective_occmax`;
6. use sign flips to test the directional mean contrast.

The strict Main scale is 5 km. Selected pairs have median separation about 2 km and are not unusually divergent on the measured environmental-PC summary relative to generic local graph edges.

Changing scale reduces broad geographical confounding by design, but it does not eliminate fine-scale unmeasured environmental, historical or observation-process confounding.

## Current result and claim ceiling

At the strict 5-km pure-transition scale, the mean occurrence-referenced focal-pollinator contrast is positive. However, the median is near zero/slightly negative, fewer than half of pairs are positive, the across-scale correction is not conventionally significant, raw cloglog support does not reproduce the result and the 10/25-km effects attenuate.

The correct interpretation is therefore:

> The sharpest local white–pigmented transitions show weak directional consistency with higher predicted availability of the documented broad focal bumblebee pollinators on the pigmented side. This is compatible with local maintenance/relaxation of a pigmentation benefit, but it is not evidence of pollinator-mediated selection.

No comparable local Bombus relationship is supported for how dark already-pigmented flowers become. If a pollinator contribution exists, the present data are therefore more consistent with **maintenance of a visible pigment state** than with progressive darkening.

## Five-species information belongs to Supporting Information

All five taxa are still ecologically useful for a different question: whether sharp flower-colour boundaries are also boundaries in predicted Bombus assemblage composition. That analysis uses matched five-species Hellinger turnover and is reported as **biogeographic correspondence**, not a directional colour mechanism.

Current manuscript-facing files:

- `scripts/build_bombus_occurrence_reference_support.R`
- `scripts/run_bombus_local_sharp_transition.R`
- `scripts/run_bombus_spatial_replication_test.R`
- `reproducibility/bombus_occurrence_reference_support_2026-08-09.md`
- `reproducibility/bombus_local_sharp_transition_current_spec_2026-08-09.md`
- `reproducibility/bombus_local_sharp_transition_current_results_2026-08-09.md`
- `reproducibility/bombus_spatial_replication_test_spec_2026-08-09.md`
- `paper/README.md`

The superseded all-five limitation-gate development is archived under `legacy/`.
