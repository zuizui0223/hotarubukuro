# Main 2 — local sharp flower-colour transition × focal Bombus availability

Date: 2026-08-09

## Manuscript role

This is the **Main 2 directional pollinator analysis** in the current paper.

The broad national analysis has already established the environment-plus-space flower-colour template. Because Bombus SDMs are themselves generated from environmental geography, Main 2 does not add national Bombus surfaces to the same broad regression. It changes the ecological comparison unit to abrupt nearby white-pigmented boundaries, where broad geographical differences are smaller by design and where pollinator-mediated selection is biologically more plausible.

The analysis does not claim to remove all environmental confounding. Its purpose is narrower: avoid treating overlap between two national, environmentally structured maps as a pollinator mechanism.

## Directional ecological hypothesis

If visible pigmentation contributes to attraction/detection by important bumblebee pollinators, the reproductive benefit maintaining a pigmented state should be greater where those pollinators are available. Where focal-pollinator availability declines, that benefit may relax and white flowers may be more easily maintained.

The primary exposure uses the two broadly distributed Bombus taxa directly documented as focal/predominant pollinators in the relevant *Campanula punctata* system:

- *Bombus ardens*;
- *B. diversus*.

Primary support:

`effective_occmax = max(A_ardens, A_diversus)`

where `A_k` is occurrence-referenced predicted habitat support defined in `reproducibility/bombus_occurrence_reference_support_2026-08-09.md`.

Raw-cloglog `max(B. ardens, B. diversus)` support is retained as an exposure-scale sensitivity.

Montane/alpine taxa and five-species community composition are **not part of Main 2**. They are tested separately in Supporting Information as biogeographic/elevation guardrails.

## Local graph

Unit: 1-km flower cell.

For each focal radius:

- link each eligible flower cell to its five nearest eligible flower cells;
- remove duplicated undirected edges;
- require common finite Bombus-support coverage;
- do not restrict edges by broad spatial fold;
- do not apply an environmental caliper.

Radii:

- 5 km — focal strict-local scale;
- 10 km — scale sensitivity;
- 25 km — scale sensitivity.

The scale family is retained because the local analysis was developed after the broad analysis and the manuscript explicitly reports that the focal pattern does not persist at broader local windows.

## Sharp colour transition

For each local edge, let `dY = pigment_share_j - pigment_share_i`.

Primary transition:

`abs(dY) = 1`

meaning one observed 1-km cell is entirely white and the other entirely pigmented in the available sample.

Sensitivity thresholds:

- `abs(dY) >= 0.75`;
- `abs(dY) >= 0.50`.

These are observed colour-state contrasts, not exact population morph frequencies.

## Bombus-blind, sign-blind pair selection

Inference uses a greedy non-overlapping transition set so no flower cell appears in more than one tested pair.

Pair selection does not use Bombus values or the white-to-pigmented direction:

1. larger absolute observed colour difference first;
2. shorter geographical distance next;
3. stable site-ID tie-break.

Only after the pair set is frozen is each pair oriented from white to pigmented.

## Directional statistic

For exposure `A`, define:

`dA = A_pigmented - A_white`.

Primary statistic: mean `dA` across non-overlapping pure-transition pairs.

Also report:

- median `dA`;
- proportion of pairs with `dA > 0`;
- all-edge descriptive summaries.

These diagnostics are required because a positive mean can be produced by a minority of large contrasts.

## Randomisation null

The selected pair identities and `abs(dA)` values are held fixed. Under no directional alignment between colour orientation and local focal-pollinator contrast, the sign of each pair's Bombus difference is exchangeable.

Use independent sign flips with:

- 100,000 Monte Carlo replicates;
- seed 20260808;
- one-sided alternative `mean(dA) > 0`.

The null tests directional alignment only. It does not remove the environmental basis of the SDMs.

## Environmental-similarity diagnostic

Using the predefined four-dimensional environmental-PC representation:

- `broad50km_pc1`;
- `broad50km_pc2`;
- `within50km_pc1`;
- `within50km_pc2`;

report median environmental distance among selected transition pairs and among all local graph edges.

Environment is not used to select, orient or statistically adjust pairs in Main 2. This comparison is a descriptive check that the focal transitions occur within local environmental contexts rather than across unusually divergent environments.

## Current implementation

- `R/local_pair_graph.R`
- `scripts/run_bombus_local_sharp_transition.R`
- `.github/workflows/bombus-local-sharp-transition.yml`

## Frozen manuscript result

- workflow run: `31263324505`;
- artifact: `9023416810`;
- artifact SHA-256: `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`.

## Claim ceiling

A positive local contrast supports only directional correspondence between observed sharp white-pigmented boundaries and predicted availability of the documented focal Bombus pollinators.

It does **not** establish actual Bombus occurrence, abundance, visitation, colour preference, pollen transfer, reproductive fitness or pollinator-mediated selection. SDM-derived support remains environmentally based, and unmeasured microenvironment or shared population history may remain.
