# Occurrence-referenced Bombus support used by the current paper

Date: 2026-08-09

## Manuscript role

This note defines the **support transformation** used by Main 2. It is not itself a directional flower-colour test.

The current paper asks whether abrupt nearby white-pigmented transitions align with availability of the two broadly distributed Bombus taxa directly documented as focal/predominant pollinators in the *Campanula punctata* system:

- *Bombus ardens*;
- *B. diversus*.

The primary exposure is:

`effective_occmax = max(A_ardens, A_diversus)`.

## Occurrence-referenced calibration

For each Bombus species k:

1. use the selected seeded ENMeval/maxnet SDM;
2. recover the exact occurrence cells stored in the saved ENMevaluation object;
3. predict selected-model cloglog support at those occurrence cells;
4. define `F_occ,k` as the empirical CDF of those occurrence-cell predictions;
5. transform the flower-cell raw SDM prediction `s_k(x)` to:

`A_k(x) = F_occ,k(s_k(x))`.

`A_k` therefore reports how the flower-cell prediction ranks relative to support values at observed occurrence cells for that species. It is a monotone occurrence-referenced support score, **not** occurrence probability, abundance, visitation, pollen transfer, attraction or selection pressure.

## Why the primary guild contains two species

The primary quantity is availability of the documented broad focal pollinators, not availability of any Bombus species. The montane/alpine taxa *B. beaticola*, *B. consobrinus* and *B. honshuensis* are strongly embedded in elevational niche replacement. Pooling them into a single primary maximum would mix focal-pollinator availability with broad Bombus biogeographic turnover and can make some-species support high almost everywhere.

Those three species remain available for Supporting Information community-turnover and near-equal-elevation guardrails.

## Frozen manuscript source

The current manuscript uses the successfully generated support artifact:

- workflow run: `31262211605`;
- artifact: `9023137743`;
- artifact SHA-256: `d04c561b09b652591b9b479f6e26a779bb562c7c1b5f9b14e61d5e7ca8e2794b`;
- manuscript input table within the artifact: `results/ecological_v17_bombus_effective_availability_refined/cell_effective_bombus_support.csv`.

The artifact was originally produced during a broader exploratory refinement. Only the occurrence-reference calibration/support table is used by the current Main 2 analysis. The superseded 10/25/50-km environment+SPDE directional analysis and its interpretation have been moved to `legacy/`.

## Current builder

The manuscript-facing support builder is:

- `scripts/build_bombus_occurrence_reference_support.R`.

It reconstructs the species occurrence-reference calibrations and writes the focal-guild and all-five support summaries without rerunning the superseded directional analysis.

## Claim ceiling

This support layer permits a local directional comparison in Main 2. It does not turn an environment-based SDM into direct pollination data. The causal ceiling remains correspondence with predicted pollinator context until realized visitation, pollen transfer and fitness are measured.
