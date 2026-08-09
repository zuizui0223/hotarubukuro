# Bombus-relaxation analysis specification (locked before rerun)

Date: 2026-08-08

This specification is written before running the new local Bombus analysis. It is designed to prevent outcome-driven choice of pollinator metric, spatial scale, thresholds, or local comparisons.

## Ecological causal hypothesis

The biological hypothesis is **pollinator-mediated relaxation of selection on floral pigmentation**. In *Campanula punctata*, geographic changes in bumblebee pollinators are already associated with changes in floral morphology and breeding system. If pigmentation contributes to attraction or effective interaction with bumblebees, lower effective bumblebee availability can reduce the pollination benefit that maintains pigmentation. The predicted direction is therefore lower bumblebee availability -> greater probability of white flowers. Pigmented-flower intensity is a hierarchical secondary prediction.

The analysis does **not** assume that pigment production cost is the principal mechanism. The primary mechanism is reduced pollinator-mediated benefit / relaxed selection.

## Causal limitation of an SDM-derived exposure

The five Bombus SDMs are environmental habitat-support models. Let E denote measured abiotic environment, S broad spatial/history structure, B* the SDM-predicted Bombus habitat support, B realized visitation/selection pressure, and C flower colour. The relevant causal structure includes E -> B*, E -> C, S -> E/B/C, and potentially B -> C through visitation and selection. Because B* is largely a function of E, a regression of C on B* while simply adding the same E variables cannot identify a causal Bombus mediation effect. B* is also not abundance, visitation, pollination effectiveness, or selection pressure.

Accordingly, the SDM analysis is an **excess local-alignment test**: does observed local flower-colour change align with predicted Bombus change more strongly than expected under a flower-colour model containing environment + spatial structure alone? This is evidence consistent with the mechanism, not causal identification of the mechanism.

## Anti-circularity rules

1. Flower colour is never used to construct local pairs, select spatial scale, define Bombus exposure, choose species, or set an environmental caliper.
2. The primary spatial radius is fixed at 25 km because it is the active local scale used before the fresh-SDM result. Fixed sensitivities are 10 and 50 km.
3. Pair endpoints must be in the same held-out spatial fold and on the five-species common SDM support.
4. The environmental-similarity caliper is fixed at <= 0.75, inherited from the active local Bombus design. It will not be tuned after looking at results.
5. The flower-colour natural reference is the already fitted cross-fitted environment + SPDE model that excludes Bombus. No new flower model is selected using Bombus results.
6. The same fixed pair graph and statistic are replayed on all natural-model posterior predictive maps. Shared endpoints, local sampling effort, and the spatial graph are therefore represented in the null.
7. No low/high Bombus threshold is selected. The failed all-five max-rank gate is not retuned.
8. Results at 10/25/50 km, both pigmentation stages, and all prespecified Bombus sensitivity metrics are retained regardless of direction.

## Primary directional estimand

Unit: response-blind local 1-km cell pair.

Primary radius: 25 km.

Eligibility: same held-out spatial fold; five-species common SDM support; environmental distance <= 0.75.

Exposure: `bombus_total_habitat_support`, the existing response-blind mean of the five within-species habitat-support ranks. This is used as a continuous guild-support index; it is not interpreted as occurrence probability or abundance.

Each eligible pair is oriented from lower to higher guild support. The primary flower response is the signed difference in pigmented share (higher-support endpoint minus lower-support endpoint). The primary statistic is the standardized partial slope of signed flower-colour difference on absolute guild-support difference, with fixed pair-level adjustment for geographic distance, environmental distance, cross-fitted natural flower-colour difference, natural prediction uncertainty, sampling effort, and mean guild support.

The observed statistic is compared to the same statistic from 1000 cross-fitted environment + SPDE posterior predictive flower maps. The prespecified alternative is positive.

## Required corroborating community-turnover test

At the same 25-km / environmental-caliper pair set, test whether absolute flower-colour turnover increases with the existing five-species Bombus community fingerprint turnover (total habitat support + composition PC1/PC2), using the established local-pair predictive-null implementation.

A strong SDM-based result requires **both** the directional primary test and this community-turnover corroboration to be positive relative to the natural predictive null. This is deliberately conservative.

## Prespecified secondary analyses

- 10-km and 50-km radii using the identical rules.
- Pigmented-only intensity as a hierarchical secondary response.
- Widespread-guild support: mean of *B. ardens* and *B. diversus* within-species ranks.
- Montane-guild support: mean of *B. beaticola*, *B. consobrinus*, and *B. honshuensis* within-species ranks.
- Community composition Hellinger turnover and total-support turnover as sensitivity predictors.

These group analyses are not allowed to replace the all-five primary result after inspection.

## Why the old max-rank gate is not the primary exposure

`max(within-species rank across five species)` asks whether **every** focal species is simultaneously low before a cell is labelled Bombus-limited. Under strong altitudinal species replacement, one species can be high wherever another is low, so the maximum can remain high everywhere. The fresh SDMs produced no cells below the fixed 0.33 gate. Changing that threshold after seeing the result would be circular. The continuous guild-support analysis avoids this threshold failure without changing the data or choosing a new cutoff.

## Island interpretation

The Izu-island bumblebee-absence literature motivates the relaxation mechanism because dispersal/history can change realized pollinator availability independently of local environmental suitability. However, the current reconstructed flower dataset does not provide an adequate independent island presence/absence contrast for a causal island test. Therefore no island causal effect will be estimated from these SDMs. Izu evidence remains ecological motivation and a target for direct field validation.

## Claim ceiling

Even if the tests are positive, the allowable claim is: local flower-colour turnover is more strongly aligned with predicted Bombus habitat/community structure than expected from the fitted abiotic + spatial flower-colour model, consistent with pollinator-mediated relaxation. The analysis cannot show that SDM suitability equals visitation or that Bombus caused the observed flower-colour evolution.
