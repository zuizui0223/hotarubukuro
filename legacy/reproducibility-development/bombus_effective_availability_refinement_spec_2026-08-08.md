# Effective-Bombus availability refinement (post-null exploratory specification)

Date: 2026-08-08

Machine-readable lock aliases used by the workflow: `post-null hypothesis-refining`; primary implementation key: `effective_occmax`. These aliases only map the already-locked prose specification to code and do not change the scientific design.

## Status and reason for refinement

This analysis is specified **after** the first fresh-SDM directional test returned a non-significant result. It is therefore a hypothesis-refining / exploratory reanalysis, not an independent confirmatory test. The original null result remains reportable and is not replaced.

The refinement is motivated by a mismatch between the ecological hypothesis and the previous exposure. The hypothesis concerns the pollination benefit of pigmentation when an effective bumblebee pollinator is locally available. The previous directional exposure averaged five species-specific within-flower-location support ranks. Such an average can decline when one bumblebee species is replaced by another even though an effective pollinator remains available, and the within-flower rank scale is not anchored to the habitat support observed at Bombus occurrence sites.

## Biological target

The target construct is **potential effective Bombus pollination opportunity**, not Bombus abundance or visitation rate.

The primary guild is restricted to *Bombus diversus* and *B. ardens* because these are the predominant Bombus pollinators directly documented for *Campanula punctata* in the relevant Japanese system (Honshu: *B. diversus*; Izu Oshima: *B. ardens* together with halictids). The three montane/alpine focal species remain prespecified sensitivity analyses; they are not assumed to have the same demonstrated pollination effectiveness for this plant merely because they are Bombus.

The hypothesized direction is:

higher effective-Bombus availability -> larger pollination benefit of a coloured floral signal -> stronger maintenance of pigmentation -> higher pigmented-flower probability.

The mechanism is relaxed benefit of pigmentation at low effective-Bombus availability, not necessarily metabolic cost saving.

## Occurrence-referenced species support

For each focal Bombus species k:

1. Read the exact seeded ENMeval object from the frozen fresh-SDM artifact.
2. Recover the selected full-data maxnet model and the exact one-record-per-predictor-cell occurrence table stored in the ENMevaluation object.
3. Predict cloglog habitat support for those occurrence cells using the selected model.
4. Define the occurrence-referenced support score at flower cell x as the empirical CDF of the occurrence-cell predictions evaluated at the fresh flower-cell raw cloglog prediction s_k(x):

   A_k(x) = F_occ,k(s_k(x)).

This is a monotone species-specific calibration that places scores on a comparable occurrence-referenced 0-1 scale. It is **not** interpreted as occurrence probability. Because the calibration uses the full-data selected model, it is called occurrence-referenced rather than held-out calibrated.

## Primary effective-availability exposure

Primary exposure:

`A_effective = max(A_ardens, A_diversus)`.

The maximum is chosen from the ecological estimand, not from the flower-colour result: if either directly documented effective Bombus pollinator has strong habitat support, the coloured-signal benefit need not be relaxed. A mean would instead penalize species replacement even when one effective pollinator remains well supported.

No low/high threshold will be introduced.

## Local design and anti-circularity

The flower response is never used to define exposure, pair eligibility, spatial scale, or environmental caliper.

Primary local design remains fixed from the previous analysis:

- 1-km flower cells;
- primary radius 25 km; fixed sensitivities 10 and 50 km;
- endpoints in the same held-out spatial fold;
- endpoints on the fresh common Bombus support;
- environmental distance <= 0.75;
- Bombus-free cross-fitted environment + SPDE flower model as the natural reference.

The response-blind pair graph is generated exactly as in the established v17 local analysis. Pair endpoints are then oriented from lower to higher `A_effective`.

## Primary statistic

For every eligible edge e, let dA_e >= 0 be the difference in effective availability and dY_e be pigmented share at the higher-availability endpoint minus pigmented share at the lower-availability endpoint.

The primary statistic is the through-origin local-contrast slope:

`T = sum(dA * dY) / sum(dA^2)`.

This asks whether increasing effective-Bombus contrast is accompanied by increasing pigmentation contrast. It avoids selecting an arbitrary availability threshold and avoids post-hoc regression adjustment by variables that are themselves functions of the same environment.

Environmental and spatial confounding are handled by design plus the predictive null: the identical fixed edges, orientations and statistic are replayed on each of the 1000 already-fitted Bombus-free environment + SPDE posterior-predictive flower maps. The one-sided empirical alternative is `T_observed > T_natural_null`.

A secondary descriptive statistic is the unweighted mean signed pigmentation difference between higher- and lower-availability endpoints.

## Prespecified robustness analyses

All are retained regardless of significance.

Exposure sensitivities:

1. primary occurrence-referenced max of *B. ardens* and *B. diversus*;
2. occurrence-referenced mean of *B. ardens* and *B. diversus*;
3. raw cloglog max of *B. ardens* and *B. diversus*;
4. occurrence-referenced max across all five focal Bombus species;
5. occurrence-referenced mean across all five species;
6. species-specific occurrence-referenced *B. ardens*;
7. species-specific occurrence-referenced *B. diversus*.

Design sensitivities:

- 10, 25 and 50 km radii;
- established all-edge graph (primary);
- a non-overlapping one-to-one edge sensitivity selected response-blind by environmental distance, then geographic distance (no flower colour and no result-based threshold).

Response hierarchy:

- primary: white versus pigmented share;
- secondary: pigmentation intensity among already-pigmented endpoints.

The existing five-species community-turnover result remains a separate corroborating ecological result and is not counted as evidence that rescues the directional availability hypothesis.

## Interpretation rule

Because this refinement follows inspection of a null result, even a small P value is exploratory evidence. A persuasive directional result requires the primary occurrence-referenced effective-guild exposure to be positive relative to the natural null at 25 km and to show the same direction at 10 and 50 km. Consistency with raw-max and/or one-to-one sensitivities strengthens interpretation; failure of these sensitivities must be shown.

The claim ceiling remains: an association stronger than the Bombus-free environment + spatial predictive null is consistent with pollinator-mediated relaxation, but SDM support is not visitation, abundance, pollen transfer or causal selection pressure.