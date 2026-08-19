# JBI species-distribution-model reporting checklist

This checklist accompanies Appendix S4 and records the model-building decisions used for the five *Bombus* habitat-support surfaces. It is a reporting aid for the current manuscript, not a claim that predicted habitat support equals realized occurrence, abundance, visitation, pollen transfer or selection pressure.

## Study objective and estimand

| Item | Current implementation |
|---|---|
| Modelling purpose | Build comparable environment-derived habitat-support surfaces for five Japanese *Bombus* species selected from the literature-recorded visitor set, then transform support to a within-species occurrence-referenced rank for the local flower-colour analysis. |
| Primary manuscript exposure | `max(A_ardens, A_diversus)`, where `A_k` is the empirical-CDF rank of flower-cell support relative to support at observed occurrence cells for species k. |
| Non-target interpretation | The surfaces are not abundance, occupancy probability, visitation, stigma contact, pollen deposition, colour preference or selection pressure. |

## Species and study domain

| Item | Current implementation |
|---|---|
| Literature candidate source | Nagano et al. (2014) recorded six *Bombus* visitors of *C. punctata*: *B. beaticola*, *B. ardens*, *B. honshuensis*, *B. ussurensis* (reported there as *B. ussuriensis*), *B. diversus* and *B. consobrinus*. This field-recorded set is not claimed to be an exhaustive national visitor list. |
| Modelled species | *Bombus ardens*, *B. diversus*, *B. beaticola*, *B. consobrinus* and *B. honshuensis*. |
| Unmodelled recorded visitor | *B. ussurensis* was omitted from the SDM family before flower-colour results were inspected because public Japanese occurrence support was judged too sparse for a comparable single-species surface. The exact historical screening count is not preserved by the current frozen build, so the manuscript does not claim that this species failed the formal 20-cell fit safeguard. |
| Ecological grouping | *B. ardens* and *B. diversus* are the broad focal-pollinator group; the other three modelled taxa are montane/alpine guardrail taxa. A modelled *B. ussurensis* surface, had one been frozen, would more naturally belong to the highland/community guardrail family than redefine the broad two-species primary exposure. |
| Geographic domain | Common mainland Japan domain: longitude 128.0–144.0°E and latitude 30.0–41.5°N, representing Honshu, Shikoku and Kyushu. |
| Projection/output grid | All predictors are aligned on one longitude/latitude raster grid before fitting and projection. |
| Extrapolation scope | Predictions are restricted to the declared common study domain; clamping is enabled. |

## Occurrence data

| Item | Current implementation |
|---|---|
| Source | GBIF occurrence records retrieved under the frozen acquisition rules for the five modelled taxa. |
| Country/status filters | Japan; occurrence status present; finite coordinates required. |
| Maximum coordinate uncertainty | 10,000 m. |
| Accepted basis of record | HUMAN_OBSERVATION, OBSERVATION, PRESERVED_SPECIMEN, MATERIAL_SAMPLE and MACHINE_OBSERVATION. |
| Duplicate control | Stable key ordering, duplicate occurrence-key removal, then one focal record per predictor cell. |
| Minimum sample rule | A declared species requires at least 20 unique occurrence cells for model fitting. This is a fit-time safeguard for the five-species configuration, not a preserved rule explaining the earlier six-to-five literature-candidate reduction. |
| Species-screen timing | The six-to-five reduction preceded flower-colour contrasts and SDM-colour tests; species were not added or removed according to whether their predicted surface supported the flower-colour hypothesis. |
| Frozen provenance | Canonical occurrence CSVs, row counts and SHA-256 hashes are written by the source-build workflow for the five modelled taxa. The exact pre-build screening count for excluded *B. ussurensis* is not preserved and is reported as a provenance limitation. |

## Background and observation process

| Item | Current implementation |
|---|---|
| Background type | Genus-wide *Bombus* target-group background, not pseudo-absence. |
| Background role | Approximate heterogeneous bumblebee observation effort under the same public-record process. |
| Background filtering | Unique predictor cells within the common mainland domain. |
| Target size | Up to 10,000 target-group cells, with a minimum of 1,000. |
| Random-background fallback | Disabled. |
| Incomplete visitor set | The exposure represents documented focal bumblebee opportunity, not total pollination service. Omitted effective visitors can attenuate correspondence if their variation is conditionally independent of flower colour, but can bias in either direction if their distributions covary systematically with flower-colour geography. |

## Environmental predictors

| Item | Current implementation |
|---|---|
| Climate | CHELSA v2.1 bioclimatic and related predictors. |
| Soil | SoilGrids 2.0 surface-soil predictors. |
| Elevation | WorldClim 2.1 elevation, with slope and roughness derived from the aligned DEM. |
| Declared candidate predictors | BIO4, BIO6, BIO13, climatic moisture index, solar radiation, bulk density, coarse fragments, sand, silt, nitrogen, pH, soil organic carbon, elevation, slope and roughness. |
| Missing-data handling | Model fitting and domain screening use complete predictor cells. |
| Collinearity screen | One response-blind shared VIF screen across the common domain; threshold VIF=10. |
| Shared feature basis | The same retained predictor set is used for all five modelled species. |

## Model fitting and tuning

| Item | Current implementation |
|---|---|
| Algorithm | `maxnet` fitted through `ENMeval`. |
| Partitioning | Spatial block partitions. |
| Feature classes | L, LQ and LQH. |
| Regularization multipliers | 1, 2, 3, 4 and 5. |
| Candidate selection | Minimum finite AICc. |
| Prediction scale | Cloglog. |
| Clamping | Enabled. |
| Species weighting | No AUC weighting. Cross-species AUC is not treated as biological importance. |

## Evaluation and diagnostics

| Item | Current implementation |
|---|---|
| Internal validation | Spatial-block evaluation metrics, selected-setting record, omission diagnostics and AICc are written for each species. |
| Raster audit | Flower-cell and occurrence-cell extractions are audited against the saved prediction rasters. |
| Repeated rebuild | The declared source build is repeated twice under fixed seeds. Selected settings, prediction rasters and hashes are compared. |
| Numerical tolerance | Raster-value comparison tolerance is 1e-12. |
| Thread control | One computational thread; deterministic RNG settings and stage-specific seeds are recorded. |

## Occurrence-referenced transformation

For each species k, the selected cloglog model is evaluated at the exact occurrence cells retained in the saved evaluation object. Let `F_occ,k` be the empirical cumulative distribution function of those occurrence-cell predictions. A flower-cell support value `s_k(x)` is transformed as:

`A_k(x) = F_occ,k(s_k(x))`.

`A_k(x)` is therefore a monotone within-species support rank. It is not an occurrence probability and is not directly comparable to a field estimate of abundance or visitation.

## Manuscript use and sensitivity structure

| Item | Current implementation |
|---|---|
| Directional Main test | Occurrence-referenced maximum of *B. ardens* and *B. diversus* at Bombus-blind white–pigmented local transitions. |
| Why these two | They combine direct natural-history support as important visitors with broad distributions that reduce the specific high-elevation confounding evident for the three modelled montane/alpine taxa. They are not assumed to be environmentally or spatially unconfounded. |
| Raw-scale sensitivity | Maximum raw cloglog support of the same two species. |
| Species-set sensitivities | All-five-modelled maximum and montane/alpine maximum are retained as robustness/guardrail analyses. “All five” does not mean all six literature-recorded visitors or all possible pollinators. |
| Elevation guardrail | Appendix S5 tests the three modelled highland taxa at near-equal elevation; the apparent montane association disappears, supporting the decision not to make high-elevation species replacement part of the primary directional exposure. |
| Community analysis | Five-species Hellinger turnover is unsigned biogeographic correspondence and remains Supporting Information. |
| Causal ceiling | Direct field observations of visitation, pollen transfer and reproductive fitness are required to test pollinator-mediated selection. |

## Reproducibility record

- source-build workflow run: `31249841493`;
- source-build artifact: `9020226937`;
- artifact SHA-256: `d5d639e8e00d1ccc2f887c53fa8041465905b29f6bca1127f816e8c7a649d708`;
- source-build configuration: `config/bombus_sdm.yml`;
- occurrence acquisition: `source_build/fetch_bombus_occurrences.R`;
- canonicalization: `source_build/canonicalize_bombus_occurrences.R`;
- model fitting: `source_build/build_bombus_sdm_mainland.R`;
- occurrence-reference support builder: `scripts/build_bombus_occurrence_reference_support.R`;
- repeated-build comparison: `validation/compare_bombus_sdm_rebuilds.R`.
