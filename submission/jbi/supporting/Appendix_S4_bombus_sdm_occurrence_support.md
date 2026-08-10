# Appendix S4. Bumblebee SDMs and occurrence-referenced habitat-support exposure

## Scope and inference ceiling

The bumblebee layer represents **predicted habitat support** from presence-background species-distribution models (SDMs). It does not measure local abundance, realized occurrence in a particular flowering season, visitation to *Campanula punctata*, stigma contact, pollen deposition, attraction strength or selection pressure. The SDMs are themselves functions of environmental geography; their role in the Main analysis is therefore restricted to a local directional comparison after the broad flower-colour geography has already been established.

Five Japanese *Bombus* species were reconstructed on a common mainland domain:

- *Bombus ardens* and *B. diversus* — broadly distributed taxa directly documented as focal or predominant pollinators in the relevant *C. punctata* system;
- *B. beaticola*, *B. consobrinus* and *B. honshuensis* — montane or alpine taxa retained for community-turnover and elevation guardrails.

The primary directional exposure is not “any bumblebee”. It is the maximum occurrence-referenced support of *B. ardens* and *B. diversus*. Pooling all five taxa into the primary maximum would change the estimand from availability of the documented broad focal pollinators to broad *Bombus* biogeographic replacement.

## Frozen occurrence snapshot

GBIF records were queried for Japan, required finite coordinates and occurrence status `present`, and were restricted to coordinate uncertainty <=10 km where uncertainty was reported. Accepted bases of record were HUMAN_OBSERVATION, OBSERVATION, PRESERVED_SPECIMEN, MATERIAL_SAMPLE and MACHINE_OBSERVATION. Files were sorted by GBIF occurrence key, duplicate keys removed and SHA-256 hashes recorded before model fitting.

**Table S4.1. Occurrence flow for the five focal species.** “Snapshot records” are the frozen filtered Japanese records; “model cells” are records inside the common domain after retaining one record per predictor cell.

| Species | Ecological role used here | Snapshot records | Model occurrence cells |
|---|---|---:|---:|
| *B. ardens* | broad focal pollinator | 2,818 | 848 |
| *B. diversus* | broad focal pollinator | 7,317 | 1,188 |
| *B. beaticola* | alpine guardrail | 939 | 122 |
| *B. consobrinus* | high-montane guardrail | 286 | 44 |
| *B. honshuensis* | montane guardrail | 1,084 | 258 |
| Genus-wide *Bombus* target group | observation-effort background | 19,016 | 2,348 unique predictor cells |

The target-group background comprises predictor cells containing a filtered Japanese *Bombus* record within the common study domain. It is a proxy for heterogeneous bumblebee observation effort, not a pseudo-absence sample and not an estimate of the total area accessible to each species.

## Common mainland domain and environmental inputs

All species were fitted on the same longitude-latitude domain (128.0–144.0 degrees E, 30.0–41.5 degrees N), covering the Honshu–Shikoku–Kyushu region relevant to the flower-colour question. A common domain and a shared predictor screen were used so that broad and montane taxa were not compared after species-specific choices of study extent or predictor set.

Candidate environmental inputs comprised CHELSA v2.1 climate variables, SoilGrids 2.0 surface-soil variables, WorldClim 2.1 elevation and terrain derivatives calculated from elevation. A response-blind variance-inflation screen was applied to a random sample of complete domain cells at VIF=10. Thirteen predictors were retained for every species.

**Table S4.2. Shared retained predictor set.**

| Predictor | Source or derivation | VIF in shared screen |
|---|---|---:|
| BIO4 | CHELSA temperature seasonality | 3.448 |
| BIO13 | CHELSA precipitation of wettest month | 5.190 |
| CMI | CHELSA climatic moisture index | 8.502 |
| RSDS | CHELSA surface downwelling shortwave radiation | 1.672 |
| Bulk density | SoilGrids 0–5 cm | 7.048 |
| Coarse fragments | SoilGrids 0–5 cm | 2.449 |
| Sand | SoilGrids 0–5 cm | 2.832 |
| Silt | SoilGrids 0–5 cm | 2.910 |
| Nitrogen | SoilGrids 0–5 cm | 4.815 |
| Soil pH | SoilGrids 0–5 cm | 4.822 |
| Soil organic carbon | SoilGrids 0–5 cm | 5.305 |
| Elevation | WorldClim 2.1 | 3.458 |
| Slope | derived from elevation | 1.909 |

BIO6 and roughness were available to the source build but were not retained by the shared VIF screen. Predictor retention does not imply that every variable is a causal determinant of *Bombus* occurrence.

## Candidate models and selection

Models were fitted with `ENMeval` and `maxnet` using spatial block partitions. The fixed candidate grid crossed feature classes L, LQ and LQH with regularization multipliers 1–5. The selected model was the candidate with the minimum finite AICc. Predictions were written on the cloglog scale with clamping enabled.

**Table S4.3. Selected models and spatial validation diagnostics.** Validation AUC and 10-percentile omission are descriptive model diagnostics; they are not used as biological weights in the flower-colour exposure.

| Species | Feature class | Regularization multiplier | Occurrence cells | Background cells | Mean validation AUC | Mean 10% omission | Non-zero coefficients |
|---|---:|---:|---:|---:|---:|---:|---:|
| *B. ardens* | LQH | 1 | 848 | 1,500 | 0.756 | 0.163 | 46 |
| *B. diversus* | LQH | 2 | 1,188 | 1,160 | 0.601 | 0.169 | 22 |
| *B. beaticola* | LQ | 5 | 122 | 2,226 | 0.913 | 0.107 | 9 |
| *B. consobrinus* | LQH | 2 | 44 | 2,304 | 0.897 | 0.182 | 12 |
| *B. honshuensis* | LQH | 1 | 258 | 2,090 | 0.870 | 0.118 | 45 |

Across the five species, validation AUC was negatively associated with log occurrence-hull area (Spearman rho=-0.90). This diagnostic illustrates why AUC was not used to weight species: cross-species AUC reflects niche breadth, background contrast and sampling structure as well as predictive discrimination. In particular, high AUC for a narrow montane species must not automatically grant that species greater importance in a focal-pollinator exposure.

## Deterministic source-build verification

The source build fixed:

- base seed 42;
- RNG kind Mersenne-Twister, normal generator Inversion and sampling method Rejection;
- one thread for R, BLAS, OpenMP and GDAL operations;
- deterministic stage- and species-specific seeds;
- sorted and hashed occurrence files;
- exact raster and output manifests.

Two independent builds were executed from the same frozen inputs. Selected-model tables and seed registries were identical, raster geometry matched and the maximum absolute cell-wise prediction difference was 0 for all five species at a declared tolerance of 1e-12.

The frozen source-build evidence is workflow run `31249841493`, artifact `9020226937`, SHA-256 `d5d639e8e00d1ccc2f887c53fa8041465905b29f6bca1127f816e8c7a649d708`.

## Occurrence-referenced calibration

Raw cloglog values are not treated as directly comparable among species. For each species k, the selected model was evaluated at the occurrence cells retained in its saved `ENMevaluation` object. Let `F_occ,k` be the empirical cumulative distribution function of those finite occurrence-cell predictions. A raw flower-cell support value `s_k(x)` was transformed as

`A_k(x) = F_occ,k(s_k(x))`.

Thus, `A_k(x)` is the rank of a flower-cell prediction relative to support values at observed occurrence cells for that same species. A score near 0 means that the flower cell has lower predicted support than most occurrence-reference cells; a score near 1 means that it is near the upper end of that species-specific occurrence reference. The transformation is monotone and does not convert support to occurrence probability.

**Table S4.4. Occurrence-reference distributions and flower-cell score medians.**

| Species | Finite occurrence-reference values | Raw support q10 | q50 | q90 | Median occurrence-referenced flower score |
|---|---:|---:|---:|---:|---:|
| *B. ardens* | 722 | 0.416 | 0.674 | 0.847 | 0.245 |
| *B. diversus* | 1,116 | 0.572 | 0.651 | 0.701 | 0.344 |
| *B. beaticola* | 122 | 0.241 | 0.788 | 0.962 | 0.041 |
| *B. consobrinus* | 44 | 0.362 | 0.748 | 0.982 | 0.000 |
| *B. honshuensis* | 255 | 0.291 | 0.757 | 0.907 | 0.063 |

The primary Main 2 exposure is

`effective_occmax(x) = max(A_ardens(x), A_diversus(x))`.

It represents the possibility that at least one of the two documented broad focal pollinators occurs in a cell with habitat support comparable to its known occurrence environments. It does not assume that the two species are equally abundant, interchangeable visitors or identical selective agents.

For Supporting Information, the same table retains:

- the three montane/alpine occurrence-referenced scores;
- `max(A_beaticola, A_consobrinus, A_honshuensis)` as an elevation guardrail;
- the maximum across all five species as an “any focal *Bombus*” sensitivity;
- five-species relative composition for community-turnover analysis;
- raw cloglog maxima as an exposure-scale sensitivity.

## Why two species define the directional exposure

The directional hypothesis concerns possible relaxation of the reproductive benefit of maintaining visible pigmentation where the important broad pollinators are less available. *B. ardens* and *B. diversus* are the broadly distributed species with direct natural-history support as focal or predominant pollinators in the relevant system. The other three species have narrower high-elevation distributions that overlap the national geography of pigmented highland flowers. Including them in the primary maximum would make some-species support high through species replacement and would re-import the elevation pattern already addressed by the broad model.

The all-five and montane alternatives are therefore retained as explicit tests of whether the focal result depends on species-set choice, not as additional primary mechanisms. Appendix S5 shows that the all-five maximum does not strengthen the strict 5-km result into robust evidence and that the montane association disappears under near-equal-elevation comparisons.

## Reproducibility resources

Current source-build and calibration files include:

- `config/bombus_sdm.yml` — frozen domain, occurrence, background, predictor, tuning and reproducibility rules;
- `source_build/fetch_bombus_occurrences.R` — public occurrence acquisition;
- `source_build/canonicalize_bombus_occurrences.R` — sorting, deduplication and hashing;
- `source_build/build_bombus_sdm_mainland.R` — common-domain ENMeval/maxnet build;
- `validation/compare_bombus_sdm_rebuilds.R` — independent repeated-build comparison;
- `scripts/build_bombus_occurrence_reference_support.R` — occurrence-reference transformation and flower-cell support table;
- `reproducibility/bombus_occurrence_reference_support_2026-08-09.md` — manuscript-facing support lock.

The frozen occurrence-reference artifact is workflow run `31262211605`, artifact `9023137743`, SHA-256 `d04c561b09b652591b9b479f6e26a779bb562c7c1b5f9b14e61d5e7ca8e2794b`.

The inferential ceiling remains local correspondence with an environment-derived pollinator context. Realized species presence, visit rates, flower contact, pollen transfer and reproductive fitness require direct field measurements.
