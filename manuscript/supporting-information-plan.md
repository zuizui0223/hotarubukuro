# Supporting Information plan for the final 1,922-observation analysis

The Supporting Information should preserve all analyses and provenance needed to keep the Main story narrow, ecological and interpretable.

## S1. YAMAP / iEcology sampling provenance, public-database benchmark and phenotype validation

- description of YAMAP as a hiking/navigation/activity platform rather than a purpose-built biodiversity database;
- activity/route/photo spatial-provenance rules used by the study;
- treatment of hidden or unusable locations;
- source-row audit from 1,965 records to the final analysis population;
- exhaustive author screening of recovered candidate records within the predefined 2023-2025 retrieval frame, including removal of taxonomic look-alikes/incorrect subjects and confirmation of the focal flower/petal region;
- image-hash duplicate and photo-coordinate audit;
- **matched-period data-volume benchmark** for *Campanula punctata* in Japan, 2023-2025: YAMAP versus iNaturalist and GBIF under fixed image+georeference filters;
- report observations and attached/unique image objects separately rather than treating them as the same unit;
- annual-count comparison to document temporal balance across 2023, 2024 and 2025;
- iNaturalist Research Grade, geoprivacy and reported positional-accuracy summaries;
- GBIF dataset/provider-overlap audit, explicitly showing that GBIF and iNaturalist are not independent additive photo pools in this focal comparison;
- data-quality matrix separating identification, date/coordinate provenance, study-specific trait readiness and observation-process bias rather than assigning a single platform "quality score";
- explicit sampling-frame caveat: route/access, flower conspicuousness and subject-selection bias remain after author review;
- distinction between taxonomic/image-validation error and observer/sampling bias;
- mountain-route interpretation: YAMAP enriches natural/semi-natural mountain sampling for the focal wild herb but does not guarantee that every record is wild;
- colour-extraction diagnostics from validated source photographs to `Data_S1`;
- full Gaussian-mixture model/BIC table, component-collapse rule and classification-confidence diagnostics.

Benchmark lock and results:

- `reproducibility/yamap_public_database_benchmark_spec_2026-08-09.md`;
- `reproducibility/yamap_public_database_benchmark_results_2026-08-09.md`;
- retrieval workflow run `31289927019`, artifact `9031041034`;
- independent GBIF-provider audit run `31290095532`, artifact `9031085975`.

Recommended wording: YAMAP is a **complementary iEcology image stream with an unusually productive mountain sampling frame for this focal species**, not an unbiased or universally superior substitute for iNaturalist/GBIF. Any claim that non-research use reduces bias should be restricted to possible taxon-reporting/research-purpose-conditioned selection; general observer bias remains.

## S2. Broad environmental and spatial model

- environmental PCA loadings and source registry;
- INLA mesh/prior rationale, full coefficient tables and hyperparameters;
- blocked-fold performance, calibration and predictive-coverage tables;
- explicit explanation that predictive AUC is not variance decomposition;
- two-part phenotype results kept separate for pigmentation state and pigmented-only intensity.

## S3. Fresh Bombus SDMs and exposure calibration

- occurrence snapshots/background definition and ENMeval/maxnet selection settings;
- species AUCs and selected models;
- occurrence-referenced empirical-CDF calibration details;
- explicit statement that occurrence-referenced support is not occurrence probability, abundance or visitation;
- explanation of why *B. ardens* + *B. diversus* define the primary broad focal-pollinator exposure;
- explicit warning that adding montane/alpine taxa changes the estimand toward geographical niche replacement.

## S4. Availability robustness family

- 5/10/25-km pure-transition results;
- transition thresholds 1.0/0.75/0.50;
- raw-cloglog effective support;
- all-five occurrence-referenced max/mean sensitivities;
- species-specific *ardens* and *diversus* sensitivities;
- all-edge descriptive summaries;
- one-to-one/non-overlap sensitivity where relevant;
- multiplicity summaries;
- environmental-similarity diagnostic showing that the focal sharp-transition set is not more environmentally divergent than generic local edges.

## S5. Community-boundary correspondence

- five-species occurrence-referenced Hellinger turnover;
- within-species-rank Hellinger sensitivity;
- 25/50/100-km matched-background windows and 10/20/50 controls;
- 5/10/25-km scale family;
- 100-km spatial blocks, shifted blocks and leave-one-block-out summaries;
- language restricted to biogeographic correspondence, not a directional flower-colour mechanism.

## S6. Montane/alpine guardrails

- *B. beaticola*, *B. consobrinus* and *B. honshuensis* support summaries;
- equal-elevation (<=50 m and <=100 m) pure-transition comparisons;
- conditional "effective guild not higher" diagnostics;
- explicit conclusion: no additional montane/alpine effect beyond shared elevational geography in the present data;
- use this negative result to explain why the main availability exposure is not an all-five maximum.

## S7. Event-based anomaly calibration

- primary 10-km/env<=1/all-white-neighbour definition;
- 5/25-km and environmental-caliper sensitivities;
- 10,000 held-out natural-map event replay;
- 200,000 joint posterior-predictive event maps;
- candidate identities and local support diagnostics;
- clear distinction between event-based departures and raw residual tails.

## S8. Human-context family and YAMAP sampling-frame sensitivity

- WorldPop scale family;
- DID feature family;
- land-use/road diagnostics where retained;
- raw and maxT familywise P-values;
- candidate DOY check only after candidate selection;
- no horticultural/provenance claim;
- explicit discussion that mountain-route sampling may compress the available urban-rural gradient and therefore reduce power for a broad human-context association;
- countervailing access bias: trailheads, roads and accessible mountain margins may be overrepresented;
- horticultural use of *C. punctata* is motivation for follow-up, not evidence used to select candidates.

## S9. Historical method-development provenance

- earlier environment+SPDE Bombus null;
- superseded five-species lower-third Bombus limitation gate and why it became structurally non-estimable under fresh SDMs;
- archived 1,909/1,923 analyses clearly marked as non-manuscript-facing;
- earlier community-turnover developments retained as provenance rather than promoted because of statistical strength alone.
