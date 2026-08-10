# Journal of Biogeography — Supporting Information architecture

The submitted Supporting Information should function as the paper's evidence reserve: it should answer robustness and provenance questions without interrupting the Main narrative. Historical method-development material that no longer informs a manuscript-facing claim should remain in the repository but **not** be submitted as Supporting Information.

## Appendix S1 — YAMAP sampling frame and matched public-database benchmark

**Purpose:** establish what the repurposed hiking-photo stream contributes and make its biases explicit.

Include:

- retrieval frame and source-row audit;
- author candidate-screening protocol;
- YAMAP/iNaturalist/GBIF matched 2023–2025 benchmark;
- Table S1: annual counts, record/photo counts, Research Grade subset and GBIF overlap;
- Table S2: observation-process/data-quality matrix;
- Figure S1: annual matched-period counts;
- image-hash and photo-coordinate audit;
- explicit statement that mountain-route sampling enriches mountain/semi-natural habitat but does not prove wild provenance.

Primary current sources:

- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md`;
- `reproducibility/yamap_public_database_benchmark_results_2026-08-09.md`.

## Appendix S2 — Image phenotyping and two-part colour response

**Purpose:** demonstrate that the trait is reproducibly constructed rather than inherited from platform labels.

Manuscript-ready text and tables:

- `submission/jbi/supporting/Appendix_S2_image_phenotyping.md`.

Included there:

- source-to-phenotype record flow and exact-hash duplicate semantics;
- petal-region/pixel-summary method and fixed extraction settings;
- sRGB-to-CIELAB transformation and optical inference ceiling;
- automated image-QC diagnostics and their non-response-based role;
- selected four-component univariate a* mixture and a*=4.968780 boundary;
- 124 retained ambiguity flags;
- high-confidence, joint-L*a*b* and naive-zero classification diagnostics;
- exact conditional-intensity definition.

Remaining production item before final SI assembly:

- export the final mixture/QC figure panels from the locked phenotype outputs. A complete BIC-candidate table should be added only if it is exported from the fitted model provenance; the current repository supports the selected-model/component table but does not contain a frozen all-candidate BIC table.

## Appendix S3 — Broad environmental and spatial model

**Purpose:** support Main 1 without filling the Main text with model specification.

Manuscript-ready text and tables:

- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`.

Included there:

- environmental source registry and response-blind PCA loadings;
- distinction between observation-level coefficient/range models and the cell-level predictive reference;
- INLA mesh, Matérn PC priors and structural East/West adjustment;
- complete environment-plus-space coefficient tables for pigmentation state and conditional intensity;
- spatial ranges and internal fit diagnostics;
- 50-km broad/within environmental PCs used by the natural predictive reference;
- five-fold blocked prediction, calibration and coverage metrics;
- explicit statement that predictive performance is not variance partitioning and the SPDE field is unresolved geography rather than a single mechanism.

Remaining production item before final SI assembly:

- export the Main/SI coefficient, national prediction and blocked-calibration panels from the checksum-locked outputs. The Appendix text should be updated to the new clean re-execution artifact only after its outputs are verified against the current numerical lock.

## Appendix S4 — Bumblebee SDMs and occurrence-referenced exposure

**Purpose:** make the local exposure reproducible and clarify its inferential ceiling.

Manuscript-ready text and tables:

- `submission/jbi/supporting/Appendix_S4_bombus_sdm_occurrence_support.md`.

Included there:

- frozen five-species occurrence flow and common mainland domain;
- genus-wide target-group background and one-record-per-predictor-cell rule;
- shared response-blind VIF screen and retained predictor set;
- fixed ENMeval/maxnet tuning grid, selected models, AUC and omission diagnostics;
- deterministic two-build comparison with identical selected models and prediction rasters;
- exact occurrence-referenced empirical-CDF transformation;
- why *B. ardens* + *B. diversus* define the directional exposure;
- explicit statement that habitat support is not abundance, visitation, pollen deposition or selection pressure.

Remaining production item before final SI assembly:

- export the five selected SDM maps and occurrence-reference calibration panels from the frozen source-build and support artifacts.

## Appendix S5 — Local pollinator robustness and biogeographic guardrails

**Purpose:** answer “what if the scale, exposure or species set changes?” while keeping the directional Main test readable.

Manuscript-ready text and tables:

- `submission/jbi/supporting/Appendix_S5_local_pollinator_robustness.md`.

Included there:

- 5/10/25-km focal availability results;
- transition thresholds 1.0/0.75/0.50;
- raw-cloglog, all-five and montane/alpine exposure sensitivities;
- all-edge descriptive checks and the magnitude-driven focal mean;
- five-species occurrence-referenced and rank-based Hellinger turnover;
- 25/50/100-km background and 10/20/50-control matching families;
- 100-km blocks, shifted blocks and leave-one-block-out summaries;
- montane/alpine <=50-m and <=100-m elevation guardrails;
- explicit conclusion that the visually strong high-elevation overlap does not support an additional montane-pollinator mechanism.

Community turnover remains labelled a **biogeographic boundary correspondence**, not a directional flower-colour mechanism.

Remaining production item before final SI assembly:

- export the scale/exposure, community-match and equal-elevation guardrail panels from the locked local-transition and spatial-replication outputs.

## Appendix S6 — Event-based local departures and human context

**Purpose:** demonstrate that candidate locations were defined independently of human variables and calibrated against the natural model.

Manuscript-ready text and tables:

- `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`.

Included there:

- the primary 10-km geographically close, environmentally similar all-white-neighbour event;
- explicit separation of event membership from predictive q or residual thresholds;
- 10,000 cross-fitted natural-map and 200,000 joint posterior-predictive calibrations;
- fold, scale, neighbour-share and environmental-caliper sensitivities;
- stable identities and support diagnostics for all 17 candidates;
- WorldPop, MLIT land-use/road, DID and natural-alternative feature families;
- maxT familywise correction and context-class composition;
- sampling-effort, environment-balance, day-of-year and pigmented-intensity diagnostics;
- explicit distinction between field/provenance targets and evidence of anthropogenic origin;
- the successful post-cleanup end-to-end reconstruction as an execution verification rather than a replacement numerical lock.

Remaining production item before final SI assembly:

- export the observed-versus-null event distributions, candidate map/local-neighbourhood examples and population/DID context panels from the checksum-locked outputs.

## Material that stays in the repository but should not be submitted as JBI Supporting Information

- superseded 1,909/1,923 manuscript narratives;
- old five-species limitation gates;
- exploratory analyses that do not support a current Main/Supp claim;
- method-development debugging and superseded national Bombus regressions, except where one concise note is needed to document why the final estimand changed.

## Main/Supp editorial rule

A supplementary result should be cited in the Main only when it changes interpretation. The Main manuscript therefore keeps only two guardrail payoffs:

1. montane/alpine Bombus correspondence disappears under near-equal-elevation comparisons; and
2. repeated natural maps show that local pigmented-in-white events are not a robust excess.

All numerical sensitivity grids remain here.
