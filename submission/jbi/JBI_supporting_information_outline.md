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

Primary existing source: `manuscript/supplementary-table-s1-yamap-public-benchmark.md`.

## Appendix S2 — Image phenotyping and two-part colour response

**Purpose:** demonstrate that the trait is reproducibly constructed rather than inherited from platform labels.

Include:

- petal-region validation and pixel-summary method;
- sRGB-to-CIELAB transformation;
- image QC diagnostics;
- mixture candidates/BIC table;
- four-component collapse rule and a*=4.968780 boundary;
- 124 ambiguity-flagged observations;
- sensitivity of broad conclusions to ambiguity handling where available.

## Appendix S3 — Broad environmental and spatial model

**Purpose:** support Main 1 without filling the Main text with model specification.

Include:

- environmental source registry;
- PCA loadings and interpretation;
- INLA mesh, priors and hyperparameters;
- complete coefficient tables for pigmentation state and conditional intensity;
- blocked-fold design;
- full prediction/calibration/coverage metrics;
- clear statement that predictive AUC is not variance partitioning and the SPDE field is not a single historical mechanism.

## Appendix S4 — Bumblebee SDMs and occurrence-referenced exposure

**Purpose:** make the local exposure reproducible and clarify its inferential ceiling.

Include:

- five species, occurrence snapshot and common domain;
- shared predictors and ENMeval/maxnet tuning;
- AUCs and selected-model information;
- exact occurrence-referenced empirical-CDF transformation;
- why *B. ardens* + *B. diversus* define the primary directional exposure;
- explicit statement that support is not abundance, visitation, pollen deposition or selection pressure.

## Appendix S5 — Local pollinator robustness and biogeographic guardrails

**Purpose:** answer “what if the scale/exposure/species set changes?” while keeping the directional Main test readable.

Include:

- 5/10/25-km availability results;
- transition thresholds 1.0/0.75/0.50;
- raw-cloglog sensitivity;
- species-specific and all-five exposure sensitivities;
- five-species Hellinger community turnover and local matched-background tests;
- 100-km blocks, shifted blocks and leave-one-block-out summaries;
- montane/alpine (*B. beaticola*, *B. consobrinus*, *B. honshuensis*) <=50 m and <=100 m elevation guardrails;
- explicit conclusion: the visually strong high-elevation overlap does not support an additional montane-pollinator mechanism.

Community turnover must remain labelled a **biogeographic boundary correspondence**, not a directional flower-colour mechanism.

## Appendix S6 — Event-based local departures and human context

**Purpose:** demonstrate that candidate locations were defined independently of human variables and calibrated against the natural model.

Include:

- primary event definition and threshold sensitivities;
- 10,000 cross-fitted natural-map replays;
- 200,000 joint posterior-predictive maps;
- candidate count/fraction references;
- 17 candidate identities and support diagnostics;
- full WorldPop/DID/land-use/road families;
- maxT familywise correction;
- explicit distinction between field targets and evidence of anthropogenic provenance.

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
