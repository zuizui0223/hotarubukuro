# Journal of Biogeography — Supporting Information architecture

The submitted Supporting Information functions as the paper's evidence reserve: it answers measurement, robustness, confounding and provenance questions without turning the Main narrative into an inventory of model development. Historical analyses that no longer constrain a manuscript-facing claim stay under `legacy/` and are not submitted as current Supporting Information.

## Appendix S1 — YAMAP sampling frame and matched public-database benchmark

**Purpose:** establish what the repurposed hiking-photo stream contributes and make its observation process explicit.

Include:

- retrieval frame and source-row audit;
- author candidate-screening protocol;
- matched 2023–2025 YAMAP/iNaturalist/GBIF benchmark;
- annual record/photo counts and provider overlap;
- observation-process/data-quality comparison;
- image-hash and photo-coordinate audit;
- explicit statement that mountain-route sampling enriches mountain/semi-natural coverage but does not prove wild provenance or areal randomness.

Primary current sources:

- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md`;
- `reproducibility/yamap_public_database_benchmark_results_2026-08-09.md`.

## Appendix S2 — Image phenotyping and two-part colour response

**Purpose:** demonstrate that the trait is reproducibly constructed before geography, Bombus or human variables enter.

Current content:

- source-to-phenotype record flow and exact-hash duplicate semantics;
- petal-region/pixel-summary method and fixed extraction settings;
- sRGB-to-CIELAB transformation and optical inference ceiling;
- image-QC diagnostics and their response-independent role;
- selected four-component univariate a* mixture and a*=4.968780 boundary;
- 124 retained ambiguity flags;
- classification sensitivities;
- exact pigmentation-state and pigmented-only intensity definitions.

Primary source:

- `submission/jbi/supporting/Appendix_S2_image_phenotyping.md`.

Any extra mixture/QC panel included at submission must be generated from the locked phenotype outputs; unsupported retrospective model-selection tables should not be invented.

## Appendix S3 — Final Broad environmental and spatial model

**Purpose:** document Main 1 and the natural-state handoff to Main 3 without overloading the Main text.

Current content:

- the finalized eight measured abiotic axes and response-blind compression;
- observation-level INLA-SPDE specification, mesh and PC priors;
- East/West as a structural geographical adjustment rather than a process axis;
- graded VIF policy and why VIF<5 is not used as a mechanical deletion rule;
- ten mechanism-prioritized interactions plus the all-28 guardrail;
- additive final pigmentation-state model;
- retained Temperature PC1 × temperature-seasonality interaction for conditional intensity;
- hydroclimate/VPD/SWB completeness checks;
- stationary/no-region/coastline-barrier spatial adjudication;
- complete final fixed effects and spatial hyperparameters;
- explicit distinction between measured environmental effects and unresolved residual geography;
- **current Main-3 handoff:** same eight measured abiotic axes, five geographical folds, cross-fitted SPDE natural reference and 10,000 locked predictive maps.

The historical 50-km broad/within four-PC natural reference is not the submitted primary. Its 17-candidate result is sensitivity/provenance only and remains in Appendix S6 as a short comparison, with full implementation under `legacy/`.

Primary source:

- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`.

## Appendix S4 — Bumblebee SDMs and occurrence-referenced exposure

**Purpose:** make the local exposure reproducible and bound its biological interpretation.

Current content:

- frozen five-species occurrence flow and common mainland domain;
- genus-wide target-group background and one-record-per-predictor-cell rule;
- shared response-blind predictive VIF screen;
- fixed maxnet/ENMeval tuning and selected-model diagnostics;
- deterministic rebuild comparison;
- exact occurrence-referenced empirical-CDF transformation;
- why *B. ardens* + *B. diversus* define the signed directional exposure;
- explicit statement that SDM habitat support is not abundance, visitation, pollen deposition or selection pressure.

Primary source:

- `submission/jbi/supporting/Appendix_S4_bombus_sdm_occurrence_support.md`.

## Appendix S5 — Local focal-pollinator robustness and biogeographic guardrails

**Purpose:** test whether the weak Main-2 signal survives alternative exposure/scale definitions and distinguish directional local correspondence from broad Bombus biogeography.

Current content:

- 67 fixed non-overlapping pure transitions at 5 km;
- 5/10/25-km focal availability family and BH correction;
- median and sign-proportion diagnostics showing the magnitude-driven mean;
- 1.0/0.75/0.50 transition-threshold sensitivities;
- raw-cloglog, all-five and montane/alpine exposure sensitivities;
- **final-eight-axis environmental balance diagnostic of the already fixed pairs** plus historical four-PC sensitivity;
- all-edge descriptive checks;
- five-species Hellinger community-boundary correspondence;
- matched-background, block-repetition and leave-one-block-out diagnostics;
- montane/alpine <=50-m and <=100-m equal-elevation negative guardrails.

Community turnover remains an **unsigned biogeographic boundary correspondence**. The focal 5-km mean remains a **weak, highly local, magnitude-driven correspondence**, not evidence of pollinator-mediated selection.

Primary source:

- `submission/jbi/supporting/Appendix_S5_local_pollinator_robustness.md`.

## Appendix S6 — Current-Broad local departures and post-selection human context

**Purpose:** demonstrate that local departure candidates are selected without human information, quantify whether the same event is excessive under the finalized natural geography, and only then characterize human context.

Current primary specification:

- 1-km flower cells;
- pigmented focal cell;
- neighbours within 10 km;
- environmental similarity defined by standardized RMS distance <=1 across the **same eight measured abiotic axes** as the finalized Broad pigmentation-state analysis;
- at least three eligible neighbours;
- all eligible neighbours observed white;
- no human variable in matching, selection or ranking.

Current evidence to include:

- **16 observed candidates**;
- 10,000-map event replay under the locked final-eight-axis cross-fitted state reference;
- candidate-count null mean 13.5908, 95% interval 7–21, P=0.27897;
- candidate-fraction upper-tail P=0.12609;
- global maxT family across the eleven mechanism-based human/natural-alternative features;
- population within 5 km as the leading feature: contrast +0.06744, directional P=0.00800, global maxT FWER P=0.05479;
- observation-effort and independent-site-support alternatives, both null after correction;
- explicit distinction among horticultural/provenance opportunity, unresolved fine-scale environmental modification and broader observation-access bias;
- historical four-PC/17-candidate and support-calibrated definitions only as environmental-representation sensitivities.

The old candidate-DOY helper, old 200,000-draw joint-PPC branch, old fixed 17-candidate support diagnostics and obsolete downstream validators are not part of the current submitted SI. They are preserved under `legacy/reproducibility-development/superseded-current-input-anomaly-pipeline-2026-08-12/` for provenance.

Primary source:

- `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`.

## Figure/table production rule

Supplementary panels should be exported only from the current checksum-locked artifacts cited in the corresponding Appendix. Useful additions include:

- S2 phenotype/mixture/QC panels;
- S3 final coefficient, interaction-response and spatial/predictive-calibration panels;
- S4 selected SDM and occurrence-reference calibration panels;
- S5 focal-scale/exposure, final8 balance, community-match and equal-elevation guardrail panels;
- S6 observed-versus-natural-map event distribution, 16-candidate map/local examples, human maxT and observation-process panels.

A panel is Supporting material only when it supports or constrains a current claim. Development diagnostics with no current inferential role remain in `legacy/`.

## Material that stays in the repository but should not be submitted as current JBI Supporting Information

- superseded 1,909/1,923 manuscript architectures;
- old five-species limitation gates and national Bombus regressions;
- the former current-input orchestration that continued into four-PC matching and 17 candidates;
- old high-rep/current-Broad wrappers that depended on that implementation;
- old candidate-DOY and joint-PPC branches;
- exploratory outputs or one-time debugging artefacts that do not support a current Main/Supp claim.

## Main/Supp editorial rule

A Supporting result enters Main only when it materially changes interpretation. In the current paper, the key guardrail payoffs are:

1. the focal Bombus effect is weak/local and the visually strong montane overlap disappears under near-equal-elevation comparison;
2. final-eight-axis balance shows the fixed Bombus transitions are environmentally closer than local edges without using environment to choose the pairs; and
3. the 16 current-Broad local departures are not excessive under natural predictive maps, while the leading 5-km settlement signal remains just above the global familywise threshold.

The detailed sensitivity grids remain in S3–S6 rather than becoming additional Main stories.
