# Journal of Biogeography — Supporting Information architecture

The Supporting Information is the evidence reserve for the adopted analysis. It documents measurement, robustness, confounding controls and claim ceilings without turning Main into an inventory of models.

## Appendix S1 — YAMAP sampling frame and public-database benchmark

**Purpose:** establish what the repurposed hiking-photo stream contributes and make its observation process explicit.

Content:

- retrieval frame and source-row audit;
- author candidate-screening protocol;
- matched 2023–2025 YAMAP/iNaturalist/GBIF benchmark;
- annual record/photo counts and provider overlap;
- observation-process/data-quality comparison;
- image-hash and photo-coordinate audit;
- explicit statement that mountain-route sampling enriches mountain/semi-natural coverage but does not prove wild provenance or areal randomness.

Primary sources:

- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md`;
- `reproducibility/yamap_public_database_benchmark_results_2026-08-09.md`.

## Appendix S2 — Image phenotyping and two-part colour response

**Purpose:** demonstrate that the phenotype is reproducibly constructed before geography, Bombus or human variables enter.

Content:

- source-to-phenotype record flow and exact-hash duplicate semantics;
- petal-region/pixel-summary method and fixed extraction settings;
- sRGB-to-CIELAB transformation and optical inference ceiling;
- image-QC diagnostics;
- selected four-component univariate a* mixture and a*=4.968780 boundary;
- 124 ambiguity flags;
- classification sensitivities;
- exact pigmentation-state and pigmented-only intensity definitions.

Primary source:

- `submission/jbi/supporting/Appendix_S2_image_phenotyping.md`.

## Appendix S3 — Broad environmental and spatial model

**Purpose:** document Main 1 and the natural-state handoff to the local-departure analysis.

Content:

- finalized eight measured abiotic axes;
- observation-level INLA-SPDE specification, mesh and priors;
- East/West as a structural geographical adjustment;
- graded VIF policy;
- interaction audit and final additive pigmentation-state model;
- retained Temperature PC1 × temperature-seasonality interaction for conditional intensity;
- hydroclimate/VPD/SWB completeness checks;
- stationary/no-region/coastline-barrier spatial adjudication;
- final fixed effects and spatial hyperparameters;
- explicit distinction between measured environmental effects and unresolved residual geography;
- final-eight-axis cross-fitted pigmentation-state natural reference used by Main 3.

Primary source:

- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`.

## Appendix S4 — Bumblebee SDMs and occurrence-referenced exposure

**Purpose:** make the local Bombus exposure reproducible and bound its biological interpretation.

Content:

- five-species occurrence flow and common mainland domain;
- genus-wide target-group background and one-record-per-predictor-cell rule;
- shared response-blind predictive VIF screen;
- fixed maxnet/ENMeval tuning and selected-model diagnostics;
- rebuild comparison used for source/model validation;
- exact occurrence-referenced empirical-CDF transformation;
- rationale for *B. ardens* + *B. diversus* as the signed directional exposure;
- explicit statement that SDM habitat support is not abundance, visitation, pollen deposition or selection pressure.

Primary source:

- `submission/jbi/supporting/Appendix_S4_bombus_sdm_occurrence_support.md`.

## Appendix S5 — Local focal-pollinator robustness and biogeographic guardrails

**Purpose:** determine the claim ceiling of the weak Main-2 result and distinguish directional local correspondence from broader Bombus biogeography.

Content:

- 67 fixed non-overlapping pure transitions at 5 km;
- 5/10/25-km focal availability family and BH correction;
- median and sign-proportion diagnostics;
- transition-threshold sensitivities;
- raw-cloglog, all-five and montane/alpine exposure guardrails;
- final-eight-axis environmental balance diagnostic of the fixed pairs;
- all-edge descriptive checks;
- five-species Hellinger community-boundary correspondence;
- matched-background, block-repetition and leave-one-block-out diagnostics;
- montane/alpine near-equal-elevation negative controls.

Community turnover remains an **unsigned biogeographic boundary correspondence**. The focal 5-km mean remains a **weak, highly local, magnitude-driven correspondence**, not evidence of pollinator-mediated selection.

Primary source:

- `submission/jbi/supporting/Appendix_S5_local_pollinator_robustness.md`.

## Appendix S6 — Local departures and post-selection human context

**Purpose:** demonstrate that local departures are selected without human information, quantify whether the same ecological event is excessive under the finalized natural geography, and then characterize human context.

Primary event specification:

- 1-km flower cells;
- pigmented focal cell;
- neighbours within 10 km;
- standardized RMS environmental distance <=1 across the same eight measured abiotic axes as the Broad pigmentation-state analysis;
- at least three eligible neighbours;
- all eligible observed neighbours white;
- no human variable in matching, selection or ranking.

Evidence:

- **16 observed candidates**;
- 10,000-map event replay under the locked final-eight-axis cross-fitted state reference;
- candidate-count null mean 13.5908, 95% interval 7–21, P=0.27897;
- candidate-fraction upper-tail P=0.12609;
- global maxT family across eleven mechanism-based human/natural-alternative features;
- population within 5 km: contrast +0.06744, directional P=0.00800, global maxT FWER P=0.05479;
- observation-effort and independent-site-support alternatives, both null after correction;
- explicit distinction among horticultural/provenance opportunity, unresolved fine-scale environmental modification and broader observation-access bias.

Primary source:

- `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`.

## Supplementary figure/table production

Supplementary panels should be generated from the same checksum-locked evidence cited by the corresponding Appendix. High-value panels include:

- S2 phenotype/mixture/QC panels;
- S3 final coefficient, interaction-response and spatial/predictive-calibration panels;
- S4 selected SDM and occurrence-reference calibration panels;
- S5 focal-scale/exposure, final-eight-axis balance, community-match and equal-elevation guardrail panels;
- S6 observed-versus-natural-map event distribution, 16-candidate map/local examples, human maxT and observation-process panels.

A panel belongs in Supporting Information only when it supports or constrains a current manuscript claim.

## Main/Supp editorial rule

A Supporting result enters Main only when it materially changes interpretation. The key guardrail payoffs are:

1. the focal Bombus effect is weak/local and the visually strong montane overlap disappears under near-equal-elevation comparison;
2. final-eight-axis balance shows that the fixed Bombus transitions are environmentally closer than local edges without using environment to choose the pairs; and
3. the 16 local departures are not excessive under natural predictive maps, while the leading 5-km settlement signal remains just above the global familywise threshold.
