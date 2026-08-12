# Journal of Biogeography — Supporting Information architecture

The Supporting Information is the evidence reserve for the ecological argument. It documents how far each step can support a hypothesis about flower-colour differentiation and maintenance, where macroecological resolution stops, and which alternative explanations remain open. It should constrain the Main story without turning it into an inventory of model development.

## Appendix S1 — YAMAP sampling frame and public-database benchmark

**Ecological role:** establish the observation process that made a contemporary national map of intraspecific flower-colour variation possible, while quantifying where that sampling frame is strong and where it is incomplete.

Content:

- retrieval frame and source-row audit;
- author candidate-screening protocol;
- matched 2023–2025 YAMAP/iNaturalist/GBIF benchmark;
- annual record/photo counts and provider overlap;
- observation-process/data-quality comparison;
- image-hash and photo-coordinate audit;
- explicit statement that mountain-route sampling enriches mountain/semi-natural coverage but does not prove wild provenance or areal randomness.

**Payoff for Main:** the hiking-photo stream expands the measurable geography of the polymorphism, but route/access/conspicuousness bias remains part of every later inference.

Primary sources:

- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md`;
- `reproducibility/yamap_public_database_benchmark_results_2026-08-09.md`.

## Appendix S2 — Image phenotyping and two-part colour response

**Ecological role:** show that pigmentation state and visible intensity after pigmentation are reproducibly defined before any environmental, Bombus or human information enters, so their later ecological divergence is not created by predictor-informed classification.

Content:

- source-to-phenotype record flow and exact-hash duplicate semantics;
- petal-region/pixel-summary method and fixed extraction settings;
- sRGB-to-CIELAB transformation and optical inference ceiling;
- image-QC diagnostics;
- selected four-component univariate a* mixture and a*=4.968780 boundary;
- 124 ambiguity flags;
- classification sensitivities;
- exact pigmentation-state and pigmented-only intensity definitions.

**Payoff for Main:** whether pigmentation is expressed and how strongly it is expressed once present can be treated as distinct ecological layers, without claiming direct pigment chemistry or pollinator perception from photographs.

Primary source:

- `submission/jbi/supporting/Appendix_S2_image_phenotyping.md`.

## Appendix S3 — Broad environmental and spatial model

**Ecological role:** define the macroecological candidate adaptive landscape and the unresolved regional geography against which finer-scale hypotheses are asked.

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

**Payoff for Main:** robust geographical association can identify candidate environmental contexts for adaptive or plastic responses, while the SPDE field identifies where population-genomic or unmeasured-environment explanations remain unresolved. Neither component alone proves local adaptation or demographic history.

Primary source:

- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`.

## Appendix S4 — Bumblebee SDMs and occurrence-referenced exposure

**Ecological role:** define a reproducible proxy for focal-pollinator opportunity and make explicit why it is one resolution step above environment-only maps but one step below realized visitation or selection.

Content:

- five-species occurrence flow and common mainland domain;
- genus-wide target-group background and one-record-per-predictor-cell rule;
- shared response-blind predictive VIF screen;
- fixed maxnet/ENMeval tuning and selected-model diagnostics;
- rebuild comparison used for source/model validation;
- exact occurrence-referenced empirical-CDF transformation;
- rationale for *B. ardens* + *B. diversus* as the signed directional exposure;
- explicit statement that SDM habitat support is not abundance, visitation, pollen deposition or selection pressure.

**Payoff for Main:** the SDMs generate a spatially explicit opportunity hypothesis that can be tested at independent flower-colour boundaries; direct visitation and fitness remain the next mechanistic resolution.

Primary source:

- `submission/jbi/supporting/Appendix_S4_bombus_sdm_occurrence_support.md`.

## Appendix S5 — Local focal-pollinator robustness and biogeographic guardrails

**Ecological role:** determine whether the weak local Bombus correspondence is compatible with a pigmentation-state maintenance hypothesis and distinguish that possibility from visually attractive but confounded biogeographic overlap.

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

Community turnover remains an **unsigned biogeographic boundary correspondence**. The focal 5-km mean remains a **weak, highly local, magnitude-driven correspondence**, not evidence of pollinator-mediated selection. The montane/elevation result is a biological negative control showing how shared habitat geography can imitate a pollinator mechanism.

**Payoff for Main:** if a Bombus contribution exists, the present evidence fits local maintenance/loss of pigmentation state better than regional darkening; direct visitation, pollen transfer and fitness are required to test selection.

Primary source:

- `submission/jbi/supporting/Appendix_S5_local_pollinator_robustness.md`.

## Appendix S6 — Local departures and post-selection human context

**Ecological role:** distinguish naturally generated local trait discordance from candidate contemporary overlays, and convert apparently anomalous populations into reproducibly selected field/provenance targets.

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

**Payoff for Main:** local colour discordance is not automatically evidence for an extra process. The weak settlement association instead identifies a contemporary provenance hypothesis that can be tested with field history, microenvironment and genomic assignment.

Primary source:

- `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`.

## Supplementary figure/table production

Supplementary panels should be generated from the same checksum-locked evidence cited by the corresponding Appendix. High-value panels include:

- S2 phenotype/mixture/QC panels;
- S3 final coefficient, interaction-response and spatial/predictive-calibration panels;
- S4 selected SDM and occurrence-reference calibration panels;
- S5 focal-scale/exposure, final-eight-axis balance, community-match and equal-elevation guardrail panels;
- S6 observed-versus-natural-map event distribution, 16-candidate map/local examples, human maxT and observation-process panels.

A panel belongs in Supporting Information only when it supports, constrains or locates the next test of a current ecological claim.

## Main/Supp editorial rule

A Supporting result enters Main only when it materially changes biological interpretation. The key payoffs are:

1. state and intensity are reproducibly distinct response layers before ecological predictors enter;
2. the focal Bombus effect is weak/local and the visually strong montane overlap disappears under near-equal-elevation comparison;
3. final-eight-axis balance shows that the fixed Bombus transitions are environmentally closer than local edges without using environment to choose the pairs; and
4. the 16 local departures are not excessive under natural predictive maps, while the leading 5-km settlement signal remains just above the global familywise threshold.

Together, S1–S6 define the resolution ladder of the paper: **observation process -> visible phenotype -> macroenvironment/spatial geography -> predicted pollinator opportunity -> local boundary correspondence -> contemporary provenance targets**.
