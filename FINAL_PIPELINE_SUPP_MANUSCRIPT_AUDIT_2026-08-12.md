# Final pipeline, Supporting Information, and manuscript consistency audit

Date: 2026-08-12

## Integrated inferential chain

The manuscript and repository use one directed chain:

1. YAMAP source frame and author screening -> deterministic image phenotyping.
2. Two-part flower-colour phenotype -> pigmentation state plus pigmented-only visible intensity.
3. Broad geography -> response-specific eight-axis environment + continuous spatial structure.
4. Local Bombus test -> sharp white-pigmented pairs selected without Bombus values, followed by signed focal *B. ardens*/*B. diversus* habitat-support contrast.
5. Bombus guardrails -> occurrence-reference/raw-support sensitivity, scale attenuation, five-species community turnover, montane near-equal-elevation control and final-eight-axis environmental-balance diagnostic.
6. Local departures -> final-eight-axis cross-fitted state reference and RMS environmental matching.
7. Human context -> examined only after human-blind event selection, using one global maxT family plus observation-process alternatives.

The paper does not interpret one same-scale omnibus regression as separating environmental, pollinator and human mechanisms.

## Current Main evidence and claim ceilings

### Broad environment + space

- 1,922 environment-complete observations: 966 white-like, 956 pigmented.
- Pigmentation state: additive eight-axis environment + East/West + stationary SPDE.
- Conditional intensity: same structure plus Temperature PC1 × temperature-seasonality.
- State maximum VIF=4.430.
- Intensity maximum VIF=6.340; retained interaction VIF=1.664.
- State spatial range=132.76 km (95% CrI 88.78–195.68).
- Intensity spatial range=65.72 km (95% CrI 31.05–132.63).
- Claim ceiling: environmental/residual geographical organization, not direct physiological causation or a named historical process.

### Local focal Bombus

- 67 non-overlapping pure transitions within 5 km.
- Mean pigmented-minus-white occurrence-referenced focal support contrast +0.03590.
- Median -0.00277; 49.3% positive.
- One-sided P=0.02716; BH q=0.08148 across 5/10/25-km pure tests.
- Raw cloglog 5-km support P=0.26715; mean contrast attenuates at 10/25 km.
- Final-eight-axis environmental distance at 5 km: selected 0.244 vs all local edges 0.318.
- Five-species turnover is unsigned Supporting biogeography.
- Near-equal-elevation montane comparisons do not support a second directional mechanism.
- Claim ceiling: weak, highly local, magnitude-driven correspondence; not pollinator-mediated selection.

### Local departures and human context

- Event definition: pigmented focal cell, >=3 eligible neighbours within 10 km, finalized eight-axis standardized RMS distance <=1, all eligible observed neighbours white.
- Human variables absent from event selection.
- 16 observed candidates.
- Candidate-count P=0.27897 under 10,000 predictive maps.
- Candidate-fraction upper-tail P=0.12609.
- Population within 5 km: contrast +0.06744, directional P=0.00800, global maxT FWER P=0.05479.
- Observation-effort and independent-site-support alternatives are null after maxT.
- Claim ceiling: near-threshold short-scale settlement exposure and field/provenance prioritization; not anthropogenic origin.

## Supporting Information map

- **Appendix S1:** YAMAP/iNaturalist/GBIF sampling-frame benchmark and observation-process limitations.
- **Appendix S2:** image screening, duplicate audit, petal ROI, CIELAB phenotype and white/pigmented boundary.
- **Appendix S3:** Broad predictor rationale, INLA-SPDE specification, VIF policy, interaction/environment/spatial guardrails and final model results.
- **Appendix S4:** five-species Bombus occurrence acquisition, SDM construction and occurrence-reference calibration.
- **Appendix S5:** focal local Bombus robustness, final-eight-axis environmental balance, scale/raw/community-turnover/elevation guardrails.
- **Appendix S6:** final-eight-axis local-event definition, 10,000-map calibration, global maxT human-context family and observation-process alternatives.

Supporting analyses exist to constrain Main claims and must use the same current estimands and numerical identities as Main.

## Cross-file consistency rules

The following identities must agree across Main, Supporting Information, figures, reproducibility locks and submission builders:

- integrated population: **1,922** observations;
- phenotype: pigmentation state + conditional visible intensity;
- Broad state model: additive final-eight-axis environment + East/West + stationary SPDE;
- Broad intensity model: same framework + Temperature PC1 × temperature-seasonality;
- local focal Bombus design: **67** fixed 5-km pure transitions;
- Bombus interpretation: occurrence-referenced focal habitat support, weak/local claim ceiling;
- local-event definition: finalized eight-axis RMS matching within 10 km, >=3 neighbours;
- observed local departures: **16**;
- natural calibration: **10,000** predictive maps;
- human-context leading global result: 5-km population exposure, maxT FWER **0.05479**;
- SDM support is never labelled realized visitation, abundance, pollen transfer or selection;
- image a* is never labelled anthocyanin concentration, spectral reflectance, UV contrast or Bombus colour contrast.

## Current evidence files

Submission boundary:

- `submission/jbi/JBI_main_manuscript_anonymized.md`
- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md` through `Appendix_S6_event_departures_human_context.md`
- `submission/jbi/JBI_main_figure_plan.md`
- `submission/jbi/JBI_main_figure_captions.md`

Primary evidence locks:

- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `reproducibility/bombus_local_sharp_transition_current_spec_2026-08-09.md`
- `reproducibility/bombus_local_sharp_transition_current_results_2026-08-09.md`
- `reproducibility/bombus_spatial_replication_test_spec_2026-08-09.md`
- `reproducibility/bombus_spatial_replication_test_results_2026-08-09.md`
- `reproducibility/current_broad_human_primary_2026-08-12.md`
- `reproducibility/final_integrated_pipeline_2026-08-12.md`

Public reproduction aids:

- `paper/README.md`
- `paper/analysis-map.md`
- `paper/active-file-map.csv`
- `docs/reproduction-guide.md`
- `docs/data-s1-dictionary.md`

## Acceptance condition

A repository state is manuscript-consistent only when:

1. the active-file registry is closed and all registered files exist;
2. current workflows reach the registered executable scripts/validators;
3. Main/SI/figure/submission validators pass;
4. checksum-locked evidence values match the manuscript identities above;
5. generated figures and editable submission files build successfully.
