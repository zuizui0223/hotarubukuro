# Final pipeline, Supporting Information, and manuscript consistency audit

Date: 2026-08-12
Status: current manuscript-facing audit

## One current inferential chain

The repository should expose one irreversible manuscript chain:

1. YAMAP source retrieval and author screening -> deterministic image phenotyping.
2. Two-part flower-colour phenotype -> pigmentation state plus pigmented-only visible intensity.
3. Broad geography -> response-specific eight-axis environment + continuous spatial structure.
4. Local Bombus test -> Bombus/environment/direction-blind sharp white-pigmented pairs, then signed focal *B. ardens*/*B. diversus* habitat-support contrast.
5. Bombus guardrails -> occurrence-reference/raw-support sensitivity, scale attenuation, five-species community turnover, montane near-equal-elevation negative control, and final-eight-axis environmental-balance audit.
6. Local departures -> current Broad eight-axis cross-fitted state reference and eight-axis RMS environmental matching.
7. Human context -> only after human-blind event selection, using one global maxT family plus observation-process alternatives.

No same-scale national regression is allowed to be narrated as separating environment, Bombus and human effects.

## Current Main claims and ceilings

### Broad

- 1,922 environment-complete observations: 966 white-like, 956 pigmented.
- Pigmentation state retains the additive eight-axis environment + spatial model; no interaction passed the full promotion rule.
- Conditional intensity retains Temperature PC1 x temperature-seasonality.
- Moderate VIF values are diagnostics, not automatic deletion triggers; the retained intensity interaction itself has low VIF.
- The spatial field is unresolved biogeography, not dispersal distance or genetic history.

### Bombus

- Main estimand: occurrence-referenced max support of *B. ardens* and *B. diversus* across 67 non-overlapping pure transitions within 5 km.
- Mean pigmented-minus-white contrast +0.03590; median -0.00277; 49.3% positive; one-sided P=0.02716; BH q=0.08148 across 5/10/25-km pure tests.
- Raw cloglog support is null and 10/25-km effects attenuate.
- Final-eight-axis environmental distance is lower for selected transitions than all local edges (5 km: 0.244 vs 0.318), but this is a descriptive guardrail, not proof that environmental confounding is absent.
- Five-species turnover is Supporting biogeography only because it is unsigned.
- Montane/alpine overlap disappears under near-equal-elevation comparisons and is a negative guardrail.
- Claim ceiling: weak, highly local, magnitude-driven correspondence; not pollinator-mediated selection.

### Local departures and human context

- Primary event definition uses the same eight Broad abiotic axes, RMS <=1 within 10 km, minimum three neighbours, and human-blind selection.
- 16 candidates; count P=0.27897 and candidate-fraction upper-tail P=0.12609 under 10,000 natural maps.
- Population within 5 km is the leading human feature: contrast +0.06744, directional P=0.00800, global maxT FWER P=0.05479.
- Observation-effort and independent-site-support alternatives are null after correction.
- Claim ceiling: near-threshold short-scale settlement exposure and provenance/field targets; not anthropogenic origin, planting, escape or horticultural introgression.

## Supporting Information map

- Appendix S1: YAMAP vs iNaturalist/GBIF benchmark and observation-process limitations.
- Appendix S2: image screening, duplicate audit, petal ROI, CIELAB phenotype and white/pigmented boundary.
- Appendix S3: Broad predictor rationale, public-data completeness, INLA-SPDE specification, VIF policy, interaction audit, VPD/SWB and spatial-structure guardrails, final fixed/hyperparameter results.
- Appendix S4: five-species Bombus SDM construction, occurrence support and calibration limitations.
- Appendix S5: focal local Bombus robustness, final-eight-axis environmental balance, scale/raw/all-five sensitivities, community-turnover biogeography and montane negative guardrail.
- Appendix S6: current-Broad event definition, 10,000-map natural calibration, historical four-PC sensitivity, global maxT human family and observation-process alternatives.

Anything needed to defend a Main claim must be in S1-S6 or in a checksum-locked reproducibility record. Development history does not belong in Supporting Information.

## Manuscript consistency rules

The manuscript must use the following identities consistently:

- 1,922 integrated observations, not older 1,909/1,923 populations.
- Broad state = additive final-eight-axis model.
- Broad conditional intensity = Temperature PC1 x temperature-seasonality interaction retained.
- Bombus Main = two-species focal occurrence-referenced local transition test, not five-species turnover and not all-five limitation-gate designs.
- Bombus pair identities are independent of Broad updates; final-eight-axis distance is a post-selection balance diagnostic only.
- Human primary = 16 current-Broad candidates under final-eight-axis matching, not the historical 17-candidate four-PC primary.
- Human result = near-threshold 5-km settlement exposure (global maxT 0.05479), not a significant anthropogenic effect.
- No SDM surface is called realized visitation, abundance, pollen transfer or selection.
- No image a* value is called anthocyanin concentration, spectral reflectance, UV contrast or Bombus colour contrast.

## Files that are current evidence

Primary manuscript/submission boundary:
- `submission/jbi/JBI_main_manuscript_anonymized.md`
- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md` through `Appendix_S6_event_departures_human_context.md`
- `submission/jbi/JBI_main_figure_plan.md`
- `submission/jbi/JBI_main_figure_captions.md`

Primary analysis locks:
- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `reproducibility/bombus_local_sharp_transition_current_*_2026-08-09.md`
- `reproducibility/bombus_spatial_replication_test_*_2026-08-09.md`
- `reproducibility/current_broad_human_primary_2026-08-12.md`
- `reproducibility/final_integrated_pipeline_2026-08-12.md`

## Legacy policy

A file is moved to `legacy/` when it encodes a superseded primary estimand, superseded analysis population, superseded candidate identity, abandoned method, obsolete manuscript, obsolete figure, or development-only result that can be mistaken for current evidence.

Sensitivity analyses that are still cited by S1-S6 remain outside legacy and must be explicitly labelled sensitivity/guardrail. Historical files may be retained for provenance but must never be registered as `Main*_final` in `paper/active-file-map.csv`.
