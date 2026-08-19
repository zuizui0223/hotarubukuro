# Current paper

This directory maps the active JBI paper and its checksum-locked evidence chain. The paper asks how one within-species flower-colour geography is assembled by processes operating at different scales.

## Final scientific sequence

1. **Trait construction:** 1,922 screened YAMAP observations become pigmentation state and pigmented-only intensity.
2. **Broad environment + space:** the final observation-level INLA-SPDE models provide directional coefficients and residual spatial ranges. Pigmentation state is less likely toward warmer Temperature PC1. Conditional intensity retains precipitation, temperature-seasonality, terrain and Temperature × seasonality associations.
3. **Supported-term distance versus fixed space:** no model is rebuilt. Fixed held-out pairs and 500 cached space-only maps show that Temperature PC1 distance orders pigmentation-state divergence beyond spatial continuity (observed 0.100608; null 0.048475; excess 0.052133; P=0.00998). The combined supported intensity-term distance does not (P=0.26347).
4. **Local Bombus boundaries:** 67 fixed pairs give mean +0.03590, median -0.00277, 49.3% positive and q=0.08148. The result is boundary heterogeneity; equal-elevation analyses reject stronger-looking highland overlap as an independent mechanism.
5. **Continuous human-context geometry:** among 674 pigmented cells, raw isolation–5-km population rho=0.251980 exceeds natural mean 0.132980 (P=0.000200). Density-corrected rho=0.285498 exceeds natural mean 0.153616 (P=0.000900). A reciprocal white displacement is not robust.
6. **Supplementary event calibration:** 16 restrictive pigmented-among-white events are compatible with natural maps and remain field/provenance targets rather than the basis of the Main human claim.

## Claim ceiling

The spatial null is unresolved geography, not drift or neutral genetic differentiation. The results do not establish selection, local adaptation, realized pollination, horticultural origin, planting, escape, plasticity or gene flow.

## Entry points

- Manuscript: `submission/jbi/JBI_main_manuscript_anonymized.md`
- Broad details: `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`
- Human details: `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`
- Analysis map: `paper/analysis-map.md`
- Audit: `python run_pipeline.py audit`
- Reproduce: `python run_pipeline.py reproduce`
