# Current paper

This directory maps the single active JBI paper and its reproducible evidence chain. The current argument is not a list of predictors. It connects distinct processes at the scale and comparison unit where each is biologically defensible.

## Current scientific sequence

1. **Trait construction:** 1,922 author-screened YAMAP observations are converted into pigmentation state and pigmented-only intensity.
2. **Broad natural geography:** response-specific INLA-SPDE models separate measured environment from coherent residual geography. In the cross-fitted space-only sensitivity, pigmentation-state divergence across environmental difference is 0.106802 versus a spatial-null median of 0.058240, an excess of 0.048562 (upper P=0.03393); conditional intensity shows no excess.
3. **Local Bombus boundary test:** 67 independently fixed white-pigmented boundaries show a mean focal-Bombus contrast of +0.03590 at 5 km. Its heterogeneity and attenuation define a local state-maintenance hypothesis; equal-elevation highland analyses prevent broad overlap from being misread as an independent pollinator mechanism.
4. **Continuous human-context geometry:** all 1,305 cells are used. Among 674 pigmented cells, median nearest-pigmented distance is 3.605551 km. Pigmented isolation correlates with 5-km population exposure (rho=0.251980) more strongly than 10,000 natural maps expect (mean 0.132980; upper P=0.000200). After scaling by local flower-cell spacing, rho=0.285498 versus natural mean 0.153616 (P=0.000900). The raw white negative sign does not survive density correction, so the guarded conclusion is a pigmented-specific human-context overlay, not reciprocal colour displacement.
5. **Event calibration and field targets:** the earlier 16 pigmented-among-white events are not excessive under natural maps (count P=0.27897; fraction P=0.12609). Their 5-km local population contrast is +0.06744 (directional P=0.00800; global maxT P=0.05479). They remain reproducible field/provenance targets, not the foundation of the main human-origin claim.

## Interpretation ceiling

The continuous result does not establish horticultural origin, planting, escape, establishment, phenotypic plasticity, pollen movement, gene flow or human causation. It identifies where human context overlays the spatial geometry of pigmentation and sharpens the populations and measurements needed for field history, voucher and genomic tests.

## Current entry points

- Manuscript: `submission/jbi/JBI_main_manuscript_anonymized.md`
- Supporting human-context details: `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`
- Analysis map: `paper/analysis-map.md`
- Canonical audit: `python run_pipeline.py audit`
- Canonical reproduction: `python run_pipeline.py reproduce`
- Continuous analysis entry: `analysis_sensitivity/run_continuous_colour_isolation_human_context.R`
- Validated result lock: `reproducibility/continuous_colour_isolation_human_context_result_2026-08-18.md`

The continuous analysis is explicitly labelled post hoc. Its sampling-density correction and 10,000-map natural guardrails are fixed and checksum locked.
