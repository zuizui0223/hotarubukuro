# Final scientific result lock — 2026-08-19

This file freezes the scientific hierarchy used by the JBI manuscript. Later repository work may improve packaging, documentation, reproducibility, rendering or submission metadata, but must not silently change these accepted scientific results.

## Analysis population

- 1,922 environment-complete flower observations in 1,305 1-km cells.
- 966 white-like and 956 pigmented observations.
- Cell-level pigmentation state: 674 pigmented and 631 white cells.
- Hotarubukuro and yamahotarubukuro are pooled as *Campanula punctata* sensu lato for the image-based colour analysis because their practical morphological distinction is concentrated in calyx characters not consistently visible in the photographs. Unpublished genetic data are mentioned only as supporting rationale for pooling and are not analysed as a result in this paper.

## Broad environment + continuous space

Final observation-level INLA-SPDE models use the eight retained abiotic axes, East/West structural adjustment and a stationary Matérn field; the conditional-intensity model additionally retains Temperature PC1 × temperature seasonality.

### Pigmentation state

- Temperature PC1 posterior mean: -0.542.
- 95% CrI: -1.033 to -0.049.
- Approximate odds ratio per +1 SD warmer: 0.58.
- Residual spatial range: 132.76 km; 95% CrI 88.78–195.68 km.

Supported-term fixed-space corroboration:

- supported distance: Temperature PC1;
- observed high-minus-low divergence: 0.100608;
- fixed space-only median: 0.048475;
- excess: +0.052133;
- one-sided posterior-predictive P=0.00998.

Interpretation ceiling: a cool-climate-aligned pigmentation-state association and held-out divergence beyond fitted spatial continuity. This does not establish selection, adaptation, genetic differentiation, plasticity or a unique causal mechanism.

### Conditional visible intensity among pigmented flowers

Retained directional terms:

- precipitation PC1: -0.174, 95% CrI -0.323 to -0.024;
- temperature seasonality: +0.207, 95% CrI +0.044 to +0.369;
- topography PC1: -0.134, 95% CrI -0.224 to -0.043;
- Temperature PC1 × temperature seasonality: -0.204, 95% CrI -0.302 to -0.107.

Residual spatial range: 65.72 km; 95% CrI 31.05–132.63 km.

Supported-term fixed-space corroboration:

- unweighted distance across precipitation PC1, temperature seasonality, topography PC1 and Temperature PC1 × temperature seasonality;
- observed high-minus-low divergence: 0.047416;
- fixed space-only median: 0.020897;
- excess: +0.026519;
- one-sided P=0.26347.

Therefore conditional intensity has directional broad environmental structure but not the stronger supported-distance divergence signature beyond fitted spatial continuity.

## Local Bombus result

- 67 non-overlapping pure white-pigmented boundaries within 5 km.
- Median pair separation: 2.0 km.
- Mean focal *B. ardens* + *B. diversus* occurrence-referenced support contrast, pigmented minus white: +0.03590.
- Median contrast: -0.00277.
- Positive pairs: 49.3%.
- One-sided sign-flip P=0.02716.
- 5/10/25-km family q=0.08148.
- Raw SDM support does not reproduce the strict 5-km result.
- The apparent montane/alpine Bombus overlap disappears under near-equal-elevation comparison.

Final interpretation: heterogeneous local correspondence concentrated in a subset of boundaries, not a pervasive Bombus shift and not evidence of pollinator-mediated selection.

The species-selection provenance is fixed as six literature-recorded visitors -> five modelled taxa -> two broad focal taxa for the directional exposure. *B. ussurensis* is acknowledged as a recorded visitor omitted before flower-colour analysis; the historical six-to-five screening count is not reconstructed retrospectively.

## Human-context result

The Main human-context estimand is threshold-free same-colour isolation for all cells.

Pigmented cells, 5-km population exposure:

- raw isolation rho=0.251980;
- natural-map mean=0.132980;
- upper-tail P=0.000200;
- density-corrected relative isolation rho=0.285498;
- natural-map mean=0.153616;
- P=0.000900.

The white-state opposite sign is not retained after density correction. The robust conclusion is an excess positive isolation-population relationship within pigmented occurrences, not reciprocal colour displacement and not proof of horticultural origin.

The earlier 16-cell threshold-event family remains Supporting Information only:

- event count P=0.27897;
- event fraction P=0.12609;
- strongest 5-km population contrast +0.06744;
- global maxT FWER P=0.05479.

These events are reproducible field/provenance targets, not anthropogenic anomalies.

## Paper-level conclusion

The final paper treats flower-colour geography as a cross-scale assembly problem:

`incidental images -> two-part quantitative phenotype -> named environment + continuous space -> supported-term divergence check -> heterogeneous local Bombus boundaries -> continuous pigmented human-context overlay`.

The manuscript must preserve the distinction between directional associations, model-informed corroboration, heterogeneous local correspondence and exploratory human context. Null and guardrail results are part of the final scientific result and must not be removed to strengthen the narrative.

## Accepted artifact provenance

The final supported-term fixed-space result is restored from GitHub Actions artifact `9350975649`, SHA-256 `4d5a1d28b8313cc0fb6c85484d21c6d94535ac7cc0881e83dc7ed02678854f03`. The artifact ZIP preserves its repository-relative `results/broad_supported_term_distance_space_null/` paths, so the canonical paper pipeline merges it at repository root rather than nesting the output directory a second time.

