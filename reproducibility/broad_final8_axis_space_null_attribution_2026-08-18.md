# Final-eight-axis Broad spatial-null attribution result

Date: 2026-08-18

## Fixed question

At comparable geographical separation, does difference in the exact final measured environmental axes organize held-out flower-colour divergence beyond an intercept + Matérn SPDE continuity expectation?

This analysis is distinct from the observation-level full environment + SPDE model. The full model estimates directional partial coefficients; this analysis tests unsigned held-out pairwise divergence.

## Fixed design

- source: frozen Broad 1-km cell table from artifact `9022276431`;
- source ZIP SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`;
- responses: pigmentation-state share and conditional-intensity median;
- five response-blind geographical folds;
- five equal-count geographical-distance strata per fold;
- up to 15,000 held-out pairs per fold;
- 500 space-only posterior-predictive realizations;
- seed `20260725`;
- exact final axes: Temperature PC1, precipitation PC1, temperature seasonality, precipitation seasonality, topography PC1, soil PC1, soil PC2 and RSDS;
- axis multiplicity: raw one-sided P, BH q and shared-draw single-step maxT FWER P within response.

## Omnibus final-eight-axis distance

| Response | Observed contrast | Space-null median | Excess | One-sided P |
|---|---:|---:|---:|---:|
| Pigmentation state | 0.0262048 | 0.0266627 | -0.0004578 | 0.5149701 |
| Conditional intensity | 0.0217251 | 0.0247445 | -0.0030195 | 0.5349301 |

Neither response shows an omnibus excess under the exact final-eight-axis distance.

## Pigmentation-state axis family

| Axis | Excess | Raw P | BH q | maxT FWER P |
|---|---:|---:|---:|---:|
| Temperature PC1 | **+0.0521333** | **0.0099800** | 0.0798403 | 0.0778443 |
| Temperature seasonality | +0.0206407 | 0.1057884 | 0.3459747 | 0.4690619 |
| RSDS | +0.0121507 | 0.1297405 | 0.3459747 | 0.5968064 |
| Topography PC1 | +0.0006230 | 0.4870259 | 0.9740519 | 0.9840319 |
| Precipitation PC1 | -0.0071789 | 0.7385230 | 0.9880240 | 1.0000000 |
| Soil PC1 | -0.0138302 | 0.9660679 | 0.9880240 | 1.0000000 |
| Soil PC2 | -0.0194172 | 0.9880240 | 0.9880240 | 1.0000000 |
| Precipitation seasonality | -0.0208806 | 0.8982036 | 0.9880240 | 1.0000000 |

Temperature PC1 is the only raw 5% axis. Its mean excess is positive in all five geographical folds and in 19/25 fold-by-distance strata. It does not survive BH or maxT correction at 0.05.

## Conditional-intensity axis family

No axis is supported. The smallest raw P values are:

- Soil PC1: excess +0.0331570; raw P=0.1437126; BH q=0.6147705; maxT P=0.6367265;
- Temperature PC1: excess +0.0384418; raw P=0.1536926; BH q=0.6147705; maxT P=0.5828343.

All remaining raw P values are at least 0.4451098.

## Relation to the observation-level full model

- Pigmentation state retains a negative Temperature PC1 coefficient in the full model (mean -0.54185; 95% CrI -1.03294 to -0.04859). The held-out temperature-axis test points to the same axis but remains familywise inconclusive.
- Conditional intensity retains directional full-model terms for precipitation PC1, temperature seasonality, topography PC1 and Temperature PC1 × temperature seasonality, but none is corroborated as axis-specific pairwise divergence beyond space.

A credible full-model coefficient and an axis-specific spatial-null excess are therefore not interchangeable forms of evidence.

## Legacy-result boundary

The earlier state result (excess +0.048562; P=0.03393) used four legacy multiscale PCs: `broad50km_pc1`, `broad50km_pc2`, `within50km_pc1`, and `within50km_pc2`. It remains a basis-specific multiscale topoclimate/radiation sensitivity. It is not a final-eight-axis result and cannot identify Temperature PC1 as its driver.

## Execution provenance

- workflow: `.github/workflows/broad-final8-axis-space-null.yml`;
- successful run: `32111354890`;
- artifact: `9315132730`;
- artifact digest: `sha256:56cb9d0da2a04f583ae97f495d6a2fd58a91602c111374a5bebf3f38925e4a1e`;
- analysis script: `scripts/fit_broad_final8_axis_space_null_attribution.R`.

## Claim ceiling

The strongest defensible statement is:

> The full model identifies a directional cool-climate association of pigmentation state. The exact final-eight-axis distance does not generate omnibus held-out divergence beyond spatial continuity. Temperature PC1 is the strongest repeated named axis, but its eight-axis-corrected evidence remains inconclusive. Conditional intensity shows directional full-model associations without held-out axis-specific divergence beyond space.

The analysis does not establish selection, local adaptation, plasticity or direct anthocyanin physiology.
