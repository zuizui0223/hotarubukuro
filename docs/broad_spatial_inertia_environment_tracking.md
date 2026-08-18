# Broad phenotype divergence beyond a cross-fitted spatial null

## Question

At the same approximate geographical separation, are environmentally dissimilar locations more phenotypically different than a model based on spatial continuity alone would predict?

The estimand is **phenotype divergence in excess of a cross-fitted space-only null, conditional on comparable geographical separation**.

`space` is an unresolved geographical expectation. It can contain unmeasured environment, dispersal or population history, sampling geometry and other spatially structured processes, so it is not interpreted as a single biological mechanism.

## Cross-fitted spatial-null design

For each response and each of five held-out geographical folds:

1. fit `intercept + Matérn SPDE` to the other four folds only;
2. generate 500 posterior-predictive phenotype realizations at locations in the held-out fold;
3. construct held-out location pairs and calculate geographical distance, environmental distance and observed phenotype divergence;
4. divide pairs into five equal-count geographical-distance strata;
5. within each stratum, contrast the upper environmental-distance quartile against the lower quartile;
6. compare the observed `high-environment minus low-environment` phenotype-divergence contrast with the identical statistic from the space-only posterior-predictive realizations.

Every tested pair lies wholly inside a fold omitted from model fitting.

## Provenance correction: the accepted result uses four legacy multiscale PCs

The accepted PR #50 numerical result is reproducible, but its later metadata description was incorrect.

The fitter calls `v16_environment_terms(50)`, which resolves to:

- `broad50km_pc1`;
- `broad50km_pc2`;
- `within50km_pc1`;
- `within50km_pc2`.

These are four legacy multiscale PCs, not six named climate/aridity/topography scores and not the final eight measured abiotic axes. Broad PC1 is principally an elevation–temperature contrast; broad PC2 is weighted toward precipitation and radiation; within-neighbourhood PC1 combines temperature, elevation and precipitation; and within-neighbourhood PC2 is strongly radiation weighted.

A former workflow overwrote the metadata with six labels that were not the columns used by the fitter. The workflow and canonical wrapper now record the implemented four-PC basis explicitly.

## Accepted legacy four-PC result

Frozen input: artifact `9022276431`, ZIP SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`; 500 posterior-predictive realizations; seed `20260725`; five geographical folds and five geographical-distance strata per fold.

| Response | Observed high-env − low-env divergence | Space-null median | Central 95% null | Excess | One-sided P |
|---|---:|---:|---:|---:|---:|
| Pigmentation state | **0.106802** | 0.058240 | 0.018098 to 0.108066 | **+0.048562** | **0.03393** |
| Conditional intensity | -0.047179 | -0.001287 | -0.075026 to 0.087732 | -0.045891 | 0.87226 |

This result supports state-specific environmental alignment along the **legacy multiscale topoclimate/radiation basis**. It cannot identify Temperature PC1 or any other final-model axis as the source of that excess.

## Final-eight-axis rerun

PR #56 reran the same design with the exact final measured axes used by the observation-level Broad model:

- Temperature PC1;
- precipitation PC1;
- temperature seasonality;
- precipitation seasonality;
- topography PC1;
- soil PC1;
- soil PC2;
- RSDS.

### Omnibus distance across all eight axes

| Response | Observed high-env − low-env divergence | Space-null median | Excess | One-sided P |
|---|---:|---:|---:|---:|
| Pigmentation state | 0.026205 | 0.026663 | -0.000458 | 0.51497 |
| Conditional intensity | 0.021725 | 0.024745 | -0.003019 | 0.53493 |

The final-eight-axis omnibus does not reproduce the legacy four-PC state result.

### Axis-specific attribution

For pigmentation state, Temperature PC1 is the only raw 5% axis:

- observed high-temperature-distance minus low-temperature-distance phenotype divergence: 0.100608;
- space-null median: 0.048475;
- excess: **+0.052133**;
- raw one-sided posterior-predictive P: **0.00998**;
- BH q across eight axes: **0.07984**;
- shared-draw maxT FWER P: **0.07784**.

The Temperature PC1 excess is positive on average in all five geographical folds and in 19 of 25 fold-by-distance strata. It is therefore the strongest named candidate axis, but it is not familywise supported at 0.05.

No other pigmentation-state axis has raw P<0.05. No conditional-intensity axis is supported; the largest positive candidates are Soil PC1 (raw P=0.14371; maxT P=0.63673) and Temperature PC1 (raw P=0.15369; maxT P=0.58283).

Successful final-eight-axis workflow run: `32111354890`. Artifact: `9315132730`; digest `sha256:56cb9d0da2a04f583ae97f495d6a2fd58a91602c111374a5bebf3f38925e4a1e`.

## Difference from the observation-level full model

The full environment + SPDE model asks which axis has a directional partial association with a response after the other measured axes and a continuous spatial field are included.

The cross-fitted space-only test instead asks whether absolute difference along an environmental basis or axis orders absolute phenotype divergence in withheld geography beyond spatial continuity. It is unsigned and pair based.

Consequently:

- a credible full-model coefficient does not imply axis-specific divergence beyond space;
- an axis-specific divergence result does not supply the direction of the trait response;
- the direction must come from the full-model coefficient, while the held-out test supplies evidence about spatially transferable divergence;
- observation-level and cell-pair results should be presented as complementary, not interchangeable.

Current triangulation is:

- **Pigmentation state:** the full model supports lower pigmentation probability toward warmer Temperature PC1; Temperature PC1 is also the strongest held-out divergence axis, but axis-family correction remains above 0.05.
- **Conditional intensity:** precipitation, temperature seasonality, topography and the Temperature PC1 × temperature-seasonality interaction remain directional full-model terms, but none is shown to organize held-out pairwise divergence beyond space.

## Reproducibility

Legacy four-PC route:

```bash
Rscript scripts/run_broad_space_null_phenotype_excess_pipeline.R \
  --cells=results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv \
  --output=results/broad_space_null_phenotype_excess \
  --samples=500 \
  --seed=20260725 \
  --max-pairs-per-fold=15000 \
  --geo-bins=5
```

Final-eight-axis route:

```bash
Rscript scripts/fit_broad_final8_axis_space_null_attribution.R \
  --cells=results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv \
  --output=results/broad_final8_axis_space_null_attribution \
  --samples=500 \
  --seed=20260725 \
  --max-pairs-per-fold=15000 \
  --geo-bins=5
```

## Claim boundary

The robust manuscript-facing conclusion is not that “environment as a whole” exceeds space under every representation.

> **The observation-level model identifies a cool-climate association of pigmentation state. A legacy multiscale topoclimate distance shows state divergence beyond spatial continuity, while the exact final-eight-axis omnibus is null. Temperature PC1 is the strongest named held-out axis, but its eight-axis-corrected evidence remains inconclusive. Conditional intensity shows directional full-model associations without held-out divergence beyond space.**

None of these results demonstrates selection, local adaptation, plasticity or a unique anthocyanin mechanism. This result does not by itself demonstrate selection or local adaptation.
