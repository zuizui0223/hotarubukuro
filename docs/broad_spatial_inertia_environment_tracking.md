# Broad phenotype divergence beyond a cross-fitted spatial null

## Question

At the same approximate geographical separation, are environmentally dissimilar locations more phenotypically different than a model based on spatial continuity alone would predict?

The scientifically relevant quantity is **phenotype divergence in excess of the cross-fitted space-only null, conditional on comparable geographical separation**.

`space` is an unresolved geographical expectation. It can contain unmeasured environment, dispersal or population history, sampling geometry and other spatially structured processes, so it is not interpreted as a single biological mechanism.

## Cross-fitted spatial-null design

The analysis reuses the frozen active Broad 1-km cell table, the existing five geographical folds and the frozen response-blind environmental basis.

For each response and each held-out geographical fold:

1. fit `intercept + Matérn SPDE` to the other four folds only;
2. generate 500 posterior-predictive phenotype realisations for locations in the held-out fold;
3. construct held-out location pairs and calculate geographical distance, environmental distance and observed phenotype divergence;
4. divide pairs into five equal-count geographical-distance strata;
5. within each geographical-distance stratum, contrast the upper environmental-distance quartile against the lower quartile;
6. compare the observed `high-environment minus low-environment` phenotype-divergence contrast with the identical contrast generated from each space-only posterior-predictive realisation.

Because every tested pair lies wholly inside a fold omitted from model fitting, the observed phenotype used to test excess is not used to fit its own spatial null.

Environmental distance is Euclidean distance in the six frozen Broad/within response-blind environmental PC scores, with scaling estimated on the corresponding training folds only.

## Primary estimand

For each response, the primary statistic is the mean across the 25 fold-by-geographical-distance strata of `mean phenotype divergence among high-environment-distance pairs - mean phenotype divergence among low-environment-distance pairs`.

The observed statistic is compared with the distribution of that statistic under the cross-fitted space-only posterior predictive null. The directional upper-tail probability is `P(space-null contrast >= observed contrast)`, estimated from 500 posterior-predictive realisations with `(1 + count) / 501`.

The stored `q025`/`q975` values are a central 95% null interval. They are not the cutoff for the one-sided 5% test; the directional cutoff is the 95th percentile.

## Executed result

The calculation has been reproduced through all 10 space-only SPDE fits (2 responses x 5 folds) on the frozen Broad input. During PR #50 integration, a metadata-table row-count defect was found after the scientific result tables had been written. It is now fixed at source: the fitter must exit normally, the canonical wrapper requires every scientific output table, and the accepted values are checked against numerical tolerances.

Frozen input: source artifact ID `9022276431`, ZIP SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`, 500 posterior-predictive realisations, seed `20260725`, five geographical folds and five geographical-distance strata per fold.

### Primary spatial-null excess test

| Response | Observed high-env - low-env divergence | Space-null median | Central 95% null | Excess over null median | One-sided posterior-predictive p |
|---|---:|---:|---:|---:|---:|
| Pigmentation state | **0.106802** | 0.058240 | [0.018098, 0.108066] | **+0.048562** | **0.03393** |
| Conditional intensity | -0.047179 | -0.001287 | [-0.075026, 0.087732] | -0.045891 | 0.87226 |

For pigmentation state, environmentally dissimilar pairs are more phenotypically divergent than environmentally similar pairs after matching pairs into comparable geographical-distance strata, and the contrast is larger than expected from the cross-fitted space-only null at the predefined one-sided 5% level (`p = 0.03393`). The observed value lies just below the 97.5th percentile of the central 95% interval; this is compatible with the directional result because the one-sided 5% test uses the 95th percentile.

For conditional intensity, the environmental contrast is negative and is not above the spatial null (`p = 0.87226`). The phenotype-excess signal is therefore specific to pigmentation state in this analysis.

Pair-level secondary correlations are descriptive only because pairs share sites and are not independent.

## Interpretation

> For pigmentation state, geographical proximity alone does not fully account for the observed pattern of differentiation. Among locations separated by comparable geographical distances, greater environmental difference is associated with greater phenotype differentiation than a cross-fitted continuous-spatial null predicts. The same excess is not detected for conditional pigment intensity.

The earlier four-model blocked predictive comparison answers a different question—what predicts withheld geography better—and remains secondary.

## Reproducibility

```bash
Rscript scripts/fit_broad_space_null_phenotype_excess.R \
  --cells=results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv \
  --output=results/broad_space_null_phenotype_excess \
  --samples=500 \
  --seed=20260725 \
  --max-pairs-per-fold=15000 \
  --geo-bins=5
```

GitHub Actions entry point: `.github/workflows/broad-spatial-inertia-environment-tracking.yml`.

Primary outputs: `primary_space_null_excess_test.csv`, `matched_distance_stratum_contrasts.csv`, `heldout_pair_space_null_excess.csv`, `heldout_space_null_predictions.csv`, `secondary_pair_diagnostics.csv`, `analysis_metadata.csv`, and `RESULT_SUMMARY.md`.

## Claim boundary

This test shows an environmental alignment beyond a fitted spatial expectation. It does not by itself distinguish causal environmental effects from omitted spatially structured factors, nor does it demonstrate selection or local adaptation.

Manuscript-safe headline:

> **Pigmentation-state divergence exceeds a cross-fitted spatial expectation along environmental difference, whereas conditional intensity does not.**
