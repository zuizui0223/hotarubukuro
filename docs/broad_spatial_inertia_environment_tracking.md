# Broad phenotype divergence beyond a cross-fitted spatial null

## Question

At the same approximate geographical separation, are environmentally dissimilar locations more phenotypically different than a model based on spatial continuity alone would predict?

This is the intended F_ST/P_ST-inspired analogy. It is **not** an F_ST, P_ST or Q_ST analysis: there are no neutral genetic markers, additive genetic variances, heritability estimates or common-garden phenotypes.

## Biological mapping

- `space-only SPDE`: an F_ST-like **spatial null / geographical expectation** in the limited sense that it represents how much phenotype resemblance or divergence is expected from continuous geography alone.
- observed flower-colour divergence: the P_ST-like **phenotypic signal** in the limited sense that it is the realised phenotype difference to be compared with that spatial expectation.
- environmental divergence: the axis used to ask whether observed phenotypic divergence systematically exceeds the space-only expectation.

The scientifically relevant quantity is therefore not an environment-versus-space model-performance comparison. It is **phenotype divergence in excess of the cross-fitted space-only null, conditional on comparable geographical separation**.

`space` remains unresolved geography, not neutral genetic F_ST. It can contain unmeasured environment, dispersal/population history, sampling geometry and other spatially structured processes.

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

For each response, the primary statistic is the mean across the 25 fold-by-geographical-distance strata of

`mean phenotype divergence among high-environment-distance pairs - mean phenotype divergence among low-environment-distance pairs`.

The observed statistic is compared with the distribution of that statistic under the cross-fitted space-only posterior predictive null.

The directional posterior-predictive upper-tail probability is

`P(space-null contrast >= observed contrast)`

estimated from 500 posterior-predictive realisations with the finite-sample correction `(1 + count) / 501`.

The stored `q025`/`q975` values are a central 95% null interval. They are not the cutoff for the one-sided 5% test; the relevant directional cutoff is the 95th percentile.

## Executed result

The calculation has been reproduced through all 10 space-only SPDE fits (2 responses x 5 folds) on the frozen Broad input. The first completed scientific artifact exposed a metadata-table row-count bug only after the scientific result tables had been written; the result values below were recovered directly from that artifact and reproduced on subsequent fits. The workflow now requires all scientific outputs, reconstructs metadata independently and validates the output contract.

Frozen input:

- source artifact ID: `9022276431`
- source artifact ZIP SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`
- posterior-predictive realisations: 500
- seed: `20260725`
- geographical folds: 5
- geographical-distance strata per fold: 5

### Primary spatial-null excess test

| Response | Observed high-env - low-env divergence | Space-null median | Central 95% null | Excess over null median | One-sided posterior-predictive p |
|---|---:|---:|---:|---:|---:|
| Pigmentation state | **0.106802** | 0.058240 | [0.018098, 0.108066] | **+0.048562** | **0.03393** |
| Conditional intensity | -0.047179 | -0.001287 | [-0.075026, 0.087732] | -0.045891 | 0.87226 |

For pigmentation state, environmentally dissimilar pairs are more phenotypically divergent than environmentally similar pairs even after matching pairs into comparable geographical-distance strata, and the magnitude of that environmental contrast is larger than expected from the cross-fitted space-only null at the predefined one-sided 5% level (`p = 0.03393`). The observed value lies just below the 97.5th percentile of the central 95% interval; this is compatible with the one-sided result because the directional 5% test uses the 95th percentile, not the 97.5th percentile.

For conditional intensity, the corresponding environmental contrast is negative and is not above the spatial null (`p = 0.87226`). Thus the phenotype-excess signal is specific to pigmentation state in this analysis.

The pair-level secondary correlations are descriptive only because pairs share sites and are not independent; they are not used to overturn or reinforce the stratum-level posterior-predictive result.

## Interpretation

The result supports the following restricted statement:

> For pigmentation state, geographical proximity alone does not fully account for the observed pattern of differentiation. Among locations separated by comparable geographical distances, greater environmental difference is associated with greater phenotype differentiation than a cross-fitted continuous-spatial null predicts. The same excess is not detected for conditional pigment intensity.

In the F_ST/P_ST-inspired intuition, pigmentation state therefore shows a **P_ST-like phenotypic divergence that exceeds an F_ST-like spatial expectation along environmental difference**, but only as an analogy to the structure of the test. It must not be rewritten as `P_ST > F_ST` because neither quantity is actually estimated here.

This analysis also changes the interpretation of the earlier four-model blocked predictive comparison. That comparison answers which information source predicts withheld geography better. It is retained only as a secondary predictive diagnostic and is not the inferential target for the F_ST/P_ST-inspired question.

## Reproducibility

Main runner:

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

Primary outputs:

- `primary_space_null_excess_test.csv`
- `matched_distance_stratum_contrasts.csv`
- `heldout_pair_space_null_excess.csv`
- `heldout_space_null_predictions.csv`
- `secondary_pair_diagnostics.csv`
- `analysis_metadata.csv`
- `RESULT_SUMMARY.md`

## Claim boundary

Do not use `F_ST > P_ST`, `P_ST > F_ST`, `selection exceeds drift`, `genetic differentiation`, `local adaptation demonstrated` or equivalent causal/genetic language. The space-only null is not a neutral genetic model, and environmental divergence can still proxy omitted spatially structured factors.

The manuscript-safe headline is:

> **Pigmentation-state divergence exceeds a cross-fitted spatial expectation along environmental difference, whereas conditional intensity does not.**
