# Broad spatial inertia versus environmental tracking

## Question

Does geographically blocked prediction of flower-colour phenotype depend more on measured environment or on residual continuous geography?

This is an **F_ST/P_ST-inspired interpretation only**. The analysis does not contain neutral genetic markers, additive genetic variance, heritability estimates or common-garden phenotypes, so it must not be described as an F_ST, P_ST or Q_ST test.

## Biological mapping

- `environment`: measured environmental tracking that transfers to withheld geography.
- `space`: residual geographical continuity after excluding measured environmental predictors. It may contain population/dispersal history, unmeasured environment and other spatially structured processes.
- observed phenotype: the combined geographical phenotype; it is not P_ST in the population-genetic sense.

The intended biological contrast is therefore **spatial inertia versus environmental tracking**.

## Model set

The executed analysis used the same 1-km analysis cells, frozen 50-km response-blind environmental basis and five approximately 100-km geographical folds as the active broad predictive reference.

1. `null`: intercept only.
2. `environment`: intercept + the frozen environmental basis.
3. `space`: intercept + the Matérn SPDE field.
4. `environment_plus_space`: intercept + environment + SPDE.

The comparison was performed on held-out geographical folds. In-sample WAIC was not used as the primary evidence because the question is whether each information source transfers geographically.

## Primary score and decomposition

For each response and held-out fold, posterior predictive log density was retained for all four models. The full-model predictive gain over the null was allocated by a two-player Shapley decomposition:

- environment gain = 0.5 * [(environment - null) + (full - space)]
- space gain = 0.5 * [(space - null) + (full - environment)]

This symmetrises model-entry order and avoids calling shared environment/space geography uniquely environmental or uniquely spatial.

## Executed result

GitHub Actions run `32037340750` completed all 40 fits (2 responses x 5 folds x 4 models) on commit `703bb3dc027b132014a1bef0a8c826a10d4ee36e`.

The frozen active Broad reference artifact was locked before fitting:

- source artifact ID: `9022276431`
- source artifact ZIP SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`
- posterior samples per fit: 500
- seed: 20260725

### Held-out log predictive density

Higher values are better.

| Response | Null | Environment | Space | Environment + space |
|---|---:|---:|---:|---:|
| Pigmentation state | -1219.7933 | -1010.2320 | **-743.9521** | -746.4872 |
| Conditional intensity | -919.1875 | -916.1431 | **-890.7378** | -896.2634 |

For both responses, the `space` model had the highest total held-out log predictive density. Adding the frozen environmental basis to the spatial field did not improve blocked predictive density: relative to `space`, the full model changed by -2.5351 for pigmentation state and -5.5256 for conditional intensity.

### Shapley allocation of predictive gain over null

| Response | Environment gain | Space gain | Full gain over null | Environment share | Space share |
|---|---:|---:|---:|---:|---:|
| Pigmentation state | 103.5131 | **369.7930** | 473.3061 | 0.2187 | **0.7813** |
| Conditional intensity | -1.2406 | **24.1647** | 22.9241 | -0.0541 | **1.0541** |

The predefined interpretation is therefore `spatial_inertia_dominant` for both responses.

Pigmentation state nevertheless contains substantial transferable environmental information: the environment-only model improves strongly over the null, and its Shapley gain is positive. The much larger spatial contribution indicates that residual continuous geography carries considerably more held-out predictive information under this model basis.

Conditional intensity gives a sharper result. The environment-only model improves only slightly over the null, and environmental information has a slightly negative Shapley contribution once its interaction with space is averaged over model-entry order. Thus the motivating expectation that conditional intensity would be more environment-dominant than pigmentation state is **not supported by this blocked predictive analysis**.

## Biological interpretation

The result strengthens the manuscript's existing claim that the broad flower-colour pattern is a geographical template rather than a single measured environmental mechanism.

A safe reading is:

> Residual continuous geography carried more transferable predictive information than the frozen measured environmental basis for both pigmentation state and conditional intensity. Measured environment contributed appreciably to pigmentation-state geography, but provided little incremental blocked predictive information once spatial continuity was represented; this was even more pronounced for conditional intensity.

This result does **not** mean that the observation-level temperature or topographic coefficients are false. The decomposition uses the cell-level predictive reference and its frozen 50-km broad/within environmental PCs, whereas the observation-level Appendix S3 model uses a different environmental parameterization. The two analyses answer different questions: association after conditioning on a spatial field versus transfer of predictive information to withheld geography.

Likewise, `space` must not be equated with population history. It remains a composite residual geographical field that can include unmeasured environment, dispersal/population history, sampling structure and other spatially structured processes.

## Reproducibility

Run locally with the frozen cell table available:

```bash
Rscript scripts/fit_broad_spatial_inertia_environment_tracking.R \
  --cells=results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv \
  --output=results/broad_spatial_inertia_environment_tracking \
  --samples=500 \
  --seed=20260725
```

The reproducible GitHub Actions entry point is `.github/workflows/broad-spatial-inertia-environment-tracking.yml`.

Successful result artifact:

- artifact ID: `9291248341`
- artifact ZIP SHA-256: `484e8485b067f48b38315402a5a9b7975ebdad75c1717b2aa86c793f6fe5a426`

Primary outputs are `fold_model_scores.csv`, `heldout_predictions.csv`, `model_log_score_totals.csv`, `shapley_predictive_decomposition.csv`, `component_interpretation.csv`, `analysis_metadata.csv`, and `RESULT_SUMMARY.md`.

## Claim boundary

Do not use phrases such as `P_ST > F_ST`, `selection exceeds drift`, `genetic differentiation`, or `local adaptation demonstrated`. Those require population-genetic and/or common-garden evidence absent from this dataset.

The manuscript-safe conclusion is not that measured environment overcomes geographical similarity. Under the frozen predictive basis, **spatial inertia dominates geographically transferable predictive information for both phenotype components, while measured environment retains a meaningful but secondary contribution for pigmentation state**.
