# Broad spatial inertia versus environmental tracking

## Question

Does geographically blocked prediction of flower-colour phenotype depend more on measured environment or on residual continuous geography?

This is an **F_ST/P_ST-inspired interpretation only**. The analysis does not contain neutral genetic markers, additive genetic variance, heritability estimates or common-garden phenotypes, so it must not be described as an F_ST, P_ST or Q_ST test.

## Biological mapping

- `environment`: measured environmental tracking that transfers to withheld geography.
- `space`: residual geographical continuity after excluding measured environmental predictors. It may contain population/dispersal history, unmeasured environment and other spatially structured processes.
- observed phenotype: the combined geographical phenotype; it is not P_ST in the population-genetic sense.

The intended biological contrast is therefore **spatial inertia versus environmental tracking**.

## Predefined model set

Use the same response definitions, analysis cells and approximately 100-km geographical folds as the active broad predictive reference.

1. `null`: intercept / structural adjustment only.
2. `environment`: null + the frozen response-blind environmental basis.
3. `space`: null + the Matérn SPDE field.
4. `environment_plus_space`: null + environment + SPDE.

The comparison must be performed on held-out geographical folds. In-sample WAIC is not the primary evidence because the question is whether each information source transfers geographically.

## Primary score and decomposition

For each response and held-out fold, retain log predictive density for all four models. Allocate the full-model predictive gain over the null by a two-player Shapley decomposition:

- environment gain = 0.5 * [(environment - null) + (full - space)]
- space gain = 0.5 * [(space - null) + (full - environment)]

This symmetrises model-entry order and avoids calling the shared environment/space geography uniquely environmental or uniquely spatial.

Secondary response-scale metrics should remain interpretable:

- pigmentation state: Brier score and AUC;
- conditional intensity: RMSE / MAE and predictive coverage.

These secondary metrics are diagnostics, not additive variance partitions.

## Interpretation gate

- environment Shapley gain > space gain: evidence for stronger environmental tracking in held-out geography;
- space gain > environment gain: evidence for stronger spatial inertia;
- similar gains: neither component dominates;
- environment-only useful but little incremental environment gain after space: much of the apparent environmental signal is shared geography.

Compare pigmentation state and conditional intensity. The motivating hypothesis is that state retains longer-range spatial inertia, while intensity may track present environment more strongly at finer scale. This is a hypothesis to test, not a result to write into the manuscript before the blocked decomposition completes.

## Required output contract

`results/broad_spatial_inertia_environment_tracking/fold_model_scores.csv` must contain:

- `response`
- `fold`
- `model`
- `log_predictive_density`

where `model` is exactly one of `null`, `environment`, `space`, `environment_plus_space`.

Run:

```bash
Rscript scripts/run_broad_spatial_inertia_environment_tracking.R
```

The runner writes:

- `shapley_predictive_decomposition.csv`
- `component_interpretation.csv`

## Claim boundary

Do not use phrases such as `P_ST > F_ST`, `selection exceeds drift`, `genetic differentiation`, or `local adaptation demonstrated`. Those require population-genetic and/or common-garden evidence absent from this dataset. A manuscript-safe phrasing is:

> Pigmentation components differed in the balance between transferable environmental tracking and residual spatial inertia.
