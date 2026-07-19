# Reproducibility and mathematical review

## Severity A — conclusions can change

1. **Observation alignment during raster extraction.** `Code_S3.R` removes rows with missing coordinates and then uses `df[seq_len(n_row), ]` before `cbind`. If an invalid coordinate occurs before the end of the table, raster values are attached to the wrong observation. Preserve a row identifier and join extracted values by that identifier.
2. **The Pigment CFA is not an independent measurement model.** Chroma is deterministically computed from Lab coordinates (`C = sqrt(a^2+b^2)`), while `Lm = -L`. Treating `a`, `Lm`, and `C` as three reflective indicators can manufacture excellent apparent coherence from algebraic dependence. Use a prespecified colour contrast/index, PCA with out-of-sample validation, or a measurement model based on genuinely distinct measurements.
3. **Second-stage qGAM is exploratory only.** It treats fitted SPDE residuals as observed without propagating posterior uncertainty, then adds another spatial smooth. P-values/intervals from this stage are too optimistic for confirmatory inference. Prefer one joint model or posterior-draw propagation.
4. **Current variance proportions are not an exact variance decomposition.** `var(fixed) + var(spatial) + residual variance` omits `2*cov(fixed, spatial)`. Fixed and spatial fitted components are generally not orthogonal. Label the current output a descriptive fitted-component allocation or compute posterior predictive variance contributions with covariance retained.

## Severity B — model geometry or estimand is unclear

1. EPSG:3857 is not a suitable metric geometry for a nationwide Japan SPDE. Use a documented Japan-wide equal-area/conformal projection and report sensitivity to mesh settings.
2. The default `inla.spde2.matern` prior is implicit and hyperparameter extraction through `Theta1`/`Theta2` is parameterization-dependent. Prefer `inla.spde2.pcmatern` with ecologically interpretable range and sigma priors.
3. Continuous predictors should be centered/scaled before fitting. This improves conditioning and makes prior and coefficient scales explicit.
4. `distinct(longitude, latitude, .keep_all=TRUE)` silently discards repeated observations. Decide whether the estimand is location-level or individual/photo-level, and aggregate explicitly with sample size and uncertainty if location-level.
5. `rowSums(..., na.rm=TRUE)` assigns zero richness when every Bombus layer is missing. Return `NA` when all component layers are missing.
6. qGAM spatial smoothing is performed in longitude/latitude degrees, so distance is anisotropic and latitude-dependent. Use projected coordinates.
7. Dividing residuals by posterior fitted-value SD is not the same as a standardized observation residual because observation noise is omitted.

## SDM concerns

1. VIF is calculated only at pooled occurrence locations. This describes correlations in occupied environments, not the available environmental domain used by MaxEnt. Evaluate collinearity over the accessible background, preferably per species.
2. A single random background for all species ignores species-specific accessible areas and sampling bias. Define M for each species and use target-group/background-bias correction where possible.
3. The mainland mask uses hard-coded latitude and area thresholds and excludes parts of the stated national study domain. Define the biological domain explicitly rather than through polygon-size heuristics.
4. Spatially blocked cross-validation is useful, but folds and metrics must be checked for small samples; `n_occ >= 5` is not enough for stable block validation and model selection.
5. Predictions should include clamping/extrapolation diagnostics and uncertainty, not only a selected suitability raster.

## Image workflow concerns

The old `Code_S2.py` used one-cluster k-means. With `k=1`, the centroid is exactly the arithmetic mean, so it is not a statistical mode or a "dominant" colour. The replacement reports median and mean RGB plus mask quality. The HSV segmentation remains a heuristic and should be validated against manually annotated masks across lighting, white/pink morphs, and backgrounds.

## Minimum publication-grade next steps

1. Repair row-safe extraction and make all paths configuration-driven.
2. Freeze R dependencies with `renv.lock`; store small test fixtures and expected outputs.
3. Replace or validate the Pigment construction before interpreting ecological coefficients.
4. Refit a single coherent spatial model with scaled predictors and explicit PC priors.
5. Treat the qGAM result as exploratory unless uncertainty is propagated from the first-stage model.
6. Rebuild Bombus SDMs around species-specific accessible areas and sampling-bias controls.
7. Add synthetic unit tests for timestamp matching, image masks, raster row alignment, and variance calculations.
