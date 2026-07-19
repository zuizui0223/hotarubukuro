# Reproducibility and mathematical review

## Severity A — conclusions can change

1. **Observation alignment during raster extraction.** `Code_S3.R` removes rows with missing coordinates and then uses `df[seq_len(n_row), ]` before `cbind`. If an invalid coordinate occurs before the end of the table, raster values are attached to the wrong observation. Preserve a row identifier and join extracted values by that identifier. A tested implementation is now provided in `R/analysis_core.R`.
2. **The Pigment CFA is not an independent measurement model.** Chroma is deterministically computed from Lab coordinates (`C = sqrt(a^2+b^2)`), while `Lm = -L`. Treating `a`, `Lm`, and `C` as three reflective indicators can manufacture excellent apparent coherence from algebraic dependence. Use a prespecified primary colour response (recommended: a* redness) and report -L* as a sensitivity response, or use a validated dimensional reduction rather than claiming three independent indicators.
3. **Second-stage qGAM is exploratory only.** It treats fitted SPDE residuals as observed without propagating posterior uncertainty, then adds another spatial smooth. P-values/intervals from this stage are too optimistic for confirmatory inference. Prefer one joint model or posterior-draw propagation.
4. **Current variance proportions are not an exact variance decomposition.** `var(fixed) + var(spatial) + residual variance` omits `2*cov(fixed, spatial)`. Fixed and spatial fitted components are generally not orthogonal. `R/analysis_core.R` now includes a symmetric covariance-aware descriptive allocation whose components sum to the total variance identity.

## Severity B — model geometry or estimand is unclear

1. EPSG:3857 is not a suitable metric geometry for a nationwide Japan SPDE. Use a documented Japan-wide projection and report sensitivity to mesh settings.
2. The default `inla.spde2.matern` prior is implicit and hyperparameter extraction through `Theta1`/`Theta2` is parameterization-dependent. Prefer `inla.spde2.pcmatern` with ecologically interpretable range and sigma priors.
3. Continuous predictors should be centered/scaled before fitting. This improves conditioning and makes prior and coefficient scales explicit.
4. `distinct(longitude, latitude, .keep_all=TRUE)` silently discards repeated observations. Decide whether the estimand is location-level or individual/photo-level, and aggregate explicitly with sample size and uncertainty if location-level.
5. `rowSums(..., na.rm=TRUE)` assigns zero when every Bombus layer is missing. Return `NA` when all component layers are missing.
6. qGAM spatial smoothing is performed in longitude/latitude degrees, so distance is anisotropic and latitude-dependent. Use projected coordinates.
7. Dividing residuals by posterior fitted-value SD is not the same as a standardized observation residual because observation noise is omitted.

## Bombus SDM and ecological interpretation

1. **The mainland polygon is defensible for the stated five-species question.** The focal covariate concerns five documented mainland Bombus visitors, so restricting calibration and prediction to Honshu, Shikoku, Kyushu and explicitly named adjacent islands is not itself a flaw. Store the exact polygon and avoid rebuilding it from latitude or polygon-area heuristics.
2. **A sum of continuous Maxent predictions is not species richness.** It is cumulative potential habitat suitability across the five focal species. Use `bombus_suitability_sum` or `potential Bombus assemblage suitability`. If richness is desired, calculate thresholded potential richness with species-specific thresholds and treat it as a sensitivity analysis.
3. **Suitability is not abundance, visitation, pollen transfer or selection intensity.** A positive coefficient can support geographic correspondence with the potential Bombus assemblage, but not the claim that pollination pressure caused darker flowers.
4. **Environmental non-independence must be explicit.** Bombus suitability is derived from climate, topography, soil and land cover. Including both plant environmental predictors and suitability can test whether the SDM surface adds predictive information, but it does not identify a causal biotic effect.
5. **The current SEM does not identify causal mediation.** The mediator is a model prediction constructed from the proposed causes. Regressing it on those same environmental predictors partly reconstructs the SDM. Remove SEM from the main causal argument or label it descriptive only.
6. VIF should represent the accessible environmental domain, not only pooled occurrence environments. Check this within the stated mainland domain, preferably by species.
7. Spatially blocked cross-validation remains appropriate, but fold balance, extrapolation and uncertainty should be reported for each focal species.

See `POLLINATOR_INTERPRETATION.md` for manuscript-safe wording and the recommended sensitivity hierarchy.

## Image workflow concerns

The old `Code_S2.py` used one-cluster k-means. With `k=1`, the centroid is exactly the arithmetic mean, so it is not a statistical mode or a "dominant" colour. The replacement reports median and mean RGB plus mask quality. The HSV segmentation remains a heuristic and should be validated against manually annotated masks across lighting, white/pink morphs, and backgrounds.

## Minimum publication-grade next steps

1. Integrate row-safe extraction into the executable main pipeline and make all paths configuration-driven.
2. Freeze R dependencies with `renv.lock`; store small test fixtures and expected outputs.
3. Replace the CFA response with a prespecified primary colour metric and sensitivity outcomes before interpreting ecological coefficients.
4. Refit a coherent spatial model with scaled predictors, an appropriate projection and explicit PC priors.
5. Treat the qGAM result as exploratory unless uncertainty is propagated from the first-stage model.
6. Rename the continuous Bombus stack, run species-specific and thresholded-richness sensitivity analyses, and remove causal mediation language.
7. Add synthetic/unit tests for timestamp matching, image masks, raster row alignment and variance calculations.
