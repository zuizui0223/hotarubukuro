# Reviewed minimal analysis workflow

This directory contains the complete analysis actually used in PR #5. It replaces the audit-scale branching in PR #4 with one executable workflow.

## Pipeline

1. Read `Data_S1.csv` and retain photograph-level records.
2. Parse observation date and derive day of year.
3. Convert display-referred sRGB to CIELAB D65 and use apparent a* as the response.
4. Extract the five retained Bombus maxnet cloglog suitability rasters by record row.
5. Calculate three alternative availability summaries:
   - summed suitability;
   - any-species index `1 - prod(1 - p_i)`;
   - maximum species suitability.
6. Build one national SPDE mesh in a Japan-centred Lambert azimuthal equal-area projection.
7. Fit four prespecified Gaussian SPDE-INLA models on one common cohort.
8. Run a five-band blocked prediction sensitivity check.
9. Treat widespread-versus-montane species summaries only as spatial-confounding diagnostics.
10. Write compact result tables under `results/bombus_availability/`.

## SPDE-INLA review decisions

The earlier implementation was not copied verbatim. PR #5 changes the following:

- EPSG:3857 is replaced by a Japan-centred metric equal-area projection.
- Repeated photographs are not silently collapsed by coordinate.
- The raster join is row-preserving.
- The duplicated CFA-derived Pigment response is not used; apparent CIELAB a* is explicit.
- Candidate models are prespecified rather than generated from the largest observed predictor correlation.
- The Matern field uses explicit PC priors: `P(range < 100 km) = 0.05` and `P(sigma > 1) = 0.05` on the standardized response scale.
- Only four models required by the ecological question are fit.
- WAIC/CPO compare SPDE models; blocked prediction is reported separately and is not described as causal validation.

## Run

```bash
Rscript analysis/run_bombus_availability.R
```

The five SDM surfaces are legacy outputs. Their values are treated as continuous suitability indices, not calibrated presence probabilities.
