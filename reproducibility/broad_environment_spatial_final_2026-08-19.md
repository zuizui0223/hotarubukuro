# Final Broad environmental + spatial decision

Date: 2026-08-19
Status: publication lock

## Responses

The paper treats flower colour as two linked but non-interchangeable responses.

### Pigmentation state

Bernoulli-logit, n = 1,922:

`state ~ East/West + Temperature_PC1 + precipitation_PC1 + temperature_seasonality + precipitation_seasonality + Topography_PC1 + Soil_PC1 + Soil_PC2 + RSDS + stationary_SPDE`

No interaction met the full promotion rule for the state response.

### Conditional visible intensity

Gaussian among pigmented observations, n = 956:

`intensity ~ East/West + Temperature_PC1 + precipitation_PC1 + temperature_seasonality + precipitation_seasonality + Topography_PC1 + Soil_PC1 + Soil_PC2 + RSDS + Temperature_PC1:temperature_seasonality + stationary_SPDE`

Retained interaction: posterior mean -0.204234; 95% CrI -0.301869 to -0.106561. The temperature slope therefore depends on long-term temperature seasonality; intensity is not reduced to a single colder-versus-warmer rule.

## Collinearity decision

VIF is a diagnostic, not an automatic deletion rule.

- State: maximum VIF = 4.430 (Soil PC1); final model is <5 throughout.
- Intensity: Temperature PC1 = 6.340; Soil PC1 = 5.233; retained interaction = 1.664; East/West = 1.822.
- VPD expansions produced VIFs around 26 and failed transferable-prediction criteria; they remain rejected.

Removing East/West merely to force every VIF below five was rejected because the intensity model lost about 5.9 WAIC units without a robust blocked-transfer gain.

## Spatial decision

The retained spatial specification for both responses is stationary Matérn SPDE + East/West. Coastline-barrier alternatives did not provide transferable improvement and are a negative guardrail rather than part of the final model.

Descriptive posterior spatial ranges:

- state: 132.76 km (95% CrI 88.78–195.68);
- conditional intensity: 65.72 km (95% CrI 31.05–132.63).

These are residual spatial correlation ranges, not dispersal distances.

## Environment-against-space test

The final supplementary spatial-null question is not coefficient weighting. Held-out geographic pairs are fixed first; the observed divergence associated with the environment term(s) supported by the final Broad model is then compared with cross-fitted space-only predictive maps.

Primary result:

- pigmentation state: observed high-minus-low environmental divergence = 0.100608; space-only median = 0.048475; excess = +0.052133; one-sided posterior-predictive P = 0.00998;
- conditional intensity: no corresponding supported-term excess (P = 0.26347).

Interpretation: pigmentation-state differentiation aligns with supported environmental differences beyond a cross-fitted expectation based only on geographic continuity. This is not an F_ST/P_ST analysis and is not labelled selection-versus-drift.

## Downstream contract

Broad feeds two downstream questions without creating alternative Broad models:

1. local white-pigmented boundaries are the comparison unit for the Bombus analysis;
2. the final eight-axis pigmentation-state reference generates 10,000 natural maps for continuous same-colour isolation.

The former 16-event local-departure detector, DID analysis and coefficient-weighted Broad null are superseded and are not part of the submission pipeline.

## Frozen evidence files

- `broad_environment_spatial_final_fixed_effects_2026-08-11.csv`
- `broad_environment_spatial_final_hyperparameters_2026-08-11.csv`
- `broad_environment_variable_evidence_registry_2026-08-11.csv`
- `broad_supported_term_distance_space_null_2026-08-19.md`

The executable route is defined by `run_pipeline.py` and `config/pipeline.yml`.
