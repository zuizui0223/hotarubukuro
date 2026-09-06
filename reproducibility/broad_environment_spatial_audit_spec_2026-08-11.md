# Broad environmental and spatial-model audit protocol

Date: 2026-08-11

## Purpose

This protocol defines the robustness audit used to finalize the two Broad observation-level flower-colour models:

1. Bernoulli pigmentation state across the environment-complete observations;
2. Gaussian visible intensity conditional on pigmentation.

The audit asks whether omitted environmental proxies, observation-process controls or alternative residual spatial structures improve geographically transferable inference enough to alter the adopted Broad models.

## Response hierarchy

The two responses are related but not interchangeable.

- **Pigmentation state** represents the visible white/pigmented transition.
- **Conditional visible intensity** represents standardized CIELAB a* variation among pigmented observations.

CIELAB a* is a visible-colour phenotype, not direct anthocyanin concentration, UV reflectance or pollinator vision.

## Retained environmental basis

The common Broad basis contains eight measured abiotic axes:

- Temperature PC1;
- precipitation PC1;
- temperature seasonality;
- precipitation seasonality;
- topography PC1;
- soil PC1;
- soil PC2;
- RSDS.

These represent warm-season thermal regime, climatic water supply, climatic variability, terrain, soil context and broad shortwave-radiation exposure.

## Additional environmental proxies audited

The audit evaluates biologically plausible extensions without presuming that additional predictors improve inference:

- vapour-pressure deficit (VPD);
- site water balance;
- BIO6 cold extreme;
- BIO13 wet extreme;
- forest fraction;
- distance to coast.

Observation-process sensitivities include:

- day of year and year;
- mask fraction;
- image overexposure flag.

Important variables not represented adequately by the harmonized 1-km public-data boundary are treated as limitations rather than inferred indirectly. These include direct UV-B, observation-year pre-anthesis weather, flower-level canopy/light/hydrology, variety/genetic identity and direct dispersal kernels.

## Interaction audit

### Pigmentation state

Candidate interactions are evaluated only when they have a biological rationale and remain estimable under the common Broad basis. No state interaction is promoted unless posterior support and geographically blocked transfer both satisfy the promotion rule.

### Conditional intensity

The focal interaction is Temperature PC1 × temperature seasonality. Joint adjudication tests whether additional seasonality interactions contribute independently after the thermal interaction is fitted.

The adopted intensity model retains Temperature PC1 × temperature seasonality and does not retain an additional precipitation PC1 × temperature-seasonality interaction.

## Spatial structures audited

All spatial candidates use the same observation population and environmental basis appropriate to the response.

Evaluated structures:

1. stationary Matérn SPDE + East/West structural adjustment;
2. stationary Matérn SPDE without East/West;
3. coastline-barrier SPDE + East/West;
4. coastline-barrier SPDE without East/West.

The coastline barrier is a covariance sensitivity motivated by island separation; it is not a mechanistic dispersal model.

More flexible anisotropic, nonstationary or genetic-cluster structures are not promoted without independent data sufficient to identify them.

## Model comparison outputs

Each candidate reports, where applicable:

- WAIC;
- DIC;
- mean negative log CPO;
- fixed effects and uncertainty;
- SPDE hyperparameters;
- VIF;
- five response-blind geographical folds;
- state: held-out log loss, AUC and Brier score;
- intensity: held-out squared error, RMSE, MAE and R²;
- spatial-block bootstrap of paired held-out loss gain;
- number of geographical folds improved.

## Promotion rule

An environmental or spatial expansion is promoted only when the evidence is jointly convincing. The audit requires:

1. successful model fit;
2. meaningful full-fit improvement, such as ΔWAIC >=2 or an analogous CPO gain;
3. improvement in the primary geographically blocked predictive loss;
4. spatial-block bootstrap support for positive predictive gain;
5. improvement in at least four of five geographical folds for strong promotion;
6. acceptable fixed-effect collinearity and numerical stability;
7. a biological interpretation justified independently of the response map.

VIF is a graded diagnostic:

- <5 preferred;
- 5–10 requires explicit stability evidence;
- >10 blocks promotion absent exceptional mechanistic and predictive justification.

Parsimonious models are preferred when a larger candidate does not provide transferable improvement.

## Adopted model decisions

### Pigmentation state

Retain:

`state ~ East/West + eight abiotic axes + stationary SPDE`

No tested interaction or environmental expansion satisfies the full promotion rule. The retained state model maximum VIF is 4.430.

### Conditional visible intensity

Retain:

`intensity ~ East/West + eight abiotic axes + Temperature PC1:temperature seasonality + stationary SPDE`

The retained interaction posterior mean is -0.204234 with 95% CrI -0.301869 to -0.106561. Maximum VIF is 6.340 for Temperature PC1; the interaction VIF is 1.664.

VPD is not retained despite biological plausibility because it produces severe collinearity and lacks sufficient held-out transfer improvement.

### Spatial structure

Retain stationary Matérn + East/West for both response parts. Coastline-barrier formulations do not show the consistent held-out improvement required for promotion.

## Interpretation boundary

The audit supports a defensible residual covariance model and a stable measured-environment basis. It does not identify the SPDE field with migration, seed dispersal, pollen movement, colonization history or one unmeasured environmental mechanism.

## Reproducibility

Historical comparison provenance:

- workflow `31258851297`;
- artifact `9022276431`;
- SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

Current executable route:

- `run_pipeline.py` (single orchestrator);
- `scripts/run_broad_environment_spatial_audit.R` (Broad audit stage).

The historical `analysis_sensitivity` wrapper and one-off Broad workflow were removed from the active repository surface after finalization; Git history preserves them.

Current retained decision records:

- `reproducibility/broad_environment_spatial_final_2026-08-19.md`;
- `reproducibility/broad_environment_spatial_final_fixed_effects_2026-08-11.csv`;
- `reproducibility/broad_environment_spatial_final_hyperparameters_2026-08-11.csv`;
- `reproducibility/broad_environment_variable_evidence_registry_2026-08-11.csv`.
