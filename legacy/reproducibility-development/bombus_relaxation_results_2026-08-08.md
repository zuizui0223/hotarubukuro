# Fresh-SDM local Bombus analysis: locked result

Run date: 2026-08-08  
Workflow run: `31260929280`  
Analysis commit: `078199c6f7b789159bfd3b522c67fcc2a3bc93b2`  
Result artifact: `9022799302`  
Artifact SHA-256: `fe5df18e8a33263a4a63838ebe071f022b434c500a6df2a9c18a62aa8ae2d65f`  
Fresh upstream artifact: `9022276431` (`0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`)

The causal/anti-circularity specification was committed before this run in `reproducibility/bombus_relaxation_analysis_spec_2026-08-08.md`.

## Primary directional relaxation test

Fixed design: 25-km response-blind local graph, same held-out spatial fold, five-species common support, environmental distance <= 0.75, continuous all-five mean habitat-support rank, and 1000 cross-fitted environment + SPDE posterior predictive flower maps as the natural null.

Pigmentation presence:

- eligible local pairs: 2,423;
- observed partial beta: 0.03036;
- null mean beta: -0.00072;
- one-sided empirical P = 0.15884;
- two-sided P = 0.31768;
- observed incremental R2 = 0.000715;
- null mean incremental R2 = 0.000726;
- observed mean high-support minus low-support pigmentation-share difference = -0.01629.

**Conclusion:** the prespecified directional prediction `higher total Bombus habitat support -> higher pigmentation` is not supported beyond the abiotic + spatial natural null. The fresh SDM analysis therefore does not support a simple total-availability relaxation gradient.

## Required local community-turnover corroboration

At the same fixed 25-km environmentally matched pair set:

- edges: 2,425;
- observed standardized partial beta for Bombus community fingerprint turnover = 0.09744;
- null mean = -0.00301;
- one-sided empirical P = 0.001998;
- two-sided P = 0.003996;
- BH q at the primary 25-km turnover tests = 0.003996;
- observed incremental R2 = 0.00707;
- null mean incremental R2 = 0.000797;
- empirical P for incremental R2 = 0.004995.

The composition-only Hellinger turnover sensitivity is at least as strong:

- beta = 0.11137;
- empirical P = 0.000999.

The fingerprint-turnover signal is spatially stable across the fixed scale sensitivities:

- 10 km: beta = 0.11540, P = 0.004995;
- 25 km: beta = 0.09744, P = 0.001998;
- 50 km: beta = 0.08806, P = 0.001998.

**Conclusion:** local white/pigmented-state turnover is more strongly associated with turnover in predicted Bombus community structure than expected from the fitted abiotic + spatial flower-colour model.

## Hierarchical intensity response

No corresponding primary fingerprint-turnover signal occurs for pigmentation intensity among already pigmented cells:

- 25-km beta = 0.00679;
- empirical P = 0.41958.

Thus the supported local association is concentrated on the white-versus-pigmented state rather than on how dark an already pigmented flower is.

## Prespecified guild sensitivities

At 25 km for pigmentation presence:

- widespread (*B. ardens* + *B. diversus*) mean support: beta = -0.03400, P = 0.89311;
- montane (*B. beaticola* + *B. consobrinus* + *B. honshuensis*) mean support: beta = 0.04975, P = 0.05694.

Neither sensitivity replaces the all-five primary result. The near-threshold montane result is not treated as confirmatory.

## Why the previous fixed gate failed

Fresh-support diagnostics:

- all-five mean support: min 0.21318, median 0.52935, max 0.76598;
- all-five maximum within-species rank: min 0.48889, median 0.85057, max 1.0.

Therefore the previous `max(five species ranks) <= 0.33` low-support gate necessarily had zero eligible cells. This reflected species replacement plus an overly stringent max-of-five gate, not evidence that Bombus effects were zero. The threshold was not changed after inspection.

## Scientific interpretation and manuscript rule

Two hypotheses must now be kept distinct.

1. **Simple Bombus-availability relaxation:** lower total predicted Bombus habitat support should be associated with white flowers. This directional prediction is **not supported** by the fresh SDM local analysis.
2. **Pollinator-community turnover:** geographic replacement in the predicted Bombus assemblage should coincide with local flower-colour state turnover beyond abiotic + spatial expectation. This prediction is **supported**, robustly across 10/25/50 km, but only for the white/pigmented state.

The positive community-turnover result must not be presented as if it rescued or confirmed the failed directional relaxation test. It supports a different, weaker ecological statement: geographically varying Bombus assemblages are associated with flower-colour state turnover in a way not reproduced by the fitted abiotic + spatial flower model.

Because the Bombus surfaces are SDM predictions generated from environmental predictors, the result does not identify a causal pollinator effect. It is compatible with pollinator-mediated geographic selection, but unmeasured/shared environmental structure can remain. Direct visitation, species-specific colour preference/effectiveness, or an independent island accessibility contrast is required to establish the causal mechanism.
