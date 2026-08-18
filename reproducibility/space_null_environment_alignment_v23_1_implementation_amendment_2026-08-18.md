# Implementation amendment to v23 — exact basis, fixed pairs and shared null draws

**Amendment version:** `v23.1_exact_basis_fixed_pairs_shared_draws`  
**Parent specification:** `space_null_environment_alignment_spec_v23_2026-08-18.md`  
**Status:** implementation clarification committed before the new null-side sensitivity run.

This amendment does not change the central hypothesis, responses, five geographical folds, space-only model, seed, posterior-predictive sample size, test direction or the status of the merged PR #50 result. It closes implementation and provenance ambiguities discovered while translating v23.0 into an executable workflow.

## 1. Preserve the exact historical result

Run A is evaluated with the exact implementation that generated the merged PR #50 result:

- environmental basis: `v16_environment_terms(50)`;
- realised columns: `broad50km_pc1`, `broad50km_pc2`, `within50km_pc1`, `within50km_pc2`;
- training-fold standardisation;
- existing pair generator and pair seed rule;
- five equal-count geographical-distance bins;
- all 25 fold-by-bin strata.

The workflow requires Run A to reproduce the accepted observed contrast, null median, excess and posterior-predictive P within the existing numerical tolerance. The later six-score metadata description is not used to define or reinterpret Run A.

## 2. Diagnose confounding on the metric and pairs actually tested

The descriptive eight-axis, all-pairs diagnostic in v23.0 remains part of the provenance record, but it is not treated as the direct diagnostic of the merged test. The executable analysis additionally calculates, for every response, fold, component, geographical-bin resolution and bin:

- Spearman correlation between geographical distance and the exact environmental-distance component;
- geographical-distance difference between upper- and lower-environmental-distance quartiles;
- geographical-distance range and median.

This is calculated on the fixed held-out pair table used by the corresponding test.

## 3. Keep pair identities fixed across every sensitivity

Pairs are generated once per response and held-out fold using the merged PR #50 rule and seed. The same pair identities are reused for 5, 10 and 20 geographical bins, for the nearest-bin exclusions, for the final-eight omnibus and for the named-axis diagnostics.

This means any profile difference is caused by the stratification or environmental-distance definition rather than a new pair sample.

## 4. Reuse one spatial-null fit rather than refitting for binning

The intercept + Matérn SPDE null does not contain `geo_bins`; binning is a post-fit summary of posterior-predictive maps. The implementation therefore fits the same ten cross-fitted nulls as Run A—two responses by five held-out folds—once, then evaluates every stratification on the identical 500 posterior-predictive maps.

This improves the comparison because:

- A–F share the same posterior-predictive realisations;
- Monte Carlo differences cannot be mistaken for binning effects;
- the expensive model fit is not repeated when the fitted model is unchanged;
- future stratum subsets can be reconstructed without refitting.

The workflow persists both held-out site-level draws and per-draw, per-stratum null contrasts.

## 5. Execute v23.0 A–F and one fixed-exclusion guardrail

The declared runs remain unchanged:

| Run | Bins | Subset |
|---|---:|---|
| A | 5 | all strata |
| B | 5 | resolution-specific nearest bin removed |
| C | 10 | all strata |
| D | 10 | resolution-specific nearest bin removed |
| E | 20 | all strata |
| F | 20 | resolution-specific nearest bin removed |

Because the resolution-specific nearest bin removes approximately 20%, 10% and 5% of pairs at 5, 10 and 20 bins respectively, the implementation also reports—but does not substitute for A–F—a fixed guardrail. It flags the pairs assigned to the nearest bin under the published five-bin design and removes those same pair identities from the 10- and 20-bin summaries.

No subset is promoted on the basis of its P value.

## 6. Extend the same profile to the corrected environmental bases

The workflow reports three logically separate result families:

1. **legacy four-PC omnibus:** historical Run A and v23 A–F sensitivities;
2. **final-eight-axis omnibus:** exact alignment with the observation-level Broad predictor set;
3. **final-eight named axes:** unsigned marginal environmental-distance components, with raw P, BH q and shared-draw maxT FWER P within each response and profile.

The named-axis tests do not provide direction. Direction continues to come from the observation-level environment + SPDE coefficients. Because the axis-distance tests are marginal rather than mutually adjusted environmental contrasts, they are reported as attribution sensitivities, not as independent causal effects.

## 7. Reporting hierarchy

- The observation-level environment + SPDE models remain the source of directional environmental associations.
- Run A remains the historical primary for the legacy four-PC sensitivity.
- A–F are read as a profile of excess magnitude, direction and posterior-predictive support, not as six isolated significance tests.
- The final-eight omnibus and axis tests determine whether the merged legacy result transfers to the named environmental basis used by the final Broad model.
- P-value threshold crossing is secondary to whether the excess changes sign, collapses in magnitude or is confined to a specific geographical stratum.

## 8. Claim ceiling

All original claim ceilings remain in force. A positive excess means that fitted spatial continuity alone does not reproduce the observed environmental ordering under that distance definition. It does not establish selection, local adaptation, plasticity, neutral drift, a genetic divergence statistic or a unique causal environmental mechanism.
