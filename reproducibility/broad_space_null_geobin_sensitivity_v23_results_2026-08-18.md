# v23 result — environment-aligned divergence beyond spatial continuity

Date: 2026-08-18  
Specification: `v23.0_environment_alignment_beyond_space`  
Implementation amendment: `v23.1_exact_basis_fixed_pairs_shared_draws`

## Question and hierarchy

This analysis distinguishes three inferential objects.

1. The observation-level environment + SPDE models estimate **directional partial associations** for named environmental terms.
2. The legacy four-PC spatial-null test asks whether an **unsigned composite multiscale environmental distance** orders held-out phenotype divergence beyond fitted spatial continuity.
3. The final-eight-axis omnibus and named-axis tests ask whether the same type of held-out divergence is reproduced on the exact predictor basis used by the final observation-level model.

The merged PR #50 five-bin legacy result remains the historical primary sensitivity. No later profile is promoted because of its P value.

## Exact execution

- frozen Broad cell artifact: `9022276431`;
- source ZIP SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`;
- five response-blind held-out geographical folds;
- maximum 15,000 fixed held-out pairs per fold;
- one intercept + Matérn SPDE fit per response and fold;
- 500 posterior-predictive maps reused across every stratification;
- seed `20260725`;
- geographical-bin profiles: 5, 10 and 20;
- subset profiles: all strata, resolution-specific nearest bin removed, and the exact pair set assigned to the nearest bin under the published five-bin design removed at every resolution.

The successful workflow reproduces the eight accepted legacy Run A numerical locks within `5e-4` and stores the site-level maps plus the full draw-by-stratum null matrix.

## 1. Legacy four-PC A–F profile

The realised legacy basis is:

- `broad50km_pc1`;
- `broad50km_pc2`;
- `within50km_pc1`;
- `within50km_pc2`.

It is not the final eight-axis basis and is not the later six-score metadata description.

### Pigmentation state

| Run | Geographical bins | Strata | Observed contrast | Space-null median | Excess | One-sided P |
|---|---:|---|---:|---:|---:|---:|
| A | 5 | all | 0.106802 | 0.058240 | **+0.048562** | **0.03393** |
| B | 5 | nearest bin removed | 0.117247 | 0.065822 | **+0.051425** | 0.05190 |
| C | 10 | all | 0.096925 | 0.057253 | **+0.039672** | **0.03992** |
| D | 10 | nearest bin removed | 0.113145 | 0.068272 | **+0.044873** | **0.03992** |
| E | 20 | all | 0.096718 | 0.058223 | **+0.038495** | **0.03992** |
| F | 20 | nearest bin removed | 0.105519 | 0.062942 | **+0.042578** | **0.04192** |

The excess remains positive in all six runs and changes little in magnitude. Finer geographical stratification does not remove the result. Removing the nearest five-bin stratum preserves the excess but moves the one-sided posterior-predictive probability just above 0.05.

### Conditional intensity

| Run | Geographical bins | Strata | Observed contrast | Space-null median | Excess | One-sided P |
|---|---:|---|---:|---:|---:|---:|
| A | 5 | all | -0.047179 | -0.001286 | -0.045892 | 0.87226 |
| B | 5 | nearest bin removed | -0.052332 | -0.002505 | -0.049828 | 0.85230 |
| C | 10 | all | -0.052896 | -0.003129 | -0.049768 | 0.89820 |
| D | 10 | nearest bin removed | -0.061917 | -0.004431 | -0.057486 | 0.92216 |
| E | 20 | all | -0.055165 | -0.003890 | -0.051275 | 0.90220 |
| F | 20 | nearest bin removed | -0.057334 | -0.003438 | -0.053896 | 0.90619 |

Conditional intensity remains negative under every profile and never approaches the predefined upper-tail alternative.

## 2. Fixed nearest-pair exclusion guardrail

Dropping the resolution-specific nearest bin removes about 20%, 10% and 5% of pairs at 5, 10 and 20 bins. A separate guardrail therefore removes the same pair identities—the pairs assigned to the nearest bin in the published five-bin design—at every resolution.

For pigmentation state:

| Bins | Excess | One-sided P |
|---:|---:|---:|
| 5 | +0.051425 | 0.05190 |
| 10 | +0.042921 | 0.05389 |
| 20 | +0.042902 | 0.05389 |

The effect size remains positive and stable, while support is consistently borderline rather than decisively below 0.05. This is the correct limitation statement for the historical result.

## 3. Exact confound diagnostic

The diagnostic was recalculated on the exact response-specific held-out pairs and exact environmental metrics used in the tests.

For pigmentation state under the published five-bin legacy basis, mean fold-specific Spearman correlations between geographical and environmental distance were:

| Geographical bin | Mean Spearman correlation |
|---:|---:|
| 1 | 0.313 |
| 2 | -0.021 |
| 3 | -0.069 |
| 4 | 0.122 |
| 5 | 0.028 |

The nearest bin is the strongest repeated source of positive correlation, but the exact held-out diagnostic does not support the literal claim that all residual imbalance is confined there. Some fold-specific imbalance also remains in wider or tail strata.

This does not mean the test simply rediscovers geographical distance. The observed and posterior-predictive contrasts are calculated on the **same exact high- and low-environment pair sets**. Any difference in their geographical-distance distributions contributes to the space-only null contrast as well. For example, the positive Run A space-null median is 0.058240; the test asks whether the observed contrast exceeds that geography-conditioned expectation. The final-eight omnibus provides an additional negative control: it has stronger geography–environment correlation in the nearest bin but produces no phenotype excess.

The sensitivity conclusion is therefore based on the excess profile, not on a claim of perfect geographical balance.

## 4. Final-eight-axis omnibus profile

### Pigmentation state

| Bins | Strata | Excess | One-sided P |
|---:|---|---:|---:|
| 5 | all | -0.000458 | 0.51497 |
| 5 | nearest removed | -0.012220 | 0.79042 |
| 10 | all | -0.008328 | 0.75250 |
| 10 | nearest removed | -0.010513 | 0.78443 |
| 20 | all | -0.004277 | 0.63673 |
| 20 | nearest removed | -0.005190 | 0.64671 |

### Conditional intensity

The all-strata P values are 0.51297, 0.55289 and 0.61277 at 5, 10 and 20 bins. No profile supports an omnibus excess.

Thus the legacy four-PC result does not transfer to an equal-weight Euclidean distance across the final eight named axes.

## 5. Named-axis attribution

### Pigmentation-state Temperature PC1 profile

| Bins | Strata | Excess | Raw P | BH q across eight axes | Shared-draw maxT FWER P |
|---:|---|---:|---:|---:|---:|
| 5 | all | **+0.052133** | **0.00998** | 0.07984 | 0.07784 |
| 5 | nearest removed | **+0.050212** | **0.02196** | 0.17565 | 0.16367 |
| 10 | all | **+0.045657** | **0.01597** | 0.12774 | 0.09780 |
| 10 | nearest removed | **+0.046793** | **0.01597** | 0.12774 | 0.11976 |
| 20 | all | **+0.043456** | **0.01397** | 0.11178 | 0.09980 |
| 20 | nearest removed | **+0.044565** | **0.01796** | 0.14371 | 0.11776 |

Temperature PC1 is the only final named axis with a repeated raw 5% state signal. Its excess remains positive after finer stratification and nearest-bin removal. It does not cross the eight-axis BH or maxT 5% threshold in any profile.

Under the fixed published-nearest-pair exclusion, raw P remains 0.02196–0.02595 and maxT FWER P remains 0.16367–0.17166.

No other pigmentation-state axis has raw P<0.05 in the published five-bin profile. No conditional-intensity axis is supported; the Temperature PC1 raw P profile ranges from approximately 0.086 to 0.178 after stratification changes, with maxT P at least 0.439.

## 6. Why legacy omnibus and final-eight omnibus differ

The legacy four-PC basis is a compressed multiscale topoclimate–radiation representation. In the frozen 50-km PCA loadings:

- broad PC1 loads approximately +0.709 on elevation and -0.701 on temperature;
- within-neighbourhood PC1 loads approximately -0.571 on elevation, +0.572 on temperature and -0.508 on precipitation;
- the PC2 axes are dominated by radiation, with additional precipitation loading at the broad scale.

The legacy omnibus therefore emphasizes a few integrated geographical gradients. The final-eight omnibus gives equal Euclidean-distance status to temperature, moisture, seasonality, terrain, two soil axes and RSDS. Axes that do not order state divergence can dilute one informative direction. The final-eight omnibus null and the Temperature-axis raw signal are therefore not logically contradictory.

Across the fixed held-out pairs, the legacy four-PC distance correlates most strongly with final Temperature PC1 distance (Spearman approximately 0.53 for pigmentation state), but this correlation is not sufficient to relabel the historical omnibus result as a Temperature-only test.

## 7. Relation to the observation-level full model

The observation-level model remains the source of directional partial associations.

- Pigmentation state: Temperature PC1 mean = -0.54185; 95% CrI = -1.03294 to -0.04859.
- Conditional intensity: precipitation PC1, temperature seasonality, topography PC1 and Temperature PC1 × temperature-seasonality retain 95% CrIs excluding zero.

The pairwise distance tests use absolute environmental differences and are unsigned. They answer a stronger but different question: whether locations separated along an axis are more phenotypically different than the same pair geometry under the space-only model predicts.

The combined state interpretation is therefore:

> The full model identifies a directional cool-climate association. Temperature PC1 is also the only named axis with repeated raw held-out divergence beyond spatial continuity, but the axis-family evidence remains inconclusive after multiplicity correction.

For conditional intensity, credible full-model coefficients coexist with no axis-specific held-out divergence excess. This is compatible with smooth environmental tracking, interaction-surface geometry, conditional-subset selection and/or insufficient power of an unsigned marginal-distance statistic. It is not evidence that the full-model coefficients are false.

## Prediction adjudication

- **P1 — not produced solely by the nearest-distance correlation:** supported in effect direction and magnitude; inferential support becomes borderline under the fixed nearest-pair exclusion.
- **P2 — stable to finer geographical stratification:** supported for the legacy state excess.
- **P3 — no conditional-intensity excess:** supported under all profiles.

## Execution provenance

- workflow: `.github/workflows/broad-space-null-geobin-v23.yml`;
- successful workflow run: `32118428188`;
- artifact: `9317764270`;
- artifact digest: `sha256:3062452997c0717946ddaaa938835ff3fd8bf90d7be3031b60b09c93e5cc9234`;
- analysis script: `scripts/fit_broad_space_null_geobin_sensitivity_v23.R`.

## Claim ceiling

The historical result is a robustly positive but borderline, basis-specific multiscale environmental-alignment sensitivity. The exact final-eight omnibus is null. Temperature PC1 is the strongest repeated named axis, but it is not familywise conclusive. None of these results demonstrates selection, local adaptation, plasticity, neutral drift, a genetic divergence statistic or direct anthocyanin physiology.
