# Executed SDM-only analysis of flower colour and focal bumblebees (2026-11-07)

**This replaces the superseded local-colour-clustering/environment-subset exploration in PR #71.** In the Bombus component, the only explanatory variable is the frozen predicted habitat-suitability index for *Bombus ardens* and *B. diversus*. Other abiotic variables and spatial fields were **not** fitted, adjusted for, balanced, matched on or tested. The geographic radius defines the previously fixed local-pair design, not an explanatory variable. No bumblebee model or primary result was refitted.

## Public data actually retrieved

1. [GitHub Actions run 32233065109](https://github.com/zuizui0223/hotarubukuro/actions/runs/32233065109), artifact **9358236910**: `continuous_isolation_cell_metrics.csv`, providing observed colour counts for 1,305 1-km cells and 1,922 photographed flowers.
2. [GitHub Actions run 32233065078](https://github.com/zuizui0223/hotarubukuro/actions/runs/32233065078), artifact **9358305657**: `bombus_occurrence_reference_support/cell_effective_bombus_support.csv` (all 1,305 focal SDM scores) and `ecological_v18_bombus_local_sharp_transition/sharp_transition_nonoverlapping_pairs.csv` (preselected transition identities, observed colour labels, and original pair SDM contrasts).

The standalone `scripts/check_bombus_sdm_only.py` joins the two cell tables by exact 1-km site ID, checks `n_pigmented+n_white=n_observations`, and verifies that paired SDM differences reproduce the frozen pair CSV before summarizing results. It **does not read environmental columns** from any source table. Uses only the existing `numpy` dependency.

## National: descriptive, one SDM predictor

Response: observed pigmented image fraction (`n_pigmented/n_observations`) in each 1-km flower cell. Predictor: the occurrence-referenced relative support maximum of the two focal bumblebee SDMs. Unadjusted Spearman rank correlation:

- **1,305 cells, 1,922 photographs: rho = -0.305577**;
- raw-cloglog alternative: **rho = -0.300076**.

| Focal occurrence-referenced SDM quartile | Cells | Pigmented images / all | Image-weighted pigmented share | Unweighted cell-mean share |
|:--|--:|--:|--:|--:|
| Q1 (lowest) | 327 | 416 / 521 | **0.7985** | 0.7883 |
| Q2 | 332 | 176 / 450 | 0.3911 | 0.3861 |
| Q3 | 322 | 185 / 457 | 0.4048 | 0.3900 |
| Q4 (highest) | 324 | 179 / 494 | 0.3623 | 0.3638 |

These are **descriptive**, because nearby cells and raw observations are not spatially independent and the SDM is environmentally generated. We do not calculate or report an independent-cells significance P-value for this national correlation. In this dataset the national sign is opposite to the simplistic prediction that higher focal-SDM support always implies more pigmentation. Do **not** infer that bumblebees favor white flowers from this.

## Local: the previously frozen signed SDM-only test

The contrast is `max(A_ardens,A_diversus)_{pigmented} - max(A_ardens,A_diversus)_{white}` at the fixed pure photo-colour transitions. Recomputed means/medians exactly reproduce the original frozen pair data. Monte Carlo sign flips give close but nonidentical P-values because Python and original R use different random-number implementations; the frozen paper P/q are retained unchanged.

| Local pair radius | n pairs | Mean SDM difference | Median | Pairs positive | Original upper-tail P |
|:--|--:|--:|--:|--:|--:|
| **5 km (primary)** | **67** | **+0.035896** | **-0.002770** | **33/67 (49.3%)** | **0.027160** |
| 10 km | 109 | +0.008401 | +0.005376 | 56/109 | 0.32454 |
| 25 km | 171 | +0.002925 | -0.001792 | 85/171 | 0.43575 |

The primary 5-km test has **BH q = 0.081479** across the frozen 5/10/25-km family. For the **raw-cloglog** SDM scale, 5-km mean difference is **+0.004431**, median **+0.002809**, 35/67 positive, frozen **P = 0.267147**. Thus the weak nominal primary mean response is **not** consistent in direction among most individual boundaries and does **not** reproduce under the alternative SDM scale.

Among the selected 5-km photo-pairs, **66/67** have only one image at least one of their two endpoints; only one pair has at least two images on **both** sides. As a result, a photo-colour contrast is not proof of a stable white/coloured population boundary. Image count here is a measurement-reliability audit **not a fitted predictor**.

## Exactly what this can and cannot answer

- **Can answer:** how photographed pigmentation relates descriptively to the focal bumblebee SDM score nationally, and whether the preselected local white-to-pigmented transitions show a directional SDM score shift.
- **Cannot answer:** whether real bees preferentially visit pigmented flowers, whether colour-specific pollen transfer/fitness depends on local morph frequency, or whether bumblebees selected these morphs.
- **Environmental confounding is explicitly a Discussion limitation:** the SDMs derive from climate, terrain and other environmental predictors. A fitted SDM coefficient or contrast therefore **cannot separate pollinator effects from environmental geography**. We intentionally did not try to separate them statistically.
- **Flower constancy remains an untested hypothesis**, not a result from this one-predictor analysis. Positive frequency dependence might reinforce either morph when locally common, not necessarily the pigmented side.

## Reproduce the computed diagnostics

Download the above two artifact ZIPs, extract the named CSV files, then run (from the repository root):

```bash
python scripts/check_bombus_sdm_only.py \
  --cells continuous_isolation_cell_metrics.csv \
  --support cell_effective_bombus_support.csv \
  --pairs sharp_transition_nonoverlapping_pairs.csv \
  --out results/bombus_sdm_only_2026_11_07 \
  --nperm 100000
```

Outputs: `summary.json`, `sdm_only_quartiles.csv`, `sdm_only_pair_results.csv`. The independently computed 5-km occurrence-referenced sign-flip upper-tail `P≈0.0267` differs from the frozen R `P=0.02716` within Monte Carlo simulation variability. Raw-cloglog independent `P≈0.2679`.

### Inference boundary

The nationwide negative and strict local weakly positive mean are based on **different comparison units and selected subsets**; this is not a demonstrated sign reversal of a causal mechanism. No claim of pollinator-mediated selection, species-specific colour preference, population fixation, or positive frequency-dependent fitness follows.

Earlier exploratory spatial label permutations and environmental-conditioned subset tests remain recoverable from Git commit history, but are **not part of the current SDM-only Bombus evidence or manuscript claim**.
