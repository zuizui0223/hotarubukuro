# Executed local-colour-clustering and bumblebee-boundary probe (2026-11-07)

**Status: executed on recovered original pipeline artefacts, exploratory observational diagnostics; NOT a direct test of flower constancy or positive frequency-dependent selection (PFDS).** The frozen 67-pair primary inference, original SDMs, and main manuscript estimates have not been changed.

## Data provenance (actual downloads and independent recomputation)

- GitHub Actions run [32233065109](https://github.com/zuizui0223/hotarubukuro/actions/runs/32233065109), retained artefact **9358236910**: `continuous_isolation_cell_metrics.csv`; 1,305 cells, 1,922 image observations, exact coordinates and sample counts.
- Run [31538548679](https://github.com/zuizui0223/hotarubukuro/actions/runs/31538548679), retained artefact **9119773035**: `bombus_pairs_with_final8_distance.csv` and final-eight-axis distance audit; 67 fixed pure 5-km pairs, their sampling counts and within-pair focal-*Bombus* SDM differences. Source is frozen before this analysis.
- Independent cross-check from run [32233065078](https://github.com/zuizui0223/hotarubukuro/actions/runs/32233065078), artefact **9358305657**: `sharp_transition_nonoverlapping_pairs.csv`, same 67-pair endpoints and signed effects.
- Supplementing with `natural_presence_probability` from the continuous-isolation table is possible descriptively, but 10,000 **joint** spatial predictive-map realizations for this specific edge-clustering statistic were not retrieved. These diagnostics must not be described as posterior-predictive spatial-null results.

All data are public derived outputs, not new independent field data. Artifact downloads are retention-bound; the document records immutable Actions run/artifact IDs. No newly imputed or invented observation was used.

## Test A — does the sharp-pair sample measure population-level morph dominance?

Among the 67 non-overlapping selected 5-km pure *observed* white/pigmented transitions:

| Pair endpoint observation support | Count |
| --- | ---: |
| Both sides have exactly one image | **49/67** |
| At least one side has one image | **66/67** |
| Both sides have at least two images | **1/67** |
| Both sides have at least three images | **0/67** |

Across all 1,305 cells, **947** have only one observed image. **88** cells have both colour states observed; the pipeline's `pigmented` label means *at least one* pigmented image in that cell, not morph-frequency majority or local fixation.

**Interpretation:** the 67 observed pure transitions are real sample contrasts, but nearly all are based on little within-cell replication. They cannot estimate population morph frequency, prove local fixation or demonstrate ecologically stable sharp boundaries.

### Domain support at the selected boundaries

For each of the 67 fixed pairs, independently count *other* pure-colour cells within 5 km of the white and pigmented endpoints (excluding the focal pair).

- **7/67** have at least one other observed same-colour cell on **both** sides;
- **0/67** have two other same-colour cells on **both** sides.

Low density of independent nearby photographic cells prevents a confident description of two stable neighbouring single-morph populations. These values are descriptive, conditional on existing fixed pairs, not P-values.

## Test B — are nearby observed flower colours clustered beyond coarse regional proportions?

Independent of Bombus predictions, construct an **undirected graph of all observed 1-km cells within 5 km**, based on `x_km,y_km` only, before looking at labels. This has **1,134 edges**, of which **916 (80.776%)** connect the same observed binary colour class. The classification is the pipeline's `any pigmented vs none`, so sensitivity to photographs-per-cell matters.

Null: **10,000 label shuffles within spatial grid blocks**, preserving the block-level number of each binary colour and the site geometry. Main diagnostic is 50-km unshifted blocks; compare other block widths and offsets, plus a block × single-image-vs-multiple-images stratification. No labels or local neighborhoods are selected using Bombus values. One-sided Monte Carlo P = (1 + number of permuted fractions >= observed fraction)/(10001).

| Graph and shuffled null | Observed same-colour fraction | Null mean | Upper-tail P |
| --- | ---: | ---: | ---: |
| All 1,134 edges; 50-km block labels | **0.8078** | 0.7556 | **0.00010** |
| 549 edges where each endpoint has one image; 50-km blocks | **0.8015** | 0.7557 | **0.0128** |
| All edges; 25-km blocks | **0.8078** | 0.7732 | **0.00100** |
| All edges; 50-km block × photo-count stratum | **0.8078** | 0.7581 | **0.00020** |
| All edges; 100-km blocks | **0.8078** | 0.6973 | **0.00010** |

These comparisons are positive under the examined block widths and simple sampling-effort control; 50-km shifted-block and 100-km shifted-block checks also retain the direction. **The null removes only coarse-scale blockwise colour counts, NOT fine-scale abiotic environment, geography, historical dispersal, spatially autocorrelated ancestry, observational process or the fitted full-SPDE joint spatial covariance.** Treat all P-values as **exploratory and related tests**, not as independent confirmatory PFDS evidence.

**Positive empirical finding:** locally observed flower colours are more spatially clustered than shuffled label configurations with the same regional colour counts. This is a necessary-compatible spatial signature for local reinforcement, but not a unique diagnostic of pollinator-mediated frequency dependence.

## Test C — does the prior directional two-species SDM mean survive the narrower local comparisons?

Keep the original signed exposure `delta_occmax = max(A_ardens,A_diversus)_{pigmented} - max(A_ardens,A_diversus)_{white}`; 100,000 *exploratory* within-pair sign-flip simulations in each subset. These are **not** additional pre-registered primary tests.

| Fixed-pair subset | n | Mean delta | Median delta | Exploratory one-sided P |
| --- | ---: | ---: | ---: | ---: |
| All pairs (published frozen primary result P=0.02716) | 67 | +0.03590 | -0.00277 | ~0.028 in new Monte Carlo |
| At least one endpoint has >=2 images | 18 | +0.02167 | -0.01271 | 0.357 |
| Both endpoints have >=2 images | 1 | -0.01157 | -0.01157 | Not estimable |
| Lower half of final-eight-axis environmental distance (at/below median) | 34 | **+0.00259** | -0.00904 | **0.433** |
| Absolute elevation difference <=50 m | 23 | +0.01429 | -0.00277 | 0.124 |
| Both endpoints have one extra same-colour neighbour each | 7 | +0.01958 | +0.00538 | 0.305 |

The original reported P = 0.02716 remains **unchanged**. The small subset differences and their post-hoc P-values do **not** establish absence of a focal-Bombus effect; nevertheless the observed positive mean is not robust to environmental-similarity or photographic-replication restrictions.

Importantly, the *positive frequency-dependence hypothesis is symmetric*: it does **not** predict positive signed `delta_occmax` toward the pigmented side. The null directional result in closer environmental pairs therefore does **not** falsify PFDS, but it does weaken the proposed independent directional SDM mechanism.

## Concrete field-validation shortlist recovered from the same table

Exactly **88** 1-km cells contain at least one photographed white and one photographed pigmented flower; **19** contain at least two image observations of **each** colour (five contain at least three of each). The stand-alone script exports these 19 reproducible, location-coded candidates as `mixed_colour_1km_candidate_cells.csv` for **validation of within-population coexistence**, not proof that morphs co-occur on one plant or within a reproductively interacting population. Before a flower-constancy experiment, check photograph identities, dates, distances within each cell, independent observers, taxonomy, and wild versus planted provenance.

## Inference verdict and next required evidence

1. **Supported as observational description:** 5-km colour clustering beyond coarse regional colour counts, and presence of sharply discordant photographed cells.
2. **Insufficient for stable state boundaries:** 66/67 sharply discordant selected pairs have only a single image on at least one side; independent repeated local patch support is sparse.
3. **Not demonstrated:** frequency-dependent visitation, flower constancy on these two colour morphs, positive frequency-dependent *per-flower* pollen transfer, morph-specific fitness, local genetic fixation, or an effect beyond a **joint** fitted environment+SPDE spatial posterior-predictive reference.
4. **Selection cannot be inferred** from spatial clustering or the 67-pair directional SDM sign flips.

The decisive test would experimentally vary local white/pigmented frequency at the **same site and environmental conditions**, track individually identifiable bumblebees' observed within-bout transitions and compare transition probabilities against offered-colour frequency, then measure per-flower pollen deposition and reproductive success. Record UV/bee-perceived colour discrimination, rewards, spatial display configuration and bee identity. Historical spatial structure and current environment also require independent control.

### Reproducibility

Standalone script in this PR: `scripts/check_bombus_constancy_probe.py`. Install `numpy pandas scipy`. Download the specific public GitHub Actions artifact ZIPs listed above, extract the two named CSVs and run:

```bash
python scripts/check_bombus_constancy_probe.py \
  --cells continuous_isolation_cell_metrics.csv \
  --pairs bombus_pairs_with_final8_distance.csv \
  --out results/bombus_constancy_boundary_probe \
  --nperm 10000 --seed 20261107
```

It produces `summary.json`, `block_permutations.csv`, `boundary_context.csv` , `bombus_subset_sensitivity.csv` and `mixed_colour_1km_candidate_cells.csv`. No existing pipeline estimator is altered.

Citations for mechanism: Smithson & Macnair 1996 and 1997 on *Bombus terrestris* artificial-flower frequency dependence. Applicability to *C. punctata* pigmentation is an untested biological assumption. Inoue & Amano 1986, Kobayashi et al. 1997 and Nagano et al. 2014 support mainland bumblebee pollination importance, **not floral colour choice**.
