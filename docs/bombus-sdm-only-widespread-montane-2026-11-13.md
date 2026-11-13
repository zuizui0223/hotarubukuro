# SDM-only widespread versus montane Bombus contrast — executed 2026-11-13

**Decision:** When the exact central-Honshu subset and the original national-rank SDM scale are used with *no environmental predictors*, the previously positive adjusted widespread-group coefficient changes direction. This is **specification dependence, not proof of a negative bumblebee effect**.

## Reproducible source identity

- Source: [successful GitHub Actions reproduction #42953552095](https://github.com/zuizui0223/hotarubukuro/actions/runs/42953552095), artifact **13831541196**, `analysis_cells/analysis_cells_1km.csv`.
- Fixed central-Honshu cohort: repository `config/central_honshu_cells.txt`; 584 listed identifiers, of which exactly **583** appear in the 1,305-cell cohort, representing **875** of 1,922 photographs. The same subset was used in the older regional Figure 3.
- Explanatory data are solely the five existing `bee_{species}` SDM columns in that exact table. For each species, compute `rank(SDM)/1305` across **all national cells before regional subsetting**. Take the mean of *B. ardens* and *B. diversus* and the mean of *B. beaticola*, *B. consobrinus* and *B. honshuensis*, each divided by 0.1.
- Response: photographed counts of pigmented versus white flowers per 1-km cell, binomial logit with both group predictors fitted together. Each coefficient and its odds ratio refer to a 0.1-unit increase in the respective mean national rank **holding the other group's SDM index constant**.
- The original regional R models included eight environmental predictors, day/year, coordinate smooths and cell random effects, and an elevation smooth in the adjusted comparison. **They are not the models fitted here.** No environment, elevation, day, year, geographic trend, spatial random effect or plant-history covariate is included in the new point-estimate model.
- Uncertainty: 50-km spatial-block cluster-robust sandwich errors, plus a **diagnostic** 10,000-draw shuffle of paired flower-colour counts (numerator and total together) within fixed 50-km regions, maintaining all locations and five SDM values. Block width/offset sensitivities use 2,500 permutations each. Spatial blocks are a resampling/null-design device, not predictive covariates.

## Main SDM-only result: exactly matched national-rank scale

| Sample | Predictor | OR per 0.1 rank (95% block-robust CI) | Wald P |
| --- | --- | --- | ---: |
| National: 1,305 cells, 1,922 photos | Widespread two | **0.6569 (0.5438–0.7934)** | 0.000013 |
| National | Montane three | 1.0409 (0.9374–1.1557) | 0.453 |
| National | Widespread versus montane coefficient, OR ratio | **0.6311 (0.5364–0.7424)** | 2.79e-8 |
| Central Honshu: 583 cells, 875 photos | Widespread two | **0.7578 (0.5658–1.0147)** | 0.0626 |
| Central Honshu | Montane three | 1.0890 (0.8962–1.3232) | 0.391 |
| Central Honshu | Widespread versus montane coefficient, OR ratio | **0.6958 (0.5559–0.8709)** | 0.00154 |

The negative widespread association at the national scale is compatible with the independent earlier nationwide descriptive Spearman correlation (focal-two occurrence-reference score against observed pigmented fraction ≈−0.306), but those are different statistics.

**Important distinction:** In the central-Honshu environment/spatially adjusted model already committed to `main`, the *same five species* were grouped and yielded widespread OR **1.6594** (95% CI 1.0904–2.5253), montane OR **0.5541** (0.2660–1.1541), and montane-to-widespread OR ratio **0.3339** (0.1382–0.8067). The old and new estimates go in **opposite directions**. Do not describe them as independent replicates; the interpretation depends critically on covariates and conditional comparisons.

## Spatial block-shuffle diagnostic: strong geography and block sensitivity

For the **Widespread coefficient minus Montane coefficient**, a two-sided tail around the null median is reported (exploratory: block scales were not a prior confirmatory test of this specific two-group contrast).

| Spatial permutation | National P | Central Honshu P |
| --- | ---: | ---: |
| 50-km block, unchanged origin, 10,000 shuffles | **0.0288** | **0.1107** |
| 25-km block, 2,500 shuffles | 0.0752 | 0.2639 |
| 50-km block, shifted 25 km, 2,500 shuffles | 0.0144 | 0.1307 |
| 100-km block, 2,500 shuffles | 0.0004 | 0.0004 |
| 100-km block, shifted 50 km, 2,500 shuffles | 0.0004 | 0.0004 |

**Inference ceiling:** The national negative association differs from some 50–100 km block-based null expectations, but is weaker against more spatially restrictive 25-km permutations. In central Honshu, the 25–50 km nulls do **not** robustly support a difference beyond retained local geographic colour structure. Wald intervals and block permutations answer different questions; prefer the latter for diagnosing inherited spatial geography and do not treat them as two independent confirmations of selection.

## Sampling sensitivity

- **National, cells with ≥2 photographs:** 358 cells / 975 photos. Widespread OR 0.6309 (0.5107–0.7793); montane OR 0.9913 (0.8679–1.1322).
- **Central Honshu, cells with ≥2 photographs:** 169 cells / 461 photos. Widespread OR 0.6969 (0.5151–0.9429); montane OR 1.0413 (0.7863–1.3790).
- Counting each cell equally instead of weighting by photographed flower counts retains the widespread negative sign: national OR 0.6657, central OR 0.7950.
- The two group indices are strongly anticorrelated (national `r≈−0.70`; central `r≈−0.76`), so partial coefficients require caution. An odds ratio for one SDM group is **not** the outcome of a field manipulation where other pollinators remain fixed.

The regional cohort was originally selected after exploration; current small P-values are not pre-registered confirmatory results.

## Scientific interpretation and decision for the manuscript

1. The predicted occurrence of colour states, as measured from photographs, is associated with SDM geography in some comparisons. **The sign is not invariant** to the regional model's environmental/spatial adjustments.
2. The 5-km five-species SDM-composition turnover test from the separate analysis remains unsupported (`rho=−0.0134`, block-shuffle `P=0.3934`). A directional group-suitability slope is not a test of guild turnover.
3. Bumblebees may be functionally important pollinators of mainland *Campanula punctata*, but SDM values represent relative predicted habitat suitability, not visitation, pollen deposition, behaviour or selection. All species' SDMs are built from environmental geography. **No SDM-only analysis can separate their effects from shared environmental causes or genetic population structure.**
4. Positive frequency-dependent selection through bumblebee flower constancy remains **untested**; it requires colour-frequency manipulation and per-flower reproductive consequences.
5. **Editorial decision:** Keep the originally reported positive *adjusted* result transparently as its own regionally conditional association, and report this SDM-only reanalysis and spatial-null sensitivity as a strong caution. Neither design is evidence that the two widespread Bombus taxa generally select pigmented rather than white flowers. Do not promote the new negative coefficient into a causal explanation merely because a Wald P-value is small.

## Executable script and actual outputs

Run after the canonical analysis-cell table has been generated:

```bash
python -m pip install -e '.[guild]'
python scripts/run_bombus_sdm_group_comparison.py \
  --cells results/analysis_cells/analysis_cells_1km.csv \
  --region config/central_honshu_cells.txt \
  --out results/bombus_sdm_group_comparison \
  --nperm 10000 --sensitivity
```

The script writes `group_sdm_only_effects.csv`, `group_block_permutation.csv` and `scope.json`; it stops if the frozen population counts (1,305/1,922 nationally, 583/875 centrally) do not match.

This report describes **computed output**, not a proposed analysis. No locked original model coefficients, Figure 3, canonical SDM source builders, or published manuscript numerical targets were overwritten.
