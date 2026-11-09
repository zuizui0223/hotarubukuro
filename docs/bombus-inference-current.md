# Bumblebee habitat and photographed flower morphs

## Exploratory regional morph-frequency analysis

In central Honshu (875 photographs in 583 cells), the odds of a photographed flower being pigmented increased with the mean national suitability rank across five species (OR 1.63 per 0.1 increase, 95% CI 1.10–2.42). The elevation-adjusted estimate was 1.29 (0.48–3.46). With elevation and space accounted for and the two group means fitted together, the corresponding ORs were 1.66 (1.09–2.53) for the two widespread species and 0.55 (0.27–1.15) for the three montane species.

When the widespread species were entered jointly as species-specific predictors, retaining the montane mean, elevation and spatial adjustment, higher predicted habitat suitability for *B. diversus* was associated with a higher recorded frequency of pigmented flowers (OR 1.29, 95% CI 1.04–1.59). The estimate for *B. ardens* was 1.11 (0.73–1.69). The direct slope contrast was inconclusive (ratio of odds ratios 1.16, 0.81–1.65). This geographic association is consistent with the predicted direction. Testing selection on flower colour requires evidence that visitation changes the relative reproductive success of the morphs.

## Reproduce the regional estimates and Figure 3

The standard pipeline runs this stage after building the 1-km cell table. From the repository root with those two generated inputs available, the individual command is:

```sh
Rscript scripts/run_regional_bombus.R results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv results/analysis_cells/analysis_cells_1km.csv results/regional_bombus/effects.csv
```

The fixed list in `config/central_honshu_cells.txt` records the 584 cell IDs of the earlier five-surface overlap footprint. Of these, 583 occur in the current cohort. The region was chosen after earlier exploration. All five current surfaces cover all 1,305 cells. Species-specific suitability ranks are calculated across those national cells before subsetting. The response is pigmentation state in individual photographs. Models include eight environmental covariates, year, smooths for day of year and coordinates, and a cell random intercept. The elevation-adjusted models add a smooth of elevation. Each species-specific model retains the montane mean. The direct comparison uses the joint covariance of the two widespread-species slopes. Approximate intervals are conditional on the fitted SDM surfaces.

The output CSV contains odds ratios, confidence limits and two-sided Wald tests. The script also writes `regional_bombus.png` beside the CSV. The supplementary boundary comparison below uses an occurrence-site reference and a two-species maximum.

## Pre-specified local boundary comparison

Primary design: 1-km flower cells; up to five nearest neighbours; 5-km radius; pure observed white-pigmented transitions; greedy non-overlap; pair selection without Bombus values or transition direction; orientation only after pair selection; mean pigmented-minus-white occurrence-referenced max(*B. ardens*, *B. diversus*) support; 100,000 sign flips.

Raw SDM values are not treated as abundance, visitation or pollination pressure. Occurrence-referenced support is a within-species relative habitat-support scale, not occurrence probability, abundance, visit rate, pollen transfer, pollination effectiveness or selection pressure.

## Final-eight-axis environmental balance diagnostic

Environment is used only as a descriptive balance diagnostic, never to select, orient or weight the fixed transition pairs. In the finalized eight standardized Broad abiotic axes, median RMS distance is **0.244 versus 0.318** for selected versus all eligible local edges at 5 km. The same ordering holds at 10 km (0.337 vs 0.429) and 25 km (0.435 vs 0.531).

Audit evidence: workflow `31538548679`, artifact `9119773035`, SHA-256 `51fc846d2f3d815d8bbf3c0b95647eabeb0acf731f1e6dd164c11a0dfe1b896f`.

## Boundary results

Primary 5-km pure-transition result: 67 pairs; median separation 2.0 km; mean contrast +0.0359; median -0.00277; 49.3% positive; one-sided P=0.0272; BH q across 5/10/25-km pure tests=0.0815. Raw-cloglog support does not reproduce the result (P=0.267), and the mean attenuates strongly at 10 and 25 km.

The manuscript therefore describes **weak, highly local, magnitude-driven consistency** with the local boundary prediction. It is not evidence of pollinator-mediated selection. No persuasive Bombus relationship is retained for conditional intensity among already pigmented flowers; if the weak local signal is biological, it is more consistent with maintenance/loss of a pigment state than progressive darkening.

## Five-species and montane guardrails

Five-species Hellinger turnover is supplementary because it is unsigned. Under occurrence-referenced matching, matched excess is +0.0330 at 5 km (P=0.0628), +0.0327 at 10 km (P=0.0142) and +0.0411 at 25 km (P=0.00010). These associations locate colour boundaries within pollinator-biogeographic transition zones but cannot determine which side should be pigmented and do not strengthen the signed Main result into a causal mechanism.

Montane/alpine national overlap is a negative guardrail. Among near-equal-elevation transitions (<=50 m), mean montane contrasts are near zero or negative and all one-sided P values are >=0.755. The appropriate interpretation is shared elevational geography rather than an additional pollinator mechanism.

## Causal ceiling and next tests

All Bombus surfaces are environment-based SDM predictions. The study does not measure realized visitation, abundance, flower choice, pollen deposition, seed set or fitness. Direct tests require species-resolved visitation, floral contact, pollen transfer, fitness, visible/UV spectra, receptor-based contrasts and local abiotic measurements.
