# Bumblebee habitat and photographed flower morphs

## New SDM-only five-species local co-transition test (2026-11-13)

**Primary 5-km result is not supported.** A five-species occurrence-referenced SDM profile was available for all 1,305 flower cells in the validated 2026-11-11 reproduction artifact. The independently executed SDM-only comparison of local predicted guild-profile turnover against photographed pigmentation difference yielded **Spearman rho −0.0134 across 1,134 cell pairs**, with upper-tail `P=0.3934` in 10,000 within-50-km-block colour-label randomisations. Restricting to the 107 pairs with at least two photographs at each endpoint gives rho −0.0806. The 10-km scale gave a very small positive rho (+0.0114; nominal P=0.0099), but 25 km did not replicate, and the 10-km P was sensitive to permutation-block positioning. Therefore the predeclared strict-local promotion criterion **fails**: no new significant independent Bombus mechanism is asserted.

This is a **separate SDM-only analysis**, unlike the environment/spatially adjusted central-Honshu GAM below. For this new test the only explanatory data are the five predicted Bombus suitability scores; geographic coordinates define the pair graph and permutation units, not environmental covariates. Since the SDMs themselves inherit climate/terrain variation, any co-structure would not establish pollinator selection; flower constancy remains unmeasured. Full source identity, code and outputs: [five-species SDM-only results](bombus-five-sdm-only-results-2026-11-13.md).

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

## Provenance of the publication inputs

The workbook flags every row sharing an exact image SHA-256. The archived analysis sorted by numeric workbook source row and retained the first occurrence per hash. Thus row 515 (obs-000514) was retained and the later obs-000568 was excluded. This is a record-order convention. The two entries have different coordinates, and order does not validate either coordinate. The replay applies the rule during analysis QC and preserves the original workbook flags in the validated table. See the [original analysis script](https://github.com/zuizui0223/hotarubukuro/blob/735de851f46ce5da86f703eb92770ce6223363c5/scripts/run_reanalysis_current_inputs.sh).

The previous fresh-source replay used current GBIF queries in API order. The publication used the 2026-08-08 snapshot sorted and deduplicated by GBIF key, including the genus-wide background. The replay's raw 50-km record count differed for 1,121 of 1,921 shared photographs (e.g. 141 versus 147 for obs-000002). This count is computed directly from occurrence records. An independent GBIF key comparison on 2026-11-11, applying the same country, coordinate, uncertainty and basis-of-record filters, found 81 additional and one missing key for B. diversus (7,317 to 7,397) and five additional keys for B. ardens (2,818 to 2,823) relative to August. The November 11 query is a separate retrieval from the November 9 replay. The flower environment also diverged upstream of the regional model (e.g. unstandardized soil PC1 differed by up to 0.00531 across matched photos). The old runner did not retain its live GBIF CSVs or raster-byte manifests, so it is not possible to assign the entire drift to individual GBIF keys, raster updates, or software versions. The five fitted surfaces also differed. See the [November replay](https://github.com/zuizui0223/hotarubukuro/actions/runs/42604860416/attempts/2).

The checksum-verified archive in `dependencies/figure3_spatial_inputs.zip` contains the publication's GBIF tables and their manifests, fitted rasters and models, selected model indices, observation-level abiotic inputs and duplicate-rule audit. Members are byte-preserving excerpts from artifact 9106183958, SHA-256 `49ed6bafcfed9c2c200689d17232058c4303f52eabb5de938b8b229dffd50b29`, from [the original reconstruction](https://github.com/zuizui0223/hotarubukuro/actions/runs/31501429395). The exposure table predates phenotype assignment. The fitted regional coefficients and Figure 3 are generated from the Zenodo-derived colours. A fresh fit of the bee distribution models using newly downloaded occurrence records addresses a different input vintage.

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
