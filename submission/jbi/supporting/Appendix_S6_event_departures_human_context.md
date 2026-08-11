# Appendix S6. Event-based local departures and post-selection human context

## Scope and inference ceiling

This analysis does not define anomalies by selecting large residuals. It defines a repeatable **local ecological configuration**, applies the identical detector to observed and simulated flower-colour maps and examines human variables only after candidate identities are fixed.

The primary inferential questions are:

1. how many pigmented cells occur inside geographically close, environmentally similar white neighbourhoods;
2. how often the fitted broad natural model generates the same event under the observed sampling geometry; and
3. whether the independently fixed candidates occupy human context that differs from their own observed white neighbours more than expected across natural predictive maps.

The result identifies field and provenance targets. It does not demonstrate planting, escape, introgression or horticultural origin.

## Primary ecological event

The unit was the 1-km flower cell from the current 1,305-cell analysis. A focal cell was eligible when it contained at least one pigmented observation and had sufficient local support. The primary neighbourhood was defined without human variables and without using the candidate's fitted residual as a response.

**Table S6.1. Primary event specification.**

| Component | Fixed rule |
|---|---|
| Focal state | at least one observed pigmented flower in the 1-km cell |
| Geographic radius | 10 km |
| Environmental representation | broad50km PC1/PC2 and within50km PC1/PC2 |
| Environmental distance | root-mean-square Euclidean distance across the four standardized PCs |
| Environmental caliper | <=1.0 |
| Minimum neighbouring cells | 3 |
| Neighbour colour condition | every eligible physical neighbour has zero observed pigmented flowers |
| Fold restriction | none in the primary physical neighbourhood; same-fold-only is a sensitivity |
| Human variables | absent from selection, neighbourhood definition and ranking |

The event can include a mixed focal cell; it does not require the focal cell to be entirely pigmented. “All neighbours white” refers to observed cell samples, not proof that the underlying populations are genetically fixed for a white morph.

For each candidate, the pipeline retains the held-out natural-model upper-tail probability (`unexpected_pigmented_q`) and standardized departure (`z`) as diagnostics. Neither quantity is thresholded to determine membership. Consequently, a candidate may be visually discordant in its local ecological neighbourhood without having an extreme marginal predictive tail probability.

The primary neighbourhood includes physical neighbours across blocked-prediction folds because fold membership is a model-validation device rather than an ecological boundary. A same-fold-only reconstruction is reported separately.

## Natural-map calibration

The observed event detector was replayed on 10,000 held-out cross-fitted natural maps using the fixed observed cell geometry and the same neighbourhood rules. Candidate fraction was calculated among cells that were pigmented in a map and had sufficient neighbourhood support. A separate joint posterior-predictive sensitivity combined 10,000 latent spatial draws with 20 observation replicates per draw, producing 200,000 maps.

**Table S6.2. Primary event relative to the natural predictive reference.**

| Reference | Metric | Observed | Null mean | 95% null interval | One-sided P |
|---|---|---:|---:|---:|---:|
| 10,000 cross-fitted maps | candidate count | 17 | 13.614 | 7–21 | 0.19958 |
| 10,000 cross-fitted maps | candidate fraction | 0.04735 | 0.03427 | 0.01746–0.05362 | 0.08739 |
| 200,000 joint posterior-predictive maps | candidate count | 17 | 14.879 | 8–22 | 0.31446 |
| 200,000 joint posterior-predictive maps | candidate fraction | 0.04735 | 0.03925 | 0.02128–0.05898 | 0.19618 |

Neither candidate count nor fraction was a robust excess under the primary cross-fitted reference, and the joint spatial posterior-predictive sensitivity was less extreme. The 17 cells are therefore reproducible local configurations, not evidence that an unmodelled process is required to generate their frequency.

## Event-definition sensitivities

No sensitivity was selected by its P value. Each changes the ecological object as well as the number of eligible cells.

**Table S6.3. Cross-fitted natural-map sensitivity family (10,000 maps).**

| Configuration | Observed count | Count P | Observed fraction | Fraction P | Interpretation |
|---|---:|---:|---:|---:|---|
| Primary: 10 km, env <=1.0, >=3 neighbours, all white | 17 | 0.19958 | 0.04735 | 0.08739 | manuscript event |
| Same-fold-only version of primary | 16 | 0.24598 | 0.04776 | 0.10669 | fold-boundary sensitivity |
| 5 km, env <=1.0, >=3 neighbours, all white | 10 | 0.10819 | 0.05848 | 0.04860 | fraction nominal only; count not significant |
| 25 km, env <=1.0, >=5 neighbours, neighbour pigment share <=0.10 | 25 | 0.00150 | 0.04488 | 0.00070 | different broader/relaxed event, not a primary-result substitute |
| 10 km, env <=0.75, >=3 neighbours, all white | 15 | 0.28217 | 0.04808 | 0.15389 | stricter environmental similarity |
| 10 km, env <=1.5, >=3 neighbours, all white | 18 | 0.20968 | 0.04545 | 0.08239 | looser environmental similarity |

The 25-km configuration detects a broader landscape boundary and permits up to 10% pigmented neighbours; it is not the same local all-white event. Its small P values therefore do not overturn the null primary result. Likewise, the nominal 5-km fraction result occurs without a corresponding candidate-count excess and is retained as a sensitivity rather than promoted to a second Main claim.

## Candidate identities and local support

The table below uses stable 1-km cell identifiers rather than exact coordinates. Exact coordinates remain in the analysis data package for authorized review and field planning. `Pigmented/observed` gives the number of pigmented photographs over the total photographs in the focal cell. `Neighbours/sites` gives the number of environmentally eligible white neighbour cells and the summed number of independent source sites represented by those neighbours. Human-context class is descriptive and was assigned only after candidate selection.

**Table S6.4. Seventeen primary local-departure candidates.**

| Rank | Stable cell ID | Pigmented/observed | Predictive q | z | Neighbours/sites | Mean neighbour distance (km) | Mean environmental distance | 5-km population rank | Post-selection context class |
|---:|---|---:|---:|---:|---:|---:|---:|---:|---|
| 1 | cell-1km-294_247 | 1/1 | 0.082 | 3.367 | 5/6 | 3.897 | 0.518 | 0.790 | DID-proximate, high population |
| 2 | cell-1km--108_-147 | 1/1 | 0.090 | 3.198 | 4/7 | 8.889 | 0.375 | 0.952 | DID-proximate, high population |
| 3 | cell-1km--73_5 | 1/1 | 0.129 | 2.609 | 5/13 | 4.693 | 0.161 | 0.917 | DID-proximate, high population |
| 4 | cell-1km--49_-100 | 1/1 | 0.144 | 2.447 | 4/4 | 7.986 | 0.321 | 0.594 | intermediate context |
| 5 | cell-1km--52_-129 | 1/1 | 0.290 | 1.568 | 3/3 | 7.133 | 0.330 | 0.814 | DID-proximate, high population |
| 6 | cell-1km--599_-255 | 1/1 | 0.298 | 1.538 | 3/3 | 6.219 | 0.478 | 0.900 | DID-proximate, high population |
| 7 | cell-1km--154_-209 | 1/3 | 0.301 | 0.807 | 3/4 | 3.814 | 0.637 | 0.389 | intermediate context |
| 8 | cell-1km--52_-161 | 1/2 | 0.350 | 0.917 | 3/3 | 3.762 | 0.351 | 0.601 | intermediate context |
| 9 | cell-1km--74_-5 | 1/4 | 0.397 | 0.491 | 6/11 | 5.703 | 0.253 | 0.875 | DID-proximate, high population |
| 10 | cell-1km--523_-287 | 3/4 | 0.402 | 0.738 | 5/7 | 5.635 | 0.621 | 0.633 | intermediate context |
| 11 | cell-1km--89_-105 | 2/4 | 0.402 | 0.617 | 3/3 | 6.765 | 0.485 | 0.923 | DID-proximate, high population |
| 12 | cell-1km--371_-273 | 1/4 | 0.424 | 0.304 | 3/3 | 4.648 | 0.451 | 0.074 | remote, low population |
| 13 | cell-1km-31_64 | 1/1 | 0.426 | 1.163 | 3/3 | 9.433 | 0.202 | 0.554 | intermediate context |
| 14 | cell-1km--591_-228 | 1/2 | 0.718 | -0.114 | 3/6 | 6.795 | 0.183 | 0.810 | DID-proximate, high population |
| 15 | cell-1km-301_75 | 1/1 | 0.775 | 0.539 | 3/3 | 8.387 | 0.222 | 0.556 | intermediate context |
| 16 | cell-1km-92_-64 | 1/1 | 0.832 | 0.449 | 3/4 | 3.904 | 0.316 | 0.067 | remote, low population |
| 17 | cell-1km-220_-43 | 1/2 | 0.863 | -0.514 | 4/4 | 3.863 | 0.085 | 0.992 | DID-proximate, high population |

Only two candidates had joint event membership and predictive q <=0.10; none had q <=0.05. This is expected because the ecological event, rather than marginal predictive extremeness, defines the target set.

## Post-selection human context

Human variables were held out of candidate definition, environmental matching and neighbour selection. Each observed candidate was compared with its own observed white cells inside the same primary neighbourhood. The complete comparison was repeated on 1,000 natural predictive maps using the identical event detector.

Human-context inputs comprised:

- WorldPop population counts summarized at the focal cell and native-raster 5-, 10-, 25- and 50-km windows;
- MLIT National Land Numerical Information L03-b 2021 land-use classes;
- road proximity and forest-human edge context;
- a 1-km raster approximation of 2015 densely inhabited districts (DID);
- composite settlement, transport, cultivation, artificial-land and human-activity scores;
- forest and mountain variables retained as natural alternatives.

Raw residuals were never used as the human-context response. Directional hypotheses were defined before comparison, and maxT familywise correction was applied within feature families.

The broad multivariate human-neighbourhood departure was not unusual (`regularized Mahalanobis P=0.90110`). Population effects were strongest at short neighbourhood scales but did not pass familywise correction.

**Table S6.5. Population-scale contrasts for the primary candidate set.** Positive values indicate higher rank at the focal candidate than the mean of its white neighbours.

| Feature | Mean focal-minus-white contrast | Directional P | maxT-FWER P |
|---|---:|---:|---:|
| Focal-cell population rank | +0.04270 | 0.11389 | 0.34466 |
| 5-km population rank | +0.05306 | 0.02697 | 0.08991 |
| 10-km population rank | +0.04902 | 0.03497 | 0.13886 |
| 25-km population rank | +0.01006 | 0.25175 | 0.64036 |
| 50-km population rank | +0.00813 | 0.14086 | 0.44555 |

The 5-km population result is suggestive but familywise inconclusive. It is not evidence that the candidates were planted or escaped from cultivation.

## Dense-settlement context

DID features were calculated only after the 17 candidates were fixed. The strongest single contrast was the combined population-DID alignment score, but it likewise did not cross the familywise threshold.

**Table S6.6. DID and population-alignment contrasts.**

| Feature | Mean focal-minus-white contrast | Directional P | maxT-FWER P |
|---|---:|---:|---:|
| DID proximity rank | +0.05019 | 0.04196 | 0.14685 |
| Within 5 km of DID | +0.05196 | 0.23676 | 0.64635 |
| Within 10 km of DID | +0.07059 | 0.18382 | 0.53147 |
| Population-DID alignment score | +0.05162 | 0.02298 | 0.07592 |
| Population beyond DID score | -0.00549 | 0.63736 | 0.98601 |

Nine of 17 candidates were classified as DID-proximate/high-population, compared with a natural-map mean fraction of 0.317; the two-sided P was 0.12188 and maxT-FWER P was 0.19780. Six candidates had intermediate context and two were remote/low-population. Context-class composition therefore did not provide familywise-robust evidence of an anthropogenic candidate set.

## Sampling, phenology and colour-intensity diagnostics

Several auxiliary checks were retained to prevent sampling or secondary colour features from being mistaken for provenance evidence.

- Observation-effort rank and independent-site support did not differ unusually from the candidate neighbourhood reference (two-sided P=0.563 and 0.915, respectively).
- The four broad/within environmental-PC balance checks were non-significant (all two-sided P>=0.372), consistent with the predefined local environmental caliper.
- Pigmented-only darkness did not show an auxiliary predictive-tail excess (directional P=0.62821).
- Candidate day of year differed by a mean of -1.65 days from all usable neighbours and +3.33 days from exact-same-year neighbours; the signs were mixed across cells rather than a consistent seasonal displacement.
- Candidate identities were selected without the land-use, road, population, DID, darkness or day-of-year variables used in these follow-ups.

These diagnostics do not prove absence of observer or access bias. They show that the current candidate definition is not simply a threshold on those measured factors.

## Interpretation and field use

The primary event frequency is compatible with the fitted natural geographical model. Therefore the 17 cells should be described as **local-departure or provenance targets**, not anthropogenic anomalies. Population and DID contrasts are in a suggestive direction at short scales but remain familywise inconclusive, while the global human-context test is clearly null.

The candidate list nevertheless has practical value. Field follow-up can prioritize:

- voucher confirmation and repeated within-population colour sampling;
- local planting and land-management history;
- standardized spectroscopy and pigment assays;
- population-genetic comparison with neighbouring white populations and horticultural material.

Such data could distinguish natural spatial variation, recent introduction, escape and introgression. The present digital-geographical analysis cannot.

## Reproducibility resources

Current implementation:

- `R/local_pigmented_isolates.R` and `scripts/run_local_pigmented_isolates.R` — event definition and natural-map replay;
- `scripts/refine_submission_isolate_null.R` — 10,000-map cross-fitted submission reference;
- `scripts/run_joint_submission_isolate_ppc.R` — 200,000-map joint spatial posterior-predictive sensitivity;
- `R/local_human_context.R` and `scripts/run_local_human_context.R` — post-selection WorldPop, land-use and road context;
- `R/did_sensitivity.R` and `scripts/run_did_sensitivity.R` — DID follow-up;
- `R/candidate_doy_check.R` and `scripts/run_candidate_doy_check.R` — phenology diagnostic;
- independent validators and audits listed in `paper/active-file-map.csv`.

The manuscript numerical lock is workflow `31258851297`, artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

After repository consolidation and validator repair, a clean end-to-end reconstruction also completed successfully as workflow `31358493033`, artifact `9051983081`, SHA-256 `1c0f59829d24e50e7ade64de1130a422525ce40d8b6ec46898b14f5e9cb6ad4d`. This verification run reproduced the 1,965-record source boundary, 1,922-observation phenotype, 1,305 cells, 17 primary candidates and the manuscript-facing Main 3 values. It is retained as an execution verification rather than silently replacing the frozen numerical reference.

The causal ceiling is unchanged: the analysis prioritizes local field and provenance targets but does not identify horticultural origin or an additional anthropogenic process.

## Final Broad-propagation and human-context robustness audit

The final Broad model review introduced one promoted interaction only for pigmented-only intensity; the pigmentation-state Broad model remained additive. Because the human analysis is based on local **pigmentation-state** departures, there is no direct computational path by which the retained intensity interaction changes candidate selection. Nevertheless, a final guardrail propagated the eight observation-level Broad environmental axes into the cell-level natural reference and environmental matching to test whether the downstream result was definition-sensitive.

### VIF policy

The final audit does not use VIF=5 or VIF=10 as an automatic deletion rule. VIF <5 is preferred; VIF 5–10 requires explicit focal-term, coefficient, spatial and geographical-transfer stability; VIF >10 ordinarily blocks promotion. Under this policy, the pigmentation-state model is unproblematic (maximum VIF 4.430). The conditional-intensity model has maximum VIF 6.340 for Temperature PC1 and 5.233 for Soil PC1, but the retained Temperature PC1 × temperature-seasonality interaction itself has VIF 1.664. Removing the East/West adjustment lowers the maximum VIF below five but worsens full fit and is not supported by blocked transfer, so the regional adjustment is retained rather than deleted to satisfy a mechanical threshold.

### Response-blind environmental-matching propagation

Using the same RMS caliper of one in the eight-axis environmental space increased neighbour support and was not a like-for-like comparison with the four-PC graph. The eight-axis caliper was therefore selected using **only response-blind neighbour support**, never flower colour or human variables. Caliper 0.81 matched the current graph almost exactly: mean neighbour count 3.4835 versus 3.4774 and exactly 657 supported cells in both graphs.

The support-calibrated eight-axis graph still changed candidate composition: 17 candidates were identified in both definitions, but only 15 identities were shared (Jaccard 0.789). Two current candidates were replaced by two alternative candidates. One newly admitted candidate was in approximately the 98th percentile for both 5-km population and built-up context, explaining part of the strengthened human contrast after the environmental definition changed. This is not human-variable leakage—the matching graph uses environmental variables only—but it demonstrates that the human result is sensitive to which natural ecological neighbourhood defines “local departure.”

### Cross-fitted natural-reference comparison

The predeclared multiscale four-PC natural reference remained slightly better than a direct eight-axis refit in the final 10,000-map comparison: mean negative log predictive mass 0.57248 versus 0.57409, AUC 0.86332 versus 0.86033 and Brier score 0.15060 versus 0.15163. The four-PC model is therefore retained as the primary downstream natural reference rather than being replaced merely because the observation-level coefficient model is written using eight environmental axes.

Across current/final-eight-axis natural models and current/support-matched environmental graphs, the observed candidate count remained 17 and was compatible with natural maps (one-sided count P=0.183–0.216). Candidate-fraction two-sided P-values were 0.141–0.191. The frequency of local departures therefore still does not require an additional human process.

### Reduced, mechanism-based human family

The final human audit treats highly correlated composites as descriptive diagnostics rather than independent mechanisms. Artificial-land score is almost identical to artificial-land fraction (Spearman rho approximately 0.986), transport-access score to road proximity (approximately 0.983), and the previous DID-population alignment measures are largely combinations of their component axes. Confirmatory interpretation therefore uses one global maxT family containing population at focal/5/10/25/50-km scales, DID proximity, road proximity, built-up fraction, forest-human edge, and two-sided forest-cover/mountainness alternatives.

Under the **predeclared current four-PC matching**, 5-km population remains the leading contrast (+0.0531 rank units; directional P=0.0305), but the global maxT FWER P is 0.2059 with the current natural model and 0.2366 with the eight-axis refit. This is not familywise support.

Under the **support-calibrated eight-axis matching**, the same short-scale population contrast strengthens (+0.0654). The global maxT FWER P is 0.0463 under the current natural model but 0.0552 under the eight-axis natural model. Population 10 km and DID proximity follow the same direction but do not pass the global correction. Road proximity, built-up fraction and forest-human edge likewise do not provide independent familywise-supported mechanisms.

Accordingly, the alternative eight-axis definition reveals a specific, near-threshold **short-scale settlement-exposure signal**, but that signal is absent after global correction under the predeclared local environmental representation and straddles 0.05 across two defensible natural references. It is a sensitivity result, not a replacement Main effect.

### Ecological interpretation and competing explanations

A short-scale population association has a biologically defensible anthropogenic-exposure interpretation: populated landscapes contain more gardens, planted material and repeated opportunities for human movement of propagules. Horticultural colour material is biologically plausible in this taxon because purple-flowered material has been bred from *Campanula punctata* × var. *rubriflora*. Substantial outcrossing reported in self-incompatible mainland/Oshima populations also makes gene exchange biologically possible if planted and wild material meet.

These facts establish plausibility, not provenance. Population density and road access are also known predictors of citizen-science recording effort, and YAMAP is an opportunistic route-based source. The natural null conditions on observed cells and observation counts and the local effort diagnostics are non-significant (10,000-map maxT P=0.605–0.897 for observation effort and 0.852–0.950 for independent-site support), which weakens a simple “more photographs in candidate cells” explanation. It cannot remove the possibility that accessible/populated landscapes are more likely to enter the source sampling frame at all.

Human context can additionally proxy fine-scale environmental modification rather than provenance. Flower anthocyanin is temperature-sensitive in experimental systems, while gardens and settlements can alter shade, irrigation, substrate and microclimate below the resolution of the national environmental layers. A population association therefore does not distinguish planted/introgressed colour from phenotype plasticity in human-modified local environments.

The final wording is consequently:

> Locally discordant pigmented cells show a suggestive short-scale association with settlement exposure under an alternative, more fully environmental matching definition, but this association is sensitive to the natural-reference/matching specification and does not establish an additional anthropogenic process.

The candidates remain **field/provenance targets, not anthropogenic anomalies**. Direct tests of planting, escape or introgression require local management history, voucher-level morphology, replicated within-population sampling and population-genetic comparison with nearby white populations and horticultural material.

Final audit implementation and numerical provenance are frozen in `reproducibility/human_context_final_audit_results_2026-08-11.md`; the 10,000-map adjudication is workflow `31472542634`, artifact `9094339466`, digest `sha256:413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`.
