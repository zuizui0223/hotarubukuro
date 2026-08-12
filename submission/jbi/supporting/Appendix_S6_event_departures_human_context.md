# Appendix S6. Event-based local departures and post-selection human context

## Scope and inference ceiling

This Appendix asks whether locally discordant pigmented cells require an additional process beyond the finalized natural flower-colour geography and, only after those cells have been defined without human information, whether they occupy distinctive human context. The analysis deliberately does **not** define anomalies by thresholding fitted residuals or by selecting locations that already have high population, road access or built land.

The downstream analysis is now aligned directly to the finalized Broad pigmentation-state model. The primary natural reference is the cross-fitted eight-axis environment + SPDE state model, and the primary local environmental matching uses the same eight measured abiotic axes. The earlier four-PC broad/within-neighbourhood representation is retained only as a sensitivity and provenance record.

The three inferential questions are:

1. how many pigmented 1-km cells occur inside geographically close, environmentally similar white neighbourhoods under the finalized Broad environmental state space;
2. how often the same ecological event arises in 10,000 natural predictive maps under the observed sampling geometry; and
3. whether independently selected candidates differ from their own white neighbours in a small, mechanism-based set of human-context variables.

The analysis can identify field and provenance targets. It cannot demonstrate planting, escape, horticultural introgression or human causation.

## Current-Broad primary ecological event

The unit was the 1-km flower cell from the 1,305-cell environment-complete cell analysis. Candidate selection was fixed before human variables were examined.

**Table S6.1. Current-Broad primary event specification.**

| Component | Fixed rule |
|---|---|
| Focal state | at least one observed pigmented flower |
| Geographic radius | 10 km |
| Environmental representation | Temperature PC1, precipitation PC1, temperature seasonality, precipitation seasonality, topography PC1, soil PC1, soil PC2 and RSDS |
| Standardization | response-blind z-standardization over the analysis cells |
| Environmental distance | root-mean-square Euclidean distance across the eight standardized Broad abiotic axes |
| Environmental caliper | <=1.0 |
| Minimum neighbouring cells | 3 |
| Neighbour colour condition | all eligible physical neighbours have zero observed pigmented flowers |
| Human variables | absent from selection, matching and ranking |

East/West remains a structural geographical adjustment in the observation-level Broad model; it is not an abiotic process axis and therefore is not used as an environmental-matching dimension. The SPDE field similarly belongs to the predictive natural reference rather than to the definition of environmental similarity between neighbouring cells.

The event can include a mixed focal cell. “White neighbours” refers to the observed cell samples rather than genetic fixation of an underlying population. Predictive tail probabilities are retained as diagnostics but are not thresholds for candidate membership.

## Natural-map calibration under the finalized Broad state model

We reused the checksum-locked 10,000 predictive draws from the final eight-axis cross-fitted pigmentation-state model and replayed the current-Broad event detector on every map. Cell locations, trial counts and the final eight-axis environmental graph were held fixed. This reuses exactly the same Broad state model rather than refitting an equivalent model solely for the human analysis.

**Table S6.2. Current-Broad primary event relative to 10,000 natural predictive maps.**

| Metric | Observed | Null mean | 95% null interval | Monte Carlo P |
|---|---:|---:|---:|---:|
| Candidate count | **16** | 13.5908 | 7–21 | 0.27897 |
| Candidate fraction | **0.04071** | 0.03107 | 0.01573–0.04861 | 0.12609 upper-tail |

The current Broad graph supported 706 cells with at least three environmentally eligible neighbours; mean neighbour support was 3.816 cells. Neither candidate count nor candidate fraction was excessive under the fitted natural geography. Thus the 16 observed cells are reproducible local colour configurations, not evidence that an additional anthropogenic process is required to generate their frequency.

A stricter joint event requiring both local isolation and an extreme predictive tail was also non-excessive: one observed q<=0.10 joint candidate compared with a null mean of 0.9505 (P=0.6242), and zero q<=0.05 joint candidates compared with a null mean of 0.1589.

## Why the downstream primary was changed from four PCs to the final eight Broad axes

The previous downstream reference used two broad 50-km PCs and two cell-minus-50-km PCs. That representation was useful for explicitly separating regional background from local deviation and, in geographical cross-validation, had marginally better predictive scores than a direct eight-axis state model. However, finalizing the Broad analysis changed the inferential target of this downstream stage: the human analysis is intended to ask whether a local colour configuration remains unusual **after conditioning on the natural environmental structure accepted in the paper's Broad state analysis**.

We therefore now prioritize inferential coherence. The same eight measured abiotic axes used to characterize pigmentation-state geography define environmental comparability in the primary local-departure analysis, while the legacy four-PC formulation remains a sensitivity rather than a second competing primary model.

This change does not imply that the earlier four-PC analysis was invalid. It makes the dependency structure explicit:

> finalized Broad pigmentation-state environment -> current-Broad local matching -> natural-map event calibration -> post-selection human context.

The promoted Temperature PC1 × temperature-seasonality interaction in the Broad analysis belongs only to pigmented-only intensity. It has no direct path into this state-based event detector.

## Sensitivity to the environmental representation

The candidate set is not perfectly invariant to how “environmentally similar” is defined. The current eight-axis matching at RMS <=1 defines the manuscript primary: 16 candidates, with 15 cells shared with the historical result.

For sensitivity only, the historical four-PC matching at RMS <=1 produced 17 candidates. A separate response-blind calibration that adjusted the eight-axis caliper to 0.81 to mimic the historical four-PC neighbour-support distribution also produced 17 candidates, again with 15 cells shared with the current primary.

This sensitivity is biologically relevant rather than a nuisance to hide. Local human contrasts are conditional on which white populations are treated as natural comparators. The current analysis therefore reports the final-eight-axis result as primary and keeps the older definitions as robustness checks. Human information is never used to choose among these graphs or to tune the current primary caliper.

Across the previous propagation scenarios, candidate frequency was non-excessive regardless of whether the four-PC or eight-axis natural reference and graph were used. The new current-Broad primary reaches the same natural-reference conclusion with 16 candidates (count P=0.279; fraction upper-tail P=0.126).

## Human-context hypothesis family

Human variables are not treated as interchangeable measurements of an abstract “urbanization” factor. Highly redundant historical composites were removed from confirmatory interpretation. The final global maxT family contains eleven mechanism-based features.

**Table S6.3. Human-context feature family.**

| Feature | Ecological role | Expected direction | Interpretation ceiling |
|---|---|---|---|
| focal-cell population rank | immediate settlement exposure | greater | nearby human presence; not planting itself |
| population rank at 5 km | short-scale residential/horticultural opportunity | greater | gardens, planted material and propagule-opportunity proxy |
| population rank at 10 km | local settlement exposure | greater | broader version of the same pathway |
| population rank at 25 km | regional settlement exposure | greater | regional human context |
| population rank at 50 km | broad settlement exposure | greater | broad human context |
| DID proximity rank | dense-settlement convergence | greater | independent settlement-definition check |
| road proximity rank | transport/access | greater | possible movement/disturbance and observation access |
| built-up fraction rank | managed/built habitat | greater | built context, not planting evidence |
| forest-human edge rank | managed-natural interface | greater | interface exposure; mechanistically less specific |
| forest cover rank | natural alternative | two-sided | residual forest context |
| mountainness rank | natural alternative | two-sided | residual mountain context |

Global maxT correction is applied across all eleven variables. No population radius is promoted from its unadjusted P value after inspecting the data.

The ecological rationale is deliberately bounded. Ornamental horticulture can create propagule pressure and repeated opportunities for escape or establishment; market availability and residential propagule pressure have predicted establishment or invasion in ornamental plants (Dehnen-Schmutz et al., 2007; Davis et al., 2016). *Campanula punctata* also has intentionally bred coloured horticultural material: the purple dwarf cultivar ‘Jiknyeo’ was produced from *C. punctata* × var. *rubriflora* (Choi et al., 2012). These observations make settlement-associated horticultural movement biologically plausible, but they do not identify the provenance of any YAMAP flower.

Roads provide another plausible pathway through transport, disturbance and secondary dispersal, while simultaneously increasing observation access. Population density and road accessibility can also structure opportunistic biological recording effort (Mair & Ruete, 2016). Finally, settlement can alter shade, irrigation, substrate and microclimate at scales unresolved by kilometre climate layers; floral anthocyanin can be environmentally plastic in other species. A settlement association is therefore not automatically an ancestry signal.

## Current-Broad 10,000-map human-context result

For each predictive map, the same current-Broad event detector was reapplied and the same focal-minus-white-neighbour human contrasts were recalculated. This propagates uncertainty in which cells would be selected under the fitted natural geography rather than treating the observed candidate set as fixed under a standard regression null.

**Table S6.4. Current-Broad primary human-context contrasts.**

| Feature | Observed focal-minus-white-neighbour contrast | Directional/two-sided P | Global maxT FWER P |
|---|---:|---:|---:|
| focal population | +0.06336 | 0.04070 | 0.28287 |
| **population 5 km** | **+0.06744** | **0.00800** | **0.05479** |
| population 10 km | +0.06009 | 0.01370 | 0.10809 |
| population 25 km | +0.00583 | 0.33717 | 0.96900 |
| population 50 km | +0.00811 | 0.19008 | 0.84242 |
| DID proximity | +0.05897 | 0.02200 | 0.15948 |
| road proximity | +0.03505 | 0.23818 | 0.89301 |
| built-up fraction | +0.12331 | 0.04580 | 0.30857 |
| forest-human edge | +0.09106 | 0.10139 | 0.56834 |
| forest cover | -0.08058 | 0.28257 | 0.90331 |
| mountainness | -0.05337 | 0.23538 | 0.83442 |

The leading feature is therefore **population exposure within 5 km**. Its directional Monte Carlo P is 0.0080, but its global familywise P is 0.0548. Population at 10 km and DID proximity point in the same direction but do not survive the global correction. Built-up fraction is directionally positive but provides no corrected independent mechanism; road proximity and forest-human edge are weaker still.

The current evidence is consequently stronger than “no human pattern at all” but remains below the paper's confirmatory claim threshold. We describe it as a **near-threshold, short-scale settlement-exposure signal**, not as a statistically established anthropogenic effect, horticultural origin or urbanization syndrome.

The spatial scale is itself informative. The contrast is concentrated at 5–10 km and largely disappears at 25–50 km, which is more consistent with a local exposure/opportunity process than with a broad national human gradient. This scale pattern nevertheless cannot distinguish planted ancestry from local environmental modification or source-data inclusion processes.

## Observation-process alternatives

The natural maps condition on the observed 1-km cells and flower counts, and the human comparison is local against environmentally similar white neighbours. These choices reduce but cannot eliminate route-access sampling bias.

Measured within-dataset effort did not distinguish current-Broad candidates from their white neighbourhoods after the same 10,000-map correction:

**Table S6.5. Observation-process alternatives.**

| Feature | Observed contrast | Two-sided P | maxT P |
|---|---:|---:|---:|
| observation-effort rank | +0.10742 | 0.92411 | 0.96320 |
| independent-site-support rank | +0.07394 | 0.71813 | 0.75642 |

Thus the 5-km population pattern is not explained simply by the 16 candidate cells having unusually many photographs or unusually many independent YAMAP source activities relative to their local white comparators. This does **not** remove the broader sampling-frame possibility that populated or accessible landscapes are more likely to enter YAMAP at all.

Earlier phenology and dark-colour diagnostics also did not establish a coherent horticultural syndrome. Those features are not used to define the current candidates.

## Ecological interpretation and claim ceiling

Three statements have different evidential status.

1. **Natural-reference conclusion — robust.** Sixteen locally discordant pigmented cells occur at a frequency compatible with the finalized eight-axis Broad natural geography. The event count therefore does not itself require a second human process.
2. **Human-context conclusion — suggestive.** Population exposure within 5 km is the leading and biologically coherent post-selection feature (directional P=0.0080), but global maxT FWER P=0.0548 remains just above 0.05. Population at 10 km and DID proximity point in the same direction without corrected support.
3. **Provenance conclusion — unresolved.** The settlement pattern is compatible with horticultural opportunity or propagule pressure, but fine-scale environmental plasticity and broad observation-accessibility remain viable alternatives. The photographs provide no ancestry or planting-history evidence.

Accordingly, the 16 current-Broad candidates are **local-departure/provenance field targets**, not anthropogenic anomalies. Stronger tests require local planting and management histories, voucher-level morphology, repeated population colour sampling, standardized spectra and pigment chemistry, and population-genetic comparison among candidate populations, neighbouring white populations and relevant horticultural material.

## Reproducibility resources

Current-Broad primary replay:

- script: `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`;
- workflow: `.github/workflows/human-context-highrep-final.yml`;
- successful workflow run: `31537102360`;
- output artifact: `9119306089`;
- artifact digest: `sha256:f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4`;
- locked final-eight-axis predictive-draw source: artifact `9094339466`, digest `sha256:413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`;
- frozen cell/human-data reference: artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

The historical four-PC downstream analysis and support-calibrated eight-axis comparison remain under `legacy/reproducibility-development/superseded-human-primary-2026-08-11/` as sensitivity provenance; they are no longer the manuscript primary.

## References added for the human-context interpretation

Choi, M. S. et al. (2012). Breeding of purple flower-colored dwarf ‘Jiknyeo’ from hybridization of *Campanula punctata* Lam. × *Campanula punctata* Lam. var. *rubriflora* Mak. DOI: 10.7235/hort.2012.12015.

Davis, A. J. S. et al. (2016). Accounting for residential propagule pressure improves prediction of urban plant invasion. *Ecosphere* 7:e01232. DOI: 10.1002/ecs2.1232.

Dehnen-Schmutz, K., Touza, J., Perrings, C. & Williamson, M. (2007). A century of the ornamental plant trade and its impact on invasion success. *Diversity and Distributions* 13:527–534. DOI: 10.1111/j.1472-4642.2007.00359.x.

Mair, L. & Ruete, A. (2016). Explaining spatial variation in the recording effort of citizen science data across multiple taxa. *PLoS ONE* 11:e0147796.