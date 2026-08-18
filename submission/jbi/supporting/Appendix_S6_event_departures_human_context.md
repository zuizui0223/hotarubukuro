# Appendix S6. Event-based local isolates and post-selection human context

## Scope and inference ceiling

This Appendix asks three sequential questions.

1. **Observed event definition:** which cells in the observed flower-colour map satisfy a fixed, human-blind local-isolate rule?
2. **Natural-frequency calibration:** how often does the finalized natural flower-colour model generate the same type of local configuration?
3. **Post-selection human context:** after the observed sites have been fixed without human information, which human-context features distinguish them from their local white comparators relative to the same predictive-map selection process?

The analysis does **not** define anomalies by thresholding fitted residuals, nor does it select locations using population density, roads, built land or other human variables. The primary natural reference is the cross-fitted final-eight-axis pigmentation-state model, and local environmental matching uses the same eight measured abiotic axes.

The analysis can identify repeatable ecological events and field/provenance targets. It cannot demonstrate planting, escape, horticultural introgression or human causation. The observed 16 sites are also not “configurations that natural processes could not reproduce”; their frequency is explicitly shown to be compatible with the fitted natural geography.

## S6.1 Why this is not the Figure 3 Bombus-boundary analysis

The two local analyses use different units and estimands.

| Component | Local Bombus boundary analysis | Local-isolate/human analysis |
|---|---|---|
| Unit | non-overlapping edge between two 1-km cells | focal 1-km cell plus its local neighbourhood |
| Colour configuration | one pure white endpoint and one pure pigmented endpoint | pigmented focal cell among environmentally similar observed white neighbours |
| Radius | primary 5 km boundary search | 10 km neighbour search |
| Main statistic | signed pigmented-minus-white Bombus support | event count/fraction, then focal-minus-neighbour human contrast |
| Natural-map replay | not used | used for event-frequency and post-selection null distributions |
| Biological question | does predicted focal-Bombus opportunity change across a sharp state boundary? | is local pigmentation isolation unusually frequent, and does it have distinctive human context? |

A cell or region can contribute to one configuration without satisfying the other. Neither site set was selected to maximize the result of the other analysis.

## S6.2 Observed local ecological event definition

The unit is the 1-km flower cell from the 1,305-cell environment-complete analysis. The event rule is fixed before human variables are examined.

**Table S6.1. Primary local-event specification.**

| Component | Fixed rule |
|---|---|
| Focal state | at least one observed pigmented flower |
| Geographic radius | 10 km |
| Environmental representation | Temperature PC1, precipitation PC1, temperature seasonality, precipitation seasonality, topography PC1, soil PC1, soil PC2 and RSDS |
| Standardization | response-blind z-standardization over analysis cells |
| Environmental distance | root-mean-square Euclidean distance across the eight standardized abiotic axes |
| Environmental caliper | <=1.0 |
| Minimum neighbouring cells | 3 |
| Neighbour colour condition | all eligible observed neighbours contain zero pigmented flowers |
| Human variables | absent from selection, matching and ranking |

East/West is a structural geographical adjustment in the observation-level Broad model and is not used as an abiotic matching dimension. The SPDE field enters through the predictive natural reference rather than through the definition of measured environmental similarity.

The event can include a mixed focal cell. “White neighbours” refers to observed cell samples rather than genetic fixation of an underlying population. Predictive tail probabilities are retained as diagnostics but are not thresholds for event membership.

Applying this rule to the **observed map** selects the 16 current field/provenance targets. The observed rule, not the posterior-predictive replay, determines their identities.

## S6.3 First use of 10,000 maps: natural-frequency calibration

The identical event detector is replayed on **10,000 checksum-locked predictive maps** from the final-eight-axis cross-fitted pigmentation-state model. Cell locations, flower counts and environmental graph are held fixed. On each map, predicted pigmentation observations replace the observed colour states and the detector recounts the event type under the fitted natural geography.

This replay asks whether the observed **number and fraction** of local-isolate events are unusual. It does not ask which observed sites are impossible under nature, and it does not require the same cell identities to recur in predictive maps.

**Table S6.2. Observed event frequency relative to 10,000 natural predictive maps.**

| Metric | Observed | Null mean | 95% null interval | Monte Carlo P |
|---|---:|---:|---:|---:|
| Candidate count | **16** | 13.5908 | 7–21 | 0.27897 |
| Candidate fraction | **0.04071** | 0.03107 | 0.01573–0.04861 | 0.12609 upper-tail |

The final environmental graph supports 706 cells with at least three eligible neighbours; mean neighbour support is 3.816 cells. Neither candidate count nor candidate fraction is excessive under the fitted natural geography.

A stricter joint diagnostic requiring both local isolation and an extreme predictive tail is also non-excessive: one observed q<=0.10 joint candidate compared with a null mean of 0.9505 (P=0.6242), and zero q<=0.05 joint candidates compared with a null mean of 0.1589.

The primary ecological conclusion is therefore that the **frequency** of the 16 observed local isolates does not require an additional anthropogenic or other second process. Natural spatial variation is sufficient to generate this many apparently discordant configurations.

## S6.4 Human-context hypothesis family

Human variables are evaluated only after the observed event rule and observed candidate identities are fixed. They are not treated as interchangeable measurements of a single “urbanization” factor. The family contains eleven mechanism-based variables, all corrected jointly with global maxT.

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

No population radius is promoted from an unadjusted P value after inspecting the data.

The ecological rationale is bounded. Ornamental horticulture can create propagule pressure and repeated opportunities for escape or establishment; market availability and residential propagule pressure have predicted establishment or invasion in ornamental plants (Dehnen-Schmutz et al., 2007; Davis et al., 2016). *Campanula punctata* also has intentionally bred coloured horticultural material: the purple dwarf cultivar ‘Jiknyeo’ was produced from *C. punctata* × var. *rubriflora* (Choi et al., 2012). These observations make settlement-associated horticultural movement biologically plausible, but they do not identify the provenance of any YAMAP flower.

Roads and trails provide another plausible pathway through transport, disturbance and secondary dispersal while simultaneously increasing observation access. Clothing and footwear can transport seeds in protected areas, and hiking trails can facilitate plant spread in other systems (Mount & Pickering, 2009; Zani et al., 2025). These studies establish possible mechanisms, not transport or establishment of *C. punctata* at the present sites. Population density and road accessibility can also structure opportunistic biological recording effort (Mair & Ruete, 2016). Settlement may alter shade, irrigation, substrate and microclimate at scales unresolved by kilometre climate layers. A settlement association is therefore not automatically an ancestry signal.

## S6.5 Second use of 10,000 maps: post-selection human null

The observed human contrasts are calculated for the 16 sites selected from the observed map and their eligible local white neighbours. A standard comparison against fixed sites would ignore that those sites were selected by a colour-configuration rule.

The predictive replay therefore repeats the **entire selection-and-comparison operation** on every map:

1. simulate the pigmentation map under the cross-fitted natural model;
2. reapply the same local-isolate detector;
3. identify the event cells generated on that map;
4. calculate the same focal-minus-white-neighbour human contrasts for that map-selected event set.

The resulting distributions incorporate uncertainty in which cells would be selected under the natural geography. Global maxT then controls the predefined eleven-feature family. This is the second, distinct role of the 10,000-map replay.

**Table S6.4. Post-selection human-context contrasts.**

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

The leading feature is **population exposure within 5 km**. Its directional Monte Carlo P is 0.0080, but its global familywise P is 0.0548. Population at 10 km and DID proximity point in the same direction without surviving global correction. Built-up fraction is directionally positive but does not provide corrected independent support; road proximity and forest-human edge are weaker.

The evidence is therefore described as a **near-threshold short-scale settlement-exposure signal**, not as a statistically established anthropogenic effect, horticultural origin or general urbanization syndrome.

The spatial scale is informative but not diagnostic of mechanism. The population contrast is concentrated at 5–10 km and largely disappears at 25–50 km, which is compatible with a local exposure/opportunity process. The pattern cannot distinguish planted ancestry from fine-scale environmental modification or broader sampling-frame accessibility.

## S6.6 Observation-process alternatives and the bidirectional YAMAP frame

The natural maps condition on the observed 1-km cells and flower counts, and the human comparison is local against environmentally similar white neighbours. These choices reduce but cannot eliminate route-access sampling bias.

Measured within-dataset effort does not distinguish candidates from their white neighbourhoods after the same 10,000-map correction.

**Table S6.5. Observation-process alternatives.**

| Feature | Observed contrast | Two-sided P | maxT P |
|---|---:|---:|---:|
| observation-effort rank | +0.10742 | 0.92411 | 0.96320 |
| independent-site-support rank | +0.07394 | 0.71813 | 0.75642 |

Thus the 5-km population pattern is not explained simply by the 16 candidate cells containing unusually many photographs or unusually many independent YAMAP source activities relative to their local white comparators. This does **not** remove the broader possibility that populated or accessible landscapes are more likely to enter the YAMAP sampling frame.

The mountain-route frame has no guaranteed one-way bias for the human contrast.

- **Possible attenuation:** conditioning candidates and comparators on traversed mountain routes can compress the represented urban-rural gradient, reducing power to detect a real settlement difference.
- **Possible inflation or confounding:** trailheads, roads and accessible mountain margins can increase observation opportunity, disturbance and propagule movement together, making settlement exposure a proxy for entry into the sampling frame or managed microenvironment.

Consequently, “the human signal is weak because hiking data removed human influence” is not a supported interpretation. The sampling process can weaken, strengthen or redirect the contrast, and the current data do not identify the net direction.

## S6.7 Ecological interpretation and claim ceiling

Four conclusions have different evidential status.

1. **Observed-selection conclusion — descriptive and reproducible.** The fixed human-blind neighbourhood rule selects 16 observed local-isolate sites.
2. **Natural-reference conclusion — supported.** Sixteen events occur at a frequency compatible with the finalized eight-axis natural geography. The event count does not itself require a second human process.
3. **Human-context conclusion — suggestive.** Population exposure within 5 km is the leading post-selection feature (directional P=0.0080), but global maxT FWER P=0.0548 remains just above 0.05. Population at 10 km and DID proximity point in the same direction without corrected support.
4. **Provenance conclusion — unresolved.** The settlement pattern is compatible with horticultural opportunity or propagule pressure, but fine-scale environmental plasticity and broad observation accessibility remain viable alternatives. The photographs provide no ancestry or planting-history evidence.

The human interpretation is therefore doubly conditional: the event type is not unusually frequent under nature, and its strongest human-context feature does not cross the familywise threshold. Accordingly, the 16 candidates are **local-isolate/provenance field targets**, not anthropogenic anomalies.

Stronger tests require local planting and management histories, route and disturbance history, voucher-level morphology, repeated population colour sampling, standardized spectra and pigment chemistry, flower-level microenvironment, and population-genetic comparison among candidate populations, neighbouring white populations and relevant horticultural material.

## S6.8 Reproducibility resources

Primary implementation:

- script: `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`;
- workflow: `.github/workflows/human-context-highrep-final.yml`;
- successful workflow run: `31537102360`;
- output artifact: `9119306089`;
- artifact digest: `sha256:f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4`;
- locked final-eight-axis predictive-draw source: artifact `9094339466`, digest `sha256:413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`;
- frozen cell/human-data reference: artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

Current result lock:

- `reproducibility/current_broad_human_primary_2026-08-12.md`.

## References

Choi, M. S. et al. (2012). Breeding of purple flower-colored dwarf ‘Jiknyeo’ from hybridization of *Campanula punctata* Lam. × *Campanula punctata* Lam. var. *rubriflora* Mak. DOI: 10.7235/hort.2012.12015.

Davis, A. J. S. et al. (2016). Accounting for residential propagule pressure improves prediction of urban plant invasion. *Ecosphere* 7:e01232. DOI: 10.1002/ecs2.1232.

Dehnen-Schmutz, K., Touza, J., Perrings, C. & Williamson, M. (2007). A century of the ornamental plant trade and its impact on invasion success. *Diversity and Distributions* 13:527–534. DOI: 10.1111/j.1472-4642.2007.00359.x.

Mair, L. & Ruete, A. (2016). Explaining spatial variation in the recording effort of citizen science data across multiple taxa. *PLoS ONE* 11:e0147796.

Mount, A. & Pickering, C. M. (2009). Testing the capacity of clothing to act as a vector for non-native seed in protected areas. *Journal of Environmental Management* 91:168–179. DOI: 10.1016/j.jenvman.2009.08.002.

Zani, D., Lischke, H., Åkerman, J. & Lehsten, V. (2025). Hiking trails facilitate the spread of a native high-Arctic species. *Ecology and Evolution* 15:e70809. DOI: 10.1002/ece3.70809.