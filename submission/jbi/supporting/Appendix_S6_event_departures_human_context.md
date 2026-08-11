# Appendix S6. Event-based local departures and post-selection human context

## Scope and inference ceiling

This Appendix asks whether locally discordant pigmented cells require an additional process beyond the fitted natural geography and, only after those cells have been defined without human information, whether they occur in distinctive human context. It deliberately does **not** define anomalies by thresholding fitted residuals or by selecting locations that already have high population, road access or built land.

The three inferential questions are:

1. how many pigmented 1-km cells occur inside geographically close, environmentally similar white neighbourhoods;
2. how often the same ecological event arises in natural predictive maps under the observed sampling geometry; and
3. whether independently selected candidates differ from their own white neighbours in a small, mechanism-based set of human-context variables.

The analysis can identify field and provenance targets. It cannot demonstrate planting, escape, horticultural introgression or human causation.

## Primary ecological event

The unit was the 1-km flower cell from the 1,305-cell analysis. The primary event was fixed before human variables were inspected.

**Table S6.1. Primary event specification.**

| Component | Fixed rule |
|---|---|
| Focal state | at least one observed pigmented flower |
| Geographic radius | 10 km |
| Environmental representation | broad50km PC1/PC2 and within50km PC1/PC2 |
| Environmental distance | root-mean-square Euclidean distance across the four standardized PCs |
| Environmental caliper | <=1.0 |
| Minimum neighbouring cells | 3 |
| Neighbour colour condition | all eligible physical neighbours have zero observed pigmented flowers |
| Human variables | absent from selection, matching and ranking |

The event can include a mixed focal cell. “White neighbours” refers to the observed cell samples rather than genetic fixation of an underlying population. Held-out predictive upper-tail probability (`unexpected_pigmented_q`) and standardized departure (`z`) are retained as diagnostics but are not thresholds for membership.

## Natural-map calibration of event frequency

The detector was replayed on 10,000 held-out cross-fitted natural maps with the observed cell geometry and trial counts fixed. A separate joint posterior-predictive sensitivity combined 10,000 latent spatial draws with 20 observation replicates per draw, giving 200,000 maps.

**Table S6.2. Primary event relative to the natural predictive reference.**

| Reference | Metric | Observed | Null mean | 95% null interval | P |
|---|---|---:|---:|---:|---:|
| 10,000 cross-fitted maps | candidate count | 17 | 13.614 | 7–21 | 0.19958 |
| 10,000 cross-fitted maps | candidate fraction | 0.04735 | 0.03427 | 0.01746–0.05362 | 0.08739 upper-tail |
| 200,000 joint posterior-predictive maps | candidate count | 17 | 14.879 | 8–22 | 0.31446 |
| 200,000 joint posterior-predictive maps | candidate fraction | 0.04735 | 0.03925 | 0.02128–0.05898 | 0.19618 |

Neither count nor fraction showed a robust excess. The 17 cells are therefore reproducible local configurations, not evidence that the natural model fails to generate such configurations.

## Event-definition sensitivities

No sensitivity was selected by its P value; each defines a different ecological object.

**Table S6.3. Previously specified event sensitivities using the cross-fitted natural reference.**

| Configuration | Observed count | Count P | Observed fraction | Fraction P | Interpretation |
|---|---:|---:|---:|---:|---|
| Primary: 10 km, env <=1.0, >=3 neighbours, all white | 17 | 0.19958 | 0.04735 | 0.08739 | manuscript event |
| Same-fold-only primary | 16 | 0.24598 | 0.04776 | 0.10669 | fold-boundary guardrail |
| 5 km, env <=1.0, >=3 neighbours, all white | 10 | 0.10819 | 0.05848 | 0.04860 | fraction nominal only |
| 25 km, env <=1.0, >=5 neighbours, neighbour pigment share <=0.10 | 25 | 0.00150 | 0.04488 | 0.00070 | broader relaxed event, not the primary local object |
| 10 km, env <=0.75, >=3 neighbours, all white | 15 | 0.28217 | 0.04808 | 0.15389 | stricter environment |
| 10 km, env <=1.5, >=3 neighbours, all white | 18 | 0.20968 | 0.04545 | 0.08239 | looser environment |

The significant 25-km result is not a substitute for the primary event because it changes both scale and neighbour-colour tolerance.

## Does final Broad model selection propagate into the human analysis?

The observation-level Broad analysis and this event detector are intentionally different model layers. The final Broad **pigmentation-state** model remains the additive eight-axis environment + East/West + stationary SPDE model; the only promoted environmental interaction belongs to pigmented-only intensity and therefore has no direct path to the state-based event detector.

The event detector nevertheless uses an environmental matching representation, so we explicitly propagated the final eight observation-level axes as a sensitivity rather than assuming invariance.

### Candidate membership under the two environmental representations

At the same raw RMS caliper of 1.0, four-PC matching produced 17 candidates and eight-axis matching 16, with 15 candidates in common (Jaccard = 0.833). Because RMS distances change when the dimensionality and correlation structure change, we then calibrated the eight-axis caliper **without using colour or human variables** so that its graph had the same neighbourhood support as the primary graph. The selected eight-axis caliper was 0.81:

- primary four-PC mean neighbours = 3.4774; supported cells = 657;
- calibrated eight-axis mean neighbours = 3.4835; supported cells = 657.

With equalized support, both graphs contained 17 observed candidates, but only 15 were identical (Jaccard = 0.789). The alternative graph replaced two primary candidates with two other cells. One newly admitted cell had very high short-scale population and built-up ranks, demonstrating why a change in the environmental matching definition can strengthen the human-context statistic without any human variable entering candidate selection.

### Which natural predictive reference should remain primary?

The direct final-eight-axis cell-level refit was evaluated with the same five geographic folds and 10,000 predictive maps.

**Table S6.4. Cross-fitted state prediction under the two defensible natural references.**

| Natural reference | Mean negative log predictive mass | AUC | Brier |
|---|---:|---:|---:|
| Current multiscale four-PC reference | **0.57248** | **0.86332** | **0.15060** |
| Direct final-eight-axis reference | 0.57409 | 0.86033 | 0.15163 |

The predeclared multiscale four-PC reference was slightly better on all three diagnostics and explicitly separates 50-km background environment from cell-minus-background environment. It therefore remains the primary downstream natural reference. Finalizing the observation-level Broad coefficient model does not require replacing this independently validated predictive layer.

### Event frequency across the final propagation scenarios

Candidate frequency remained non-excessive under every combination of natural reference and environmental graph.

**Table S6.5. Candidate-count guardrail across Broad-propagation scenarios.**

| Natural model | Environmental graph | Observed count | Count P |
|---|---|---:|---:|
| current four-PC | current four-PC | 17 | 0.1996 |
| current four-PC | calibrated eight-axis | 17 | 0.2163 |
| final eight-axis | current four-PC | 17 | 0.1830 |
| final eight-axis | calibrated eight-axis | 17 | 0.1854 |

Candidate-fraction two-sided P values were likewise non-robust (approximately 0.175, 0.191, 0.141 and 0.147). Thus the conclusion that the event frequency is compatible with natural geography does not depend on the Broad variable representation.

## Final human-context hypothesis family

Human variables were not treated as interchangeable measures of an abstract “urbanization” factor. The earlier exploratory surface included highly redundant composites; for example, artificial-land and transport composites were almost rank-equivalent to their constituent artificial-land and road variables, and population–DID composites were almost rank-equivalent to population or DID alone. Those composites are retained only as historical diagnostics and are not counted as independent mechanisms.

The final confirmatory/guardrail family contains eleven variables in one global maxT family:

**Table S6.6. Mechanism-based human-context family.**

| Feature | Ecological role | Expected direction | Interpretation ceiling |
|---|---|---|---|
| focal-cell population rank | immediate settlement exposure | greater | nearby human presence; not planting itself |
| population rank at 5 km | short-scale residential/horticultural opportunity | greater | gardens, planted material and propagule opportunity proxy |
| population rank at 10 km | local settlement exposure | greater | broader version of same pathway |
| population rank at 25 km | regional settlement exposure | greater | regional human context |
| population rank at 50 km | broad settlement exposure | greater | broad human context |
| DID proximity rank | dense-settlement convergence | greater | independent settlement-definition check |
| road proximity rank | transport/access | greater | potential movement/disturbance **and** observation-access proxy |
| built-up fraction rank | managed/built habitat | greater | built context, not evidence of horticultural planting |
| forest–human edge rank | managed–natural interface | greater | interface exposure; mechanistically less specific |
| forest cover rank | natural alternative | two-sided | candidate placement in forest context |
| mountainness rank | natural alternative | two-sided | residual mountain context |

The biological rationale is intentionally asymmetric. Ornamental horticulture can create propagule pressure and repeated opportunities for escape or establishment; historical market availability has predicted invasion success across ornamental taxa, and residential propagule-pressure metrics can improve prediction of urban plant invasion (Dehnen-Schmutz et al., 2007; Davis et al., 2016). *C. punctata* itself has horticulturally bred coloured material: the purple dwarf cultivar ‘Jiknyeo’ was produced from *C. punctata* × var. *rubriflora* (Choi et al., 2012). These studies establish plausibility of horticultural movement and colour manipulation, **not** provenance of any present candidate.

Roads supply a second plausible pathway through transport, disturbance and secondary dispersal, but road proximity also increases access. Because opportunistic biological recording is often concentrated near roads and populated areas (Mair & Ruete, 2016), any positive road or population contrast has an observation-process interpretation as well as a biological one.

Finally, settlement can alter microclimate, irrigation, shade and substrate. Floral anthocyanin can be temperature-plastic in other species (e.g. Stiles et al., 2007), so a local settlement association could reflect environmental plasticity at scales unresolved by kilometre climatologies rather than planted ancestry. Population density is therefore a **provenance-opportunity proxy**, not a provenance measurement.

## Final 10,000-map human-context test

The final adjudication replayed candidate selection and the same focal-minus-white-neighbour contrasts on 10,000 natural maps for four defensible combinations of natural reference and environmental graph. Global maxT correction was applied across all eleven variables, rather than separately within several correlated feature families.

### Primary predeclared matching

Under the current four-PC natural model and current four-PC graph, the leading feature was 5-km population rank:

- observed focal-minus-white-neighbour contrast = +0.05306 rank units;
- directional P = 0.03050;
- **global maxT FWER P = 0.20588**.

Using the final-eight-axis natural model on the same graph gave global maxT FWER P = 0.23658. Thus the primary human-context analysis is not familywise-supported.

### Support-calibrated eight-axis matching

Changing only the environment-matching graph strengthened short-scale settlement contrasts:

- 5-km population contrast = +0.06542;
- current natural model: directional P = 0.00620; **global maxT FWER P = 0.04630**;
- final-eight-axis natural model: directional P = 0.00680; **global maxT FWER P = 0.05519**.

Population at 10 km and DID proximity showed the same directional ordering but did not survive the global maxT correction; road proximity, built-up fraction and forest–human edge likewise did not provide an independent corrected mechanism. The apparent 5-km settlement signal therefore straddles the conventional 0.05 threshold across two defensible natural references and is absent after global correction under the predeclared matching definition.

The appropriate conclusion is **matching-sensitive short-scale settlement exposure**, not a robust anthropogenic effect or an “urbanization syndrome”. The sensitivity itself is informative: human interpretation depends partly on which natural environmental dimensions are used to define “locally comparable” white populations.

## Observation-process and phenology alternatives

The natural maps condition on the observed cells and their flower counts, and human comparisons are local. These features reduce but do not remove route-access sampling bias.

In the final 10,000-map replay, candidate cells did not show unusual measured within-dataset effort:

- observation-effort global maxT P ranged approximately 0.605–0.897 across the four scenarios;
- independent-site-support global maxT P ranged approximately 0.852–0.950.

Thus the settlement pattern is not explained simply by candidates having more photographs or more independent YAMAP source activities than their local white comparisons. However, the broader sampling frame can still favour populated or road-accessible landscapes entering the dataset at all.

Earlier diagnostics also found no coherent candidate-specific dark-colour or phenology convergence. The primary candidate set is therefore not defined by unusually dark flowers or a consistent flowering-date shift.

## Final ecological interpretation

Three statements are supported at different strengths.

1. **Natural-reference conclusion — robust:** locally discordant pigmented events occur at a frequency compatible with the fitted natural geography under both the primary and propagated eight-axis representations.
2. **Human-context conclusion — suggestive:** short-scale population exposure is the only repeatedly leading human feature, but its corrected strength depends on environmental matching and natural-reference specification.
3. **Provenance conclusion — unresolved:** population density is compatible with horticultural opportunity/propagule pressure, but the same pattern can arise through observation accessibility or fine-scale environmental plasticity. Current photographs provide no ancestry or planting-history evidence.

Accordingly, the 17 primary cells remain **local-departure / provenance targets**, not anthropogenic anomalies. The appropriate next tests are local planting and management histories, voucher-level morphology, repeated population colour sampling, standardized spectra and pigment chemistry, and population-genetic comparison of candidates with neighbouring white populations and relevant horticultural material.

## Reproducibility resources

Primary implementation:

- `R/natural_predictive_model.R` and `scripts/run_natural_predictive_model.R` — cross-fitted natural references;
- `R/local_pigmented_isolates.R` and `scripts/run_local_pigmented_isolates.R` — event definition;
- `scripts/refine_submission_isolate_null.R` — 10,000-map primary reference;
- `scripts/run_joint_submission_isolate_ppc.R` — 200,000-map joint posterior-predictive guardrail;
- `R/local_human_context.R` and `scripts/run_local_human_context.R` — local human contrasts;
- `R/did_sensitivity.R` and `scripts/run_did_sensitivity.R` — DID context;
- `R/candidate_doy_check.R` — phenology guardrail;
- `reproducibility/human_context_final_audit_results_2026-08-11.md` — final propagation/VIF/human-context adjudication.

Final high-rep adjudication:

- workflow run `31472542634`;
- artifact `9094339466`;
- artifact digest `sha256:413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`.

The frozen manuscript-facing natural reference remains workflow `31258851297`, artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

## References added for the human-context interpretation

Choi, M. S. et al. (2012). Breeding of purple flower-colored dwarf ‘Jiknyeo’ from hybridization of *Campanula punctata* Lam. × *Campanula punctata* Lam. var. *rubriflora* Mak. DOI: 10.7235/hort.2012.12015.

Davis, A. J. S. et al. (2016). Accounting for residential propagule pressure improves prediction of urban plant invasion. *Ecosphere* 7:e01232. DOI: 10.1002/ecs2.1232.

Dehnen-Schmutz, K., Touza, J., Perrings, C. & Williamson, M. (2007). A century of the ornamental plant trade and its impact on invasion success. *Diversity and Distributions* 13:527–534. DOI: 10.1111/j.1472-4642.2007.00359.x.

Mair, L. & Ruete, A. (2016). Explaining spatial variation in the recording effort of citizen science data across multiple taxa. *PLoS ONE* 11:e0147796. DOI: 10.1371/journal.pone.0147796.

Stiles, E. A. et al. (2007). Temperature-sensitive anthocyanin production in flowers of *Plantago lanceolata*. *Physiologia Plantarum*. DOI: 10.1111/j.1399-3054.2007.00855.x.
