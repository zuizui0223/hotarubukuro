# Final Broad-to-human-context audit results

Date: 2026-08-11

## Decision summary

The finalized Broad analysis does **not** justify replacing the downstream local-departure/human analysis wholesale. Three distinctions are essential.

1. The accepted pigmentation-state observation-level Broad model did not acquire VPD, site water balance or a new interaction. It remains the eight-axis additive environment + stationary SPDE + East/West model. The only promoted Broad interaction is in pigmented-only intensity, which is not the response used by the local pigmented-departure/human analysis.
2. The local-departure stage has its own predeclared multiscale four-PC environmental matching representation. Replacing that representation by the eight observation-level Broad axes is therefore a sensitivity analysis, not an automatic consequence of Broad finalization.
3. That sensitivity is not exactly invariant: it changes two of the 17 observed candidate identities. The current four-PC natural reference remains the primary downstream reference because it retains the predeclared local broad-versus-within environmental decomposition and has slightly better geographically cross-fitted predictive performance than the direct eight-axis alternative.

The final human-context claim remains **post-selection short-scale settlement exposure, suggestive and environmentally-definition-sensitive**, not an anthropogenic-causation or horticultural-provenance result.

## 1. Final VIF policy

A hard rule of either VIF <5 or VIF <10 is not used as a universal model-validity switch. The adopted policy is:

- **VIF <5:** preferred;
- **VIF 5–10:** retain only with explicit stability evidence for the focal term, geographically blocked transfer, and spatial/coefficient robustness;
- **VIF >10:** do not promote an environmental expansion without exceptional mechanistic and predictive support.

This is consistent with O'Brien (2007), who cautioned against mechanical VIF rules, and with ecological guidance emphasizing stronger scrutiny at lower VIF values when individual coefficient interpretation matters (Zuur, Ieno & Elphick 2010; Dormann et al. 2013).

Exact VIFs from the frozen 1,922-observation Broad evidence are:

### Pigmentation state

- maximum VIF = **4.430** for Soil PC1;
- Temperature PC1 = 4.146;
- all other fixed terms <2.62.

The state model therefore passes even a hard VIF <5 convention.

### Conditional intensity

With East/West and the retained Temperature PC1 × temperature-seasonality interaction:

- maximum VIF = **6.340** for Temperature PC1;
- Soil PC1 = 5.233;
- retained thermal interaction = **1.664**;
- East/West = 1.822.

Removing East/West lowers the maximum VIF to 4.885 and the focal interaction VIF to 1.585, but the Broad spatial audit showed that this no-region model worsened full fit by about 5.9 WAIC units and did not obtain robust blocked-transfer support. East/West is therefore not deleted simply to cross an arbitrary VIF=5 boundary.

The elevated intensity-model VIF is mainly shared geography among Temperature PC1, Soil PC1 and temperature seasonality; it is not driven by the promoted interaction. In the pigmented subset the pairwise correlations are approximately Temperature–Soil = -0.731, Temperature–temperature-seasonality = -0.673 and Soil–precipitation = +0.719, whereas the thermal interaction itself has only moderate correlations with the main axes.

The rejected +VPD expansions had VIFs around 25.8–26.0 and did not improve geographical transfer, so their rejection is unchanged under either a 5 or 10 convention.

## 2. Does Broad finalization propagate into the human-analysis upstream reference?

### Direct dependency

No accepted new pigmentation-state variable propagates automatically. The downstream human act uses local **pigmentation-state** departures. The promoted Broad Temperature × temperature-seasonality interaction belongs only to conditional intensity and has no direct path to that event detector.

### Environmental-matching sensitivity

Nevertheless, the final eight observation-level environmental axes were propagated deliberately as a guardrail.

At the same raw RMS environmental caliper (=1):

- current four-PC matching: 17 candidates;
- eight-axis matching: 16 candidates;
- overlap: 15;
- Jaccard: 0.833.

Because RMS distance in eight correlated dimensions changes neighbour support, the eight-axis caliper was then calibrated **without colour or human data** to match the current graph's support distribution. The selected caliper was 0.81:

- current mean neighbours = 3.4774; supported cells (>=3 neighbours) = 657;
- calibrated eight-axis mean neighbours = 3.4835; supported cells = 657.

Under this like-for-like support calibration:

- candidate count returns to 17;
- 15 of the original 17 remain;
- two current candidates are replaced by two alternative candidates;
- Jaccard = **0.789**.

One of the two newly admitted eight-axis candidates has population-5-km rank 0.980 and built-up rank 0.969. This explains part of why the human-context contrast strengthens after changing environmental matching: the change is caused by candidate composition, not by human variables entering the detector.

### Natural-reference predictive comparison

Using 10,000 predictive maps and the same five geographical folds:

| Model | Mean negative log predictive mass | AUC | Brier |
|---|---:|---:|---:|
| current multiscale four-PC natural reference | **0.57248** | **0.86332** | **0.15060** |
| direct final-eight-axis natural reference | 0.57409 | 0.86033 | 0.15163 |

The current reference is slightly better on all three primary diagnostics. There is therefore no evidence-based reason to replace it merely because the observation-level coefficient model is described with eight environmental axes.

## 3. Are the local pigmented departures themselves excessive under natural maps?

No. With 10,000 natural maps the observed candidate count is 17 in the current and support-calibrated eight-axis graphs.

Primary candidate-count tests remain non-significant:

- current model + current graph: p = 0.1996;
- current model + calibrated eight-axis graph: p = 0.2163;
- final-eight-axis model + current graph: p = 0.1830;
- final-eight-axis model + calibrated eight-axis graph: p = 0.1854.

Candidate-fraction two-sided p-values are 0.175, 0.191, 0.141 and 0.147, respectively. Thus the event frequency itself does not establish an extra process beyond the natural reference.

## 4. Final human-feature family

The previous feature surface contained many near-duplicate ranks and composites. They are no longer interpreted as independent ecological mechanisms. Examples include:

- artificial-land score versus artificial-land fraction: Spearman rho about 0.986;
- transport-access score versus road proximity: rho about 0.983;
- population-DID composites versus their constituent population/DID axes: approximately 0.97 in the prior DID audit.

The final confirmatory/guardrail family is therefore mechanism-based:

1. population exposure at focal, 5, 10, 25 and 50 km scales;
2. DID proximity (dense-settlement convergence check);
3. road proximity (transport/access hypothesis);
4. built-up fraction (managed/built habitat context);
5. forest-human edge (managed-natural interface);
6. forest cover and mountainness as two-sided natural alternatives.

All eleven variables are included in one maxT family. No population radius is promoted from its unadjusted P value after looking at the data.

## 5. Final 10,000-map human-context result

The only repeatedly leading feature is **population density at 5 km**.

### Predeclared current four-PC matching

- observed focal-minus-white-neighbour contrast = 0.0531 rank units;
- directional p = 0.0305 under the current natural model;
- global maxT FWER p = **0.2059**;
- under the refitted eight-axis natural model, global maxT FWER p = **0.2366**.

This is not familywise-supported.

### Support-calibrated eight-axis matching

- observed population-5-km contrast = 0.0654;
- current natural model: directional p = 0.00620, global maxT FWER p = **0.0463**;
- eight-axis natural model: directional p = 0.00680, global maxT FWER p = **0.0552**.

Thus the short-scale population signal becomes strong under the alternative eight-axis environmental matching, but its familywise classification straddles 0.05 across two defensible natural references. More importantly, it is absent after global correction under the predeclared matching definition. It is therefore classified as a **matching-sensitive short-scale settlement-exposure signal**, not a robust additional anthropogenic process.

Population 10 km and DID proximity show the same directional ordering but do not survive the global maxT correction. Road proximity, built-up fraction and forest-human edge likewise do not provide independent familywise-supported mechanisms. There is therefore no coherent multivariate 'urbanization syndrome' in the current evidence.

## 6. Observation-process alternative

The natural-map reference conditions on the observed 1-km cells and their observation counts, and the human comparison is local against white neighbours. These design choices reduce but cannot eliminate accessibility bias in which places enter the YAMAP dataset.

Measured local effort alternatives do not show candidate-specific departures in the 10,000-map replay:

- observation-effort global maxT p ranges from 0.605 to 0.897 across the four final scenarios;
- independent-site-support global maxT p ranges from 0.852 to 0.950.

This weakens a simple explanation based only on more photographs or more independent YAMAP activities at candidate cells. It does **not** remove the broader sampling-frame possibility that populated/accessible landscapes are more likely to appear in the source dataset at all. Citizen-science literature shows that road access and population density can predict spatial recording effort, so this remains an explicit competing interpretation.

## 7. Ecological hypothesis audit

### Supported as plausible context hypotheses

**Short-scale settlement / horticultural opportunity.** Population density is a defensible proxy for nearby gardens, planted material, repeated human contact and propagule opportunity. Across ornamental plants, horticultural market availability/propagule pressure can affect escape and invasion success (Dehnen-Schmutz et al. 2007). This is a general pathway analogy, not evidence that the present Japanese cells were planted.

**Horticultural colour material is biologically plausible in the taxon.** Purple-flowered material has been deliberately bred from *Campanula punctata* × var. *rubriflora* (Choi et al. 2012). This supports plausibility of horticultural manipulation/movement of pigmented material but does not identify the provenance of any YAMAP flower.

**Potential gene exchange after contact.** Allozyme work on *C. punctata* reports substantial outcrossing in self-incompatible mainland/Oshima populations and strong geographical differentiation (Inoue & Kawahara 1990). If planted and wild material coexist, gene flow is therefore biologically possible, but current photographs contain no ancestry evidence.

**Transport/disturbance is a plausible general pathway.** Roads can mediate secondary seed dispersal and disturbed establishment in plants (e.g. Lemke et al. 2019). In this dataset road proximity is not independently supported, so this remains background rationale rather than a result.

### Mandatory alternatives

**Observation accessibility.** Population density and road access can increase citizen-science recording effort (Mair & Ruete 2016). YAMAP is route-based opportunistic imagery, so a settlement association cannot be interpreted only as biological exposure.

**Fine-scale environmental modification/plasticity.** Flower anthocyanin can be temperature-sensitive, with darker flowers/greater anthocyanin under cooler conditions in experimental systems (e.g. Stiles et al. 2007). Gardens, shade, irrigation, soil modification and urban microclimate can therefore generate local phenotype differences not captured by kilometre-scale long-term climate. A settlement signal is not automatically a genetic or provenance signal.

## 8. Final claim ceiling

The strongest defensible statement is:

> Locally discordant pigmented cells show a suggestive short-scale association with settlement exposure under an alternative, more fully environmental matching definition, but this signal is sensitive to the natural-reference/matching specification and does not establish an additional anthropogenic process.

If a shorter biological interpretation is needed:

> The human-context analysis identifies **provenance/field targets**, not anthropogenic anomalies.

Do not state that the candidates are planted, escaped cultivars, horticultural introgressants or human-caused colour morphs. Testing those hypotheses requires field history, voucher-level morphology, repeated population sampling and population-genetic comparison among candidate populations, nearby white populations and relevant horticultural material.

## Evidence provenance

- frozen Broad/human reference artifact: `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`;
- first Broad-propagation/human audit: workflow `31470301344` and later successful reruns;
- population-scale multiplicity guardrail: workflow `31471431799`, artifact `9093623399`, digest `sha256:392caa562b236f89994edf03333e360a0caaa9f80bb4d43e1d0ff65b87ab10e6`;
- response-blind support-calibrated 1,000-map guardrail: workflow `31472085293`, artifact `9093798757`, digest `sha256:0812425340336e4d6f3108019c1b6110dc5f97ddef67b787875fe2859d38732f`;
- final 10,000-map adjudication: workflow `31472542634`, artifact `9094339466`, digest `sha256:413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`.
