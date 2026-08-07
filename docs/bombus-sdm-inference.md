# Bombus SDM inference: current ceiling and stronger design

## Current 1,909 analysis

The active local analysis tests a bounded question:

> Do nearby flower-colour transitions align with turnover in a fixed, predicted five-species *Bombus* community fingerprint more strongly than expected from the fitted flower environment-plus-SPDE natural model and the observed sampling design?

This is deliberately weaker than a test of pollinator-mediated selection.

### What is propagated

The stage-02 flower models generate 1,000 cross-fitted posterior-predictive maps. Stage 03 recomputes the identical local pair statistic on every map. This propagates uncertainty and observation variation on the flower natural-model side while preserving the same response-blind graph, shared endpoints, geographic distance, environmental distance, expected natural colour difference and sampling-support structure.

### What is not propagated

The five *Bombus* prediction surfaces are checksum-locked archived inputs. Stage 03 treats them as fixed. The current 1,000-map reference therefore does **not** propagate:

- uncertainty in the GBIF occurrence sample and filtering;
- accessible-area/background selection;
- ENMeval feature-class or regularization selection;
- fitted SDM parameter uncertainty;
- uncertainty among similarly supported candidate models; or
- uncertainty among alternative prediction surfaces.

The historical ENMeval candidate/tuning objects needed to reproduce the original model-selection path were not retained. Consequently, the downstream analysis is reproducible **conditional on the frozen prediction surfaces**, not from occurrence records to final local inference.

### Ecological interpretation ceiling

The fingerprint is relative predicted habitat support and composition. It is not abundance, visitation, pollen transfer, pollination effectiveness or selection pressure. Because the *Bombus* surfaces are themselves functions of abiotic environment, a flower-colour/*Bombus* association can still arise from shared or incompletely measured environmental structure. The present environmental-distance adjustment and flower natural-model reference reduce that problem but cannot identify a causal interaction.

The active result should therefore be described as a **scale-specific predicted-community / flower-trait turnover correspondence**.

## Why simply fitting a JSDM is not a complete fix

Joint species distribution models are useful for separating measured environmental responses from residual multispecies association, but residual association is not automatically a biological interaction. Missing environmental covariates, spatial structure and community confounding can generate residual dependence. An environment-dependent JSDM can be a useful sensitivity analysis, but it does not by itself convert opportunistic distributions into causal interaction data.

Relevant methodological examples include Pollock et al. (2014; doi:10.1111/2041-210X.12180), Tikhonov et al. (2017; doi:10.1111/2041-210X.12723), Godsoe et al. (2017; doi:10.1002/ece3.2657), and the community-confounding analysis of Spence et al. (2022; doi:10.1038/s41598-022-15694-6).

## Recommended SDM-only redesign

If the study is rerun from occurrences, the preferred design is a **cross-fitted, environment-orthogonal, double-uncertainty turnover analysis**.

### 1. Freeze the complete Bombus source build

For each species, preserve and hash:

- occurrence download/query and all filtering rules;
- accessible region and background sample;
- environmental predictors and raster versions;
- spatial folds;
- candidate feature classes and regularization multipliers;
- ENMeval selection criterion and the complete candidate table;
- fitted model objects or sufficient information to refit every retained candidate; and
- every prediction raster used downstream.

The model-selection and prediction path must be rerunnable before SDM uncertainty can be propagated credibly. ENMeval 2.0 provides the model-evaluation framework, but the project must preserve the concrete inputs and candidate outputs needed to replay it (Kass et al. 2021; doi:10.1111/2041-210X.13628).

### 2. Generate out-of-fold Bombus predictions

Use spatially blocked cross-fitting for each *Bombus* species. A flower cell in a held-out region should receive its *Bombus* prediction only from a model fitted without occurrence records from that region.

This does not make the predictor causal, but it avoids evaluating the downstream correspondence against an in-sample species surface and makes the predictive target explicit.

### 3. Represent SDM uncertainty with multiple valid surfaces

Within each spatial fold, generate multiple defensible *Bombus* predictions by resampling occurrences and/or retaining a small ensemble of similarly supported ENMeval candidates. Store the realizations rather than averaging them immediately.

The analysis should ask whether the flower-colour result is stable across these realizations. Useful outputs are:

- median and 95% interval of the local beta across SDM realizations;
- proportion of realizations with the same sign;
- proportion whose observed statistic exceeds its flower-natural-model reference; and
- sensitivity to the choice of candidate-model weighting.

A single p-value conditional on one selected surface should no longer be the only inferential summary.

### 4. Separate composition turnover from total habitat support

Do not make a combined Euclidean fingerprint the only primary predictor. Use two pre-specified components:

1. **community-composition turnover**, based on Hellinger-transformed or otherwise compositional species support; and
2. **total habitat-support difference**, treated separately as a secondary exposure.

This distinction is biologically useful because a change in which *Bombus* species are supported is a different hypothesis from a change in total predicted bumblebee habitat support.

### 5. Remove measured abiotic information from the Bombus exposure inside training folds

The strongest SDM-only control for shared environment is to residualize the *Bombus* community summaries against the **same raw abiotic predictors used to build the bee SDMs**, using training data only. The residualization should be flexible enough to capture the nonlinear relationships used by the SDMs and must be applied to held-out cells without refitting.

The primary local predictor can then be turnover in the out-of-fold, environment-orthogonalized community composition. Raw fingerprint turnover remains a transparent secondary analysis.

This asks whether species-specific predicted community structure contains local information beyond the measured abiotic gradients. It still does not prove interaction because unmeasured environment and SDM misspecification remain possible.

### 6. Strengthen the local pair design with environmental matching

Keep the graph response-blind, but define the primary 25-km comparison among pairs that also satisfy a pre-specified environmental caliper using the raw SDM predictors. This is stronger than relying only on a linear environmental-distance covariate after the pairs have been formed.

A practical primary design is:

- endpoints within 25 km;
- both endpoints on common *Bombus* support;
- same flower-model held-out fold;
- environmental Mahalanobis or standardized Euclidean distance below a pre-specified caliper; and
- at most a fixed number of nearest matched neighbours per cell.

The present unrestricted 25-km graph can remain as a sensitivity analysis.

### 7. Propagate flower and Bombus uncertainty together

For *Bombus* realization `b` and flower posterior-predictive map `m`, calculate the identical pair statistic. This creates a nested reference rather than holding the predictor surface fixed.

For each *Bombus* realization, standardize the observed local beta against the corresponding flower-natural null. Then summarize the distribution of these null-standardized statistics across *Bombus* realizations. This keeps flower-model uncertainty and predictor-SDM uncertainty conceptually separate rather than mixing them into one opaque p-value.

### 8. Preserve scale as a biological hypothesis

Use 25 km as the pre-specified primary scale only if it is justified before viewing the new results. Retain 10 and 50 km as sensitivities. The purpose is not to search for the most significant radius but to test whether a local correspondence is concentrated at a scale compatible with turnover in pollinator assemblages.

### 9. Add explicit negative controls

At minimum include:

- composition turnover versus total-support turnover;
- raw versus environment-orthogonalized *Bombus* turnover;
- primary environmentally matched graph versus the unrestricted local graph; and
- the same analysis at the pre-specified sensitivity radii.

If the association appears only for raw environment-derived surfaces and disappears after orthogonalization/matching, interpret it as shared environmental geography rather than specific biotic information.

## Stronger than an SDM-only test: integrated field validation

No SDM-only redesign can establish pollinator-mediated selection because the predictor remains predicted habitat suitability rather than the interaction itself. The strongest next step is to use the SDM analysis to choose transition zones and then collect direct local data:

- species-resolved visitation or occupancy/abundance;
- flower reflectance or colour state;
- pollen transfer and/or seed set; and
- the same abiotic covariates used in the broad-scale models.

The SDM can then serve as a prior ecological-context layer or site-selection tool, while the direct visitation/fitness data test the interaction mechanism. A mediation-style or hierarchical model can ask whether local *Bombus* community composition explains flower-colour differences after measured environment, rather than treating SDM suitability itself as the selection pressure.

## Recommended manuscript language under the current 1,909 pipeline

Use wording of the form:

> Local turnover in predicted *Bombus* community composition corresponded to turnover in flower pigmentation state beyond the fitted flower natural-model expectation at the pre-specified scale. Because the *Bombus* fingerprint is derived from fixed habitat-suitability surfaces and SDM uncertainty is not propagated, this result is interpreted as a scale-specific biotic-context correspondence rather than evidence of visitation, interaction strength or pollinator-mediated selection.

Avoid wording that states or implies that the analysis estimated a causal *Bombus* effect.
