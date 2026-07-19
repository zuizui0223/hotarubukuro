# Ecological interpretation of the Bombus SDM covariate

## Study-domain decision

The five focal species are mainland Bombus visitors used to characterize the mainland pollinator context of *Campanula punctata*. Restricting model calibration and prediction to a biologically stated mainland polygon is therefore defensible. The polygon must be stored as a reproducible spatial object and described directly (Honshu, Shikoku, Kyushu and specified adjacent islands), rather than reconstructed from latitude or polygon-area heuristics.

## What the stacked prediction is

For species `j`, let `s_j(x)` be the continuous Maxent prediction at location `x`. The current sum

`S(x) = sum_j s_j(x)`

is a **cumulative environmental suitability index for the five focal Bombus species**. It is not directly observed species richness, abundance, visitation rate, pollen delivery, or selection intensity.

The revised code therefore distinguishes:

- `bombus_suitability_sum`: cumulative continuous suitability;
- `bombus_suitability_mean`: mean suitability across the five species;
- `potential_species_richness`: number of species exceeding independently justified species-specific thresholds.

The manuscript should not call a continuous sum “richness.” If thresholded richness is used, conclusions must be checked across threshold choices. The continuous sum is preferable as the primary broad-scale covariate because thresholding discards information, but it must be named and interpreted conservatively.

## Defensible ecological chain

The analysis can support the following chain:

1. Climate, land cover and topography define where each focal Bombus species could plausibly occur.
2. Stacking the five predictions summarizes regional overlap in potential Bombus habitat.
3. A flower-colour association with this index shows that pigmentation covaries geographically with the potential mainland Bombus assemblage after measured abiotic gradients and residual spatial structure are considered.
4. This pattern is **consistent with**, but does not demonstrate, pollinator-mediated selection.

The analysis cannot by itself establish:

- local Bombus abundance;
- visitation frequency to *C. punctata*;
- effective pollen transfer;
- a Bombus colour preference in this species;
- a causal mediation pathway from climate through Bombus to pigmentation.

## Why “pollination pressure” is too strong

Presence-background suitability is not a calibrated abundance or activity measure. Abundance can be low in environmentally suitable cells because floral resources, nesting sites, competition, disease, land management and local history are omitted. Conversely, transient or locally abundant populations may occur in cells assigned moderate suitability. Summed suitability should therefore be described as **potential Bombus assemblage suitability**, not pollination pressure.

## Recommended model hierarchy

### Primary model

Use `bombus_suitability_sum` as a pre-specified covariate in the spatial model. Phrase the estimand as:

> the association between floral pigmentation and regional overlap in potential habitat suitability for five documented mainland Bombus visitors.

### Sensitivity analyses

Fit the same spatial model using:

1. mean suitability;
2. thresholded potential species richness;
3. each species separately;
4. leave-one-species-out cumulative suitability;
5. an abiotic-only model without the Bombus covariate.

Report whether the sign and magnitude are robust. If the association is driven almost entirely by *B. diversus*, interpret it as species-specific potential overlap rather than diversity.

### Environmental confounding check

Bombus suitability is itself predicted from climate and other environmental layers. Therefore, a regression containing both environmental PCs and Bombus suitability does not create an independent experimental contrast. Check:

- correlations and spatial concurvity between Bombus metrics and plant environmental predictors;
- coefficient stability with and without the Bombus term;
- variance inflation / posterior correlation;
- spatial cross-validation or blocked predictive comparison;
- residual association after flexible environmental effects.

A remaining Bombus coefficient is evidence of additional predictive information encoded by the SDMs, not proof of a biotic causal effect.

## SEM recommendation

Do not present the current SEM as causal mediation. The mediator is a deterministic prediction derived partly from the same environmental variables used as upstream causes. Regressing predicted suitability on those variables largely reconstructs the SDM rather than observing an independent biological mediator.

Replace the causal language with one of these options:

1. **Remove SEM from the main paper.** Keep the spatial regression and species-specific sensitivity analyses. This is the strongest option with the available data.
2. Present SEM only as a descriptive path decomposition and state explicitly that indirect paths are not identified causal mediation effects.
3. Reserve causal mediation for future data containing local Bombus visitation or abundance measured independently of the environmental predictors.

## Manuscript-safe language

Recommended:

> Floral pigmentation was positively associated with cumulative habitat suitability for five mainland Bombus visitors. This association indicates geographic correspondence between pigmentation and the potential Bombus assemblage, but suitability is not a measure of local abundance or visitation and the result does not by itself demonstrate pollinator-mediated selection.

Avoid:

- “pollinator activity increased pigmentation”;
- “pollination pressure drove darker flowers”;
- “temperature affected colour primarily through pollinators”;
- “SEM demonstrated causal mediation”;
- “Bombus richness” when the predictor is a sum of continuous suitability values.

## Strongest biological interpretation available now

The defensible contribution is not that SDMs prove selection. It is that a broad-scale pigmentation pattern contains a biotic-geographic component aligned with the potential distributional overlap of known mainland Bombus visitors, beyond a specified set of abiotic covariates and spatial dependence. This generates a testable pollinator-mediated hypothesis for field visitation, bee-vision colour measurements and reciprocal/common-garden work.
