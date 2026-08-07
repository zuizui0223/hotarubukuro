# Bombus SDM inference: active limitation gate, current ceiling, and stronger future design

## Active 1,909 question

The active local analysis now asks a directional, bounded question:

> Among nearby flower cells with similar measured environment, is pigmentation lower where **all five focal Bombus species have low predicted availability** than where **at least one focal species is moderately supported**?

This corresponds to a benefit-relaxation hypothesis: if bumblebees are important effective pollinators and pigmentation contributes to attraction, the benefit of maintaining a pigmented signal may weaken where focal bumblebees are poorly available. Pigment-production cost could reinforce the transition toward white flowers but is not measured and is not required for the prediction.

The active stage is not a test of measured visitation or selection.

## What stage 03 actually does

The manuscript-facing lower-third gate is:

- *Bombus-limited*: maximum within-species support rank across the five focal taxa <=0.33, so all five are in their lower third of predicted support;
- *Bombus-available*: at least one focal taxon has within-species support rank >=0.50.

Candidate pairs are formed before flower colour is read and must be:

- within 25 km;
- in the same held-out flower-model fold;
- on five-species common support;
- within environmental RMS distance <=0.75 on the four broad/within-50-km environmental axes; and
- used at most once in the one-to-one matched set.

The pair is oriented `limited -> available` from the Bombus surfaces alone. The primary response is the directed difference in pigmented share. Conditional intensity is secondary.

The lower-third gate was adopted as the active, biologically interpretable gate **after exploratory design development**. Therefore the full 0.10/0.20/0.25/0.33 grid and its across-grid multiplicity correction are retained. This prevents the lower-third result from being retrospectively relabelled as an independently preregistered test.

## Why environment and space are not fitted a second time locally

The national environment-plus-INLA-SPDE model defines broad flower-colour geography. The bee SDMs are also environmentally structured. Adding the same environment and another spatial field to a local regression would not magically identify a pollinator effect and could remove much of the ecological variation represented by predicted bee availability.

Instead, stage 03 controls the largest shared gradients by design:

1. geographic restriction to <=25 km;
2. environmental matching before the response is read; and
3. restriction to the same held-out flower-model fold.

The 1,000 cross-fitted flower natural maps are then replayed on the fixed matched pairs as a **predictive reference only**. They are not covariates in a second local regression. This asks whether the observed directed contrast is unusual relative to the broad natural geography without claiming that environment and Bombus have been fully separated.

## What is propagated

The stage-02 flower models generate 1,000 cross-fitted posterior-predictive maps. Stage 03 recomputes the directed matched-pair contrast on those maps. This propagates uncertainty and observation variation on the **flower natural-model side** while keeping the pair definition and Bombus orientation fixed.

## What is not propagated

The five Bombus prediction surfaces are checksum-locked archived inputs. The current pipeline does **not** propagate:

- uncertainty in the GBIF occurrence sample and filtering;
- accessible-area/background selection;
- observer-effort or opportunistic-record bias;
- ENMeval feature-class or regularization selection;
- fitted SDM parameter uncertainty;
- uncertainty among similarly supported candidate models; or
- uncertainty among alternative prediction surfaces.

The historical ENMeval candidate/tuning objects needed to replay the original selection path were not all retained. Therefore the active analysis is reproducible **conditional on the frozen prediction surfaces**, not from occurrence records to the final stage-03 result.

## Ecological interpretation ceiling

The within-species SDM ranks represent **predicted habitat availability / potential encounter opportunity**. They are not abundance, actual visitation to *Campanula punctata*, pollen transfer, reproductive success, pollination service or selection pressure.

The strongest manuscript statement is therefore of the form:

> Pigmentation was directionally greater on environmentally matched local endpoints with at least one moderately supported focal Bombus taxon than on endpoints where all focal taxa had low predicted support. The pattern is consistent with relaxation of a Bombus-associated attraction benefit under low predicted bumblebee availability, but it does not demonstrate visitation-mediated selection.

Because the bee predictions are themselves environmentally generated, unmeasured environmental structure, distribution history and observation bias can still create correspondence. The common-support restriction also means the current analysis concerns **low predicted availability within analyzable support**, not literal bumblebee absence.

## Why a summed “Bombus pressure” is not used

A raw sum of MaxEnt/ENMeval values would assume, without calibration, that:

1. suitability is proportional to abundance;
2. output scales are quantitatively comparable among species;
3. local abundance is proportional to visits to *C. punctata*; and
4. each species contributes the same selective effect per unit of the SDM score.

The gate avoids these assumptions. It asks whether all species are low on their **own** support scales and whether at least one species is locally moderate, without converting the values to visit counts.

## Stronger future SDM-only design

If the Bombus component is rebuilt from occurrences, the next version should improve the latent **availability** model rather than create a more elaborate pseudo-abundance index.

### 1. Freeze the complete source build

For every focal species preserve and hash:

- occurrence query, download and filtering rules;
- accessible region and background definition;
- environmental predictors and raster versions;
- observer-effort or target-group-bias controls;
- spatial folds;
- complete ENMeval tuning grid and selection criterion;
- fitted model objects or sufficient information to refit them; and
- every downstream prediction surface.

### 2. Generate spatially out-of-fold Bombus predictions

A flower cell should receive a species prediction only from a model trained without occurrence records from its held-out region. The downstream flower analysis should never evaluate a species surface on the same records used to fit that local portion of the SDM.

### 3. Preserve an ensemble rather than one surface

Use occurrence bootstrap/resampling and/or a predeclared set of similarly supported ENMeval candidates. Keep multiple valid prediction realizations instead of averaging first.

For pair A and B define the probability that every focal species is low at A and that at least one is available at B across the SDM ensemble. Pair orientation can then be restricted to high-confidence limitation contrasts, for example posterior/ensemble probability >=0.8.

This converts SDM uncertainty into uncertainty about the **gate itself**, which is more relevant than adding a standard error to one arbitrary summed score.

### 4. Consider foraging accessibility

The plant interacts with mobile foragers, not a single raster pixel. A stronger availability measure can kernel-average each species' predicted support over a predeclared surrounding foraging radius. If species-specific movement parameters are poorly known, use a common kernel plus fixed sensitivity radii rather than pretending precise species-specific foraging distances are known.

### 5. Preserve local environmental matching

Retain response-blind local matching on the raw abiotic predictors used in the bee SDMs. Compare raw and stricter calipers as sensitivities, but do not select the caliper by significance.

### 6. Propagate flower and bee uncertainty separately

For each Bombus ensemble realization, define high-confidence limited/available pairs and replay the flower natural-map reference. Summarize:

- distribution of the observed directed pigmentation contrast across SDM realizations;
- proportion of realizations with the predicted sign;
- distribution of predictive tail probabilities or null-standardized contrasts; and
- sensitivity to the gate thresholds.

The aim is not a single magical p-value but stability of the biological direction under uncertainty in both latent processes.

## Useful secondary analyses

The previously developed analyses remain useful as sensitivities but are not the active causal story:

- unsigned community-composition turnover tests whether flower-colour and predicted bee-community mosaics change together;
- strict five-of-five directional dominance tests a monotone “more Bombus everywhere” hypothesis and was not supported;
- environment-residualized community composition checks whether species-composition correspondence survives removal of measured broad environmental information.

These should not be mixed with the limitation gate as if they were interchangeable estimands.

## Stronger than any SDM-only test: field validation

No SDM-only redesign can establish the attraction mechanism. The next causal test should use the national analysis to choose bumblebee-limited/available transition zones and collect:

- species-resolved Bombus visitation or local occupancy/abundance;
- first approach and landing choices between white and pigmented flowers;
- standardized visible and UV reflectance;
- pollen removal/deposition and/or seed set; and
- the same local abiotic covariates used for matching.

The Izu-island system is especially valuable as independent motivation and a possible natural experiment because bumblebee-fauna changes and breeding-system shifts are already documented. Island flower colour, however, should be measured directly rather than treated as a causal result of the present nationwide SDM analysis.
