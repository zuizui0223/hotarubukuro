# Bombus-limitation gate: design-development result and active local hypothesis

## Biological hypothesis

The directional benefit–relaxation hypothesis is:

> When all focal *Bombus* species have low predicted local availability, the attraction benefit of pigmentation should be relaxed and white flowers should be relatively more common. Where at least one focal *Bombus* species is locally available, pigmentation can retain a potential attraction-mediated benefit. A physiological cost of pigment production could reinforce loss of pigmentation when that benefit is absent, but pigment cost is not measured here and is not required for the primary prediction.

The secondary prediction is that stronger *Bombus* opportunity might be associated with greater visible intensity among already pigmented flowers. Because SDM support is not visitation rate, this dose-like prediction is intentionally weaker.

## Design-development run

The gate family was first evaluated in the frozen 1,909 analysis in:

- workflow: `Bombus limitation gate`
- Actions run: `31168019534`
- source head: `b956a1fa320c248fb8f8646ba77b4a74a36ac140`
- output artifact: `bombus-limitation-gate-b956a1fa320c248fb8f8646ba77b4a74a36ac140`
- artifact SHA-256: `4a6dad8d98f212515eb1ec2f72c40f59b82f6f06b755ced3168bc5dcf77d9384`
- frozen input artifact: canonical 1,909 submission run `31149006557`
- flower predictive reference: 1,000 cross-fitted environment-plus-SPDE posterior-predictive maps

The 0.10, 0.20, 0.25 and 0.33 low-support thresholds were fixed as a grid before that exploratory run. **The later decision to adopt the 0.33 lower-third gate as the active manuscript-facing gate was made after inspecting this design-development grid.** It must therefore not be described as preregistered or independently confirmatory. The active pipeline preserves the complete grid and its across-grid multiplicity correction.

## Exposure definition

No cross-species sum of MaxEnt/ENMeval suitability is called `Bombus pressure`.

For each 1-km flower cell, the analysis uses the five within-species ranks of predicted habitat support and defines:

- `best Bombus support rank` = maximum rank across the five focal species;
- *Bombus-limited* = this maximum is below the low threshold, meaning all five focal species are low on their own support scale;
- *Bombus-available* = at least one focal species has rank >=0.50.

The 0.33 gate has a simple biological interpretation: every focal species is in its lower third of predicted support, whereas the available endpoint has at least one species at or above its median support.

## Local design

The local stage deliberately does **not** fit a second environment-plus-space model.

Pairs are constructed without flower-colour information and require:

- <=25 km geographical separation;
- the same held-out flower-model fold;
- environmental RMS distance <=0.75 on the four broad/within-50-km environmental axes; and
- one *Bombus*-limited and one *Bombus*-available endpoint.

Pairs are greedily matched one-to-one using environmental similarity and geographical distance only. Each pair is oriented `limited -> available` before flower colour is read.

The primary statistic is:

`pigmented share_available - pigmented share_limited`.

The 1,000 national flower predictive maps are a predictive reference only: the same fixed pairs and orientation are replayed on every map. Environment and SPDE are not reintroduced as local regression covariates.

## Design-development results

The 0.10 gate had no eligible local matched pairs. The other gates all showed a positive pigmentation direction.

| all-species low threshold | matched pairs | pigmentation difference (available - limited) | natural-null mean | upper-tail p | BH q within threshold | BH q across all gate tests |
|---|---:|---:|---:|---:|---:|---:|
| 0.20 | 2 | +0.250 | +0.050 | 0.341 | 0.681 | 0.638 |
| 0.25 | 6 | +0.333 | -0.001 | 0.053 | 0.106 | 0.159 |
| 0.33 | 22 | **+0.223** | +0.001 | **0.017** | **0.034** | 0.102 |

The lower-third pairs occurred in all five spatial folds. Mean directed pigmentation contrast was positive in four folds and zero in the fifth, with none negative. Across the nine non-zero pair differences, eight were positive and one negative. These are descriptive robustness checks.

The correct evidence statement is therefore **directionally coherent exploratory support**. The nominal lower-third predictive tail is strong, but the active gate was chosen after design development and the retained across-grid BH q is 0.102.

## Conditional intensity

Conditional intensity was not supported. At the lower-third gate only six pairs had pigmented observations at both endpoints and the directed contrast was approximately -0.615 (upper-tail p=0.904). The tighter gates had only one usable intensity pair each.

A broad descriptive correlation within cells classed as *Bombus*-available was positive (Spearman rho approximately 0.248 across 602 pigmented cells), but this is not the local matched inferential test and can contain broad environmental structure. It is not evidence that greater *Bombus* opportunity causes darker flowers.

## Ecological interpretation

The result fits the two-part flower-colour biology better than a simple dose-response model.

- **Pigmentation state:** directionally consistent with a threshold-like hypothesis in which low availability of all focal *Bombus* species relaxes the benefit of maintaining a conspicuous pigmented signal.
- **Pigmentation intensity:** no local matched evidence that stronger predicted *Bombus* opportunity produces darker flowers once pigmentation is already present.

This does not demonstrate the mechanism. The *Bombus* exposure is occurrence-derived predicted habitat support, not abundance, actual visitation to *C. punctata*, pollen transfer, fitness or selection pressure. The bee predictions are environmentally generated, so shared or unmeasured environmental structure cannot be completely separated even after local matching. The common-support requirement also means that the current test concerns low predicted availability within analyzable support rather than literal bumblebee absence.

The manuscript-safe interpretation is:

> Among nearby environmentally similar cells, pigmentation was greater on the side with at least one moderately supported focal *Bombus* species than on the side where all five species had low predicted support. The lower-third contrast is directionally coherent but remains exploratory after retaining the complete gate-development grid. The pattern is consistent with relaxation of a *Bombus*-associated attraction benefit when focal bumblebees are poorly available, but it does not demonstrate visitation-mediated selection or pigment-production costs.

The Izu-island natural history is independent motivation for this hypothesis, not evidence that the nationwide SDM contrast has already established island flower-colour evolution.
