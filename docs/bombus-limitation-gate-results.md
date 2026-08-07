# Bombus-limitation gate sensitivity: 1,909 frozen analysis

## Biological hypothesis

This exploratory analysis was designed around a directional benefit–relaxation hypothesis rather than a generic community-turnover association:

> When all focal *Bombus* species have low predicted local availability, the attraction benefit of pigmentation should be relaxed and white flowers should be relatively more common. Where at least one effective *Bombus* species is locally available, pigmentation can retain an attraction-mediated benefit. A physiological cost of pigment production could reinforce loss of pigmentation when that benefit is absent, but pigment cost is not measured here and is not required for the primary prediction.

The secondary prediction was that stronger *Bombus* opportunity might be associated with greater visible intensity among already pigmented flowers. Because SDM support is not visitation rate, this dose-like prediction is intentionally treated as weaker.

## Reproducible run

- workflow: `Bombus limitation gate`
- Actions run: `31168019534`
- source head: `b956a1fa320c248fb8f8646ba77b4a74a36ac140`
- output artifact: `bombus-limitation-gate-b956a1fa320c248fb8f8646ba77b4a74a36ac140`
- artifact SHA-256: `4a6dad8d98f212515eb1ec2f72c40f59b82f6f06b755ced3168bc5dcf77d9384`
- frozen input artifact: canonical 1,909 submission run `31149006557`
- flower predictive reference: 1,000 cross-fitted environment-plus-SPDE posterior-predictive maps

## Exposure definition

No cross-species sum of MaxEnt/ENMeval suitability was called `Bombus pressure`.

For each 1-km flower cell, the analysis used the five within-species ranks of predicted habitat support and defined:

- `best Bombus support rank` = the maximum rank across the five focal species;
- a cell as *Bombus-limited* when this maximum was below a fixed low threshold, meaning **all five focal species** were below that within-species rank;
- a cell as *Bombus-available* when at least one focal species had rank >= 0.50.

Four low thresholds were fixed before the run: 0.10, 0.20, 0.25 and 0.33. The 0.33 definition has a simple interpretation: every focal species is in its lower third of predicted support, while the available endpoint has at least one species at or above its median.

## Local design

The analysis deliberately did **not** fit a second local environment-plus-space model.

Pairs were constructed without flower-colour information and required:

- <=25 km geographic separation;
- the same held-out flower-model fold;
- environmental RMS distance <=0.75 on the four broad/within-50-km environmental axes; and
- one Bombus-limited endpoint and one Bombus-available endpoint.

Pairs were greedily matched one-to-one using environmental similarity and geographic distance only, so a cell was not repeatedly reused. Each pair was oriented `limited -> available` before flower colour was read.

The primary statistic was the mean directed pigmentation-share difference:

`pigmented share_available - pigmented share_limited`.

The 1,000 national flower predictive maps were used only as a predictive reference: the same fixed pairs and direction were replayed on every map to ask whether the observed contrast exceeded what the fitted broad natural geography would generate. Environment and SPDE were therefore not reintroduced as local regression covariates.

## Results

The strictest 0.10 gate had no eligible local matched pairs. The remaining fixed thresholds all showed a **positive pigmentation direction**.

| all-species low threshold | matched pairs | pigmentation difference (available - limited) | natural-null mean | upper-tail p | BH q within threshold | BH q across all gate tests |
|---|---:|---:|---:|---:|---:|---:|
| 0.20 | 2 | +0.250 | +0.050 | 0.341 | 0.681 | 0.638 |
| 0.25 | 6 | +0.333 | -0.001 | 0.053 | 0.106 | 0.159 |
| 0.33 | 22 | **+0.223** | +0.001 | **0.017** | **0.034** | 0.102 |

Thus, in the only gate with a useful local matched sample, flower cells where every focal *Bombus* species was in its lower third of predicted support had substantially less pigmentation than nearby environmentally similar cells where at least one focal species was at or above its median support.

The 0.33 pairs occurred in all five spatial folds rather than one region only. The mean directed pigmentation contrast by fold was positive in four folds and exactly zero in the fifth; no fold had a negative mean. Across the nine non-zero pair differences, eight were positive and one negative. These fold/sign summaries are descriptive robustness checks, not additional confirmatory tests.

Because several low thresholds were examined as a fixed exploratory grid, the across-grid BH correction was 0.102. The result should therefore be treated as **directionally coherent exploratory support**, not as a newly pre-registered confirmatory finding.

## Conditional intensity

The intensity prediction was not supported in low-versus-available matched pairs. At the 0.33 gate only six pairs had pigmented observations at both endpoints, and the observed directed intensity contrast was negative (approximately -0.615; upper-tail p=0.904). The tighter gates had only one usable intensity pair each.

A broad descriptive correlation within cells classed as Bombus-available was positive (Spearman rho approximately 0.248 across 602 pigmented cells), but this is not the local matched inferential test and can contain broad environmental structure. It should not be used as evidence that greater Bombus opportunity causes darker flowers.

## Ecological interpretation

The result fits the two-part flower-colour biology better than a simple dose-response interpretation.

- **Pigmentation state:** the data are directionally consistent with a threshold-like hypothesis in which low availability of all focal *Bombus* species relaxes the benefit of maintaining a conspicuous pigmented signal.
- **Pigmentation intensity:** there is currently no local matched evidence that stronger predicted Bombus opportunity produces darker flowers once pigmentation is already present.

This suggests a biologically coherent division of labour for the hurdle response: pollinator availability may be more relevant to the **presence/absence of pigmentation as a signal**, whereas the amount of visible pigmentation among pigmented flowers may be more strongly governed by abiotic physiology, genetic background or other processes.

## What this does and does not test

This design is stronger than regressing flower colour on a summed SDM score because it uses a gate that has a within-species interpretation and local environmental matching. It also avoids fitting environment and spatial structure twice in the local model.

However, the *Bombus* exposure is still occurrence-derived predicted habitat support. It is not abundance, actual visitation to *Campanula punctata*, pollen transfer, fitness or selection pressure. The bee predictions themselves are environmentally generated, so shared or unmeasured environmental structure cannot be completely separated even after local matching.

The manuscript-safe interpretation is therefore:

> Among nearby environmentally similar cells, pigmentation was greater on the side with at least one moderately supported focal *Bombus* species than on the side where all five species had low predicted support, with the clearest contrast for the lower-third limitation gate. This pattern is consistent with relaxation of a Bombus-associated attraction benefit when focal bumblebees are poorly available, but it does not demonstrate visitation-mediated selection or pigment-production costs.

The Izu-island natural history is best used as independent biological motivation for this hypothesis rather than as if it were already tested by the nationwide SDM contrast.
