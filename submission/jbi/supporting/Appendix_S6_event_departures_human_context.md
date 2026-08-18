# Appendix S6. Continuous colour isolation, natural-map guardrails and event-based provenance targets

## Scope and inference ceiling

This Appendix separates two human-context questions that use the same 1,305-cell flower-colour frame but different estimands.

1. **Continuous geometry:** as a cell becomes more isolated from other occurrences of the same colour, does its human context change, and does the pigmented relationship exceed the fitted natural flower-colour geography?
2. **Event calibration:** how often does the finalized natural model generate the more restrictive configuration of a pigmented cell surrounded by environmentally similar white cells, and which populations should be revisited as extreme provenance targets?

The continuous analysis is a post hoc exploratory generalisation. Its motivating correlations and white comparison were inspected before formal specification. Sampling-density correction, five-fold diagnostics and the 10,000-map natural guardrail were fixed before the validated execution. Neither analysis can demonstrate planting, escape, horticultural introgression, gene flow or human causation.

## S6.1 Threshold-free continuous isolation

Colour state was defined as pigmented when a 1-km cell contained at least one pigmented observation and white otherwise. This gave 674 pigmented and 631 white cells. Raw same-colour isolation was the Euclidean distance to the nearest other cell with the same state.

The measure is defined for every cell and contains no event radius, environmental caliper, minimum-neighbour rule, residual cutoff or candidate threshold. The primary human variable was rank-transformed population exposure within 5 km. Spearman rho was calculated within pigmented and white cells, and the direct colour contrast was rho(pigmented) - rho(white).

Raw nearest-neighbour distance can increase where the complete flower-cell frame is sparse. The primary geometric sensitivity was therefore:

`relative isolation = log(same-colour nearest distance / any-colour nearest distance)`.

A fifth-nearest-any-cell denominator, observation-effort rank and independent-site-support rank were additional diagnostics. Five leave-one-geographical-fold-out estimates assessed regional stability. Feature values were permuted within colour-by-fold strata for descriptive restricted-randomisation checks.

## S6.2 Observed isolation-human relationships

Median raw same-colour isolation was 3.605551 km for pigmented cells and 4.123106 km for white cells.

**Table S6.1. Population-scale Spearman correlations for raw isolation.**

| Population scale | Pigmented rho | White rho | Direct difference |
|---|---:|---:|---:|
| focal cell | +0.270675 | +0.003185 | +0.267491 |
| **5 km** | **+0.251980** | **-0.071544** | **+0.323524** |
| 10 km | +0.171803 | -0.140706 | +0.312509 |
| 25 km | +0.025556 | -0.180213 | +0.205769 |
| 50 km | -0.057818 | -0.147627 | +0.089809 |

All five leave-one-fold-out 5-km direct differences were positive: 0.267233, 0.306648, 0.312990, 0.338032 and 0.390880. Fold-specific differences were heterogeneous (0.520584, 0.434835, 0.377150, 0.243953 and 0.051226), so the national pattern is not equally strong in every region.

**Table S6.2. Sampling-density sensitivity at 5 km.**

| Isolation metric | Pigmented rho | White rho | Direct difference |
|---|---:|---:|---:|
| raw same-colour distance | +0.251980 | -0.071544 | +0.323524 |
| relative, same NN / any NN | **+0.285498** | +0.078506 | +0.206992 |
| relative, same NN / any fifth NN | +0.239650 | +0.049250 | +0.190400 |

The raw white negative sign is therefore not robust to local flower-cell density. The stable descriptive feature is the positive pigmented relationship.

Raw pigmented isolation was positively associated with artificial land (rho=0.259522), forest-human edge (0.261464), built land (0.250268) and agriculture (0.231037), and negatively associated with mountainness (-0.361222). White raw isolation showed the same land-cover signs. These shared signs are treated as general landscape/access geometry rather than independent colour-specific proof.

Observation effort was unrelated to raw pigmented isolation (rho=-0.031662; within-fold feature-permutation P=0.428786). Relative pigmented isolation also lacked a clear effort relationship (rho=+0.065238; P=0.106447). Independent-site support did not explain the pigmented pattern.

## S6.3 Natural-map guardrail

The complete isolation geometry was replayed on 10,000 checksum-locked final-eight-axis predictive maps. Every map used the same 1,305 cells, cell-level binomial trial counts and `simulated pigmented count > 0` state rule. Nearest same-colour distance, relative isolation and population correlations were recomputed from scratch.

### S6.3.1 Direct colour contrast

**Table S6.3. Natural-map comparison of the 5-km direct rho difference.**

| Metric | Observed | Natural mean | Natural SD | 95% interval | Upper P | five-scale maxT P |
|---|---:|---:|---:|---:|---:|---:|
| raw isolation | **+0.323524** | +0.204692 | 0.058254 | 0.087897-0.315987 | 0.019398 | 0.046495 |
| relative isolation | +0.206992 | +0.151743 | 0.055081 | 0.043865-0.259876 | 0.158584 | 0.118988 |

Only 19 maps exactly matched the observed 674 pigmented cells, below the fixed minimum of 200. The deterministic fallback retained the 1,000 maps closest to the observed count; the maximum count difference was 19 cells. Under that sensitivity, the raw 5-km direct difference remained elevated (P=0.034965) but conditioned maxT weakened to 0.076923. The relative direct difference did not separate (P=0.242757; conditioned maxT=0.173826).

The direct colour contrast is therefore sensitive to how local flower-cell density and map-level colour counts are handled.

### S6.3.2 Focal pigmented relationship

**Table S6.4. Natural-map comparison of pigmented rho at 5 km.**

| Isolation/null set | Observed rho | Natural mean | 95% interval | Upper P |
|---|---:|---:|---:|---:|
| raw, all maps | **+0.251980** | +0.132980 | 0.071008-0.196076 | 0.000200 |
| raw, nearest-count maps | **+0.251980** | +0.148578 | 0.085447-0.213286 | 0.001998 |
| relative, all maps | **+0.285498** | +0.153616 | 0.068209-0.236059 | 0.000900 |
| relative, nearest-count maps | **+0.285498** | +0.165475 | 0.078298-0.246119 | 0.003996 |

The raw white rho was almost exactly its natural expectation (observed -0.071544; natural mean -0.071713; upper P=0.495650). Relative white rho was positive (observed +0.078506; natural mean +0.001873) but less stable across all and count-conditioned maps.

The strongest guarded conclusion is therefore:

> Pigmented occurrences farther from other pigmented occurrences are more population-exposed than expected from fitted natural flower-colour geography, and this relationship remains after local flower-cell spacing is removed. A robust reciprocal white displacement is not supported.

## S6.4 Scale interpretation

**Table S6.5. Relative pigmented isolation across population scales.**

| Radius | Observed rho | Natural mean | 95% interval | Upper P |
|---:|---:|---:|---:|---:|
| focal | +0.274619 | +0.156645 | 0.072546-0.238519 | 0.001200 |
| 5 km | +0.285498 | +0.153616 | 0.068209-0.236059 | 0.000900 |
| 10 km | +0.252812 | +0.127920 | 0.040631-0.212178 | 0.002500 |
| 25 km | +0.190872 | +0.075435 | -0.014515-0.163736 | 0.005699 |
| 50 km | +0.121861 | +0.034881 | -0.059923-0.129392 | 0.035096 |

Raw observed correlations attenuated at 25-50 km, but observed-minus-natural displacement extended more broadly. The profile does not uniquely identify a short-range horticultural propagule-pressure mechanism. It is described as a local-to-regional human-context overlay whose causal pathway remains unresolved.

## S6.5 Supplementary event calibration and field targets

The retained event detector required a pigmented focal cell, at least three neighbours within 10 km, root-mean-square environmental distance <=1 across the eight standardized abiotic axes, and only observed white flowers among eligible neighbours. Human variables were absent from selection.

**Table S6.6. Observed event frequency relative to 10,000 natural predictive maps.**

| Metric | Observed | Null mean | 95% null interval | Monte Carlo P |
|---|---:|---:|---:|---:|
| candidate count | **16** | 13.5908 | 7-21 | 0.27897 |
| candidate fraction | **0.04071** | 0.03107 | 0.01573-0.04861 | 0.12609 upper-tail |

Neither event count nor fraction was excessive. A stricter joint tail diagnostic was also non-excessive. The 16 cells are therefore calibrated extreme configurations, not departures that nature fails to reproduce.

After candidate identities were fixed, population exposure within 5 km was +0.06744 rank units higher than in local white comparators (directional P=0.00800; global maxT FWER P=0.05479). Observation-effort and independent-site-support contrasts were null after the same correction. This event result is directionally consistent with the continuous geometry but is retained as a supplementary alternative estimand and field-target selector.

## S6.6 Ecological interpretation and claim ceiling

The evidence has three levels.

1. **Pigmented spatial geometry — robust exploratory pattern.** The positive pigmented isolation-population relationship exceeds the frozen natural geography in raw, density-scaled, all-map and nearest-count comparisons.
2. **Reciprocal colour contrast — sensitivity dependent.** The raw pigmented-white difference is elevated, but the white negative sign and the relative direct contrast do not survive every density and natural-map guardrail.
3. **Provenance — unresolved.** Horticultural planting or escape, human-modified establishment, fine-scale plasticity and access-linked observation remain viable alternatives. Photographs provide no ancestry or planting-history evidence.

Stronger tests require local planting and management histories, vouchers, repeated population colour sampling, standardized spectra and pigment chemistry, local microenvironment, and population-genetic comparison among isolated pigmented populations, neighbouring white populations and horticultural material.

## S6.7 Reproducibility resources

Continuous isolation:

- entry script: `analysis_sensitivity/run_continuous_colour_isolation_human_context.R`;
- workflow: `.github/workflows/continuous-colour-isolation-human-context.yml`;
- successful run: `32116805570`;
- tested branch head: `8bcdf82292bece55f8f2cca6d6e18baf35ccd98a`;
- output artifact: `9317087893`;
- artifact ZIP SHA-256: `6fd26d9a938b68d3f0c56512cd1620597c740d44ba91ab5a7ccbb9daa99d5386`;
- result lock: `reproducibility/continuous_colour_isolation_human_context_result_2026-08-18.md`.

Event calibration:

- script: `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`;
- workflow: `.github/workflows/human-context-highrep-final.yml`;
- successful run: `31537102360`;
- output artifact: `9119306089`;
- artifact digest: `sha256:f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4`.

Shared frozen inputs:

- final-eight-axis predictive maps: artifact `9094339466`, SHA-256 `413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`;
- cell/human reference: artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

## References

Choi, M. S. et al. (2012). Breeding of purple flower-colored dwarf ‘Jiknyeo’ from hybridization of *Campanula punctata* Lam. x *Campanula punctata* Lam. var. *rubriflora* Mak. DOI: 10.7235/hort.2012.12015.

Davis, A. J. S. et al. (2016). Accounting for residential propagule pressure improves prediction of urban plant invasion. *Ecosphere* 7:e01232. DOI: 10.1002/ecs2.1232.

Dehnen-Schmutz, K., Touza, J., Perrings, C. & Williamson, M. (2007). A century of the ornamental plant trade and its impact on invasion success. *Diversity and Distributions* 13:527-534. DOI: 10.1111/j.1472-4642.2007.00359.x.

Mair, L. & Ruete, A. (2016). Explaining spatial variation in the recording effort of citizen science data across multiple taxa. *PLoS ONE* 11:e0147796.
