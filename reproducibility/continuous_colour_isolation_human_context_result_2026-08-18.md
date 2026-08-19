# Locked result: continuous colour-isolation human context

**Status:** completed and validated as a post hoc exploratory generalisation.

**Specification:** `v23.0_continuous_colour_isolation_human_context`

This analysis is not preregistered. The motivating raw correlations and the idea of a white comparison were inspected before the design note was committed. The additional sampling-density and 10,000-natural-map guardrails were fixed in `reproducibility/continuous_colour_isolation_human_context_2026-08-18.md` before the validated execution reported here.

## 1. Validated execution and provenance

- workflow: `Continuous colour-isolation human context`;
- workflow run: `32116805570`;
- tested branch head: `8bcdf82292bece55f8f2cca6d6e18baf35ccd98a`;
- artifact: `9317087893`;
- artifact name: `continuous-colour-isolation-9652a67678dd0f6e06630b98cafcd6ed73feb69c-32116805570`;
- artifact ZIP SHA-256: `6fd26d9a938b68d3f0c56512cd1620597c740d44ba91ab5a7ccbb9daa99d5386`;
- workflow conclusion: `success`.

The workflow passed focused unit tests, verified both frozen input ZIP checksums, analysed all 1,305 cells, replayed the geometry on all 10,000 final-eight-axis natural maps, validated every output contract and wrote an 18-file internal checksum manifest. Independent download verification recovered the same artifact ZIP SHA-256 and all 18 internal checksums without mismatch.

## 2. Threshold-free observed geometry

Colour state was defined identically to the retained analyses: a cell was pigmented when `n_pigmented > 0` and white otherwise. Raw isolation was the Euclidean distance to the nearest other cell with the same colour state. No event radius, environmental caliper, minimum-neighbour rule, residual cutoff or candidate threshold entered this quantity.

| Quantity | Pigmented | White |
|---|---:|---:|
| Cells | 674 | 631 |
| Median nearest same-colour distance | 3.605551 km | 4.123106 km |
| Spearman rho with focal population | +0.270675 | +0.003185 |
| Spearman rho with 5-km population | **+0.251980** | -0.071544 |
| Spearman rho with 10-km population | +0.171803 | -0.140706 |
| Spearman rho with 25-km population | +0.025556 | -0.180213 |
| Spearman rho with 50-km population | -0.057818 | -0.147627 |

At 5 km, the direct raw colour contrast was:

\[
\rho_{pigmented}-\rho_{white}
=0.251980-(-0.071544)=0.323524.
\]

All five leave-one-geographical-fold-out estimates of this raw 5-km contrast remained positive: `0.267233`, `0.306648`, `0.312990`, `0.338032` and `0.390880`. Fold-specific contrasts were heterogeneous (`0.520584`, `0.434835`, `0.377150`, `0.243953`, `0.051226`), showing that the national result is not equally strong in every region.

## 3. Sampling-density correction changes the white-control reading

Raw nearest-neighbour distance can increase wherever the full flower-cell frame is sparse. The primary geometric sensitivity divided same-colour nearest distance by nearest distance to any flower cell and log-transformed the ratio.

At 5 km:

| Metric | Pigmented rho | White rho | Direct difference |
|---|---:|---:|---:|
| Raw same-colour distance | +0.251980 | -0.071544 | +0.323524 |
| Relative isolation, `log(same NN / any NN)` | **+0.285498** | +0.078506 | +0.206992 |

Therefore the headline **“white occurrences show the opposite population relationship” is not robust to geometric sampling-density correction**. The raw white negative relationship becomes weakly positive after relative isolation is used. The robust descriptive feature is instead that the pigmented relationship remains positive and stronger than the white relationship.

Observation effort was not associated with raw pigmented isolation (`rho = -0.031662`, within-fold feature-permutation `P = 0.428786`). Under relative isolation it was `rho = +0.065238` (`P = 0.106447`). Independent-site support likewise did not explain the pigmented result. These controls do not replace the geometric correction but argue against a simple focal-cell effort explanation.

## 4. Natural-map guardrail: direct colour contrast

Each of the 10,000 locked natural maps was converted to the same `count > 0` state rule, after which nearest same-colour distance and the population correlations were recomputed from scratch.

### Raw isolation, 5-km population

| Quantity | Value |
|---|---:|
| Observed rho difference | **+0.323524** |
| Natural-map mean | +0.204692 |
| Natural-map SD | 0.058254 |
| Central natural interval | 0.087897–0.315987 |
| Upper-tail Monte Carlo P | 0.019398 |
| Population-scale maxT P | 0.046495 |

The observed raw contrast is just above the upper edge of the central natural interval. Across the five population scales, the all-map maxT result is also at the edge of the familywise distribution.

Only 19 natural maps had exactly the observed 674 pigmented cells, below the fixed minimum of 200. The deterministic fallback therefore retained the 1,000 maps closest to the observed pigmented count; their maximum count difference was 19 cells.

Under this count-conditioned sensitivity:

- 5-km raw difference upper-tail `P = 0.034965`;
- conditioned population-scale maxT `P = 0.076923`.

Thus the single 5-km raw contrast remains elevated, while its familywise separation is weaker when map-level pigmented counts are closely matched.

### Relative isolation, 5-km population

| Quantity | Value |
|---|---:|
| Observed rho difference | +0.206992 |
| Natural-map mean | +0.151743 |
| Natural-map SD | 0.055081 |
| Central natural interval | 0.043865–0.259876 |
| Upper-tail Monte Carlo P | 0.158584 |
| Population-scale maxT P | 0.118988 |

The direct colour contrast therefore **does not clearly exceed natural geography after local flower-cell density is removed**. The conditioned relative results are weaker again (`P = 0.242757`; conditioned maxT `P = 0.173826`).

## 5. Natural-map guardrail: the focal pigmented relationship

The focal biological statement is narrower than the direct colour contrast:

> Are pigmented occurrences that are farther from other pigmented occurrences more population-exposed than expected from the fitted natural flower-colour geography?

This component is robust to both principal guardrails.

### 5-km population

| Pigmented rho | Observed | Natural mean | Central natural interval | Upper-tail P |
|---|---:|---:|---:|---:|
| Raw same-colour distance, all maps | **+0.251980** | +0.132980 | 0.071008–0.196076 | 0.000200 |
| Raw, count-conditioned maps | **+0.251980** | +0.148578 | 0.085447–0.213286 | 0.001998 |
| Relative isolation, all maps | **+0.285498** | +0.153616 | 0.068209–0.236059 | 0.000900 |
| Relative, count-conditioned maps | **+0.285498** | +0.165475 | 0.078298–0.246119 | 0.003996 |

The raw white 5-km rho, in contrast, was almost exactly the natural expectation (`observed = -0.071544`, natural mean `-0.071713`, upper-tail `P = 0.495650`). The apparent raw sign reversal therefore mainly reflects a natural white-geography pattern plus an additional positive displacement in the pigmented relationship. Relative isolation removes the raw white reversal, but the additional pigmented association remains.

This distinction is the main scientific update produced by the guardrails:

> **The evidence is strongest for an excess positive isolation–population relationship within pigmented occurrences, not for a robustly opposite relationship between pigmented and white occurrences.**

## 6. Scale profile

The observed raw pigmented correlations were strongest from the focal cell through 10 km and disappeared by 25–50 km. However, the fitted natural model expected increasingly negative pigmented correlations at broad radii, so the observed-minus-natural displacement remained positive through 25 km and was still directionally elevated at 50 km.

Relative isolation produced positive pigmented correlations at every radius:

| Radius | Observed relative pigmented rho | Natural mean | Upper-tail P |
|---:|---:|---:|---:|
| focal | +0.274619 | +0.156645 | 0.001200 |
| 5 km | +0.285498 | +0.153616 | 0.000900 |
| 10 km | +0.252812 | +0.127920 | 0.002500 |
| 25 km | +0.190872 | +0.075435 | 0.005699 |
| 50 km | +0.121861 | +0.034881 | 0.035096 |

Consequently, **“the effect exists only at short distance” is too strong**. The raw correlation attenuates at broader radii, but the excess over the natural expectation is not confined cleanly to focal–10-km scales. The profile is compatible with a local-to-regional human-context overlay and does not uniquely identify short-range horticultural propagule pressure.

## 7. Role of land cover and controls

Raw pigmented isolation was positively associated with artificial land, built land, agriculture and forest–human edge and negatively associated with mountainness. White raw isolation showed the same land-cover signs, although generally smaller population relationships. After relative isolation, the land-cover relationships weakened but remained positive for pigmented cells.

Because several land-cover signs occur in both colours, they are evidence of a general landscape/access geometry rather than independent colour-specific proof. Population and DID provide the clearest pigmented-specific descriptive displacement, while the natural-map and density corrections set its claim ceiling.

## 8. Scientific decision

This analysis is substantially better than the 16-event detector for describing the human-context pattern because it uses every colour cell, avoids threshold-defined candidates and directly quantifies spatial geometry. It should be considered for the **main descriptive Part 4 axis**.

It does **not** justify deleting the earlier calibrated event analyses. Their revised roles are:

- v20–v22: supplementary calibration, alternative natural-null estimands and reproducible selection of field/provenance targets;
- continuous isolation: main descriptive geometry and the all-cell human-context result;
- the former 16 candidates: extreme field targets, not the statistical foundation of the main human-context claim.

Promotion to the manuscript should use the guarded statement below rather than the original unqualified sign-reversal claim:

> Pigmented flower occurrences were spatially clustered, yet pigmented cells farther from other pigmented cells occurred in more population-exposed landscapes than expected from the fitted natural flower-colour geography. This relationship remained after isolation was scaled by local flower-cell spacing. White occurrences did not retain a robust opposite relationship under the same density correction, so the result supports a pigmented-specific human-context overlay rather than a simple reciprocal displacement of the two colour states.

## 9. Claim ceiling

The result does not establish horticultural origin, planting, escape, establishment, phenotypic plasticity, pollen movement, gene flow or causation by people. It identifies a spatial pattern that strengthens human-context and provenance hypotheses and provides a clearer basis for targeted field history, voucher and genetic tests.
