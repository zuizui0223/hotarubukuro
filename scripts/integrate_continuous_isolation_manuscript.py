#!/usr/bin/env python3
"""Integrate the validated continuous colour-isolation result into JBI text.

This script is intentionally idempotent and branch-scoped. It promotes the
all-cell continuous geometry to the descriptive human-context result, retains
the 16-event family as supplementary calibration/field targeting, and keeps
the post hoc status and causal ceiling explicit.
"""

from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
MAIN = ROOT / "submission/jbi/JBI_main_manuscript_anonymized.md"
APPENDIX = ROOT / "submission/jbi/supporting/Appendix_S6_event_departures_human_context.md"
JA = ROOT / "submission/jbi/JBI_translated_abstract_ja.md"


def replace_line(text: str, label: str, replacement: str) -> str:
    pattern = re.compile(rf"^{re.escape(label)}.*$", re.MULTILINE)
    updated, count = pattern.subn(replacement, text, count=1)
    if count != 1:
        raise RuntimeError(f"Expected one line beginning {label!r}; found {count}")
    return updated


def replace_section(text: str, start: str, end: str, replacement: str) -> str:
    pattern = re.compile(
        rf"^{re.escape(start)}\n.*?(?=^{re.escape(end)}\n)",
        re.MULTILINE | re.DOTALL,
    )
    updated, count = pattern.subn(replacement.rstrip() + "\n\n", text, count=1)
    if count != 1:
        raise RuntimeError(f"Expected one section {start!r} -> {end!r}; found {count}")
    return updated


def write_if_changed(path: Path, text: str) -> None:
    current = path.read_text(encoding="utf-8")
    if current != text:
        path.write_text(text, encoding="utf-8")


def update_main() -> None:
    text = MAIN.read_text(encoding="utf-8")

    text = replace_line(
        text,
        "**Methods:**",
        "**Methods:** We converted author-screened YAMAP hiking photographs into a national two-part phenotype: pigmentation state and pigmented-only intensity. INLA-SPDE models separated measured environmental associations from continuous residual geography, and 67 independently fixed white-pigmented boundaries supplied the local bumblebee comparison. For human context, we measured distance to the nearest occurrence of the same colour in all 1,305 cells, scaled it by local flower-cell spacing and replayed the geometry on 10,000 natural predictive maps; a threshold event detector was retained to define field targets.",
    )
    text = replace_line(
        text,
        "**Results:**",
        "**Results:** The curated dataset contained 1,922 observations; the matched YAMAP stream contained 3.81 times as many focal-species records as iNaturalist. Pigmentation state and intensity differed geographically; only state exceeded a cross-fitted spatial expectation along environmental difference. At local boundaries, focal-bumblebee support was higher on pigmented sides on average, while equal-elevation analysis showed that stronger-looking highland overlap was shared mountain geography. Pigmented cells farther from other pigmented cells were more population-exposed at 5 km (rho=0.252) than natural maps expected (mean 0.133; P=0.0002), and the association remained after scaling by local flower-cell spacing (rho=0.285; natural mean 0.154; P=0.0009). The raw white relationship did not retain its opposite sign after density correction. Predictive replay retained 16 event-defined populations as calibrated field targets.",
    )
    text = replace_line(
        text,
        "**Main conclusions:**",
        "**Main conclusions:** The study resolves one flower-colour map into distinct environmental, spatial, local biotic and provenance hypotheses. Changing scale revealed a small state-specific local Bombus pattern that broad maps would blur, while the highland guardrail showed why national overlap alone is insufficient. Human context was expressed most clearly as an excess positive isolation-population relationship within pigmented occurrences, not as proof of reciprocal colour displacement or horticultural origin. Integrative trait biogeography therefore gains resolution by connecting, rather than collapsing, processes that operate at different scales and comparison units.",
    )

    text = replace_section(
        text,
        "### 1.4 From broad geography to local boundaries and calibrated departures",
        "### 1.5 Predictions",
        """### 1.4 From broad geography to local boundaries and human overlays

The local pollinator test has a strong natural-history and scale basis in *C. punctata*. Bumblebees are effective pollinators of its tubular flowers, and geographical changes in *Bombus* fauna are linked to pollen-removal ecology and breeding-system change (Inoue & Amano, 1986; Inoue, 1988; Nagano et al., 2014). Pollinator-mediated selection is realized through local visitation, pollen transfer and reproductive success, and divergent selection on floral traits can arise among populations separated by less than 10 km when local pollinator assemblages differ (Torres-Vanegas et al., 2024). We therefore made nearby white-pigmented boundaries the primary comparison and asked whether focal-bumblebee habitat opportunity increased specifically from the white side to the pigmented side.

The 5-km radius is not assumed to be an exact bumblebee foraging or selection distance. It was the finest predeclared scale that retained enough replicated colour transitions and it targets the population-neighbourhood scale at which pollinator assemblages and floral selection can vary. National overlap and equal-elevation comparisons were then used as a guardrail: they ask whether a stronger-looking broad pattern can be explained by shared mountain environment. In this design, the local comparison is the biological test and the broad comparison shows why that localization is necessary.

The broad natural template also makes the spatial geometry of human context testable without first declaring a small set of anomalies. For every 1-km flower cell, we measured distance to the nearest other cell with the same colour state. This continuous isolation measure is defined for all pigmented and white cells and requires no event radius, environmental caliper, minimum-neighbour rule or residual cutoff. We then asked whether isolation covaried with population exposure within each colour, whether that relationship changed after scaling by the local spacing of all flower cells, and whether the fitted natural model generated the same pattern across 10,000 predictive maps.

A separate relational event—a pigmented cell surrounded by nearby environmentally similar white cells—was retained for a different job. Replaying that event on natural maps calibrates visual surprise and provides reproducibly selected populations for field history, vouchers and genetic assignment. Because *C. punctata* is cultivated, planting, escape or introgression remain plausible provenance hypotheses, but neither continuous geometry nor event membership is treated as ancestry evidence.""",
    )

    text = replace_section(
        text,
        "### 1.5 Predictions",
        "## 2. Materials and Methods",
        """### 1.5 Predictions and exploratory human-context question

The analysis follows one dependent sequence. First, if flower colour contains two ecological layers, pigmentation state and pigmented-only intensity should show different broad geography. Second, if focal bumblebees help maintain visible pigmentation, predicted habitat support should be higher on the pigmented side of nearby white-pigmented boundaries; the signal should be strongest at the local scale where plant-pollinator neighbourhoods differ, whereas broad highland overlap may reflect shared environment.

The all-cell isolation analysis was developed after inspection of the earlier event-based human-context results and is therefore explicitly post hoc rather than a preregistered third prediction. Its frozen guardrail question is narrower: are pigmented occurrences that lie farther from other pigmented occurrences more population-exposed than expected from the fitted natural flower-colour geography, and does that result survive local sampling-density correction? The event analysis remains a supplementary calibration and field-target procedure; an excess event count is not required for the continuous human-context question to be answerable.""",
    )

    text = replace_section(
        text,
        "### 2.5 Defining local departures before reading human context",
        "### 2.6 Reproducibility and inferential order",
        """### 2.5 Continuous colour isolation and event-based field targets

The human-context analysis used the 1,305 environment-complete 1-km cells. A cell was pigmented when it contained at least one pigmented observation and white otherwise, giving 674 pigmented and 631 white cells. For every cell, raw colour isolation was the Euclidean distance to the nearest other cell with the same colour state. The primary human feature was population-exposure rank within 5 km. We calculated Spearman correlations separately for pigmented and white cells and their direct difference, rho(pigmented) - rho(white). Focal, 10-, 25- and 50-km population ranks, DID proximity, land-cover variables, mountainness and observation-process measures were retained as labelled secondary features.

Raw nearest-neighbour distance can increase where the full flower-cell frame is sparse. We therefore calculated nearest and fifth-nearest distances to any flower cell and repeated the primary relationship with relative isolation, log(same-colour nearest distance / any-colour nearest distance). Five leave-one-geographical-fold-out estimates assessed regional stability. Within-colour feature values were permuted within geographical folds for descriptive restricted-randomisation checks.

The decisive natural-geography guardrail replayed the complete isolation geometry on 10,000 checksum-locked final-eight-axis predictive maps. Each simulated map used the same `count > 0` state rule, cell geometry and observed binomial trial counts. For every map we recomputed pigmented and white correlations and their difference. The primary 5-km comparison was reported for all nondegenerate maps and for a fixed count-conditioned sensitivity. Only 19 maps contained exactly the observed 674 pigmented cells, so the pre-specified fallback retained the 1,000 maps closest to the observed count. Population scales were evaluated as one maxT family. The motivating correlations were inspected before this analysis was specified; the analysis is reported as a post hoc exploratory generalisation, while the density and natural-map guardrails were frozen before the validated execution.

For continuity with the earlier workflow, we also retained the threshold-defined ecological event. A candidate required a pigmented focal cell with at least three neighbours within 10 km, root-mean-square environmental distance <=1 across the eight standardized abiotic axes, and only observed white flowers among eligible neighbours. Human variables were absent from the event rule. The identical detector was applied to 10,000 cross-fitted predictive maps to calibrate event count and fraction, after which predefined human contrasts were calculated. This event family now serves as supplementary calibration and as a reproducible selector of extreme field/provenance targets rather than the primary human-context estimand. Appendix S6 gives complete definitions and results.""",
    )

    text = replace_section(
        text,
        "### 2.6 Reproducibility and inferential order",
        "## 3. Results",
        """### 2.6 Reproducibility and inferential order

The study provides more than a fitted model: it exposes the full route from source images to manuscript claims. Derived data, source-construction code, analysis code, fixed seeds, software manifests, evidence identities and validation rules are versioned, and manuscript values are checked against checksum-locked artifacts in continuous integration. Phenotype construction precedes ecological predictors; the broad natural template precedes the local Bombus test; and Bombus pairs are fixed before Bombus values are compared. Event identities are fixed before their human variables are read.

The continuous-isolation analysis has a different evidential status and is labelled accordingly. Its motivating pattern was observed before formal specification, so it is not presented as preregistered confirmation. Reproducibility instead locks the exact all-cell geometry, sampling-density correction, frozen human feature registry, 2,000 restricted permutations and 10,000-map natural guardrail. This separates transparent exploration from outcome-dependent redefinition while preserving the paper's sequence of broad template, local mechanism and contemporary overlay.""",
    )

    text = replace_section(
        text,
        "### 3.4 Predictive replay converted apparent exceptions into calibrated field targets",
        "## 4. Discussion",
        """### 3.4 Continuous isolation revealed a pigmented human-context overlay

The 674 pigmented cells were strongly clustered: median distance to the nearest other pigmented cell was 3.61 km. Pigmented isolation increased with population exposure at the focal cell (rho=0.271), 5 km (rho=0.252) and 10 km (rho=0.172), but the raw correlation weakened at 25 km (rho=0.026) and was negative at 50 km (rho=-0.058). The corresponding raw white correlations were 0.003, -0.072, -0.141, -0.180 and -0.148. At 5 km, the direct raw colour contrast was therefore +0.324, and all five leave-one-fold-out estimates remained positive (0.267-0.391), although fold-specific strength varied.

The raw white sign reversal was not robust to sampling-density correction. When same-colour distance was scaled by nearest distance to any flower cell, the 5-km correlations were +0.285 for pigmented and +0.079 for white cells, giving a smaller direct difference of +0.207. Observation effort was unrelated to raw pigmented isolation (rho=-0.032; within-fold permutation P=0.429) and did not explain relative isolation.

Natural-map replay separated the focal pigmented relationship from the less stable colour contrast. For raw 5-km isolation, pigmented rho was +0.252 compared with a natural-map mean of +0.133 and a 95% interval of 0.071-0.196 (upper-tail P=0.0002). Relative isolation gave rho=+0.285 versus a natural mean of +0.154 and interval 0.068-0.236 (P=0.0009). The same conclusions held in the 1,000 nearest-count maps (raw P=0.0020; relative P=0.0040).

The direct raw colour difference lay just above its natural interval (observed +0.324; null mean +0.205; interval 0.088-0.316; P=0.0194; five-scale maxT P=0.0465), but the relative-isolation difference did not (observed +0.207; null mean +0.152; interval 0.044-0.260; P=0.1586; maxT P=0.1190). Thus the robust result is an excess positive isolation-population relationship within pigmented occurrences, not a density-independent reciprocal displacement of pigmented and white states. The observed raw correlation attenuated at broad radii, but observed-minus-natural displacement was not confined cleanly to focal-10-km scales.

### 3.5 Predictive replay retained extreme populations as field targets

Sixteen pigmented cells met the predefined local-departure rule. Applying the identical detector to 10,000 natural predictive maps produced a mean of 13.59 candidates and a 95% interval of 7-21. The observed count was compatible with that reference (P=0.27897), as was candidate fraction (observed 0.04071; null mean 0.03107; upper-tail P=0.12609).

The event-based human comparison remained directionally consistent with the continuous result: 5-km population exposure was +0.06744 rank units higher at candidates than at local white comparators (directional P=0.00800; global maxT FWER P=0.05479). These results do not make the 16 sites anthropogenic anomalies. They define extreme, reproducibly selected populations for planting-history, voucher, microenvironment and genomic tests, while the all-cell continuous analysis supplies the main descriptive human-context geometry.""",
    )

    text = replace_section(
        text,
        "### 4.4 Natural-map calibration produced interpretable field targets",
        "### 4.6 A spatially varying balance can maintain flower-colour polymorphism",
        """### 4.4 Human landscapes overlay the spatial geometry of pigmentation

The continuous analysis changes the human-context conclusion from a near-threshold result in 16 selected sites to an all-cell property of the pigmented distribution. Pigmented occurrences that were farther from other pigmented occurrences were more population-exposed than the fitted natural geography predicted. This was not only a consequence of sparse flower sampling: the pigmented relationship remained when same-colour distance was scaled by the local spacing of all flower cells, under all 10,000 natural maps and in the fixed near-count sensitivity.

The guardrails also prevent the clearest-looking descriptive contrast from being overstated. White cells showed a negative raw isolation-population relationship, but that sign disappeared after local flower-cell density was removed, and the raw white rho was almost exactly the natural expectation. The defensible inference is therefore not that human landscapes move the two colour states in perfectly opposite directions. It is that the pigmented state carries an additional positive human-context association beyond the broad environmental, spatial and sampling geometry already represented by the natural maps.

The scale profile does not uniquely identify mechanism. Raw pigmented correlations were concentrated from the focal cell through 10 km, but the excess over natural expectation extended more broadly after density correction. Horticultural planting or escape, human-modified establishment conditions, fine-scale environmental plasticity and access-linked observation remain distinguishable hypotheses rather than interchangeable explanations. The YAMAP mountain-route frame may both compress the full urban-rural gradient and overrepresent accessible edges. Repeated field history, voucher-level morphology, local environmental measurement and genomic comparison are required before the overlay can be assigned to provenance or causation.

### 4.5 Event calibration defines targets without turning them into causes

The earlier event analysis retains a useful but narrower role. A residual map can always make a few sites look extraordinary. By defining a relational ecological configuration and replaying it on 10,000 natural maps, we showed that 16 observed pigmented-among-white events are compatible with natural spatial variation. The event count is therefore a calibration guardrail, not a prerequisite for asking whether human context changes the geometry of all pigmented occurrences.

At the same time, the detector supplies 16 reproducible populations selected without human variables. The continuous analysis places them in a broader distribution rather than making them the sole evidence. They are extreme local configurations that can be revisited for planting and management histories, vouchers, repeated colour sampling, pigment chemistry and ancestry comparison. Their status is stronger than an anecdotal point and more honest than an anthropogenic anomaly: they are calibrated provenance targets.""",
    )

    text = replace_section(
        text,
        "### 4.6 A spatially varying balance can maintain flower-colour polymorphism",
        "## Acknowledgements",
        """### 4.6 A spatially varying balance can maintain flower-colour polymorphism

The full study converges on one ecological model. Climate can change the physiological expression, benefit and heat cost of anthocyanin pigmentation. Population history can preserve or redistribute colour variants across regions. Local bumblebee opportunity may modify the reproductive value of retaining a visible pigmented state. Human landscapes can overlay that natural geography by changing where isolated pigmented occurrences persist or are observed. These are not four interchangeable predictors; they leave distinguishable signatures on different components of colour and at different spatial scales.

Under this model, neither white nor pigmented flowers must be favoured everywhere. The same pigment can be physiologically useful under one thermal or moisture context, costly under another, and reproductively valuable only where effective visitors are locally available. Spatially changing benefits, combined with historical movement and persistence of variants, provide a coherent adaptive hypothesis for why one colour has not fixed across Japan. The current data do not directly estimate fitness or provenance, but they identify the phenotype component, geographical scale, candidate process and population set for each causal test.

The broader biogeographic contribution lies in this resolution. Repurposed images revealed the phenotype; broad models separated measured environment from coherent residual geography, while cross-fitted spatial-null replay detected environment-aligned divergence specifically for pigmentation state; local boundaries identified a short-range state-specific Bombus signal; an equal-elevation guardrail showed why broad highland overlap was not an adequate biotic test; continuous all-cell geometry identified a pigmented isolation-population relationship beyond natural geography and local sampling density; and event replay retained calibrated field targets without turning visual exceptions into causes. The study gains clarity by letting strong, heterogeneous and null results perform different inferential jobs within one connected sequence.""",
    )

    write_if_changed(MAIN, text)


def update_appendix() -> None:
    text = """# Appendix S6. Continuous colour isolation, natural-map guardrails and event-based provenance targets

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
"""
    write_if_changed(APPENDIX, text)


def update_japanese_abstract() -> None:
    text = """# 日本語要旨（Translated Abstract Not for Review 用）

**目的：** 花色多型は、生理的な環境応答と送粉者へのシグナルという複数の機能をもち得るため、白花・有色花の分化や維持には、非生物環境、個体群史、生物間相互作用、さらに近年の人為的影響が異なる空間スケールで関与する可能性がある。本研究では、ホタルブクロの色素発現状態と有色花内の可視色強度を分け、生態学的な問いごとに空間スケールと比較単位を変えることで、マクロな地理パターンから適応・歴史・局所相互作用に関する仮説をどこまで解像できるかを検討した。

**調査地域：** 日本。

**対象分類群：** ホタルブクロ *Campanula punctata*（キキョウ科）。

**方法：** 2023–2025年のYAMAP登山写真から白／有色という花色状態と、有色花に限った可視色強度の二段階形質を構築した。全国スケールでは環境＋INLA-SPDEモデルにより、測定された環境応答と連続的な残存空間構造を分けた。送粉者仮説では、5 km以内の67個の白―有色境界をBombus情報なしで固定し、主要送粉者 *Bombus ardens* と *B. diversus* の予測生息機会を局所比較した。人為的文脈については、1,305セルすべてで最近接の同色セルまでの距離を連続的な孤立度として求め、局所的な花セル密度で相対化したうえで、10,000枚の自然予測地図上に同じ幾何を再現した。従来の16地点イベント検出器は、主検定ではなく現地・provenance調査対象を選ぶ補助的解析として残した。

**結果：** 最終データは1,922枚の写真からなった。花色状態では暖地ほど有色化確率が低く、有色花内の強度ではTemperature PC1 × 気温季節性の相互作用が支持された。地理距離を揃えたspace-only nullとの比較では、環境差に沿う分化が花色状態で空間期待を上回ったが、有色花内強度では上回らなかった。67個の局所境界では有色側で主要Bombusの平均予測生息機会が高かったが、広域の高山性Bombusとの対応は等標高比較で消失した。有色セルでは、同じ有色セルから孤立するほど5 km人口曝露が高かった（rho=0.252）。これは自然地図の期待（平均rho=0.133、P=0.0002）を上回り、局所花セル間隔で相対化しても残った（rho=0.285、自然平均0.154、P=0.0009）。白花の生の相関は逆符号だったが、密度補正後には逆符号を保たなかった。16地点の局所イベント数は自然予測地図下で過剰ではなかった。

**主な結論：** ホタルブクロの花色多型は、一つの白―濃色軸ではなく、色素発現状態と発現後の強度という異なる生態的層をもつ。広域環境・空間史、局所的なBombus機会、人間景観は、同じ地図に重なりながら異なる比較単位で検出された。人為的結果の中心は「白花と有色花が完全に逆方向へ配置される」ことではなく、自然の花色地理と標本密度を考慮しても、有色花の孤立度が人口曝露と余分に正対応することである。これは園芸由来を証明せず、植栽史・標本・局所環境・遺伝解析を向ける仮説と対象個体群を具体化する。
"""
    write_if_changed(JA, text)


def main() -> None:
    update_main()
    update_appendix()
    update_japanese_abstract()


if __name__ == "__main__":
    main()
