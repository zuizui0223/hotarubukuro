# Flower-colour geography and the bumblebee hypothesis — manuscript text

**Editorial rule:** In the Bombus analysis, the **only explanatory variable is the two-species SDM index** `max(A_ardens, A_diversus)`; flower-colour state is the response. No temperature, soil, elevation, precipitation, environmental dissimilarity or spatial surface is added to this test. Environmental confounding belongs explicitly in **Discussion**. The separate national Broad environment + SPDE component remains unchanged.

## Ecological motivation

On the Japanese mainland, *Campanula punctata* provides a biologically constrained pollination system because bumblebees are documented major pollinators. Pollen-transfer experiments demonstrate selection on corolla morphology through *B. diversus*, and geographical differences in bumblebee assemblages correspond to floral-size variation (Kobayashi et al. 1997; Nagano et al. 2014). These studies do not demonstrate bee preferences for white versus pigmented corollas. On several Izu Islands, halictid bees are predominant, preventing a general claim that this species is pollinated only by bumblebees (Inoue & Amano 1986).

The separate behavioural hypothesis is that bumblebee colour learning and repeated visits to locally common flowers could produce **positive frequency-dependent reproductive success**, reinforcing white-dominated or pigmented-dominated patches. Bumblebee colour-frequency and flower-constancy effects are known from other experimental flower systems (Smithson & Macnair 1996, 1997); the mechanism remains **untested** in these *Campanula* data. Unlike a directional habitat-suitability prediction, positive frequency dependence can favor **either colour** depending on its local frequency.

## Manuscript-ready English

### Introduction

> Bumblebees are major recorded pollinators of mainland *Campanula punctata*, and experiments show that pollen-transfer efficiency can shape its corolla morphology. Their biological importance makes predicted bumblebee habitat suitability a relevant, narrowly specified correlate of flower-colour geography, although the existing pollination studies do not establish a preference for pigmentation. Bumblebee flower constancy also suggests a separate possibility: repeated visits to locally common colour morphs could reinforce contrasting colour states across neighbouring populations. We tested the association with predicted bumblebee habitat suitability; frequency-dependent pollination itself was not measured.

### Methods: SDM-only pollinator analysis

> We summarized the occurrence-referenced predicted habitat suitability for the widespread focal pollinators *Bombus ardens* and *B. diversus* as the larger of their two species-specific scores. We first reported a descriptive rank association between this single index and the observed pigmented fraction of 1-km flower cells across the full dataset. Our primary local test used 67 previously fixed non-overlapping pure white–pigmented transitions within 5 km. Pairs were selected without using bumblebee scores and oriented from the white to the pigmented endpoint only after selection. The mean paired change in the SDM index was compared with a directional sign-flip null. We retained the fixed 10- and 25-km and raw-cloglog SDM checks. **No environmental predictors were entered into these pollinator analyses.**

### Results: direction depends on comparison unit

> Across 1,305 flower cells, the observed pigmented fraction was negatively associated with the focal-bumblebee SDM score (descriptive Spearman's rho = -0.306). In contrast, the 67 sharp 5-km white–pigmented pairs had a weakly positive mean pigmented-minus-white SDM contrast (+0.0359; original one-sided P = 0.0272; across the three pure-transition distances, BH q = 0.0815). The median was slightly negative (-0.0028), and only 33 of 67 pairs had greater suitability on the pigmented side. The mean contrast decreased at 10 km (+0.0084) and 25 km (+0.0029); it was not reproduced using untransformed cloglog SDM values (5-km P = 0.267). Nearly all sharp pairs (66/67) contained just one image at one or both endpoints. Thus, the local positive mean was heterogeneous and sensitive to the SDM scale; it was not a general relationship across the entire sample.

### Discussion: limitation placed here, not added as a covariate

> The importance of bumblebees in the pollination biology of mainland *C. punctata* makes them plausible contributors to floral-colour maintenance. Nevertheless, the SDM-only associations differed between nationwide flower cells and selected sharp local transitions, and the local response was not consistent among pairs. Crucially, the bumblebee suitability maps were derived from environmental variables that may also influence flower colour. **We therefore cannot separate a pollinator effect from shared environmental geography**, and we do not interpret these associations as pollinator-mediated selection. The sharp photo-colour transitions also do not establish abrupt boundaries between reproductively stable single-colour populations, because photographic replication is sparse. Bumblebee flower constancy could in principle reinforce whichever colour state is locally common, but testing this frequency-dependent mechanism requires morph-frequency manipulations, individual-bee visit sequences, per-flower pollen deposition and reproductive fitness. This remains a natural-history-based hypothesis rather than a result of the present SDM analysis.

## Figure 3 and Supporting Information

- Figure 3: observed focal 5-km white/pigmented pair contrasts in **one SDM predictor** only; show the weak mean alongside the median and proportion positive. Do not label it a pollinator-selection test.
- Supplement: SDM scale and pair-distance checks, details of the 67-pair photographic support. Five-species community summaries are contextual, not a second directional mechanism.
- Do not draw a causal or frequency-dependent fitness curve as though it had been measured. Any optional schematic must be explicitly labeled `hypothesis, not observed`.

## What can actually test flower constancy

At genuinely mixed local populations, or in experimental arrays where white/pigmented presentation frequency is randomized while flower reward, number and position are controlled, record sequential visits by individually tracked bees, per-flower stigma pollen deposition, male pollen export/paternity if possible, and reproductive output. The key quantity is **relative per-flower reproductive success as a function of the local frequency of that flower colour**. A positive SDM-colour correlation cannot supply this quantity.

## Reproducibility

The active SDM-only calculation and full output are in [the executed 2026-11-07 evidence record](bombus_sdm_only_validation_2026-11-07.md). It uses the fixed 1,305-cell colour table, the two-species SDM support surface, and already fixed pair identities; it does not alter the canonical INLA-SPDE, SDM source build, pair-selection code or numerical-result locks.

## Literature

- Kobayashi S, Inoue K, Kato M (1997), *Oecologia*: pollen transfer efficiency and corolla size under *B. diversus* visitation.
- Nagano Y et al. (2014), *Ecology and Evolution*: geographical bumblebee turnover and floral size.
- Inoue K & Amano M (1986), *Plant Species Biology*: variation in pollinator communities including Izu Island halictids.
- Inoue K (1988), *Plant Species Biology*: Izu Island breeding-system differences.
- Smithson A & Macnair MR (1996, 1997): bumblebee flower constancy and frequency dependence in experimental systems.
