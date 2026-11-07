# Current Bombus inference for the manuscript

## Biological leverage and a separate flower-constancy hypothesis

The value of `Campanula punctata` as a study system is not that a two-species SDM necessarily explains pigmentation. In studied mainland populations, bumblebees are the predominant recorded pollinators; experimental work has demonstrated that `B. diversus` pollination can favor a wider corolla through more efficient pollen transfer (Kobayashi et al. 1997), and pollinator species/size varies geographically with corolla size (Nagano et al. 2014). **Those studies test floral morphology and pollination, not colour preference.** On several Izu Islands halictid bees replace bumblebees as predominant pollinators; the broad statement "only bumblebees pollinate this species" is therefore incorrect (Inoue & Amano 1986; Inoue 1988).

Two **distinct, noninterchangeable mechanisms** follow:

- **H1: directional habitat-support correspondence (already analysed).** At sharp, nearby white/pigmented transitions, predicted relative habitat support for the broadly distributed documented visitors `B. ardens` and `B. diversus` might be higher on the pigmented side. This is the fixed, weak, scale- and calibration-sensitive 67-pair Main result described below; it does not measure total functional pollinator availability.
- **H2: positive frequency-dependent selection through bumblebee flower constancy (not tested).** If foraging bumblebees learn and repeatedly visit locally common, distinguishable flower-colour morphs, the rare morph may receive lower *per-flower* effective pollen transfer. With sufficient relative fitness consequences, locally common states could become more common, potentially resulting in nearby predominantly white and predominantly pigmented patches even under similar broad abiotic conditions. Unlike H1, **H2 is symmetric**: it could favor whichever colour is locally common, not inherently the pigmented morph. Bumblebee common-colour preference and same-colour sequential visits have been experimentally demonstrated in other systems (Smithson & Macnair 1996; Smithson & Macnair 1997), but **not measured for white/pigmented `C. punctata` in this dataset**.

Existing 5-km pairs were selected by observed sharp colour transitions, without an environmental caliper. Their broadly similar measured environments are a **descriptive balance diagnostic**, not proof of abiotic equivalence. Abrupt colour boundaries are *compatible* with H2 but do not distinguish it from fine-scale habitat differences, population genetic structure, founding effects, restricted dispersal or human translocation. The absence of within-site white/pigmented co-occurrence in the sampling stream does not estimate local morph-frequency-dependent fitness; a 1-km "pure" observed cell is not proof of monomorphism in the underlying population.

**Inferential rule:** Do not reinterpret the 67-pair SDM sign-flip P as a test of flower constancy, frequency dependence, preference, assortative mating or fitness. This is a *mechanistic hypothesis suggested by boundary geometry*, not a new positive statistical result. It complements, rather than rescues, the weak H1 result.

**Decisive next test:** At naturally mixed sites, where available, or in local white/pigmented reciprocal arrays, estimate morph-specific per-flower visitation and effective pollen deposition across predefined local morph frequencies; track same-colour sequential visits after accounting for offered flower frequencies, and measure seed set/outcrossing. Record bee identity, floral abundance, reward, UV/visible reflectance, microclimate and population genetic context. Positive frequency dependence requires a frequency-dependent **per-flower reproductive advantage**, not merely more total visits to the numerically common colour.

Manuscript-ready paragraphs and a figure-boundary note: [Bombus constancy and local pigmentation boundaries](bombus-constancy-frequency-dependence.md).

References: [Inoue & Amano (1986)](https://doi.org/10.1111/j.1442-1984.1986.tb00018.x); [Inoue (1988)](https://doi.org/10.1111/j.1442-1984.1988.tb00178.x); [Kobayashi et al. (1997)](https://doi.org/10.1007/s004420050268); [Nagano et al. (2014)](https://doi.org/10.1002/ece3.1191); [Smithson & Macnair (1996)](https://doi.org/10.1046/j.1420-9101.1996.9050571.x); [Smithson & Macnair (1997)](https://doi.org/10.1006/bijl.1996.0104).

## Stable inferential labels

**2 species = directional local availability hypothesis.**  
**5 species = supplementary community/biogeographic turnover.**

The two-species label refers to *Bombus ardens* and *B. diversus*. The five-species supplementary analysis additionally includes *B. beaticola*, *B. consobrinus* and *B. honshuensis*.

## Main question and design

After the national environment+space analysis establishes the broad flower-colour template, the pollinator question changes scale. Bombus SDMs are environmentally structured, so a national flower-colour/Bombus map association alone would not identify a biotic mechanism. The Main test asks whether predicted habitat opportunity for the documented broad focal bumblebees increases from the white side to the pigmented side of abrupt nearby colour boundaries.

Primary design: 1-km flower cells; up to five nearest neighbours; 5-km radius; pure observed white-pigmented transitions; greedy non-overlap; pair selection without Bombus values or transition direction; orientation only after pair selection; mean pigmented-minus-white occurrence-referenced max(*B. ardens*, *B. diversus*) support; 100,000 sign flips.

Raw SDM values are not treated as abundance, visitation or pollination pressure. Occurrence-referenced support is a within-species relative habitat-support scale, not occurrence probability, abundance, visit rate, pollen transfer, pollination effectiveness or selection pressure.

## Final-eight-axis environmental balance diagnostic

Environment is used only as a descriptive balance diagnostic, never to select, orient or weight the fixed transition pairs. In the finalized eight standardized Broad abiotic axes, median RMS distance is **0.244 versus 0.318** for selected versus all eligible local edges at 5 km. The same ordering holds at 10 km (0.337 vs 0.429) and 25 km (0.435 vs 0.531).

Audit evidence: workflow `31538548679`, artifact `9119773035`, SHA-256 `51fc846d2f3d815d8bbf3c0b95647eabeb0acf731f1e6dd164c11a0dfe1b896f`.

## Main result and claim ceiling

Primary 5-km pure-transition result: 67 pairs; median separation 2.0 km; mean contrast +0.0359; median -0.00277; 49.3% positive; one-sided P=0.0272; BH q across 5/10/25-km pure tests=0.0815. Raw-cloglog support does not reproduce the result (P=0.267), and the mean attenuates strongly at 10 and 25 km.

The manuscript therefore describes **weak, highly local, magnitude-driven consistency** with the directional local availability hypothesis. It is not evidence of pollinator-mediated selection. No persuasive Bombus relationship is retained for conditional intensity among already pigmented flowers; if the weak local signal is biological, it is more consistent with maintenance/loss of a pigment state than progressive darkening.

## Five-species and montane guardrails

Five-species Hellinger turnover is supplementary because it is unsigned. Under occurrence-referenced matching, matched excess is +0.0330 at 5 km (P=0.0628), +0.0327 at 10 km (P=0.0142) and +0.0411 at 25 km (P=0.00010). These associations locate colour boundaries within pollinator-biogeographic transition zones but cannot determine which side should be pigmented and do not strengthen the signed Main result into a causal mechanism.

Montane/alpine national overlap is a negative guardrail. Among near-equal-elevation transitions (<=50 m), mean montane contrasts are near zero or negative and all one-sided P values are >=0.755. The appropriate interpretation is shared elevational geography rather than an additional pollinator mechanism.

## Causal ceiling and next tests

All Bombus surfaces are environment-based SDM predictions. The study does not measure realized visitation, abundance, flower choice, pollen deposition, seed set or fitness. Direct tests require species-resolved visitation, floral contact, pollen transfer, fitness, visible/UV spectra, receptor-based contrasts and local abiotic measurements.
