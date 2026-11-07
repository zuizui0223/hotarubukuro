# Current Bombus inference for the manuscript — SDM-only scope

**Current rule (2026-11-07):** Use observed flower-colour state as the response and focal-*Bombus* SDM support as the **only explanatory variable** in the Bombus component. No climate, elevation, soil, radiation, other abiotic predictors, environment-distance matching, or additional environment/spatial adjustment enters this analysis. The 5/10/25-km distances define local flower-colour pair selection only; they are not predictors in the test.

## Why Bombus is a biologically motivated candidate

In studied mainland populations of *Campanula punctata* sensu lato, bumblebees are major recorded effective flower visitors. Experimental work on *B. diversus* demonstrates that pollen-transfer efficiency can favor corolla morphology (Kobayashi et al. 1997); Nagano et al. (2014) report changes in bumblebee pollinator assemblages with altitude. **These are evidence of bumblebee pollination and morphological selection, not proof of a preference for white versus pigmented flowers.** In some Izu Island populations, halictid bees replace bumblebees as main visitors (Inoue & Amano 1986; Inoue 1988), so the universal statement "only bumblebees pollinate this plant" is not defensible.

The five taxa included in the retained SDM family are *B. ardens*, *B. diversus*, *B. beaticola*, *B. consobrinus*, and *B. honshuensis*; the field-visitor list also included *B. ussurensis*, which was excluded before flower-colour analysis. The **fixed primary focal predictor** is occurrence-referenced `max(A_ardens, A_diversus)`. The other SDMs represent supplementary community-biogeographic context, not an additional environmental correction.

## Focal species selection versus the wider effective visitor guild (audit 2026-11-07)

**Decision: retain the two widespread documented focal species for the current directional Main test, but never call this an index of total bumblebee or effective-pollinator availability.** The estimand is strictly the occurrence-referenced predicted suitability of *B. ardens* and *B. diversus*. It is not the absence/presence or abundance of all *Bombus* species.

Ecological boundary: Nagano et al. (2014, *Ecology and Evolution*, DOI 10.1002/ece3.1191) observed 1,140 *Bombus* visits to *C. punctata* along a central-Japanese altitude gradient. *B. ardens* and *B. diversus* were prominent at low elevations (700–1,000 m), while *B. beaticola* almost exclusively visited the studied high-elevation flowers (1,900–2,300 m). *B. consobrinus* was exceptionally prominent at one intermediate site; *B. honshuensis* and *B. ussurensis* were also observed at low frequencies. Thus low predicted suitability for the two chosen species **does not imply no bumblebee visitors**, especially in the mountains.

**Data availability audit:** the original frozen GitHub Actions output (run `32233065078`, artifact `9358305657`) contains `bombus_occurrence_reference_support/cell_effective_bombus_support.csv` for 1,305 flower cells. Both focal two-species occurrence-referenced SDM columns have 1,305 nonmissing values, but the three auxiliary columns `beaticola_occurrence_reference`, `consobrinus_occurrence_reference`, and `honshuensis_occurrence_reference`, as well as `all5_occmax`, have **zero** nonmissing values. `occurrence_reference_calibration.csv` has **two** species rows, not five. This is the archived two-species-support artifact, **not evidence that the three other bees are absent**. The source-build code permits auxiliary models to be unavailable while requiring the focal two. Any future five-species analysis must first construct and validate all five actual predictions; replacing missing values with zeros would be scientifically incorrect.

Keep the current two-species Main analysis unchanged, with the limitation in Discussion; do **not** describe `max(A_ardens,A_diversus)` as overall `Bombus availability`, `pollinator absence`, or the environmental source of floral pigmentation. The geographically variable visitor guild, including high-elevation *B. beaticola*, remains an explicit limitation of taxon coverage. It does not by itself justify selecting additional species after observing the colour results.

## What is tested

**H1 — directional SDM correspondence (tested).** In pre-selected sharp neighbouring 1-km white/pigmented colour-cell contrasts, is the focal two-species support higher on the pigmented side? The 5-km primary analysis selects observed pure transitions without using any Bombus SDM score or sign, applies greedy non-overlap, then computes `SDM_pigmented - SDM_white`. The inferential statistic is its mean, assessed with 100,000 directional sign flips. This is a comparison of **relative predicted habitat suitability**, not abundance, presence, visitation, attractiveness, effective pollen transfer or selection pressure. Ten- and 25-km windows and raw-cloglog values retain the pre-existing interpretation checks.

**H2 — positive frequency-dependent pollination via flower constancy (not tested by SDM).** Bumblebee colour learning, sequential within-colour visits, and the higher reproductive success of the *locally common* morph could maintain locally different colour states. This hypothesis does **not** require the pigmented side to have higher SDM values. Testing H2 requires local morph frequencies, individual-bee visit sequences and per-flower pollen-transfer/reproductive success; the current photographs and habitat suitability surfaces do not measure those quantities.

## Executed independent SDM-only reanalysis (2026-11-07)

Public frozen cell, SDM and preselected-pair tables were downloaded and re-analysed without environmental predictors. The independent script is `scripts/check_bombus_sdm_only.py` and its evidence record is [SDM-only executed results](bombus_sdm_only_validation_2026-11-07.md).

- Nationwide exploratory descriptive association, 1,305 1-km cells (1,922 photographed flowers): Spearman rho of *observed cell pigmented share* against focal occurrence-referenced SDM = **-0.3056**; raw-cloglog alternative = **-0.3001**. These are spatially structured associations, **not** causal estimates or spatially independent P-values.
- Fixed 5-km sharp transition result: **67** pairs; occurrence-referenced mean pigmented-minus-white SDM = **+0.03590**, median **-0.00277**, **33/67** in the hypothesised direction; original fixed one-sided `P=0.02716`, three-distance `BH q=0.08148`. The raw-cloglog 5-km exposure is not supported (`P=0.26715`).
- The occurrence-referenced mean declines at 10 km (**+0.00840**, fixed `P=0.32454`, 109 pairs) and 25 km (**+0.00292**, fixed `P=0.43575`, 171 pairs).
- **66/67** selected sharp 5-km pairs have only one image on at least one endpoint. The pairs are observed photo-colour contrasts, **not** demonstrations that two reproductively stable monomorphic populations meet.
- A nationally negative descriptive association and a weakly positive *mean* at a subset of local transitions are compatible with differences in comparison unit and geographical structure. They must not be called a proven ecological sign reversal or a pollinator effect.

The original final calculation and recorded P/q values are unchanged; independent sign-flip recalculation uses another random-number implementation and therefore yields slightly different Monte Carlo values.

## Interpretation boundary — put environment confounding in Discussion, not in the model

The focal SDMs were generated **from environmental predictors**. Therefore any SDM–colour association could reflect shared environmental and geographic gradients rather than pollinator-mediated selection. **This analysis intentionally does not separate those effects and does not claim to do so.** Nearby cells also need not have identical microhabitats or population histories. No independent selection inference, and no support for frequency dependence, follows from the SDM-only association.

For manuscript English, a concise, technically accurate statement is:

> We related flower-colour state to predicted habitat suitability for the two documented widespread bumblebee visitors without adding environmental covariates to the pollinator analysis. Because these suitability surfaces were themselves derived from environmental variables, the observed associations cannot distinguish pollinator effects from shared environmental geography. Bumblebee flower constancy remains a plausible mechanism for local colour-state reinforcement, but our data do not measure frequency-dependent pollination or fitness.

## Reproducibility and manuscript placement

- Main: primary local 5-km SDM-only contrast with mean, median, number positive, original P/q, and explicit limitations.
- Supporting: 10/25-km scale and raw-SDM checks; supplementary five-species community context. Historical environmental-balance or elevation-matching diagnostics are **not used to establish an independent Bombus effect**.
- Separate broad analysis may contain environmental predictors, but **those predictors are never carried into this Bombus test**.

Manuscript-ready paragraphs: [Bumblebee constancy and flower-colour boundaries](bombus-constancy-frequency-dependence.md).

References: Inoue & Amano (1986); Inoue (1988); Kobayashi et al. (1997); Nagano et al. (2014); Smithson & Macnair (1996, 1997). The last two support flower-constancy hypotheses from other systems, not empirical constancy in this species.
