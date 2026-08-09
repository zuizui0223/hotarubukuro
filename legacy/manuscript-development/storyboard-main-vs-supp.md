# Main story / Supporting Information storyboard

## Core editorial rule

The Main text is not an inventory of everything that was analysed. It is the shortest sequence of evidence needed for the reader to understand **why the next ecological question becomes necessary**.

A result belongs in the Main only if it does at least one of two things:

1. **pays off a question planted earlier**, or
2. **creates the question that drives the next section**.

Everything that establishes robustness, alternative estimands, negative controls, data-source details, or method-development provenance belongs in Supporting Information unless the reader cannot understand the next scene without it.

In internal terms, the paper should feel like a film with foreshadowing and payoff. The manuscript itself should remain scientific and restrained; the cinematic logic is an editorial device, not prose style.

---

# MAIN = the film

## Prologue — How can a national flower-colour trait even be measured?

### Question planted
Range-wide flower-colour studies often have either broad but categorical morph data or quantitative colour measurements from relatively few populations. Can a recent, spatially explicit quantitative trait dataset be built at national scale?

### Main evidence
- YAMAP is introduced as a hiking/recreation image stream rather than a purpose-built biodiversity database.
- 2023–2025 provides a short, contemporary, GPS-linked mountain-photo frame.
- Every recovered candidate is author-screened before phenotyping.
- Images are converted reproducibly to a two-part phenotype: white/pigmented state + pigmented-only intensity.
- One sentence only: a matched Supplementary benchmark showed that this YAMAP stream supplied several-fold more focal-species image records than the same-period iNaturalist comparator.

### Foreshadowing
1. The **mountain-route sampling frame** is useful for natural trait geography, but it may later weaken a broad human-gradient test.
2. Separating **pigmentation state from intensity** creates the possibility that different ecological processes act on the two components.

### Keep out of Main
- full YAMAP/iNaturalist/GBIF table;
- provider overlap details;
- annual CV calculations;
- positional-accuracy comparisons;
- all image-QC fields;
- all mixture-model alternatives.

Those are Supporting Information.

---

## Act I — What is the broad geographical template?

### Question planted
Once the phenotype is measurable, what environmental and spatial structure organizes it across Japan?

### Main evidence
- national environment + INLA-SPDE models;
- key environmental axes only;
- spatial ranges / blocked predictive performance;
- state and conditional intensity shown separately.

### Payoff of the Prologue
The two-part phenotype matters: broad geography is not identical for whether pigmentation is expressed and how intense already-pigmented flowers become.

### New tension / foreshadowing
A strong broad geographical template also creates an inferential trap: any national Bombus SDM will share climate and space with the flower-colour map. A visually convincing national overlap cannot automatically be read as pollinator selection.

This sentence should naturally motivate the scale change rather than introducing Bombus as another predictor family.

### Keep out of Main
- every environmental coefficient;
- full PCA loadings;
- mesh/prior diagnostics;
- all blocked-fold metrics;
- variance-style decompositions not central to the narrative.

---

## Act II — Zoom in: do sharp local colour boundaries follow focal pollinator availability?

### Question planted by Act I
If broad map overlap is ambiguous, can the pollinator question be asked at the local scale where selection should act?

### Main design beat
Do not add Bombus to the national regression. Change the comparison unit.

- identify abrupt nearby white/pigmented boundaries without Bombus information;
- use the documented broad focal pollinators *B. ardens* + *B. diversus*;
- orient pairs only after transition selection;
- ask whether the predicted availability contrast points from white toward pigmented.

### Main result
Report the focal 5-km result and its claim ceiling in one compact paragraph:
- mean contrast in hypothesis direction;
- but median/sign proportion/raw-SDM/broader scales show fragility;
- therefore weak local consistency, not demonstrated selection.

### Payoff 1 — two-part phenotype
The local Bombus signal concerns **pigmentation state**, not progressively darker colour among pigmented flowers. This pays off the decision made in the Prologue to separate state and intensity.

### Payoff 2 — why only two Bombus species?
One Main-text sentence is enough:

> apparent montane/alpine Bombus–pigmentation correspondence disappeared in near-equal-elevation comparisons, indicating shared high-elevation geography rather than an additional pollinator effect.

This resolves the potential objection that all five species should simply be pooled. Details stay in Supplement.

### Keep out of Main
- five-species community turnover;
- 25/50/100-km matching grids;
- all-five max/mean exposures;
- montane equal-elevation tables;
- species-by-species sensitivity results;
- historical environment+SPDE Bombus null analyses.

These are the **extra episodes**: important for trust and interpretation, but not needed to carry the directional mechanism story.

---

## Act III — What remains locally discordant with the broad natural template?

### Question planted
Even after the broad natural geography is well described, some local configurations look ecologically discordant. How should these be identified without declaring large residuals to be biological anomalies?

### Main design beat
Change the inferential object again:

`raw residual` -> `repeatable ecological event`

Define a pigmented cell embedded among geographically close, environmentally similar white neighbours, then replay the same event detector on repeated natural predictive maps.

### Main result
- 17 reproducibly defined locations;
- their overall frequency is not robustly above the natural predictive reference;
- therefore they are not "unexplained anthropogenic anomalies".

### Foreshadowing payoff from Prologue
The mountain-focused YAMAP frame now matters in the opposite direction: it is useful for natural mountain geography but can compress the human-modification gradient.

### Final species-specific question
Because *C. punctata* is cultivated ornamentally, ask only after candidates are fixed whether their human context is distinctive.

### Ending
Population/DID signals are suggestive but familywise inconclusive. The paper ends not with a forced causal answer, but with a sharply defined set of field/provenance targets and explicit next tests.

This ending should feel like the mystery has been narrowed rather than artificially solved.

### Keep out of Main
- all anomaly threshold grids;
- full 10,000/200,000-map calibration tables;
- all human variables/scales;
- all maxT families;
- individual candidate diagnostics.

---

# SUPPLEMENT = the extra episodes / director's evidence

Supporting Information should answer the reader's "but what if...?" questions without interrupting the Main narrative.

## S1 Data-source and phenotype backstage
- matched YAMAP vs iNaturalist/GBIF volume table;
- annual counts and provider overlap;
- observation-process / quality matrix;
- source-row audit;
- author-screening/QC details;
- colour extraction and mixture diagnostics.

## S2 Broad-model robustness
- full environmental coefficients;
- PCA loadings;
- INLA mesh/prior/hyperparameter details;
- complete predictive metrics and calibration.

## S3 Bombus SDMs and availability sensitivities
- SDM construction/AUC/calibration;
- occurrence-reference details;
- all scales / thresholds / raw support / all-five alternatives.

## S4 Pollinator biogeography side story
- five-species community turnover;
- spatial/elevational matching;
- spatial-block replication;
- interpret only as boundary correspondence.

## S5 High-elevation guardrail
- *B. beaticola*, *B. consobrinus*, *B. honshuensis*;
- <=50 m / <=100 m equal-elevation comparisons;
- explicit negative conclusion: no additional montane effect beyond shared elevational geography.

## S6 Anomaly and human-context robustness
- event-definition sensitivity;
- cross-fit and joint posterior-predictive reference details;
- complete human-context/maxT families;
- candidate-level tables.

## S7 Historical development
- superseded gates and old national Bombus models;
- retained only for provenance.

---

# Foreshadowing -> payoff map

| Early clue | Later payoff |
|---|---|
| YAMAP is mountain-route biased | useful for natural geography; later explains why anthropogenic gradient may be compressed |
| YAMAP is not a biodiversity-reporting app | supports a complementary observation process; author screening becomes essential before trait inference |
| state and intensity are separated | broad ecology differs; local Bombus result concerns state but not intensity |
| broad environment + space explains strong geography | motivates refusing a national Bombus map-overlap interpretation |
| Bombus SDMs are environmentally generated | motivates changing scale instead of adding another national coefficient |
| montane Bombus overlap pigmented highlands | equal-elevation guardrail reveals shared high-elevation geography, explaining why they are not in the primary availability metric |
| ornamental use is mentioned without claiming escape | only after natural candidates are fixed does human context become a legitimate follow-up |
| anomalies are defined as ecological events, not residual tails | repeated natural maps show that 17 locations are targets rather than proof of an extra process |

---

# Compression rule for the final manuscript

For each Main section, retain only:

1. one ecological question;
2. one design choice that makes the question interpretable;
3. one principal result;
4. one claim ceiling;
5. one sentence that opens the next question.

If a paragraph does not perform one of those five jobs, it should normally move to Supporting Information.

The intended reader experience is:

**"So that is why they used this data source" -> "so that is why colour had to be split in two" -> "so that is why space had to be modelled" -> "so that is why Bombus could not be put in the same national model" -> "so that is why the analysis zoomed to local boundaries" -> "so that is why high-alpine species were a confounding guardrail rather than stronger evidence" -> "so that is why residuals were not enough" -> "so that is why human context is the final, cautious follow-up."**

That sequence is the paper's narrative payoff.
