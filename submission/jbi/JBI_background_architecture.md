# Introduction plan — one mystery, four dependent reveals

This file is the editorial lock for the Introduction and Discussion. The paper must read as one investigation in which each solution exposes the next problem, not as four analyses placed side by side.

## The single mystery

> Why do white and pigmented flowers remain geographically structured within one species, why does colour intensity vary again within pigmented flowers, and how can environment, spatial history, pollinators and human context be distinguished when they share geography?

The reader should enter with one biological question and leave with four increasingly precise causal tests.

## Act 1 — The phenotype could not be studied at the required resolution

### The practical gap

Large-scale flower-colour geography is much easier to assemble as named morphs or categorical states than as dense, georeferenced measurements of variation within a colour state. Do not claim that quantitative flower-colour studies do not exist. The defensible gap is narrower and stronger:

> no ready-made database supplied enough recent, georeferenced focal-species images to resolve both the white–pigmented transition and continuous variation among pigmented flowers across Japan.

Occurrence coordinates alone would not solve that problem. The missing resource was a large image stream from which the trait itself could be reconstructed.

### Why YAMAP is part of the discovery

YAMAP is not introduced as a weaker substitute for a biodiversity database. It is an alternative observation process: hiking and activity records incidentally contain route-linked flower photographs that were not created primarily as focal-taxon reports. For the matched 2023–2025 window, the study recovered 1,964 georeferenced focal-species YAMAP rows versus 516 iNaturalist observations with photographs.

The mountain-route frame has a dual role that must remain explicit:

- for natural flower-colour geography, it enriches mountain and semi-natural settings in which self-sustaining populations are plausible;
- it never proves wild provenance for an individual record;
- for the later human-context test, the same frame can compress the represented urban–rural gradient and reduce power for a broad anthropogenic contrast, while route access can also increase both observation opportunity and human exposure.

The reveal is therefore not merely “more photographs”. Author screening and deterministic phenotyping turn an incidental image stream into two responses:

- pigmentation state — whether visible pigmentation is expressed;
- conditional intensity — how strong the visible red–green phenotype is after pigmentation is present.

Solving measurement changes the biological question.

## Act 2 — A dense map exposes spatial attribution rather than solving it

A national trait map makes causal attribution harder because environment, topography, population history, dispersal, sampling geography and environmentally built Bombus SDMs can all be spatially structured.

The Broad stage therefore has two linked jobs:

1. estimate response-specific environmental associations with INLA-SPDE while retaining coherent continuous geography rather than treating space as independent residual noise;
2. ask whether environmentally dissimilar held-out locations differ phenotypically more than a cross-fitted space-only continuity expectation predicts at comparable geographical separation.

The FST–PST analogy is conceptual only. The space-only SPDE is an empirical FST-like continuity expectation, not neutral genetic differentiation, and observed phenotype divergence is not PST. Never write `PST > FST`, “selection exceeds drift”, or “local adaptation is demonstrated”.

### Result-direction lock

The current result is the opposite of an “intensity tracks environment beyond space” story:

- pigmentation state: observed high-environmental-distance minus low-environmental-distance divergence = 0.106802; space-null median = 0.058240; excess = +0.048562; one-sided posterior-predictive P = 0.03393;
- conditional intensity: observed contrast = -0.047179; space-null median = -0.001287; excess = -0.045891; P = 0.87226.

The manuscript-facing conclusion is:

> Pigmentation-state divergence exceeds a cross-fitted spatial expectation along environmental difference, whereas conditional intensity does not.

This closes the loop opened in Act 1. Quantifying the phenotype did not merely add precision; it revealed that the environmentally ordered white–pigmented switch and variation within the pigmented state are different ecological layers.

## Act 3 — The pollinator hypothesis becomes testable only after changing scale

A national regression of flower colour on Bombus SDM support would not isolate a pollinator mechanism because both maps inherit climate and mountain geography. The solution is not to add more national covariates. It is to change the comparison unit.

### Why the focal system is unusually tractable

The large tubular flowers of *Campanula punctata* constrain the plausible effective-pollinator set, and bumblebees are documented effective visitors in the system. The Izu-island Bombus-absence and breeding-system literature supplies a species-specific natural-history motivation for asking whether local bumblebee opportunity changes the reproductive value of maintaining a visible pigment signal. It does not by itself demonstrate colour selection, and the white-flowered island form must not be presented as causal evidence.

### Why nearby colour boundaries are the primary test

The primary units are 67 independently fixed, non-overlapping white–pigmented transitions within 5 km. These are replicated local neighbourhood contrasts, not one continuous transect and not an exact estimate of bumblebee foraging distance.

At this scale:

- geographic separation is narrow;
- selected pairs occupy tighter environmental neighbourhoods than ordinary local edges;
- a change in focal-Bombus habitat opportunity can be read in the biologically predicted white-to-pigmented direction;
- the scale is compatible with local variation in visitor assemblages and floral selection while still retaining national replication.

Use *Bombus ardens* and *B. diversus* as the broad-ranging focal opportunity signal. The three montane/alpine species are not discarded; they perform a different inferential job. Their attractive national overlap with pigmented high-elevation flowers disappears in near-equal-elevation comparisons, showing that shared mountain geography can masquerade as a pollinator mechanism.

The local result is heterogeneous and state-specific. It supports, at most, local maintenance or loss of a pigmented state in a subset of neighbourhoods. It does not show that Bombus progressively darkens flowers, and SDM support is habitat opportunity rather than visitation, pollen transfer, fitness or realized selection.

## Act 4 — Apparent exceptions must survive the natural model before human context is read

The local question now changes again. Act 3 studies a sharp transition between neighbouring colour states. Act 4 studies a different configuration: a pigmented cell isolated among environmentally similar white neighbours. Do not merge these into one generic “hotspot” analysis.

A large residual is not a biological anomaly. The relational event is therefore fixed without human variables and replayed on 10,000 cross-fitted natural predictive maps. This asks whether the observed configuration recurs more often than the finalized natural geography itself can generate.

The observed 16 events are compatible with the natural reference. That null result is part of the design achievement: it prevents visual surprise from becoming a causal claim while preserving 16 reproducible field and provenance targets.

Only after those sites are fixed is human context examined. The horticultural use of *C. punctata* makes planting, escape, introgression or repeated propagule contact plausible hypotheses, but current data do not establish any of them. Population exposure within 5 km is the leading feature, with global maxT FWER P = 0.05479. The correct interpretation is a short-range human-context clue that remains compatible with access bias, managed microenvironment and natural local variation.

The YAMAP sampling frame returns here as a deliberate final callback: mountain-route enrichment helped establish the natural template, but the same enrichment can narrow the represented anthropogenic gradient. This can be discussed as a power and sampling-frame limitation, not used to turn a near-threshold result into stronger evidence.

## Locked causal chain

The Introduction and Discussion should preserve this irreversible sequence:

`categorical/sparse trait evidence -> incidental mountain-image stream -> two-part quantitative phenotype -> broad environment + continuous space -> state-specific environmental excess beyond spatial continuity -> replicated local Bombus boundaries -> mountain-confounding guardrail -> naturally calibrated isolate events -> post-selection human-context hypothesis`

Each handoff answers one question and creates the next:

1. We could not explain a national quantitative trait that had not been measured nationally.
2. Once measured, shared geography made broad causal attribution unreliable.
3. Once broad environment and space were established, the Bombus question had to move to replicated local boundaries.
4. Once the natural template existed, visually unusual local states could be calibrated before human context was inspected.

## Claims that must not enter the paper

- YAMAP records are all wild populations.
- YAMAP is globally less biased or higher quality than iNaturalist.
- The SPDE field is population history, genetic structure or dispersal distance.
- The spatial-null test is an actual FST–PST or QST–FST test.
- Environmental alignment proves selection or local adaptation.
- Bombus SDM support is visitation, pollination or selection.
- The Izu natural-history contrast proves bumblebees caused white flowers.
- The 16 departures are anthropogenic anomalies, planted populations or escaped cultivars.
- P = 0.05479 is corrected statistical support for a human mechanism.

## Required reader movement

1. “The existing evidence could not resolve the trait at national quantitative scale.”
2. “The hiking-image stream makes the hidden phenotype visible.”
3. “The phenotype is not one white-to-dark axis.”
4. “The national map exposes spatial confounding.”
5. “Environment orders the state transition beyond spatial continuity, but not within-pigmented intensity.”
6. “The pollinator hypothesis therefore has to be tested at replicated local boundaries.”
7. “The strongest-looking highland overlap is actually a mountain-confounding warning.”
8. “Even striking local isolates must first survive a natural predictive null.”
9. “Human context ends as a targeted provenance hypothesis, not a declared cause.”

## Plain-English rule

Use concrete causal verbs and make each paragraph complete one transition.

Prefer:

> Making the national pattern visible did not solve the causal problem; it exposed it.

over:

> The resulting enhanced phenotype resolution necessitated an additional scale-aware attribution framework.

Methods may be sophisticated. The prose should make their biological necessity feel unavoidable.