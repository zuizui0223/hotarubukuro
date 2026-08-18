# Introduction plan — one mystery, four dependent reveals

This file is the editorial lock for the Introduction and Discussion. The paper must read as one investigation in which each solution exposes the next problem, not as four analyses placed side by side.

## The single mystery

> Why do white and pigmented flowers remain geographically structured within one species, why does colour intensity vary again within pigmented flowers, and how can environment, spatial history, pollinators and human context be distinguished when they share geography?

The reader should enter with one biological question and leave with increasingly precise, experimentally testable alternatives. The goal is not to name one cause from maps, but to convert one trait map into distinct mechanistic questions at defensible scales.

## Act 1 — The phenotype could not be studied at the required resolution

### The practical gap

Large-scale flower-colour geography is much easier to assemble as named morphs or categorical states than as dense, georeferenced measurements of variation within a colour state. Do not claim that quantitative flower-colour studies do not exist. The defensible gap is narrower and stronger:

> no ready-made database supplied enough recent, georeferenced focal-species images to resolve both the white–pigmented transition and continuous variation among pigmented flowers across Japan.

Occurrence coordinates alone would not solve that problem. The missing resource was a large image stream from which the trait itself could be reconstructed.

### Why YAMAP is part of the discovery

YAMAP is not introduced as a weaker substitute for a biodiversity database. It is an alternative observation process: hiking and activity records incidentally contain route-linked flower photographs that were not created primarily as focal-taxon reports. For the matched 2023–2025 window, the study recovered 1,964 georeferenced focal-species YAMAP rows versus 516 iNaturalist observations with photographs.

The mountain-route frame has a dual and potentially bidirectional role:

- for natural flower-colour geography, it enriches mountain and semi-natural settings in which self-sustaining populations are plausible;
- it never proves wild provenance for an individual record;
- for the later human-context test, it can compress the represented urban–rural gradient and reduce contrast;
- simultaneously, trailheads, roads and accessible mountain margins can increase observation opportunity, disturbance and human-mediated movement.

Do not write that route sampling can only weaken the human signal. Its net direction is unresolved.

The reveal is therefore not merely “more photographs”. Author screening and deterministic phenotyping turn an incidental image stream into two responses:

- pigmentation state — whether visible pigmentation is expressed;
- conditional intensity — how strong the visible red–green phenotype is after pigmentation is present.

### Conditional-response ceiling

Intensity is observed only after a flower crosses the pigmentation boundary. This is biologically useful because white flowers should not be treated as merely low-intensity pigmented flowers, but it creates a conditional subset. If measured or unmeasured factors influence both state and intensity, conditioning on state can create or mask associations in the intensity model. The two parts are distinct observational questions, not automatically independent regulatory pathways.

Solving measurement changes the biological question.

## Act 2 — A dense map exposes physiological and spatial attribution rather than solving it

A national trait map makes causal attribution harder because environment, topography, population history, dispersal, sampling geography and environmentally built Bombus SDMs can all be spatially structured.

The Broad stage therefore has two linked jobs:

1. estimate response-specific environmental associations with INLA-SPDE while retaining coherent continuous geography rather than treating space as independent residual noise;
2. ask whether environmentally dissimilar held-out locations differ phenotypically more than a cross-fitted space-only continuity expectation predicts at comparable geographical separation.

### Ecological interpretation lock

The Broad result must be interpreted through competing benefits and costs of anthocyanin, not through P values alone and not through one universal “stress makes flowers darker” rule.

- **Cool-climate state association:** compatible with temperature-responsive anthocyanin regulation or environment-aligned population differentiation; photographs cannot distinguish plasticity from adaptation.
- **Temperature × seasonality for conditional intensity:** the warm-climate decline strengthens with thermal seasonality. Darker petals may gain thermal benefit in cool settings yet incur radiative, cooling or hydraulic costs in warm settings. This is a mechanistic hypothesis, not a direct flower-temperature result.
- **Lower intensity toward wetter climate:** compatible with stress-related pigment benefit at the drier end in some systems, but precipitation is not flower water status, VPD or proof of drought adaptation.
- **Lower intensity on rugged terrain:** rejects a generic “greater terrain stress means darker” account. Relief can mix aspect, shade, drainage, exposure, cold-air pooling and history.
- **Null final RSDS effect and non-promoted dryness × radiation sensitivity:** do not claim a resolved national radiation or UV mechanism.
- **Residual spatial fields:** sampling guides for ancestry, isolation, dispersal and unmeasured microclimate; not mechanism labels or dispersal distances.

The FST–PST analogy is conceptual only. The space-only SPDE is an empirical FST-like continuity expectation, not neutral genetic differentiation, and observed phenotype divergence is not PST. Never write `PST > FST`, “selection exceeds drift”, or “local adaptation is demonstrated”.

### Result-direction lock

The current result is the opposite of an “intensity tracks environment beyond space” story:

- pigmentation state: observed high-environmental-distance minus low-environmental-distance divergence = 0.106802; space-null median = 0.058240; excess = +0.048562; one-sided posterior-predictive P = 0.03393;
- conditional intensity: observed contrast = -0.047179; space-null median = -0.001287; excess = -0.045891; P = 0.87226.

The manuscript-facing conclusion is:

> Pigmentation-state divergence exceeds a cross-fitted spatial expectation along environmental difference, whereas conditional intensity does not.

This closes the loop opened in Act 1. Quantifying the phenotype did not merely add precision; it revealed that the environmentally ordered white–pigmented switch and variation within the pigmented state are different ecological layers. The conditional-intensity ceiling remains: absence of a spatial-null excess within the pigmented subset is not proof that environment has no role in pigment amount.

## Act 3 — The pollinator hypothesis becomes a heterogeneity question after changing scale

A national regression of flower colour on Bombus SDM support would not isolate a pollinator mechanism because both maps inherit climate and mountain geography. The solution is not to add more national covariates. It is to change the comparison unit.

### Why the focal system is unusually tractable

The large tubular flowers of *Campanula punctata* constrain the plausible effective-pollinator set, and bumblebees are documented effective visitors in the system. The Izu-island Bombus-absence and breeding-system literature supplies a species-specific natural-history motivation for asking whether local bumblebee opportunity changes the reproductive value of maintaining a visible pigment signal. It does not by itself demonstrate colour selection, and the white-flowered island form must not be presented as causal evidence.

### Why nearby colour boundaries are the local test

The units are 67 independently fixed, non-overlapping white–pigmented transitions within 5 km. These are replicated local neighbourhood contrasts, not one continuous transect and not an exact estimate of bumblebee foraging distance.

At this scale:

- geographic separation is narrow;
- selected pairs occupy tighter environmental neighbourhoods than ordinary local edges;
- a change in focal-Bombus habitat opportunity can be read in the biologically predicted white-to-pigmented direction;
- the scale is compatible with local variation in visitor assemblages and floral selection while still retaining national replication.

Use *Bombus ardens* and *B. diversus* as the broad-ranging focal opportunity signal. The three montane/alpine species are not discarded; they perform a different inferential job. Their attractive national overlap with pigmented high-elevation flowers disappears in near-equal-elevation comparisons, showing that shared mountain geography can masquerade as a pollinator mechanism.

### Heterogeneity lock

Do not summarize the result as “Bombus support is higher on pigmented sides” without the distribution.

- mean contrast = +0.03590;
- median = -0.00277;
- positive pairs = 49.3%;
- q across 5/10/25 km = 0.08148;
- mean attenuates at 10 and 25 km;
- raw support does not reproduce the result;
- conditional intensity shows no persuasive correspondence.

The biological result is a subset-driven geographical mosaic, not a pervasive difference. The 67-site programme should compare strongly positive boundaries with near-zero and negative boundaries to ask what local visitor, visual, reproductive, genomic and microenvironmental conditions generate the heterogeneity.

At most, the evidence motivates local maintenance or loss of a visible pigmented state in some neighbourhoods. It does not show that Bombus progressively darkens flowers, and SDM support is habitat opportunity rather than visitation, pollen transfer, fitness or realized selection.

## Act 4 — A different local configuration is selected, naturally calibrated and then tested for human context

The local question now changes again. Do not merge Act 3 and Act 4 into one generic “hotspot” analysis.

### Two local estimands

- **Act 3 is edge based:** a non-overlapping white–pigmented pair is oriented white -> pigmented and receives a signed Bombus contrast.
- **Act 4 is node and neighbourhood based:** a pigmented focal cell is surrounded by at least three environmentally similar cells within 10 km containing no observed pigmented flower.

A location can satisfy one design without satisfying the other. The 67 boundaries and 16 isolates are not alternative labels for the same sites.

### Three-stage event logic

1. **Observed selection:** applying the fixed relational rule to the observed map selects 16 sites. Human variables and residual thresholds do not enter.
2. **Natural-frequency calibration:** the same detector is replayed on 10,000 cross-fitted predictive maps. This asks whether the count and fraction of the event type are unusual under the finalized natural geography.
3. **Post-selection human null:** on every predictive map the detector reselects event cells and the same human contrasts are recalculated. This propagates uncertainty in which cells would be selected under nature.

The 10,000-map replay does **not** identify 16 sites that natural processes cannot reproduce. The observed 16-event count and fraction are compatible with the natural reference (P = 0.27897 and 0.12609). The null result prevents visual surprise from becoming a causal claim while preserving 16 reproducible field and provenance targets.

Only after those observed sites are fixed is human context examined. The horticultural use of *C. punctata* makes planting, escape, introgression or repeated propagule contact plausible hypotheses, but current data do not establish any of them. Population exposure within 5 km is the leading feature, with global maxT FWER P = 0.05479.

The human conclusion is doubly conditional:

- the event type is not more frequent than the natural model predicts;
- its strongest human-context feature does not cross the familywise threshold.

The correct interpretation is a short-range provenance hypothesis that remains compatible with access bias, managed microenvironment, natural local variation and horticultural movement.

## Locked causal chain

The Introduction and Discussion should preserve this irreversible sequence:

`categorical/sparse trait evidence -> incidental mountain-image stream -> two-part quantitative phenotype -> broad physiological hypotheses + continuous space -> state-specific environmental excess beyond spatial continuity -> heterogeneous local Bombus boundaries -> mountain-confounding guardrail -> observed local-isolate selection -> natural-frequency and post-selection replay -> bounded human-context hypothesis`

Each handoff answers one question and creates the next:

1. We could not explain a national quantitative trait that had not been measured nationally.
2. Once measured, shared geography made broad causal attribution unreliable and exposed competing physiological hypotheses.
3. Once Broad environment and space were established, the Bombus question had to move to local boundaries and became a question about heterogeneity.
4. Once the natural template existed, a separate local-isolate event could be selected, calibrated and tested for human context without circularity.

## Claims that must not enter the paper

- YAMAP records are all wild populations.
- YAMAP is globally less biased or higher quality than iNaturalist.
- Route sampling can only weaken a human-context association.
- The SPDE field is population history, genetic structure or dispersal distance.
- The spatial-null test is an actual FST–PST or QST–FST test.
- Environmental alignment proves selection or local adaptation.
- A negative precipitation coefficient proves drought adaptation.
- Ruggedness or radiation supplies one universal stress-darkening mechanism.
- State and conditional intensity are causally independent because they were fitted separately.
- Bombus SDM support is visitation, pollination or selection.
- The positive focal mean is a majority-of-boundaries effect.
- The Izu natural-history contrast proves bumblebees caused white flowers.
- The 16 isolates are natural-process-resistant, anthropogenic anomalies, planted populations or escaped cultivars.
- P = 0.05479 is corrected statistical support for a human mechanism.

## Required reader movement

1. “The existing evidence could not resolve the trait at national quantitative scale.”
2. “The hiking-image stream makes the hidden phenotype visible.”
3. “The phenotype is not one white-to-dark axis, although the two parts remain statistically dependent.”
4. “The national map exposes physiological alternatives and spatial confounding.”
5. “Environment orders the state transition beyond spatial continuity, but not within-pigmented intensity.”
6. “The pollinator hypothesis therefore has to be tested at replicated local boundaries.”
7. “The result is heterogeneous: some boundaries drive the mean and most do not.”
8. “The strongest-looking highland overlap is a mountain-confounding warning.”
9. “A distinct local-isolate rule selects 16 sites, but natural replay reproduces that event frequency.”
10. “Human context ends as a doubly conditional provenance hypothesis, not a declared cause.”

## Plain-English rule

Use concrete causal verbs and make each paragraph complete one transition.

Prefer:

> Making the national pattern visible did not solve the causal problem; it exposed it.

over:

> The resulting enhanced phenotype resolution necessitated an additional scale-aware attribution framework.

Methods may be sophisticated. The prose should make their biological necessity feel unavoidable.