# Current paper — start here

This is the **single biological entry point** for the current JBI paper. For exact scripts, evidence files and artifact identities, use [`analysis-map.md`](analysis-map.md).

## The biological question

Why do white and pigmented flowers of the spotted bellflower (*Campanula punctata*) remain geographically structured across Japan, and why does colour intensity vary again within pigmented flowers?

The paper argues that this question cannot be answered by adding environment, space, pollinators and human variables to one national regression. The required phenotype was first missing at national quantitative resolution; once recovered, environmental mechanisms had to be separated from continuous geography; the pollinator hypothesis then had to move to replicated local boundaries; and a different local-isolate event had to be naturally calibrated before human context was read.

The fixed causal sequence is:

`trait recovery -> physiological hypotheses + continuous space -> heterogeneous local Bombus boundaries -> selected and naturally calibrated local isolates -> bounded provenance hypothesis`

## 1. A national quantitative trait dataset from an alternative image stream

Occurrence coordinates alone do not measure flower colour, and the matched public biodiversity-image streams were substantially sparser for this species and period. Author-screened YAMAP hiking photographs were therefore converted into a traceable flower-colour dataset.

- 1,922 observations in 1,305 1-km cells;
- 966 white-like and 956 pigmented;
- matched YAMAP retrieval 3.81 times larger than iNaturalist for the same focal species and period;
- taxon, focal flower, petal region, coordinates and duplicate images checked before deterministic colour extraction;
- pigmentation state separated from visible intensity among already pigmented flowers.

The mountain-route frame has no single bias direction. It enriches natural and semi-natural settings in which self-sustaining populations are plausible, but does not prove wild provenance. In the human analysis it can compress the urban–rural gradient, while routes, roads and trailheads can also concentrate observation, disturbance and opportunities for human-mediated movement.

**Scientific gain:** national intraspecific trait geography becomes measurable as a two-part quantitative phenotype rather than inferred only from a small set of categorical populations.

**Conditional-response ceiling:** intensity is modelled only after a flower enters the pigmented subset. This avoids treating white flowers as merely low-intensity flowers, but state and intensity cannot be assumed to be causally independent; shared unmeasured causes could create selection or collider-like distortion in the conditional model.

Details: Appendices S1–S2.

## 2. Broad environment and continuous geography generate competing ecological hypotheses

Separate INLA-SPDE models show that pigmentation state and conditional intensity are not one white-to-dark axis.

- pigmentation is less likely in warmer climates;
- among pigmented flowers, the warm-climate decline in intensity strengthens with temperature seasonality;
- intensity is lower toward wetter and more rugged geography;
- soil, precipitation seasonality and RSDS do not show independently resolved final effects;
- residual spatial ranges differ between state and intensity, but are not genetics or dispersal distances.

These directions should not be collapsed into “stress makes flowers darker.” The cool-climate state pattern is compatible with temperature-responsive anthocyanin regulation or environment-aligned population differentiation. The drier-side intensity pattern motivates a water-balance hypothesis, but dark petals may also incur radiative and hydraulic cooling costs. The negative ruggedness coefficient argues against a universal terrain-stress darkening rule, and the null final RSDS effect does not establish a national radiation mechanism.

**Scientific gain:** the Broad stage yields testable thermal, moisture and terrain alternatives plus coherent unresolved geography for common-garden, flower-temperature, water-relation, microclimate, ancestry, isolation-by-distance and admixture tests.

Details: Appendix S3.

### Broad supporting test: environment-aligned divergence beyond continuous space

Merged PR #50 asks a different question from coefficient estimation: at comparable geographical separation, are environmentally dissimilar held-out locations more phenotypically different than an intercept + Matérn SPDE null predicts?

| Response | Observed high-env − low-env divergence | Space-null median | Excess | One-sided posterior-predictive P |
|---|---:|---:|---:|---:|
| Pigmentation state | **0.106802** | 0.058240 | **+0.048562** | **0.03393** |
| Conditional intensity | -0.047179 | -0.001287 | -0.045891 | 0.87226 |

**Scientific gain:** the white–pigmented state transition carries environmental alignment that geographical proximity alone does not fully reproduce, whereas variation within the observed pigmented subset does not show the same excess. The space-exceeding signal belongs to state, not intensity.

**Claim ceiling:** the spatial null contains unresolved geography. This is an FST–PST-inspired empirical continuity comparison, not FST/PST/QST, and it does not establish selection, local adaptation or a unique causal environmental mechanism.

Details: [`../docs/broad_spatial_inertia_environment_tracking.md`](../docs/broad_spatial_inertia_environment_tracking.md).

## 3. Replicated local boundaries reveal heterogeneity, not a uniform Bombus shift

The broad map cannot isolate Bombus because flower colour, climate and environmentally built Bombus SDMs share geography. The 5-km boundary comparison therefore changes the comparison unit rather than adding another national coefficient. Five kilometres is the finest predeclared scale with sufficient replicated transitions, not an exact bumblebee foraging distance.

Sixty-seven non-overlapping white-pigmented boundaries were fixed before Bombus values were read. These are geographically repeated local contrasts, not one continuous transect.

- mean occurrence-referenced focal contrast: **+0.03590**;
- median: **-0.00277**;
- positive pairs: **49.3%**;
- q across 5/10/25 km: **0.08148**;
- mean attenuates at 10 and 25 km;
- raw SDM support does not reproduce the result;
- no persuasive relationship appears for conditional intensity.

The positive mean is therefore driven by a subset of large boundaries rather than a pervasive pairwise shift. **Boundary heterogeneity is the result.** The 67 sites should be stratified into strongly positive, near-zero and negative classes for direct tests of visitor abundance and phenology, alternative pollinators, bee visual contrast, stigma contact, pollen transfer, breeding system, gene flow, ancestry and microenvironment.

The large tubular flowers and the Izu-island pollinator and breeding-system literature make bumblebees a species-specific, biologically tractable hypothesis. That natural history motivates the test but does not prove that bumblebee absence caused white flowers.

The three montane/alpine species perform a different job. Their attractive national overlap with pigmented high-elevation flowers disappears when nearby endpoints are compared at similar elevation, showing how shared mountain geography can masquerade as a pollinator mechanism.

**Claim ceiling:** the SDMs represent habitat opportunity rather than abundance, visitation, pollen transfer or realized selection. At most, the pattern motivates a local mosaic of pigmented-state maintenance or loss in a subset of neighbourhoods, not national darkening.

Details: Appendices S4–S5.

## 4. Sixteen local isolates are a different estimand from the 67 boundaries

The paper contains two local designs that must not be collapsed.

| | Bombus boundaries | Local isolates |
|---|---|---|
| Unit | non-overlapping edge | focal cell plus neighbourhood |
| Colour pattern | pure white–pigmented transition | pigmented focal cell among environmentally similar observed white neighbours |
| Question | signed Bombus change across the edge | event frequency, then human context |
| 10,000-map replay | not used | used twice |

Applying the fixed, human-blind isolate rule to the **observed map** selects the 16 sites. The replay does not select sites that natural processes cannot reproduce.

### First use of the 10,000 maps: event-frequency calibration

The detector is reapplied to each cross-fitted natural predictive map. This asks whether the count or fraction of the event type is unusual.

- observed targets: 16;
- null mean count: 13.59;
- candidate-count P=0.27897;
- candidate-fraction P=0.12609.

The event frequency is compatible with natural spatial variation. The sites are reproducibly selected field targets, not “natural-process-resistant” anomalies.

### Second use of the 10,000 maps: post-selection human null

On every predictive map, the detector selects a new map-specific event set and the same human contrasts are recalculated. This propagates uncertainty in which cells the ecological rule would select, rather than comparing the fixed 16 sites with a standard regression null.

**Scientific gain:** a biologically legible configuration is separated from evidence that an additional process is required, and later human inference accounts for the site-selection rule.

## 5. Human context ends as a doubly conditional provenance hypothesis

After the observed 16 sites were fixed, population exposure within 5 km emerged as the leading feature (+0.06744; directional P=0.00800; global maxT FWER P=0.05479), while observation-effort alternatives were null.

The inference is doubly conditional:

1. the isolate event is not more frequent than the natural model predicts;
2. the strongest human feature remains just outside global familywise support.

The mountain-route frame can attenuate a real human contrast by narrowing settlement variation, but it can also inflate or confound it because accessible trails and roads covary with observation, disturbance and movement. It is not valid to argue that the weak result must be conservative because the data came from mountains.

**Scientific gain:** the analysis identifies a short-range provenance hypothesis and its likely comparison scale without assigning horticultural origin.

**Claim ceiling:** the 16 sites are not demonstrated plantings, escapes, cultivars or anthropogenic populations. Settlement exposure remains compatible with horticultural opportunity, access bias, managed local environment and natural variation.

Details: Appendix S6.

## The ecological model

The results converge on one working model of polymorphism maintenance:

1. climate changes the regulation, physiological benefit and thermal or hydraulic cost of anthocyanin pigmentation;
2. unresolved historical and population processes preserve or redistribute colour variants;
3. local bumblebee opportunity may modify the reproductive value of maintaining a visible pigmented state in some neighbourhoods;
4. human movement or managed microenvironment may occasionally add a local layer.

White and pigmented flowers can persist together because these benefits and costs vary across space and act on different components of colour. The strength of the paper is not one spectacular P value. Positive, heterogeneous and null results perform different inferential jobs in a fixed sequence.

## Direct next tests

- thermal/moisture geography → common-garden and reciprocal-transplant experiments, pigment chemistry, absorptance, flower temperature, transpiration, water relations and fitness;
- conditional state–intensity dependence → experimental pigment induction and joint modelling with standardized spectra and chemistry;
- spatial-null excess and residual geography → denser microclimate plus ancestry, isolation-by-distance, isolation-by-environment and admixture tests;
- 67 local Bombus boundaries → stratified species-resolved visitation, visual contrast, stigma contact, pollen deposition, seed set and selection gradients;
- 16 local isolates → repeated field records, vouchers, route/management/planting history, local microenvironment and genomic provenance.

## What to read next

- **Exact result → evidence mapping:** [`analysis-map.md`](analysis-map.md)
- **How to rerun the full paper:** [`../docs/reproduction-guide.md`](../docs/reproduction-guide.md)
- **Current manuscript:** [`../submission/jbi/JBI_main_manuscript_anonymized.md`](../submission/jbi/JBI_main_manuscript_anonymized.md)
- **Narrative lock:** [`../submission/jbi/JBI_background_architecture.md`](../submission/jbi/JBI_background_architecture.md)
- **Supporting Information:** [`../submission/jbi/supporting/`](../submission/jbi/supporting/)
- **Data dictionary:** [`../docs/data-s1-dictionary.md`](../docs/data-s1-dictionary.md)

Original YAMAP photographs are third-party content and are not redistributed. `Data_S1.csv` is the public derived trait/source table.