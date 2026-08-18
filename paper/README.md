# Current paper — start here

This is the **single biological entry point** for the current JBI paper. For exact scripts, evidence files and artifact identities, use [`analysis-map.md`](analysis-map.md).

## The biological question

Why do white and pigmented flowers of the spotted bellflower (*Campanula punctata*) remain geographically structured across Japan, and why does colour intensity vary again within pigmented flowers?

The paper argues that this question could not be answered by adding environment, space, pollinators and human variables to one national regression. The required phenotype was first missing at national quantitative resolution; once recovered, environment and continuous space shared geography; the pollinator hypothesis therefore had to move to replicated local boundaries; and apparent local exceptions had to be calibrated against the natural model before human context was read.

The fixed causal sequence is:

`trait recovery -> broad environment + continuous space -> local Bombus boundary -> calibrated local isolate -> provenance hypothesis`

## 1. A national quantitative trait dataset from an alternative image stream

Occurrence coordinates alone do not measure flower colour, and the matched public biodiversity-image streams were substantially sparser for this species and period. Author-screened YAMAP hiking photographs were therefore converted into a traceable flower-colour dataset.

- 1,922 observations in 1,305 1-km cells;
- 966 white-like and 956 pigmented;
- matched YAMAP retrieval 3.81 times larger than iNaturalist for the same focal species and period;
- taxon, focal flower, petal region, coordinates and duplicate images checked before deterministic colour extraction;
- pigmentation state separated from visible intensity among already pigmented flowers.

The mountain-route sampling frame has two roles. It enriches natural and semi-natural settings in which self-sustaining populations are plausible, but it does not prove wild provenance for each record. The same frame later narrows the represented urban–rural gradient and can affect both observation opportunity and human exposure.

**Scientific gain:** national intraspecific trait geography becomes measurable as a two-part quantitative phenotype rather than inferred only from a small set of categorical populations.

Details: Appendices S1–S2.

## 2. Broad environment and continuous geography do not affect both colour components alike

Separate INLA-SPDE models show that pigmentation state and pigmented-only intensity are not one white-to-dark axis.

- pigmentation is less likely in warmer climates;
- intensity depends on Temperature PC1 × temperature seasonality and is lower toward wetter and more rugged geography;
- residual spatial ranges differ between state and intensity;
- coherent spatial structure is treated as an estimable geographical layer, not hidden in independent residuals or labelled as genetics in advance.

**Scientific gain:** the Broad stage yields both a candidate abiotic landscape and coherent unresolved geography for future microclimate, ancestry, isolation-by-distance and admixture tests.

Details: Appendix S3.

### Broad supporting test: environment-aligned divergence beyond continuous space

Merged PR #50 asks a different question from coefficient estimation: at comparable geographical separation, are environmentally dissimilar held-out locations more phenotypically different than an intercept + Matérn SPDE null predicts?

| Response | Observed high-env − low-env divergence | Space-null median | Excess | One-sided posterior-predictive P |
|---|---:|---:|---:|---:|
| Pigmentation state | **0.106802** | 0.058240 | **+0.048562** | **0.03393** |
| Conditional intensity | -0.047179 | -0.001287 | -0.045891 | 0.87226 |

**Scientific gain:** the white–pigmented state transition carries environmental alignment that geographical proximity alone does not fully reproduce, whereas variation within the pigmented state does not show the same excess. This is the key result-direction lock: the space-exceeding signal belongs to state, not intensity.

**Claim ceiling:** the spatial null contains unresolved geography. This is an FST–PST-inspired empirical continuity comparison, not FST/PST/QST, and it does not establish selection, local adaptation or a unique causal environmental mechanism. It remains a supporting Broad sensitivity rather than replacing the observation-level JBI model.

The result is reported directly in Main Methods 2.3, Results 3.2 and Discussion 4.2, with the complete design and Table S3.5 in Appendix S3.

Details: [`../docs/broad_spatial_inertia_environment_tracking.md`](../docs/broad_spatial_inertia_environment_tracking.md).

## 3. Replicated local boundaries reveal where Bombus opportunity may matter

The broad map cannot isolate Bombus because flower colour, climate and environmentally built Bombus SDMs share geography. Merged PR #51 therefore makes the local 5-km boundary comparison primary. Pollinator-mediated selection is realized through local visitation, pollen transfer and reproductive success; 5 km is the finest predeclared scale with sufficient replicated transitions, not an exact bumblebee foraging distance.

Sixty-seven non-overlapping white-pigmented boundaries were fixed before Bombus values were read. These are geographically repeated local contrasts, not one continuous transect. Mean occurrence-referenced support for the broad-ranging focal species *B. ardens* and *B. diversus* was **+0.03590** higher on the pigmented side at 5 km. The median was -0.00277, 49.3% of pairs were positive and q=0.08148 across the 5/10/25-km family. The mean attenuated at 10 and 25 km and no persuasive relationship appeared for pigmented-only intensity.

The large tubular flowers and the Izu-island pollinator and breeding-system literature make bumblebees a species-specific, biologically tractable hypothesis. That natural history motivates the test but does not prove that bumblebee absence caused white flowers.

Only after the local result is established does the highland guardrail enter. The three montane/alpine species show attractive national overlap with pigmented high-elevation flowers, but the contrast disappears when nearby white and pigmented endpoints are compared at similar elevation. They therefore demonstrate how mountain environment can masquerade as a pollinator mechanism rather than entering the primary broad-ranging focal index.

**Scientific gain:** the scale change identifies a small, state-specific and heterogeneous local Bombus pattern, explains why a national overlay is insufficient, and supplies 67 sites for direct visitation and selection tests.

**Claim ceiling:** the SDMs represent habitat opportunity rather than abundance, visitation, pollen transfer or realized selection.

Details: Appendices S4–S5.

## 4. Sixteen calibrated local isolates are a different question from the 67 boundaries

The paper contains two local designs that must not be collapsed.

- the 67 boundaries ask whether Bombus opportunity changes in the predicted direction across a nearby white–pigmented transition;
- the departure analysis asks whether a pigmented cell isolated among environmentally similar white neighbours requires an additional process.

The isolate event was defined before human variables were read, and the identical detector was replayed on 10,000 natural predictive maps.

- 16 observed targets;
- candidate-count P=0.27897;
- candidate-fraction P=0.12609.

**Scientific gain:** the sites are reproducibly selected and naturally calibrated rather than assumed to be anthropogenic anomalies or arbitrary residual extremes. Their observed frequency remains compatible with natural spatial variation.

## 5. Human context ends as a focused provenance hypothesis

After candidate identities were fixed, population exposure within 5 km emerged as the leading feature (+0.06744; directional P=0.00800; global maxT FWER P=0.05479), while observation-effort alternatives were null.

The mountain-focused sampling frame may compress the full urban–rural gradient and reduce power for a broad human effect. Conversely, trailheads, roads and accessible mountain margins can increase both photographic opportunity and human exposure. The weak near-threshold result therefore cannot be promoted by claiming that YAMAP has removed human influence.

**Scientific gain:** the analysis identifies a short-range provenance hypothesis and its likely comparison scale without assigning horticultural origin.

**Claim ceiling:** the 16 sites are not demonstrated plantings, escapes, cultivars or anthropogenic populations. Settlement exposure remains compatible with horticultural opportunity, access bias, managed local environment and natural variation.

Details: Appendix S6.

## The ecological model

The results converge on one working model of polymorphism maintenance:

1. climate changes the physiological expression, benefit and heat cost of anthocyanin pigmentation;
2. unresolved historical and population processes preserve or redistribute colour variants;
3. local bumblebee opportunity may modify the reproductive value of maintaining a visible pigmented state;
4. human movement may occasionally add a local source.

White and pigmented flowers can persist together because these benefits and costs vary across space and act on different components of colour. The strength of the paper is not one spectacular P value. Strong, heterogeneous and null results perform different inferential jobs in a fixed sequence.

## Direct next tests

- thermal/moisture geography → common-garden and reciprocal-transplant experiments, pigment chemistry, physiology and fitness;
- spatial-null excess and residual geography → denser microclimate plus ancestry, isolation-by-distance, isolation-by-environment and admixture tests;
- 67 local Bombus boundaries → species-resolved visitation, visual contrast, stigma contact, pollen deposition, seed set and selection gradients;
- 16 local isolates → repeated field records, vouchers, planting history and genomic provenance.

## What to read next

- **Exact result → evidence mapping:** [`analysis-map.md`](analysis-map.md)
- **How to rerun the full paper:** [`../docs/reproduction-guide.md`](../docs/reproduction-guide.md)
- **Current manuscript:** [`../submission/jbi/JBI_main_manuscript_anonymized.md`](../submission/jbi/JBI_main_manuscript_anonymized.md)
- **Narrative lock:** [`../submission/jbi/JBI_background_architecture.md`](../submission/jbi/JBI_background_architecture.md)
- **Supporting Information:** [`../submission/jbi/supporting/`](../submission/jbi/supporting/)
- **Data dictionary:** [`../docs/data-s1-dictionary.md`](../docs/data-s1-dictionary.md)

Original YAMAP photographs are third-party content and are not redistributed. `Data_S1.csv` is the public derived trait/source table.