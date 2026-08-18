# Current paper — start here

This is the **single biological entry point** for the current JBI paper. For exact scripts, evidence files and artifact identities, use [`analysis-map.md`](analysis-map.md).

## The biological question

Why do white and pigmented flowers of the spotted bellflower (*Campanula punctata*) remain geographically structured across Japan?

The paper argues that one visible polymorphism contains several ecological layers. Abiotic environment, unresolved population/history geography, local pollinator opportunity and occasional human movement can draw similar maps, but they do not operate at the same comparison scale. The analysis therefore moves from national phenotype construction to broad geography, then to local boundaries and finally to calibrated local departures.

## 1. A national quantitative trait dataset

Author-screened YAMAP hiking photographs were converted into a traceable flower-colour dataset.

- 1,922 observations in 1,305 1-km cells;
- 966 white-like and 956 pigmented;
- matched YAMAP retrieval 3.81 times larger than iNaturalist for the same focal species and period;
- taxon, focal flower, petal region, coordinates and duplicate images checked before deterministic colour extraction.

**Scientific gain:** national intraspecific trait geography becomes measurable rather than inferred from a small set of categorical populations.

Details: Appendices S1–S2.

## 2. Two flower-colour components with different broad geography

Separate INLA-SPDE models show that pigmentation state and pigmented-only intensity are not one white-to-dark axis.

- pigmentation is less likely in warmer climates;
- intensity depends on Temperature PC1 × temperature seasonality and is lower toward wetter and more rugged geography;
- residual spatial ranges differ between state and intensity.

**Scientific gain:** the Broad stage yields both a candidate abiotic landscape and coherent unresolved geography for future microclimate and genomic tests.

Details: Appendix S3.

### Broad supporting test: environment-aligned divergence beyond continuous space

Merged PR #50 asks a different question from coefficient estimation: at comparable geographical separation, are environmentally dissimilar held-out locations more phenotypically different than an intercept + Matérn SPDE null predicts?

| Response | Observed high-env − low-env divergence | Space-null median | Excess | One-sided posterior-predictive P |
|---|---:|---:|---:|---:|
| Pigmentation state | **0.106802** | 0.058240 | **+0.048562** | **0.03393** |
| Conditional intensity | -0.047179 | -0.001287 | -0.045891 | 0.87226 |

**Scientific gain:** the state transition carries environmental alignment that geographical proximity alone does not fully reproduce, whereas the already-pigmented intensity response does not show the same excess. This sharpens the state-versus-intensity distinction.

**Claim ceiling:** the spatial null contains unresolved geography. This is not FST/PST/QST and does not establish selection, local adaptation or a unique causal environmental mechanism. It is retained as a reproducible Broad sensitivity, not substituted for the current observation-level JBI model.

The result is now reported directly in Main Methods 2.3, Results 3.2 and Discussion 4.2, with the complete design and Table S3.5 in Appendix S3.

Details: [`../docs/broad_spatial_inertia_environment_tracking.md`](../docs/broad_spatial_inertia_environment_tracking.md).

## 3. Local boundaries reveal where Bombus opportunity may matter

Merged PR #51 fixed the biological order: the local 5-km boundary comparison is primary because pollinator-mediated selection is realized through local visitation, pollen transfer and reproductive success. The radius is the finest predeclared scale with sufficient replicated transitions, not an exact bumblebee foraging distance.

Sixty-seven white-pigmented boundaries were fixed before Bombus values were read. Mean focal-Bombus support was **+0.03590** higher on the pigmented side at 5 km. The median was -0.00277, 49.3% of pairs were positive and q=0.08148 across the 5/10/25-km family. The mean attenuated at 10 and 25 km and no persuasive relationship appeared for pigmented-only intensity.

Only after that local result is established does the highland guardrail enter. Apparent national overlap with montane/alpine Bombus disappears when nearby white and pigmented endpoints are compared at similar elevation. Broad maps can therefore confuse a pollinator hypothesis with shared mountain environment.

**Scientific gain:** the scale change identifies a small, state-specific and heterogeneous local Bombus pattern, explains why a national overlay is insufficient, and supplies 67 sites for direct visitation and selection tests.

**Claim ceiling:** the SDMs represent habitat opportunity rather than abundance, visitation, pollen transfer or realized selection.

Details: Appendices S4–S5.

## 4. Sixteen calibrated field and provenance targets

A relational local event was defined before human variables were read: a pigmented cell surrounded by environmentally similar white cells. The same detector was replayed on 10,000 natural predictive maps.

- 16 observed targets;
- candidate-count P=0.27897;
- candidate-fraction P=0.12609.

**Scientific gain:** the sites are reproducibly selected and naturally calibrated rather than assumed to be anthropogenic anomalies or arbitrary residual extremes.

## 5. A leading short-range human-context hypothesis

After candidate identities were fixed, population exposure within 5 km emerged as the leading feature (+0.06744; directional P=0.00800; global maxT FWER P=0.05479), while observation-effort alternatives were null.

**Scientific gain:** the analysis identifies a focused provenance hypothesis and its likely spatial scale without assigning horticultural origin.

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
- 16 local targets → repeated field records, vouchers, planting history and genomic provenance.

## What to read next

- **Exact result → evidence mapping:** [`analysis-map.md`](analysis-map.md)
- **How to rerun the full paper:** [`../docs/reproduction-guide.md`](../docs/reproduction-guide.md)
- **Current manuscript:** [`../submission/jbi/JBI_main_manuscript_anonymized.md`](../submission/jbi/JBI_main_manuscript_anonymized.md)
- **Supporting Information:** [`../submission/jbi/supporting/`](../submission/jbi/supporting/)
- **Data dictionary:** [`../docs/data-s1-dictionary.md`](../docs/data-s1-dictionary.md)

Original YAMAP photographs are third-party content and are not redistributed. `Data_S1.csv` is the public derived trait/source table.
