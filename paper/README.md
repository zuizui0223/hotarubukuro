# Current paper — start here

This is the **single entry point** for the biology of the current paper. For exact scripts, evidence files and artifact IDs, use [`analysis-map.md`](analysis-map.md).

## The mystery

Why do white and pigmented flowers of *Campanula punctata* remain geographically structured across Japan?

The paper does not treat climate, population history, pollinators and human context as four parallel predictor families. It starts with one hidden trait pattern and moves inward each time the previous answer reveals a new ambiguity.

## First reveal — the national phenotype had to be built

We constructed a quantitative flower-colour dataset from author-screened YAMAP hiking photographs.

Every retrieved candidate was checked for taxon identity, focal flower, usable petal region, coordinates and duplicate images before colour extraction.

**Current analysis:** 1,922 observations in 1,305 1-km cells; 966 white-like and 956 pigmented.

The key discovery is that flower colour contains two responses:

- **pigmentation state:** whether visible pigmentation is present;
- **conditional intensity:** how strong colour is after pigmentation is present.

A single white-to-dark axis would have hidden the rest of the story.

Details: Appendices S1-S2.

## Second reveal — the two responses have different broad geography

Pigmentation was less likely in warmer climates. Among pigmented flowers, intensity depended on Temperature PC1 × temperature seasonality and was lower in wetter and more rugged environments.

Substantial continuous spatial structure remained after measured environment.

This answers one question and creates the next. Climate is related to flower colour, but it does not exhaust the geography; the remaining field may contain unmeasured environment, population structure, dispersal and sampling geography.

Details: Appendix S3.

## Third reveal — the attractive pollinator map weakens when we zoom in

Bombus SDMs are themselves built from environmental geography. A national overlap with flower colour could therefore mistake shared climate for a biotic mechanism.

We first fixed 67 non-overlapping white-pigmented boundaries within 5 km, then read focal-Bombus support.

**Result:** mean contrast +0.03590; median -0.00277; 49.3% positive pairs; P=0.02716; q=0.08148 across the three main scales. The effect fades at 10 and 25 km and is not reproduced by raw SDM support.

A stronger-looking overlap with montane Bombus also disappears when nearby endpoints are constrained to similar elevation.

**Interpretation:** broad map agreement is not the mechanism. If the weak focal signal is biological, it fits local maintenance of a pigmented state better than progressive darkening.

Details: Appendices S4-S5.

## Fourth reveal — apparent anomalies first belong to nature

We defined a local event before reading human variables: a pigmented cell surrounded by at least three nearby, environmentally similar white cells.

The same detector was replayed on 10,000 natural predictive maps.

**Result:** 16 observed departures; count P=0.27897; candidate-fraction P=0.12609.

The sites look unusual, but their frequency is compatible with the natural model.

Only then was human context tested. Population exposure within 5 km was the strongest feature (+0.06744; directional P=0.00800), but global maxT FWER P=0.05479.

**Interpretation:** human context leaves a provenance clue, not an origin answer.

Details: Appendix S6.

## The ecological answer

The layers converge on one working model:

- climate changes the physiological expression, benefit and cost of anthocyanin pigmentation;
- population history preserves or moves colour variants across regions;
- local pollinator opportunity may modify the reproductive value of a visible pigmented state;
- human movement may occasionally add a local source.

White and pigmented flowers can persist together if that balance changes across space. The current data identify this adaptive hypothesis without claiming to prove selection.

## The next causal layer

The national analysis now specifies where each mechanism should be tested directly:

- temperature/moisture gradients → common-garden and reciprocal-transplant experiments, pigment physiology and fitness;
- residual spatial geography → ancestry, isolation by distance and admixture;
- local Bombus boundaries → visitation, pollen transfer and seed set;
- 16 departures → field history, vouchers and genomic provenance.

## What to read next

- **Exact result → evidence mapping:** [`analysis-map.md`](analysis-map.md)
- **How to rerun each stage:** [`../docs/reproduction-guide.md`](../docs/reproduction-guide.md)
- **Current manuscript:** [`../submission/jbi/JBI_main_manuscript_anonymized.md`](../submission/jbi/JBI_main_manuscript_anonymized.md)
- **Supporting Information:** [`../submission/jbi/supporting/`](../submission/jbi/supporting/)
- **Data dictionary:** [`../docs/data-s1-dictionary.md`](../docs/data-s1-dictionary.md)

Original YAMAP photographs are third-party content and are not redistributed. `Data_S1.csv` is the public derived trait/source table.
