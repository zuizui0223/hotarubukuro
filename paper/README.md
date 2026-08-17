# Current paper — start here

This is the **single entry point for the biology** of the current paper. The single execution entry is [`../run_pipeline.py`](../run_pipeline.py); exact scripts, evidence files and artifact IDs are mapped in [`analysis-map.md`](analysis-map.md).

## The biological question

Why do white and pigmented flowers of *Campanula punctata* vary across Japan?

The paper tests whether different parts of flower colour respond to different ecological processes. It separates:

- **pigmentation state:** white-like versus visibly pigmented;
- **colour intensity:** how strong the visible colour is after pigmentation is present.

The study then asks four questions.

## 1. Can we measure the polymorphism across Japan?

We built the dataset from YAMAP hiking photographs.

Every retrieved candidate was screened by the authors. We checked the taxon, focal flower, usable petal region, coordinates and duplicate images before extracting colour.

**Current analysis:** 1,922 observations in 1,305 1-km cells; 966 white-like and 956 pigmented.

**Why this matters:** the study does not rely on an existing trait database. The national quantitative phenotype was constructed for this project.

Details: Appendices S1-S2.

## 2. How do environment and geography relate to flower colour?

We fitted separate spatial models for pigmentation state and colour intensity.

Main result:

- pigmentation was less likely in warmer climates;
- among pigmented flowers, intensity depended on Temperature PC1 × temperature seasonality and was lower in wetter and more rugged environments;
- substantial spatial structure remained after measured environment.

**What this means:** state and intensity are not one simple white-to-dark ecological axis.

**What it does not mean:** the models do not prove local adaptation or identify the residual spatial field as population history.

Details: Appendix S3.

## 3. Do bumblebees help explain local white-pigmented boundaries?

We did not add Bombus SDMs to the national environmental model. Those SDMs are themselves built from environmental data, so national overlap can be hard to interpret.

Instead, we selected nearby white-pigmented boundaries first and read Bombus support only afterwards.

**Primary design:** 67 non-overlapping pure transitions within 5 km.

**Result:** mean focal-Bombus contrast +0.03590; median -0.00277; 49.3% positive pairs; P=0.02716; q=0.08148 across the three main scales.

**Interpretation:** weak local correspondence only. If it is biological, it fits local maintenance of a pigmented state better than progressive darkening.

Details: Appendices S4-S5.

## 4. Are local pigmented departures unusual, and do they have a human context?

We defined a local event before looking at any human variable: a pigmented cell surrounded by at least three nearby, environmentally similar white cells.

The same detector was replayed on 10,000 predictive maps.

**Result:** 16 observed departures; count P=0.27897; candidate-fraction P=0.12609.

So the observed number is not unusual under the natural model.

Only then did we test human context. Population exposure within 5 km was the strongest feature (+0.06744; directional P=0.00800), but global maxT FWER P=0.05479.

**Interpretation:** these sites are good provenance targets, not demonstrated anthropogenic anomalies.

Details: Appendix S6.

## The ecological story

The working model is simple:

1. climate changes the physiological context of anthocyanin pigmentation;
2. local pollinators may change the reproductive value of visible pigmentation;
3. population history can preserve or move colour variants across space;
4. human movement may occasionally add a local source.

The current data do not prove those mechanisms. They tell us where to test them next.

- temperature/moisture gradients → common-garden and reciprocal-transplant experiments;
- residual spatial geography → population genomics, ancestry and admixture;
- local Bombus boundaries → visitation, pollen transfer and seed set;
- 16 departures → field history, vouchers and genomic provenance.

## Reproduce or audit the paper

```bash
python run_pipeline.py audit
python run_pipeline.py reproduce
```

`audit` verifies manuscript/repository/evidence-lock alignment. `reproduce` reruns the accepted downstream analyses and rebuilds figures and the review bundle from checksum-locked evidence. Live source refresh is intentionally separate.

## What to read next

- **Exact result → evidence mapping:** [`analysis-map.md`](analysis-map.md)
- **Canonical reproduction contract:** [`../docs/reproduction-guide.md`](../docs/reproduction-guide.md)
- **Pipeline lock:** [`../config/paper_pipeline.lock.json`](../config/paper_pipeline.lock.json)
- **Current manuscript:** [`../submission/jbi/JBI_main_manuscript_anonymized.md`](../submission/jbi/JBI_main_manuscript_anonymized.md)
- **Supporting Information:** [`../submission/jbi/supporting/`](../submission/jbi/supporting/)
- **Data dictionary:** [`../docs/data-s1-dictionary.md`](../docs/data-s1-dictionary.md)

Original YAMAP photographs are third-party content and are not redistributed. `Data_S1.csv` is the public derived trait/source table.
