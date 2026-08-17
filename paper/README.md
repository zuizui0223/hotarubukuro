# Current paper — start here

This is the **single entry point** for the biology of the current paper. For exact scripts, evidence files and artifact IDs, use [`analysis-map.md`](analysis-map.md).

## The biological question

Why do white and pigmented flowers of the spotted bellflower (*Campanula punctata*) remain geographically structured across Japan?

The paper argues that one visible polymorphism can carry several layers of ecological information. The value of pigmentation may change with abiotic environment, local pollinator opportunity, population history and occasional human movement. Those processes cannot be read reliably from one national overlap, so the analysis changes comparison scale as the mechanism becomes more local.

## What the study delivers

### 1. A new national quantitative trait dataset

We converted author-screened YAMAP hiking photographs into a traceable flower-colour dataset.

- 1,922 observations in 1,305 1-km cells;
- 966 white-like and 956 pigmented;
- matched YAMAP retrieval 3.81 times larger than iNaturalist for the same focal species and period;
- taxon, focal flower, petal region, coordinates and duplicate images checked before deterministic colour extraction.

**Scientific gain:** national intraspecific trait geography becomes measurable rather than inferred from a small set of population categories.

Details: Appendices S1–S2.

### 2. Two flower-colour components with different ecology

Separate INLA-SPDE models show that pigmentation state and pigmented-only intensity are not one white-to-dark axis.

- pigmentation is less likely in warmer climates;
- intensity depends on Temperature PC1 × temperature seasonality and is lower toward wetter and more rugged geography;
- residual spatial ranges differ between state and intensity.

**Scientific gain:** the broad stage delivers both a candidate abiotic landscape and coherent unresolved geography for future microclimate and genomic tests.

Details: Appendix S3.

### 3. Local boundaries reveal where Bombus opportunity may matter

Pollinator-mediated selection is realized through local visits, pollen transfer and reproductive success, so the primary test was placed at the population-neighbourhood scale rather than on a national map. The 5-km radius was the finest predeclared scale with enough replicated transitions; it was not treated as an exact bumblebee foraging distance.

Sixty-seven white-pigmented boundaries were fixed before Bombus values were read. Mean focal-Bombus support was +0.03590 higher on the pigmented side at 5 km. Its concentration in a subset of boundaries, attenuation at 10 and 25 km and absence along the intensity axis localize the plausible contribution to short-range maintenance of pigmentation state.

Only after that local result is established does the highland guardrail enter. Apparent national overlap with montane/alpine Bombus disappears when nearby endpoints are matched for elevation, showing why broad maps can confuse pollinators with shared mountain environment.

**Scientific gain:** the scale change identifies a small, state-specific local Bombus signal, explains why it would be blurred or confounded nationally, and supplies 67 sites for direct visitation and selection tests.

Details: Appendices S4–S5.

### 4. Sixteen calibrated field and provenance targets

A relational local event was defined before any human variable was read: a pigmented cell surrounded by environmentally similar white cells.

The same detector was replayed on 10,000 natural predictive maps.

- 16 observed targets;
- candidate-count P=0.27897;
- candidate-fraction P=0.12609.

**Scientific gain:** the sites are reproducibly selected and naturally calibrated, rather than assumed to be anthropogenic anomalies or arbitrary large residuals.

### 5. A leading short-range human-context hypothesis

After candidate identities were fixed, population exposure within 5 km emerged as the leading feature (+0.06744; directional P=0.00800; global maxT FWER P=0.05479), while observation-effort alternatives were null.

**Scientific gain:** the analysis identifies the most promising provenance hypothesis and its spatial scale without assigning origin prematurely.

Details: Appendix S6.

## The ecological model

The results converge on one model of polymorphism maintenance:

1. climate changes the physiological expression, benefit and heat cost of anthocyanin pigmentation;
2. population history preserves or redistributes colour variants;
3. local bumblebee opportunity may modify the reproductive value of maintaining a visible pigmented state;
4. human movement may occasionally add a local source.

White and pigmented flowers can persist together because these benefits and costs vary across space and act on different components of colour.

The study does not rely on one spectacular P value. Its strength is that strong, heterogeneous and null results each perform a different inferential job and generate direct next tests:

- thermal/moisture geography -> common-garden, reciprocal-transplant, pigment, physiology and fitness experiments;
- residual geography -> microclimate, ancestry, isolation-by-distance and admixture analyses;
- 67 Bombus boundaries -> visitation, visual contrast, pollen transfer, seed set and selection;
- 16 local targets -> field history, vouchers and genomic provenance.

## What to read next

- **Exact result -> evidence mapping:** [`analysis-map.md`](analysis-map.md)
- **How to rerun each stage:** [`../docs/reproduction-guide.md`](../docs/reproduction-guide.md)
- **Current manuscript:** [`../submission/jbi/JBI_main_manuscript_anonymized.md`](../submission/jbi/JBI_main_manuscript_anonymized.md)
- **Supporting Information:** [`../submission/jbi/supporting/`](../submission/jbi/supporting/)
- **Data dictionary:** [`../docs/data-s1-dictionary.md`](../docs/data-s1-dictionary.md)

Original YAMAP photographs are third-party content and are not redistributed. `Data_S1.csv` is the public derived trait/source table.
