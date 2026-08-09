# Manuscript-facing analysis map

This file answers one question: **which analyses are actually used by the current paper?**

## Evidence hierarchy

| Layer | Ecological question | Manuscript role | Active implementation | Claim ceiling |
|---|---|---|---|---|
| YAMAP/iEcology | Can a recent national quantitative flower-colour dataset be built from non-biodiversity recreational imagery? | cross-cutting data/method contribution | YAMAP benchmark + `Data_S1` construction | complementary observation process; not unbiased or universally superior |
| Main 1 | What broad environment + space organizes pigmentation state and pigmented-only intensity? | MAIN | current-input reanalysis; environment + INLA-SPDE | broad geographical template; not variance partition or proof of abiotic causation |
| Main 2 | Do sharp nearby white-pigmented boundaries align with focal Bombus availability? | MAIN | 5-km pure non-overlap transition design; *B. ardens* + *B. diversus* | weak/local directional consistency; not pollinator-mediated selection |
| Main 3 | Which local colour configurations depart from the natural template, and what human context do they occupy? | MAIN | repeatable ecological-event detector + predictive-map replay + post-selection human context | field/provenance targets; no anthropogenic-origin claim |
| Bombus community turnover | Do colour boundaries also coincide with broader Bombus-community boundaries? | SUPPLEMENT | five-species matched Hellinger turnover | biogeographic correspondence only; no direction of colour selection |
| montane/alpine Bombus | Is high-elevation Bombus overlap an additional colour mechanism? | SUPPLEMENT / negative guardrail | near-equal-elevation transition comparison | no additional effect beyond shared elevational geography |

## Current frozen numerical references

### Broad + anomaly

- run: `31258851297`
- artifact: `9022276431`
- artifact SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`

### Fresh Bombus source build

- run: `31249841493`
- artifact: `9020226937`
- artifact SHA-256: `d5d639e8e00d1ccc2f887c53fa8041465905b29f6bca1127f816e8c7a649d708`

### Occurrence-referenced focal-pollinator support

- run: `31262211605`
- artifact: `9023137743`
- artifact SHA-256: `d04c561b09b652591b9b479f6e26a779bb562c7c1b5f9b14e61d5e7ca8e2794b`

### Local sharp-transition test

- run: `31263324505`
- artifact: `9023416810`
- artifact SHA-256: `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`

### Supplementary community-boundary / montane guardrail

- run: `31285234317`
- artifact: `9029595037`
- artifact SHA-256: `067dd3408b2a7f046ba263732ffa4cefa2f54a7f1fb672478be46bca425f6bf0`

### YAMAP public-source benchmark

- retrieval run/artifact: `31289927019` / `9031041034`
- retrieval SHA-256: `3e53669395cfd926a0942b3488f844720dca2cb97b9ea210627262691e69f31a`
- provider-overlap audit run/artifact: `31290095532` / `9031085975`

## What is deliberately excluded from the current paper

The following development ideas are historical and belong under `legacy/`:

- the 1,909-observation publication architecture;
- the old all-five lower-third Bombus limitation gate;
- national environment+SPDE Bombus null tests as a Main mechanism;
- relaxation/local-contrast development variants superseded by the 5-km sharp-transition design;
- older E&E manuscript drafts and figure plans;
- local Bombus turnover implementations superseded by the spatially matched five-species boundary analysis;
- one-time manuscript patch scripts/workflows and superseded submission locks.

The historical analyses can still be cited in internal provenance notes, but they do not determine current claims.
