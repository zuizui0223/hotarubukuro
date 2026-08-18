# Supporting Information map

The Main paper keeps the biological questions and principal results. Supporting Information keeps the technical detail needed to reproduce, stress-test and limit those results.

## S1 — Where did the photographs come from?

**Main question supported:** can YAMAP provide a useful national image stream for this flower-colour study?

Contains:

- retrieval and author-screening rules;
- YAMAP/iNaturalist/GBIF benchmark;
- duplicate and coordinate audits;
- observation-process limits.

**Main point:** YAMAP adds a large mountain-route image stream, but it is not unbiased occurrence sampling.

## S2 — How was flower colour measured?

**Main question supported:** are pigmentation state and colour intensity defined before ecological predictors enter?

Contains:

- petal-region and colour-extraction settings;
- CIELAB conversion;
- image QC;
- four-component a* mixture;
- a*=4.968780 white/pigmented boundary;
- ambiguity and alternative-classification checks;
- exact state and intensity definitions.

**Main point:** the two colour responses are reproducible visible traits, not direct anthocyanin chemistry or bee colour space.

## S3 — How robust is the broad environment/spatial model?

**Main question supported:** do state and intensity have different broad environmental and spatial patterns?

Contains:

- final eight abiotic axes;
- INLA-SPDE model specification;
- interaction screen;
- VIF checks;
- VPD and water-balance tests;
- alternative spatial models;
- final coefficients and spatial ranges;
- cross-fitted space-only phenotype-divergence sensitivity;
- cross-fitted natural model used later.

**Main point:** broad environmental associations are stable enough to motivate physiological hypotheses, and pigmentation-state divergence shows environmental alignment beyond a cross-fitted spatial expectation. Neither result proves adaptation; the spatial field remains unresolved geography.

## S4 — How were Bombus habitat-support maps built?

**Main question supported:** what exactly does the Bombus exposure represent?

Contains:

- occurrence filtering;
- common mainland domain;
- predictor screen;
- tuning and model selection;
- spatial validation;
- occurrence-reference transformation;
- focal-species rationale.

**Main point:** the maps represent predicted habitat opportunity, not visitation, abundance or selection.

## S5 — How fragile is the local Bombus result?

**Main question supported:** does the 5-km Bombus contrast survive alternative scales and confounding checks?

Contains:

- 67 fixed local pairs;
- 5/10/25-km tests;
- median and positive-pair fraction;
- raw-SDM and all-five-species checks;
- environmental-locality diagnostic;
- community-turnover analysis;
- spatial repetition;
- equal-elevation montane negative control.

**Main point:** the positive mean at 5 km is weak and scale dependent. The high-elevation overlap is not an independent pollinator mechanism.

## S6 — How were local departures and human context tested?

**Main question supported:** are local pigmented departures unusual, and what happens when human context is tested afterwards?

Contains:

- exact local-event rule;
- 10,000-map replay;
- count and fraction null distributions;
- 16 observed sites;
- full 11-feature human family;
- global maxT correction;
- observation-effort alternatives.

**Main point:** the 16 sites are not unusually common under the natural model. The short-range population signal is a provenance hypothesis, not proof of human origin.

## Editorial rule

A technical result belongs in Main only if it changes the biological interpretation.

Otherwise it stays here.

The reader should be able to read Main without knowing the model-development history, while a reviewer should be able to find every important robustness check in S1-S6.
