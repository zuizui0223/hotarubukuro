# hotarubukuro

Reproducible multiscale analysis of flower-colour geography in *Campanula punctata* from author-reviewed, GPS-linked YAMAP hiking photographs.

## What this project contributes

The manuscript is not organized as an omnibus `environment + Bombus + human` regression. It deliberately changes the **data representation, spatial scale and comparison unit** as the ecological question becomes more specific.

A cross-cutting methodological contribution is the use of **YAMAP as an iEcology source**. YAMAP is a hiking/navigation and activity-diary platform rather than a purpose-built biodiversity database. Publicly shared hiking activities can retain route-linked photograph locations, allowing incidental mountain-plant photographs to be recovered even when they were not uploaded as formal species observations. Author review verifies the focal taxon, flower and image region before quantitative colour extraction. This does not remove route/access or subject-selection bias; it creates a complementary observation process with explicit GPS/activity provenance.

The final manuscript-facing baseline is derived from the 1,965-row source table:

| Quantity | Active value |
|---|---:|
| source rows | 1,965 |
| final phenotype observations | 1,922 |
| white-like observations | 966 |
| pigmented observations | 956 |
| ambiguous mixture assignments retained | 124 |
| 1-km analysis cells | 1,305 |
| response-blind a* boundary | 4.968780 |

## The paper: broad -> fine -> anomaly

### Main 1 - Broad quantitative trait geography

The first layer closes a measurement gap before any more adventurous hypothesis is tested.

1. flower colour is represented as a **two-part quantitative phenotype**: whether visible pigmentation is expressed, and visible intensity conditional on pigmentation;
2. national environmental associations are estimated while explicitly representing continuous residual geography with INLA-SPDE;
3. geographically blocked prediction defines a natural geographical reference rather than treating clustered photographs as independent observations.

Five blocked folds give AUC=0.863 for pigmentation state and RMSE=0.919 for conditional intensity. This is a strong geographical template, not a claim that environment alone explains most phenotypic variance.

### Main 2 - Fine-scale, species-specific pollinator hypothesis

Predicted *Bombus* surfaces are generated from environmental geography, so the final paper does **not** put national environment, space and Bombus SDMs into one causal regression. Instead, it changes scale.

The primary exposure is occurrence-referenced predicted availability of the two broadly distributed focal taxa with direct evidence as predominant pollinators in the *C. punctata* system:

`effective_occmax = max(A_ardens, A_diversus)`.

The test first identifies non-overlapping **pure white-versus-pigmented transitions** among the five nearest neighbours within 5 km without using Bombus values or transition direction. Only after the transition set is fixed are pairs oriented white -> pigmented and compared for focal-pollinator availability.

The strict set contains 67 pairs with median separation 2 km. Mean pigmented-minus-white availability is +0.0359 (one-sided sign-flip P=0.027), but the median is -0.0028, only 49.3% of pairs are positive, across-scale q=0.081, and raw-SDM and 10/25-km sensitivities are null. The claim ceiling is therefore **weak/local exploratory consistency with a pigmentation-benefit relaxation hypothesis**, not pollinator-mediated selection.

Why are the three montane/alpine taxa not added to the primary availability index? Because doing so changes the estimand from local availability of documented broad focal pollinators to geographical replacement among Bombus niches. In the fresh data, the all-five maximum rank never becomes low, and apparent montane-Bombus/pigmentation associations disappear under near-equal-elevation comparisons. This is treated as a guardrail against recycling shared high-elevation geography as a pollinator mechanism.

### Main 3 - Event-based departures and human context

The final layer does not call a cell anomalous because one fitted residual is large. A candidate is a **pigmented cell embedded among geographically close, environmentally similar white neighbours**. The identical local event is replayed on repeated natural predictive maps before any human variable is inspected.

The fresh data contain 17 candidates. Their count/fraction are compatible with held-out cross-fitted and joint posterior-predictive natural references, so the set is not evidence that an additional process is required. Population at 5 km and population-DID alignment are suggestive, but maxT familywise P-values are 0.090 and 0.076. Because *C. punctata* is also cultivated ornamentally, these are useful provenance/field targets, not evidence of horticultural origin.

The YAMAP sampling frame matters here as well. Mountain-route sampling may compress the full urban-rural gradient and thereby reduce power for a broad human-context signal, while trailheads, roads and access can create observation-opportunity bias in the opposite direction. Human-context results are therefore interpreted conservatively.

## Supplement - biogeographic guardrails, not extra main stories

Supporting analyses include:

- five-species Bombus community turnover at flower-colour boundaries;
- spatial/elevational matched-background and spatial-block replication diagnostics;
- montane/alpine equal-elevation guardrails;
- all-five, raw-SDM and scale/transition-threshold availability sensitivities;
- full anomaly natural-map and human-context sensitivity families; and
- historical environment+SPDE Bombus analyses as method-development provenance.

Five-species turnover is interpreted only as **biogeographic correspondence between flower-colour and predicted pollinator-community boundaries**. It is unsigned and therefore does not provide a directional flower-colour mechanism.

## Active manuscript and design documents

The manuscript-facing paper is now:

`manuscript/ecology-and-evolution-manuscript-final.md`

Supporting design files:

- `manuscript/figure-map.md`
- `manuscript/supporting-information-plan.md`
- `docs/yamap-iecology-rationale.md`
- `reproducibility/final_paper_pipeline_2026-08-09.md`

The previous `manuscript/ecology-and-evolution-manuscript.md` is retained as the immediately preceding draft/provenance snapshot and should not be used as the current narrative source.

## Final integration workflow

The numerical integration workflow is:

`.github/workflows/final-paper-analysis.yml`

It restores checksum-locked current-input and occurrence-referenced Bombus artifacts, reruns the manuscript-facing local availability test and supplementary boundary guardrails, validates the Main/Supp hierarchy and produces a provenance artifact.

## Inference ceilings

- CIELAB a* is an image-derived visible phenotype, not anthocyanin concentration or Bombus receptor contrast.
- YAMAP provides a complementary route-linked observation stream, not an unbiased population survey.
- National environment/SPDE models describe broad geography; predictive AUC is not variance decomposition.
- Bombus SDMs represent potential habitat availability, not visitation, abundance, pollen transfer or selection.
- Changing to local sharp transitions reduces broad geographical confounding by design but does not eliminate unmeasured environmental confounding.
- Montane/alpine Bombus associations are adequately explained by shared elevational geography in the present data.
- Local departure frequency is not robustly greater than the natural predictive reference.
- Human-context results are post-selection and familywise-inconclusive; horticultural provenance is not demonstrated.

Do not merge the manuscript integration branch into `main` without explicit approval.