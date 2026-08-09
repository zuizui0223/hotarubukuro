# hotarubukuro

Reproducible multiscale analysis of flower-colour geography in *Campanula punctata* from author-reviewed YAMAP photographs.

## Manuscript-facing analysis: broad -> fine -> anomaly

The active paper no longer uses the historical 1,909-observation five-species limitation gate. The manuscript-facing numerical baseline is the fresh current-input rerun derived from the 1,965-row source table:

| Quantity | Active value |
|---|---:|
| source rows | 1,965 |
| final two-part phenotype observations | 1,922 |
| white-like observations | 966 |
| pigmented observations | 956 |
| ambiguous mixture assignments retained | 124 |
| 1-km analysis cells | 1,305 |
| response-blind a* boundary | 4.968780 |

The paper is organized into three main questions and one supplementary family.

### Main 1 - Broad natural template

National environment-plus-INLA-SPDE models describe the broad geography of:

1. whether visible pigmentation is expressed; and
2. visible intensity conditional on pigmentation.

The broad model is a geographical reference, not a claim that environment alone explains the phenotype. Five blocked folds give AUC=0.863 for pigmentation state and RMSE=0.919 for conditional intensity.

### Main 2 - Fine-scale pollinator hypothesis

The primary pollinator exposure is occurrence-referenced predicted availability of **two documented broad focal pollinators**, *Bombus ardens* and *B. diversus*:

`effective_occmax = max(A_ardens, A_diversus)`.

The final local test does not regress national flower colour on environmentally derived Bombus SDMs. It instead selects non-overlapping **pure white-versus-pigmented transitions** without Bombus information among the five nearest neighbours within 5 km, then asks whether the Bombus contrast points from the whiter to the more pigmented side. The primary 5-km set contains 67 pairs (median separation 2 km). The mean occurrence-referenced contrast is +0.0359 (one-sided sign-flip P=0.027), but the median is -0.0028, only 49.3% of pairs are positive, across-scale q=0.081, and raw SDM support / 10-25 km sensitivities are null. The claim ceiling is **weak, local exploratory consistency with the pigmentation-benefit relaxation hypothesis**, not pollinator-mediated selection.

Why only two species in the main availability metric? Adding *B. beaticola*, *B. consobrinus* and *B. honshuensis* changes the estimand from local availability of documented broad focal pollinators to geographical replacement among lowland/montane Bombus niches. In the fresh data, the maximum rank across all five species never becomes low (minimum=0.489), and apparent montane-Bombus/pigmentation associations disappear in near-equal-elevation comparisons. High-elevation taxa are therefore sensitivity/guardrail analyses, not primary mechanistic evidence.

### Main 3 - Event-based anomaly screen and human context

The final anomaly stage does **not** define unusual cells by a raw residual cutoff. A candidate is a pigmented cell embedded among geographically close, environmentally similar white neighbours. The same local event is replayed on repeated natural predictive maps before any human variable is examined.

The fresh data contain 17 primary candidates. Their count/fraction are compatible with both held-out cross-fitted and joint posterior-predictive natural references, so the candidate set is **not evidence that an additional process is required**. Post-selection human context is suggestive: 5-km population and population-DID alignment have nominal P<0.03, but maxT familywise P=0.090 and 0.076. These locations are follow-up targets, not evidence of horticultural origin.

### Supplement - broader Bombus biogeography and robustness

Supplementary analyses contain:

- five-species Bombus community turnover at flower-colour boundaries;
- spatial/elevational matched-background and spatial-block replication diagnostics;
- montane/alpine equal-elevation guardrails;
- all-five, raw-SDM and scale/transition-threshold availability sensitivities;
- full anomaly natural-map and human-context sensitivity families; and
- historical environment+SPDE Bombus-null analyses as method-development provenance.

Five-species turnover is treated as **biogeographic correspondence between flower-colour and predicted pollinator-community boundaries**, not as a directional flower-colour mechanism.

## Final integration workflow

The manuscript-facing integration workflow is:

`.github/workflows/final-paper-analysis.yml`

It restores checksum-locked upstream artifacts for the fresh broad/anomaly rerun and occurrence-referenced Bombus support, reruns the final local availability test and supplementary community-boundary guardrails, validates the final stage family, and uploads one integration artifact.

The locked numerical provenance and the Main/Supp claim hierarchy are documented in:

`reproducibility/final_paper_pipeline_2026-08-09.md`

The active manuscript is:

`manuscript/ecology-and-evolution-manuscript.md`

with figure and Supporting Information roles in:

- `manuscript/figure-map.md`
- `manuscript/supporting-information-plan.md`

## Repository status

Historical workflows such as `analysis-1909.yml`, `run_analysis_1909.sh` and the earlier five-species limitation gate remain in the repository for provenance and compatibility. They are **not manuscript-facing final estimands**. Do not infer the current paper from those historical entry points.

The branches used to develop the final analysis remain unmerged unless explicitly requested. The current integration branch is `agent/final-broad-fine-anomaly-pipeline`.

## Inference ceilings

- CIELAB a* is an image-derived visible phenotype, not anthocyanin concentration or Bombus visual contrast.
- National environment/SPDE models describe broad geography; predictive AUC is not variance decomposition.
- Bombus SDMs are predictions from environment and represent potential habitat availability, not visitation, abundance, pollen transfer or selection.
- Changing to local sharp transitions reduces broad spatial/environmental confounding by design but does not eliminate unmeasured environmental confounding.
- Montane/alpine Bombus associations are adequately explained by shared elevational geography in the present data.
- Local departure frequency is not robustly greater than the natural predictive reference.
- Human-context results are post-selection and familywise-inconclusive; horticultural provenance is not demonstrated.
