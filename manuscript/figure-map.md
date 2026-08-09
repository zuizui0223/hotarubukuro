# Figure map for the final YAMAP + broad -> fine -> anomaly manuscript

The figures should make the logic visible without turning the paper into a list of statistical models.

| Figure | Main role | Required content |
|---|---|---|
| **Figure 1** | **Data innovation + phenotype + observed geography** | concise schematic `GPS hiking diary -> public route/photo position -> author taxon/flower-region review -> CIELAB extraction -> two-part phenotype`; national observation map; response-blind a* mixture; white/pigmented state; pigmented-only conditional intensity |
| **Figure 2** | **Broad natural template** | key environmental associations for state/intensity; SPDE ranges; geographically cross-fitted predictions/performance; emphasize that space is explicitly represented rather than ignored |
| **Figure 3** | **Fine-scale focal Bombus availability** | schematic/map of Bombus-blind 5-km pure transitions; environmental-similarity diagnostic; occurrence-referenced *B. ardens* + *B. diversus* pigmented-minus-white contrasts; sign-flip reference; concise claim ceiling |
| **Figure 4** | **Event-based local departures + human context** | ecological-event definition; 17 candidate locations; observed candidate count/fraction against repeated natural maps; 5-km population and population-DID post-selection contrasts with maxT uncertainty |

## Figure 1 communication rule: YAMAP is a design feature, not a claim of unbiased sampling

The schematic should show why YAMAP adds a different observation stream: photographs originate in hiking/activity documentation, not in a dedicated species-recording task, and retain route-linked spatial provenance when locations are public.

Do **not** imply that YAMAP is inherently less biased or universally better than iNaturalist/GBIF. State visually or in the caption that:

- trail/access, flower-conspicuousness and subject-selection bias remain;
- users may hide photo locations;
- author review controls taxon, duplicate and flower-region errors, not the original sampling process;
- mountain-route sampling enriches the natural/semi-natural mountain habitats relevant to the focal wild plant but does not guarantee wild provenance;
- the contribution is the repurposing of a non-biodiversity recreational platform for quantitative trait geography.

## Supplementary data-source benchmark products

The YAMAP-versus-public-source comparison should be explicit rather than left as prose.

### Table S1 - Matched 2023-2025 data volume and quality dimensions

Recommended rows/columns:

| Source | 2023 | 2024 | 2025 | photo+geo observations/rows | attached or unique images | quality/identification layer | trait-ready after native export? | key sampling caveat |
|---|---:|---:|---:|---:|---:|---|---|---|
| YAMAP study retrieval | 642 | 687 | 635 | 1,964 | 1,963 unique hashes | exhaustive study author screening + image/coordinate QC | yes, after study pipeline | mountain-route/access/conspicuousness bias |
| iNaturalist | 95 | 156 | 265 | 516 | 882 attached photos | community ID; 472 Research Grade | no petal-level colour natively | purpose-built reporting + access/observer bias |
| GBIF | 69 | 112 | 212 | 393 | 679 media entries | provider-dependent aggregation | no petal-level colour natively | heterogeneous/syndicated records |

Caption must state that 389/393 matched GBIF records are syndicated from iNaturalist, so GBIF and iNaturalist counts are not additive independent samples.

### Figure S1 - Annual matched-period image-record counts

A simple 2023/2024/2025 count plot can show that the YAMAP sampling frame is both larger and more temporally balanced over the study period. Treat this as a sampling/provenance figure, not a biological trend analysis. If attached-photo totals are shown, keep observation counts and image-object counts visibly distinct.

### Table S2 - Data-quality / observation-process matrix

Compare dimensions rather than assigning a single quality score:

- observation purpose;
- taxonomic validation mechanism;
- date/coordinate provenance;
- public geoprivacy or uncertainty where available;
- duplicate/image-hash audit;
- flower/petal-region validation;
- availability of quantitative RGB/CIELAB trait fields;
- mountain-habitat enrichment;
- remaining observer/access biases.

The point is that iNaturalist has a strong native community-identification system, whereas the YAMAP study adds a strong **study-specific trait-validation pipeline**. Neither should be declared universally higher quality.

## Figure 2 communication rule: the conventional layer still closes a real gap

Make the two-part phenotype visible in the broad-scale result. The point is not simply `colour ~ environment`; it is that a national quantitative image phenotype can distinguish:

- **whether pigmentation is expressed**, and
- **how intense colour is among already pigmented flowers**,

while continuous residual spatial structure is represented explicitly.

## What is not a main figure

Five-species Bombus community turnover is Supporting Information even though its matched-background pattern is statistically stronger than the main availability result. It is an unsigned biogeographic boundary correspondence and lacks a direct directional flower-colour mechanism.

Montane/alpine Bombus (*B. beaticola*, *B. consobrinus*, *B. honshuensis*) are Supplement-only. Their apparent pigmentation association disappears in near-equal-elevation comparisons and functions as a guardrail against interpreting shared high-elevation niche geography as a pollinator effect.

Raw-SDM, all-five availability, 10/25-km availability and relaxed transition-threshold results are robustness panels, not additional main hypotheses.

## Figure 3 communication rule

Do not display only the nominal P=0.027 mean contrast. The main panel/caption must also make clear that the 5-km result is magnitude-driven: median Delta is approximately zero/negative, 49.3% of pairs are positive, across-scale q=0.081, and raw-SDM support does not reproduce the effect.

The panel should emphasize the **change of scale**: the broad environmental/spatial template is analysed nationally, but the pollinator hypothesis is asked only at abrupt nearby boundaries rather than by overlaying two national maps.

## Figure 4 communication rule

Do not label the 17 cells "anthropogenic" or "horticultural" anomalies. They are reproducibly defined local departures whose overall frequency is compatible with natural predictive references. Human context is a post-selection follow-up only.

If sampling-frame context is shown, note that mountain-route sampling may compress the urban-rural gradient while roads/trailheads/access can also create observation-opportunity bias.
