# Active manuscript products

The active paper is now [`ecology-and-evolution-manuscript-final.md`](ecology-and-evolution-manuscript-final.md), using the fresh **1,922-observation / 1,305-cell** baseline.

The immediately preceding [`ecology-and-evolution-manuscript.md`](ecology-and-evolution-manuscript.md) is retained as provenance but is no longer the narrative source for submission.

## Scientific arc

The paper is designed as a sequence of ecological questions rather than a list of predictors:

0. **YAMAP / iEcology data layer** - repurpose GPS-linked hiking photographs created for recreation, exhaustively screen recovered candidates within the 2023-2025 frame, then apply taxon/flower-region review, hash/coordinate audit and deterministic image phenotyping;
1. **Broad natural template** - two-part quantitative flower-colour phenotype followed by national environment-plus-INLA-SPDE geography;
2. **Fine-scale pollinator hypothesis** - sharp local white/pigmented transitions tested against occurrence-referenced availability of *Bombus ardens* + *B. diversus*;
3. **Event-based anomaly screen** - pigmented cells embedded in environmentally similar white neighbourhoods, replayed on natural predictive maps before post-selection human-context analysis.

This ordering is deliberate. The data source expands the measurable trait geography; the phenotype representation preserves both a white/pigmented state transition and quantitative variation among pigmented flowers; the broad model establishes environmental/spatial geography; the pollinator question changes scale to reduce broad map confounding; and the anomaly question changes the inferential object from residual magnitude to a repeatable ecological event.

Five-species Bombus community turnover and montane/alpine taxa are Supporting Information. Turnover is interpreted as a biogeographic correspondence between colour and predicted pollinator-community boundaries, not as a directional colour mechanism. Montane/alpine associations disappear under near-equal-elevation comparisons and therefore receive no mechanistic main-text interpretation.

The main local Bombus result remains explicitly weak/exploratory. The 5-km pure-transition set contains 67 non-overlapping pairs; the occurrence-referenced mean *B. ardens*/*B. diversus* contrast is positive, but the median/sign proportion, raw-SDM sensitivity and 10/25-km analyses do not show a robust general effect.

The anomaly stage does not use a raw residual threshold. The 17 current candidates are not a robust excess over repeated natural references. Human-context analyses remain post-selection and familywise-inconclusive. The ornamental use of *C. punctata* motivates provenance follow-up but does not convert these candidates into anthropogenic cases.

## YAMAP framing and matched public-database benchmark

YAMAP should be presented as a **complementary iEcology source with an unusually productive mountain sampling frame for this focal species**, not as an unbiased or universally superior alternative to iNaturalist/GBIF. Its distinctive feature is that the original behaviour is hiking documentation rather than biodiversity submission, while public activity/photo locations retain route-linked spatial provenance. This may reduce taxon-reporting/research-purpose-conditioned selection, but it does not remove route/access, conspicuousness or subject-choice bias.

A pre-specified Japan/2023-2025/image+georeference benchmark found:

- YAMAP study retrieval: **1,964** author-screened records, **1,963** unique image hashes, yearly counts **642/687/635**;
- iNaturalist: **516** photo+geo observations, **882** attached photos, **472** Research Grade;
- GBIF: **393** matched human-observation image records, of which **389 (99.0%)** were syndicated from iNaturalist.

Thus YAMAP supplied **3.81x** the matched iNaturalist observation count and **2.23x** the total attached-photo count in the same three-year window. GBIF and iNaturalist are not additive independent pools in this comparison.

The mountain-route frame is useful for Main 1/2 because it enriches natural/semi-natural mountain habitats where wild *C. punctata* populations are plausible; it does not prove every record is wild. The same restriction can compress the urban-rural gradient and reduce power in Main 3, while trailheads/roads can create countervailing access bias.

The study-specific quality layer is equally important: every recovered candidate was visually screened before inclusion, taxonomic look-alikes were removed, the flower/petal ROI was checked, image hashes and coordinates were audited, and images were converted through one deterministic RGB-CIELAB/QC pipeline. `Data_S1` is therefore a curated quantitative-trait product rather than a direct occurrence export.

Benchmark documents:

- [`../reproducibility/yamap_public_database_benchmark_spec_2026-08-09.md`](../reproducibility/yamap_public_database_benchmark_spec_2026-08-09.md)
- [`../reproducibility/yamap_public_database_benchmark_results_2026-08-09.md`](../reproducibility/yamap_public_database_benchmark_results_2026-08-09.md)

See [`../docs/yamap-iecology-rationale.md`](../docs/yamap-iecology-rationale.md) and [`design-logic-and-novelty-ja.md`](design-logic-and-novelty-ja.md) for reviewer-facing logic.

The integration/provenance lock is [`../reproducibility/final_paper_pipeline_2026-08-09.md`](../reproducibility/final_paper_pipeline_2026-08-09.md).

Figures are planned in [`figure-map.md`](figure-map.md), and robustness/community-turnover details are assigned in [`supporting-information-plan.md`](supporting-information-plan.md).
