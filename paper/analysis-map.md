# Manuscript-facing analysis map

This file answers one question: **which analyses are actually used by the current paper, in what dependency order, and with what claim ceiling?**

## Final evidence hierarchy

| Layer | Ecological question | Manuscript role | Current primary implementation | Claim ceiling |
|---|---|---|---|---|
| YAMAP/iEcology | Can a recent national quantitative flower-colour dataset be built from non-biodiversity recreational imagery? | cross-cutting data/method contribution | YAMAP benchmark + author-screened `Data_S1` + deterministic colour extraction | complementary observation process; not unbiased or universally superior |
| Main 1 — Broad natural template | What broad environment + unresolved geography organize pigmentation state and pigmented-only intensity? | MAIN | final response-specific INLA-SPDE models using eight abiotic axes; state additive, intensity includes supported Temperature PC1 × temperature-seasonality interaction | broad geographical structure; not causal variance partition |
| Main 2 — focal Bombus boundary test | Do the sharpest nearby white-pigmented boundaries align directionally with availability of documented focal bumblebees? | MAIN | fresh five-species SDMs -> occurrence-reference calibration -> Bombus-blind 5-km pure non-overlap transitions -> signed *B. ardens*/*B. diversus* contrast | weak/local correspondence; not pollinator-mediated selection |
| Main 3 — local departure + human context | Which local pigmented configurations remain discordant within the finalized natural state space, and what human context do they occupy? | MAIN | final-eight-axis cross-fitted state maps + final-eight-axis RMS local matching + 10,000-map event replay + post-selection global-maxT human family | local/provenance field targets; near-threshold settlement exposure; no anthropogenic-origin claim |
| Bombus community turnover | Do colour boundaries also coincide with broader predicted Bombus-community boundaries? | SUPPORTING | five-species matched Hellinger turnover | unsigned biogeographic correspondence only |
| montane/alpine Bombus | Is visually strong high-elevation Bombus overlap an additional colour mechanism? | SUPPORTING / negative guardrail | near-equal-elevation transition comparison | no additional effect beyond shared elevational geography |

## Dependency logic and anti-circularity

The final manuscript is a directed analysis chain rather than one omnibus regression.

1. **Phenotype construction is upstream of every ecological predictor.** The white/pigmented boundary and conditional intensity are constructed without geography, Bombus or human variables.
2. **Broad environment + space establishes the natural national template.** Environmental expansion, interactions and spatial alternatives are promoted only with the stated posterior, collinearity and geographically blocked-transfer guardrails.
3. **Bombus is tested at a different comparison scale.** SDMs are built from Bombus occurrence/environment data without flower colour. Occurrence-reference calibration is species-specific and flower-colour-blind. Sharp transition pairs are selected without Bombus or environmental values and oriented only after pair identities are fixed.
4. **Current-Broad environmental distance is only a Bombus balance diagnostic.** It does not select or weight the 67 focal pairs and therefore cannot create the Bombus contrast. The finalized eight-axis diagnostic confirms that fixed transitions are environmentally closer than local graph edges overall.
5. **Human variables enter last.** Local-departure candidates are defined in the finalized eight-axis natural state space and replayed on 10,000 natural maps before any population, DID, road, built-land or interface feature is evaluated. All human features are then assessed in one global maxT family.

This separation is intentional: the paper does not claim statistical partitioning of independent abiotic, biotic, historical and human effects.

## Current implementation by layer

### YAMAP / phenotype

- `Data_S1.csv`
- `Code_S1.py`
- `source_build/build_data_s1.py`
- `source_build/extract_color.py`
- `.github/workflows/yamap-public-database-benchmark.yml`
- `.github/workflows/yamap-public-database-overlap-audit.yml`

### Main 1 — finalized Broad

- `scripts/run_environment_interaction_inla_screen.R`
- `.github/workflows/environment-interaction-inla-screen.yml`
- `scripts/run_broad_environment_spatial_audit.R`
- `analysis_sensitivity/run_broad_environment_spatial_audit_wrapper.R`
- `.github/workflows/broad-environment-spatial-audit.yml`
- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `reproducibility/broad_environment_spatial_final_fixed_effects_2026-08-11.csv`
- `reproducibility/broad_environment_spatial_final_hyperparameters_2026-08-11.csv`
- `reproducibility/broad_environment_variable_evidence_registry_2026-08-11.csv`

Primary pigmentation state: eight abiotic axes + East/West + stationary SPDE, no promoted interaction. Primary conditional intensity: the same measured abiotic framework with the retained Temperature PC1 × temperature-seasonality interaction and stationary SPDE. VIF is diagnostic rather than a universal hard deletion threshold.

The frozen current-input/cell artifact used by these audits was generated earlier from the curated source boundary. Its former orchestration script also generated the superseded four-PC/17-candidate downstream branch; that orchestration has therefore been moved to `legacy/reproducibility-development/superseded-current-input-anomaly-pipeline-2026-08-12/`. The artifact remains an input provenance object, not a current inferential pipeline.

### Main 2 — focal Bombus

- `config/bombus_sdm.yml`
- `source_build/build_bombus_sdm_mainland.R`
- `scripts/build_bombus_occurrence_reference_support.R`
- `.github/workflows/bombus-occurrence-reference-support.yml`
- `scripts/run_bombus_local_sharp_transition.R`
- `.github/workflows/bombus-local-sharp-transition.yml`
- `analysis_sensitivity/audit_bombus_final8_environment_distance.R`
- `.github/workflows/bombus-final8-environment-audit.yml`
- `reproducibility/bombus_occurrence_reference_support_2026-08-09.md`
- `reproducibility/bombus_local_sharp_transition_current_results_2026-08-09.md`

Primary directional exposure is `max(A_ardens, A_diversus)`, where each A is occurrence-referenced habitat support. The 5-km pure-transition test has 67 fixed non-overlapping pairs. Current-Broad environmental similarity is a post-selection diagnostic only.

### Supporting Bombus biogeography

- `scripts/run_bombus_spatial_replication_test.R`
- `.github/workflows/bombus-spatial-replication-test.yml`
- `reproducibility/bombus_spatial_replication_test_results_2026-08-09.md`

Community turnover is not used to strengthen the signed focal-Bombus test. Montane/alpine support is a negative guardrail.

### Main 3 — current-Broad local departure and human context

- `R/natural_predictive_model.R`
- `R/candidate_null_tools.R`
- `R/local_pigmented_isolates.R`
- `R/local_human_context.R`
- `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`
- `.github/workflows/human-context-highrep-final.yml`
- `reproducibility/current_broad_human_primary_2026-08-12.md`
- `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`

Primary matching uses standardized RMS distance <=1 across the same eight measured abiotic axes as the finalized pigmentation-state Broad analysis, within 10 km and with at least three neighbours. The historical four-PC broad/within representation is sensitivity provenance only and its executable implementation now lives under `legacy/`.

### Integration / submission lock

- `.github/workflows/final-paper-analysis.yml`
- `reproducibility/final_integrated_pipeline_2026-08-12.md`
- `FINAL_PIPELINE_SUPP_MANUSCRIPT_AUDIT_2026-08-12.md`

The integration workflow restores checksum-locked Broad, Bombus and current-Broad human evidence and verifies the manuscript/Supp numerical hierarchy. It does not rerun the superseded four-PC/17-candidate downstream branch.

## Current frozen numerical references

### Broad/current cell source reference

- run: `31258851297`
- artifact: `9022276431`
- artifact SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`

This artifact is the checksum-locked current-input/cell/static-support source used by later audits. Final response-specific Broad model decisions are recorded in the 2026-08-11 Broad final-model reproducibility files. Its historical generator is preserved under `legacy/` because the same old orchestration also contained a superseded downstream inferential branch.

### Fresh Bombus source build

- run: `31249841493`
- artifact: `9020226937`
- artifact SHA-256: `d5d639e8e00d1ccc2f887c53fa8041465905b29f6bca1127f816e8c7a649d708`

### Occurrence-referenced focal-pollinator support

- run: `31262211605`
- artifact: `9023137743`
- artifact SHA-256: `d04c561b09b652591b9b479f6e26a779bb562c7c1b5f9b14e61d5e7ca8e2794b`

The paper uses its occurrence-reference calibration/support table; superseded national environment+SPDE directional refinements are historical.

### Local sharp-transition focal-Bombus test

- run: `31263324505`
- artifact: `9023416810`
- artifact SHA-256: `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`
- focal result: 67 pairs, mean delta +0.03590, one-sided P=0.02716, three-scale BH q=0.08148; median delta -0.00277 and proportion positive 0.493.

### Current-Broad environmental diagnostic for fixed Bombus pairs

- run: `31538548679`
- artifact: `9119773035`
- artifact SHA-256: `51fc846d2f3d815d8bbf3c0b95647eabeb0acf731f1e6dd164c11a0dfe1b896f`
- focal 5-km final8 distance: selected median 0.24408 versus all-local-edge median 0.31752, ratio 0.76871.
- same ordering at 10 km (0.3371 vs 0.4291) and 25 km (0.4355 vs 0.5315).

### Supplementary community-boundary / montane guardrail

- run: `31285234317`
- artifact: `9029595037`
- artifact SHA-256: `067dd3408b2a7f046ba263732ffa4cefa2f54a7f1fb672478be46bca425f6bf0`

### Current-Broad human-context primary

- successful replay run: `31537102360`
- artifact: `9119306089`
- artifact SHA-256: `f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4`
- locked final-eight-axis predictive-draw source artifact: `9094339466`, SHA-256 `413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`
- 16 candidates; count P=0.27897; candidate-fraction upper-tail P=0.12609.
- leading human feature: population within 5 km, contrast +0.06744, directional P=0.00800, global maxT FWER P=0.05479.
- observation-effort and independent-site-support alternatives are null after maxT (0.96320 and 0.75642).

### YAMAP public-source benchmark

- retrieval run/artifact: `31289927019` / `9031041034`
- retrieval SHA-256: `3e53669395cfd926a0942b3488f844720dca2cb97b9ea210627262691e69f31a`
- provider-overlap audit run/artifact: `31290095532` / `9031085975`

## Final manuscript claims

- **Broad:** pigmentation state has a broad cool-climate association plus unresolved regional geography; conditional intensity has a supported thermal-context interaction and additional moisture/terrain associations.
- **Bombus:** predicted availability of documented broad focal bumblebees shows weak, magnitude-driven alignment with a subset of the sharpest nearest colour boundaries. It does not establish visitation, pollen transfer, fitness or pollinator-mediated selection.
- **Bombus community:** colour boundaries show broader predicted community-boundary correspondence, but this is unsigned and environment-derived.
- **Montane Bombus:** apparent high-elevation overlap disappears under near-equal-elevation comparison and is not an additional mechanism.
- **Human:** current-Broad local departures are not excessive under natural predictive maps. Population exposure within 5 km is the leading post-selection feature but remains just above the global familywise threshold; it motivates provenance/field follow-up rather than an anthropogenic-origin claim.

## Infrastructure retained for a current dependency

`inputs/canonical_snapshot.json` and `scripts/canonical_snapshot.sh` remain outside `legacy/` because current human-context replay restores static WorldPop/MLIT/DID support originating from that immutable source bundle. The old 1,909 flower-population identity associated with the snapshot is not current evidence.

## Deliberately excluded from the current paper

The following development ideas or superseded execution paths do not determine current claims:

- the 1,909-observation publication architecture and its population checker;
- the old current-input orchestration that continued into the four-PC/17-candidate anomaly/human branch;
- the old all-five lower-third Bombus limitation gate;
- national environment+SPDE Bombus null tests as a Main mechanism;
- the superseded 10/25/50-km effective-availability refinement built around the old broad natural null;
- relaxation/local-contrast variants superseded by the 5-km sharp-transition design;
- old four-PC human matching as a manuscript primary;
- the old candidate-DOY helper, which was descriptive and fed no current selection or claim;
- older Ecology & Evolution manuscript drafts and figure plans;
- local Bombus turnover implementations superseded by the spatially matched five-species boundary analysis;
- old submission-reference bundles, final registries and one-time manuscript patch scripts/workflows.

Historical analyses remain auditable under `legacy/`. The current inferential lock is `reproducibility/final_integrated_pipeline_2026-08-12.md`.
