# Analysis map: question → result → evidence

This page answers one practical question: **where does each current-paper result come from?** For the biological story, start at [`paper/README.md`](README.md). For execution, use [`docs/reproduction-guide.md`](../docs/reproduction-guide.md).

## 1. National flower-colour dataset

**Question:** Can a hiking-photo archive be turned into a quantitative national flower-colour dataset?

**Answer:** Yes. The final environmental analysis contains 1,922 observations: 966 white-like and 956 pigmented.

**Construction:** author screening → flower/petal validation → image-hash audit → fixed RGB/CIELAB extraction → white/pigmented state + pigmented-only intensity.

**Main evidence:**

- `Data_S1.csv`
- `Code_S1.py`
- `source_build/build_data_s1.py`
- `source_build/extract_color.py`
- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md`
- `submission/jbi/supporting/Appendix_S2_image_phenotyping.md`

**Interpretation limit:** this is a reproducible visible-colour phenotype, not calibrated spectroscopy, anthocyanin chemistry or unbiased occurrence sampling.

## 2. Broad environment and continuous residual geography

**Question:** Do pigmentation state and colour intensity follow the same broad geography?

**Answer:** No.

- Pigmentation is less likely toward warmer Temperature PC1.
- Among pigmented flowers, intensity depends on Temperature PC1 × temperature seasonality and is lower toward wetter and more rugged conditions.
- Continuous spatial structure remains after measured environment.

**Main evidence:**

- `scripts/run_environment_interaction_inla_screen.R`
- `scripts/run_broad_environment_spatial_audit.R`
- `.github/workflows/environment-interaction-inla-screen.yml`
- `.github/workflows/broad-environment-spatial-audit.yml`
- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`

**Key spatial results:** residual range 132.76 km for pigmentation state and 65.72 km for conditional intensity.

**Interpretation limit:** the environmental pattern is a candidate physiological/adaptive context, not proof of local adaptation. The spatial field is unresolved geography, not measured genetic structure or dispersal distance.

### 2a. Cross-fitted space-only sensitivity merged in PR #50

**Question:** At comparable geographical separation, does environmental difference align with phenotype divergence beyond an intercept + Matérn SPDE expectation?

**Answer:** For pigmentation state, yes under the predefined one-sided posterior-predictive test; for conditional intensity, no.

| Response | Observed contrast | Space-null median | Excess | One-sided P |
|---|---:|---:|---:|---:|
| Pigmentation state | 0.106802 | 0.058240 | +0.048562 | 0.03393 |
| Conditional intensity | -0.047179 | -0.001287 | -0.045891 | 0.87226 |

**Design:** five geographical folds; each test pair lies wholly within a held-out fold; five geographical-distance strata per fold; high versus low environmental-distance quartiles; 500 posterior-predictive realizations; seed 20260725.

**Evidence and execution:**

- `scripts/fit_broad_space_null_phenotype_excess.R`
- `scripts/run_broad_space_null_phenotype_excess_pipeline.R`
- `.github/workflows/broad-spatial-inertia-environment-tracking.yml`
- `docs/broad_spatial_inertia_environment_tracking.md`
- canonical stage: `run_broad_space_null_excess` in `config/paper_pipeline.lock.json`

**Frozen input:** Broad artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

**Interpretation limit:** this is an FST/PST-inspired but non-genetic spatial-null comparison. It does not establish selection, local adaptation, drift or a unique causal environmental mechanism. It supports the state/intensity distinction and remains a supporting Broad sensitivity rather than replacing the current JBI observation-level model.

## 3. Local focal-Bombus test — primary biotic story after PR #51

**Question:** At nearby white-pigmented boundaries, is focal-bumblebee habitat support higher on the pigmented side?

**Answer:** Weakly on average at the finest replicated scale, but not consistently across pairs or sensitivities.

**Primary design:** 67 pure, non-overlapping transitions within 5 km. Pair identities are fixed before Bombus values or final contrast direction are read. Five kilometres is the finest predeclared replicated comparison scale, not an exact bumblebee foraging distance.

**Main result:**

- mean pigmented-minus-white contrast +0.03590;
- median -0.00277;
- 49.3% positive pairs;
- one-sided P=0.02716;
- q=0.08148 across the 5/10/25-km family;
- attenuation at 10 and 25 km;
- no persuasive relationship for pigmented-only intensity;
- raw SDM support does not reproduce the 5-km result.

**Main evidence:**

- `source_build/build_bombus_sdm_mainland.R`
- `scripts/build_bombus_occurrence_reference_support.R`
- `scripts/run_bombus_local_sharp_transition.R`
- `analysis_sensitivity/audit_bombus_final8_environment_distance.R`
- `submission/jbi/supporting/Appendix_S4_bombus_sdm_occurrence_support.md`
- `submission/jbi/supporting/Appendix_S5_local_pollinator_robustness.md`

**Guardrail after the local result:** apparent high-elevation overlap with montane/alpine Bombus disappears when nearby white and pigmented endpoints are compared at similar elevation. The broad overlap is therefore compatible with shared mountain geography and is not the primary biotic mechanism test.

**Interpretation limit:** SDM support is habitat opportunity, not abundance, visitation, pollen transfer or realized selection. If biological, the weak pattern fits local maintenance/loss of pigmentation state better than progressive darkening.

## 4. Local departures and human context

**Question:** Are pigmented cells surrounded by environmentally similar white cells more common than the natural model predicts?

**Answer:** No.

**Event rule:**

- pigmented focal cell;
- at least three neighbours within 10 km;
- root-mean-square environmental distance ≤1 across the final eight abiotic axes;
- every eligible observed neighbour is white;
- no human variable defines the event.

The same detector is applied to 10,000 predictive maps.

**Main result:**

- 16 observed candidates;
- count P=0.27897;
- candidate-fraction P=0.12609.

**Human follow-up:** population exposure within 5 km gives the largest contrast (+0.06744; directional P=0.00800), but global maxT FWER P=0.05479.

**Main evidence:**

- `R/natural_predictive_model.R`
- `R/candidate_null_tools.R`
- `R/local_pigmented_isolates.R`
- `R/local_human_context.R`
- `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`
- `.github/workflows/human-context-highrep-final.yml`
- `reproducibility/current_broad_human_primary_2026-08-12.md`
- `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`

**Interpretation limit:** the 16 sites are naturally calibrated field/provenance targets, not demonstrated anthropogenic anomalies.

## Why the order matters

1. Flower colour is defined before environment, Bombus or human data are read.
2. Broad environment and space are established before the local Bombus test.
3. The PR #50 space-only sensitivity asks whether state/intensity divergence exceeds fitted spatial continuity without relabelling the spatial field as genetics.
4. Local Bombus pairs are fixed before Bombus values are compared.
5. The PR #51 narrative leads with those local boundaries; the highland overlap enters only as a confounding guardrail.
6. Local departures are fixed before human variables are read.

## Exact evidence identities

- Broad/current cells: run `31258851297`, artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`
- Bombus source build: run `31249841493`, artifact `9020226937`, SHA-256 `d5d639e8e00d1ccc2f887c53fa8041465905b29f6bca1127f816e8c7a649d708`
- occurrence-referenced support: run `31262211605`, artifact `9023137743`, SHA-256 `d04c561b09b652591b9b479f6e26a779bb562c7c1b5f9b14e61d5e7ca8e2794b`
- local sharp-transition test: run `31263324505`, artifact `9023416810`, SHA-256 `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`
- final-eight-axis Bombus balance audit: run `31538548679`, artifact `9119773035`, SHA-256 `51fc846d2f3d815d8bbf3c0b95647eabeb0acf731f1e6dd164c11a0dfe1b896f`
- Bombus community/elevation guardrails: run `31285234317`, artifact `9029595037`, SHA-256 `067dd3408b2a7f046ba263732ffa4cefa2f54a7f1fb672478be46bca425f6bf0`
- local-departure/human replay: run `31537102360`, artifact `9119306089`, SHA-256 `f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4`
- predictive draws: artifact `9094339466`, SHA-256 `413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`
- current PR #51 four-figure bundle: artifact `9291438085`, SHA-256 `51dde9026c4348205c494db3594414c0f099166f8878dc1c82edbb173f9e2848`
- YAMAP benchmark: run/artifact `31289927019` / `9031041034`; provider-overlap audit `31290095532` / `9031085975`

Integrated cross-file validation is run by `.github/workflows/final-paper-analysis.yml`.

Canonical integrated lock: `reproducibility/final_integrated_pipeline_2026-08-12.md`.

Canonical execution and all active locks are in `run_pipeline.py` and `config/paper_pipeline.lock.json`.
