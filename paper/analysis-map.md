# Analysis map: question -> result -> evidence

This page answers one practical question: **where does each manuscript result come from?**

For a scientific overview, start at [`paper/README.md`](README.md). For commands and reruns, use [`docs/reproduction-guide.md`](../docs/reproduction-guide.md).

## 1. National flower-colour dataset

**Question:** Can a hiking-photo archive be turned into a quantitative national flower-colour dataset?

**Answer:** Yes. The final environmental analysis contains 1,922 observations: 966 white-like and 956 pigmented.

**What we built:** author screening -> flower/petal validation -> image-hash audit -> fixed RGB/CIELAB extraction -> white/pigmented state + pigmented-only intensity.

**Main evidence:**

- `Data_S1.csv`
- `Code_S1.py`
- `source_build/build_data_s1.py`
- `source_build/extract_color.py`
- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md`
- `submission/jbi/supporting/Appendix_S2_image_phenotyping.md`

**Interpretation limit:** this is a reproducible visible-colour phenotype. It is not calibrated spectroscopy, anthocyanin chemistry or unbiased occurrence sampling.

## 2. Broad environment and remaining spatial structure

**Question:** Do pigmentation state and colour intensity follow the same geography?

**Answer:** No.

- Pigmentation is less likely toward warmer Temperature PC1.
- Among pigmented flowers, intensity depends on Temperature PC1 × temperature seasonality and is lower toward wetter and more rugged conditions.
- Strong spatial structure remains after measured environment.

**Main evidence:**

- `scripts/run_environment_interaction_inla_screen.R`
- `scripts/run_broad_environment_spatial_audit.R`
- `.github/workflows/environment-interaction-inla-screen.yml`
- `.github/workflows/broad-environment-spatial-audit.yml`
- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`

**Key spatial results:** residual range 132.76 km for pigmentation state and 65.72 km for conditional intensity.

**Interpretation limit:** the environmental pattern is a candidate physiological/adaptive context, not proof of local adaptation. The spatial field is unresolved geography, not a measured genetic structure or dispersal distance.

## 3. Local focal-Bombus test

**Question:** At nearby white-pigmented boundaries, is focal-bumblebee habitat support higher on the pigmented side?

**Answer:** Weakly on average, but not consistently across pairs or sensitivities.

**Primary design:** 67 pure, non-overlapping transitions within 5 km. Pair selection is done before reading Bombus values or the final contrast direction.

**Main result:**

- mean pigmented-minus-white contrast +0.03590;
- median -0.00277;
- 49.3% positive pairs;
- one-sided P=0.02716;
- q=0.08148 across the 5/10/25-km primary family;
- the signal fades at 10 and 25 km and is not reproduced by raw SDM support.

**Main evidence:**

- `source_build/build_bombus_sdm_mainland.R`
- `scripts/build_bombus_occurrence_reference_support.R`
- `scripts/run_bombus_local_sharp_transition.R`
- `analysis_sensitivity/audit_bombus_final8_environment_distance.R`
- `submission/jbi/supporting/Appendix_S4_bombus_sdm_occurrence_support.md`
- `submission/jbi/supporting/Appendix_S5_local_pollinator_robustness.md`

**Useful negative control:** the apparent high-elevation match with montane Bombus disappears when white and pigmented endpoints are compared at similar elevation.

**Interpretation limit:** SDM support is habitat opportunity, not visitation or selection. If the weak pattern is biological, it fits local maintenance of a pigmented state better than progressive darkening.

## 4. Local departures and human context

**Question:** Are pigmented cells surrounded by environmentally similar white cells more common than the natural model predicts?

**Answer:** No.

**Event rule:**

- pigmented focal cell;
- at least three neighbours within 10 km;
- root-mean-square environmental distance <=1 across the final eight abiotic axes;
- every eligible observed neighbour is white;
- no human variable is used to define the event.

The same event detector is applied to 10,000 predictive maps.

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

**Interpretation limit:** the 16 sites are field/provenance targets. They are not demonstrated anthropogenic anomalies.

## Why the order matters

The analyses are deliberately one-way:

1. flower colour is defined before environment, Bombus or human data are read;
2. broad environment and space are fitted before the local Bombus test;
3. local Bombus pairs are fixed before Bombus values are compared;
4. local departures are defined before human variables are read.

This prevents later hypotheses from defining the observations that are then used to support them.

## Next direct tests

The macroecological results point to four concrete next steps:

- thermal/moisture geography -> common-garden and reciprocal-transplant experiments;
- residual spatial geography -> ancestry, isolation-by-distance and admixture tests;
- local Bombus boundaries -> visitation, stigma contact, pollen deposition and seed set;
- 16 local departures -> vouchers, planting history and genomic provenance.

## Exact evidence IDs

These IDs lock the manuscript-facing evidence. Use them for an exact audit; use the reproduction guide for a full rerun.

- broad/current cells: run `31258851297`, artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`
- Bombus source build: run `31249841493`, artifact `9020226937`, SHA-256 `d5d639e8e00d1ccc2f887c53fa8041465905b29f6bca1127f816e8c7a649d708`
- occurrence-referenced support: run `31262211605`, artifact `9023137743`, SHA-256 `d04c561b09b652591b9b479f6e26a779bb562c7c1b5f9b14e61d5e7ca8e2794b`
- local sharp-transition test: run `31263324505`, artifact `9023416810`, SHA-256 `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`
- final-eight-axis Bombus balance audit: run `31538548679`, artifact `9119773035`, SHA-256 `51fc846d2f3d815d8bbf3c0b95647eabeb0acf731f1e6dd164c11a0dfe1b896f`
- Bombus community/elevation guardrails: run `31285234317`, artifact `9029595037`, SHA-256 `067dd3408b2a7f046ba263732ffa4cefa2f54a7f1fb672478be46bca425f6bf0`
- local-departure/human replay: run `31537102360`, artifact `9119306089`, SHA-256 `f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4`
- predictive draws: artifact `9094339466`, SHA-256 `413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`
- YAMAP benchmark: run/artifact `31289927019` / `9031041034`; provider-overlap audit `31290095532` / `9031085975`

Integrated cross-file validation is run by `.github/workflows/final-paper-analysis.yml`.
