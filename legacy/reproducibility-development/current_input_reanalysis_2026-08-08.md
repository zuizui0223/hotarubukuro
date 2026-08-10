# Current-input full reanalysis — 2026-08-08

This note records the completed rerun requested after replacing the upstream flower/environment reconstruction and Bombus SDMs while retaining the active scientific analysis flow.

## Scientific lock

The rerun did **not** target historical `n = 1923` or `n = 1909`. It retained the active stage order, response definitions, natural environment + spatial model, Bombus limitation threshold grid (`0.10, 0.20, 0.25, 0.33`; primary `0.33`), available threshold (`0.50`), 25-km same-fold local matching, environmental caliper (`0.75`), one-to-one matching, local-isolate definition, natural-null procedure, and human-context analyses.

No Bombus threshold was retuned after seeing the fresh SDM output. The only stage-03 code change was to represent an empty fixed gate as `not estimable` instead of failing while constructing an output table.

## Provenance

- Source flower table: `Data_S1.csv`, 1,965 rows.
- Current analysis-copy SHA-256: `27518baa61dd3a22b59ed2cf5d1790f3588f06cdd1373fbd46c7e1475d4098d7`.
- Exact-image duplicate rule: stable source-row order, retain first identical SHA-256; one extra row is excluded. The original two-row duplicate flag is preserved separately for provenance.
- Historical upstream implementation commit used to rebuild the v11/v15 boundary: `1d7c575f1191e4e035db01a617f52b75eeaca313`.
- Fresh seeded Bombus artifact: `9020226937`, SHA-256 `d5d639e8e00d1ccc2f887c53fa8041465905b29f6bca1127f816e8c7a649d708`.
- Fresh-v16 intermediate artifact: `9022017169`, SHA-256 `1167ad8abc635b69964cc4af20b8a7de1b7db9675085ecade625ca725ad3fe3a`.
- Successful downstream workflow run: `31258851297`.
- Successful complete result artifact: `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

## Population flow and phenotype

- Source rows: **1,965**.
- Canonical extra exact-image duplicate: **1**.
- All five fresh Bombus predictions finite before analysis-specific filtering: **1,923 / 1,965**.
- Fresh phenotype analysis population: **1,922**.
- White observations: **966**.
- Pigmented observations: **956**.
- Fresh 1-km analysis cells: **1,305**.
- Response-blind Gaussian-mixture pigmentation boundary on CIELAB `a*`: **4.96878**.

The v11 INLA models completed with no non-finite CPO values. The primary Bombus cross-fit warning count was zero. One held-out warning remains in a residual-tail sensitivity fit; it is retained as a diagnostic rather than used to modify the analysis population or tuning.

## Fresh national natural reference

For the active environment + SPDE reference model:

- Pigmentation presence: trial-weighted AUC **0.86348**; mean negative log predictive mass **0.57105**; Bernoulli Brier score **0.15043**.
- Pigmented-only conditional intensity: RMSE **0.91924**; MAE **0.71472**; 95% predictive coverage **0.94362**.

Bombus is excluded from this stage as in the active design.

## Fresh Bombus SDMs and fixed limitation gate

Fresh ENMeval validation AUC means for the selected models were approximately:

- `Bombus ardens`: **0.7564**.
- `B. diversus`: **0.6009**.
- `B. beaticola`: **0.9129**.
- `B. consobrinus`: **0.8965**.
- `B. honshuensis`: **0.8699**.

At the 1-km flower cells, the unchanged stage-03 exposure `best_bombus_support_rank = max(five within-species ranks)` had:

- minimum **0.488889**;
- median **0.850575**;
- maximum **1.0**.

Consequently, the number of cells satisfying the fixed low-support definition was **0** at every pre-specified threshold `0.10`, `0.20`, `0.25`, and `0.33`. The primary lower-third gate therefore produced **0 matched pairs** and is reported as:

`lower_third_gate_not_estimable_no_fixed_gate_pairs`

This is a result of replacing the SDMs, not a reason to move the gate.

## Local isolate / natural-null stage

The unchanged primary local-isolate definition yielded **17 candidates**.

High-draw cross-fitted natural null (10,000 maps):

- candidate count: observed **17**, null mean **13.614**, one-sided empirical `p = 0.1996`;
- candidate fraction: observed **0.04735**, null mean **0.03427**, one-sided empirical `p = 0.0874`.

Joint full-data posterior predictive sensitivity (10,000 latent draws x 20 observation replicates = 200,000 maps):

- candidate count: observed **17**, null mean **14.879**, `p = 0.3145`;
- candidate fraction: observed **0.04735**, null mean **0.03925**, `p = 0.1962`.

The joint-PPC candidate identity and boundary count both matched the observed 17-candidate definition exactly.

## Human-context follow-up

The broad multivariate local human-landscape departure was not unusual under the natural null (`regularized Mahalanobis` global test `p = 0.9011`).

Short-range population context showed a directional nominal signal at 5 km:

- 5-km population-rank contrast: **+0.05306**, directional `p = 0.02697`, maxT-FWER `p = 0.08991`.

The DID-aligned population composite was similar:

- DID-aligned population score: **+0.05162**, directional `p = 0.02298`, maxT-FWER `p = 0.07592`.

Neither survives the relevant maxT multiplicity correction at 0.05, so these remain exploratory human-context signals rather than evidence of horticultural origin.

## Immediate interpretation

With the fresh upstream reconstruction, the flower-colour natural model is estimable on 1,922 observations and the local-isolate/human-context stages run to completion. The largest substantive change is stage 03: the fresh five-species SDM ensemble does not generate a low-Bombus cell under any of the pre-specified limitation thresholds, so the planned low-versus-available Bombus contrast is not estimable without changing the analysis definition. The local-isolate excess is also weaker than the natural null, while the human-context follow-up shows only short-range nominal signals that do not survive multiplicity correction.
