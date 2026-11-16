# Verified raw-to-results replay for the three-part JBI analysis (audited 2026-11-16)

## Scope and main-branch identity

This record distinguishes **analysis code fixed on `main`** from the evidence for a successful **full, raw-source replay**. It does not certify pollinator-mediated selection or horticultural escape.

- Audited `main` analysis commit: `b5f0e473ed64d978b0085cab7083949d2032c1bd`.
- Successful opt-in full replay: [Actions run 42953552095](https://github.com/zuizui0223/hotarubukuro/actions/runs/42953552095), 2026-11-11, branch `codex/full-manuscript-replay`, commit `ce5c79089c31de3a3f2b6479c925624fe375545f`.
- Git comparison: that replay commit is **one commit ahead of the audited main SHA**, changing **only** `.github/workflows/analysis-ci.yml` to upload result files; *no scientific analysis/source-build code differs*.
- Downloadable artifact: [manuscript-replay, ID 13831541196](https://github.com/zuizui0223/hotarubukuro/actions/runs/42953552095/artifacts/13831541196). Verified ZIP SHA-256: `e58eed4c3fca4ea89eb795029b5249958b366b008a02ad431c14f675758d66e2`. The ZIP contained **198 paths** including source/analysis manifests and Part 1–3 CSV outputs.

## Source reconstruction and command chain

Source: [Zenodo 22334596](https://zenodo.org/records/22334596), `Supplementary_Table_S1.xlsx`.

- Workbook MD5 expected and observed: `a923616e45f10f24a5463eefd09b06dd`.
- Rebuilt lean colour-input table: **1,965 rows, 38 columns**; SHA-256 `9e543b64a824aff82dbb55da1bca8843fb337a51399bfd60ad0a09c9bca3c33c`, exactly equal to `reproducibility/source_contract.json`. The source-contract audit explicitly reported `exact_analysis_contract: true`.
- Ecological phenotype cohort: **1,922 photos, 956 recorded as pigmented**; spatial analysis cells: **1,305**, with 1,922 observations in total.
- `analysis_reproduction/run_manifest.json`: `status=SUCCESS`, **all 18 recorded pipeline stages SUCCESS** and `error=null`. Includes source reconstruction, public raster setup, model fitting, local Bombus and community comparison, Broad + spatial null, 10,000 natural maps and continuous colour isolation.

To rerun from zero on Linux, install the declared Python/R/system dependencies following [the canonical guide](../docs/REPRODUCE_FROM_ZENODO.md), then run:

```bash
python source_build/reproduce_from_zenodo.py
python run_pipeline.py reproduce --no-resume
```

The single-command form is `python source_build/reproduce_from_zenodo.py --run-analysis --no-resume-analysis`. The **equivalent two-command route** above was the one exercised by the successful Actions job.

## Verified outputs by manuscript part

| Part | Direct output in artifact | Replayed result | Inference limit |
| --- | --- | --- | --- |
| **1: quantitative colour differentiation, environment + space** | `broad_environment_spatial_audit/*/spatial_fixed_effects.csv`; `broad_supported_term_distance/primary_supported_term_distance_test.csv` | State: Temperature PC1 mean **-0.542907** (95% CrI **-1.034953 to -0.048683**). Environmental state divergence exceeds space-only expectation, **P=0.011976**. Conditional intensity: Temperature-PC1 × seasonality mean **-0.204730** (CrI **-0.302556 to -0.106867**), but environment-distance excess not supported, **P=0.257485**. | Space-only model captures unresolved geographical structure, not neutral genetic structure. |
| **2: potential Bombus assemblage and colour geography** | `regional_bombus/effects.csv`; `bombus_spatial_guardrail/community_spatial_match_summary.csv`; `bombus_local_boundaries/sharp_transition_summary.csv` | Central Honshu five-species mean **OR=1.632580** (CI 1.100296–2.422364), **P=0.014901**, attenuating with added elevation (**OR=1.291164, P=0.611176**); conditional `B. diversus` in joint model **OR=1.287674**, **P=0.019195**. Five-species occurrence-referenced Hellinger matched excess **+0.032683 at 10 km, P=0.014230** and **+0.041072 at 25 km, P=0.000100**, but at 5 km **P=0.062829**. | SDM-based association, not visitation, frequency-dependent selection, or evidence of maintenance. Selected boundaries and regional models answer different statistical questions. |
| **3: human-associated colour isolation** | `continuous_colour_isolation/natural_map_summary.csv`; `continuous_colour_isolation/isolation_cell_metrics.csv` | Relative same-colour isolation of pigmented cells vs 5-km population rank: **Spearman rho=+0.285498**, upper-tail **P=0.000800** against 10,000 natural maps. Count-conditioned maps: **P=0.003996**. | Human spatial context is supported; horticultural origin, escape, introduction and direct causation are untested. |

The local focal-Bombus five-kilometre directional result is **67 pairs**, mean difference **+0.03590**, one-sided sign-flip **P=0.02716**, but **BH q=0.08148** across three distances; raw cloglog suitability does not confirm it (**P=0.26715**). It must not be presented as a robust pollinator-selection result.

## Numerical drift relative to earlier historical result locks

| Quantity | Historical result document | 2026-11-11 raw replay |
| --- | ---: | ---: |
| State environmental distance beyond space: P | 0.00998 | **0.011976** |
| Intensity environmental distance beyond space: P | 0.26347 | **0.257485** |
| Pigmented relative isolation × 5-km population: upper-tail P | 0.000900 | **0.000800** |

**The conclusions and directions agree; the full model outputs are not bitwise identical to all historical numerical summaries.** Do not silently replace the 2026-08 scientific lock, or claim exact numerical replication of posterior/simulation statistics. The exact source-table hash and all-stage SUCCESS are a stronger claim than these model-level numbers support. Finite Monte Carlo and fitted-model computations can vary; the audit did not isolate the underlying cause of every difference.

## What remains outside main

- The 2026-11-13 exploratory **SDM-only** Bombus sensitivity analyses are on unmerged [PR #72](https://github.com/zuizui0223/hotarubukuro/pull/72). This includes the negative strict-local turnover result and the SDM-only widespread-group coefficient reversal. They must not be described as merged main results.
- The 2026-11-14/16 revised **Word manuscript is not committed to this repository**. `results/` is intentionally ignored; derived CSV, figures and full raster inputs are **not** Git-tracked publication inputs.
- Default `push`/`pull_request` CI validates Python/R contracts, dry-run command graphs and unit tests. It **skips** the opt-in full raw-source job. A green default CI icon is **not** itself evidence of full data reproduction.
- Photo coordinate provenance is inherited from the uploaded workbook and not independently GPS-verified by the raw-source contract. In particular, same-grid-colour records are not a census of actual local morph co-occurrence.

## Audit conclusion

**The three canonical analysis stages currently on main have a successful raw-to-output replay using identical analysis code.** The raw input is exactly reproducible. Results have been reproduced with stable substantive conclusions but not universal bit-for-bit equality. The three mechanistic claims (pollinator-mediated maintenance, positive frequency dependence, horticultural escape) remain hypotheses and should be presented at the evidence level stated above.
