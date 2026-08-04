# Local-isolate candidates are identical before and after the phenology removal

The requirement: removing the phenology component must not change the number of
local-isolate candidates or which cells they are.

## Why this is a proof by construction and not an A/B run

The obvious check — run the pipeline once with phenology and once without, and
diff `local_isolate_candidates.csv` — cannot be performed. The phenology
component never completes. Fold 4 failed in 6 of 6 measured runs, across a
5 × 8 `control.inla(diagonal=)` grid and under single-threaded INLA with and
without BLAS thread pinning (`diagnostic/phenology-inla-instability`). There is
no phenology checkpoint on this population and none exists anywhere in the
repository or the input snapshot, so the "before" side of an A/B run cannot be
produced.

What can be established, and is stronger than a single paired run, is that no
phenology-derived value is readable at any point where a candidate is selected
or matched. That is a property of the code, not of one run's numbers.

## The data flow, by line

Selection and matching in `scripts/run_local_pigmented_isolates.R`, at the
pre-removal line numbers (commit `ab77cad`, so the phenology uses can be cited
in the same numbering):

| step | line | reads |
| --- | --- | --- |
| `observed_q` | 80 | `cells$n_pigmented`, `presence$draws` |
| `observed_z` | 84 | `presence$draws`, observed counts |
| neighbour graph | 99 | `cells` geometry and environmental axes |
| observed local profile | 106 | `cells$n_pigmented`, graph |
| candidate set | 142 | `primary$observed$candidate`, landscape completeness |
| control set | 145 | `primary$observed$present`, neighbour pigment share |
| match options | 153 | `cells`, `presence$latent_mean` |
| match filter | 154 | `primary$graph` |
| matched pairs | 157 | cases, controls, `observed_q`, `observed_z`, ids |
| candidate table and rank | 271 | `cells`, `features`, graph, profile, `observed_q`, `observed_z` |

`candidate_rank` is assigned inside `v20_candidate_table`
(`R/local_pigmented_isolates.R:264-269`) by ordering on
`unexpected_pigmented_q` and `unexpected_pigmented_z`, both presence-only, with
`exact_site_id` as the deterministic tiebreak.

Every read of the phenology-derived facet, in the same numbering:

| use | line | position |
| --- | --- | --- |
| `auxiliary_features$early_tail_depth` via `v19_pair_contrasts` | 165, 206 | after matching (157) |
| `auxiliary_profile$early_tail_10` case/control counts | 242–261 | after matching (157) |
| `early_predictive_q`, `early_tail_depth`, `early_tail_10` merged into the candidate table | 266–278 | after the table and its ranks exist (271) |

The lowest line at which any phenology-derived value is read is 165. The
highest line at which a candidate or a pair is decided is 157, and the
candidate table with its ranks is built at 271 from presence-only inputs. No
read of a phenology value precedes any selection or matching decision.

The merge at line 275 is `all.x = TRUE` on `exact_site_id` into an
already-ordered `candidate_table`, and the subsequent reorder is on
`candidate_rank`, which was assigned before the merge. Adding or removing
merged columns therefore cannot reorder or resize the table.

The same holds in `R/natural_predictive_model.R`. `v16_candidate_null` selected
candidates through `v16_presence_scores(presence_result, cells)`, which reads
`result$draws` and cell counts only. The phenology argument entered exactly one
place: the `mean_early_phenology_surprise` entry of `metric_definition`, a
post-selection facet comparison. Removing it removes rows from
`predictive_replication_candidate_null.csv`; it changes no other row, because
each metric's null comparison is computed independently from the same fixed
candidate sets.

## What the removal does change

- `predictive_replication_candidate_null.csv` loses the
  `mean_early_phenology_surprise` rows. The remaining metrics' `empirical_p`
  values are unchanged; their `BH_q` values change, because the BH family is
  now smaller. This is the intended consequence of withdrawing a facet, not a
  side effect on candidate identity.
- `local_isolate_candidates.csv` loses the `early_predictive_q`,
  `early_tail_depth` and `early_tail_10` columns. Row count, row order,
  `exact_site_id` values and `candidate_rank` are untouched.
- `local_isolate_auxiliary_facets_summary.csv` and `_null.csv` drop to the
  single `dark_tail_depth` feature. The maxT familywise adjustment is now over
  one feature rather than two.
- `convergence_count` in `v18_profile` is now the count over the
  human-context and pigmented-intensity facets rather than three facets. It is
  not used in selection, is not claimed in the manuscript, and appears only as
  a descriptive column.

## What was not touched

`early_phenology_surprise_v15` in the frozen upstream v15 cell table is
retained unchanged and unrecomputed (`R/multiscale_hotspots.R`). It is no
longer read for candidate selection, scoring, or any claim. The frozen upstream
v11 module (`R/natural_biotic_covariates.R`, `R/environment_spatial.R`) and its
own DOY quantile-regression rungs are a separate published analysis and were
not modified.
