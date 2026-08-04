# The public reconstruction

## What is canonical, and what this is

**The published manuscript analysis is canonical. It is computed on 1,923
observations.** Nothing in this repository re-baselines it.

The **public reconstruction** is a separate, secondary artefact: the same locked
pipeline run on an analysis population rebuilt entirely from `Data_S1.csv` and
the pinned public CHELSA, SoilGrids, WorldClim, WorldPop, MLIT and GBIF sources.
It contains **1,909 observations**. It is not the paper's analysis and is never
described as canonical.

The two differ because the published analysis-input tables no longer exist. They
were never committed, and are absent from the recorded publication commit, from
both published snapshots and from every retained workflow artifact. What was
searched, and what survives, is recorded in `docs/established-inputs.md`. Row
inclusion is decided by `complete.cases()` over extracted covariates, so any
coverage difference in a public raster changes the population.

Because the published inputs are unrecoverable, the published analysis cannot be
re-executed from source. The reconstruction is the closest reproducible
approximation, and its purpose is to make the *method* runnable and traceable —
not to restate the paper's numbers.

## Where the 14 observations go

The complete-case filter is instrumented, so the difference is measured rather
than assumed. `R/environment_spatial.R` writes, before `complete.cases()`:

| file | contents |
|---|---|
| `analysis_completeness_audit.csv` | one row per observation: every required variable's finiteness, the first failing variable, and the included/excluded outcome |
| `analysis_exclusions_by_first_variable.csv` | exclusions grouped by the first variable that failed |
| `analysis_exclusions_by_combination.csv` | exclusions grouped by the full combination of missing variables |

Measured result: **all 54 exclusions are observations whose *Bombus* predictor
(`Bombus_W`) is not finite.** No environmental variable excludes an observation
on its own. The following were each measured and eliminated as explanations:

| candidate | verdict |
|---|---|
| environmental raster coverage (CHELSA, SoilGrids, WorldClim) | eliminated — no observation is excluded by an environmental variable |
| extraction method (bilinear versus nearest) | eliminated — 0 of 54 recovered by nearest; nearest is strictly worse |
| CRS and axis order | eliminated — both surfaces EPSG:4326, verified |
| raster geometry and extent | eliminated — 0 exclusions out of extent |
| bilinear NA propagation | eliminated — 0 exclusions from stencil neighbours |

All 54 are points whose containing cell is NA in the *Bombus* prediction
surfaces. The remaining uncertainty is which prediction surfaces the original
workflow consumed: the reconstruction substitutes the surfaces committed at
`bcceb7c7`, because the ENMeval tuning grid and fitted candidate objects were
never versioned. That substitution is recorded, not resolved.

## The comparison is descriptive

`scripts/compare_reconstruction_to_published.R` writes
`reproducibility/reconstruction_vs_published.csv` and a readable report giving,
per quantity: the published value, the reconstructed value, the difference and
the relative difference.

**It issues no verdict.** There is no `robust`/`differs` column, no pass/fail
and no threshold. The two sides are computed on different observation sets, so a
difference describes that fact; whether it matters is a scientific judgement for
the reader, not something a script can settle from two numbers.

The published reference values are committed under `inputs/published_reference/`
so a run cannot overwrite them. All ten are verified against the MD5s recorded
in the publication lock by `scripts/verify_locked_artifacts.py`, so the
comparison is anchored to the real publication rather than to a later rebuild.

## Stale artifacts cannot be reported as results

A run that stops early leaves the repository's committed publication outputs on
disk — the very files `inputs/published_reference/` was copied from. Reading
those as "reconstructed" would compare a file with itself and report every
difference as zero, which reads as perfect agreement.

Two mechanisms prevent it:

- every read of a reconstructed artifact is gated on the file's mtime against
  `HOTARUBUKURO_RUN_STARTED`; a file this run did not write is refused, and the
  comparison stops rather than reporting;
- `reproducibility/output_manifest.csv` carries `produced_in_run` per file, so a
  hashed file that predates the run is never presented as the run's work.

## Withdrawn: the phenology component of stage 02

The published analysis fitted a fifth cross-fitted component,
`national_environment_year_spde_phenology`, alongside national presence,
national intensity, common-support presence and common-support *Bombus*
presence. It has been withdrawn.

It aborts inside INLA's `inla.qsample` with a non-positive-definite precision
matrix, and the failure is not deterministic: the same fold and settings
complete on one run and abort on another. Two measured interventions did not
fix it. A 5 × 8 `control.inla(diagonal=)` grid produced no value at which all
five folds complete (10 of 39 measured cells survived, with no monotone
structure, and the same cell gave opposite outcomes across runs). Pinning
`num.threads = 1:1` raised completion to 10 of 15, and pinning the BLAS threads
as well changed nothing — but fold 4 completed 0 times out of 6 in both. The
measurement records and the fold-4 conditioning diagnostic are on the
`diagnostic/phenology-inla-instability` branch.

The conditioning diagnostic rules out the collinearity explanation that was
written into earlier comments here: the fold-4 design matrix is full rank (7 of
7), its condition number is 1.36–1.50 across folds, its minimum singular value
is 25–27, and |r| between `median_year_centered` and its quadratic term is at
most 0.022. The instability is numerical in INLA's sampler, not a degenerate
design.

Early flowering was an auxiliary post-selection facet, not part of the main
analysis, so the component is withdrawn rather than patched. Withdrawing it
does not change which cells are local-isolate candidates:

- candidate selection (`v16_presence_scores`, `v20_local_profile`) reads
  presence draws and observed counts only;
- case-control matching (`v18_match_options(cells, presence$latent_mean)`) reads
  presence only;
- every use of the phenology-derived facet occurred strictly after both.

What was withdrawn with it: the `early_phenology_surprise` null-comparison
metric, the `early_predictive_q` / `early_tail_10` / `early_tail_depth`
auxiliary columns and the claims conditioned on them, the early-flowering
series in Figure 4d, and the phenology-specific stabilisation, threading, sweep
and diagnostic workflows.

What was **not** touched: the frozen upstream `early_phenology_surprise_v15`
column in the v15 cell table, which is retained unchanged and unrecomputed but
is no longer used for selection, scoring or any claim; and the presence,
intensity, *Bombus*, local-isolate and human-context analyses.

## The two supplementary analyses

Both run after the candidate set is fixed, and neither contributes to it.

**Direction check (S1).** The main local event is directional: a pigmented cell
among white neighbours. Treating only that direction as the signal builds the
answer into the question. S1 replays the same locked neighbourhood graph and
the same natural null counting a white cell among pigmented neighbours as the
same kind of departure, so the assumed direction can be checked rather than
taken for granted. It selects no candidates and ranks none.

**Flowering-date check (S2).** Each candidate's median observation
day-of-year minus the mean of its environment-similar neighbours under the same
locked graph; negative means the candidate flowers earlier. The matched
non-isolated pigmented controls carry the same statistic, so the candidate
values have a reference. Two neighbour sets are reported and never merged:
every environment-similar neighbour, and the subset sharing the focal cell's
median observation year. A candidate with no same-year neighbour is reported as
NA, never back-filled from the unrestricted set.

S2 fits no model. It is arithmetic on the frozen cell table and the locked
graph; the withdrawn national phenology component is not restored. Flowering
date is used for no candidate selection, no ranking, and no claim in the
manuscript.
