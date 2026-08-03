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

## Known limitation: stage 02 does not complete on the reconstruction

The phenology component of the cross-fitted model aborts inside INLA's
`inla.qsample` with a non-positive-definite precision matrix, and the failure is
not deterministic — the same fold and settings succeed on one run and abort on
another. It is tracked separately as an INLA numerical-stability issue and is
not part of this pull request.

Consequently the reconstruction does not currently run to completion, and the
descriptive comparison above cannot be populated end to end. That is stated
here rather than worked around: the freshness gate refuses to emit a comparison
built from artifacts a run did not produce.
