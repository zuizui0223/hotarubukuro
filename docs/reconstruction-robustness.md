# Reconstruction robustness analysis

## What this is

A fully reproducible methodological reconstruction of the analysis, plus new
exploratory analyses that were not part of the manuscript.

It is **not** an attempt to reproduce the published numbers, and it is no longer
trying to be. The historical 1923-observation analysis population depended on
intermediate inputs that were never archived and are currently unrecoverable.
That was investigated exhaustively rather than assumed — see
`docs/established-inputs.md` and the audits below — and the following were
each measured and eliminated as explanations for the difference:

| candidate | verdict |
|---|---|
| environmental raster coverage (CHELSA, SoilGrids, WorldClim) | eliminated — no observation is excluded by an environmental variable |
| extraction method (bilinear versus nearest) | eliminated — 0 of 54 exclusions recovered by nearest; nearest is strictly worse |
| CRS and axis order | eliminated — both surfaces EPSG:4326, verified |
| raster geometry and extent | eliminated — 0 exclusions out of extent |
| bilinear NA propagation | eliminated — 0 exclusions from stencil neighbours |

All 54 exclusions are points whose containing cell is NA in the *Bombus*
prediction surfaces. The one remaining uncertainty is which prediction surfaces
the original v9/v11 workflow consumed: the reconstruction substitutes the
surfaces committed at `bcceb7c7`, because the ENMeval tuning grid and fitted
candidate objects were never versioned. That substitution is recorded, not
resolved.

## The canonical analysis population

The **1909-observation reconstruction is the canonical analysis population** for
this work. It is rebuilt end to end from `Data_S1.csv` and the pinned public
CHELSA, SoilGrids, WorldClim, WorldPop, MLIT and GBIF sources, with every input
checksummed in an immutable Release snapshot. Nothing about it is provisional or
a fallback: it is the population every analysis here is defined on.

The published analysis is retained as **context**. The comparison asks whether
the qualitative conclusions remain similar, not whether every published number
is identical. Identity is neither expected nor sought.

## The new biological question

The manuscript's local analysis concerned pigmented isolates — pigmented cells
among white neighbours. This work generalises that to **bidirectional local
colour-state discordance**, using exactly the same neighbourhood definition and
the same natural null model:

- a **pigmented** focal cell among **white** neighbours;
- a **white** focal cell among **pigmented** neighbours.

Both directions are departures from the locally expected colour state. Treating
only one of them as the signal builds a directional assumption into the
question.

The hypothesis under test is therefore:

> Human-associated landscapes may increase local departures from the naturally
> expected flower-colour state, rather than preferentially causing
> white-to-pigmented transitions.

This is directionally agnostic by construction. The human-context analyses ask
whether anthropogenic landscape measures are associated with discordance *in
either direction*, so a result that is symmetric and a result that is
one-directional are distinguishable rather than conflated.

These analyses are exploratory and were not pre-specified in the manuscript.
They are reported as such.

## How the frozen audits are handled

The pipeline's frozen upstream audits exist to confirm that a run is operating
on the published inputs. Under this analysis they are not, by design. Rather
than delete the audits or fake a pass, the pipeline runs them with
`--baseline reconstruction`:

| check | published mode | reconstruction mode |
|---|---|---|
| `analysis_n` | must equal 1923 | `not_applicable`, records observed vs published |
| `species_warning_isolated` | must be exactly 2 rows on `bee_consobrinus_ns` fold 5 | re-expressed as `species_warnings_remain_isolated`: warnings absent, or confined to one predictor and one fold |
| `population_grain` | must equal 1307 cells | `not_applicable`, records observed vs published |
| every other check | enforced | enforced, unchanged |

Eight of the ten phenotype checks are dataset-independent — the binary response,
the conditional-intensity domain, the measurement-count consistency, the twelve
INLA models with finite WAIC, the cross-fitted residual completeness — and all
of them are enforced in both modes. `not_applicable` is reported as its own
state and is never counted as a pass.

The re-expressed warning check deserves naming explicitly, because it is the one
place where a check's *form* changed. The published check asserted a specific
historical outcome. The scientific requirement behind it is that convergence
trouble stays isolated rather than becoming widespread, since a warning in many
folds would undermine the cross-fitted predictions the later stages rest on.
That requirement is what the reconstruction mode enforces.

## Numerical stabilisation of the phenology sampler — measured, and it does not work

**Result: diagonal stabilisation alone is insufficient, and the abort is not
deterministic.** No value is in use; `--phenology-diagonal` defaults to `0`.

The full grid, five folds by eight values, measured one CI job per cell at the
analysis draw count of 1000 (run 30793913721). `OK` is a completed fit with
1000 posterior draws; `.` is the SIGABRT in `inla.qsample`; `?` is the one cell
lost to a runner shutdown.

| fold | 0 | 1e-8 | 1e-7 | 1e-6 | 1e-5 | 1e-4 | 1e-3 | 1e-2 |
|---|---|---|---|---|---|---|---|---|
| 1 | . | OK | . | . | OK | . | . | . |
| 2 | OK | . | OK | . | . | . | . | . |
| 3 | OK | . | . | OK | . | OK | ? | . |
| 4 | . | OK | . | . | . | . | OK | . |
| 5 | . | . | OK | . | . | . | . | . |

Three things follow, and none of them is "use a bigger value".

**No value survives every fold.** All eight columns contain at least one
confirmed abort. The single missing cell cannot change that: its column,
`1e-3`, already holds three confirmed aborts. So the selection rule returns
nothing, and it would return nothing even from a complete grid.

**The diagonal has no systematic effect.** Survival is 10 of 39 cells, 25.6%,
scattered with no structure along the value axis. `1e-2`, the largest value
tried, is the *only* column with no survivors at all. Nothing here behaves like
a stabilisation parameter.

**The outcome is not reproducible.** Fold 1 at `1e-7` survived in 37 s in run
30793653311 and aborted in 64 s in run 30793913721 — same fold, same value, same
draw count, same sampler seed, same code. Whether the Cholesky succeeds is
therefore not a function of `(fold, diagonal)`.

That last point is what makes retrying useless rather than merely slow. A
canonical run needs all five folds to complete in one pass. At the observed
per-cell rate that is roughly 0.26<sup>5</sup> ≈ 0.1% per attempt.

The three pipeline observations all agree with the grid — fold 1 aborts at `0`,
survives `1e-8`, aborts at `1e-7` — which is evidence that the standalone
diagnostic faithfully reproduces the in-pipeline fit. Given the non-determinism,
that agreement is partly luck, and it is reported as consistency rather than as
confirmation.

### Pinning INLA's thread count

The non-determinism above is what the second setting addresses.
`--phenology-num-threads` pins INLA's thread count for the phenology
**inference** stage; both reconstruction drivers set it explicitly to `1:1`,
and it is empty — INLA's default — everywhere else. The sampling call was
already single-threaded (`inla.posterior.sample(..., num.threads = 1)`); the
fit was not, and the fit is what determines which hyperparameter
configurations get stored for `inla.qsample` to factorise.

It is a reproducibility setting, not a scientific one. No formula, prior,
likelihood, spatial fold, draw count, seed, diagonal, threshold or downstream
definition changes, and the presence, intensity and both common-support
components keep INLA's default threading.

**Succeeding once would not settle it.** Default threading already succeeded
about a quarter of the time by chance, so a single green run is not evidence.
`.github/workflows/phenology-threading-check.yml` therefore runs every fold
three times and `scripts/summarise_phenology_threading.sh` requires two
distinct things:

| requirement | how it is tested | failure means |
|---|---|---|
| completion | every fold completes in every repeat | exit 3 — threading was not sufficient |
| determinism | every repeat of a fold produced the *same* posterior draw range | exit 5 — completion is reproducible, the draws are not |

An incomplete replication exits 4 and supports no conclusion at all. Requiring
identical draw ranges under a fixed seed is the stronger test: completion alone
would still permit run-to-run variation.

If completion fails under one thread, that is the answer — threading was not
sufficient — and not a cue to start changing the model.

Where the setting is recorded: `inla_num_threads` per fold in
`predictive_replication_model_log.csv`, `phenology_inla_num_threads` per run in
`predictive_replication_component_scope.csv`, `run_mode.txt` and
`run_provenance.txt`, and the `02_run_natural_predictive_model` row of the stage
registry. A checkpoint fitted under different threading is refitted rather than
reused, for the same reason one fitted under a different diagonal is.

### What this rules out, and what it leaves

Ruled out: that the phenology component fails for want of a larger diagonal.
Measured across four orders of magnitude, it does not.

Left open: why the same fit succeeds or fails across runs. The INLA inference
stage runs multi-threaded — its own logs report "Efficiency using 4 threads" —
so the set of stored hyperparameter configurations can differ between runs even
under a fixed seed, and `inla.qsample` factorises whichever configurations were
stored. That would make the failure a property of thread scheduling rather than
of the model. It is a hypothesis, not a finding; testing it means changing
`num.threads`, which is a model-configuration change beyond the stabilisation
that was authorised, so it has not been done.

## Numerical stabilisation of the phenology sampler: mechanism and provenance

One deviation from the locked configuration is required to make the
reconstruction complete, and it is recorded rather than absorbed.

On the reconstruction, the phenology component of stage 02 aborts in fold 1.
`inla.qsample` reports `Matrix is not (numerical) positive definite` from
`GMRFLib_init_problem` and terminates the INLA process with SIGABRT before a
single posterior draw is written, so the stage produces nothing at all. The
phenology model is the only one carrying `median_year_centered` and its square
alongside the SPDE field, and on this cell set that near-collinear pair leaves
the joint precision matrix numerically singular.

The pipeline therefore accepts `--phenology-diagonal`, which is passed to
`control.inla(diagonal=)` **for that one component**. It adds a constant to the
diagonal of the precision matrix so the Cholesky factorisation stays defined.

What this is:

- a property of the solver, not of the model;
- applied to the phenology component only — the presence, intensity and both
  common-support components are fitted exactly as before;
- escalated from the smallest value upward, and only on a measured failure.

The escalation record, so the choice is evidence rather than preference:

| value | outcome |
|---|---|
| `0` | aborts at phenology fold 1 (run 30755431516) |
| `1e-8` | aborts at fold 2, sampler seed 20462725 — fold 1 survived (run 30769056060) |
| `1e-7` | aborts at fold 1, sampler seed 20461725 (run 30769361233) |

Each abort is identified by the sampler seed the failing `inla.qsample`
subprocess reports, which is `20260725 + 200000 + 1000 × fold`. The fold loop
echoes that seed and prints a per-fold completion line, so which fold survived
is read off the log rather than reconstructed.

Those three rows say something the first two on their own did not: **the
response is not monotone in the value.** A larger diagonal moved the failure
back to an earlier fold. So the value cannot be chosen by rerunning the whole
pipeline one guess at a time — that measures one cell of the table per twelve
minutes and, worse, invites reading "got further" as "closer to correct".

The value is therefore chosen from a measurement over the whole grid.
`scripts/sweep_phenology_stabilisation.sh` measures the whole grid — every fold
at every value — running each attempt in its own process because the abort is a
SIGABRT that kills the R session. `scripts/diagnose_phenology_stabilisation.R`
performs a single attempt, calling the same module functions the pipeline calls,
so the fold it fits is the fold the pipeline fits.

Two properties of the sweep were learned by getting them wrong, and both are
worth stating because both produced confident-looking numbers that were not
measurements of the model.

**It must record setup failures separately.** The first run (30769694286)
reported all forty attempts as `aborted` and exited green. None had reached the
model: the workflow restored the canonical snapshot without materialising it, so
every attempt failed to open the cell table in four seconds — against the forty
a real fit takes. A grid of setup failures is indistinguishable from a grid of
genuine numerical failures unless the two are recorded apart, and the dangerous
direction is the one that occurred: measuring nothing while appearing to have
measured everything. The diagnostic now exits with status 2 when an attempt
never reached the model, and the sweep records `setup_error`, stops rather than
filling the grid, and exits non-zero.

**It must use the analysis draw count.** The second run (30770591247) swept at
20 draws on the reasoning that the factorisation fails once regardless of how
many draws are requested. That reasoning is wrong.
`inla.posterior.sample` allocates draws across the stored hyperparameter
configurations by weight and calls `inla.qsample` once per configuration. The
failing call in run 30769361233 carried a count of 4 out of 1000 — a
configuration holding about 0.4% of the posterior weight, whose expected
allocation at 20 draws is 0.08. At a reduced draw count such a configuration is
never visited, its precision matrix is never factorised, and an indefinite Q
goes undetected. The 20-draw grid duly recorded fold 2 as surviving at diagonal
`0`, while the pipeline had aborted at fold 2 with a *larger* diagonal. The
draw count is now pinned to the analysis value and a smaller one is refused.

**The grid is measured one CI job per cell.** It was first run as a single job
of 40 sequential attempts, and that job was lost to runner failure twice — run
30771243504 at 17 minutes, run 30772489489 at 70 — taking every measurement
with it both times. Streaming rows into the step summary was tried as a
mitigation and does not work: the summary is published when a *step* completes,
and the step never completed, so both runs ended with an empty summary, no
artifact, and 404 logs. A matrix job per cell makes a lost runner cost one cell
instead of forty, and drops wall-clock time from over an hour to about the cost
of one attempt. `scripts/summarise_phenology_sweep.sh` assembles the per-cell
results and applies the selection rule.

**No value may be adopted from a partial grid.** The selection rule is *the
smallest value for which all five folds complete*, and a truncated grid cannot
answer that: a value whose measured cells all survived may still have an
unmeasured fold that aborts, and a smaller usable value may simply not have been
reached yet. Truncation is a real mode — run 30771243504 lost its runner
mid-sweep — so the sweep counts its rows against folds × values and exits 4 if
they disagree, rather than leaving the judgement to whoever reads the table. The
ladder is fixed and is not extended past the measured range in search of a
value; if none of the measured values survives every fold, the conclusion is
that diagonal stabilisation alone is insufficient.

The sweep's exit status carries the outcome: `0` complete grid with a usable
value, `2` nothing reached the model, `3` complete grid with no usable value,
`4` partial grid.

**It does not assume monotonicity.** An earlier version stopped at the first
value each fold survived. The pipeline pair above rules that out: a larger
diagonal changes the fitted hyperparameter configurations themselves, so it
changes which precision matrices are factorised rather than simply conditioning
all of them better. The sweep therefore reports, per value, how many folds
survived it, and the answer is the smallest value whose whole column survived —
which is also exactly what the pipeline needs, since it must run every fold with
one value.

What it does not change: no formula, prior, likelihood, spatial fold, draw
count, seed, neighbourhood definition or threshold. The default is `0`, so the
published mode is bit-for-bit the locked configuration.

Where the value is recorded, so a stabilised fit can never be mistaken for an
unstabilised one:

| record | field |
|---|---|
| `reproducibility/pipeline_stage_registry.csv` | the `invariant` column of `02_run_natural_predictive_model` |
| `results/ecological_v16_predictive_replication/predictive_replication_model_log.csv` | `inla_diagonal`, per component and per fold |
| `results/ecological_v16_predictive_replication/predictive_replication_component_scope.csv` | `phenology_inla_diagonal` |
| the fitted checkpoint `.rds` | `inla_diagonal` on the result object |
| `reproduction_status/run_provenance.txt` | `phenology_inla_diagonal` |
| `reproducibility/reproduction_summary.md` | a *Numerical stabilisation* section, read back from the model log |
| `predictive_replication_audit.csv` | the `INLA_numerical_stabilisation` row |

The audit reports it as a `RESULT` rather than scoring it, because the correct
value is not something an audit can assert — but a reader must never have to
guess whether it was used.

## Getting to one complete run

The objective is a single end-to-end execution on GitHub Actions from
reproducible public inputs — not agreement with the published numbers. The
comparison is qualitative context and runs afterwards.

The pipeline is a serial chain and every stage after 02 consumes stage 02's
cross-fitted checkpoints, so while stage 02 is blocked nothing downstream can be
validated at all: stages 03 through 08 have never executed on the reconstruction
population. Discovering their failures one per run would cost a CI cycle each.

A **survey run** therefore exists, purely as a development diagnostic: it
records a failing stage and continues, so one run enumerates the whole queue.

It is kept structurally separate from the canonical reconstruction rather than
being a flag on it, because "off by default" would still leave the canonical
workflow one input away from producing a tolerated-failure run:

| | canonical | survey |
|---|---|---|
| driver | `scripts/run_reconstruction_analysis.sh` | `scripts/survey_reconstruction_pipeline.sh` |
| workflow | `reconstruction-analysis.yml` | `reconstruction-survey.yml` |
| trigger label | `run-reconstruction-analysis` | `run-reconstruction-survey` |
| every stage must pass in sequence | **yes** | no |
| comparison against the manuscript | yes | **never runs** |
| figures, reproducibility report | yes | **never written** |
| artifact name | `reconstruction-analysis-…` | `reconstruction-survey-DIAGNOSTIC-…` |

The canonical workflow has no `continue_on_failure` input at all and its driver
passes `false` unconditionally, so **the reference reconstruction cannot come
from a run that tolerated a failing stage.**

Three independent mechanisms enforce this, so no single mistake collapses it:

1. the two workflows and drivers are distinct, with distinct trigger labels and
   artifact names;
2. `run_publication_pipeline.R` stamps `run_mode.txt` in the output directory
   with `canonical` or `survey`, before the run starts and unconditionally, so a
   canonical run in a reused workspace overwrites a stale survey stamp rather
   than inheriting it;
3. `compare_reconstruction_to_published.R` reads that stamp and refuses outright
   to compare a directory marked `survey`.

The third matters on its own. The freshness gate catches a run whose stages
produced nothing, but a survey run can fail a *late* stage while every artifact
the comparison reads was regenerated — freshness alone would let that through.
The run mode is checked directly for that reason.

A survey run is never green either: each stage keeps its own `PASS` or `FAIL` in
`final_stage_manifest.csv`, and the run exits non-zero if anything failed.

**The reference reconstruction is a canonical run in which every stage passed in
sequence.** Once the pipeline is stable, that is the run to cite, and the survey
path plays no part in it.

## The discordance diagnostic

On top of the rebuilt pipeline, this analysis runs the bidirectional local
colour-state discordance diagnostic (`v23`, implemented in
`R/local_state_asymmetry.R`; "asymmetry" in the code identifiers and
"discordance" in prose refer to the same thing).

It reuses the locked primary local-isolate graph and asks whether a pigmented
cell surrounded by white neighbours is more common than a white cell surrounded
by pigmented neighbours, beyond the discordance that the fitted natural
environment-plus-SPDE baseline and the observed sampling design already produce.
The comparison is against 1000 cross-fitted natural predictive maps drawn from
the same posterior the locked pipeline produces.

It is a symmetry diagnostic, not a pre-specified test, and it has no published
counterpart — the comparison reports it as `new_analysis` rather than scoring it
against a number that does not exist.

## What is compared

`scripts/compare_reconstruction_to_published.R` writes
`reproducibility/reconstruction_vs_published.csv` and a readable report, in five
sections:

1. **Sample size** — observation and 1-km cell counts per model.
2. **Environmental model** — cross-fitted AUC and RMSE per component, and the
   national *Bombus* AUC gain.
3. **Local *Bombus*** — the 25-km partial turnover slopes and their corrected
   p-values, for both hurdle stages.
4. **Local discordance** — the new diagnostic and its position against the
   natural baseline.
5. **Human context** — isolate counts and their natural-null p-value, the
   population-scale and DID contrasts, and the DID-proximate fraction.

Every row carries two verdicts:

- `agreement` — whether the value is numerically close, on a scale stated per
  quantity. Informative, not decisive.
- `conclusion` — whether the claim the manuscript makes still holds: the sign of
  an effect, which side of a threshold a test lands on, whether discrimination
  stays useful. `robust` or `differs`. **This is the column that matters.**

The published reference values are committed under `inputs/published_reference/`
so they cannot be overwritten by a run. They are copies of the committed
publication outputs, all ten of which `scripts/verify_locked_artifacts.py`
verifies against the MD5s recorded in the publication lock — so the comparison
is anchored to the real publication rather than to a later rebuild.

## Relationship to PR17

PR17 preserves the historical published analysis and the reproducible pipeline
infrastructure. It deliberately does not re-baseline anything onto the
reconstruction, and its `inputs/numerical_reference.csv` and
`inputs/established_input_expectations.csv` still hold the published values.

This analysis changes none of that. It adds a second, clearly labelled mode
alongside it.
