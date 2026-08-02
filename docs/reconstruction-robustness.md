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

## Numerical stabilisation of the phenology sampler

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
`scripts/sweep_phenology_stabilisation.sh` walks the ladder upward per fold and
stops at the first value that fold survives, running every attempt in its own
process because the abort is a SIGABRT that kills the R session. The binding
value for the pipeline is the largest of the per-fold minima: any smaller value
leaves at least one fold aborting. `scripts/diagnose_phenology_stabilisation.R`
performs a single attempt, calling the same module functions the pipeline calls,
so the fold it fits is the fold the pipeline fits.

The sweep distinguishes three outcomes, not two. Its first run (30769694286)
reported all forty attempts as `aborted` and exited green, when in fact none of
them had reached the model: the workflow restored the canonical snapshot without
materialising it, so every attempt failed to open the cell table. A grid of
setup failures is indistinguishable from a grid of genuine numerical failures
unless the two are recorded separately, and the more dangerous of the two
directions is the one that happened — measuring nothing while appearing to have
measured everything. The diagnostic therefore exits with status 2 when an
attempt never reached the model, the sweep records that as `setup_error`, stops
immediately rather than filling the grid, and exits non-zero.

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
