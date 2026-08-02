# Canonical analysis pipeline DAG

The authoritative stage sequence is `scripts/run_publication_pipeline.R`. This
document and the machine-readable
[`reproducibility/pipeline_stage_registry.csv`](../reproducibility/pipeline_stage_registry.csv)
describe that runner; they do not define a second pipeline. Where the two
disagree, the runner is correct and the registry is a bug.

```text
Data_S1.csv (versioned phenotype record)
  → public raster acquisition (CHELSA, SoilGrids, WorldClim, WorldPop)
  → MLIT land-use rasters
  → environmental predictor extraction and PCA
  → environmental/spatial analysis table
  → Bombus predicted-community inputs
  → two-part flower-colour models
  → 1-km multiscale cell context
  ───────────── canonical analysis-input snapshot boundary ─────────────
  → national natural predictive model with spatial cross-validation   (v16)
  → local Bombus-turnover analysis                                    (v17)
  → human-landscape features                                          (v19)
  → locally discordant pigmentation (pigmented-isolate) definition     (v20)
  → anthropogenic-context characterization                            (v21)
  → DID sensitivity                                                   (v22)
  → publication lock, independent validation, claim audit
  → manuscript tables and figures
```

Everything above the boundary belongs to **workflow B**
(`.github/workflows/raw-data-reconstruction.yml`), which depends on external
public services. Everything below belongs to **workflow A**
(`.github/workflows/canonical-analysis.yml`), which starts from the immutable,
checksummed snapshot and must be reliable on a clean runner.

## Environment (shared by every stage)

| Item | Declaration |
|---|---|
| R | `dependencies/r-version.txt` |
| CRAN packages and scopes | `dependencies/r-packages.csv` |
| CRAN resolution | `dependencies/cran-snapshot.txt` (one dated Posit Package Manager snapshot) |
| INLA | `dependencies/inla.csv` (version, candidate sources, recorded resolved URL and SHA-256) |
| System libraries | `dependencies/apt-packages.txt` |
| Preflight | `scripts/preflight.R` |

## Stages

The per-stage inputs, outputs, package groups, seeds, determinism, output class
and invariant are recorded in `reproducibility/pipeline_stage_registry.csv`.
The narrative below covers what the table cannot.

### Snapshot inputs (stages `00_*`)

The snapshot carries the frozen phenotype stage, the frozen 1-km cell context,
the MLIT-derived human rasters, the raw MLIT primary-mesh cache, the MLIT
A16-15 DID archive, the five Bombus prediction surfaces, and the WorldPop
layer. `scripts/verify_canonical_snapshot.R` rehashes every member against
`inputs/canonical_snapshot.json` and then materialises them into the paths the
locked runner expects. That materialisation step is what replaced the pipeline's
former dependence on one developer machine's directory layout.

### `02_run_natural_predictive_model`

Five components — national presence, national intensity, national
year-plus-phenology, common-support presence, and common-support Bombus
presence — each cross-fitted over five response-blind 100-km spatial folds with
1,000 predictive draws. Seed `20260725`; the binomial replicate draw uses
`seed + 1` per fold.

All five are required. The audit
(`validation/audit_natural_predictive_model.R`) checks for exactly these five
models with five folds each, and `validation/validate_local_pigmented_isolates.R`
loads the presence, intensity **and phenology** draw checkpoints to build the
auxiliary facet profile behind `local_isolate_natural_null_summary.csv`, which
is a required publication artifact. The phenology component therefore cannot be
dropped from the canonical DAG.

`--components` exists so a partial rerun can refit one component; the canonical
workflow does not pass it, so all five are fitted and the selection is recorded
in `predictive_replication_component_scope.csv`.

### `05_run_local_human_context` and `05_run_did_sensitivity`

These two stages read raw MLIT products rather than only the derived rasters:
the local-human-context stage re-processes the L03-b primary-mesh archives to
separate seven land-use classes, and the DID stage rasterises the A16-15
Densely Inhabited District polygons. Both caches ship inside the snapshot and
are addressed through `HOTARUBUKURO_MLIT_CACHE` and `HOTARUBUKURO_DID_CACHE`, so
neither stage contacts MLIT during a canonical run.

### `07_build_publication_figures`

`rnaturalearth::ne_countries(scale = "medium")` resolves from the
`rnaturalearthdata` package rather than the network. That package is declared
explicitly; without it the figure stage would attempt a download mid-run.

## Determinism

Distinguish two properties.

**Bitwise reproducible.** The frozen snapshot inputs, and every table derived
from them by deterministic code. Two clean runs produce identical SHA-256
hashes.

**Statistically reproducible only.** Anything downstream of
`INLA::inla.posterior.sample()`. The sampling call is seeded and
single-threaded, but the INLA fit preceding it is not bit-deterministic across
runs, so the draw matrices differ slightly between runs. Two independent clean
runs of the previous canonical configuration produced a bit-identical 1-km cell
table and bit-identical observed statistics, while Monte Carlo p-values computed
against the draws moved in the third decimal (0.2018 versus 0.2138).

Consequences:

- report Monte Carlo p-values from draw-based stages to two decimals;
- numerical regression checks on draw-derived quantities use tolerances rather
  than equality, recorded in `reproducibility/numerical_regression_report.csv`;
- a `.rds` checkpoint hash is not a determinism check — the v16 checkpoints also
  store per-fold wall-clock timings.

The random-forest null draws in `04_run_human_landscape_features` and the
permutation nulls in the v20/v21/v22 stages are likewise seeded per run but
compared on tolerance.

## Restartability

`scripts/run_natural_predictive_model.R` writes each component checkpoint
through a temporary file and renames it into place, so a checkpoint on disk is
either complete or absent, never truncated. It reloads a checkpoint only when
the recorded `analysis_spec_version`, model name and draw count all match the
requested configuration, and refits otherwise; file existence alone is never
treated as proof of validity.

## Failure conditions

The canonical workflow fails, rather than degrading, when:

- the snapshot descriptor is missing, or any member checksum does not match;
- a required column, CRS, extent or value range in a snapshot input is wrong;
- a declared namespace fails to load, or the INLA posterior-sampling, sf/terra,
  or figure-device smoke tests fail;
- the stage registry references a script that does not exist;
- any stage in `run_publication_pipeline.R` returns non-zero;
- any independent validation or claim-audit row is not `PASS`.

## Audit notes

- **Bombus provenance.** The five species prediction surfaces are restored from
  the committed publication commit `bcceb7c7`, not regenerated. The repository
  versions neither the ENMeval tuning grid nor the fitted candidate objects that
  `scripts/select_enmeval_models.R` reads, so AICc reselection cannot run from
  this repository alone. They are treated as archived immutable inputs. See
  `reproducibility/reproduction_summary.md`.
- **WorldPop naming.** `docs/data-sources/public-environment-sources.md`
  describes the population layer as "unconstrained/unadjusted" but links the
  `Global_2000_2020_1km_UNadj` directory. The reconstruction uses
  `Global_2000_2020_1km/2020/JPN/jpn_ppp_2020_1km_Aggregated.tif`, the
  unadjusted product, and records that exact URL in the snapshot manifest. The
  documentation contradicts itself and should be corrected.
