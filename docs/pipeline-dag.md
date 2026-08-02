# Analysis pipeline DAG

Every node below records its generating script, required inputs, expected
outputs, deterministic seed, software requirements, and whether the output is an
immutable input, a derived checkpoint, or a final result.

```text
Data_S1.csv (raw/versioned phenotype input)
  → environmental input preparation
  → environmental/spatial analysis table
  → Bombus predictors
  → two-part pigmentation models
  ────────────── canonical analysis-input snapshot boundary ──────────────
  → 1-km cell table
  → 1,000 cross-fitted natural predictive maps
  → local bidirectional asymmetry analysis
  → validation and results artifact
```

Nodes above the boundary belong to **workflow B**
(`.github/workflows/raw-data-reconstruction.yml`), which depends on external
public services. Nodes below the boundary belong to **workflow A**
(`.github/workflows/canonical-analysis.yml`), which starts from the immutable,
checksummed snapshot and must be reliable on a clean runner.

## Environment (shared by every node)

| Item | Declaration | Notes |
|---|---|---|
| R | `dependencies/r-version.txt` | 4.5.3, installed by `r-lib/actions/setup-r` |
| CRAN packages | `dependencies/r-packages.csv` + `dependencies/cran-snapshot.txt` | One dated Posit Package Manager snapshot pins every version |
| INLA | `dependencies/inla.csv` | Version pinned; candidate URLs validated as real archives declaring that version; resolved URL and SHA-256 recorded to `reproducibility/inla_resolution.csv` |
| System libraries | `dependencies/apt-packages.txt` | Installed identically by every workflow |
| Preflight | `scripts/preflight.R` | Loads every declared namespace and runs `INLA::inla.posterior.sample()` on a tiny model before any expensive stage |

## Node table

### 1. Raw/versioned phenotype input

| | |
|---|---|
| Generating script | none; committed to the repository |
| Inputs | — |
| Outputs | `Data_S1.csv` |
| Seed | not applicable |
| Software | — |
| Class | **immutable input** |

Author-reviewed colour measurements, source identifiers, image hashes, and QC
fields. The raw photographs are not redistributable; `Data_S1.csv` is the
versioned phenotype record.

### 2. Public raster acquisition

| | |
|---|---|
| Generating script | `scripts/download_rasters.R` then `scripts/prepare_rasters.R` |
| Inputs | `config/raster_sources.csv`, `config/pipeline.yml` |
| Outputs | `data/cache/rasters/`, `data/processed/rasters/`, `data/processed/raster_download_manifest.csv`, `data/processed/raster_manifest.csv` |
| Seed | not applicable |
| Software | terra, sf, yaml, digest, GDAL/GEOS/PROJ |
| Class | **immutable input** (recorded URL, retrieval time, SHA-256 per source) |

CHELSA v2.1 climatologies and WorldClim 2.1 elevation are pinned by URL;
SoilGrids 2.0 layers are fetched through the WCS coverage identifiers named in
the registry and masked by valid bulk-density cells. Sources materialised as
study-area subsets record the checksum actually produced together with the
source URL, bounding box, and code hashes, because GDAL versions may differ
byte-for-byte while producing identical raster values.

### 3. Environmental input preparation

| | |
|---|---|
| Generating script | `scripts/build_environment_input.R` |
| Inputs | `Data_S1.csv`, prepared rasters (18 layers) |
| Outputs | `results/environment_v3/ecological_input_v2.csv` |
| Seed | not applicable (deterministic extraction and PCA with fixed sign convention) |
| Software | terra, jsonlite |
| Class | **derived checkpoint** |

### 4. Human-landscape rasters

| | |
|---|---|
| Generating script | `scripts/build_human_raster.R` |
| Inputs | `Data_S1.csv`, MLIT National Land Numerical Information L03-b 2021 |
| Outputs | `results/public_rasters/mlit_human_forest_edge_2021/*.tif` |
| Seed | not applicable |
| Software | terra, foreign, ranger |
| Class | **derived checkpoint** |

### 5. Bombus predictors

| | |
|---|---|
| Generating scripts | `scripts/fetch_bombus_occurrences.R`, `scripts/select_enmeval_models.R`, `scripts/run_natural_biotic_covariates.R` |
| Inputs | GBIF occurrences for five *Bombus* species, prepared rasters, `results/ecological_v9_final_public_HRNA_50km/analysis_data.csv` |
| Outputs | `results/bombus_occurrence_phenology_cache/`, `results/enmeval_aicc_reselected/predictions/*.tif`, `results/ecological_v10_final_mechanism_HRNA/analysis_data_mechanism_v3.csv` |
| Seed | ENMeval partition seed set in `scripts/select_enmeval_models.R`; occurrence download date recorded in the cache manifest |
| Software | rgbif, ENMeval, maxnet, terra, sf, FNN, dplyr, readr |
| Class | **derived checkpoint** |

### 6. Environmental/spatial analysis table

| | |
|---|---|
| Generating script | `scripts/run_environment_spatial.R` |
| Inputs | `results/environment_v3/ecological_input_v2.csv`, `Data_S1.csv`, Bombus prediction surfaces, H/R/N/A rasters |
| Outputs | `results/ecological_v9_final_public_HRNA_50km/analysis_data.csv` |
| Seed | spatial-block assignment is deterministic (50-km blocks, 5 folds, 136.5°E cut) |
| Software | sf, terra, mgcv, INLA |
| Class | **derived checkpoint** |

Projected coordinates use the analysis CRS
`+proj=laea +lat_0=36 +lon_0=137 +datum=WGS84 +units=m`, divided by 1000 to give
`x_km`/`y_km`. Downstream stages re-derive and check this projection.

### 7. Two-part pigmentation models

| | |
|---|---|
| Generating script | `scripts/run_phenotype_hurdle.R` |
| Inputs | `results/ecological_v10_final_mechanism_HRNA/analysis_data_mechanism_v3.csv` |
| Outputs | `results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv` and the hurdle fixed-effect, hyperparameter, and collinearity tables |
| Seed | INLA inference is deterministic given the input; mixture classification uses `mclust` with a fixed confidence cut of 0.8 |
| Software | INLA, mclust, mgcv, qgam, quantreg, sf, terra |
| Class | **derived checkpoint** |

### 8. Canonical analysis-input snapshot

| | |
|---|---|
| Generating scripts | `scripts/stage_canonical_snapshot.R`, `scripts/canonical_snapshot.sh publish` |
| Inputs | node 7 output; the four multiscale environment layers and the WorldPop layer from node 2 |
| Outputs | GitHub Release asset named in `inputs/canonical_snapshot.json`, plus `SNAPSHOT_MANIFEST.csv` inside the archive |
| Seed | not applicable |
| Software | terra, digest, jsonlite |
| Class | **immutable input for workflow A** |

The archive is a deterministic tar (sorted members, fixed mtime and ownership).
Its SHA-256 and every member SHA-256 are committed to
`inputs/canonical_snapshot.json`. Workflow A refuses to run if the descriptor is
missing or a checksum does not match. The snapshot is a Release asset rather
than an Actions artifact or cache, so it does not expire and is not tied to a
run.

### 9. 1-km cell table

| | |
|---|---|
| Generating script | `scripts/run_multiscale_hotspots.R` |
| Inputs | `analysis_data_pigmentation_hurdle.csv`, `elevation_Japan_crop.tif`, `bio10_Japan_crop_30s.tif`, `bio12_Japan_crop_30s.tif`, `RSDS_Japan_crop_30s.tif`, `population_count_Japan_crop.tif` (all from the snapshot) |
| Outputs | `results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv` and the environment-context, provenance, and contrast tables |
| Seed | bootstrap resampling uses 1000 replicates with the stage's fixed seed |
| Software | INLA, mgcv, sf, terra |
| Class | **derived checkpoint** |

Each observed 1-km cell carries a 50-km neighbourhood mean and a
cell-minus-neighbourhood deviation for the four environment layers, summarised
by two response-blind principal components per scale
(`broad50km_pc1`, `broad50km_pc2`, `within50km_pc1`, `within50km_pc2`). These
four columns define the environmental distance used by the locked neighbourhood
graph.

### 10. 1,000 cross-fitted natural predictive maps

| | |
|---|---|
| Generating script | `scripts/run_natural_predictive_model.R --components=national_environment_spde_presence --draws=1000 --seed=20260725` |
| Inputs | `analysis_data_pigmentation_hurdle.csv`, `multiscale_hotspot_cells_1km.csv` |
| Outputs | `results/ecological_v16_predictive_replication/checkpoints/national_environment_spde_presence_draws1000.rds`, `predictive_replication_component_scope.csv`, `predictive_replication_component_checkpoints.csv` |
| Seed | `20260725` for `INLA::inla.posterior.sample()`; `seed + 1` for the binomial replicate draw, set per fold inside `v16_fit_fold()` |
| Software | INLA (with `sn` reached through `inla.posterior.sample`), Matrix, sf, terra |
| Class | **derived checkpoint** |

Five response-blind 100-km spatial folds; the SPDE field is constrained to zero
mean at fold-specific training locations; predictions are drawn from INLA's
projected APredictor at held-out cells with observed trial counts fixed. The
checkpoint is written through a temporary file and renamed, so a resumed run
never loads a truncated draw matrix.

`--components` selects which of the five stage-02 components to fit. The
canonical analysis workflow fits only the natural presence baseline, which is
the model the asymmetry diagnostic compares against; the selection is recorded
in `predictive_replication_component_scope.csv`. The raw reconstruction workflow
fits all five.

### 11. Local bidirectional asymmetry analysis

| | |
|---|---|
| Generating script | `scripts/run_local_state_asymmetry.R --required-maps=1000 --pseudocount=0.5` |
| Inputs | `multiscale_hotspot_cells_1km.csv`, `national_environment_spde_presence_draws1000.rds` |
| Outputs | `results/ecological_v23_local_state_asymmetry/local_state_asymmetry_{rules,summary,null,candidates,metadata}.csv`, `README.md` |
| Seed | none introduced; the diagnostic is a deterministic function of the cell table and the predictive draws. Upstream seeds are recorded in the metadata |
| Software | base R, sf (CRS check) |
| Class | **final result** |

The neighbourhood graph is read programmatically from the primary row of
`v20_configuration_table()` in `R/local_pigmented_isolates.R` (10-km radius,
environmental RMS-distance caliper 1, at least 3 neighbours, the locked
fold-boundary setting). Both event directions are extracted on that identical
graph. The primary statistic is
`log(pigmented_in_white_rate / white_in_pigmented_rate)` with a 0.5 pseudocount
on both the numerator and the two-pseudocount denominator, compared against the
same statistic on each of the 1,000 natural predictive maps using
`p = (1 + number at least as extreme) / (usable maps + 1)`.

### 12. Validation and results artifact

| | |
|---|---|
| Generating scripts | `validation/validate_local_state_asymmetry.R`, `scripts/write_reproducibility_report.R` |
| Inputs | the node 11 outputs |
| Outputs | `local_state_asymmetry_validation.csv`, `VALIDATION.md`, `reproducibility/{input_manifest.csv,output_manifest.csv,sessionInfo.txt,dependency_audit.txt,pipeline_dag.md,reproduction_summary.md}` |
| Seed | not applicable |
| Software | base R, digest |
| Class | **final result** |

Validation recomputes every reported Monte Carlo statistic from the per-map null
table, checks the opportunity normalisation identity, checks that the state
definitions never overlap, checks that the recorded locked configuration
reproduces the primary row of `v20_configuration_table()`, and checks the post
hoc label and claim ceiling.

## Failure conditions

The canonical workflow fails, rather than degrading, when:

- fewer than 1,000 predictive maps are available;
- cell identifiers are duplicated or cannot be aligned one-to-one with the
  predictive checkpoint;
- checkpoint trial counts or observed counts disagree with the cell table;
- any predictive count falls outside `[0, n_observations]` or is not an integer;
- the stored projected coordinates disagree with the analysis CRS;
- a locked environmental column is missing or non-finite;
- the locked primary configuration is not present exactly once;
- a snapshot checksum does not match `inputs/canonical_snapshot.json`;
- a declared namespace fails to load, or the INLA posterior-sampling smoke test
  fails.

## Audit notes

- `docs/data-sources/public-environment-sources.md` describes the population
  layer as the WorldPop "unconstrained/unadjusted" product but links to the
  `Global_2000_2020_1km_UNadj` directory. The reconstruction uses
  `Global_2000_2020_1km/2020/JPN/jpn_ppp_2020_1km_Aggregated.tif`, the
  unadjusted product, and records that exact URL in the snapshot manifest. The
  population columns are not used by the asymmetry graph, which depends only on
  `n_pigmented`, `n_observations`, `x_km`, `y_km`, `spatial_fold`, and the four
  environmental principal components.
