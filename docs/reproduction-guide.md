# Reproducing the 1,909-observation analysis

## Canonical route: GitHub Actions

The canonical run restores the immutable analysis-input snapshot named in `inputs/canonical_snapshot.json`. Historical result-directory names inside that snapshot are retained only to preserve file identity; their old generators are under `legacy/implementations/`.

1. Open the repository's **Actions** tab.
2. Choose **1909 analysis pipeline**.
3. Click **Run workflow** and select `main`.
4. Keep `build_figures=true` unless only numerical outputs are needed.
5. Start the workflow.
6. Download `analysis-1909-<commit>-<run-id>` after completion.

The workflow performs, in order:

1. restore the pinned R version and declared dependencies;
2. restore and SHA-256 verify the immutable snapshot;
3. verify exactly 1,909 observations, 955 white-like and 954 pigmented;
4. run dependency and INLA smoke tests;
5. execute the active stages declared in `reproducibility/pipeline_stage_registry.csv`;
6. run active unit tests, independent validators and claim audits;
7. build figures from fresh outputs; and
8. upload outputs, logs, manifests and provenance as one artifact.

The post-hoc bidirectional colour-state asymmetry diagnostic is not in this sequence. Candidate DOY is retained only as a supplementary post-selection description.

## Success criteria

Check:

```text
reproducibility/analysis_population_check.csv
results/final_analysis_pipeline/final_stage_manifest.csv
results/final_analysis_pipeline/final_independent_validation.csv
results/final_analysis_pipeline/final_claim_audit.csv
results/final_analysis_pipeline/final_result_registry.csv
reproducibility/reproduction_summary.md
```

All population rows and all executed stages must be `PASS`. A scientific result on either side of a statistical threshold is recorded as a result and is not itself a software failure.

## Local execution

```bash
git clone https://github.com/zuizui0223/hotarubukuro.git
cd hotarubukuro

Rscript scripts/setup_r_environment.R \
  --report-dir reproducibility \
  --scopes analysis,reproducibility,testing,figures,reporting

bash scripts/run_analysis_1909.sh
```

Optional controls:

```bash
BUILD_FIGURES=false bash scripts/run_analysis_1909.sh
RUN_TESTS=false bash scripts/run_analysis_1909.sh
SNAPSHOT_DIR=/absolute/path/to/snapshot bash scripts/run_analysis_1909.sh
```

Public GitHub release assets normally restore anonymously. `GITHUB_TOKEN` can be supplied for rate-limit or repository-policy reasons.

## Direct active commands

Restore the snapshot:

```bash
bash scripts/canonical_snapshot.sh restore \
  inputs/canonical_snapshot.json \
  reproduction_inputs/snapshot
```

Verify the population:

```bash
Rscript scripts/check_analysis_population.R \
  --expectations inputs/analysis_1909_expectations.csv \
  --report-dir reproducibility \
  --strict true
```

Run the numerical and validation stages:

```bash
Rscript scripts/run_publication_pipeline.R \
  --mode full \
  --baseline analysis_1909 \
  --tests true
```

Build figures:

```bash
Rscript scripts/build_publication_figures.R
```

These are the only supported canonical entry points. Exact active modules and scripts are listed in `config/code_manifest.csv`.

## Inputs and generated outputs

`Data_S1.csv` contains curated derived flower-colour measurements and source identifiers. Raw YAMAP photographs are not redistributed. The snapshot supplies the fixed upstream phenotype/cell tables and public spatial layers; its asset and member hashes are declared in `inputs/canonical_snapshot.json`.

Files under `results/`, generated `reproducibility/` reports and `manuscript/figures/` are run products. The complete runner clears previous generated products before restoring inputs, so a new run cannot silently reuse committed or stale numerical outputs.

## Source-build utilities

`source_build/` contains optional utilities for colour extraction and public-data acquisition or alignment. They are audited as code but are not called by the canonical DAG. Running them creates a new source-build exercise rather than reproducing the checksum-locked 1,909 analysis.

## Legacy material

- `legacy/published-1923/`: earlier 1,923 fixed outputs, manuscript and workflows.
- `legacy/implementations/frozen-upstream/`: superseded v11/v15 implementation code, runners and tests.
- `legacy/diagnostics/local-state-asymmetry/`: post-hoc reverse-direction diagnostic.
- `legacy/reconstruction-prototypes/`: historical public-reconstruction experiments.

Nothing in the active commands imports these directories.

## Statistical rather than bitwise reproducibility

INLA posterior samples can differ at the bit level despite fixed seeds, folds and draw counts. The pipeline therefore verifies input hashes, fixed definitions, stage completion, finite results, claim ceilings and output provenance. Report effect sizes, realised p/q values, uncertainty and the run commit together.
