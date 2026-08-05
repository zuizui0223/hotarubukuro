# Reproducing the 1,909-observation analysis

## Recommended: GitHub Actions

The standard reproducible run starts from the immutable input snapshot named in `inputs/canonical_snapshot.json`. The historical filename is retained for compatibility; it is the active **1,909 analysis snapshot**.

1. Go to the repository's **Actions** tab.
2. Choose **1909 analysis pipeline**.
3. Click **Run workflow** and select `main`.
4. Keep `build_figures=true` unless only numerical outputs are needed.
5. Start the workflow.
6. At completion, download `analysis-1909-<commit>-<run-id>`.

The workflow performs, in order:

1. restore the pinned R version and declared system/R dependencies;
2. restore and SHA-256 verify the immutable analysis-input snapshot;
3. verify the active population is exactly 1,909 observations, 955 white-like and 954 pigmented;
4. run dependency and INLA smoke tests;
5. run the full national and local analysis with `--baseline=analysis_1909`;
6. run all independent validators, audits, software tests, and the final claim lock;
7. build figures from fresh outputs, never from committed result tables; and
8. upload results, logs, manifests, and provenance as one artifact.

### Success criteria

Check these files in the downloaded artifact:

```text
reproducibility/analysis_population_check.csv
results/final_analysis_pipeline/final_stage_manifest.csv
results/final_analysis_pipeline/final_independent_validation.csv
results/final_analysis_pipeline/final_claim_audit.csv
results/final_analysis_pipeline/final_result_registry.csv
reproducibility/reproduction_summary.md
```

All three population rows and every stage must be `PASS`. Claim-audit rows distinguish a scientific result from a pipeline failure; a p-value on either side of 0.05 is not itself a software failure.

## Local execution

### 1. Clone and enter the repository

```bash
git clone https://github.com/zuizui0223/hotarubukuro.git
cd hotarubukuro
```

### 2. Install the declared environment

Use the R version in `dependencies/r-version.txt`. On Ubuntu/WSL, install the packages listed in `dependencies/apt-packages.txt`, then run:

```bash
Rscript scripts/setup_r_environment.R \
  --report-dir reproducibility \
  --scopes analysis,reproducibility,testing,figures,reporting
```

INLA is installed and verified against `dependencies/inla.csv`. On Windows, use WSL or Git Bash and set `TEMP`/`TMP` to an ASCII-only path if INLA has temporary-path problems.

### 3. Run the analysis

```bash
bash scripts/run_analysis_1909.sh
```

Optional environment variables:

```bash
BUILD_FIGURES=false bash scripts/run_analysis_1909.sh
RUN_TESTS=false bash scripts/run_analysis_1909.sh
SNAPSHOT_DIR=/absolute/path/to/snapshot bash scripts/run_analysis_1909.sh
```

For a public GitHub release, snapshot restoration normally works anonymously. Set `GITHUB_TOKEN` if API rate limits or repository policy require authentication.

## Direct component commands

Restore the snapshot only:

```bash
bash scripts/canonical_snapshot.sh restore \
  inputs/canonical_snapshot.json \
  reproduction_inputs/snapshot
```

Verify the analysis population:

```bash
Rscript scripts/check_analysis_population.R \
  --expectations inputs/analysis_1909_expectations.csv \
  --report-dir reproducibility \
  --strict true
```

Run the model and validation pipeline after the snapshot has been materialized:

```bash
Rscript scripts/run_publication_pipeline.R \
  --mode full \
  --baseline analysis_1909 \
  --tests true
```

Build figures from the newly generated outputs:

```bash
Rscript scripts/build_publication_figures.R
```

## Inputs and outputs

`Data_S1.csv` contains the curated derived flower-colour measurements and source identifiers. Raw YAMAP photographs are not redistributed. The immutable snapshot supplies the fixed upstream phenotype/cell tables and public spatial layers needed by the active analysis. Its asset checksum and every member checksum are declared in `inputs/canonical_snapshot.json`.

Generated files under `results/`, `reproducibility/`, and `manuscript/figures/` are run products. They must not be treated as inputs to a new run. The driver sets a run-start timestamp and the output manifest records which files were produced in that run.

## Statistical rather than bitwise reproducibility

INLA posterior samples can differ at the bit level despite fixed seeds, folds, and draw counts. The active pipeline therefore logs checkpoint hashes and verifies scientific invariants. Candidate identities and major result directions have been stable in repeated runs, while Monte Carlo values near a decision boundary can shift slightly. Report effect sizes, realised p/q values, uncertainty, and the run commit together.

## Legacy material

All 1,923-observation fixed outputs, the old manuscript, old comparison code, and recovery workflows are in `legacy/published-1923/`. Nothing in the active commands above imports from that directory.
