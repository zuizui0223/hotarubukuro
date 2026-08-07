# hotarubukuro

Reproducible nationwide analysis of flower-colour geography in *Campanula punctata* from author-reviewed YAMAP photographs.

## Supported analysis

The repository has one active and rerunnable baseline: the **1,909-observation analysis** reconstructed from `Data_S1.csv` and the immutable snapshot declared in `inputs/canonical_snapshot.json`.

| Quantity | Required |
|---|---:|
| observations | 1,909 |
| white-like observations | 955 |
| pigmented observations | 954 |

The active pipeline contains only the ordered scientific arc used for the current reconstruction:

1. audit the frozen two-part phenotype and 1-km cell inputs;
2. fit the national environment-plus-INLA-SPDE natural baseline;
3. test local flower-colour turnover against the predicted five-species *Bombus* fingerprint;
4. define pigmented-in-white local isolates and replay the same event on 1,000 natural maps;
5. characterize population, land use and DID context only after candidates are fixed;
6. describe candidate flowering date as a held-out supplementary check; and
7. write independent validation, claim and artifact locks.

The *Bombus* fingerprint represents relative predicted habitat support and composition, not abundance, visitation or selection. The active 1,909 analysis treats the checksum-locked *Bombus* prediction surfaces as **fixed archived inputs**. The downstream local-turnover analysis is reproducible conditional on those surfaces, but uncertainty from GBIF sampling, ENMeval model selection, fitted SDM parameters and alternative prediction surfaces is **not propagated** through the 1,000 flower-colour predictive maps. Accordingly, the local result is a conditional community-turnover correspondence, not a causal interaction or pollinator-selection estimate. See [`docs/bombus-sdm-inference.md`](docs/bombus-sdm-inference.md) for the inference ceiling and a stronger future design.

Human-context outputs prioritize follow-up sites and do not establish horticultural origin.

## Supported entry points

| Purpose | Command or file |
|---|---|
| GitHub-hosted complete run | `.github/workflows/analysis-1909.yml` |
| Local complete run | `bash scripts/run_analysis_1909.sh` |
| Ordered numerical stages | `Rscript scripts/run_publication_pipeline.R --mode full --baseline analysis_1909` |
| Active code declaration | `config/code_manifest.csv` |
| Stage declaration | `reproducibility/pipeline_stage_registry.csv` |

Everything called by the active pipeline is declared in both the loader or stage registry and the code manifest. CI fails when an undeclared executable file is added to the non-legacy code roots.

## Run on GitHub Actions

1. Open **Actions**.
2. Select **1909 analysis pipeline**.
3. Select **Run workflow** on `main`.
4. Leave `build_figures=true` and start the run.
5. Download `analysis-1909-<commit>-<run-id>`.

A successful artifact contains:

- `reproducibility/analysis_population_check.csv` with three `PASS` rows;
- `results/final_analysis_pipeline/final_stage_manifest.csv` with all stages `PASS`;
- `final_independent_validation.csv` and `final_claim_audit.csv`;
- newly generated result tables; and
- newly generated manuscript figures when requested.

## Run locally

```bash
git clone https://github.com/zuizui0223/hotarubukuro.git
cd hotarubukuro

Rscript scripts/setup_r_environment.R \
  --report-dir reproducibility \
  --scopes analysis,reproducibility,testing,figures,reporting

bash scripts/run_analysis_1909.sh
```

See [`docs/reproduction-guide.md`](docs/reproduction-guide.md) for exact checks and outputs and [`docs/pipeline-dag.md`](docs/pipeline-dag.md) for the stage graph.

## Repository layout

```text
R/                               active analysis modules and one declared source-build helper
scripts/                         active runners and reproducibility support only
validation/                      active input, stage and final validators
tests/                           tests for active code and declared source-build utilities
source_build/                    optional raw/public-data construction utilities; not canonical input
inputs/                          immutable 1,909 snapshot descriptor and population expectations
config/code_manifest.csv         exact non-legacy executable-file declaration
reproducibility/                 stage registry and generated run reports
results/                         generated output; never a committed source of truth
legacy/published-1923/           archived 1,923 manuscript, outputs and historical workflows
legacy/implementations/          superseded upstream implementations and their tests
legacy/diagnostics/              non-paper or withdrawn diagnostics
legacy/reconstruction-prototypes/ historical reconstruction experiments
```

`source_build/` is deliberately outside the canonical DAG. It documents how public or derived inputs were assembled, but a standard 1,909 reproduction restores checksum-locked inputs instead of downloading or rebuilding them.

## Legacy boundary

No active loader, stage, validator or test imports `legacy/`. Historical filenames such as `ecological_v11_*` and `ecological_v15_*` remain inside the immutable snapshot because changing them would alter the preserved input package; the implementations that originally generated them are archived.

The bidirectional local colour-state asymmetry diagnostic is also archived. It was explicitly post hoc and is not part of the manuscript pipeline. The only active flowering-date component is the held-out candidate DOY description, which cannot select or rank candidates.

## Reproducibility ceiling

The target is method and statistical reproducibility, not guaranteed bitwise identity of INLA posterior samples. Seeds, folds, draw counts, input hashes, candidate definitions and validators are fixed. Report each run's realised estimates and Monte Carlo uncertainty with its commit rather than treating a rounded threshold-adjacent p-value as immutable.

For the *Bombus* component, reproducibility currently begins at the archived prediction surfaces rather than at ENMeval model selection. Rebuilding alternative SDMs from occurrences constitutes a new source-build analysis and must carry its own occurrence snapshot, background definition, tuning grid, model objects or equivalent selection table, prediction hashes and uncertainty analysis.
