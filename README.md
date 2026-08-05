# hotarubukuro

Reproducible nationwide analysis of flower-colour geography in *Campanula punctata* from author-reviewed YAMAP photographs.

## Active analysis: 1,909 observations

`main` contains one active analysis baseline: the **1,909-observation analysis** reconstructed from `Data_S1.csv` and the pinned immutable input snapshot described by `inputs/canonical_snapshot.json`.

Expected analysis population:

| Quantity | Expected |
|---|---:|
| observations | 1,909 |
| white-like observations | 955 |
| pigmented observations | 954 |

The active pipeline estimates:

1. a two-part flower-colour response (pigmentation presence and pigmented-only visible intensity);
2. national environmental and INLA-SPDE structure;
3. local flower-colour turnover against a predicted five-species *Bombus* fingerprint;
4. repeated local pigmented-isolate events under 1,000 natural-map draws; and
5. post-selection human-landscape context.

The predicted *Bombus* fingerprint is relative habitat support and composition, not abundance, visitation, pollen transfer, or direct selection pressure. Human-context results prioritize follow-up sites; they do not establish horticultural origin.

## Run on GitHub Actions

1. Open **Actions**.
2. Select **1909 analysis pipeline**.
3. Select **Run workflow** on `main`.
4. Leave `build_figures=true` and start the run.
5. Download the artifact named `analysis-1909-<commit>-<run-id>`.

A successful run contains:

- `reproducibility/analysis_population_check.csv` with three `PASS` rows;
- `results/final_analysis_pipeline/final_stage_manifest.csv` with all stages `PASS`;
- independent validation and claim-audit tables;
- regenerated result tables; and
- regenerated manuscript figures when requested.

Exact commands, output paths, and interpretation limits are in [`docs/reproduction-guide.md`](docs/reproduction-guide.md). The stage graph is in [`docs/pipeline-dag.md`](docs/pipeline-dag.md).

## Run locally

The simplest supported local route is Linux, macOS, or WSL/Git Bash with the pinned R version:

```bash
git clone https://github.com/zuizui0223/hotarubukuro.git
cd hotarubukuro

Rscript scripts/setup_r_environment.R \
  --report-dir reproducibility \
  --scopes analysis,reproducibility,testing,figures,reporting

bash scripts/run_analysis_1909.sh
```

The shell driver restores and verifies the immutable snapshot, checks the 1,909-row population, runs the complete analysis, validates every stage, and writes a run summary.

## Repository layout

```text
R/                         active analysis functions
scripts/                   active runners and data-build scripts
validation/                independent validation and claim audits
tests/                     software tests
inputs/                    active snapshot descriptor and 1,909 expectations
docs/                      active 1,909 documentation
results/                   generated at run time; not a source of truth
manuscript/                active manuscript notes and generated figures
legacy/published-1923/     archived 1,923 artifacts, manuscript, and old runners
```

## Legacy 1,923 analysis

The earlier 1,923-observation outputs, manuscript, reference tables, and comparison workflows are preserved under `legacy/published-1923/` for provenance only. They are not read by the active workflow or CI. The original 1,923 analysis-input tables were not retained, so that analysis is not presented as fully rerunnable.

## Reproducibility ceiling

The pipeline is designed for **method and statistical reproducibility**, not guaranteed bitwise identity of INLA posterior samples. Seeds, folds, draw counts, input hashes, candidate definitions, validators, and output manifests are fixed. Candidate identities and major directions should remain stable, but Monte Carlo quantities close to a threshold can vary slightly between runs; report the realised estimate and uncertainty rather than treating a single rounded p-value as immutable.
