# Pipeline DAG: active 1,909 analysis

```text
Data_S1.csv
    + inputs/canonical_snapshot.json
                |
                v
restore + SHA-256 verification
                |
                v
01 fixed phenotype and 1-km cells
    n = 1,909; white = 955; pigmented = 954
                |
                v
02 national environment + INLA-SPDE
    presence / conditional intensity
    5 blocked folds / 1,000 predictive draws
          |                     |
          |                     +--------------------+
          v                                          v
03 local Bombus turnover                    04 local isolate event
    fixed response-blind graphs                 same event on 1,000 maps
          |                                          |
          +--------------------+---------------------+
                               v
05 post-selection human context
    population / DID / land use / roads
    familywise maxT control
                               |
                               v
06 final lock
    stage manifest / independent validation
    claim audit / output manifest / provenance
```

## Executable entry points

| Purpose | Entry point |
|---|---|
| GitHub-hosted complete run | `.github/workflows/reconstruction-analysis.yml` |
| Local complete run | `scripts/run_analysis_1909.sh` |
| Numerical stages and validation | `scripts/run_publication_pipeline.R` |
| Input population check | `scripts/check_analysis_population.R` |
| Figure generation | `scripts/build_publication_figures.R` |
| Machine-readable stage registry | `reproducibility/pipeline_stage_registry.csv` |

The active DAG reads no file under `legacy/`.
