# Pipeline DAG: active 1,909 analysis

```text
Data_S1.csv + inputs/canonical_snapshot.json
                    |
                    v
snapshot restore + SHA-256 verification
                    |
                    v
frozen phenotype and 1-km cell input audits
n = 1,909; white-like = 955; pigmented = 954
                    |
                    v
02 national environment + INLA-SPDE natural baseline
presence / conditional intensity / five blocked folds / 1,000 draws
              |                                |
              v                                v
03 local Bombus turnover            04 pigmented-in-white isolate event
fixed response-blind graphs          identical extractor on 1,000 maps
              |                                |
              |                       S1 held-out candidate DOY
              |                       description only; no selection
              +--------------------+-----------+
                                   v
05 post-selection human context
population / land use / roads / DID / familywise maxT
                                   |
                                   v
06 final result lock + independent validation + claim audit
                                   |
                                   v
07 figures generated from fresh outputs
```

## Explicit interfaces

| Purpose | Entry point |
|---|---|
| GitHub-hosted complete run | `.github/workflows/analysis-1909.yml` |
| Local complete run | `scripts/run_analysis_1909.sh` |
| Numerical stages and validation | `scripts/run_publication_pipeline.R` |
| Input population gate | `scripts/check_analysis_population.R` |
| Figure generation | `scripts/build_publication_figures.R` |
| Exact code declaration | `config/code_manifest.csv` |
| Machine-readable stage declaration | `reproducibility/pipeline_stage_registry.csv` |

## Boundaries

The DAG reads no file under `legacy/` or `source_build/`. Frozen upstream result-directory names are preserved inside the immutable snapshot, but the old implementations that created those files are not loadable through `R/pipeline_support.R`.

The reverse white-in-pigmented asymmetry diagnostic is archived rather than shown as a paper stage. Source-build utilities are a separate provenance layer and do not replace the immutable snapshot during canonical reproduction.
