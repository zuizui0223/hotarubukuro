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
NO Bombus term or national Bombus comparison
              |                                |
              v                                v
03 local Bombus limitation gate      04 pigmented-in-white isolate event
<=25 km + environmental matching      identical extractor on natural maps
all 5 low -> >=1 species available               |
directed pigmentation contrast        S1 held-out candidate DOY
              |                       description only; no selection
              |                                |
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

## Stage-03 inference boundary

The active local Bombus test is not a second national regression. Pairs are formed without flower-colour information, restricted to 25 km and the same held-out fold, and matched on the four broad/within-50-km environmental axes before the flower response is read. The active lower-third gate contrasts a cell where all five focal *Bombus* species have within-species predicted-support rank <=0.33 with a matched cell where at least one species has rank >=0.50.

The 0.33 gate was adopted for biological interpretability after exploratory design development. The complete 0.10/0.20/0.25/0.33 grid and across-grid multiplicity remain in the outputs and claim lock. Thus stage 03 is a mechanistically motivated local sensitivity, not a retrospectively relabelled preregistered test.

The 1,000 stage-02 flower maps are replayed on the fixed matched pairs as a predictive reference. Environment and an SPDE field are not fitted again locally. The *Bombus* surfaces are checksum-locked predicted habitat-support layers: they do not measure abundance, visitation, reproductive success or selection, and shared/unmeasured environmental structure can remain.

The previous unsigned *Bombus*-community turnover analysis is outside the active pipeline. The active paper tests *Bombus* only through the local limitation gate.

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

The DAG reads no implementation under `legacy/` or `source_build/`. Frozen upstream result-directory names are preserved inside the immutable snapshot, but the old implementations that created those files are not loadable through `R/pipeline_support.R`.

Source-build utilities are a separate provenance layer and do not replace the immutable snapshot during canonical reproduction. The historical ENMeval candidate-selection path is not claimed to be reproducible from occurrences; downstream Bombus inference is reproducible conditional on the frozen prediction surfaces.
