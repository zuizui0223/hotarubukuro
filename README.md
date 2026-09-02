# hotarubukuro

Public analysis repository for the submission on geographical flower-colour polymorphism in *Campanula punctata*.

This repository now contains **one publication analysis path**. Development-only alternatives, superseded candidate detectors and one-off CI workflows are intentionally removed rather than left beside the final code.

## Reproduce

```bash
python run_pipeline.py audit
python run_pipeline.py reproduce
```

`audit` checks the committed derived dataset and the files required by the submission pipeline. `reproduce` rebuilds the analysis from `Data_S1.csv` plus the declared public environmental and occurrence sources. Live third-party sources can change; frozen paper claims and decision records are retained under `reproducibility/`.

## Final analysis path

1. **Quantitative phenotype** — `Code_S1.py`, `scripts/run_phenotype_hurdle.R`
   - pigmentation state: white versus pigmented;
   - conditional visible intensity: analysed only among pigmented flowers.
2. **Broad geography** — `scripts/run_broad_environment_spatial_audit.R`, `scripts/build_fixed_space_null_cache.R`, `scripts/fit_broad_supported_term_distance_space_null.R`
   - environment + stationary SPDE;
   - environment-aligned differentiation tested against a cross-fitted space-only expectation.
3. **Local Bombus boundary test** — `scripts/build_bombus_occurrence_reference_support.R`, `scripts/run_bombus_local_sharp_transition.R`, `scripts/run_bombus_spatial_replication_test.R`
   - focal local boundary analysis plus the equal-elevation/spatial guardrail.
4. **Continuous isolation / human context** — `scripts/fit_final8_presence_null.R`, `scripts/run_continuous_colour_isolation.R`
   - all 1-km colour cells are analysed continuously;
   - human context is WorldPop exposure at focal, 5, 10, 25 and 50 km scales;
   - the same isolation statistic is replayed on 10,000 natural colour maps.

The shared 1-km analysis table is built once by `scripts/build_analysis_cells.R`. Its frozen geometry is **1,305 cells: 674 pigmented and 631 white**.

## Repository map

- `R/` — reusable functions required by the final pipeline.
- `scripts/` — publication analysis entry points.
- `source_build/` — public raster and Bombus source reconstruction.
- `config/` — frozen acquisition/model configuration.
- `dependencies/` — R/system dependency records.
- `reproducibility/` — final scientific decisions, result locks and benchmark records.
- `tests/` — tests for modules that remain in the publication path.

