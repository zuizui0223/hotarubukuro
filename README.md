# hotarubukuro

Public analysis repository for the submission on geographical flower-colour polymorphism in *Campanula punctata*.

This repository contains **one publication analysis path** plus a raw-data bootstrap that reconstructs its derived colour input from the public image-bearing Zenodo workbook. Development-only alternatives, superseded candidate detectors and one-off CI workflows are intentionally removed rather than left beside the final code.

## Reproduce from zero: Zenodo images -> colour table -> final analyses

The raw public source is Zenodo record [`22334596`](https://zenodo.org/records/22334596), file `Supplementary_Table_S1.xlsx` (MD5 `a923616e45f10f24a5463eefd09b06dd`). It contains the image-bearing starting table.

The exact zero-reproduction chain implemented in this repository is:

```text
Zenodo Supplementary_Table_S1.xlsx
  -> source_build/extract_color.py
  -> results/source_reconstruction/colour_extraction_from_zenodo.csv
  -> source_build/build_data_s1.py
  -> results/source_reconstruction/Data_S1_from_zenodo.csv
  -> strict downstream-input equivalence audit against frozen Data_S1.csv
  -> run_pipeline.py reproduce --data-s1 results/source_reconstruction/Data_S1_from_zenodo.csv
  -> final retained analyses
```

The complete chain is wrapped in one command:

```bash
python -m pip install -e '.[test]'
python source_build/reproduce_from_zenodo.py --dry-run --run-analysis
python source_build/reproduce_from_zenodo.py --run-analysis
```

`source_build/reproduce_from_zenodo.py` downloads and checksum-verifies the Zenodo workbook, extracts each embedded photograph through its workbook cell relationship, and recomputes petal colour with `source_build/extract_color.py`. The intermediate extraction is then converted by `source_build/build_data_s1.py` into the public analysis-table contract, including the deterministic site identifiers and provenance/QC fields required downstream.

The rebuilt 1,965-row `Data_S1_from_zenodo.csv` is then audited against the frozen `Data_S1.csv` by immutable `observation_id` and the retained fields that can alter downstream analysis: RGB and median RGB, coordinates, date, image hashes, duplicate/overexposure/QC and review-status fields, mask metrics, site/grid identifiers and coordinate/source provenance fields where present in the frozen input. A mismatch stops the chain.

After the audit passes, the **rebuilt table itself** is supplied to `run_pipeline.py reproduce --data-s1 ...`; the downstream analysis does not switch back to the committed `Data_S1.csv`. The ordinary `run_pipeline.py reproduce` command remains unchanged and continues to use the frozen table by default.

Full step-by-step instructions, checkpoints and failure interpretation are in [`docs/REPRODUCE_FROM_ZENODO.md`](docs/REPRODUCE_FROM_ZENODO.md).

> **Active vs historical code.** Image colour reconstruction is implemented by `source_build/extract_color.py` followed by `source_build/build_data_s1.py`. The historical root-level `Code_S1.py` was a GPX/photo-time georeferencing utility, not the colour extractor. It has been moved to `legacy/Code_S1_georeference.py` and is not part of the active publication path.

### Why `Data_S1.csv` remains in the repository

`Data_S1.csv` is retained deliberately for two roles: (1) the frozen reference contract against which a zero-from-Zenodo reconstruction is audited, and (2) the faster default starting point for `run_pipeline.py reproduce`. It is **not** the raw-data starting point for the zero-reproduction route.

## Faster reproduction from the frozen derived input

If raw image reconstruction is not required, the original publication-input route remains:

```bash
python run_pipeline.py audit
python run_pipeline.py reproduce
```

`audit` checks the committed derived dataset and the active source-build files required by the submission pipeline. `reproduce` rebuilds the analysis from `Data_S1.csv` plus the declared public environmental and occurrence sources. Live third-party sources can change; frozen paper claims and decision records are retained under `reproducibility/`.

For an already verified alternative reconstruction, the same retained graph can be pointed at that table explicitly:

```bash
python run_pipeline.py reproduce --data-s1 results/source_reconstruction/Data_S1_from_zenodo.csv
```

## Final analysis path

1. **Quantitative phenotype** — `source_build/extract_color.py`, `source_build/build_data_s1.py`, `scripts/run_phenotype_hurdle.R`
   - embedded photographs can be reconstructed from the Zenodo XLSX through `source_build/reproduce_from_zenodo.py`;
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
- `source_build/` — active Zenodo image reconstruction plus public raster and Bombus source reconstruction.
- `config/` — frozen acquisition/model configuration.
- `dependencies/` — R/system dependency records.
- `reproducibility/` — final scientific decisions, result locks and benchmark records.
- `tests/` — tests for modules that remain in the publication path.
- `legacy/` — provenance-only utilities retained outside the active publication path.
