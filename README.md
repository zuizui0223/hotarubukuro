# hotarubukuro

Public analysis repository for the submission on geographical flower-colour polymorphism in *Campanula punctata*.

This repository has **one canonical data source and one retained publication-analysis path**. The canonical source is the public image-bearing Zenodo workbook. Derived colour tables are generated locally and are not committed as analysis inputs.

## Reproduce from zero

Canonical raw source:

- Zenodo record: [`22334596`](https://zenodo.org/records/22334596)
- DOI: `10.5281/zenodo.22334596`
- file: `Supplementary_Table_S1.xlsx`
- frozen MD5: `a923616e45f10f24a5463eefd09b06dd`
- expected observations: `1965`

The implemented chain is:

```text
Zenodo Supplementary_Table_S1.xlsx
  -> source_build/extract_color.py
  -> results/source_reconstruction/colour_extraction_from_zenodo.csv
       (rich technical extraction record)
  -> source_build/build_data_s1.py
  -> results/source_reconstruction/Data_S1_from_zenodo.csv
       (lean deterministic 38-column analysis input)
  -> reproducibility/source_contract.json exact lean-contract validation
  -> run_pipeline.py reproduce
  -> final retained analyses
```

Install and run the complete chain:

```bash
python -m pip install -e '.[test]'
python source_build/reproduce_from_zenodo.py --run-analysis
```

Inspect the complete command graph without downloading or running analyses:

```bash
python source_build/reproduce_from_zenodo.py --dry-run --run-analysis
```

`source_build/extract_color.py` resolves embedded Excel images through workbook cell/OOXML relationships, not by positional joining.

### Rich extraction record versus analysis input

The two generated CSVs have deliberately different roles.

`colour_extraction_from_zenodo.csv` is the **rich technical intermediate**. It retains candidate colour statistics, detailed extraction/QC diagnostics, legacy comparison fields and the run-time `processed_at` value. It is useful for auditing the image-processing step, but it is not the table consumed by the ecological analyses.

`Data_S1_from_zenodo.csv` is the **lean deterministic analysis input**. `source_build/build_data_s1.py` projects the rich extraction to the 38 fields actually needed by the retained source-build and analysis code, derives site/grid and provenance fields, removes run-time-only metadata, normalises numeric text and sorts by `observation_id`.

The frozen lean contract is:

- rows: `1965`
- Git blob: `e119137efac89cbcfd789236f3d6a3c9599575af`
- SHA-256: `9e543b64a824aff82dbb55da1bca8843fb337a51399bfd60ad0a09c9bca3c33c`
- automated QC: `1180 ok`, `785 manual_review_required`

These values are recorded in [`reproducibility/source_contract.json`](reproducibility/source_contract.json). The historical full `Data_S1.csv` blob (`74b951898814f4ed15f314061e3129d8b05823d5`) is retained there only as provenance; the 3.6 MB CSV itself is not an active input and is not stored in the current tree.

This distinction avoids treating run timestamps, legacy RGB comparisons or unused candidate diagnostics as part of the ecological-analysis contract while retaining exact reproducibility for every field that the current analysis can consume.

The former root-level `Code_S1.py` was a GPX/photo-time georeferencing utility, not the colour extractor. It is not part of the active tree; Git history retains it for provenance. The active colour code is `source_build/extract_color.py`.

Detailed checkpoints and failure interpretation are in [`docs/REPRODUCE_FROM_ZENODO.md`](docs/REPRODUCE_FROM_ZENODO.md).

## Running the downstream pipeline separately

After the canonical analysis table has been generated and validated, the retained downstream graph can be rerun with:

```bash
python run_pipeline.py audit
python run_pipeline.py reproduce
```

`run_pipeline.py` accepts no alternative colour-table argument. Its only flower-colour analysis input is:

```text
results/source_reconstruction/Data_S1_from_zenodo.csv
```

To validate only the repository structure before the raw table exists:

```bash
python run_pipeline.py audit --structure-only
python run_pipeline.py reproduce --dry-run --skip-setup
```

## Final analysis path

1. **Quantitative phenotype** — `source_build/extract_color.py`, `source_build/build_data_s1.py`, `scripts/run_phenotype_hurdle.R`
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

- `source_build/` — canonical Zenodo/image reconstruction and public external-source builders.
- `run_pipeline.py` — the single retained downstream orchestrator.
- `R/` — reusable functions used by retained analyses.
- `scripts/` — retained publication-analysis stages.
- `config/` — acquisition/model configuration still consumed by the active pipeline.
- `dependencies/` — pinned R/system dependency records.
- `reproducibility/` — source contract, final scientific decisions, result locks and benchmark records.
- `tests/` — Python and R tests for retained modules.
- `results/` — local generated outputs; ignored by Git except for its README placeholder.

Development-only alternatives, obsolete workflow wrappers, orphan validation helpers and historical utilities are intentionally left to Git history rather than coexisting with the active publication surface.
