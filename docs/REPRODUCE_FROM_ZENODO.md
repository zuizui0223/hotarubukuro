# Reproduce from the raw Zenodo workbook

This is the **only canonical data route** for the paper. The repository does not commit a derived flower-colour CSV as an analysis input.

## Frozen source and analysis-input contract

Raw source:

- Zenodo record: <https://zenodo.org/records/22334596>
- DOI: `10.5281/zenodo.22334596`
- file: `Supplementary_Table_S1.xlsx`
- frozen MD5: `a923616e45f10f24a5463eefd09b06dd`
- expected observations: `1965`

The deterministic downstream analysis table is checked against [`reproducibility/source_contract.json`](../reproducibility/source_contract.json).

Contract v2 distinguishes the historical rich public table from the actual current analysis-input contract:

- historical full `Data_S1.csv` Git blob: `74b951898814f4ed15f314061e3129d8b05823d5` — provenance only;
- canonical lean analysis table: 38 columns, 1,965 rows;
- lean Git blob: `e119137efac89cbcfd789236f3d6a3c9599575af`;
- lean SHA-256: `9e543b64a824aff82dbb55da1bca8843fb337a51399bfd60ad0a09c9bca3c33c`.

## Exact chain

```text
Zenodo Supplementary_Table_S1.xlsx
  -> embedded photograph bound to its workbook cell
  -> source_build/extract_color.py
  -> results/source_reconstruction/colour_extraction_from_zenodo.csv
       rich technical extraction/QC record
  -> source_build/build_data_s1.py
  -> results/source_reconstruction/Data_S1_from_zenodo.csv
       lean deterministic analysis input
  -> source_build/source_contract.py
  -> exact validation against reproducibility/source_contract.json
  -> run_pipeline.py reproduce
  -> retained publication analyses
```

`source_build/extract_color.py` resolves Excel in-cell rich images directly from OOXML relationships. It does not attach photographs to observations by row-order joining.

The rich extraction CSV intentionally retains technical information useful for auditing the image-processing step: candidate colour methods, detailed QC/sensitivity fields, historical RGB-comparison fields and the run-time `processed_at` timestamp.

`source_build/build_data_s1.py` then projects that rich record to the **38 fields actually consumed by the retained source-build and ecological analyses**. It derives site/grid and provenance fields, removes run-time-only and unused technical fields, normalises numeric text and sorts rows by `observation_id`. The analysis table is therefore deterministic even though the technical extraction record contains a run timestamp.

The old root-level `Code_S1.py` was a GPX/photo-time georeferencing utility. It was never the active image-colour extractor and is retained only in Git history.

## 1. Clone and install

```bash
git clone https://github.com/zuizui0223/hotarubukuro.git
cd hotarubukuro
python -m venv .venv
source .venv/bin/activate          # Windows: .venv\Scripts\activate
python -m pip install --upgrade pip
python -m pip install -e '.[test]'
```

The Python dependencies needed for the raw workbook route include `numpy`, `Pillow` and `openpyxl`.

## 2. Inspect the command graph

```bash
python source_build/reproduce_from_zenodo.py --dry-run --run-analysis
```

This prints the Zenodo source/checksum, rich extraction command, lean-table materialization command, exact-contract target and downstream pipeline command. No network access or analysis is performed in dry-run mode.

## 3. Rebuild the canonical analysis table

```bash
python source_build/reproduce_from_zenodo.py
```

The bootstrap will:

1. download the Zenodo workbook to `.repro_cache/zenodo/` unless a verified copy is already cached;
2. verify the workbook MD5;
3. extract workbook images and full colour/QC measurements;
4. write the rich technical record `results/source_reconstruction/colour_extraction_from_zenodo.csv`;
5. materialize the lean deterministic `results/source_reconstruction/Data_S1_from_zenodo.csv`;
6. validate exact ordered schema, row count, unique/sorted observation IDs, numeric bounds and QC counts;
7. require both the lean Git blob and SHA-256 to match contract v2;
8. write `results/source_reconstruction/zenodo_rebuild_audit.json`.

A failed lean contract stops the chain.

### Use a workbook already downloaded

```bash
python source_build/reproduce_from_zenodo.py \
  --workbook /path/to/Supplementary_Table_S1.xlsx
```

The local workbook is still required to match the frozen Zenodo MD5.

### Deliberately overwrite local generated products

```bash
python source_build/reproduce_from_zenodo.py --overwrite-output
```

Use `--overwrite-download` only when intentionally replacing the cached workbook.

## 4. Run from raw source through final analyses

```bash
python source_build/reproduce_from_zenodo.py --run-analysis
```

The analysis starts only after lean-contract validation succeeds. The generated table is consumed at the fixed path:

```text
results/source_reconstruction/Data_S1_from_zenodo.csv
```

There is no `--data-s1` override and no committed derived-data fallback.

Equivalent two-step form:

```bash
python source_build/reproduce_from_zenodo.py
python run_pipeline.py reproduce
```

To force all downstream stages to rerun:

```bash
python source_build/reproduce_from_zenodo.py \
  --overwrite-output \
  --run-analysis \
  --no-resume-analysis
```

## 5. Audit the generated input separately

After reconstruction:

```bash
python run_pipeline.py audit
```

Before reconstruction, CI/developers can check only the executable repository surface:

```bash
python run_pipeline.py audit --structure-only
python run_pipeline.py reproduce --dry-run --skip-setup
```

## What is and is not part of the exact contract

The lean analysis contract includes the raw fields that can affect retained analyses: observation/source identity, dates and coordinates, RGB/median RGB, image hashes, duplicate/overexposure status, mask size/fraction/component metrics, exposure-filtered fraction, site/grid identifiers and coordinate/source/QC provenance.

The following remain in the rich extraction intermediate but are deliberately **not** analysis-input contract fields because the retained pipeline does not consume them:

- `processed_at` run timestamp;
- `legacy_R/G/B` and differences from legacy RGB;
- candidate/joint peak details and sensitivity-only peak diagnostics;
- narrative `qc_note` and QC-sampling bookkeeping.

This is a scope reduction, not a weaker scientific check: changing any field that the retained analysis actually reads changes the lean table hash and stops the pipeline.

## GitHub Actions

Normal pull-request CI does not repeatedly download the 109.7 MB workbook. It checks the command graph, source-contract logic, Python tests, retained R surface and R unit tests.

The workflow has one opt-in end-to-end job, `raw-zenodo-reproduction`. When manually enabled, it performs:

```text
Zenodo download
-> rich image/colour reconstruction
-> deterministic lean-table materialization
-> exact lean-contract validation
-> R/system setup
-> run_pipeline.py reproduce --no-resume
```

There is no separate full-reproduction route starting from a committed derived CSV.

## Outputs to inspect

Raw reconstruction:

- `results/source_reconstruction/colour_extraction_from_zenodo.csv` — rich technical record;
- `results/source_reconstruction/Data_S1_from_zenodo.csv` — lean analysis input;
- `results/source_reconstruction/zenodo_rebuild_audit.json`;
- `results/source_reconstruction/qc/`.

Full analysis:

- normal stage outputs under `results/`;
- `results/analysis_reproduction/run_manifest.json`.

The downstream manifest records the canonical source, source-contract hash, generated analysis-table hash/blob and Git blobs of the active raw-reconstruction source files.

## Failure interpretation

- **Zenodo MD5 mismatch** — the workbook is not the frozen deposited source; stop.
- **Workbook/image mapping failure** — inspect workbook schema or explicit `--sheet`, `--header-row`, `--image-column`, `--id-column` values.
- **Row/schema/QC invariant failure** — the generated lean analysis table no longer satisfies the frozen source contract.
- **Lean Git-blob/SHA mismatch** — at least one current analysis-input value differs from the frozen lean reference; do not run downstream analyses.
- **Downstream R/public-source failure after successful lean validation** — raw image reconstruction succeeded; diagnose the named retained pipeline stage separately.
