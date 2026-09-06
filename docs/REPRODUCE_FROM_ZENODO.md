# Reproduce from the raw Zenodo workbook

This is the **only canonical data route** for the paper. The repository does not commit a derived colour CSV as an analysis input.

## Frozen source and exact-output contract

Raw source:

- Zenodo record: <https://zenodo.org/records/22334596>
- DOI: `10.5281/zenodo.22334596`
- file: `Supplementary_Table_S1.xlsx`
- frozen MD5: `a923616e45f10f24a5463eefd09b06dd`
- expected observations: `1965`

The generated public table is checked against [`reproducibility/source_contract.json`](../reproducibility/source_contract.json). That small contract stores the expected Git blob of the deterministic public table, plus row/schema/QC invariants. The historical derived CSV itself is not kept in the active tree.

## Exact chain

```text
Zenodo Supplementary_Table_S1.xlsx
  -> embedded photograph bound to its workbook cell
  -> source_build/extract_color.py
  -> results/source_reconstruction/colour_extraction_from_zenodo.csv
  -> source_build/build_data_s1.py
  -> results/source_reconstruction/Data_S1_from_zenodo.csv
  -> source_build/source_contract.py
  -> exact validation against reproducibility/source_contract.json
  -> run_pipeline.py reproduce
  -> retained publication analyses
```

`source_build/extract_color.py` resolves Excel in-cell rich images directly from OOXML relationships. It does not attach photographs to observations by row-order joining.

`source_build/build_data_s1.py` materializes deterministic public fields, including site/grid identifiers and QC/provenance states. It writes LF line endings explicitly, so the output Git blob is platform-independent.

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

This prints the Zenodo source/checksum, extraction command, public-table materialization command, exact-contract validation target and the downstream pipeline command. No network access or analysis is performed in dry-run mode.

## 3. Rebuild the canonical table

```bash
python source_build/reproduce_from_zenodo.py
```

The bootstrap will:

1. download the Zenodo workbook to `.repro_cache/zenodo/` unless a verified copy is already cached;
2. verify the workbook MD5;
3. extract workbook images and colour/QC measurements;
4. write `results/source_reconstruction/colour_extraction_from_zenodo.csv`;
5. materialize `results/source_reconstruction/Data_S1_from_zenodo.csv`;
6. validate row count, schema, unique observation IDs, numeric bounds and QC counts;
7. require the generated table Git blob to equal the frozen exact-output blob in `reproducibility/source_contract.json`;
8. write `results/source_reconstruction/zenodo_rebuild_audit.json`.

A failed exact contract stops the chain.

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

The analysis starts only after exact-contract validation succeeds. The generated table is then consumed at the fixed path:

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

## GitHub Actions

Normal pull-request CI does not repeatedly download the 109.7 MB workbook. It checks the command graph, source-contract logic, Python tests, retained R surface and R unit tests.

The workflow has one opt-in end-to-end job, `raw_zenodo_reproduction`. When manually enabled, it performs:

```text
Zenodo download
-> raw image reconstruction
-> exact generated-table contract validation
-> R/system setup
-> run_pipeline.py reproduce --no-resume
```

There is no separate full-reproduction job starting from a committed derived CSV.

## Outputs to inspect

Raw reconstruction:

- `results/source_reconstruction/colour_extraction_from_zenodo.csv`
- `results/source_reconstruction/Data_S1_from_zenodo.csv`
- `results/source_reconstruction/zenodo_rebuild_audit.json`
- `results/source_reconstruction/qc/`

Full analysis:

- normal stage outputs under `results/`
- `results/analysis_reproduction/run_manifest.json`

The downstream manifest records the canonical source, source-contract hash, generated table hash/blob and the Git blobs of the active raw-reconstruction source files.

## Failure interpretation

- **Zenodo MD5 mismatch** — the workbook is not the frozen deposited source; stop.
- **Workbook/image mapping failure** — inspect workbook schema or explicit `--sheet`, `--header-row`, `--image-column`, `--id-column` values.
- **Row/schema/QC invariant failure** — the generated public table no longer satisfies the frozen source contract.
- **Exact Git-blob mismatch** — at least one generated byte differs from the validated historical public table; do not run downstream analyses.
- **Downstream R/public-source failure after successful exact validation** — raw image reconstruction succeeded; diagnose the named retained pipeline stage separately.
