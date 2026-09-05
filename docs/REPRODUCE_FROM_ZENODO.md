# Reproduce from the raw Zenodo workbook

This is the public **zero-to-analysis** route for the paper. It starts from the image-bearing workbook deposited at Zenodo rather than from the already-derived `Data_S1.csv` committed in this repository.

## Frozen raw source

- Zenodo record: <https://zenodo.org/records/22334596>
- DOI: `10.5281/zenodo.22334596`
- File: `Supplementary_Table_S1.xlsx`
- Published record size: about 109.7 MB
- Frozen MD5: `a923616e45f10f24a5463eefd09b06dd`
- Expected observations after reconstruction: `1965`

The bootstrap refuses to continue if the downloaded workbook does not match the frozen MD5.

## What is reconstructed

The route is:

```text
Zenodo Supplementary_Table_S1.xlsx
  -> embedded photo associated with its workbook row/cell
  -> source_build/extract_color.py
  -> deterministic petal mask + RGB / CIELAB summaries + QC
  -> results/source_reconstruction/colour_extraction_from_zenodo.csv
  -> source_build/build_data_s1.py
  -> deterministic public Data_S1 contract
  -> results/source_reconstruction/Data_S1_from_zenodo.csv
  -> downstream-input equivalence audit against frozen Data_S1.csv
  -> run_pipeline.py reproduce --data-s1 Data_S1_from_zenodo.csv
  -> final publication analyses driven by the rebuilt table itself
```

The cell-to-image association is not a positional join. `source_build/extract_color.py` resolves Excel in-cell rich images directly from OOXML relationships and binds each image to its workbook cell before emitting the immutable `observation_id`.

`source_build/build_data_s1.py` then materializes the public analysis-table contract from that verified extraction. This stage restores deterministic site/grid identifiers and the provenance/QC fields expected by the retained analysis without using row-order joins.

The equivalence audit is deliberately broader than an RGB check. It compares immutable observation IDs and, where present in the frozen analysis input, the fields that can change the retained downstream analysis: `R/G/B`, median RGB, coordinates, date, QC/review status, duplicate-image identity, overexposure state, image hash, mask pixels and visible-mask fraction, site/grid identifiers and coordinate/source provenance. Run-specific fields such as processing timestamps and QC output paths are not used as equality criteria.

## Active code and historical `Code_S1.py`

The active image-colour reconstruction is `source_build/extract_color.py` followed by `source_build/build_data_s1.py`.

The former root-level `Code_S1.py` is a **GPX photo-time georeferencing utility**, not the image-colour extractor. It is retained only for provenance at `legacy/Code_S1_georeference.py` and is outside the active publication analysis path. Git history preserves the original root location.

## 1. Clone and install Python dependencies

```bash
git clone https://github.com/zuizui0223/hotarubukuro.git
cd hotarubukuro
python -m venv .venv
source .venv/bin/activate          # Windows: .venv\Scripts\activate
python -m pip install --upgrade pip
python -m pip install -e '.[test]'
```

The standard package dependencies include `numpy`, `Pillow` and `openpyxl`; no separate Excel extra is required for the public raw route. The image-bearing XLSX is read through OOXML-aware code and is not joined to a separate table by row order.

## 2. Inspect the complete command without downloading anything

```bash
python source_build/reproduce_from_zenodo.py --dry-run --run-analysis
```

This prints the frozen Zenodo URL/checksum, the exact colour-extraction and Data_S1-materialization commands, and the downstream command. The downstream command includes `--data-s1 results/source_reconstruction/Data_S1_from_zenodo.csv`, which is the key guarantee that the reconstructed table itself enters the analysis graph.

## 3. Rebuild the colour table from Zenodo

```bash
python source_build/reproduce_from_zenodo.py
```

The script will:

1. download the workbook to `.repro_cache/zenodo/Supplementary_Table_S1.xlsx`;
2. verify the Zenodo MD5;
3. run the deterministic workbook image extractor;
4. write masks/overlays/QC material under `results/source_reconstruction/qc/`;
5. write `results/source_reconstruction/colour_extraction_from_zenodo.csv`;
6. run `source_build/build_data_s1.py` to materialize the public analysis-table contract;
7. write `results/source_reconstruction/Data_S1_from_zenodo.csv`;
8. compare the reconstructed table against frozen `Data_S1.csv` by `observation_id` and the downstream-relevant input contract described above;
9. write `results/source_reconstruction/zenodo_rebuild_audit.json`.

A mismatch stops the chain. The script does **not** silently replace the committed publication input and it does not enter the final analysis on a failed audit.

### Use a workbook already downloaded manually

```bash
python source_build/reproduce_from_zenodo.py \
  --workbook /path/to/Supplementary_Table_S1.xlsx
```

The local file is still checked against the frozen Zenodo MD5.

### Re-run extraction deliberately

```bash
python source_build/reproduce_from_zenodo.py --overwrite-output
```

Use `--overwrite-download` only when you intentionally want to replace the local Zenodo cache.

## 4. Go from the raw workbook all the way to the paper analyses

```bash
python source_build/reproduce_from_zenodo.py --run-analysis
```

The downstream analysis begins only if the rebuilt table passes the downstream-input equivalence audit. It is then passed directly to the retained pipeline as the active analysis input. The committed `Data_S1.csv` remains untouched and continues to serve as the frozen reference contract.

Equivalent explicit two-step form:

```bash
python source_build/reproduce_from_zenodo.py
python run_pipeline.py reproduce \
  --data-s1 results/source_reconstruction/Data_S1_from_zenodo.csv
```

`run_pipeline.py` verifies the frozen `Data_S1.csv` contract and the active image-reconstruction source files (`source_build/extract_color.py` and `source_build/build_data_s1.py`). It separately validates the selected analysis table's row count, unique observation IDs and minimum schema. The archived GPX utility is not required by this audit.

The downstream pipeline resumes completed stages by default. To force a clean downstream rerun instead:

```bash
python source_build/reproduce_from_zenodo.py \
  --overwrite-output \
  --run-analysis \
  --no-resume-analysis
```

## 5. Existing faster publication-input route

If you only need to reproduce the analyses from the already reconstructed colour table, the original commands remain valid:

```bash
python run_pipeline.py audit
python run_pipeline.py reproduce
```

That route starts from frozen `Data_S1.csv`; it is not the raw-image bootstrap. No `--data-s1` argument is required for the canonical publication-input path.

`Data_S1.csv` remains committed deliberately because it is both the frozen comparison target for the zero-from-Zenodo audit and the fast default input for this route.

## GitHub Actions route

The repository workflow includes a manual `raw_zenodo_reproduction` option. When selected with **Actions -> submission-analysis-contract -> Run workflow**, GitHub Actions first downloads and rebuilds the Zenodo colour table and checks the downstream-input contract. Only after that succeeds does it install the R/system analysis environment and execute `run_pipeline.py reproduce --data-s1 results/source_reconstruction/Data_S1_from_zenodo.csv`.

Normal pull-request CI dry-runs the raw bootstrap command graph and compiles the active Python source-build surface so ordinary PRs do not repeatedly download the 109.7 MB workbook.

## Outputs to check

After raw reconstruction, the main checkpoints are:

- `results/source_reconstruction/colour_extraction_from_zenodo.csv`
- `results/source_reconstruction/Data_S1_from_zenodo.csv`
- `results/source_reconstruction/zenodo_rebuild_audit.json`
- `results/source_reconstruction/qc/`

After full analysis, `run_pipeline.py` writes its normal stage outputs and `results/analysis_reproduction/run_manifest.json`. The manifest records `analysis_data_s1_path`, its SHA-256, whether the active input was the canonical committed table, and the Git blob identities of the frozen Data_S1 contract and active image-reconstruction source files.

## Failure interpretation

- **Zenodo MD5 mismatch**: the raw file is not the frozen deposited workbook; do not continue.
- **Workbook/image-column error**: inspect the workbook schema or supply `--sheet`, `--header-row`, `--image-column`, or `--id-column` explicitly.
- **1965-row or observation-ID mismatch**: the reconstruction is not equivalent to the publication data contract.
- **Core numeric mismatch**: RGB or coordinates differ; inspect `zenodo_rebuild_audit.json` before proceeding.
- **Downstream exact/numeric mismatch**: a date or QC-relevant field differs even if RGB is identical; do not launch the final analysis until the difference is resolved.
- **Downstream R/public-source error after a successful raw audit**: raw image reconstruction succeeded; diagnose the named retained stage in `run_pipeline.py` separately.
