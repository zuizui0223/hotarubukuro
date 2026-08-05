# Source-build utilities

These utilities document optional construction of derived colour tables and public environmental or occurrence inputs. They are not called by `.github/workflows/analysis-1909.yml` or `scripts/run_analysis_1909.sh`.

- `extract_color.py` and `build_data_s1.py`: visible-colour and public-table utilities.
- `download_rasters.R` and `prepare_rasters.R`: public-raster acquisition and alignment.
- `fetch_bombus_occurrences.R`: GBIF occurrence acquisition.
- `build_human_raster.R`: MLIT human-landscape raster preparation.
- `audit_bombus_extraction.R`: extraction audit utility.

The canonical 1,909 run restores checksum-locked inputs instead of rebuilding them. Running this directory creates a new source-build exercise whose outputs need their own hashes and validation.

Historical reconstruction scripts that depend on superseded v11/v15 implementations are under `legacy/reconstruction-prototypes/` rather than here.
