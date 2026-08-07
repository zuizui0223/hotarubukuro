# Source-build utilities

These utilities document optional construction of derived colour tables and public environmental or occurrence inputs. They are not called by `.github/workflows/analysis-1909.yml` or `scripts/run_analysis_1909.sh`.

- `extract_color.py` and `build_data_s1.py`: visible-colour and public-table utilities.
- `download_rasters.R` and `prepare_rasters.R`: public-raster acquisition and alignment.
- `fetch_bombus_occurrences.R`: GBIF occurrence acquisition.
- `build_human_raster.R`: MLIT human-landscape raster preparation.
- `audit_bombus_extraction.R`: extraction audit utility.

The canonical 1,909 run restores checksum-locked inputs instead of rebuilding them. Running this directory creates a new source-build exercise whose outputs need their own hashes and validation.

## Bombus SDM boundary

This directory can reacquire occurrence records, but it does **not** contain a complete source-to-surface reconstruction of the archived five-species *Bombus* SDMs used by the active analysis. In particular, the original ENMeval candidate/tuning objects needed to reproduce the historical model-selection path were not retained. The active pipeline therefore starts from checksum-locked prediction surfaces and treats them as fixed inputs.

Any future attempt to propagate pollinator-SDM uncertainty must be versioned as a new source-build analysis. At minimum it should freeze the occurrence snapshot and filtering rules, accessible/background region, environmental predictor stack, spatial partitions, candidate feature classes and regularization values, model-selection rule, fitted candidates or an equivalent complete selection table, prediction rasters, and hashes for every realization. See `docs/bombus-sdm-inference.md`.

Historical reconstruction scripts that depend on superseded v11/v15 implementations are under `legacy/reconstruction-prototypes/` rather than here.
