# Source-build utilities

These utilities construct derived colour tables and public environmental or occurrence inputs. They remain separate from the current manuscript pipeline until a new source build is validated and explicitly adopted.

- `extract_color.py` and `build_data_s1.py`: visible-colour and public-table utilities.
- `download_rasters.R` and `prepare_rasters.R`: public-raster acquisition and alignment.
- `fetch_bombus_occurrences.R`: live GBIF occurrence acquisition.
- `canonicalize_bombus_occurrences.R`: sorted, deduplicated, hashed occurrence snapshot.
- `build_bombus_sdm.R`: seeded five-species ENMeval/maxnet SDM source build.
- `build_human_raster.R`: MLIT human-landscape raster preparation.
- `audit_bombus_extraction.R`: prediction-surface extraction audit utility.

## Seeded Bombus SDM source build

`.github/workflows/rebuild-bombus-sdm.yml` reconstructs all five Bombus SDMs from public inputs rather than consuming the manuscript's archived prediction TIFFs. The workflow uses `config/bombus_sdm.yml` as the complete model specification.

The recovered source-build design is:

1. acquire the declared CHELSA v2.1, SoilGrids 2.0 and WorldClim 2.1 predictors on one 30-arc-second grid;
2. query Japanese GBIF occurrences for the five focal species, apply the declared coordinate and basis-of-record filters, then freeze the returned records by GBIF occurrence key;
3. thin occurrences to one record per predictor cell;
4. define species-specific accessible area M as an equal-area convex hull buffered by 15% of range diagonal, bounded to 100-300 km;
5. screen predictors with VIF <= 10 from a seeded environmental-domain sample;
6. build a target-group background from pooled focal-Bombus occurrence cells, with a seeded random cap of 10,000 cells and a seeded within-M fallback when necessary;
7. fit ENMeval `maxnet` candidates with block partitioning, feature classes L/LQ/LQH and regularization multipliers 1-5;
8. select the minimum finite AICc candidate and write a cloglog relative-suitability surface.

The reproducibility lock is explicit: base seed 42, Mersenne-Twister/Inversion/Rejection RNG, one computational thread, per-species/per-stage derived seeds, a pinned dated CRAN snapshot, configuration hashes and file hashes. The GitHub workflow performs two independent SDM rebuilds from the same frozen occurrence and raster inputs and fails unless selected-model tables and raster predictions agree within `1e-12`.

A live GBIF refresh is not itself a permanent frozen input because GBIF can gain or revise records. Therefore the first successful source-build artifact must be promoted to an immutable source snapshot before its SDMs are adopted by the manuscript pipeline. The frozen snapshot should contain the canonical occurrence CSVs, prepared predictor rasters or equivalent immutable source assets and checksums, configuration, selected-model tables, fitted ENMeval objects and final prediction surfaces.

The existing 1,909 manuscript analysis remains unchanged until this new source build passes repeated-build validation and the downstream flower-colour analysis is rerun deliberately with the newly generated surfaces.
