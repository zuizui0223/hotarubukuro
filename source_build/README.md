# Source-build utilities

These utilities construct the derived flower-colour table and the environmental, Bombus and human-landscape inputs used by the manuscript-facing workflows.

For the scientific hierarchy, start with `paper/README.md`; for exact acquisition/reproduction boundaries, use `docs/reproduction-guide.md`.

## Flower-colour trait construction

- `extract_color.py` — deterministic visible-colour extraction from the validated focal flower/petal region;
- `build_data_s1.py` — construct the distributable `Data_S1.csv` table with source/date/coordinate/image-hash/QC provenance.

Public column definitions and excluded provenance fields are documented in `docs/data-s1-dictionary.md`.

## Environmental and human inputs

- `download_rasters.R` — acquire declared public raster sources;
- `prepare_rasters.R` — align and prepare raster inputs;
- `build_human_raster.R` — prepare human-landscape raster support.

Source identities are declared under `config/`. Mutable public sources are never allowed to silently replace checksum-locked manuscript evidence.

## Bombus source build

- `fetch_bombus_occurrences.R` — acquire GBIF focal and target-group occurrences;
- `canonicalize_bombus_occurrences.R` — sort, deduplicate and hash the occurrence snapshot;
- `build_bombus_sdm_mainland.R` — fit the common-mainland ENMeval/maxnet models;
- `audit_bombus_extraction.R` — audit prediction-surface extraction;
- `../validation/compare_bombus_sdm_rebuilds.R` — repeated-build reproducibility check.

Execution:

- `.github/workflows/rebuild-bombus-sdm.yml`.

Model specification:

- `config/bombus_sdm.yml`.

The source build:

1. acquires the declared CHELSA v2.1, SoilGrids 2.0 and WorldClim 2.1 predictors on one grid;
2. queries Japanese GBIF occurrences for the five focal species plus target-group background;
3. applies the declared filtering and deduplication rules;
4. uses one common Honshu–Shikoku–Kyushu study domain and shared predictor screen;
5. fits ENMeval `maxnet` candidates under spatial block partitioning;
6. selects the minimum finite-AICc candidate; and
7. writes cloglog habitat-support surfaces, diagnostics and hashes.

For the manuscript, these SDMs are **environment-derived predicted habitat support**, not abundance, realized visitation, pollen transfer or selection pressure. Species surfaces are subsequently transformed to occurrence-referenced support before the local flower-colour boundary test.

The directional Main analysis uses *B. ardens* and *B. diversus*. The other three focal taxa contribute to Supporting community-turnover and montane/elevation guardrails.

## Frozen evidence versus live refreshes

A live GBIF/raster refresh is a new source build. The manuscript uses the checksum-locked artifacts listed in `paper/analysis-map.md`. Replacement of a locked evidence object requires an explicit rerun, validation and scientific decision rather than file recency.
