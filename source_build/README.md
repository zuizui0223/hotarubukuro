# Source-build utilities used by the current paper

These utilities construct the derived flower-colour table and the public environmental, Bombus and human-landscape inputs that feed the manuscript-facing workflows.

For the scientific hierarchy, start with `paper/README.md`. Source construction is a data layer, not a separate manuscript story after the trait table has been built.

## Flower-colour trait construction

- `extract_color.py` — deterministic visible-colour extraction from the validated focal flower/petal region.
- `build_data_s1.py` — constructs the auditable derived `Data_S1` table with source/date/coordinate/image-hash/QC provenance.

The active JBI Supporting Information treats this conversion from recreational photographs to quantitative traits as a methodological contribution.

## Environmental and human inputs

- `download_rasters.R` and `prepare_rasters.R` — public-raster acquisition and alignment.
- `build_human_raster.R` — human-landscape raster preparation.

## Fresh Bombus source build

The current five-species SDM route is deliberately singular:

- `fetch_bombus_occurrences.R` — GBIF focal and target-group occurrence acquisition;
- `canonicalize_bombus_occurrences.R` — sorted, deduplicated and hashed occurrence snapshot;
- `build_bombus_sdm_mainland.R` — **active ecology-aligned common-mainland ENMeval/maxnet build**;
- `audit_bombus_extraction.R` — prediction-surface extraction audit;
- `validation/compare_bombus_sdm_rebuilds.R` — repeated-build reproducibility check.

`.github/workflows/rebuild-bombus-sdm.yml` reconstructs all five focal Bombus SDMs over the common Honshu–Shikoku–Kyushu study domain. The model specification is in `config/bombus_sdm.yml`.

The active source build:

1. acquires the declared CHELSA v2.1, SoilGrids 2.0 and WorldClim 2.1 predictors on one grid;
2. queries Japanese GBIF occurrences for the five focal species plus target-group background records;
3. applies the frozen filtering/deduplication rules;
4. uses the common mainland study domain and shared predictor screen;
5. fits ENMeval `maxnet` candidates under spatial block partitioning;
6. selects the minimum finite AICc candidate; and
7. writes cloglog habitat-support surfaces plus diagnostics and hashes.

Earlier species-specific-accessible-area and national-projection prototypes have been removed from `source_build/` and indexed under `legacy/source-build-prototypes/`.

For the manuscript, these SDMs are **environment-derived predicted habitat support**, not visitation, abundance, pollen transfer or selection pressure. Species surfaces are subsequently calibrated against the prediction distribution at observed occurrence cells before the local flower-colour test.

## Frozen manuscript evidence versus live refreshes

GBIF and other public inputs can change. A live source refresh is therefore a new source-build exercise, not an automatic replacement for the manuscript evidence. The current paper uses checksum-locked successful artifacts listed in `paper/analysis-map.md`.

The current directional pollinator analysis uses *B. ardens* and *B. diversus* because they define the documented broad focal-pollinator availability estimand. The other three taxa remain available for Supporting Information community-turnover and montane/elevation guardrails.

Historical source-reconstruction prototypes, standalone population-flow audits and superseded analysis architectures are under `legacy/`.
