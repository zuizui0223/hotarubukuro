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

- `fetch_bombus_occurrences.R` — GBIF occurrence acquisition.
- `canonicalize_bombus_occurrences.R` — sorted, deduplicated occurrence snapshot.
- `build_bombus_sdm.R` — seeded five-species ENMeval/maxnet SDM build.
- `audit_bombus_extraction.R` — prediction-surface extraction audit.

`.github/workflows/rebuild-bombus-sdm.yml` reconstructs all five focal Bombus SDMs over the common study domain. The model specification is in `config/bombus_sdm.yml`.

The recovered design is:

1. acquire the declared CHELSA v2.1, SoilGrids 2.0 and WorldClim 2.1 predictors on one 30-arc-second grid;
2. query Japanese GBIF occurrences for the five focal species and apply the declared filters;
3. thin occurrences to one record per predictor cell;
4. define species-specific accessible areas;
5. screen environmental predictors;
6. build a target-group background;
7. fit ENMeval `maxnet` candidates under spatial block partitioning; and
8. select the minimum finite AICc candidate and write cloglog support surfaces.

For the manuscript, these SDMs are **environment-derived predicted habitat support**, not visitation, abundance, pollen transfer or selection pressure. Species surfaces are subsequently calibrated against the prediction distribution at observed occurrence cells before the local flower-colour test.

## Frozen manuscript evidence versus live refreshes

GBIF and other public inputs can change. A live source refresh is therefore a new source-build exercise, not an automatic replacement for the manuscript evidence. The current paper uses checksum-locked successful artifacts listed in `paper/analysis-map.md`.

The current directional pollinator analysis uses *B. ardens* and *B. diversus* because they define the documented broad focal-pollinator availability estimand. The other three taxa remain available for Supporting Information community-turnover and montane/elevation guardrails.

Historical source-reconstruction prototypes and superseded analysis architectures are under `legacy/`.
