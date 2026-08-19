# hotarubukuro

Analysis code for the *Campanula punctata* flower-colour project.

This public repository is intentionally limited to reusable analysis/source-building code, tests, safe method-level provenance, and one derived analysis table needed for reproducibility.

## Public derived data

- `Data_S1.csv` — derived analysis table intentionally retained so the public analysis code has a stable reproducibility input.

Original third-party photographs, manuscript drafts, journal-submission materials, author metadata, cover letters, review bundles, and paper-level frozen binary payloads are not stored in the public repository.

## Main code areas

- `R/` — reusable statistical and spatial-analysis functions
- `scripts/` — analysis entry scripts
- `analysis_sensitivity/` — focused robustness and diagnostic analyses
- `source_build/` — source acquisition and data-construction code
- `config/` — analysis configuration
- `tests/` and `validation/` — unit and consistency checks
- `.github/workflows/` — analysis workflows

## Data boundary

`Data_S1.csv` is the deliberate exception to the general no-data rule because it is required as a derived input for reproducible analysis. Raw/private source material must remain outside this repository.
