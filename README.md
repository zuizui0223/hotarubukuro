# hotarubukuro

Analysis code for quantitative flower-colour and spatial-ecology workflows in *Campanula punctata*.

## Public repository scope

This public repository intentionally contains analysis code, reusable functions, tests, and method-level provenance only.

The following are **not published in this repository**:

- manuscripts, drafts, journal targeting or submission strategy;
- title pages, cover letters, reviewer bundles, portal checklists, and journal-specific files;
- derived supplementary datasets prepared for submission;
- exact-reproduction binary payloads and private research data;
- original third-party YAMAP photographs.

These materials must remain outside the public Git repository.

## Repository map

- `R/` — reusable R functions.
- `analysis_sensitivity/` — focused analysis and diagnostic entry points.
- `scripts/` — analysis utilities and data-processing scripts.
- `source_build/` — source acquisition and data-construction code.
- `validation/` — analysis-level validation.
- `tests/` — automated tests.
- `reproducibility/` — method and analysis provenance that is safe to publish.

## Data policy

Do not commit private images, derived submission datasets, manuscript files, frozen binary inputs, credentials, or personally identifying metadata. Local-only and unpublished materials are excluded through `.gitignore`.
