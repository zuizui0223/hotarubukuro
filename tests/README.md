# Current unit tests

The top-level test suite covers current reusable analysis modules and the public colour-extraction source build.

- `Rscript tests/testthat.R` runs all R tests under `tests/testthat/`.
- `python -m pytest` runs the Python tests declared in `pyproject.toml`.

The R tests check deterministic helper behavior: cell aggregation, predictive-tail rules, response-blind neighbourhood construction, human-context feature definitions, MLIT mesh arithmetic and DID classification. They do not re-run the expensive INLA or raster pipelines.

Tests for a module that has moved to `legacy/` must move with it. `.github/workflows/paper-checks.yml` rejects active test or validation files that are missing from `paper/active-file-map.csv`.
