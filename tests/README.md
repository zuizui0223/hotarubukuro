# tests

Automated tests for the retained source-build and analysis code.

- `python/` checks image extraction, deterministic public-table materialization and the Zenodo exact-output source contract.
- `testthat/` checks reusable R analysis functions.
- `testthat.R` is the R test entry point used by CI.

Tests do not depend on manuscripts, submission bundles or private research payloads.
