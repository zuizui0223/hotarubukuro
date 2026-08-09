# Current input boundary

The current manuscript does not use the historical 1,909-observation snapshot as its active analysis population.

## Manuscript-facing inputs

- `Data_S1.csv` at repository root is the curated derived flower-colour/source table.
- The fresh 1,965-row reconstruction and 1,922-observation phenotype are rebuilt by `.github/workflows/reanalysis-current-inputs.yml` using the source-build utilities and pinned upstream implementation documented in `paper/analysis-map.md`.
- Large Bombus, raster and benchmark inputs used for the manuscript are restored from checksum-locked workflow artifacts rather than committed as opaque result directories.

The historical `canonical_snapshot.json` has been moved to `legacy/reproducibility-development/inputs/` because it belongs to the superseded 1,909 architecture.

For the current evidence map, start with `paper/README.md` and `paper/active-file-map.csv`.
