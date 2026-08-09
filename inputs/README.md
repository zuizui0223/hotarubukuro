# Current input boundary

The current manuscript does **not** use the historical 1,909-observation population as its active flower-colour dataset.

## Manuscript-facing phenotype inputs

- `Data_S1.csv` at repository root is the curated derived flower-colour/source table.
- The fresh 1,965-row reconstruction and 1,922-observation phenotype are rebuilt by `.github/workflows/reanalysis-current-inputs.yml` using the source-build utilities and pinned upstream implementation documented in `paper/analysis-map.md`.
- Fresh Bombus support and public-source benchmark inputs used by the manuscript are restored from checksum-locked workflow artifacts.

## Why `canonical_snapshot.json` remains here

`canonical_snapshot.json` is retained only because the current anomaly/human-context downstream script restores **static human-landscape support files** from that immutable bundle (WorldPop, MLIT land-use and DID caches). It is **not** the active flower-colour population definition and its old 1,909 identity must not be used to validate the current manuscript.

`scripts/canonical_snapshot.sh` therefore remains current infrastructure for restoring those static files. The old 1,909 population expectation file and its population-check architecture are under `legacy/`.

For the current evidence map, start with `paper/README.md` and `paper/active-file-map.csv`.
