# Current input boundary

The current manuscript does **not** use the historical 1,909-observation population or the old canonical snapshot as an active flower-colour input definition.

## Manuscript-facing source boundary

- `Data_S1.csv` at repository root is the curated derived flower-colour/source table.
- The environment-complete integrated analysis contains 1,922 observations after the documented topographic/soil support exclusions.
- Current Broad, Bombus and human analyses restore their frozen source/evidence objects from checksum-locked GitHub Actions artifacts listed in `paper/analysis-map.md` and the dated reproducibility locks.

## Historical canonical snapshot

The former `inputs/canonical_snapshot.json` and `scripts/canonical_snapshot.sh` were used to restore static WorldPop/MLIT/DID support during the earlier current-input downstream reconstruction. The final current-Broad human workflow now restores the frozen static-human/source evidence directly from checksum-locked artifacts, so the snapshot restorer is no longer an active execution dependency.

Both files are preserved for provenance at:

`legacy/reproducibility-development/superseded-current-input-anomaly-pipeline-2026-08-12/`

For the current evidence boundary, start with `paper/README.md`, `paper/analysis-map.md` and `paper/active-file-map.csv`.
