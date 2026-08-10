# Archived reproducibility utilities

These scripts are retained for provenance but are not part of the current JBI execution path.

- `preflight.R` and `write_reproducibility_report.R` belong to the older generic canonical-run interface and expect the superseded pipeline-stage registry.
- `write_submission_analysis_lock.R` encodes the earlier 18-candidate submission lock and historical final-registry outputs.
- `stage_canonical_snapshot.R` and `verify_canonical_snapshot.R` publish/verify the older full canonical analysis snapshot. The current paper retains `inputs/canonical_snapshot.json` and `scripts/canonical_snapshot.sh` only to restore checksum-locked static human-context support files.

Do not call files in this directory from active workflows. Current manuscript-facing infrastructure is listed in `paper/active-file-map.csv`.
