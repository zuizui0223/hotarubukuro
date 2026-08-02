# Canonical analysis inputs

`canonical_snapshot.json` is the committed descriptor of the immutable
analysis-input snapshot that the canonical analysis workflow starts from. It
records:

- `release_tag` and `asset_name`: where the snapshot lives, as a GitHub Release
  asset rather than an Actions artifact or cache, so it does not expire and is
  not tied to a workflow run;
- `asset_sha256`: the checksum of the archive, verified on every restore;
- `contents`: every member of the archive with its byte size, SHA-256, role,
  provider, and source URL.

The descriptor is produced by the raw external-data reconstruction workflow
(`.github/workflows/raw-data-reconstruction.yml`), which rebuilds everything
upstream of the snapshot from the pinned public sources, validates it, and
publishes it. The checksums are then committed here.

`.github/workflows/canonical-analysis.yml` refuses to run when this descriptor
is missing or when any checksum does not match. Nothing in the snapshot may be
edited by hand; to change its contents, rerun the reconstruction workflow with a
new snapshot identifier and commit the new descriptor. A published snapshot is
never overwritten in place.

See `docs/pipeline-dag.md` for where the snapshot sits in the pipeline.
