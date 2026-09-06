# Repository scope

This public repository intentionally contains only material needed to reconstruct and audit the retained publication analysis.

## Active execution surface

1. `source_build/reproduce_from_zenodo.py` — canonical raw-data bootstrap.
2. `run_pipeline.py` — single downstream orchestrator.
3. `source_build/`, `R/`, `scripts/`, `config/`, `dependencies/` — code/configuration called by those entry points.
4. `reproducibility/` — exact source contract plus scientific decision/result locks.
5. `tests/` and `.github/workflows/analysis-ci.yml` — executable contract checks.

## Intentionally absent from the active tree

- committed derived colour tables;
- historical GPX `Code_S1` utility;
- superseded analysis variants and one-off workflow wrappers;
- orphan validation/source-build helpers;
- manuscript/submission bundles;
- generated raster/model/result payloads.

These remain recoverable from Git history or their public upstream sources where applicable. Their absence is deliberate: they must not appear as co-equal current analysis routes.
