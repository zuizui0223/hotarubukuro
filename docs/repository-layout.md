# Repository layout after the 2026-08-18 freeze

The repository is intentionally organized around one manuscript-facing execution path.

## Stable public interfaces

- `python run_pipeline.py audit`
- `python run_pipeline.py reproduce`
- installed CLI: `hotarubukuro-paper`
- installed colour CLI: `hotarubukuro-color`
- package namespace: `hotarubukuro_analysis`

The root `run_pipeline.py` and `source_build` modules remain as compatibility locations. Installed console scripts route through `hotarubukuro_analysis.cli` so new code has one package namespace without changing the accepted scientific implementation.

## Active scientific tree

- `config/` — frozen execution and source configuration.
- `R/` — reusable R analysis functions.
- `scripts/` — manuscript-facing R/Python command adapters.
- `analysis_sensitivity/` — retained sensitivity analyses that are part of the accepted claim hierarchy.
- `source_build/` — reproducible source preparation code.
- `validation/` and `tests/` — numerical, manuscript, package and submission contracts.
- `paper/` and `submission/jbi/` — active manuscript map and submission source.
- `reproducibility/` — frozen numerical results, provenance and archival manifests.

## Removed from the active tree

`legacy/` was removed after freezing commit `ddb1c262b8332c94f4b94e572ab930a648e59553` on `archive/jbi-final-2026-08-18-pre-cleanup`. No accepted scientific result depends on `legacy/`; historical implementations remain recoverable from Git history and the preservation branch.

## Cleanup rule

A file belongs in the active tree only if it is one of the following: an execution entry point, current manuscript/submission source, accepted analysis component, reproducibility/provenance record, source-build component, dependency declaration, test/validator, or workflow that directly exercises one of those roles. Experimental or superseded implementations should not be reintroduced into the active root; preserve them in Git history or a dedicated archive ref instead.
