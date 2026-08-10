# Legacy material

Everything under `legacy/` is **historical provenance, method development or superseded manuscript material**. It is not current evidence for the active paper unless `paper/analysis-map.md` explicitly says otherwise.

The active paper starts at [`../paper/README.md`](../paper/README.md).

## What belongs here

- `published-1923/` — preserved 1,923-observation publication architecture, manuscript, figures, fixed outputs and historical workflows.
- `manuscript-development/` — superseded Ecology & Evolution drafts, storyboards, figure plans and internal novelty notes replaced by the current JBI submission package.
- `method-development/` — obsolete analysis estimands, interpretation notes and code paths, including the old five-species Bombus limitation gate, relaxation/local-contrast variants, pre-consolidation Bombus notes, old publication orchestrator and superseded local-turnover implementation.
- `workflows/` — CI/workflow definitions tied to historical analysis identities or exploratory branches.
- `reproducibility-development/` — superseded analysis specs/results, one-time patches and old stage registries.
- `source-build-prototypes/` — superseded Bombus source/projection builders and standalone source audits replaced by the current common-mainland source build.
- `reporting/` — historical report-generation material not used by the current paper.
- `diagnostics/` — post-hoc diagnostics excluded from the manuscript pipeline.
- `implementations/frozen-upstream/` — frozen code that generated earlier upstream boundaries and remains available for provenance.
- `reconstruction-prototypes/` — historical public-reconstruction and model-reselection experiments.

## Why material is retained instead of deleted

The project involved genuine scientific iteration: alternative pollinator estimands, scale choices, candidate definitions, source-build routes and manuscript architectures were tested and then rejected, replaced or demoted when they did not answer the final ecological question cleanly. Keeping those files makes the history auditable without making readers guess which result is current.

## Legacy rule

1. Do not import executable code from `legacy/` into the current manuscript pipeline.
2. Do not cite a legacy result as a current result.
3. Do not infer narrative priority from statistical significance in historical analyses.
4. If a historical analysis informed a design decision, cite it only as method-development provenance and point to the current replacement.
5. If two current-looking files answer the same question, consolidate them before submission and archive the redundant route here.

Legacy files are not guaranteed to remain runnable against the current repository layout.
