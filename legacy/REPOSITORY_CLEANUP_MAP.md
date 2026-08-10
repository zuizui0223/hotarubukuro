# Repository cleanup map

This document classifies material that is **not part of the current JBI paper execution or submission path**. Historical material is preserved for provenance rather than silently deleted.

## Governing rule

A file remains outside `legacy/` if it is required to:

1. reconstruct current source data or Bombus support;
2. run or validate an adopted Main analysis;
3. run or validate an adopted Supporting Information analysis or negative guardrail;
4. build current figures/tables or the YAMAP benchmark;
5. explain the current JBI narrative/background architecture; or
6. prepare/validate the current JBI submission package.

Only files outside all six categories belong in `legacy/`.

## Historical analysis populations

Keep under legacy:

- old `.github/workflows/analysis-1909.yml`;
- historical fixed-population audit wrappers tied to 1,909;
- `scripts/run_analysis_1909.sh` and its historical population checker/expectations;
- old `docs/analysis-1909.md` and result registries tied to that identity;
- all of `legacy/published-1923/`.

Reason: the current paper starts from the curated 1,965-row source table. Current QC/support rules produce the current phenotype population (1,922 in the adopted broad/anomaly reference); neither 1,909 nor 1,923 defines the present pipeline.

## Bombus analyses

### Current — do **not** archive

- `.github/workflows/bombus-occurrence-reference-support.yml`;
- `scripts/build_bombus_occurrence_reference_support.R`;
- `.github/workflows/bombus-local-sharp-transition.yml`;
- `scripts/run_bombus_local_sharp_transition.R`;
- current sharp-transition spec/results in `reproducibility/`;
- `.github/workflows/bombus-spatial-replication-test.yml`;
- `scripts/run_bombus_spatial_replication_test.R`;
- current spatial-replication spec/results in `reproducibility/`;
- seeded Bombus SDM source-build machinery and deterministic rebuild validation.

Reason: the 5-km occurrence-referenced *B. ardens* + *B. diversus* sharp-transition analysis is the adopted Main pollinator test. The five-species community-boundary analysis and equal-elevation montane test are adopted Supporting Information / negative guardrails.

### Historical — keep under legacy

- old all-five lower-third limitation-gate workflows/scripts/validators;
- superseded relaxation/local-contrast variants;
- earlier effective-availability refinements that preceded the adopted sharp-transition test;
- national Bombus fingerprint/increment formulations when treated as a Main mechanism;
- old unsigned turnover implementation superseded by the current spatially matched five-species boundary analysis;
- one-off diagnostics/specifications whose numerical identities are not used by the current Main or SI.

## Current-input reconstruction

Keep active:

- `.github/workflows/reanalysis-current-inputs.yml`;
- `scripts/run_reanalysis_current_inputs.sh`;
- `scripts/run_downstream_current_inputs.sh`;
- current natural-model, isolate, human-context and predictive-calibration drivers/validators;
- deterministic environment/snapshot/source support still required by these stages.

Archive only obsolete resume wrappers and failed/intermediate reconstruction attempts.

## Manuscript and submission material

### Current — do **not** archive

The authoritative submission package is `submission/jbi/`, including:

- `JBI_main_manuscript_anonymized.md`;
- `JBI_title_page_template.md`;
- `JBI_cover_letter.md`;
- `JBI_main_figure_plan.md`;
- `JBI_supporting_information_outline.md`;
- `JBI_translated_abstract_ja.md`;
- `JBI_submission_checklist.md`;
- `JBI_background_architecture.md`;
- `JBI_narrative_dependency_architecture.md`;
- `supporting/`;
- `validate_jbi_submission.py`.

The background/narrative architecture files are current editorial logic, not historical notes: they encode the JBI framing of measurement → attribution → exception and the dependent sequence from YAMAP trait construction to broad geography, local Bombus inference and calibrated departures.

### Historical — keep under legacy

- Ecology & Evolution manuscript drafts and associated figure/story maps;
- duplicate manuscript-development notes whose current content is already represented in `submission/jbi/`;
- superseded cover letters/checklists/submission references.

## Reproducibility material

The active `reproducibility/` surface should contain evidence needed by the current paper, especially:

- `final_paper_pipeline_2026-08-09.md`;
- current broad/anomaly reference;
- current occurrence-reference Bombus support note;
- current sharp-transition spec/results;
- current spatial-replication/community-boundary + montane guardrail spec/results;
- YAMAP benchmark specification/results;
- current package/source/seed locks needed to reproduce those analyses.

Exploratory dated narratives that no longer support a Main/SI result belong under `legacy/reproducibility-development/`.

## Active workflows

The active Actions surface may include separate workflows when they correspond to distinct adopted/reproducible components:

- `reanalysis-current-inputs.yml` — current broad/anomaly reconstruction;
- `rebuild-bombus-sdm.yml` — deterministic Bombus SDM source build;
- `bombus-occurrence-reference-support.yml` — focal support calibration;
- `bombus-local-sharp-transition.yml` — Main 2;
- `bombus-spatial-replication-test.yml` — Supplement community boundary + montane guardrail;
- `yamap-public-database-benchmark.yml` and overlap audit — Appendix S1 evidence;
- `final-paper-analysis.yml` — scientific integration;
- `jbi-submission-format.yml` — submission-format guard;
- `paper-checks.yml` — active/legacy boundary and paper integrity checks.

Historical workflows are stored as inert text under `legacy/workflows/`; nothing under `legacy/` should trigger Actions.

## Migration safety rules

The cleanup is complete only when:

- every Main and SI claim maps to an active implementation or frozen current artifact;
- the current pipeline executes without importing `legacy/`;
- source-build locks and seeds are preserved;
- YAMAP benchmark/Table S1 remains in the active submission path;
- sharp-transition Main 2 and the spatial/montane Supplement remain active;
- JBI narrative/background architecture remains in `submission/jbi/`;
- old 1,909/1,923 identities and superseded Bombus formulations are visibly historical;
- repository checks reject undeclared active executable files and active references to `legacy/`.
