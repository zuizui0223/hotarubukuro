# Repository cleanup map

This document classifies material that is **not part of the current paper execution path**. It is preserved for provenance rather than silently deleted.

## Historical analysis populations

Move/retain under legacy:
- `.github/workflows/analysis-1909.yml`
- `.github/workflows/audit-population-1965.yml` once the current 1,965-row flow is the only active population audit
- `scripts/run_analysis_1909.sh`
- `scripts/check_analysis_population.R` if its acceptance criteria remain tied to the historical 1,909 identity
- `inputs/analysis_1909_expectations.csv`
- `docs/analysis-1909.md`
- all of `legacy/published-1923/` (already correctly archived)

Reason: the paper now starts from the curated 1,965-row source table and lets exclusions/support determine downstream n rather than fixing 1,909 or 1,923 upstream.

## Superseded Bombus questions

Move/retain under legacy unless explicitly required as a Supplement sensitivity:
- `.github/workflows/bombus-effective-availability-refined.yml`
- `.github/workflows/bombus-local-sharp-transition.yml`
- `.github/workflows/bombus-relaxation-local-current-inputs.yml` after the adopted local-availability driver is consolidated
- `.github/workflows/bombus-spatial-replication-test.yml`
- corresponding one-off `scripts/run_bombus_*` drivers and dated reproducibility specs/results
- `R/local_bombus_turnover.R`, `scripts/run_local_bombus_turnover.R`, and their tests/validators if turnover is represented only in Supplement

Keep active:
- seeded Bombus source-build machinery and its deterministic rebuild validators
- the adopted current-input local availability/limitation analysis
- minimal sensitivity code needed to substantiate widespread-vs-montane and leave-one-species-out checks

Reason: the Main biological claim is availability/relaxation at local scale. Community turnover is descriptive/supplementary; montane overlap is a confounding guardrail, not a second Main mechanism.

## Superseded reconstruction wrappers

Move/retain under legacy after the current-input path is consolidated:
- `.github/workflows/reanalysis-current-inputs.yml` if replaced by one canonical paper workflow
- `.github/workflows/resume-reanalysis-current-inputs.yml`
- `scripts/resume_reanalysis_from_fresh_v16.sh`
- old reporting wrappers that only describe failed/intermediate attempts

Keep active:
- one canonical current-input rebuild driver
- one downstream paper driver
- deterministic environment setup, snapshot restore, source acquisition, and validation required by those two drivers

## Superseded manuscripts and editorial notes

Move/retain under legacy:
- `manuscript/ecology-and-evolution-manuscript.md`
- `manuscript/ecology-and-evolution-manuscript-final.md`
- obsolete E&E figure/story maps once their useful content is represented in `submission/jbi/`

Keep active:
- `submission/jbi/JBI_main_manuscript_anonymized.md`
- title page, cover letter, figure plan, supporting-information outline, translated abstract, checklist, and JBI validator
- design/background dependency notes that directly guide the current manuscript

## Reproducibility notes

Dated specs and result narratives from exploratory attempts should be archived by topic under `legacy/attempts/`. The active `reproducibility/` directory should eventually contain only:

1. current paper input/source locks,
2. current paper result/claim registry,
3. seed/environment/package provenance,
4. independent validation summaries,
5. the final YAMAP benchmark used in the paper/Supplement.

## Workflows

The desired final `.github/workflows/` surface is small:

- `paper-analysis.yml` — canonical adopted analysis from current upstream inputs
- `rebuild-bombus-sdm.yml` — deterministic SDM source build
- `jbi-submission-format.yml` — submission-format guard
- `pr-checks.yml` / `repository-checks.yml` — lightweight integrity checks
- YAMAP benchmark workflow only if the benchmark remains a generated Supplement result

All historical workflow files should be stored as inert text under `legacy/workflows/`; files under `legacy/` must never trigger Actions.

## Safety rule for the migration

Do not weaken reproducibility by deleting history. A migration is complete only when:

- every active manuscript claim maps to an active result/registry;
- the canonical pipeline can run without importing `legacy/`;
- source-build locks and seeds are unchanged;
- legacy files are inert but discoverable;
- repository checks reject undeclared active executable files;
- the JBI manuscript and Supplement point only to adopted outputs.
