# hotarubukuro

Reproducible range-wide flower-colour biogeography in *Campanula punctata* from author-reviewed YAMAP hiking photographs.

## Current paper — start here

The paper follows one analysis chain:

`YAMAP/iEcology -> two-part flower-colour phenotype -> broad environment + space -> local focal-Bombus boundaries -> calibrated local departures -> post-selection human context`

For readers who want to reproduce or audit the study:

- **Scientific overview and exact entry points:** [`paper/README.md`](paper/README.md)
- **Analysis-to-evidence map:** [`paper/analysis-map.md`](paper/analysis-map.md)
- **Machine-readable active-file registry:** [`paper/active-file-map.csv`](paper/active-file-map.csv)
- **Reproduction guide:** [`docs/reproduction-guide.md`](docs/reproduction-guide.md)
- **Current submission sources:** [`submission/jbi/`](submission/jbi/)

## What is reproducible

The environment-complete analysis contains **1,922 observations in 1,305 1-km cells**. The repository exposes the derived trait table, analysis code, model definitions, seeds, validation rules, figure builders and checksum-locked evidence provenance used for manuscript claims.

The current inferential stages are:

1. **Image phenotype:** author-reviewed records are converted to a two-part flower-colour phenotype: white/pigmented state and pigmented-only visible intensity.
2. **Broad natural template:** eight measured abiotic axes and continuous spatial structure are fitted separately to state and conditional intensity.
3. **Local focal-pollinator test:** 67 nearby white-pigmented boundaries, selected without Bombus information, are compared with occurrence-referenced habitat support for *Bombus ardens* and *B. diversus*.
4. **Local departures:** the finalized eight-axis natural reference defines 16 observed local events and replays the same detector over 10,000 predictive maps before human context is examined.
5. **Supporting analyses:** sampling-frame benchmarking, model guardrails, Bombus SDM diagnostics, community turnover, elevation controls and human-context robustness constrain the interpretation of the Main results.

## Reproduce the paper

The preferred audit route is the versioned GitHub Actions workflows because large raster-derived and predictive-draw inputs are checksum locked. Start with [`docs/reproduction-guide.md`](docs/reproduction-guide.md), which identifies the workflow and evidence artifact for each manuscript stage.

The repository distinguishes three things explicitly:

- **source/derived data** that can be redistributed;
- **third-party inputs** that must be reacquired or restored from declared sources/artifacts;
- **generated outputs** that should be reproducible from the declared workflow and locked evidence.

Original YAMAP photographs are third-party content and are **not redistributed**. `Data_S1.csv` is the distributable derived observation/trait table used by the analysis.

## Transparency and validation

`paper/active-file-map.csv` defines the public manuscript-facing interface. CI verifies that active scripts and validators are reachable, source files parse, submission text is internally consistent, and numerical claims agree with the locked evidence hierarchy.

The canonical integrated evidence lock is:

- [`reproducibility/final_integrated_pipeline_2026-08-12.md`](reproducibility/final_integrated_pipeline_2026-08-12.md)

Submission-facing consistency is independently summarized in:

- [`FINAL_PIPELINE_SUPP_MANUSCRIPT_AUDIT_2026-08-12.md`](FINAL_PIPELINE_SUPP_MANUSCRIPT_AUDIT_2026-08-12.md)

## Repository structure

- `paper/` — scientific entry point, evidence map and active-file registry
- `R/` — reusable analysis functions
- `scripts/` — executable analysis, figure and submission builders
- `source_build/` — declared source-construction utilities
- `analysis_sensitivity/` — manuscript-relevant robustness analyses
- `validation/` and `tests/` — independent checks
- `reproducibility/` — frozen model decisions, provenance and numerical locks
- `submission/jbi/` — current manuscript, Supporting Information and submission validators
- `legacy/` — provenance archive outside the current reproduction path
