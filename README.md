# hotarubukuro

Range-wide flower-colour biogeography in *Campanula punctata* using author-reviewed YAMAP hiking photographs.

## Current paper — start here

**[`paper/README.md`](paper/README.md)** is the single entry point for the manuscript-facing project.

Current first-choice target: **Journal of Biogeography**.

Active submission manuscript:

- `submission/jbi/JBI_main_manuscript_anonymized.md`

Active analysis hierarchy:

1. **YAMAP / iEcology data layer** — recreational GPS-linked photographs -> author screening -> quantitative two-part flower-colour phenotype;
2. **Main 1: broad natural template** — national environment + continuous spatial structure for pigmentation state and pigmented-only intensity;
3. **Main 2: local focal-pollinator test** — abrupt nearby white-pigmented boundaries versus predicted availability of *Bombus ardens* + *B. diversus*;
4. **Main 3: event-based local departures** — repeatable pigmented-in-white neighbourhood events calibrated against natural predictive maps before human context is examined;
5. **Supporting Information** — YAMAP/public-database benchmark, five-species community turnover, montane/alpine guardrail and robustness families.

The machine-readable map of manuscript-facing files is `paper/active-file-map.csv`; the biological evidence hierarchy is `paper/analysis-map.md`.

## Current acceptance boundary

`.github/workflows/paper-checks.yml` treats the current repository as a closed, explicitly classified interface. Manuscript, workflow, analysis, source-build, validation, test, configuration, dependency, input, result-documentation and reproducibility files must all be registered in `paper/active-file-map.csv`.

The check also requires that current validators have an active execution route, parses every active R source, validates the JBI package, and runs the current R and Python unit tests. Reconstruction-specific numerical findings are kept distinct from structural PASS/FAIL checks; historical result-identity audits remain under `legacy/`.

## Legacy

Historical manuscripts, abandoned estimands, obsolete Bombus gates, development diagnostics and superseded workflows belong under **`legacy/`**. They are retained for provenance only and are not current manuscript evidence.

Do not infer the active paper from file age, old stage numbers or statistical significance. Use `paper/README.md` and `paper/active-file-map.csv`.

## Reproducibility boundary

The current paper uses a fresh 1,965-row source reconstruction yielding 1,922 phenotype observations in 1,305 1-km cells. Manuscript-facing numerical claims are tied to checksum-locked GitHub Actions artifacts listed in `paper/analysis-map.md` and `reproducibility/final_paper_pipeline_2026-08-09.md`.

Original YAMAP photographs are third-party content and are not redistributed.
