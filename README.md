# hotarubukuro

Range-wide flower-colour biogeography in *Campanula punctata* using author-reviewed YAMAP hiking photographs.

## Current paper — start here

**[`paper/README.md`](paper/README.md)** is the single entry point for the manuscript-facing project.

Current first-choice target: **Journal of Biogeography**.

Active submission manuscript:

- `submission/jbi/JBI_main_manuscript_anonymized.md`

Active analysis hierarchy:

1. **YAMAP / iEcology data layer** — recreational GPS-linked photographs -> author screening -> quantitative two-part flower-colour phenotype;
2. **Main 1: final Broad natural template** — eight measured abiotic axes + continuous spatial structure; pigmentation state additive, pigmented-only intensity with Temperature PC1 × temperature-seasonality;
3. **Main 2: local focal-pollinator test** — 67 Bombus/environment-blind sharp nearby white-pigmented boundaries versus occurrence-referenced support of *Bombus ardens* + *B. diversus*;
4. **Main 3: current-Broad local departures** — final-eight-axis local events replayed on 10,000 natural predictive maps before human context is examined; 16 observed candidates;
5. **Supporting Information** — YAMAP/public-database benchmark, Broad guardrails, Bombus SDMs, five-species community turnover, montane/elevation negative guardrail and current human-context robustness.

The machine-readable map of manuscript-facing files is `paper/active-file-map.csv`; the biological evidence hierarchy is `paper/analysis-map.md`.

## Current acceptance boundary

`.github/workflows/paper-checks.yml` treats the current repository as a closed, explicitly classified interface. Current manuscript, workflow, analysis, source-build, validation, test, configuration, dependency, documentation and reproducibility files must be registered in `paper/active-file-map.csv`.

The check also rejects superseded execution paths from the current interface, validates workflow reachability, parses every active R source, validates the JBI package and runs the current R/Python unit tests. Historical result-identity audits remain under `legacy/`.

## Legacy

Historical manuscripts, abandoned estimands, obsolete Bombus gates, the former four-PC/17-candidate downstream implementation, development diagnostics and superseded workflows belong under **`legacy/`**. They are retained for provenance only and are not current manuscript evidence.

Do not infer the active paper from file age, old stage numbers or statistical significance. Use `paper/README.md`, `paper/analysis-map.md` and `paper/active-file-map.csv`.

## Reproducibility boundary

The current environment-complete analysis contains 1,922 phenotype observations in 1,305 1-km cells. Manuscript-facing numerical claims are tied to checksum-locked GitHub Actions artifacts listed in `paper/analysis-map.md` and the canonical current lock:

- `reproducibility/final_integrated_pipeline_2026-08-12.md`.

Repository/Supp/manuscript consistency is summarized in:

- `FINAL_PIPELINE_SUPP_MANUSCRIPT_AUDIT_2026-08-12.md`.

Original YAMAP photographs are third-party content and are not redistributed.
