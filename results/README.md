# Generated results

Committed `results/` tables are working/generated products rather than the manuscript evidence boundary. Manuscript claims are tied to checksum-locked GitHub Actions artifacts and dated reproducibility records listed in `paper/analysis-map.md`.

Current manuscript-facing generation routes are:

- Broad interaction screen: `.github/workflows/environment-interaction-inla-screen.yml`;
- Broad environmental/spatial audit: `.github/workflows/broad-environment-spatial-audit.yml`;
- local focal-Bombus test: `.github/workflows/bombus-local-sharp-transition.yml`;
- fixed-pair final-eight-axis Bombus balance audit: `.github/workflows/bombus-final8-environment-audit.yml`;
- Bombus community/elevation guardrails: `.github/workflows/bombus-spatial-replication-test.yml`;
- 10,000-map local-departure/human replay: `.github/workflows/human-context-highrep-final.yml`;
- Figure 1–4 bundle: `.github/workflows/jbi-main-figures.yml`;
- integrated manuscript/SI numerical audit: `.github/workflows/final-paper-analysis.yml`.

Generated outputs normally remain Actions artifacts. A small derived table or summary is committed only when it serves as a stable reproducibility lock, manuscript/SI source or validator fixture.

For exact artifact IDs, SHA-256 digests and expected numerical values, use `paper/analysis-map.md` and `reproducibility/final_integrated_pipeline_2026-08-12.md`.
