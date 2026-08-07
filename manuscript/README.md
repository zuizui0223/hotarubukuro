# Active manuscript products

The active paper uses the 1,909-observation analysis. Figures are regenerated from fresh outputs by:

```bash
Rscript scripts/build_publication_figures.R
```

and written to `manuscript/figures/`. The numerical analysis does not read manuscript files.

The earlier 1,923-observation manuscript, cover letter, submission checklist, and figures are archived under `legacy/published-1923/manuscript/` and are not part of the active pipeline.

For manuscript wording, the local *Bombus* result must be described as a **community-turnover correspondence conditional on the frozen SDM prediction surfaces**. The 1,000 natural-map reference propagates flower natural-model uncertainty and observation design, but it does not propagate uncertainty in occurrence sampling, ENMeval model selection or alternative *Bombus* prediction surfaces. It is therefore not evidence of visitation, interaction strength or pollinator-mediated selection. See `docs/bombus-sdm-inference.md`.
