# Generated results

The active pipeline writes all model outputs, validation tables, and final registries under `results/` during each run. No committed table in this directory is an analysis input or a numerical reference.

Run:

```bash
bash scripts/run_analysis_1909.sh
```

The full GitHub Actions artifact also contains `reproducibility/` logs and provenance. Earlier committed 1,923-observation result directories are preserved under `legacy/published-1923/results/`.
