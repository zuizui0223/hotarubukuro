# Reproducibility records

`submission_analysis_lock_2026-08-19.md` is the current shortest map from manuscript claims to executable code. `source_contract.json` freezes the canonical Zenodo source and the exact identity of the deterministic generated public table.

The other dated files record scientific decisions, benchmark results and robustness checks that led to the retained analysis. They are evidence/provenance records, not alternate runnable pipelines.

Current execution always starts from:

```text
Zenodo Supplementary_Table_S1.xlsx
-> source_build/reproduce_from_zenodo.py
-> results/source_reconstruction/Data_S1_from_zenodo.csv
-> run_pipeline.py reproduce
```

Historical workflow IDs, removed wrapper paths or old CI names in Git history should not be interpreted as current entry points. The active CI workflow is `.github/workflows/analysis-ci.yml`.
