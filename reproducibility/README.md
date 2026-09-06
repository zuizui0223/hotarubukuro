# Reproducibility records

`submission_analysis_lock_2026-08-19.md` is the shortest map from manuscript claims to executable code. `source_contract.json` freezes the canonical Zenodo source and the exact identity of the deterministic **lean analysis table** generated from it.

Contract v2 keeps two identities deliberately separate:

- historical full `Data_S1.csv` blob `74b951898814f4ed15f314061e3129d8b05823d5` — provenance only;
- current 38-column analysis-table blob `e119137efac89cbcfd789236f3d6a3c9599575af` and SHA-256 `9e543b64a824aff82dbb55da1bca8843fb337a51399bfd60ad0a09c9bca3c33c` — executable input contract.

The full technical image-extraction record is generated locally and can contain run-time/audit-only fields such as `processed_at`, candidate colour diagnostics and historical RGB comparisons. Those fields are not part of the retained ecological-analysis input unless the active code actually consumes them.

The other dated files record scientific decisions, benchmark results and robustness checks that led to the retained analysis. They are evidence/provenance records, not alternate runnable pipelines.

Current execution always starts from:

```text
Zenodo Supplementary_Table_S1.xlsx
-> source_build/reproduce_from_zenodo.py
-> rich colour_extraction_from_zenodo.csv
-> lean Data_S1_from_zenodo.csv
-> exact source-contract validation
-> run_pipeline.py reproduce
```

Historical workflow IDs, removed wrapper paths or old CI names in Git history should not be interpreted as current entry points. The active CI workflow is `.github/workflows/analysis-ci.yml`.
