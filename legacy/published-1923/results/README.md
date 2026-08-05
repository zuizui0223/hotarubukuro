# Results provenance

`final_analysis_pipeline/` is the publication handoff. Its result, claim,
exclusion, checksum, stage, and publication-stage registries define the files
and interpretations retained for the paper.

The committed scientific summaries come from these frozen result sets:

| Publication role | Provenance directory |
|---|---|
| Two-part phenotype | `ecological_v11_pigmentation_hurdle/` |
| Multiscale cell context | `ecological_v15_multiscale_hotspots/` |
| Natural predictive baseline | `ecological_v16_predictive_replication/` |
| Local *Bombus* turnover | `ecological_v17_local_pair_turnover/` |
| Local pigmented-isolate definition | `ecological_v20_local_white_isolates/` |
| Local population context | `ecological_v21_local_human_neighbourhood/` |
| DID sensitivity | `ecological_v22_did_human_context/` |

Version labels are retained only as immutable provenance identifiers. Executable
code uses stable stage names under `R/`, `scripts/`, and `validation/`.

Large rasters, model objects, caches, logs, and development outputs are not
versioned. The final registries record the required compact artifacts and input
checksums. Results from the superseded all-flower continuous-a* workflow,
residual-response regressions, and development horticultural classifiers are
not part of the publication inference.
