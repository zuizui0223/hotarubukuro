# Submission analysis lock

Date: 2026-08-19  
Execution-surface cleanup: 2026-09-06

This file is the shortest map from manuscript claim to the current executable analysis. Historical alternatives are not co-equal pipelines.

## Canonical source

The only canonical colour-data source is Zenodo record `22334596`, file `Supplementary_Table_S1.xlsx`. `source_build/reproduce_from_zenodo.py` verifies its MD5, reconstructs the rich image/colour extraction record, materializes the deterministic lean analysis table, and requires the exact lean-table identity in `reproducibility/source_contract.json` before downstream analysis can begin.

No derived colour CSV is committed as an active input and `run_pipeline.py` accepts no alternative colour-table path.

| Manuscript role | Executable path | Frozen interpretation |
|---|---|---|
| Quantitative phenotype | `source_build/extract_color.py` → `source_build/build_data_s1.py` → `scripts/run_phenotype_hurdle.R` | image-derived colour is materialized into the deterministic analysis-table contract; pigmentation state and pigmented-only conditional intensity are analysed separately |
| Broad geography | `scripts/run_broad_environment_spatial_audit.R` | environment + stationary SPDE; state and intensity have different supported structure |
| Environment vs spatial continuity | `scripts/build_fixed_space_null_cache.R` → `scripts/fit_broad_supported_term_distance_space_null.R` | state divergence exceeds space-only expectation along supported environmental difference; intensity does not |
| Local Bombus | `scripts/build_bombus_occurrence_reference_support.R` → `scripts/run_bombus_local_sharp_transition.R` | local, heterogeneous correspondence; SDM support is not visitation or selection pressure |
| Bombus guardrail | `scripts/run_bombus_spatial_replication_test.R` | broad/high-elevation overlap is not promoted when the equal-elevation/spatial guardrail removes it |
| Human context | `scripts/fit_final8_presence_null.R` → `scripts/run_continuous_colour_isolation.R` | same-colour isolation of all cells is associated with population exposure beyond natural-map expectation; exploratory, not provenance or causation |

The orchestration of these stages is defined only by `run_pipeline.py`.

## Source-build clarification

The historical root-level `Code_S1.py` was a GPX/photo-time georeferencing utility, not the image-colour extractor. It is absent from the active tree; Git history preserves it. The current image-colour extractor is `source_build/extract_color.py`.

The former 3.6 MB committed `Data_S1.csv` is also absent from the active tree. Its Git blob `74b951898814f4ed15f314061e3129d8b05823d5` remains provenance only. That historical table contained run-time and audit/development columns that the retained ecological analysis does not consume.

Contract v2 instead freezes the deterministic 38-column analysis projection actually consumed downstream:

- expected rows: 1,965;
- Git blob: `e119137efac89cbcfd789236f3d6a3c9599575af`;
- SHA-256: `9e543b64a824aff82dbb55da1bca8843fb337a51399bfd60ad0a09c9bca3c33c`.

The rich extraction intermediate still retains `processed_at`, candidate colour diagnostics, legacy RGB comparisons and other technical audit fields. Their exclusion from the lean analysis table does not alter the ecological estimand; it prevents unused run-time/development metadata from becoming a second analysis contract.

## Frozen geometry

- 1-km analysis cells: 1,305
- pigmented cells: 674
- white cells: 631
- natural-map replay for final human-context analysis: 10,000

## Human-context headline checks

At the 5-km population scale, the publication implementation reproduces approximately:

- raw pigmented same-colour isolation: Spearman rho = +0.251980;
- density-relative pigmented isolation: Spearman rho = +0.285498.

These are spatial-context associations. They do not establish cultivation, introduction, escape, introgression or causal human movement.

## Superseded paths removed from the active surface

- hotspot/candidate ranking and the 16-event local-departure primary;
- DID and MLIT land-cover candidate classification;
- coefficient-weighted or inertia-style Broad null variants;
- development-only interaction screens and one-off workflows;
- historical GPX utility and orphan validation/source-build helpers.

Git history retains their provenance. They should not be restored as alternate manuscript analyses unless the scientific estimand itself changes.
