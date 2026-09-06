# Submission analysis lock

Date: 2026-08-19  
Execution-surface cleanup: 2026-09-06

This file is the shortest map from manuscript claim to the current executable analysis. Historical alternatives are not co-equal pipelines.

## Canonical source

The only canonical colour-data source is Zenodo record `22334596`, file `Supplementary_Table_S1.xlsx`. `source_build/reproduce_from_zenodo.py` verifies its MD5, reconstructs the public table, and requires the exact generated-table identity in `reproducibility/source_contract.json` before downstream analysis can begin.

No derived colour CSV is committed as an active input and `run_pipeline.py` accepts no alternative colour-table path.

| Manuscript role | Executable path | Frozen interpretation |
|---|---|---|
| Quantitative phenotype | `source_build/extract_color.py` → `source_build/build_data_s1.py` → `scripts/run_phenotype_hurdle.R` | image-derived colour is materialized into the deterministic public-table contract; pigmentation state and pigmented-only conditional intensity are analysed separately |
| Broad geography | `scripts/run_broad_environment_spatial_audit.R` | environment + stationary SPDE; state and intensity have different supported structure |
| Environment vs spatial continuity | `scripts/build_fixed_space_null_cache.R` → `scripts/fit_broad_supported_term_distance_space_null.R` | state divergence exceeds space-only expectation along supported environmental difference; intensity does not |
| Local Bombus | `scripts/build_bombus_occurrence_reference_support.R` → `scripts/run_bombus_local_sharp_transition.R` | local, heterogeneous correspondence; SDM support is not visitation or selection pressure |
| Bombus guardrail | `scripts/run_bombus_spatial_replication_test.R` | broad/high-elevation overlap is not promoted when the equal-elevation/spatial guardrail removes it |
| Human context | `scripts/fit_final8_presence_null.R` → `scripts/run_continuous_colour_isolation.R` | same-colour isolation of all cells is associated with population exposure beyond natural-map expectation; exploratory, not provenance or causation |

The orchestration of these stages is defined only by `run_pipeline.py`.

## Source-build clarification

The historical root-level `Code_S1.py` was a GPX/photo-time georeferencing utility, not the image-colour extractor. It is absent from the active tree; Git history preserves it. The current image-colour extractor is `source_build/extract_color.py`.

The former committed derived table is also absent from the active tree. Its validated exact Git-blob identity is retained in `reproducibility/source_contract.json`, so a new Zenodo reconstruction must reproduce the historical public table exactly without treating that table as a second source.

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
