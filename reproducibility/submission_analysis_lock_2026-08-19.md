# Submission analysis lock

Date: 2026-08-19

This file is the shortest map from manuscript claim to executable analysis. Historical alternatives are not co-equal pipelines.

| Manuscript role | Executable path | Frozen interpretation |
|---|---|---|
| Quantitative phenotype | `source_build/extract_color.py` → `source_build/build_data_s1.py` → `scripts/run_phenotype_hurdle.R` | image-derived colour is materialized into the public Data_S1 contract; pigmentation state and pigmented-only conditional intensity are analysed separately |
| Broad geography | `scripts/run_broad_environment_spatial_audit.R` | environment + stationary SPDE; state and intensity have different supported structure |
| Environment vs spatial continuity | `scripts/build_fixed_space_null_cache.R` → `scripts/fit_broad_supported_term_distance_space_null.R` | state divergence exceeds space-only expectation along supported environmental difference; intensity does not |
| Local Bombus | `scripts/build_bombus_occurrence_reference_support.R` → `scripts/run_bombus_local_sharp_transition.R` | local, heterogeneous correspondence; SDM support is not visitation or selection pressure |
| Bombus guardrail | `scripts/run_bombus_spatial_replication_test.R` | broad/high-elevation overlap is not promoted when the equal-elevation/spatial guardrail removes it |
| Human context | `scripts/fit_final8_presence_null.R` → `scripts/run_continuous_colour_isolation.R` | same-colour isolation of all cells is associated with population exposure beyond natural-map expectation; exploratory, not provenance or causation |

## Source-build clarification

The former root-level `Code_S1.py` was a GPX/photo-time georeferencing utility, not the image-colour extractor. It is archived at `legacy/Code_S1_georeference.py` for provenance and is **not part of the executable submission path above**. The active raw-image route is wrapped by `source_build/reproduce_from_zenodo.py`.

The committed `Data_S1.csv` remains the frozen public analysis-input contract and fast default input; a zero-from-Zenodo run reconstructs `Data_S1_from_zenodo.csv`, audits it against that frozen contract, and then supplies the rebuilt table itself downstream.

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

## Superseded paths removed from the publication repository surface

- hotspot/candidate ranking and the 16-event local-departure primary;
- DID and MLIT land-cover candidate classification;
- coefficient-weighted or inertia-style Broad null variants;
- development-only interaction screens and their one-off CI workflows.

Git history retains their provenance. They should not be restored as alternate manuscript analyses unless the scientific estimand itself changes.
