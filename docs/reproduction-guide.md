# Reproducing the manuscript-facing analyses

This guide gives a direct route from each manuscript claim to its executable workflow and checksum-locked evidence. It intentionally describes only the adopted analysis.

Start with:

- `paper/README.md` for the scientific hierarchy;
- `paper/analysis-map.md` for claim-to-artifact mapping;
- `paper/active-file-map.csv` for the machine-readable current interface;
- `reproducibility/final_integrated_pipeline_2026-08-12.md` for the integrated numerical lock.

## Reproducibility model

The paper combines executable code with frozen evidence artifacts. This is deliberate: several analyses depend on large raster-derived inputs, fitted spatial models and predictive draws that should not be silently replaced by newer external data.

A manuscript audit therefore has two valid modes:

1. **Exact evidence audit:** restore the checksum-locked artifact recorded in `paper/analysis-map.md`, rerun the corresponding validator and confirm the manuscript numbers.
2. **Full scientific rerun:** rebuild the declared inputs and rerun the analysis workflow. Any refreshed external source is treated as a new analysis and should be compared explicitly before replacing a manuscript lock.

## Stage 1 — phenotype construction

Distributed analysis table:

- `Data_S1.csv`

Construction and colour extraction:

- `source_build/build_data_s1.py`
- `source_build/extract_color.py`
- `Code_S1.py`

Supporting documentation:

- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md`
- `submission/jbi/supporting/Appendix_S2_image_phenotyping.md`

The current environment-complete analysis has **1,922 observations** in **1,305 1-km cells**. Original YAMAP photographs are third-party material and are not redistributed; the derived trait/source table is the public analysis input.

## Stage 2 — Broad environment + spatial models

Question: how are pigmentation state and pigmented-only visible intensity structured by measured environment and residual geography?

Current workflows:

- `.github/workflows/environment-interaction-inla-screen.yml`
- `.github/workflows/broad-environment-spatial-audit.yml`

Primary scripts:

- `scripts/run_environment_interaction_inla_screen.R`
- `scripts/run_broad_environment_spatial_audit.R`
- `analysis_sensitivity/run_broad_environment_spatial_audit_wrapper.R`

Final decision records:

- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `reproducibility/broad_environment_spatial_final_fixed_effects_2026-08-11.csv`
- `reproducibility/broad_environment_spatial_final_hyperparameters_2026-08-11.csv`
- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`

Expected model structure:

- pigmentation state: eight abiotic axes + East/West + stationary SPDE;
- conditional intensity: same structure + Temperature PC1 × temperature-seasonality.

The analysis reports VIF, fit criteria and geographically blocked predictive performance. Promotion of added terms requires transfer evidence, not significance or WAIC alone.

## Stage 3 — Bombus source models and occurrence reference

Current source/model workflow:

- `.github/workflows/rebuild-bombus-sdm.yml`

Occurrence-reference calibration:

- `scripts/build_bombus_occurrence_reference_support.R`
- `.github/workflows/bombus-occurrence-reference-support.yml`

Supporting documentation:

- `submission/jbi/supporting/Appendix_S4_bombus_sdm_occurrence_support.md`
- `submission/jbi/JBI_sdm_model_building_checklist.md`

The SDMs provide habitat-support indices. They are not interpreted as abundance, visitation rate, pollen transfer or selection pressure.

## Stage 4 — local focal-Bombus boundary test

Primary execution:

- `scripts/run_bombus_local_sharp_transition.R`
- `.github/workflows/bombus-local-sharp-transition.yml`

Environmental-balance diagnostic:

- `analysis_sensitivity/audit_bombus_final8_environment_distance.R`
- `.github/workflows/bombus-final8-environment-audit.yml`

Supporting robustness:

- `scripts/run_bombus_spatial_replication_test.R`
- `.github/workflows/bombus-spatial-replication-test.yml`
- `submission/jbi/supporting/Appendix_S5_local_pollinator_robustness.md`

Expected primary result:

- 67 pure, non-overlapping 5-km white-pigmented transitions;
- mean occurrence-referenced focal contrast +0.03590;
- one-sided sign-flip P=0.02716;
- three-scale BH q=0.08148;
- median=-0.00277;
- 49.3% positive pairs.

The final-eight-axis environmental audit evaluates the fixed pair set; it does not select or weight pairs.

## Stage 5 — calibrated local departures

Primary implementation:

- `R/natural_predictive_model.R`
- `R/candidate_null_tools.R`
- `R/local_pigmented_isolates.R`
- `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`
- `.github/workflows/human-context-highrep-final.yml`

Event definition:

- pigmented focal cell;
- at least three neighbours within 10 km;
- standardized RMS distance <=1 across the finalized eight abiotic axes;
- all eligible observed neighbours are white.

The identical detector is replayed over **10,000** predictive maps.

Expected result:

- 16 observed candidates;
- candidate-count P=0.27897;
- candidate-fraction upper-tail P=0.12609.

Primary evidence lock:

- `reproducibility/current_broad_human_primary_2026-08-12.md`
- `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`

## Stage 6 — post-selection human context

Human features are read only after ecological event selection.

Primary implementation:

- `R/local_human_context.R`
- `R/human_landscape_features.R`
- `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`
- `.github/workflows/human-context-highrep-final.yml`

Expected leading contrast:

- population exposure within 5 km: +0.06744;
- directional P=0.00800;
- global maxT FWER P=0.05479.

Observation-effort and independent-source-support alternatives are included in the same calibrated framework. The result is interpreted as a field/provenance prioritization signal, not proof of anthropogenic origin.

## Stage 7 — figures and submission package

Current figure builders:

- `scripts/build_jbi_figure_bundle.R`
- `scripts/build_jbi_figure_bundle_final_broad.R`
- `scripts/render_jbi_main_figures.R`
- `scripts/render_jbi_main_figures_final_broad.R`
- `.github/workflows/jbi-main-figures.yml`

The current submission bundle restores the checksum-locked figure artifact declared in `.github/workflows/jbi-submission-bundle.yml` and then builds/validates the editable review package.

Submission workflow:

- `.github/workflows/jbi-submission-bundle.yml`

Submission validator:

- `submission/jbi/validate_jbi_submission.py`
- `scripts/validate_jbi_submission_bundle.py`

## Final integrated validation

Run:

- `.github/workflows/final-paper-analysis.yml`

This checks that the Broad, Bombus, local-departure and manuscript/Supporting Information numerical hierarchy agree. It does not fit a new omnibus model.

## Environment setup

For local development, restore the declared R environment before running analysis scripts:

```bash
Rscript scripts/setup_r_environment.R \
  --report-dir reproducibility \
  --scopes analysis,reproducibility,acquisition,testing,figures,reporting
```

Python dependencies are declared in `pyproject.toml`; R and system dependencies are declared under `dependencies/`.

## External-data boundary

- `Data_S1.csv` is distributed with the repository.
- Original YAMAP photographs are not redistributed.
- Raster and public occurrence sources are declared in configuration/source-build files.
- Artifact IDs and SHA-256 digests used for manuscript evidence are recorded in `paper/analysis-map.md` and dated reproducibility records.
- Live external services may change; a refreshed acquisition must never silently overwrite the frozen manuscript evidence.

## Verification target

A successful reproduction should recover the same analysis population, estimands, event definitions and claim-level numerical values within the documented computational tolerance. Exact bitwise identity of stochastic spatial-model draws across platforms is not required; statistical and evidential identity is.
