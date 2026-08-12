# Reproduction guide

This guide is for people who want to check the manuscript numbers or rerun an analysis.

If you only want to understand the science, start at [`paper/README.md`](../paper/README.md).

## Two ways to reproduce the paper

### 1. Exact evidence audit

Use this when you want to check the numbers in the manuscript.

1. Find the result in [`paper/analysis-map.md`](../paper/analysis-map.md).
2. Restore the listed checksum-locked artifact.
3. Run the matching validator or paper workflow.
4. Confirm that the manuscript value is recovered.

This is the preferred route for submission QC.

### 2. Full scientific rerun

Use this when you want to rebuild an analysis from its declared inputs.

External databases and rasters can change. A refreshed source is therefore treated as a **new analysis**, not as a silent replacement for the manuscript evidence.

## Stage 1. Build the flower-colour dataset

Public analysis table:

- `Data_S1.csv`

Main construction code:

- `Code_S1.py`
- `source_build/build_data_s1.py`
- `source_build/extract_color.py`

Documentation:

- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md`
- `submission/jbi/supporting/Appendix_S2_image_phenotyping.md`

Expected analysis population:

- 1,922 observations;
- 1,305 1-km cells;
- 966 white-like;
- 956 pigmented.

Original YAMAP photographs are third-party material and are not redistributed.

## Stage 2. Fit broad environment and spatial models

Question: how do pigmentation state and colour intensity vary with measured environment, and how much spatial structure remains?

Workflows:

- `.github/workflows/environment-interaction-inla-screen.yml`
- `.github/workflows/broad-environment-spatial-audit.yml`

Main scripts:

- `scripts/run_environment_interaction_inla_screen.R`
- `scripts/run_broad_environment_spatial_audit.R`
- `analysis_sensitivity/run_broad_environment_spatial_audit_wrapper.R`

Final model record:

- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`

Expected structure:

- pigmentation state: eight abiotic axes + East/West + stationary SPDE;
- conditional intensity: the same terms + Temperature PC1 × temperature seasonality.

Appendix S3 contains the interaction screen, VIF checks, hydroclimate tests and alternative spatial models.

## Stage 3. Build Bombus habitat-support surfaces

Main source workflow:

- `.github/workflows/rebuild-bombus-sdm.yml`

Occurrence-reference calibration:

- `scripts/build_bombus_occurrence_reference_support.R`
- `.github/workflows/bombus-occurrence-reference-support.yml`

Documentation:

- `submission/jbi/supporting/Appendix_S4_bombus_sdm_occurrence_support.md`
- `submission/jbi/JBI_sdm_model_building_checklist.md`

Important: these surfaces are habitat-support indices. They are not bee abundance, visitation or pollen transfer.

## Stage 4. Test local white-pigmented boundaries

Main script and workflow:

- `scripts/run_bombus_local_sharp_transition.R`
- `.github/workflows/bombus-local-sharp-transition.yml`

Environmental-locality check:

- `analysis_sensitivity/audit_bombus_final8_environment_distance.R`
- `.github/workflows/bombus-final8-environment-audit.yml`

Full robustness:

- `submission/jbi/supporting/Appendix_S5_local_pollinator_robustness.md`

Expected primary result:

- 67 non-overlapping pure transitions within 5 km;
- mean focal contrast +0.03590;
- median -0.00277;
- 49.3% positive pairs;
- one-sided P=0.02716;
- q=0.08148 across the 5/10/25-km primary family.

The environmental-distance check is run **after** the pair set is fixed. It does not choose or weight pairs.

## Stage 5. Calibrate local departures

Main implementation:

- `R/natural_predictive_model.R`
- `R/candidate_null_tools.R`
- `R/local_pigmented_isolates.R`
- `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`
- `.github/workflows/human-context-highrep-final.yml`

Event rule:

- pigmented focal cell;
- at least three neighbours within 10 km;
- root-mean-square environmental distance <=1 across the final eight abiotic axes;
- all eligible observed neighbours are white.

The same detector is replayed on 10,000 predictive maps.

Expected result:

- 16 observed candidates;
- count P=0.27897;
- candidate-fraction P=0.12609.

Documentation:

- `reproducibility/current_broad_human_primary_2026-08-12.md`
- `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`

## Stage 6. Test human context

Human variables are read only after the 16 observed candidates are fixed.

Main code:

- `R/local_human_context.R`
- `R/human_landscape_features.R`
- `analysis_sensitivity/run_human_context_current_broad_primary_fast.R`

Expected leading result:

- population exposure within 5 km: +0.06744;
- directional P=0.00800;
- global maxT FWER P=0.05479.

This is treated as a provenance/field-work signal, not proof of human origin.

## Stage 7. Build figures and the review package

Figures:

- `.github/workflows/jbi-main-figures.yml`
- `scripts/render_jbi_main_figures.R`

Review package:

- `.github/workflows/jbi-submission-bundle.yml`

Validation:

- `submission/jbi/validate_jbi_submission.py`
- `scripts/validate_jbi_submission_bundle.py`

## Final cross-file check

Run:

- `.github/workflows/final-paper-analysis.yml`

This checks that the manuscript, Supporting Information and frozen evidence agree. It does not fit a new combined model.

## Local environment

R packages:

```bash
Rscript scripts/setup_r_environment.R \
  --report-dir reproducibility \
  --scopes analysis,reproducibility,acquisition,testing,figures,reporting
```

Python dependencies are in `pyproject.toml`. R and system dependencies are under `dependencies/`.

## What counts as success?

A successful reproduction should recover the same:

- analysis population;
- model definitions;
- local-boundary and local-departure rules;
- manuscript-level numerical results.

Exact stochastic draws do not need to be bit-for-bit identical across platforms. The scientific result and the locked evidence should agree.
