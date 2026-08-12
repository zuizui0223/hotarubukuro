# Current validation boundary

This directory contains only validators with a current workflow route. A validator is not current merely because it existed in a successful historical reconstruction.

## Main-figure bundle

- `validate_jbi_figure_bundle.R` — shared figure-bundle validation used by the core builder.
- `validate_jbi_figure_bundle_final_broad.R` — current final-Broad/Bombus/human numerical lock for the manuscript-facing Figure 1–4 bundle.

The execution route is `.github/workflows/jbi-main-figures.yml`.

## Bombus source build

- `compare_bombus_sdm_rebuilds.R` — compare two seeded mainland Bombus SDM rebuilds for reproducibility.

The execution route is `.github/workflows/rebuild-bombus-sdm.yml`. Source-extraction QC remains with the source builder at `source_build/audit_bombus_extraction.R`.

## Archived downstream validators

The former natural-predictive, 17-candidate, human-context, DID, joint-PPC and candidate-DOY validators belonged to the superseded four-PC downstream orchestration. Their full code is preserved at:

`legacy/reproducibility-development/superseded-current-input-anomaly-pipeline-2026-08-12/validation/`

They must not be invoked to validate the current 16-candidate/final-eight-axis human analysis. The current human result is instead locked by `.github/workflows/human-context-highrep-final.yml`, `.github/workflows/final-paper-analysis.yml`, `reproducibility/current_broad_human_primary_2026-08-12.md` and Appendix S6.

Older fixed-result development audits remain under other `legacy/reproducibility-development/validation/` locations and are likewise outside the current paper interface.
