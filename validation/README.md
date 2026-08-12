# Validation boundary

This directory contains validators used by the adopted manuscript workflows.

## Main-figure bundle

- `validate_jbi_figure_bundle.R` — structural/source/numerical validation used by the shared figure builder;
- `validate_jbi_figure_bundle_final_broad.R` — current Broad/Bombus/local-departure numerical lock for Figures 1–4.

Execution:

- `.github/workflows/jbi-main-figures.yml`.

## Bombus source build

- `compare_bombus_sdm_rebuilds.R` — compare seeded mainland Bombus SDM rebuilds for reproducibility.

Execution:

- `.github/workflows/rebuild-bombus-sdm.yml`.

Source-extraction QC is implemented in `source_build/audit_bombus_extraction.R`.

## Local-departure/human validation

The current result is validated at the workflow/integration level rather than by a separate fixed-result validator:

- `.github/workflows/human-context-highrep-final.yml` regenerates the final-eight-axis event replay and maxT summaries;
- `reproducibility/current_broad_human_primary_2026-08-12.md` records the expected event definition and numerical identity;
- `.github/workflows/final-paper-analysis.yml` checks cross-stage manuscript/SI consistency;
- `submission/jbi/validate_jbi_submission.py` checks submission-facing current science tokens.

## Validation principle

A validator is manuscript-facing only when it is registered in `paper/active-file-map.csv` and reachable from an adopted workflow. Numerical locks are checked against checksum-identified evidence, not against whichever generated file happens to be newest.
