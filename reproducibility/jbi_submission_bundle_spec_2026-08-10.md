# JBI review/submission bundle specification

Updated: 2026-08-12

## Purpose

This file defines the generated delivery layer for the current *Journal of Biogeography* submission. Scientific definitions live in the manuscript, Supporting Information and analysis/evidence map; this specification records how those sources are assembled and validated for review.

Source of truth:

- `submission/jbi/JBI_main_manuscript_anonymized.md`;
- `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md` through `Appendix_S6_event_departures_human_context.md`;
- `paper/analysis-map.md`;
- `paper/active-file-map.csv`;
- `reproducibility/final_integrated_pipeline_2026-08-12.md`.

## Current figure input

The submission workflow restores the checksum-locked Main-figure artifact produced from the current figure renderer and current evidence hierarchy:

- figure workflow run: `31559274663`;
- figure artifact: `9127198711`;
- artifact SHA-256: `ff5d43e8f71224261b8b74ddb2d6e24a66a4f2349ad53fb72032118492bca924`;
- source figure commit: `8f1b80d7994f948e16050edf217ad16c9e513df8`.

The artifact contains four validated 600-dpi PNGs, four vector PDFs, source hashes and the manuscript numerical lock. Figure maps include explicit scale information and panel labels follow the current JBI-facing convention.

## Generated files

`python scripts/build_jbi_submission_bundle.py` creates:

1. `01_Main_Manuscript_Anonymized.docx`, with the current Main text, Figure 1–4 legends and embedded PNGs;
2. `02_Supporting_Information_Appendices_S1-S6.docx`;
3. title-page and cover-letter DOCX templates;
4. Japanese translated-abstract DOCX;
5. Bombus SDM/model-building checklist DOCX;
6. four separate PNG/PDF figure pairs;
7. readiness JSON/Markdown, file manifest and delivery ZIP.

## Acceptance checks

`python scripts/validate_jbi_submission_bundle.py` independently checks:

- six structurally valid DOCX files;
- exactly four embedded Main figures;
- required Main sections and Appendices S1–S6;
- absence of known identifying strings from anonymized Main XML;
- valid PNG/PDF signatures;
- byte-size and SHA-256 agreement with the generated manifest;
- complete ZIP contents;
- explicit separation of anonymous scientific completeness from author-controlled portal readiness.

The GitHub workflow additionally converts all generated DOCX files through LibreOffice and requires non-empty rendered PDFs and first-page previews.

## Scientific invariance

Packaging and reformatting must preserve the adopted evidence hierarchy:

- 1,922 environment-complete phenotype observations in 1,305 1-km cells;
- the two-part phenotype and a*=4.968780 boundary;
- finalized response-specific Broad environment/spatial models;
- 67-pair focal 5-km Bombus boundary test;
- final-eight-axis local-event definition;
- 16 observed local-departure candidates;
- 10,000-map natural calibration;
- 5-km population-context global maxT FWER P=0.05479;
- all associated claim ceilings.

The submission builder does not choose among analyses and does not reinterpret results.

## Readiness semantics

`review_science_bundle_complete=true` means the anonymous scientific files, figures and Supporting Information have been assembled and structurally validated.

`portal_ready=false` remains expected until authors complete and approve:

- author order, names, ORCIDs, affiliations and corresponding-author details;
- acknowledgements and funding;
- conflict-of-interest statement;
- CRediT contributions;
- cover-letter sign-off;
- randomized private-for-peer-review data/code URL;
- required permission-cleared taxon image;
- any disclosure required by the current Wiley/JBI policy.

The builder must never infer these author-controlled fields.

## Public reproducibility principle

The repository-facing documentation points readers to the adopted analysis and its exact evidence rather than requiring them to reconstruct the project's development sequence. Historical development material may remain under `legacy/` for provenance, but it is outside the public reproduction path.
