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

The submission workflow restores the checksum-locked ecological-layout Main-figure artifact produced from the current renderer and evidence hierarchy:

- figure workflow run: `31584573276`;
- figure artifact: `9136663517`;
- artifact SHA-256: `98aebc0216499542a0883c5584ed3d165c253b36bb7d7b9eac5c22c3ef2fb979`;
- source renderer commit: `2475c2f821534e78b3c3594acae0c4cff2395f1e`.

The artifact passed numerical-lock, text-package and file validation and was visually checked after generation. Its four Main figures follow the same biological progression as the manuscript: two phenotypic colour layers -> broad environmental/residual geography -> weak local pollinator-maintenance correspondence -> natural-map-calibrated contemporary departures. The artifact contains four 600-dpi PNGs, four vector PDFs, source hashes and the manuscript numerical lock. All map panels retain explicit 100-km scales and JBI-facing lowercase panel labels.

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

## Ecological narrative invariance

Packaging must preserve the current biological hierarchy without strengthening causal claims:

- pigmentation state and pigmented-only intensity are distinct ecological layers of the flower-colour phenotype;
- Broad environmental associations define candidate adaptive/developmental contexts, while the SPDE field remains unresolved geography;
- the focal Bombus result is weak and, if biological, is framed as local maintenance/loss of pigmentation state rather than progressive darkening;
- apparent high-elevation Bombus overlap remains a negative shared-geography guardrail;
- local departures are calibrated under the natural model before human context, which remains a contemporary provenance/local-modification hypothesis;
- the final synthesis connects macroecological patterns to direct common-garden, pollination, genomic and provenance tests.

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
