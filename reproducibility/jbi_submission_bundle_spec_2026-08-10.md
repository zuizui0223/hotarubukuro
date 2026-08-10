# JBI review/submission bundle specification

Date: 2026-08-10

## Purpose

This specification defines the generated delivery layer for the current Journal of Biogeography submission. It does not define a new analysis and does not replace any manuscript evidence lock.

The source-of-truth remains:

- current anonymous manuscript and Appendices S1–S6 under `submission/jbi/`;
- the current four-figure source/validator workflow;
- checksum-locked broad/anomaly and local focal-pollinator artifacts;
- the current JBI textual validator and active-file map.

## Frozen figure input

The delivery workflow restores the successful current Main-figure artifact:

- figure workflow run: `31375294535`;
- figure artifact: `9057707602`;
- artifact SHA-256: `a1b2d44c09b5dd7d00f73dcab4232128936e9bfd5f8af7c433a617ad114968eb`;
- source figure head: `aae380cfae669058d5df27d8d8eed95c8f378a3c`.

The artifact contains four validated 600-dpi PNGs, four vector PDFs, source hashes and the manuscript numerical lock.

## Generated files

`python scripts/build_jbi_submission_bundle.py` creates:

1. `01_Main_Manuscript_Anonymized.docx`, containing the current Main text plus Figure 1–4 legends and embedded PNGs;
2. `02_Supporting_Information_Appendices_S1-S6.docx`, one combined editable SI file;
3. separate title-page and cover-letter DOCX templates;
4. optional Japanese translated-abstract DOCX;
5. a current Bombus SDM/model-building checklist DOCX;
6. four separate PNG/PDF figure pairs;
7. readiness JSON/Markdown, file manifest and one ZIP archive.

## Acceptance checks

`python scripts/validate_jbi_submission_bundle.py` independently requires:

- six structurally valid DOCX files;
- exactly four embedded images in the anonymized Main DOCX;
- all required Main sections and Appendices S1–S6;
- no known identifying repository/author strings in the anonymized Main XML;
- four valid PNG and four valid PDF signatures;
- byte-size and SHA-256 agreement with the generated file manifest;
- a ZIP containing every required upload-oriented file;
- an explicit readiness record that distinguishes anonymous scientific completeness from portal readiness.

The GitHub workflow additionally converts all six DOCX files through LibreOffice and requires one non-empty PDF plus a rendered first-page PNG per document.

## Readiness semantics

`review_science_bundle_complete=true` means the anonymous scientific files, figures and SI have been assembled and validated.

`portal_ready=false` is expected until author-controlled information is completed and approved. Known blockers include:

- author order, names, ORCIDs, affiliations and corresponding-author details;
- acknowledgements, funding, conflict of interest and CRediT contributions;
- cover-letter sign-off;
- randomized private-for-peer-review Dryad/equivalent URL;
- required author-owned or permission-cleared taxon image;
- any disclosure required under the current Wiley/JBI AI-generated-content policy.

The builder must never infer or silently fill these fields.

## Scientific invariance

Generating or reformatting the delivery package must not change:

- the 1,922-observation phenotype population;
- the two-part phenotype or a*=4.968780 boundary;
- broad environment/space estimates;
- the 67-pair focal 5-km Bombus test;
- the 17 local-departure candidates;
- any P value, artifact identity or claim ceiling.
