# Journal of Biogeography submission package

Target article type: **Research Article**.

This folder contains the current manuscript, Supporting Information, figures and submission checks.

## Read in this order

1. `JBI_main_manuscript_anonymized.md` — the paper.
2. `JBI_main_figure_captions.md` — the four-figure story.
3. `supporting/` — technical detail and robustness.
4. `JBI_submission_checklist.md` — journal and portal checks.

Other files support the submission process:

- `JBI_title_page_template.md`
- `JBI_cover_letter.md`
- `JBI_translated_abstract_ja.md`
- `JBI_sdm_model_building_checklist.md`
- `validate_jbi_submission.py`

## The paper in four questions

### 1. What is the flower-colour phenotype?

We built a national dataset from author-screened YAMAP hiking photographs.

The phenotype has two parts:

- white-like versus pigmented state;
- colour intensity among pigmented flowers.

### 2. How do environment and geography relate to those two traits?

Pigmentation is less common in warmer climates.

Colour intensity follows a different pattern involving temperature seasonality, moisture and terrain.

A strong spatial pattern remains after measured environment.

### 3. Do local white-pigmented boundaries line up with focal bumblebee habitat opportunity?

Only weakly.

The main test uses 67 fixed local boundaries. The signal is small, fragile and not treated as evidence of pollinator-mediated selection.

### 4. Are local pigmented departures unusual?

No.

Sixteen observed departures are compatible with 10,000 natural predictive maps.

Human context is tested only after those sites are fixed. Short-range population exposure is the strongest feature, but global maxT FWER P=0.05479.

## What stays in Supporting Information?

Supporting Information keeps the details that are important for audit but would slow the Main story.

- **S1:** YAMAP/public-database benchmark and sampling process
- **S2:** image phenotyping, mixture model and QC
- **S3:** full environmental/spatial model checks
- **S4:** Bombus SDM construction and calibration
- **S5:** local Bombus sensitivities and negative controls
- **S6:** 10,000-map departure calibration and full human-context family

## Writing rule

The Main text uses short, direct English.

A paragraph should answer one question. Technical safeguards stay in Supporting Information unless the reader needs them to understand the biological result.

Editorial guides:

- `JBI_background_architecture.md` — Introduction order
- `JBI_narrative_dependency_architecture.md` — whole-paper story and plain-English rules

## Generated review package

`.github/workflows/jbi-submission-bundle.yml` builds:

- anonymized Main DOCX with Figures 1-4;
- combined Supporting Information DOCX;
- title-page, cover-letter, translated-abstract and SDM-checklist DOCX files;
- separate figure files;
- readiness and hash manifests.

CI also renders every DOCX through LibreOffice.

`review_science_bundle_complete=true` means the anonymous scientific package passed structural checks. It does **not** mean that author-controlled portal fields are complete.

For reproduction, use:

- `../../paper/README.md`
- `../../paper/analysis-map.md`
- `../../docs/reproduction-guide.md`

Original YAMAP photographs are third-party content and are not redistributed. The public derived table is `../../Data_S1.csv`.
