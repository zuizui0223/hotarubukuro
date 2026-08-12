# Journal of Biogeography submission package

Target article type: **Research Article**.

This directory contains the submission-facing representation of the adopted analysis:

`YAMAP/iEcology -> two-part phenotype -> Broad environment + space -> local focal-Bombus boundary test -> calibrated local departures -> post-selection human context`

## Submission sources

- `JBI_main_manuscript_anonymized.md` — anonymized Main manuscript;
- `JBI_title_page_template.md` — identifying title-page fields;
- `JBI_cover_letter.md` — JBI-specific cover letter;
- `JBI_main_figure_plan.md` — four-figure scientific plan;
- `JBI_main_figure_captions.md` — standalone Figure 1–4 legends;
- `figures/README.md` — figure-generation and artifact policy;
- `JBI_supporting_information_outline.md` — six-appendix SI structure;
- `JBI_sdm_model_building_checklist.md` — five-species Bombus SDM/model-building checklist;
- `JBI_translated_abstract_ja.md` — Japanese translated abstract;
- `JBI_submission_checklist.md` — submission-format/readiness checklist;
- `validate_jbi_submission.py` — manuscript/SI/figure consistency and anonymity validator;
- `bundle/README.md` — generated delivery-package semantics;
- `supporting/Appendix_S1_yamap_public_benchmark.md`;
- `supporting/Appendix_S2_image_phenotyping.md`;
- `supporting/Appendix_S3_broad_environment_spatial_model.md`;
- `supporting/Appendix_S4_bombus_sdm_occurrence_support.md`;
- `supporting/Appendix_S5_local_pollinator_robustness.md`;
- `supporting/Appendix_S6_event_departures_human_context.md`.

## Scientific division of labour

### Main

Main contains the dependent inferential sequence:

1. construct an auditable quantitative flower-colour phenotype from reviewed recreational photographs;
2. separate white/pigmented state from visible intensity conditional on pigmentation;
3. establish broad environmental and residual spatial organization;
4. test the focal Bombus hypothesis at sharp nearby white-pigmented boundaries;
5. define local ecological departures using the finalized eight-axis natural reference;
6. calibrate those events over 10,000 predictive maps before examining human context.

### Supporting Information

Supporting Information contains the evidence needed to evaluate robustness and claim ceilings:

- public-database sampling-frame benchmark;
- image measurement and QC detail;
- full Broad model/interaction/spatial diagnostics;
- Bombus occurrence/SDM construction and occurrence-reference calibration;
- local Bombus scale/exposure robustness;
- community-turnover and elevation guardrails;
- event calibration, human-context maxT family and observation-process alternatives.

## Figures

`.github/workflows/jbi-main-figures.yml` generates four Main figures from checksum-locked evidence. The workflow outputs:

- four 600-dpi PNG review copies;
- four vector PDFs;
- per-file SHA-256 hashes;
- source manifest;
- numerical lock.

Figure maps include explicit scale information and the panel labels/captions are validated against the current submission sources.

## Editable review package

`.github/workflows/jbi-submission-bundle.yml` builds the review package from the current manuscript/SI and the locked Figure artifact. It produces:

- anonymized Main DOCX with embedded Figures 1–4;
- one combined editable Supporting Information DOCX;
- title-page DOCX;
- cover-letter DOCX;
- translated-abstract DOCX;
- SDM/model-building checklist DOCX;
- separate PNG/PDF figures;
- file manifest, readiness record and ZIP archive.

All generated Word files are converted through LibreOffice in CI. `review_science_bundle_complete=true` means the anonymous scientific package has passed structural validation. `portal_ready=false` remains expected until author-controlled fields, private review links, taxon image and required disclosures are completed.

## Narrative architecture

- `JBI_background_architecture.md` records the measurement → attribution → exception logic used to keep the Introduction dependent and concise.
- `JBI_narrative_dependency_architecture.md` records the transitions that connect the paper's spatial scales.

These files are editorial aids; scientific definitions and numerical evidence remain in the manuscript, SI and `paper/analysis-map.md`.

## Validation

`validate_jbi_submission.py` checks, among other things:

- title/running-title limits;
- structured abstract and keyword constraints;
- manuscript section order and word count;
- known identifying strings;
- current 1,922 / 67 / 16 / 0.0548 science locks across submission-facing text;
- Figure 1–4 caption order and current panel/map requirements;
- translated-abstract and checklist consistency.

The official JBI Author Guidelines and live submission portal must still be checked immediately before upload because editorial requirements can change.

For scientific reproduction, start at `paper/README.md` and `docs/reproduction-guide.md`. For exact evidence identities, use `paper/analysis-map.md` and `reproducibility/final_integrated_pipeline_2026-08-12.md`.
