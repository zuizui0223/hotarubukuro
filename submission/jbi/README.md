# Journal of Biogeography submission package

Target: **Research Article**.

This directory contains the current JBI-facing version of the adopted broad → fine → anomaly analysis. It is intentionally **not** a catalogue of the full project history. Main keeps only evidence that resolves a planted question or makes the next ecological question necessary; robustness grids, secondary biogeographic patterns and negative guardrails move to Supporting Information.

## Current submission files

- `JBI_main_manuscript_anonymized.md` — the only active Main manuscript for double-anonymous review;
- `JBI_title_page_template.md` — identifying title-page fields to complete with all co-authors;
- `JBI_cover_letter.md` — concise Senior Editor cover-letter draft;
- `JBI_main_figure_plan.md` — four-main-figure storyboard;
- `JBI_supporting_information_outline.md` — six-appendix SI architecture;
- `JBI_translated_abstract_ja.md` — optional Japanese translated abstract;
- `JBI_submission_checklist.md` — current format/submission checklist;
- `validate_jbi_submission.py` — stable-format and anonymity checks;
- `supporting/Appendix_S1_yamap_public_benchmark.md` — manuscript-ready YAMAP/iNaturalist/GBIF benchmark;
- `supporting/Appendix_S2_image_phenotyping.md` — manuscript-ready image extraction, QC and two-part phenotype specification;
- `supporting/Appendix_S3_broad_environment_spatial_model.md` — manuscript-ready broad environment, INLA-SPDE and blocked-prediction specification;
- `supporting/Appendix_S4_bombus_sdm_occurrence_support.md` — manuscript-ready five-species SDM source build and occurrence-reference calibration;
- `supporting/Appendix_S5_local_pollinator_robustness.md` — manuscript-ready directional sensitivities, community-boundary correspondence and elevation guardrails;
- `supporting/Appendix_S6_event_departures_human_context.md` — manuscript-ready event calibration, candidate diagnostics and post-selection landscape context.

## Current editorial-architecture files

These are **active submission logic**, not historical brainstorming:

- `JBI_background_architecture.md` — measurement → attribution → exception; makes each later analysis feel necessary;
- `JBI_narrative_dependency_architecture.md` — explains why the four publication-level contributions form one dependent argument rather than four stapled mini-papers.

They are retained beside the manuscript because they define the JBI-specific compression and transition rules used to finish Main and SI.

## Main-film rule

The paper should read as one dependent sequence:

1. **Measurement:** an alternative recreational GPS/photo stream is converted into an auditable quantitative flower-colour trait dataset.
2. **Trait representation:** pigmentation state is separated from pigmented-only visible intensity.
3. **Broad attribution:** environment + continuous space establish the national natural template.
4. **Scale shift:** because Bombus SDMs share environmental geography with the trait, the pollinator question changes comparison unit to abrupt nearby boundaries rather than adding another national coefficient.
5. **Focal pollinator test:** the adopted Main exposure is occurrence-referenced `max(B. ardens, B. diversus)` at 5-km pure white↔pigmented transitions; the inference is weak/local/exploratory.
6. **Guardrail:** five-species community turnover and equal-elevation montane analyses stay in SI so shared biogeography is not promoted to mechanism.
7. **Exception:** local departures are defined as repeatable ecological events and calibrated against natural predictive maps before human variables are examined.
8. **Ending:** horticultural/human provenance remains a field/genetic follow-up hypothesis, not a causal conclusion.

The general JBI pitch is that **integration should connect scales rather than collapse processes operating at different scales into one omnibus model**.

## Supporting Information rule

Supporting Information is the **director's cut / evidence archive**, not a second Main story. It contains:

- YAMAP/iNaturalist/GBIF benchmark and complete data/QC detail;
- phenotype/mixture diagnostics;
- broad-model coefficient and spatial details;
- Bombus SDM construction and occurrence-reference calibration;
- all scale/exposure sensitivities;
- five-species community-boundary correspondence;
- montane/equal-elevation guardrails;
- anomaly natural-map and human-context sensitivity families.

Historical/debugging analyses are not promoted to SI merely because they remain reproducible.

## Historical material

Earlier Ecology & Evolution manuscripts, old 1,909/1,923 publication architectures, superseded limitation/relaxation analyses, and one-time editorial patches belong under `legacy/`. They remain discoverable for provenance but are not current submission sources.

## Current format status

`jbi-submission-format.yml` runs the stable constraints captured for this draft: title <=115 characters without a Latin binomial, running title <40 characters, structured abstract <=300 words, 6–10 alphabetical keywords, Introduction-through-Discussion <=6,000 words, required section order and basic double-anonymous identity guards.

The official JBI Author Guidelines and submission portal should still be re-checked immediately before upload because portal requirements can change.

For the manuscript-facing analysis hierarchy, see `paper/analysis-map.md`; for the frozen scientific hierarchy, see `reproducibility/final_paper_pipeline_2026-08-09.md`.
