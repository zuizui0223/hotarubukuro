# Journal of Biogeography submission package

Target: **Research Article**.

This directory contains the JBI-facing rewrite of the final broad -> fine -> anomaly analysis. It is intentionally **not** a copy of the full project narrative. The Main manuscript keeps only evidence that either resolves a question planted earlier or creates the next ecological question; robustness grids and secondary biogeographic patterns move to Supporting Information.

## Current files

- `JBI_main_manuscript_anonymized.md` — double-anonymous Main manuscript;
- `JBI_title_page_template.md` — identifying title-page fields to complete with all co-authors;
- `JBI_cover_letter.md` — concise Senior Editor cover-letter draft;
- `JBI_supporting_information_outline.md` — six-appendix SI architecture;
- `JBI_submission_checklist.md` — current Wiley/JBI format requirements checked 2026-08-09;
- `JBI_translated_abstract_ja.md` — optional Japanese translated abstract;
- `validate_jbi_submission.py` — lightweight stable-format/anonymity checks.

## JBI-facing editorial choices

### Main film

1. **Data/phenotype opening:** repurposed hiking photographs make a national quantitative trait possible; white/pigmented state is separated from pigmented-only intensity.
2. **Broad biogeography:** environment + continuous space establish the geographical template.
3. **Local pollinator test:** the question changes scale rather than adding environmentally generated Bombus SDMs to the national model.
4. **Guardrail payoff:** high-elevation Bombus overlap disappears when elevation is locally controlled, so it is not promoted as a mechanism.
5. **Local departures:** ecological events, not raw residual tails, define candidate locations; repeated natural maps show they are targets rather than proof of an extra process.
6. **Ending:** horticultural/human provenance remains a field/genetic hypothesis, not a conclusion.

### Supporting Information

- YAMAP/iNaturalist/GBIF benchmark and full data QC;
- phenotype/mixture diagnostics;
- broad-model coefficient and spatial details;
- Bombus SDM construction and occurrence-reference calibration;
- all scale/exposure sensitivities, five-species turnover and montane guardrails;
- anomaly natural-map and human-context sensitivity families.

Historical superseded analyses remain in the repository for provenance but are not automatically submitted as SI.

## Current format status

The branch has a GitHub Actions validator for the stable JBI constraints used in the draft: title <=115 characters without a Latin binomial, running title <40 characters, structured abstract <=300 words, 6–10 alphabetical keywords, Introduction-through-Discussion <=6,000 words, required section order, and simple double-anonymous identity guards.

The official JBI Author Guidelines and submission portal must be re-checked immediately before upload because portal requirements can change.
