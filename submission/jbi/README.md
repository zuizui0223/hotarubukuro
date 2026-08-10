# Journal of Biogeography submission package

Target: **Research Article**.

This directory now contains only the **current JBI-facing submission package and files needed to finish that package**. Editorial brainstorming, narrative architecture notes, earlier E&E drafts, superseded figures and abandoned analysis variants are archived under `legacy/`.

## Start here

The current manuscript is:

- `JBI_main_manuscript_anonymized.md` — the only active Main manuscript for double-anonymous review.

Files intended for, or directly needed for, journal submission:

- `JBI_title_page_template.md` — identifying title-page fields to complete with all co-authors;
- `JBI_cover_letter.md` — concise Senior Editor cover-letter draft;
- `JBI_translated_abstract_ja.md` — Japanese translated abstract for optional upload;
- `JBI_submission_checklist.md` — current format/submission requirements;
- `validate_jbi_submission.py` — stable-format and anonymity checks.

Files used to finish the manuscript package:

- `JBI_main_figure_plan.md` — four-main-figure storyboard;
- `JBI_supporting_information_outline.md` — six-appendix SI architecture;
- `supporting/Appendix_S1_yamap_public_benchmark.md` — manuscript-ready YAMAP/iNaturalist/GBIF benchmark.

## Main-film rule

The Main manuscript is intentionally **not** a catalogue of everything that was analysed. It keeps only evidence that either resolves a question planted earlier or makes the next ecological question necessary.

1. **Data/phenotype opening:** repurposed hiking photographs make a national quantitative trait possible; white/pigmented state is separated from pigmented-only intensity.
2. **Broad biogeography:** environment + continuous space establish the natural geographical template.
3. **Local pollinator test:** the question changes scale rather than adding environmentally generated Bombus SDMs to the same national model.
4. **Guardrail payoff:** apparent high-elevation Bombus overlap disappears in near-equal-elevation comparisons and is not promoted as a mechanism.
5. **Local departures:** ecological events, not raw residual tails, define candidate locations; repeated natural maps show they are targets rather than proof of an extra process.
6. **Ending:** horticultural/human provenance remains a field/genetic hypothesis, not a conclusion.

## Supporting Information rule

Supporting Information is the **director's cut / evidence archive**, not a second Main story. It contains:

- YAMAP/iNaturalist/GBIF benchmark and complete data/QC detail;
- phenotype/mixture diagnostics;
- broad-model coefficient and spatial details;
- Bombus SDM construction and occurrence-reference calibration;
- all scale/exposure sensitivities, five-species community-boundary correspondence and montane guardrails;
- anomaly natural-map and human-context sensitivity families.

Historical/debugging analyses are not promoted to SI simply because they remain reproducible.

## What was moved out of this directory

The following drafting notes are now under `legacy/manuscript-development/jbi-notes/`:

- background-architecture notes;
- narrative-dependency architecture notes.

Earlier Ecology & Evolution manuscripts and their planning files are also legacy. They remain available for provenance but are not current submission sources.

## Current format status

The repository validator checks the stable JBI constraints captured for this draft: title <=115 characters without a Latin binomial, running title <40 characters, structured abstract <=300 words, 6–10 alphabetical keywords, Introduction-through-Discussion <=6,000 words, required section order and basic double-anonymous identity guards.

The official JBI Author Guidelines and submission portal must be re-checked immediately before upload because portal requirements can change.

For the manuscript-facing analysis hierarchy, see `paper/README.md` and `paper/analysis-map.md`.
