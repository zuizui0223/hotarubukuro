# Journal of Biogeography submission package

Target article type: **Research Article**.

This folder contains the current manuscript, Supporting Information, figures and submission checks.

## JBI fit

The paper is framed as **intraspecific trait biogeography**. Its central mystery is why white and pigmented flowers remain geographically structured within one species when environment, history, pollinators and human movement share space.

The conceptual advance is not that many predictor families were analysed. It is that each answer reveals the next confounding layer, so the comparison unit changes as the explanation becomes more local.

## Read in this order

1. `JBI_main_manuscript_anonymized.md` — the paper.
2. `JBI_main_figure_captions.md` — the four-figure visual story.
3. `supporting/` — technical detail and robustness.
4. `JBI_submission_checklist.md` — journal and portal checks.

Other files support the submission process:

- `JBI_title_page_template.md`
- `JBI_cover_letter.md`
- `JBI_translated_abstract_ja.md`
- `JBI_sdm_model_building_checklist.md`
- `validate_jbi_submission.py`

## The paper is one investigation

1. **Reveal the phenotype.** YAMAP images make a national quantitative polymorphism visible and separate pigmentation state from intensity.
2. **Split the geography.** State and intensity show different environmental patterns, while substantial unresolved space remains.
3. **Distrust the broad overlap.** The Bombus hypothesis moves to local boundaries; strong-looking highland overlap disappears, and the focal signal is weak and local.
4. **Calibrate the apparent exceptions.** Sixteen departures look striking but remain compatible with natural predictive maps; human context is a later provenance clue.
5. **Converge on one model.** Physiological value, population history, local reproductive benefit and occasional human movement can contribute at different spatial scales.

Each answer creates the need for the next comparison.

## Numbered Main structure

- **1. Introduction** — the mystery and why each confounding layer requires a new scale.
- **2. Materials and Methods** — 2.1–2.6, from YAMAP sampling to inferential order.
- **3. Results** — 3.1–3.5, from trait discovery to the human-context clue.
- **4. Discussion** — 4.1–4.6, from the first reveal to direct causal tests.

Acknowledgements, References and declarations remain unnumbered.

## What stays in Supporting Information?

Supporting Information keeps details that are important for audit but would interrupt the unfolding Main argument.

- **S1:** YAMAP/public-database benchmark and sampling process
- **S2:** image phenotyping, mixture model and QC
- **S3:** full environmental/spatial model checks
- **S4:** Bombus SDM construction and calibration
- **S5:** local Bombus sensitivities and negative controls
- **S6:** 10,000-map departure calibration and full human-context family

## Writing rule

A paragraph should complete one movement:

`setup -> evidence -> changed interpretation -> next implication`

The reader should always understand why the next subsection had to exist. Technical safeguards stay in Supporting Information unless they change the biological inference.

Editorial guides:

- `JBI_background_architecture.md` — the three revealed bottlenecks in the Introduction
- `JBI_narrative_dependency_architecture.md` — whole-paper sequence and paragraph rules

## Generated review package

`.github/workflows/jbi-submission-bundle.yml` builds:

- anonymized Main DOCX with Figures 1–4;
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
