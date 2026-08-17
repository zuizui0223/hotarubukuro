# Journal of Biogeography submission package

Target article type: **Research Article**.

This folder contains the current manuscript, Supporting Information, figures and submission checks.

## JBI fit

The paper is framed as **intraspecific trait biogeography**. It asks how one flower-colour polymorphism can record environment, coherent residual geography, local pollinator opportunity and possible human movement at different spatial scales.

The conceptual advance is not a larger predictor set. It is **scale-matched attribution**: each analysis returns a positive scientific output and determines the comparison unit needed for the next process.

## The achievement chain

1. **Create the missing trait geography.** Author-screened YAMAP images yield a national quantitative dataset that is 3.81 times denser than the matched iNaturalist record for the focal species.
2. **Reveal two ecological layers.** Pigmentation state and pigmented-only intensity show different environmental and spatial organization.
3. **Partition broad environment from coherent residual geography.** INLA-SPDE delivers both a candidate abiotic landscape and spatial scales for future microclimate/genomic sampling.
4. **Localize the pollinator hypothesis.** A scale change separates shared highland geography from a small local state-boundary signal and identifies 67 sites for direct pollination tests.
5. **Calibrate local exceptions.** Predictive replay turns 16 striking configurations into reproducible field/provenance targets rather than untested anomalies.
6. **Identify the leading human clue.** Post-selection analysis points to short-range settlement exposure and its likely spatial scale without assigning provenance.
7. **Converge on one model.** Spatial variation in physiological and reproductive value, history and occasional human movement can maintain the polymorphism.

The paper does not become rigorous by minimizing these outputs. It becomes rigorous by letting strong, heterogeneous and null results perform different inferential jobs.

## Numbered Main structure

- **1. Introduction** — the biogeographic problem, dual function of colour and dependent hypotheses.
- **2. Materials and Methods** — 2.1–2.6, including software versions, reproducibility and inferential order.
- **3. Results** — 3.1–3.5, organized around the information gained at each stage.
- **4. Discussion** — 4.1–4.6, from new trait geography to a spatially varying polymorphism-maintenance model.

Acknowledgements, References and declarations remain unnumbered.

## Read in this order

1. `JBI_main_manuscript_anonymized.md` — the paper.
2. `JBI_main_figure_captions.md` — the four-figure visual story.
3. `supporting/` — technical detail and robustness.
4. `JBI_submission_checklist.md` — journal and portal checks.

Other submission files:

- `JBI_title_page_template.md`
- `JBI_cover_letter.md`
- `JBI_translated_abstract_ja.md`
- `JBI_sdm_model_building_checklist.md`
- `validate_jbi_submission.py`

## What stays in Supporting Information?

Supporting Information keeps details needed for audit that would interrupt the Main argument.

- **S1:** YAMAP/public-database benchmark and sampling process
- **S2:** image phenotyping, mixture model and QC
- **S3:** full environmental/spatial model checks
- **S4:** Bombus SDM construction and calibration
- **S5:** local Bombus sensitivities and negative controls
- **S6:** 10,000-map departure calibration and full human-context family

## Generated review package

`.github/workflows/jbi-submission-bundle.yml` builds:

- anonymized Main DOCX with Figures 1–4;
- combined Supporting Information DOCX;
- title-page, cover-letter, translated-abstract and SDM-checklist DOCX files;
- separate figure files;
- readiness and hash manifests.

CI validates scientific tokens, anonymity, JBI formatting, software-version reporting and cross-file consistency, then renders every DOCX through LibreOffice.

`review_science_bundle_complete=true` means the anonymous scientific package passed structural checks. It does **not** mean that author-controlled portal fields are complete.

For reproduction, use:

- `../../paper/README.md`
- `../../paper/analysis-map.md`
- `../../docs/reproduction-guide.md`

Original YAMAP photographs are third-party content and are not redistributed. The public derived table is `../../Data_S1.csv`.
