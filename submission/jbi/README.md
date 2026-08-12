# Journal of Biogeography submission package

Target article type: **Research Article**.

This directory contains the submission-facing representation of one ecological argument: **how a geographically structured flower-colour polymorphism is differentiated, potentially maintained and locally modified across ecological scales**.

The resolution sequence is:

`hiking photographs -> pigmentation state + intensity -> broad environmental/residual geography -> local focal-Bombus state boundaries -> natural-map-calibrated local departures -> contemporary provenance context -> direct experiments and genomics`

## Read the submission in this order

- `JBI_main_manuscript_anonymized.md` — anonymized Main manuscript;
- `JBI_main_figure_plan.md` and `JBI_main_figure_captions.md` — four-figure ecological progression;
- `supporting/Appendix_S1_yamap_public_benchmark.md` through `supporting/Appendix_S6_event_departures_human_context.md` — evidence and claim-bounding details;
- `JBI_translated_abstract_ja.md` — Japanese translated abstract;
- `JBI_submission_checklist.md` — submission-format/readiness checklist.

Additional delivery files are:

- `JBI_title_page_template.md`;
- `JBI_cover_letter.md`;
- `JBI_sdm_model_building_checklist.md`;
- `validate_jbi_submission.py`;
- `figures/README.md`;
- `bundle/README.md`.

## Biological hierarchy

### 1. Make the polymorphism measurable

Author-reviewed YAMAP hiking photographs provide a contemporary national visible-colour dataset. The phenotype is divided before ecological predictors enter into:

- **pigmentation state** — whether visible pigmentation is expressed; and
- **pigmented-only intensity** — how strong visible colour is after pigmentation is present.

This distinction is biological as well as statistical: the manuscript asks whether the two components have different environmental and biotic organization.

### 2. Define the broad candidate ecological landscape

Response-specific INLA-SPDE models estimate measured environmental associations while retaining unresolved continuous geography. Broad thermal, moisture and terrain associations are interpreted as candidate adaptive/developmental contexts, not direct proof of local adaptation. The spatial field is unresolved geography, not a named demographic mechanism.

### 3. Change scale for the focal-pollinator hypothesis

Because Bombus SDMs are themselves environment-derived, the Main test does not insert them as another national causal predictor. It moves to independently selected nearby white–pigmented boundaries. The resulting focal-Bombus correspondence is weak and local; if biological, it is framed as possible maintenance/loss of pigmentation state rather than progressive darkening or demonstrated pollinator-mediated selection.

### 4. Calibrate local departures before contemporary interpretation

Local pigmented configurations are defined in the finalized eight-axis natural state space and replayed on 10,000 predictive maps before human variables are examined. The 16 observed departures are not collectively excessive under the fitted natural geography. Short-range settlement exposure is suggestive post-selection context, not evidence of anthropogenic or horticultural origin.

### 5. End at the next mechanistic resolution

The manuscript closes by mapping macroecological results to direct tests:

- environmental gradients -> common-garden/reciprocal-transplant, physiology and fitness;
- residual spatial geography -> population genomics, ancestry, isolation by distance and admixture;
- focal Bombus SDMs -> realized visitation, flower contact, pollen deposition and reproductive success;
- local departures -> field provenance, planting history and genomic assignment.

The macro-scale analyses therefore locate where adaptive, demographic, biotic and contemporary mechanisms should next be tested rather than substituting for those direct measurements.

## Supporting Information

Supporting Information functions as the paper's evidence reserve:

- S1 — observation process and YAMAP/public-database benchmark;
- S2 — reproducible phenotype construction and optical claim ceiling;
- S3 — Broad environmental/spatial model and macro-resolution limits;
- S4 — Bombus SDM construction and occurrence-reference calibration;
- S5 — local pollinator robustness, community biogeography and elevation guardrails;
- S6 — natural-map departure calibration, human-context maxT family and observation-process alternatives.

A Supporting result enters Main only when it materially changes biological interpretation.

## Figures

`.github/workflows/jbi-main-figures.yml` generates four Main figures from checksum-locked evidence. The four panels are designed to read as an ecological progression rather than a methods inventory:

1. reveal pigmentation state and intensity as two colour layers;
2. locate broad environmental and residual geographic structure;
3. test weak local pollinator-maintenance correspondence at state boundaries;
4. calibrate natural departures before contemporary provenance follow-up.

The workflow outputs four 600-dpi PNGs, four vector PDFs, source hashes and a numerical lock. Map scales and panel-label requirements are validated.

## Editable review package

`.github/workflows/jbi-submission-bundle.yml` builds the review package from the current manuscript/SI and the checksum-locked Figure artifact. It produces the anonymized Main DOCX, one combined Supporting Information DOCX, title page, cover letter, translated abstract, SDM checklist, separate figures, file manifest, readiness record and delivery ZIP.

All generated Word files are converted through LibreOffice in CI. `review_science_bundle_complete=true` means the anonymous scientific package has passed structural validation. `portal_ready=false` remains expected until author-controlled fields, private review links, the permission-cleared taxon image and required disclosures are completed.

## Editorial architecture and validation

- `JBI_background_architecture.md` records the ecological foreshadowing and payoff structure of the Introduction.
- `JBI_narrative_dependency_architecture.md` records the scale changes that connect the paper's ecological questions.
- `validate_jbi_submission.py` checks format, anonymity, current 1,922 / 67 / 16 / 0.0548 numerical locks, figure requirements, translated abstract and checklist consistency.

These editorial files do not replace the scientific source of truth. For scientific reproduction, start at `../../paper/README.md`, `../../paper/analysis-map.md` and `../../docs/reproduction-guide.md`. The integrated evidence lock is `../../reproducibility/final_integrated_pipeline_2026-08-12.md`.

Original YAMAP photographs are third-party content and are not redistributed. The public derived trait table and its data dictionary are `../../Data_S1.csv` and `../../docs/data-s1-dictionary.md`.
