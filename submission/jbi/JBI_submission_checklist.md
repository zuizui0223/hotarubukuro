# Journal of Biogeography — submission checklist

Checked against the current Wiley `Journal of Biogeography` Author Guidelines on **2026-08-17**.

## Article category and journal fit

- Target: **Research Article**, not Methods and Tools or Data paper.
- Main numbered headers: **1. Introduction, 2. Materials and Methods, 3. Results, 4. Discussion**, followed by unnumbered Acknowledgements, References and declarations.
- The manuscript presents new biogeographic research on one novel question: how an intraspecific flower-colour polymorphism is structured by ecological processes operating at different spatial scales.
- The conceptual advance is **scale-matched attribution**: new trait data establish the national pattern; INLA-SPDE separates measured environment from coherent residual geography; the Bombus hypothesis is tested as heterogeneity among local colour boundaries; and a distinct local-isolate event is selected, naturally calibrated and only then examined for human context.
- The paper is achievement-forward without inflating causality. Positive, heterogeneous and null results each provide a distinct scientific output: response-specific physiological hypotheses, unresolved spatial geography, a subset-driven local biotic correspondence, naturally calibrated field targets and a bounded provenance hypothesis.
- Methods are described for repeatability and linked to versioned code, fixed seeds, software manifests, checksum-locked artifacts and independent validators.

## Length and front matter

- Main-text target: <=6,000 words.
  - Current Introduction-through-Discussion count = **5,696 words** by repository validator.
- Title: <=115 characters including spaces; no abbreviations or Latin binomial.
  - Current title = 108 characters.
- Running title: <40 characters.
  - Current running title = 26 characters.
- Structured abstract: <=300 words under **Aim, Location, Taxon, Methods, Results, Main conclusions**.
  - Current abstract = **277 words** by repository validator.
- Keywords: 6–10, alphabetical.
  - Current count = 8.
- The first abstract mention uses common name followed by the scientific name: spotted bellflower (*Campanula punctata*).
- **Taxon authority** is provided at the first main-text Latin binomial: *Campanula punctata* Lam.

## Software reporting

- Main text gives **R 4.5.3** at first mention and cites R Core Team (2026).
- Main text gives **INLA 25.10.19** at first mention and cites Rue et al. (2009), with SPDE references retained.
- Full package roles and reproducible dependency manifests remain versioned under `dependencies/` and workflow artifacts.

## Double-anonymous review

- Submit anonymized main manuscript separately from title page.
- Remove author names, affiliations, acknowledgements and identifying repository links from the main manuscript and its Word metadata.
- Supply one corresponding author on the separate title page/submission form.
- Use an anonymized/private-for-review data-code link rather than the public author-identifying GitHub repository in the review manuscript.
- Generated `01_Main_Manuscript_Anonymized.docx` is structurally checked for known identifying strings and embeds exactly four Main figures.

## Figures and maps

- Keep the Main paper to four narrative figures unless a fifth is essential.
- Embed main figures and legends in the main document for review.
- Use lowercase panel labels **(a), (b), (c), (d)** in the figures and legends.
- Make every legend standalone by naming the geographical region and study taxon where applicable.
- Every map panel must include a bar scale; current national map panels use WGS84 longitude/latitude and a **100-km bar scale**.
- Supply separate vector PDF files and 600-dpi PNG review copies from the checksum-locked figure artifact.
- Keep the full YAMAP/iNaturalist/GBIF benchmark, full model tables and sensitivity grids in Supporting Information while reporting the 3.81-fold benchmark result in Main.
- Figure 3 must show the distributional heterogeneity behind the positive mean: median, sign proportion and scale/exposure sensitivities cannot be visually hidden.
- Figure 4 must distinguish the observed-map selection of 16 local isolates from the two uses of 10,000-map replay.
- Tables must remain editable and self-contained.

## Supporting Information

- Supply Supporting Information separately from the Main document.
- Prefer one combined editable file; the generated bundle provides `02_Supporting_Information_Appendices_S1-S6.docx`.
- Organize it as Appendices S1–S6; number tables/figures consecutively within appendices.
- Do not submit historical/debugging analyses merely because they remain versioned in the repository.
- Confirm every appendix, supplementary table and supplementary figure is cited from the Main or its relevant appendix text.
- Sources cited only in Supporting Information must remain in the supplementary reference sections rather than the Main list.

## Species-distribution-model reporting

- Supply the current SDM/model-building checklist (`JBI_sdm_model_building_checklist.md` / generated DOCX).
- Appendix S4 reports the five species, shared domain, occurrence filtering, target-group background, shared predictor screen, tuning grid, model selection, validation and repeated-build comparison.
- Preserve the occurrence-referenced transformation and its inference ceiling: support is not abundance, visitation, pollen transfer or selection pressure.
- Main reports all mandatory distributional diagnostics for the 67 local boundaries: mean +0.03590, median -0.00277, 49.3% positive pairs, q=0.08148 across 5/10/25 km, attenuation at broader radii and failure of raw support to reproduce the nominal result.
- The biological result is boundary heterogeneity, not a pervasive pigmented-side advantage. The 67 sites are passed forward as strongly positive, near-zero and negative field strata.
- The equal-elevation highland analysis then shows why broad overlap is an environmentally confounded guardrail rather than a second positive mechanism.
- The 5-km radius is described as the finest predeclared scale with enough replicated transitions and a population-neighbourhood comparison, not as an exact bumblebee foraging distance.
- Confirm at final upload whether the editorial office expects any additional ODMAP-formatted worksheet beyond the current checklist.

## Data and code

- JBI requires underlying data and code to be available to peer reviewers at submission.
- Preferred submission plan: private-for-peer-review Dryad or equivalent anonymized repository containing derived data, code, seeds, manifests and analysis outputs.
- Original YAMAP photographs are third-party content and cannot be redistributed; the restriction is stated explicitly.
- Insert the randomized private review URL before upload.
- Replace the private review link with a permanent public repository/DOI upon acceptance.

## Other submission files and declarations

- Separate title page with authors, affiliations, ORCIDs and corresponding-author information.
- Concise JBI-specific cover letter prepared as a template; complete the sign-off and re-check whether the live portal marks it mandatory.
- Taxon image: required by JBI; prepare one study-taxon image with documented ownership/permission.
- Conflict of Interest statement: finalize with all co-authors.
- Author Contributions: finalize with all co-authors using CRediT roles; keep names out of the anonymized file.
- Funding/Acknowledgements: finalize with co-authors and place only in identifying submission materials during double-anonymous review.
- Optional translated abstract: Japanese version prepared for upload as `Translated Abstract Not for Review`.
- Generative-AI disclosure: determine and document any use that falls within Wiley/JBI's current AIGC policy; do not treat this as an automated or editorial-only decision.

## Generated delivery package

- `.github/workflows/jbi-submission-bundle.yml` restores the locked figure artifact and generates:
  - anonymized Main DOCX with Figures 1–4;
  - one combined Supporting Information DOCX;
  - title-page, cover-letter, translated-abstract and SDM-checklist DOCX files;
  - separate PNG/PDF figures;
  - SHA-256 manifest, readiness report and ZIP archive.
- Every generated DOCX must convert successfully through LibreOffice before merge.
- `review_science_bundle_complete=true` does not imply `portal_ready=true`.
- Do not upload the package while any blocker in `Submission_Readiness.json` remains unresolved.

## Pre-submission scientific checks

- **Data killing part:** the Main reports that the matched YAMAP stream contained 3.81 times as many focal-species records as iNaturalist and explains how author screening, image hashing and deterministic phenotyping created a new national quantitative trait dataset.
- **Sampling-frame part:** YAMAP's mountain-route frame is treated bidirectionally. It may enrich plausible natural settings and compress a settlement gradient, but accessible routes can also concentrate observation, disturbance and human-mediated movement. The manuscript must not claim a one-way conservative human bias.
- **Phenotype killing part:** state and conditional intensity are presented as different observational layers, while explicitly acknowledging that restriction to pigmented flowers can create selection or collider-like distortion and does not prove independent regulatory pathways.
- **Broad killing part:** environment is interpreted through competing anthocyanin benefits and costs rather than significance alone. The cool-climate state association, temperature × seasonality interaction, drier-side intensity, negative ruggedness and null final RSDS effect are all given distinct ecological meanings and claim ceilings. A cross-fitted space-only sensitivity shows environment-aligned divergence beyond spatial continuity for pigmentation state but not conditional intensity, without relabelling that excess as genetic differentiation or adaptation.
- **Bombus killing part:** boundary heterogeneity is the Main result. A minority of strong contrasts raises the mean; most boundaries do not show the predicted direction. The result motivates stratified direct pollination and fitness tests rather than a national Bombus mechanism.
- **Highland guardrail:** equal-elevation comparisons remove the stronger-looking montane/alpine overlap, preventing shared mountain geography from being narrated as pollinator selection.
- **Local-design separation:** the 67 Bombus transitions are edge-based signed comparisons. The **16 current-Broad departures** are node-and-neighbourhood local isolates selected by a separate human-blind observed-map rule.
- **Replay part:** the first 10,000-map use calibrates event count and fraction; the second reselects event cells and recomputes human contrasts on each map to create the post-selection null. Replay does not select “natural-process-resistant” sites.
- **Human killing part:** event frequency is compatible with the natural model (P=0.27897/0.12609), and the leading 5-km population feature remains familywise inconclusive (global maxT FWER P=0.05479). The inference is doubly conditional and does not claim provenance.
- **Izu role:** the Izu Bombus-absence/breeding-system literature motivates why bumblebees are focal in this species; it is not evidence that bumblebee absence caused white flowers.
- **Synthesis:** the Discussion converges on spatially changing regulation, physiological benefit, thermal/hydraulic cost, local reproductive value, unresolved geography and possible contemporary movement.
- The final paragraph states that positive, heterogeneous and null results perform different inferential jobs rather than displaying only convenient results.
- Five-species community turnover remains Supporting Information.
- No claim that SDM support equals realized visitation or selection.
- No claim that YAMAP removes observer bias or that every mountain photograph is wild.
- No claim that the 16 isolates are anthropogenic, resistant to natural processes or more frequent than the finalized natural model expects.

## Remaining portal-controlled blockers

- randomized private-for-review data/code URL;
- permission-cleared taxon image;
- final authors, affiliations, ORCIDs and corresponding author;
- Acknowledgements/Funding, Conflict of Interest and CRediT contributions;
- final cover-letter sign-off and live-portal disclosure wording.

## Final portal check immediately before submission

Journal portals and wording can change. Re-open the official Wiley JBI Author Guidelines immediately before upload and verify the current file designations, mandatory fields, data-repository workflow, map requirements, taxon-image specifications and disclosure wording.