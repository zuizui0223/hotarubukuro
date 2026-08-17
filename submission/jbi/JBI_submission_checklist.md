# Journal of Biogeography — submission checklist

Checked against the current Wiley `Journal of Biogeography` Author Guidelines on **2026-08-17**.

## Article category

- Target: **Research Article**.
- Main numbered headers: **1. Introduction, 2. Materials and Methods, 3. Results, 4. Discussion**, followed by unnumbered Acknowledgements, References and declarations.
- Numbered subheadings such as 2.1–2.6 and 4.1–4.6 identify scientific topics; they do not repeat four parallel research questions.
- JBI asks authors to state the theoretical foundation and conceptual advance clearly. The submission framing is **intraspecific trait biogeography + scale-dependent ecological inference**, with *Campanula punctata* as the focal system rather than the only point of interest.
- The Main text follows one dependent mystery: national trait measurement reveals two phenotypes; broad environment reveals residual geography; broad pollinator overlap forces a local test; and apparent local exceptions are calibrated against natural maps before human context is read.
- The conceptual advance is the sequence itself: each answer exposes the next confounding layer, so the comparison unit changes as the biological explanation becomes more local.
- Repeatability is supported by detailed methods, versioned code, fixed seeds, checksum-locked artifacts and independent validators.

## Length and front matter

- Main-text target: <=6,000 words.
  - Current Introduction-through-Discussion count = **3,543 words** by repository validator.
- Title: <=115 characters including spaces; no abbreviations or Latin binomial.
  - Current title = 108 characters.
- Running title: <40 characters.
  - Current running title = 26 characters.
- Structured abstract: <=300 words under **Aim, Location, Taxon, Methods, Results, Main conclusions**.
  - Current abstract = **300 words** by repository validator.
- Keywords: 6–10, alphabetical.
  - Current count = 8.

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
- Keep the matched YAMAP/iNaturalist/GBIF benchmark, full model tables and sensitivity grids in Supporting Information.
- Tables must remain editable and self-contained.

## Supporting Information

- Supply Supporting Information separately from the Main document.
- Prefer one combined editable file; the generated bundle provides `02_Supporting_Information_Appendices_S1-S6.docx`.
- Organize it as Appendices S1–S6; number tables/figures consecutively within appendices.
- Do not submit historical/debugging analyses merely because they remain versioned in the repository.
- Confirm every appendix, supplementary table and supplementary figure is cited from the Main or its relevant appendix text.

## Species-distribution-model reporting

- Supply the current SDM/model-building checklist (`JBI_sdm_model_building_checklist.md` / generated DOCX).
- Appendix S4 reports the five species, shared domain, occurrence filtering, target-group background, shared predictor screen, tuning grid, model selection, validation and repeated-build comparison.
- Preserve the occurrence-referenced transformation and its inference ceiling: support is not abundance, visitation, pollen transfer or selection pressure.
- Confirm at final upload whether the editorial office expects any additional ODMAP-formatted worksheet beyond the current checklist.

## Data and code

- JBI requires underlying data and code to be available to peer reviewers at submission.
- Preferred submission plan: private-for-peer-review Dryad or equivalent anonymized repository containing derived data, code, seeds, manifests and analysis outputs.
- Original YAMAP photographs are third-party content and cannot be redistributed; the restriction must be stated explicitly.
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
  - anonymized Main DOCX with embedded Figures 1–4;
  - one combined Supporting Information DOCX;
  - title-page, cover-letter, translated-abstract and SDM-checklist DOCX files;
  - separate PNG/PDF figures;
  - SHA-256 manifest, readiness report and ZIP archive.
- Every generated DOCX must convert successfully through LibreOffice before merge.
- `review_science_bundle_complete=true` does not imply `portal_ready=true`.
- Do not upload the package while any blocker in `Submission_Readiness.json` remains unresolved.

## Pre-submission scientific checks

- The manuscript opens with one biological mystery: why white and pigmented flowers remain geographically structured within one species.
- The Introduction makes the dependency explicit: solving measurement exposes attribution; resolving broad geography exposes the scale problem; the broad natural model defines what can count as a local exception.
- Results contain three genuine reversals rather than four parallel outputs:
  - one apparent colour gradient separates into state and intensity;
  - a convincing broad Bombus overlap collapses under local/elevation control, leaving only weak focal correspondence;
  - visually striking local departures remain plausible under natural predictive maps.
- Discussion converges on one ecological model: the physiological and reproductive value of pigmentation changes across space, while population history and occasional human movement modify where variants occur.
- The final synthesis maps environmental geography to common-garden/fitness tests, residual space to genomics, local Bombus boundaries to realized pollination, and local departures to provenance work.
- Five-species community turnover remains Supporting Information.
- High-elevation Bombus remains a negative control, not a second positive mechanism.
- Main Bombus result is labelled weak/local and reports its median, sign proportion, scale attenuation and raw-support failure.
- No claim that SDM support equals realized visitation or selection.
- No claim that YAMAP removes observer bias or that every mountain photograph is wild.
- No claim that the **16 current-Broad departures** are anthropogenic or more frequent than the finalized natural model expects.

## Final portal checks immediately before submission

Journal portals and wording can change. Re-open the official Wiley JBI Author Guidelines immediately before upload and verify the current file designations, mandatory fields, data-repository workflow, map requirements, taxon-image specifications and disclosure wording.
