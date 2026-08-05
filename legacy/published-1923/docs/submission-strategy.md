# Submission strategy for the locked manuscript

Decision date: 2026-07-24

Official journal information checked: 2026-07-24

## Decision

**First target:** *Ecology and Evolution*, Research Article

**Fallback:** *AoB PLANTS*, Study

**Current gate:** **NO-GO for upload; GO for submission-package completion.**

The analytical result is locked and ready to support a paper. The repository reports:

- 17/17 independent validation checks passed;
- 12/12 claim-audit checks passed;
- all 17 pipeline stages completed without failure;
- the two-part phenotype is the only retained response;
- the national *Bombus* gain remains small;
- local predicted-*Bombus* turnover is supported for both response parts at the planned 25-km scale;
- local pigmented isolates are not excessive under the natural model; and
- human-context results remain exploratory and do not demonstrate horticultural origin.

Evidence: [`final_independent_validation.csv`](../results/final_analysis_pipeline/final_independent_validation.csv), [`final_claim_audit.csv`](../results/final_analysis_pipeline/final_claim_audit.csv), and [`manuscript-story.md`](manuscript-story.md).

The upload remains blocked by four author-controlled items:

1. YAMAP terms, permissions, privacy, attribution, and releasable identifier/coordinate fields are not yet signed off.
2. The data-and-code package has not been deposited with a DOI; the manuscript statement is still provisional.
3. Full affiliation, corresponding-author email/ORCID, and funding information are not final.
4. Final submission files have not been checked against the journal upload system.

No new biological observations are required to clear this gate. If the derived analysis data cannot be shared at a level sufficient to reproduce the results, *Ecology and Evolution* becomes **NO-GO**, because the journal requires data, metadata, and code at submission and states that manuscripts submitted without data will not proceed to an editor.[^ee-author]

## Why *Ecology and Evolution* first

The journal welcomes research across ecology, evolution, and conservation, uses a Research Article category, places no page limit on manuscripts, and permits free-format initial submission.[^ee-home][^ee-overview][^ee-author] Its published record includes work assessing colour measurements from citizen-science photographs and work using social-media observations while explicitly evaluating recorder effort.[^photo-colour][^social-media]

This makes the journal plausible for the present combination of supervised digital phenotyping, spatial ecology, predicted community turnover, and explicit negative/exploratory findings. It does **not** make the paper automatically in scope. The manuscript must read as one ecological study, not as several pipelines joined together.

The current *Ecology and Evolution* APC is USD 2,700 / GBP 1,960 / EUR 2,210, with no submission or page fee.[^ee-apc] Kyoto University is listed under Wiley's Japanese institutional agreement for primary research in fully OA and hybrid journals, subject to corresponding-author eligibility, available allocations at acceptance, and institutional approval.[^wiley-japan][^wiley-institutions] Confirm this with the Kyoto University library before upload.

## Fallback trigger

Use *AoB PLANTS* after:

- an *Ecology and Evolution* desk rejection based on scope or editorial priority;
- a reviewed rejection where the biological conclusions remain sound after revision; or
- confirmation that Wiley OA funding is unavailable and the author prefers the lower stated APC.

*AoB PLANTS* directly covers rigorous organismal, environmental, ecological, and evolutionary plant research, including descriptive and confirmatory studies.[^aob-guide] Submit as a **Study**. The limit is 6,000 words from Introduction through Acknowledgements and 10 total figures plus tables. The current manuscript has approximately 4,905 whitespace-delimited words from Introduction through Conclusions and has four main figures, two main tables, and one appendix table, so it is structurally compatible after normal word-count verification.

The journal is fully OA, CC BY, with a stated charge of GBP 1,118.[^aob-charge][^aob-license] Its initial review is double-anonymous, so use a separate title page and an anonymized manuscript, supplement, filenames, and review-data link.[^aob-guide]

## Title and framing

### Submission title

> **Flower-colour state and conditional visible intensity in *Campanula punctata*: environmental and spatial structure across Japan**

Use `visible intensity`, not `pigment concentration` or `anthocyanin concentration`. The measurement is uncalibrated human-visible image colour. The title deliberately does not advertise *Bombus* selection or horticultural origin, because those are not directly demonstrated.

### One-sentence framing

Supervised route-linked photographs provide a nationwide quantitative floral-colour dataset when pigmentation state and conditional visible intensity are separated, broad environmental and continuous spatial structure define the baseline, and predicted biotic turnover and human context are evaluated only at the scales and claim strengths their proxies support.

### Claim order

1. **Core confirmation:** supervised YAMAP phenotype; pigmentation presence versus pigmented-only visible intensity.
2. **National inference:** conditional environmental clines and continuous residual geography from INLA-SPDE.
3. **Planned local test:** correspondence between flower-colour turnover and a predicted five-species *Bombus* fingerprint.
4. **Candidate definition:** local pigmented isolates replayed on 1,000 natural predictive maps.
5. **Exploratory ceiling:** weak human-context directions; no horticultural-provenance conclusion.

Do not promote stage 3 or 5 above the two-part phenotype and nationwide analysis in the title or opening abstract sentence.

## Article architecture

Use *Ecology and Evolution*'s Research Article structure:

1. Title page
2. Abstract
3. 4–6 keywords
4. Introduction
5. Materials and Methods
6. Results
7. Discussion
8. Conclusions
9. Data Accessibility Statement
10. Acknowledgements and funding
11. Author Contributions using CRediT
12. Conflict of Interest
13. AI-use disclosure
14. References
15. Tables
16. Figure legends
17. In-article appendices

The current abstract is 273 whitespace-delimited words and has six keywords. The journal does not state a page limit or a fixed abstract word limit in its current public guidance, but concise presentation remains preferable.[^ee-author][^ee-overview]

## Appendix versus Supporting Information

**Decision for the initial *Ecology and Evolution* submission: use in-article appendices, not a general Supporting Information file.**

### In-article appendices: essential for evaluating inference

Keep the following in appendices attached to the main article because *Ecology and Evolution* has no page limit and copyedits and typesets appendices, whereas Supporting Information is published as supplied.[^ee-author]

- **Appendix A — Sampling and optical measurement:** YAMAP frame, manual confirmation, duplicate/taxonomic screening, colour extraction, mixture boundary, QC rules, and why white-flower a* is not a pigment-quantity response.
- **Appendix B — National and predicted-community models:** full INLA-SPDE formulae and priors, spatial folds, ENMeval selection, fingerprint construction, common support, and the distinction between suitability and abundance.
- **Appendix C — Local reference tests:** pair construction, natural-map replay, local-isolate definition, maxT families, and the separation between candidate definition and human-context characterization.

These details determine what the estimands mean and must not be available only in an unedited supplement.

### DOI archive: full diagnostics

Place full foldwise tables, tuning grids, null draws, large sensitivity tables, and machine-readable diagnostics in the DOI archive. Cite each archived file and describe it in the data-package README. Do not duplicate these machine-readable outputs in a PDF.

Create one Supporting Information S1 PDF **only if** the upload size or editor requires secondary visual diagnostics to be separated from the article. It must contain no method or result required to evaluate a headline claim. The existing [`supporting-information-plan.md`](../manuscript/supporting-information-plan.md) is therefore a source inventory, not the required final packaging plan.

## Data and code package

Deposit a versioned, DOI-bearing archive before submission. A GitHub branch alone is not a permanent data archive.

### Required contents

- `Data_S1.csv` with the final permitted fields;
- a data dictionary defining units, missing values, QC flags, provenance status, and release restrictions;
- exact 1-km cell, local-edge, candidate, and human-context analysis tables used in the paper;
- complete `R/`, `scripts/`, `validation/`, and test code at the recorded commit;
- ENMeval occurrence-query provenance, cleaning rules, tuning grid, selected models, and prediction hashes;
- public raster source/version registry, download scripts, resampling rules, and checksums;
- model formulas, seeds, fold definitions, configuration, and input/output manifests;
- all adopted compact result tables and the final validation/claim registries;
- `R_session_info.txt` plus an environment lock or explicit package versions;
- the dated publication snapshot manifest and repository commit SHA;
- a top-level README giving `verify`, `extensions`, and `full` reproduction commands;
- a licence for original code and separately stated reuse terms for author-generated derived data.

### Do not silently include

- original YAMAP user photographs;
- third-party data under a new author licence;
- direct identifiers or coordinates whose release has not been approved;
- superseded analyses, residual-as-response results, individual *Bombus* coefficients, or old horticultural classifiers.

If original photographs cannot be redistributed, the archive should contain the permitted source identifiers or stable references, retrieval date, image hash, reviewed derived measurement, QC provenance, and an explicit access restriction. If exact reviewed coordinates cannot be released, determine whether the reproducible 1-km cell data can be released and ask the journal office whether this satisfies review and publication requirements before submitting.

Replace the provisional Data Accessibility Statement with the DOI and exact file-level access description. *Ecology and Evolution* requires data, metadata, and code at submission; formal public archiving and linked Data Accessibility Statement are required by revision.[^ee-author][^ee-minimum]

## Cover-letter pitch

Use the following as the substantive core; add the formal author declarations and repository DOI.

> Dear Editors,
>
> We submit “Flower-colour state and conditional visible intensity in *Campanula punctata*: environmental and spatial structure across Japan” as a Research Article. Geographical studies of intraspecific floral colour are constrained by sparse quantitative trait data and by the tendency to mix pigment absence, conditional colour intensity, broad spatial structure, and local biotic hypotheses. We converted 1,965 author-reviewed, route-linked YAMAP photographs into a two-part optical phenotype and used geographically blocked INLA-SPDE prediction to define the national environmental and spatial baseline.
>
> Temperature was conditionally associated with both response parts, while soil and topographic axes differed between pigmentation state and pigmented-only intensity. A predicted five-species *Bombus* fingerprint added little national discrimination but corresponded to local turnover in both colour components relative to 1,000 natural-map replications. Locally isolated pigmented observations were not excessive under that baseline, and human-context contrasts did not pass familywise correction. These bounded positive and negative results show how non-random community photographs can support scale-specific ecological inference without treating predicted suitability as pollinator abundance or exploratory human context as horticultural provenance.
>
> The complete derived data, metadata, code, seeds, model-selection records, and independent validation are available at [DOI]. The work is original, is not under consideration elsewhere, and has been approved by all authors.

Do not call the study the first of its kind. Do not pitch it as proof of pollinator-mediated selection or horticultural escape.

## Main figures, tables, and upload files

### Main presentation

- Figure 1: two-part digital phenotype
- Figure 2: environment and continuous spatial structure
- Figure 3: national versus local predicted-*Bombus* information
- Figure 4: isolate reference test and bounded human-context result
- Table 1: public spatial data and role in analysis
- Table 2: locked results and claim ceilings

Four figures plus two tables are proportionate and remain below the *AoB PLANTS* fallback limit of 10 presentation items.[^aob-guide]

### *Ecology and Evolution* upload set

1. Editable main `.docx`, with tables on separate pages after the references and complete figure legends.
2. Four separate production-quality figure files. Use vector PDF/EPS for maps and line graphics where accepted, and TIFF for raster components; do not use the embedded review PNGs as the only submission masters.
3. Cover letter.
4. DOI-linked data-and-code archive.
5. One Supporting Information S1 PDF only if the appendices or secondary diagnostics must be separated.
6. Any optional Japanese translated abstract as a separate file; omit unless the author wants it.

All tables and figures must be cited in order. Wiley recommends separate figure files, preferably EPS/TIFF, and editable text/table formats.[^ee-author] Check image dimensions, font embedding, colour-blind accessibility, and alt text before upload.

### *AoB PLANTS* conversion if needed

- anonymized single manuscript PDF plus separate title page;
- at least 1.5 line spacing, line and page numbers;
- no more than 6,000 words from Introduction through Acknowledgements;
- no more than 10 figures plus tables;
- DOI supplied for references where available;
- each figure legend followed by an `Alt text:` description at submission;
- after acceptance, figures prepared to the journal's stated 600-dpi JPG, RGB/grayscale, and size requirements;
- permanent open-access data repository or Supporting Information access as required.[^aob-guide]

## Desk-reject risk register

| Priority | Risk | Submission control |
|---:|---|---|
| 1 | Required data/code unavailable or YAMAP release basis unclear | Hard NO-GO until permissions and DOI package are resolved |
| 2 | Paper looks like several pipelines rather than one ecological question | Lead with the two-part phenotype; present *Bombus* and human context as scale-specific extensions |
| 3 | Opportunistic photograph colour is treated as calibrated pigmentation | Use `visible intensity`; put extraction/QC/limitations in Appendix A; cite direct photographic-colour validation literature |
| 4 | Observation opportunity is confused with human influence | State that population/access variables can represent both processes; retain the negative corrected result |
| 5 | Predicted *Bombus* suitability is presented as abundance, visitation, or selection | Use `predicted community fingerprint` and `turnover correspondence` everywhere |
| 6 | Exploratory candidate analysis is presented as horticultural escape | Report non-excess isolates and non-significant corrected human contrasts; reserve provenance for field/genetic follow-up |
| 7 | Single-species geography lacks wider relevance | Emphasize the general response hierarchy and scale-matched inference, not Japanese natural history alone |
| 8 | Public repository and AI-assisted preparation are not transparently disclosed | Add DOI statement and Wiley-compliant AI disclosure before upload |
| 9 | Formal submission metadata are incomplete | Finalize authorship, affiliation, funding, acknowledgements, ORCID, conflicts, and file order |

Wiley requires transparent disclosure when AI technologies contributed to manuscript preparation or research work beyond basic spelling and grammar, with human verification and responsibility retained.[^wiley-ai] The author should disclose the actual uses of Codex in code development/refactoring, validation support, and manuscript structuring/editing, without listing AI as an author or implying that it made the scientific decisions.

## Final go/no-go checklist

### Hard GO gates

- [ ] YAMAP use, attribution, privacy, identifier, coordinate, and derived-data release decisions documented
- [ ] journal office consulted if reproducibility requires restricted fields
- [ ] DOI archive deposited and tested from a clean checkout
- [ ] Data Accessibility Statement contains the DOI and accurately lists restrictions
- [x] appendices A–C complete and cited
- [ ] no appendix or optional Supporting Information item is cited unless it exists
- [ ] every manuscript number checked against `final_result_registry.csv`
- [ ] `verify --tests=true` rerun from the recorded release commit
- [ ] 17/17 independent validations and 12/12 claim checks still pass
- [ ] title and abstract retain the claim hierarchy
- [ ] author list, affiliation, corresponding email, ORCID, CRediT roles, funding, acknowledgements, and conflicts final
- [ ] AI-use disclosure accurately describes tools, purposes, and author verification
- [ ] four figures and two tables cited in order and uploaded in compliant editable/high-resolution formats
- [ ] final Word/PDF visually checked
- [ ] Wiley OA eligibility confirmed with Kyoto University

### Automatic NO-GO conditions

- derived analysis data cannot be made available for peer review;
- the DOI package does not reproduce the locked verification;
- the title or abstract claims pollinator-mediated selection or horticultural origin;
- individual species SDM coefficients are interpreted as independent pollinator effects;
- original YAMAP photographs or user information would be redistributed without an approved basis; or
- appendix or optional Supporting Information items referenced by the manuscript do not exist.

Once every hard gate passes, submit to *Ecology and Evolution*. Do not reopen the locked biological analysis merely to manufacture stronger *Bombus* or horticultural results.

## Official sources

[^ee-home]: *Ecology and Evolution*, journal home and scope, Wiley: https://onlinelibrary.wiley.com/journal/20457758
[^ee-overview]: *Ecology and Evolution*, overview and editorial philosophy: https://onlinelibrary.wiley.com/page/journal/20457758/homepage/productinformation.html
[^ee-author]: *Ecology and Evolution*, Author Guidelines: https://onlinelibrary.wiley.com/page/journal/20457758/homepage/forauthors.html
[^ee-apc]: *Ecology and Evolution*, Article Publication Charges: https://onlinelibrary.wiley.com/page/journal/20457758/homepage/open-access
[^ee-minimum]: Jenkins et al., journal minimum standards for data and code: https://onlinelibrary.wiley.com/doi/10.1002/ece3.9961
[^photo-colour]: Laitly et al., photographic-colour validation in *Ecology and Evolution*: https://onlinelibrary.wiley.com/doi/10.1002/ece3.7307
[^social-media]: Stephenson et al., social-media data and recorder effort in *Ecology and Evolution*: https://onlinelibrary.wiley.com/doi/10.1002/ece3.71086
[^wiley-japan]: Wiley, Open Access for Authors in Japan: https://authors.wiley.com/author-resources/Journal-Authors/open-access/affiliation-policies-payments/japan-agreement.html
[^wiley-institutions]: Wiley, Institutional Payments and Japan member institutions: https://authors.wiley.com/author-resources/Journal-Authors/open-access/affiliation-policies-payments/institutional-funder-payments.html
[^wiley-ai]: Wiley, Best Practice Guidelines on Research Integrity and Publishing Ethics, Artificial Intelligence: https://authorservices.wiley.com/ethics-guidelines/index.html
[^aob-guide]: *AoB PLANTS*, General Instructions: https://academic.oup.com/aobpla/pages/General_Instructions
[^aob-charge]: *AoB PLANTS*, Publication Charges: https://academic.oup.com/aobpla/pages/publication_charges
[^aob-license]: *AoB PLANTS*, Open-Access License and Copyright: https://academic.oup.com/aobpla/pages/oa_copyright
