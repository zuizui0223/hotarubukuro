# Paper analysis map

This is the entry point for the current manuscript. The repository has accumulated many exploratory analyses; **only the items listed here are part of the adopted paper path**. Historical alternatives remain reproducible but should be read under `legacy/` (or treated as candidates to move there during cleanup).

## The paper in one line

**YAMAP image census -> two-part flower-colour phenotype -> broad environment + spatial geography -> local Bombus availability test -> repeated local-departure event -> post-selection human-context characterization.**

The design connects scales rather than collapsing all predictors into one national model.

## Adopted Main path

| Act | Ecological question | Adopted implementation / evidence | Manuscript role |
|---|---|---|---|
| 0. Data/trait | Can incidental, route-linked photographs recover quantitative intraspecific trait geography? | `Data_S1.csv`, `Code_S1.py`, `source_build/extract_color.py`; YAMAP benchmark in manuscript/SI | Main motivation + Fig. 1; benchmark in SI |
| 1. Broad geography | How are pigmentation state and pigmented-only intensity structured by environment and continuous geography? | fresh current-input v11 phenotype boundary; `scripts/run_natural_predictive_model.R`; INLA/SPDE + blocked prediction | Main, Fig. 2 |
| 2. Local biotic hypothesis | Is pigmentation benefit relaxed where effective widespread Bombus availability is locally low? | widespread species (*B. ardens*, *B. diversus*); local matched/availability analysis; SDMs are proxies, not visitation | Main, deliberately weak/scale-specific claim |
| 3. Natural departure | Are locally isolated pigmented states more frequent than expected under the natural baseline? | `scripts/run_local_pigmented_isolates.R`, `scripts/refine_submission_isolate_null.R`; repeated predictive maps | Main, Fig. 4 |
| 4. Human follow-up | After candidates are fixed without human variables, do they have unusual human context? | `scripts/run_human_landscape_features.R`, `scripts/run_local_human_context.R`, `scripts/run_did_sensitivity.R` | Main follow-up; provenance not inferred |

## Supporting information, not the Main causal story

- YAMAP vs iNaturalist/GBIF data-volume and data-quality benchmark.
- Bombus SDM construction, model-selection tables, prediction hashes and reproducibility checks.
- Montane/alpine Bombus overlap as a **guardrail/sensitivity**: apparent overlap with pigmented mountain geography must not be interpreted as an independent pollinator mechanism unless it survives like-elevation controls.
- Community-turnover analyses: interesting biogeographic correspondence, but no directional ecological mechanism is assigned; keep in SI.
- Alternative radii, warning-free colour subsets, flowering-date checks, DID and other human-context sensitivities.

## Current submission manuscript

Start here: `submission/jbi/JBI_main_manuscript_anonymized.md`.

Submission support:

- `submission/jbi/JBI_main_figure_plan.md`
- `submission/jbi/JBI_supporting_information_outline.md`
- `submission/jbi/JBI_cover_letter.md`
- `submission/jbi/JBI_submission_checklist.md`
- `submission/jbi/validate_jbi_submission.py`

The older Ecology & Evolution drafts under `manuscript/` are historical writing stages and are **not** the current submission text.

## Reproducibility path

The current source boundary is the full 1,965-row `Data_S1.csv`; no historical sample size is a scientific target. Exact duplicate semantics and environmental/Bombus support determine the downstream analysis population.

The seeded Bombus source build remains reproducible through `source_build/`, `config/bombus_sdm.yml`, and its validation scripts. The current-input reconstruction driver is `scripts/run_reanalysis_current_inputs.sh`, with `scripts/run_downstream_current_inputs.sh` continuing the adopted downstream stages.

## What is legacy

`legacy/` is for superseded scientific specifications, historical fixed-n analyses, abandoned candidate definitions, alternative Bombus hypotheses, old manuscripts/figures, one-off diagnostics, and reconstruction prototypes. A legacy result may be scientifically informative, but it is not evidence used for a Main claim unless explicitly listed above or in the JBI Supporting Information outline.

### Cleanup rule

A file belongs outside `legacy/` only if it is needed to:

1. reconstruct the current source data/SDMs;
2. run or validate an adopted Main/SI analysis;
3. build current figures/tables; or
4. prepare the current JBI submission.

Everything else should be archived under `legacy/` with provenance preserved.

## Claim ceiling

- YAMAP: a useful alternative observation process and quantitative-trait source, not an unbiased survey.
- Environment/SPDE: broad conditional associations and spatial structure, not proof of adaptation or history.
- Bombus SDM: predicted availability/opportunity proxy, not abundance, visitation, pollen transfer, or selection.
- Montane Bombus overlap: guardrail unless independent signal survives niche/elevation matching.
- Human context: candidate characterization after natural calibration, not horticultural provenance.
