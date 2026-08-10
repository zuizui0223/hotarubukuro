# Paper pipeline — start here

This branch separates the analysis used by the current paper from exploratory history.

## The paper in one line

**YAMAP images → supervised quantitative flower-colour phenotype → broad environment + spatial geography → local Bombus availability test → replicated natural departures → post-selection human context.**

The scientific integration is sequential: each stage creates the defensible baseline or comparison unit required by the next stage. Do not collapse all predictors into one national regression.

## Active paper components

| Paper role | Active implementation / evidence | Main or Supplement |
|---|---|---|
| Data source and quantitative phenotype | `Data_S1.csv`, `Code_S1.py`, `source_build/extract_color.py`, `source_build/build_data_s1.py`, `docs/yamap-iecology-rationale.md` | Main + data-method supplement |
| Broad environment + continuous space | fresh current-input v11 boundary produced by `scripts/run_reanalysis_current_inputs.sh`; `R/natural_predictive_model.R`; `scripts/run_natural_predictive_model.R` | Main |
| Bombus source models | `config/bombus_sdm.yml`, `source_build/fetch_bombus_occurrences.R`, `source_build/canonicalize_bombus_occurrences.R`, `source_build/build_bombus_sdm_mainland.R`, rebuild validation scripts | Methods / Supplement; supplies local test |
| Local Bombus availability / pigmentation-state hypothesis | `scripts/run_bombus_limitation_gate_current_inputs.R` and matching validation/audit | Main |
| Bombus community turnover and montane-species checks | retained only as sensitivity/guardrail evidence | Supplement |
| Natural departures / pigmented isolates | `R/natural_predictive_model.R`, `R/local_pigmented_isolates.R`, `scripts/refine_submission_isolate_null.R` | Main |
| Human context after candidate definition | `R/human_landscape_features.R`, `R/local_human_context.R`, `R/did_sensitivity.R` plus drivers/validation | Main, with detailed sensitivities in Supplement |
| Current submission text | `submission/jbi/JBI_main_manuscript_anonymized.md` | Main manuscript |
| Submission package | `submission/jbi/` | JBI submission |
| Locked claim/reference material | `reproducibility/submission_reference/` and current-input reanalysis report | Reproducibility |

## Canonical execution path

1. Start with all 1,965 curated source rows in `Data_S1.csv`; do not hard-code 1,909 or 1,923 as the upstream population.
2. Rebuild the flower-colour upstream boundary and use the validated seeded Bombus SDM artifact with `scripts/run_reanalysis_current_inputs.sh`.
3. Run the unchanged downstream stages with `scripts/run_downstream_current_inputs.sh`.
4. The broad natural model establishes the environment + spatial template.
5. The main Bombus question is local availability/relaxation, not a national independent causal coefficient and not five-species community turnover.
6. Define locally discordant pigmented events without human predictors, replay the same event under the natural predictive model, and only then characterize human context.
7. Build figures and update `submission/jbi/` only from adopted outputs.

## Interpretation ceiling

- YAMAP provides a route-linked, author-reviewed quantitative trait sampling frame; it is not a random field survey.
- CIELAB measurements are visible optical phenotypes, not anthocyanin concentration or Bombus receptor contrast.
- Environment + SPDE describes broad geographical structure; the spatial field is not a named historical mechanism.
- Bombus SDMs are predicted availability/suitability proxies, not observed visitation, pollen transfer, or selection.
- The main Bombus result concerns whether pigmentation benefit may relax where widespread effective Bombus availability is low. Montane species are guardrails/sensitivities because their geography strongly shares the mountain niche.
- Human-context results characterize pre-defined departures and prioritize provenance tests; they do not demonstrate horticultural origin.

## What belongs in `legacy/`

Anything retained only because it documents how the final design was reached belongs in `legacy/`: fixed-1,909 runs, fixed-1,923 publication snapshots, superseded Bombus formulations (national fingerprint, turnover-as-main, sharp-transition prototypes, spatial-replication experiments), obsolete reconstruction wrappers, superseded Ecology & Evolution manuscripts, one-off reports/specifications, and validators whose acceptance criteria encode historical result identities.

`legacy/` is provenance, **not an import path**. Active paper code must never source executable material from it.

## Reader shortcuts

- **What is the story?** `submission/jbi/JBI_main_manuscript_anonymized.md`
- **Why this integrated design?** `submission/jbi/JBI_narrative_dependency_architecture.md`
- **How is the background staged?** `submission/jbi/JBI_background_architecture.md`
- **What goes in Main vs Supplement?** `submission/jbi/JBI_supporting_information_outline.md`
- **What are the four main figures?** `submission/jbi/JBI_main_figure_plan.md`
- **How do I rerun the adopted current-input analysis?** `scripts/run_reanalysis_current_inputs.sh` then `scripts/run_downstream_current_inputs.sh`

Historical alternatives should not be cited as current results merely because their files remain in Git history or under `legacy/`.
