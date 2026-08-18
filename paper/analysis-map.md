# Current analysis map

The active paper follows one dependent inference chain. Previous stages create the comparison unit required by later stages; integration connects scales rather than collapsing them into one national regression.

| Layer | Active question | Comparison unit | Current role | Main evidence |
|---|---|---|---|---|
| Trait construction | Can an incidental hiking-photo stream recover quantitative flower-colour geography? | observation | Main | 1,922 screened observations; state and intensity separated |
| Broad environment + space | Which measured environmental associations and residual spatial scales structure each response? | observation / 1-km cell | Main | response-specific INLA-SPDE models; state-only environment-aligned excess over cross-fitted space-only expectation |
| Local focal Bombus | Does habitat opportunity rise from white to pigmented across independently fixed local boundaries? | 67 non-overlapping pairs | Main | mean +0.03590 at 5 km; heterogeneity and scale attenuation; highland overlap rejected as an independent mechanism after elevation matching |
| Continuous human context | Does isolation from the same colour covary with human exposure beyond natural geography and local flower-cell spacing? | all 1,305 cells; 674 pigmented | Main exploratory geometry | raw pigmented rho=0.251980, natural mean=0.132980, P=0.000200; relative rho=0.285498, natural mean=0.153616, P=0.000900 |
| Event calibration | Are restrictive pigmented-among-white configurations excessive, and which populations should be revisited? | 16 event-defined cells | Supporting / field targeting | count P=0.27897; fraction P=0.12609; local population contrast +0.06744, global maxT P=0.05479 |

## Continuous-isolation implementation

- Core geometry: `R/continuous_colour_isolation.R`
- One-shot entry: `analysis_sensitivity/run_continuous_colour_isolation_human_context.R`
- Observed geometry: `analysis_sensitivity/continuous_colour_isolation/01_observed_geometry.R`
- Natural-map replay: `analysis_sensitivity/continuous_colour_isolation/02_natural_map_guardrail.R`
- Reporting: `analysis_sensitivity/continuous_colour_isolation/03_reporting.R`
- Dedicated workflow: `.github/workflows/continuous-colour-isolation-human-context.yml`
- Design/status note: `reproducibility/continuous_colour_isolation_human_context_2026-08-18.md`
- Validated result: `reproducibility/continuous_colour_isolation_human_context_result_2026-08-18.md`

The analysis is post hoc because its motivating raw correlations were inspected before specification. It is not relabelled as preregistered. The density correction, fold diagnostics and 10,000-map guardrail are nevertheless frozen and independently validated.

## Event-family role

The retained event scripts and v20-v22 results remain active because they answer different questions: frequency calibration, local candidate-versus-white contrast and reproducible selection of provenance targets. They no longer define the primary human-context estimand.

## Canonical execution

`python run_pipeline.py audit` validates manuscript, figure, file-map and claim contracts. `python run_pipeline.py reproduce` restores checksum-locked evidence, regenerates the accepted Broad and local analyses, restores the validated continuous-isolation output, renders the four Main figures and builds the JBI review package.

## Checksum-locked artifact registry

| Artifact key | GitHub Actions artifact ID | ZIP SHA-256 |
|---|---:|---|
| `broad` | 9022276431 | `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240` |
| `bombus_sdm` | 9020226937 | `d5d639e8e00d1ccc2f887c53fa8041465905b29f6bca1127f816e8c7a649d708` |
| `final8_draws` | 9094339466 | `413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1` |
| `accepted_local_bombus` | 9023416810 | `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34` |
| `accepted_bombus_final8_audit` | 9119773035 | `51fc846d2f3d815d8bbf3c0b95647eabeb0acf731f1e6dd164c11a0dfe1b896f` |
| `accepted_human_context` | 9119306089 | `f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4` |
| `accepted_figures` | 9291438085 | `51dde9026c4348205c494db3594414c0f099166f8878dc1c82edbb173f9e2848` |
| `accepted_continuous_colour_isolation` | 9317087893 | `6fd26d9a938b68d3f0c56512cd1620597c740d44ba91ab5a7ccbb9daa99d5386` |
