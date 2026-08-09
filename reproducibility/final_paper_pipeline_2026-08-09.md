# Final paper pipeline lock — YAMAP data layer + broad -> fine -> departure

Date: 2026-08-09

This file is the compact scientific lock for the current **Journal of Biogeography** manuscript. For the current repository entry point and complete file map, use `paper/README.md`, `paper/analysis-map.md` and `paper/active-file-map.csv`.

The paper is one dependent ecological argument, not an omnibus `environment + Bombus + human` regression and not a collection of all analyses tried during development.

## 0. Cross-cutting data layer — YAMAP as iEcology

The paper treats YAMAP as a methodological contribution because recreational hiking photographs can be converted into a recent, spatially explicit quantitative trait dataset.

Current data facts:

- predefined study window: 2023-2025;
- YAMAP source rows: 1,964;
- unique image hashes: 1,963;
- final phenotype observations: 1,922;
- matched iNaturalist photo+geo observations: 516;
- matched GBIF human-observation image records: 393, of which 389/393 were syndicated from iNaturalist;
- YAMAP yearly counts: 642/687/635.

The study-specific pipeline is:

`hiking photo -> date/GPS provenance -> exhaustive candidate review -> taxon/subject/petal-ROI validation -> image-hash/coordinate audit -> deterministic RGB/CIELAB extraction -> QC -> two-part flower-colour phenotype`.

YAMAP is therefore a complementary observation process, not an unbiased or universally superior platform. Route choice, access, flower conspicuousness, subject choice, hidden locations and uneven mountain use remain sampling biases. The mountain-route frame is nevertheless useful for natural/semi-natural mountain trait geography and can also compress the human-modification gradient available to Main 3.

Frozen benchmark:

- retrieval run/artifact: `31289927019` / `9031041034`;
- retrieval SHA-256: `3e53669395cfd926a0942b3488f844720dca2cb97b9ea210627262691e69f31a`;
- provider-overlap audit run/artifact: `31290095532` / `9031085975`.

## Main 1 — Broad quantitative natural template

**Question:** What broad environmental and continuous spatial structure organizes (i) pigmentation state and (ii) visible intensity among pigmented flowers across Japan?

Active population:

- source rows: 1,965;
- phenotype observations: 1,922;
- white-like: 966;
- pigmented: 956;
- ambiguous mixture assignments retained: 124;
- 1-km cells: 1,305;
- response-blind a* boundary: 4.968780.

Current implementation:

- `.github/workflows/reanalysis-current-inputs.yml`;
- `scripts/run_reanalysis_current_inputs.sh`;
- `scripts/run_downstream_current_inputs.sh`;
- `scripts/report_reanalysis_current_inputs.R`;
- `reproducibility/current_broad_anomaly_reference_2026-08-09.md`.

Frozen manuscript reference:

- run: `31258851297`;
- artifact: `9022276431`;
- SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

Key performance:

- pigmentation image AUC=0.86348;
- cell any-pigmented AUC=0.85799;
- cell majority-pigmented AUC=0.87066;
- Brier=0.15043;
- conditional-intensity RMSE=0.91924;
- MAE=0.71472;
- 95% predictive coverage=0.94362.

**Claim ceiling:** a strong broad geographical natural template containing measured environmental associations and substantial unresolved spatial structure. Predictive performance is not variance decomposition and does not show that environment alone explains most phenotypic variation.

## Main 2 — Fine-scale focal-pollinator availability

**Question:** Within the broad template, do abrupt nearby white-pigmented boundaries align directionally with predicted availability of the documented broad focal Bombus pollinators?

Primary exposure:

`effective_occmax = max(A_ardens, A_diversus)`

where each `A_k` is predicted habitat support transformed to its empirical rank relative to support values at that species' occurrence cells. It is not occurrence probability, visitation or selection pressure.

Why two species: *B. ardens* and *B. diversus* are the broadly distributed taxa with direct evidence as focal/predominant pollinators in the relevant *C. punctata* system. Adding montane/alpine taxa changes the estimand toward Bombus biogeographic replacement and re-imports the high-elevation geography already handled in Main 1.

Primary design:

- 1-km cells;
- five nearest eligible neighbours within 5 km;
- pure white-pigmented transitions (`abs(d pigment_share)=1`);
- greedy non-overlap;
- pair selection Bombus-blind and sign-blind;
- orient white -> pigmented only after selection;
- 100,000 sign flips;
- environment used only as a local-similarity diagnostic.

Current implementation:

- `scripts/build_bombus_occurrence_reference_support.R`;
- `.github/workflows/bombus-occurrence-reference-support.yml`;
- `reproducibility/bombus_occurrence_reference_support_2026-08-09.md`;
- `R/local_pair_graph.R`;
- `scripts/run_bombus_local_sharp_transition.R`;
- `.github/workflows/bombus-local-sharp-transition.yml`;
- `reproducibility/bombus_local_sharp_transition_current_spec_2026-08-09.md`;
- `reproducibility/bombus_local_sharp_transition_current_results_2026-08-09.md`.

Frozen support reference:

- run/artifact: `31262211605` / `9023137743`;
- SHA-256: `d04c561b09b652591b9b479f6e26a779bb562c7c1b5f9b14e61d5e7ca8e2794b`.

Frozen local-transition reference:

- run/artifact: `31263324505` / `9023416810`;
- SHA-256: `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`.

Focal 5-km result:

- n=67 non-overlapping pairs;
- median distance=2.0 km;
- selected median environmental distance=0.180 versus 0.343 for all local 5-km edges;
- mean pigmented-minus-white support=+0.035896;
- median=-0.002770;
- proportion positive=0.492537;
- one-sided sign-flip P=0.027160;
- BH q across 5/10/25-km pure primary tests=0.081479;
- raw-cloglog 5-km sensitivity P=0.267147;
- 10/25-km primary contrasts are null.

**Claim ceiling:** weak and highly local directional consistency with a pigmentation-benefit relaxation hypothesis. The mean is magnitude-driven rather than a majority-of-pairs pattern; the result does not establish pollinator-mediated selection.

## Main 3 — Event-based local departures + post-selection human context

**Question:** Which local colour-state configurations depart from the broad natural reference, how often does the fitted natural model itself generate the same event, and what human context characterizes independently defined candidates?

The inferential object is an ecological event rather than a raw fitted residual: a pigmented cell embedded among geographically close, environmentally similar white neighbours. Human variables do not enter candidate definition.

Calibration:

- 10,000 held-out cross-fitted natural maps;
- 200,000 joint spatial posterior-predictive maps as sensitivity.

Current result:

- 17 candidates;
- cross-fit count P=0.19958;
- cross-fit fraction P=0.08739;
- joint PPC count P=0.31446;
- joint PPC fraction P=0.19618.

Post-selection human context:

- 5-km population rank directional P=0.02697; maxT-FWER P=0.08991;
- population-DID alignment P=0.02298; maxT-FWER P=0.07592;
- broad multivariate human-context global P=0.9011.

**Claim ceiling:** reproducible field/provenance targets with suggestive human context only. Candidate frequency is compatible with the natural predictive reference, and horticultural origin is not demonstrated.

## Supporting Information — evidence, not extra Main stories

### Five-species Bombus boundary correspondence

Sharp flower-colour boundaries show greater predicted five-species Bombus compositional turnover than locally matched non-transition edges in several scale/matching configurations. Because Hellinger turnover is unsigned, this is interpreted only as **biogeographic correspondence between colour boundaries and predicted pollinator-community boundaries**, not as a directional colour-selection mechanism.

Frozen reference:

- run/artifact: `31285234317` / `9029595037`;
- SHA-256: `067dd3408b2a7f046ba263732ffa4cefa2f54a7f1fb672478be46bca425f6bf0`.

### Montane/alpine elevation guardrail

Among pure transitions with endpoint elevation difference <=50 m, mean pigmented-minus-white support from *B. beaticola* + *B. consobrinus* + *B. honshuensis* is non-positive at 5/10/25 km and all one-sided P-values are non-significant. The striking national overlap between montane/alpine Bombus and pigmented highland flowers is therefore treated as shared elevational biogeography rather than an additional pollinator mechanism.

Current implementation:

- `R/local_pair_graph.R`;
- `scripts/run_bombus_spatial_replication_test.R`;
- `.github/workflows/bombus-spatial-replication-test.yml`;
- `reproducibility/bombus_spatial_replication_test_spec_2026-08-09.md`;
- `reproducibility/bombus_spatial_replication_test_results_2026-08-09.md`.

## Integrated logic

Each layer removes a different ambiguity before the next question is asked:

1. **Data source:** recover a dense recent image stream and validate it ecologically.
2. **Trait construction:** turn photographs into an auditable two-part quantitative phenotype.
3. **Broad geography:** model environment while retaining continuous residual space.
4. **Local biotic question:** change scale rather than treating an environment-derived SDM as an independent national predictor.
5. **Biogeographic guardrail:** separate attractive high-elevation map overlap from an additional pollinator mechanism.
6. **Departure object:** replace residual tails with a repeatable local ecological event.
7. **Human context:** characterize pre-fixed departures rather than using human variables to create them.

The paper's novelty is therefore the deliberate alignment of **data source, trait representation, spatial scale and comparison unit** with the ecological question.

## Current manuscript-facing entry points

Use only:

- `paper/README.md`;
- `paper/analysis-map.md`;
- `paper/active-file-map.csv`;
- `submission/jbi/JBI_main_manuscript_anonymized.md`;
- `submission/jbi/` for the submission package;
- the current scripts/workflows/reproducibility files listed in `paper/active-file-map.csv`.

Superseded Ecology & Evolution drafts, old 1,909/1,923 architectures, all-five limitation gates, broad Bombus-null refinements, montane-substitution developments, old local-turnover code and obsolete submission registries are under `legacy/`.

Do not merge the cleanup branch into `main` without explicit approval.
