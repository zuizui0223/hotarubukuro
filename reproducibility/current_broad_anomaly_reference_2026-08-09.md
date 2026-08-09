# Current broad natural-template and anomaly reference

Date: 2026-08-09

## Manuscript role

This note records the frozen numerical reference used by:

- **Main 1:** broad environment + continuous space for pigmentation state and conditional visible intensity;
- **Main 3:** event-based local departures and post-selection human-context follow-up.

It deliberately excludes the superseded all-five Bombus limitation gate. The current pollinator question is Main 2 and is tested separately at sharp local colour boundaries.

## Frozen source and artifact

- source table: `Data_S1.csv`, 1,965 rows;
- exact-image duplicate removed by stable source-row order: 1;
- phenotype analysis population: 1,922;
- white-like observations: 966;
- pigmented observations: 956;
- 1-km analysis cells: 1,305;
- response-blind CIELAB a* boundary: 4.968780;
- successful workflow run: `31258851297`;
- artifact: `9022276431`;
- artifact SHA-256: `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.

## Main 1 — broad natural template

Pigmentation state:

- image-level geographically blocked AUC: 0.86348;
- cell any-pigmented AUC: 0.85799;
- cell majority-pigmented AUC: 0.87066;
- Brier score: 0.15043.

Conditional visible intensity among pigmented observations:

- evaluated cells: 674;
- RMSE: 0.91924;
- MAE: 0.71472;
- 95% predictive coverage: 0.94362.

The observation-level INLA-SPDE model gives substantial residual geographical structure after measured environment. The broad model is therefore a geographical natural reference, not a claim that environment alone explains most phenotypic variance.

## Main 3 — event-based departures

Primary event: a pigmented cell embedded among geographically close, environmentally similar white neighbours under the predefined neighbourhood rule. Human variables do not enter event selection.

Observed candidates: 17.

Held-out cross-fitted natural-map replay, 10,000 maps:

- count: null mean 13.614; one-sided P=0.1996;
- candidate fraction: observed 0.04735, null mean 0.03427; P=0.0874.

Joint spatial posterior-predictive sensitivity, 200,000 maps:

- candidate-count P=0.3145;
- candidate-fraction P=0.1962.

Thus the observed local-departure frequency is not a robust excess over the fitted natural reference.

## Human-context follow-up

After candidate identities were fixed:

- 5-km population-rank contrast: +0.05306; directional P=0.02697; maxT-FWER P=0.08991;
- population-DID alignment: +0.05162; directional P=0.02298; maxT-FWER P=0.07592;
- broad multivariate human-landscape departure: global P=0.9011.

These are suggestive post-selection context signals only. Horticultural origin, planting, escape or introgression is not demonstrated.

## Current implementation

- `.github/workflows/reanalysis-current-inputs.yml`
- `scripts/run_reanalysis_current_inputs.sh`
- `scripts/run_downstream_current_inputs.sh`
- `scripts/report_reanalysis_current_inputs.R`

The downstream pipeline restores static human-landscape support files from `inputs/canonical_snapshot.json`. That retained snapshot is infrastructure for WorldPop/MLIT/DID inputs only; its old 1,909 flower-population identity is not an active manuscript constraint.

## Claim ceiling

Main 1 establishes a strong broad geographical template containing both measured environmental associations and unresolved spatial structure. Main 3 identifies reproducible local field/provenance targets but does not show an excess process beyond that natural template or a general anthropogenic origin.
