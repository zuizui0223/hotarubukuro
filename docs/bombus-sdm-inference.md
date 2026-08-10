# Bombus SDM inference — current manuscript hierarchy

## Why Bombus is not another national predictor

The broad flower-colour stage already models national environmental gradients and continuous residual geography. The Bombus SDMs are also generated from environmental predictors. A national `flower colour ~ environment + space + Bombus SDM` analysis would therefore mix a biological hypothesis with the same broad geography that produced the pollinator surfaces.

The current paper does **not** interpret an SDM coefficient as an independent pollinator effect. Instead it changes scale and comparison unit when the biological question changes.

## Fresh five-species SDMs

The current source build fits five Japanese bumblebee taxa on a common Honshu–Shikoku–Kyushu study domain using shared environmental predictors, target-group background, spatial block partitions, `maxnet`, and minimum finite AICc selection:

- *Bombus ardens*;
- *B. diversus*;
- *B. beaticola*;
- *B. consobrinus*;
- *B. honshuensis*.

The workflow is `.github/workflows/rebuild-bombus-sdm.yml` and the active source implementation is `source_build/build_bombus_sdm_mainland.R`.

The manuscript uses the frozen successful source-build artifact rather than silently refreshing GBIF records at submission time. A fresh live rebuild is a new source-build exercise and must not automatically replace the locked evidence.

## SDM interpretation ceiling

The surfaces represent **predicted relative habitat support**. They do not measure:

- bee abundance;
- realized local occupancy;
- visits to *C. punctata*;
- pollen transfer;
- reproductive success;
- selection on flower colour.

This distinction is carried through every downstream analysis.

## Species-specific occurrence-reference calibration

Raw cloglog values are not treated as cross-species probabilities. For each species, flower-cell support is calibrated relative to the support distribution at that species' observed occurrence-reference cells using an empirical CDF.

This yields a within-species, occurrence-referenced support scale. It asks where a flower cell lies relative to environments supporting observed occurrences of the same taxon; it does not turn suitability into abundance or visitation.

The current support builder is:

- `scripts/build_bombus_occurrence_reference_support.R`
- `.github/workflows/bombus-occurrence-reference-support.yml`
- `reproducibility/bombus_occurrence_reference_support_2026-08-09.md`

## Main 2: directional availability uses two documented broad focal pollinators

The Main pollinator estimand uses *B. ardens* and *B. diversus* only. The question is:

> Across abrupt nearby white→pigmented boundaries, does predicted availability of the documented broad focal bumblebee pollinators increase toward the pigmented endpoint?

This is tested at the strict 5-km local boundary scale after the transition set has been selected without Bombus values or direction.

Why two species rather than all five?

1. *B. ardens* and *B. diversus* are the documented broad focal pollinators relevant to the range-wide *C. punctata* system.
2. They remain widespread enough for local availability to vary within the national flower-colour template.
3. Adding montane/alpine taxa changes the estimand toward species replacement and broad Bombus biogeography.
4. An all-five maximum becomes structurally high under fresh SDMs because different species replace one another geographically.
5. The three montane/alpine taxa share high-elevation geography with pigmented flowers, and this apparent association disappears under near-equal-elevation local guardrails.

The correct wording is therefore **documented broad focal pollinators**, not “all effective Bombus”.

## Main result ceiling

The 5-km pure-transition mean contrast is positive, but the evidence is weak: the median is close to zero, the fraction of positive pairs is approximately one half, the effect attenuates at 10/25 km, the across-scale adjustment is not conventionally significant, and raw cloglog support does not reproduce the result.

The paper therefore states only that the sharpest nearby white–pigmented transitions show **weak local directional consistency** with focal-pollinator availability. It does not claim pollinator-mediated selection.

The absence of a corresponding relationship with pigmented-only intensity is biologically informative: if the local association reflects pollination at all, it is more naturally framed as maintenance/loss of a visible pigment state than as selection for progressively darker flowers.

## Supplement: five-species predicted-community turnover

All five species are used for a different, unsigned biogeographic question:

> Are sharp flower-colour boundaries also embedded in unusually large changes in predicted Bombus assemblage composition relative to nearby matched background edges?

This uses occurrence-referenced five-species composition and Hellinger distance, with geographic/elevational matching and spatial-block replication.

The result is interpreted as **pollinator-community boundary correspondence / potential functional pollination-context turnover**. It does not predict which assemblage should favour white or pigmented flowers and therefore is not a Main causal hypothesis.

## Montane/alpine guardrail

The raw national overlap of pigmented flowers with *B. beaticola*, *B. consobrinus* and *B. honshuensis* is largely shared high-elevation geography. When abrupt white–pigmented transitions are restricted to nearly equal endpoint elevations, the additional montane/alpine support difference disappears.

This negative guardrail is a key inferential result: visually compelling overlap between two environmentally structured biological variables is not sufficient evidence for a direct ecological mechanism.

## Why the analysis is integrated rather than omnibus

The sequence is intentional:

1. Main 1 establishes broad environment + spatial geography of flower colour.
2. That broad structure reveals why national Bombus-map overlap is ambiguous.
3. Main 2 changes scale to a directional local boundary contrast.
4. Supplementary five-species analyses characterize broader community replacement and guard against shared elevational geography.

Integration therefore means **connecting evidence across scales while preserving the scale appropriate to each process**, not placing all predictor families into one saturated national regression.

## Current files

- `source_build/build_bombus_sdm_mainland.R`
- `.github/workflows/rebuild-bombus-sdm.yml`
- `scripts/build_bombus_occurrence_reference_support.R`
- `scripts/run_bombus_local_sharp_transition.R`
- `scripts/run_bombus_spatial_replication_test.R`
- `reproducibility/bombus_occurrence_reference_support_2026-08-09.md`
- `reproducibility/bombus_local_sharp_transition_current_spec_2026-08-09.md`
- `reproducibility/bombus_local_sharp_transition_current_results_2026-08-09.md`
- `reproducibility/bombus_spatial_replication_test_spec_2026-08-09.md`
- `reproducibility/bombus_spatial_replication_test_results_2026-08-09.md`

Superseded all-five limitation gates, environment+SPDE Bombus refinements and earlier national fingerprint architectures are under `legacy/`.
