# Specification: placement-null human-context reanalysis (v22)

**Status:** predeclared. This file is committed **before** any statistic in Sections 4–7 is computed.
**Spec version:** `v22.0_placement_null_human_context`
**Supersedes:** nothing. This specification **adds** tests. No v20/v21 result is withdrawn, rewritten or removed from the manuscript.

---

## 1. Why this reanalysis exists

The v20 natural calibration generates 10,000 predictive maps, applies the event detector to each, and then reduces every map to two scalars:

```r
candidate_count     = colSums(simulated_candidate)
candidate_fraction  = colSums(simulated_candidate) / pmax(colSums(simulated_supported_present), 1)
```

`colSums()` discards the identity of the cells each simulated map selected. The retained statistics answer **"does nature produce this many such configurations?"** (answer: yes; count P = 0.27897, fraction P = 0.12609).

The paper's target question is different: **"does human context accompany the configurations that actually exist?"** That question is about *where* events sit, not *how many* there are. The count statistic cannot address it, and no excess-count result is required for it to be answerable.

**Declared basis for revision (outcome-independent):** the primary statistic did not match the target question. This justification does not depend on the value of any P statistic and would have been valid before v20 was run. The count calibration is **retained**, with its role stated explicitly as *establishing the right to name sites*, not as evidence of anomaly.

**Explicitly not a basis for revision:** the value of the v21 global maxT statistic (P = 0.05479). No threshold-clearing objective enters this specification. Section 8 fixes reporting rules that apply identically to supporting and refuting outcomes.

---

## 2. Frozen inputs

Reused without modification, by checksum:

| Input | Identity |
|---|---|
| Broad/current cells | run `31258851297`, artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240` |
| Predictive draws (10,000 natural maps) | artifact `9094339466`, SHA-256 `413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1` |
| Local-departure / human replay | run `31537102360`, artifact `9119306089`, SHA-256 `f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4` |

Unchanged components: the 1,305-cell analysis frame; per-cell `n_observations` used as binomial size; the cross-fitted natural model and its eight abiotic axes; the five geographical folds; the neighbour graph built once by `v20_neighbour_graph()` from observed `cells` and shared by observed and simulated profiles; the event rule (pigmented focal cell, ≥3 eligible neighbours within 10 km, RMS environmental distance ≤ 1, every eligible observed neighbour white); all seeds.

**Frame equivalence (verified, not assumed):** simulated maps are binomial draws at the same 1,305 cells with per-cell trials equal to observed `n_observations`, and both observed and simulated profiles are evaluated against the same neighbour graph, the same environmental distance matrix and the same `supported` flags. Trail-based sampling geography, per-cell effort and neighbourhood structure therefore enter the null at the same rate as they enter the observation. No restriction of the evaluation frame is required.

---

## 3. What must not change

The following are fixed and may not be adjusted after any statistic in this specification is computed:

- event-rule geometry (radius 10 km, caliper 1, minimum 3 neighbours, all-white requirement);
- the 16 observed candidate identities;
- the primary human variable (Section 5);
- the scale ladder (Section 6);
- the natural model and its predictor set;
- the direction of the primary detector.

Configurations already listed in `v20_configuration_table()` remain sensitivities with their existing roles.

---

## 4. Primary new test — placement null

**Question.** Given that nature produces approximately the same *number* of these configurations, do the configurations that actually exist sit in more human-exposed locations than the ones nature places?

**Statistic.** Let `h` be the rank-transformed human exposure vector over all 1,305 cells (Section 5), `C_obs` the observed candidate indicator (16 cells), and `C_sim` the 1,305 × 10,000 logical matrix already returned as `simulated_profile$candidate`.

```
E_obs      = mean(h[C_obs])
E_null[d]  = sum(h * C_sim[, d]) / sum(C_sim[, d])      for draw d = 1..10,000
```

**Inference.** One-sided upper-tail comparison of `E_obs` against `{E_null[d]}` via the existing `v18_null_comparison(observed, simulated, "greater")`, reporting observed value, null mean, null SD, central 95% interval, percentile and Monte Carlo SE.

**Degenerate draws.** Draws with `sum(C_sim[, d]) == 0` are excluded and their count reported. Exclusion is defined here, before the number is known.

**Why this null is the correct one.** The natural maps are generated from measured environment and fitted spatial continuity only; no human variable enters the natural model, the candidate selection or the neighbour graph. Simulated events therefore fall wherever environment and unresolved geography put them — including, if the correlation exists, in warm lowland white-dominated regions. The confound named in the manuscript (that short-range population exposure may proxy western-Japanese lowland geography) is consequently absorbed **into the null distribution itself**, rather than being argued away in prose. This is the property the v21 neighbour contrast does not have.

**Relationship to v21.** The two tests share the target but not the baseline, and both are reported:

| Test | Baseline | Question |
|---|---|---|
| v21 `v21_local_contrasts()` | candidate cell vs its own white neighbours | is the candidate more exposed than its immediate surroundings? |
| v22 placement null | observed candidates vs simulated candidates | is the candidate more exposed than where nature would place it? |

`v21_local_contrasts()` accepts a `candidate` matrix argument, so the **identical contrast statistic** may additionally be replayed across the 10,000 simulated candidate matrices. This variant is declared here as a secondary form of the primary test: statistic unchanged, baseline replaced.

---

## 5. Primary human variable

**Primary: population exposure within 5 km, rank-transformed over all 1,305 cells via `v19_rank01()`.** This choice is fixed and does not depend on any result.

Rationale, declared in advance:

1. **Causal chain length.** The horticultural-introduction hypothesis runs residence → planting → escape or planted individual → establishment or pollen flow from pigmented plants. *Campanula punctata* has small gravity-dispersed seed, rhizomatous vegetative spread, and bumblebee-mediated pollen movement over hundreds of metres to a few kilometres. No link in that chain operates at 25 km. A 25 km window measures regional urbanisation, not propagule pressure, and is therefore a poorer operationalisation of the hypothesis.
2. **Geometry of the existing contrast.** The v21 statistic compares a candidate with white neighbours inside 10 km. A 25 km exposure window is shared almost entirely between case and controls, so the contrast is structurally compressed toward zero irrespective of biology. A short window is required for the v21 statistic to be able to express a difference at all.
3. **Separability from the regional confound.** Long-window population exposure is close to collinear with the lowland/western geography that also carries white dominance. Short-window exposure can vary within a mountain block and is therefore the scale at which the hypothesis is most distinguishable from its leading alternative.

The rank transform is computed once over all cells and is outcome-independent. Missing exposure values, if any, exclude the cell from both observed and simulated statistics identically; the count is reported.

**No new human variables are introduced.** Distance-to-settlement and building density are arguably more direct proxies for propagule pressure, but adding variables now would convert a declared scale prediction into a variable search. The existing exposure variable at declared radii is sufficient for the discrimination in Section 6.

---

## 6. Scale ladder as a discriminating prediction, not a robustness check

Radii: **2, 5, 10, 25 km**, run on the **placement null only** (Section 4), never on the v21 neighbour contrast.

The v21 neighbour contrast compresses mechanically at radii approaching and exceeding the 10 km neighbour graph, so a decay observed there would be an artefact of the comparison geometry rather than evidence about biology. The placement null compares across the full cell frame and does not carry that compression.

**Competing predictions, fixed before execution:**

| Hypothesis | Predicted profile across 2 → 25 km |
|---|---|
| Horticultural introduction (propagule pressure) | maximum at short radius, **monotone decay** with increasing radius |
| Regional-geography proxy (lowland / urbanised west) | **flat or increasing** with radius |

The profile shape is therefore an additional falsifiable prediction rather than an additional degree of freedom. A flat or increasing profile is a substantive refutation of the horticultural reading and will be reported as such.

**Inference rules for the ladder:**

- the primary claim is the **shape** of the profile, not per-radius significance;
- per-radius statistics are reported with effect sizes and null intervals, and are treated as one maxT family; no radius is declared significant in isolation;
- the 5 km radius remains primary **regardless of which radius yields the largest contrast**;
- radii below 2 km are excluded in advance: population sums in montane cells approach zero and the rank transform degenerates under the resulting floor effect.

This mirrors the 5/10/25 km attenuation logic already used for the local *Bombus* test, giving the paper one consistent methodological stance across its biotic and anthropogenic sections.

---

## 7. Negative control — reversed detector

The event rule is directional by hypothesis: pigment appearing among white, not white appearing among pigment. The direction follows from (i) white morphs commonly arising from loss of function in the anthocyanin pathway, so pigmented phenotypes are expressed in crosses, and (ii) horticultural material being predominantly pigmented. The reversed configuration therefore serves as a control the hypothesis predicts should be null.

**Reversed event definition.** Focal cell with `n_pigmented == 0`; ≥3 eligible neighbours within 10 km at RMS environmental distance ≤ 1; every eligible neighbour pigmented. Implemented by inverting `present` in `v20_local_profile()` and requiring neighbour pigment share = 1. All other machinery — graph, folds, draws, human variable, rank transform, null comparison — is identical.

**Prediction.** Elevated exposure for pigmented-among-white; **no elevation** for white-among-pigmented.

**Interpretation grid:**

| Forward | Reverse | Reading |
|---|---|---|
| elevated | null | supports directional human introduction |
| elevated | elevated | exposure indexes regional geography, not directional introduction |
| null | null | no detectable human placement signal at this scale |
| null | elevated | unanticipated; reported as such, not reinterpreted |

**Asymmetry note.** Forward and reverse detectors are not exactly symmetric: `present` is defined as ≥1 pigmented observation in a cell, so in multi-observation mixed cells the two conditions are not complements. Observed counts of eligible focal cells under each direction are reported so that any difference in detector opportunity is visible rather than absorbed into the comparison.

---

## 8. Reporting rules, fixed in advance

- Effect sizes and null intervals are primary. No threshold language ("significant", "confirmed") is used for any result in this specification.
- **Expected power is low.** Each simulated statistic averages roughly 13.6 cells, so the null distribution of a mean exposure rank will be wide. A null outcome is an expected possibility and is reported with the same prominence and wording as a supporting outcome.
- The v20 count and fraction results (P = 0.27897; P = 0.12609) remain in the manuscript unchanged, described as calibration of the selection rule.
- The v21 result at 5 km (+0.06744; directional P = 0.00800; global maxT FWER P = 0.05479) remains in the manuscript unchanged.
- Claim ceilings retained verbatim: the 16 cells are field and provenance targets, not demonstrated anthropogenic populations; no result here establishes horticultural origin, and no photograph-derived phenotype is treated as evidence of cultivar identity.
- New claim ceiling: an elevated placement statistic shows that observed configurations sit in more human-exposed locations than the fitted natural process places them. It does not establish introduction, gene flow, or the direction of any pollen movement.

---

## 9. Implementation touchpoints

| File | Change |
|---|---|
| `R/local_pigmented_isolates.R` | `v20_metric_rows()`: retain `colSums()` metrics; add placement statistics computed from `simulated_profile$candidate` without collapsing cell identity. Add reversed-direction profile option. |
| `R/local_human_context.R` | expose `v21_local_contrasts()` for replay across simulated candidate matrices; add 2/10/25 km exposure ranks alongside the existing 5 km variable. |
| `R/candidate_null_tools.R` | reuse `v18_null_comparison()` unchanged; no modification to the post-selection facet boundary. |
| `config/paper_pipeline.lock.json` | add stage `run_placement_null_human_context` with seed, inputs and output checksums. |
| `.github/workflows/` | new workflow mirroring `human-context-highrep-final.yml`. |

Outputs: per-radius placement summary table, per-draw null distributions, reversed-detector summary, and a profile figure showing observed contrast against null intervals across the four radii.

---

## 10. Execution protocol

1. Commit this file to `reproducibility/`.
2. Tag the commit. The tag precedes any run of the new stage.
3. Execute the new stage with the frozen inputs of Section 2.
4. Lock outputs by checksum in `config/paper_pipeline.lock.json`.
5. Report the outcome as written in Section 8, whichever direction it takes.

Steps 1–2 are the substance of this document. The public commit history is what makes the declared basis in Section 1 verifiable; the specification loses its function if it is committed after step 3.
