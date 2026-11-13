# Five-species Bombus SDM composition versus local photographed flower-colour divergence

**Execution date:** 2026-11-13  
**Status:** Completed on the 2026-11-11 successful reproduction artifact.  
**Primary finding:** The 5-km hypothesis **is not supported**. This result must not be promoted as evidence for pollinator-mediated or frequency-dependent selection.

## What was fixed before opening the five-species outputs

The proposed design was recorded on 2026-11-07 in [the prior draft branch](https://github.com/zuizui0223/hotarubukuro/blob/agent/bombus-pollination-specialization-20261107/docs/bombus_guild_first_design_2026-11-07.md). It calls for:

- Five species: *B. ardens*, *B. diversus*, *B. beaticola*, *B. consobrinus*, *B. honshuensis*.
- **Response:** absolute difference in the observed share of pigmented flowers between two 1-km cells.
- **Sole explanatory information:** Hellinger distance between their five-dimensional *relative occurrence-referenced SDM support* vectors. No elevation, climate, soil, radiation or broad INLA/SPDE terms enter this test.
- Fixed all-cell neighbour graph at **5 km primary**, with 10/25 km scale sensitivities.
- Spearman rank correlation, with a one-sided positive alternative.
- 10,000 permutations of colour counts **together with their photo counts** within fixed 50-km spatial blocks; spatial coordinates only define the graph and permutation strata. These are not a model of fine-scale environmental causation.
- Predeclared main-text promotion requires a robust **positive** strict-local relationship. A smaller P at another radius alone is not promotion.

The older 2-species source output was missing all three montane taxa and could not execute this test. The missing values were never interpreted as biological absence.

## Real five-species data recovered and verified

The completed public [GitHub Actions run #42953552095](https://github.com/zuizui0223/hotarubukuro/actions/runs/42953552095) uploaded **artifact 13831541196** (\`manuscript-replay\`). The relevant, directly checked paths are:

- \`analysis_cells/analysis_cells_1km.csv\` (1,305 cells; 1,922 retained photographs; 956 pigmented photographs)
- \`bombus_occurrence_reference_support/cell_effective_bombus_support.csv\` (five occurrence-referenced columns, each finite for **1,305/1,305** cell IDs)
- \`bombus_occurrence_reference_support/occurrence_reference_calibration.csv\` (five species and their actual occurrence calibration records).

The same archive contains \`analysis_reproduction/run_manifest.json\` with \`status=SUCCESS\`, a frozen spatial-input SHA-256 \`4f87f284641bcbeb1a012cd449cd5eb78bf3e1dfbadfd0c580e84ae40f7ad104\`, and the Zenodo-derived canonical analysis-table SHA-256 \`9e543b64a824aff82dbb55da1bca8843fb337a51399bfd60ad0a09c9bca3c33c\`. The five surfaces were reconstructed through the source-build; **no missing SDM values were imputed**. The full five species are now available and therefore the previous 3-species-missing blocker is resolved for this specific archive.

## Executed primary and secondary-window results

| Radius | Neighbour edges | Spearman rho | 50-km block null mean rho | One-sided 10,000-permutation P | Edges with >=2 flower photos on both sides | Their rho |
|---|---:|---:|---:|---:|---:|---:|
| **5 km (primary)** | **1,134** | **−0.013401** | −0.020941 | **0.393361** | 107 | −0.080570 |
| 10 km | 2,734 | +0.011381 | −0.037032 | 0.009899 | 228 | +0.024299 |
| 25 km | 11,694 | −0.016077 | −0.027593 | 0.188281 | 849 | +0.024776 |

All 1,305 cells have well-defined five-dimensional profiles, so none of these graph edges required exclusion for undefined five-species composition.

**Decision:** The 5-km primary rho has the wrong sign and is not significant against the fixed regional label null. The result is not a positive five-species signal. At 10 km the P-value is relatively small even though the observed coefficient is **only +0.0114** and the permutation mean is negative. This does **not** show a substantial ecological effect and the finding is not reproduced at 25 km or in stricter photo-replicated cells.

### Sensitivity and scale specificity

A separate *post-primary* sensitivity analysis checked block widths/offsets (5,000 permutations for each):

| Randomisation geography | P at 5 km | P at 10 km |
|---|---:|---:|
| 25-km blocks | 0.2258 | 0.0030 |
| 50-km blocks shifted 25 km | 0.2274 | 0.0078 |
| 100-km blocks | 0.1770 | 0.0104 |
| **100-km blocks shifted 50 km** | **0.6187** | **0.2062** |

The 10-km result depends on how local spatial structure is preserved. It cannot rescue the non-supporting primary result.

The planned leave-one-species-out direction check gives **5-km rho from −0.0483 to +0.0124** depending on omitted taxon. A separate five-species scale variant using each species' rank across the national cells gives **5-km rho +0.07563, P=0.26477** (10,000 50-km permutations). This alternative index also fails the 5-km inference gate. The standardized ranks are not abundances.

### Sampling caveat

Only **107/1,134** pairs within 5 km have >=2 images at both endpoints. Among those, the five-species rho is negative (−0.08057). A 1-km cell with only one observed colour is not necessarily a single-morph biological population. Cells and pairs may share samples; permutation is a diagnostic of spatial co-structure, not an independent-population experiment.

## What this means for the paper

The five-species test is of **predicted SDM suitability-profile turnover**, NOT observed guild turnover, visitation pressure, flower-choice behaviour, reproductive fitness, or flower constancy. Occurrence-referenced species suitability scores are normalised to compare relative prediction profiles but cannot be assumed proportional to the five species' true visitation frequencies.

**No evidence here to promote the hypothesis that the five-species bumblebee SDM composition explains the sharpest local white/pigmented boundaries.** Keep this analysis as a transparent exploratory/negative supplementary check. The earlier two-species 67-pair mean comparison was itself weak (q=0.0815, 33/67 directional matches) and does not become stronger because more species were added.

The November 2026 main-branch regional Figure 3 models include several abiotic covariates and smoothing terms; they are a **different estimand** and should not be misrepresented as results of this SDM-only test. This PR does not overwrite those models or redefine their coefficients.

**Discussion limitation (not an added covariate):** Every species' SDM is derived from environmental predictors, so any ecological correspondence may reflect a shared environmental/spatial niche rather than pollinator causation. No method here separates those effects.

**Mechanistic limit:** Positive frequency-dependent selection via bumblebee flower constancy remains untested. Verification would require known local morph frequency, individual bumblebee visit sequences and per-flower pollen-transfer/seed-set comparisons across experimentally varied white/pigmented frequencies.

## Reproduce the full-guild test

After \`python run_pipeline.py reproduce\` has produced the five-species score table and 1-km flower cells, install optional support \`pip install -e '.[guild]'\` and run:

\`\`\`bash
python scripts/run_bombus_five_sdm_turnover.py \
  --cells results/analysis_cells/analysis_cells_1km.csv \
  --sdm results/bombus_occurrence_reference_support/cell_effective_bombus_support.csv \
  --out results/bombus_five_sdm_turnover \
  --radii 5,10,25 --block-km 50 --nperm 10000
\`\`\`

The same five-species test was independently executed from the frozen 2026-11-11 GitHub Actions output in this analysis. The local result package supplied in the associated ChatGPT conversation contains the minimal, non-abiotic cell and score inputs, executable source, fixed results, diagnostics and SHA manifest; this sandbox path is an interactive-conversation artifact, not a repository-relative link.
