# Bombus redesign decision — 7 November 2026

## Decision: change the ecological question, not the P-value

For a biologically meaningful new Bombus analysis in *Campanula punctata*, **stop using the two widespread species' maximum habitat-suitability score to claim an effect on pigmented versus white flowers**. Positive frequency-dependent selection through flower constancy would reinforce **whichever colour morph is locally common**, not necessarily favour pigmentation.

A sharper biogeographic question is:

> Do local changes in the *predicted assemblage of documented bumblebee visitors* co-occur with changes in photographed pigmentation state?

The **only explanatory information is Bombus SDM**. No climate, elevation, precipitation, soil, environmental distance or environmental/SPDE predictor enters this Bombus test. Geography defines the nearest-neighbour graph and optional label-permutation blocks; environmental confounding remains a Discussion limitation.

## Why the species set must change for the new estimand

Retain the original `B. ardens` + `B. diversus` maximum only as the previously frozen **two widespread focal species** analysis. This index cannot represent all potential bumblebee visitors: Nagano et al. (2014, DOI 10.1002/ece3.1191) found predominant `B. beaticola` visitation to the studied *C. punctata* populations at 1,900–2,300 m, unlike the lowland sites dominated by `B. ardens`/`B. diversus`.

For a *new* visitor-guild composition hypothesis, predeclare five SDMs:
`ardens, diversus, beaticola, consobrinus, honshuensis`.
Use the five species as a **vector**, rather than maximising over only the widespread two or summing non-comparable niche suitability values. The field visitor inventory also included `B. ussurensis`; acknowledge the omitted species as an incomplete-guild limitation.

**Current hard blocker:** the recovered original `cell_effective_bombus_support.csv` has 1,305/1,305 finite rows for both widespread species but **0/1,305 finite** for each of the other three. All-five scores are absent, not biological zero. The full-five hypothesis has **not** been executed and must not be reported as supported. The existing five-species source-builder must regenerate genuinely modelled support before the test can be run.

## Proposed main estimand, frozen before five-species reconstruction

Use all original 1,305 observed 1-km flower cells (not a selectively chosen set of 67 one-photo 'pure' transitions).

For each undirected pair of distinct cells within **5 km** (primary; **10 and 25 km** sensitivity):

- Response: `dColour = abs(pigmented_share_i - pigmented_share_j)`, where the share is directly observed `n_pigmented/n_observations`.
- Sole SDM-derived exposure: `dBee = Hellinger(normalise(A_i), normalise(A_j))` for five species.
- Primary statistic: Spearman association of `dColour` with `dBee`. The hypothesis has a positive direction: stronger turnover in predicted visitors coincides with stronger flower-colour change. Report raw effect sizes.
- A 10,000-replicate graph-respecting **50-km block label randomisation** permutes the observed numerator/denominator pair together within fixed regional blocks. Keep coordinates, graph edges and SDMs fixed. Blocks are a null-design device, not environmental predictors. Spatial history and fine-scale environment are not fully controlled, so a positive result remains biogeographic co-structure, not selection.
- Mandatory sample-support audit: 1,134 graph edges within 5 km, **only 107 with at least two images at BOTH endpoints** and 14 with at least three. Check direction when restricting to the 107. Photographed `pure` 1-km cells are not proof of locally fixed colour morphs.

**Promotion rule:** Five-species results may be elevated to the manuscript Main only if the actual prediction surfaces can be reconstructed, the predeclared strict-local correlation is positive and supported, and its direction is not contradicted by replication/scale guardrails. Otherwise the Bombus result remains a narrow, exploratory Supplement/Discussion component; do not tune the window or variable set to obtain significance.

## What was actually executed with the available two species — exploratory screen

Source: archived public GitHub Actions 1,305-cell colour table and two-species `cell_effective_bombus_support.csv`. No environmental predictors were read.

**Directional SDM test (already frozen)**: 67 pure 5-km photo-colour pairs, mean pigmented-minus-white two-species support +0.0359, median -0.00277, 33/67 positive, across-scale BH q=0.08148. It does not support a consistent directional mechanism.

**Same-colour clustering versus availability:** among 1,134 cell-pairs within 5 km, the fraction with matching observed colour labels was 0.8803 for the lowest quartile of pairwise focal-SDM support and 0.7289 for the highest. Under 50-km blocks crossed with SDM quartiles, the observed high-minus-low difference did not exceed the null (exploratory one-sided P ~0.68). This does not demonstrate flower constancy or strong SDM-dependent reinforcement.

**Two-species guild-turnover screen:** 11 of 1,305 cells had undefined relative profiles because both focal scores were zero; 1,101 of 1,134 local edges remained. At 5 km, correlation between absolute photographed pigment-share change and two-species Hellinger turnover was **rho=-0.01813**. Across 10,000 within-50-km-block colour-label randomisations, the one-sided upper P was **0.8770**. At 10 and 25 km, exploratory rho=+0.04586/+0.06218; exploratory upper P=0.0733/0.1210 from 3,000 randomisations. These are **not** five-species findings and should not be presented as the final design's results.

## Inference ceiling and manuscript placement

Even a positive five-species SDM co-transition would establish **correspondence of flower-colour boundaries with predicted bumblebee-assemblage boundaries**, not floral colour preference, flower constancy, positive frequency-dependent *fitness*, or causality. All SDMs inherit environmental geography; say so plainly in the Discussion rather than adding environmental covariates.

Bumblebee flower constancy on rewarded artificial flowers has been demonstrated in other systems (Smithson & Macnair 1996; 1997), but is **not measured in these photographs**. A direct mechanism test requires morph-frequency manipulations, individual bee visit sequences and per-flower effective pollen transfer/fitness.

The manuscript Main can remain anchored by the independently supported Broad phenotype/environment/space analysis. The existing two-species Bombus test does not have to be strengthened post hoc before submission.

**Implementation:** a standalone executable five-species-first prototype has been prepared and validated locally against the actual two-species archival data; it intentionally fails if any of the three missing species is coded as absent or supplied incompletely. It is not connected to the canonical manuscript pipeline pending full-five source recovery.

Sources: [Nagano et al. 2014](https://doi.org/10.1002/ece3.1191); [Smithson & Macnair 1996](https://doi.org/10.1046/j.1420-9101.1996.9050571.x); [Smithson & Macnair 1997](https://doi.org/10.1111/j.1095-8312.1997.tb01503.x).
