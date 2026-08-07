# Bombus pollinator-opportunity proxy: an eDNA-inspired limitation design

## The measurement problem

The biological hypothesis concerns attraction and pollinator-mediated selection, but the broad-scale data contain *Bombus* occurrences and SDM predictions rather than visits to *Campanula punctata*. The correct task is therefore **not** to manufacture a visitation-pressure number from habitat suitability. It is to define the strongest latent pollinator-availability contrast that the occurrence data can defend.

The active biological prediction is threshold-like:

> If pigmentation has an attraction benefit when effective bumblebees are available, then that benefit can relax where all focal *Bombus* taxa are poorly available. White flowers should therefore be relatively more common under local bumblebee limitation. A pigment-production cost could strengthen this transition but is not measured or required.

## Why the eDNA analogy is useful

The analogy is conceptual. eDNA studies often separate a latent ecological state such as occupancy from imperfect detection. They do not require DNA concentration to equal organism abundance. Quantitative abundance inference requires additional calibration because shedding, transport, degradation, sampling and amplification alter the concentration–abundance relationship.

The corresponding hierarchy here is:

```text
eDNA:   latent occurrence -> molecular detection -> calibrated abundance only with extra information
Bombus: latent availability -> SDM support       -> visitation/selection only with extra information
```

The nationwide study can defend the middle step—relative evidence for local *Bombus* availability—not the final visitation or selection step.

Relevant eDNA examples motivating this distinction include Schmidt et al. (2013; doi:10.1111/2041-210X.12052), Willoughby et al. (2016; doi:10.1111/1755-0998.12531), Dorazio & Erickson (2018; doi:10.1111/1755-0998.12735), and Rourke et al. (2022; doi:10.1002/edn3.185).

## Why `sum(MaxEnt suitability)` is not Bombus pressure

A raw sum would assume without calibration that:

1. suitability is proportional to abundance;
2. the proportionality is the same among species;
3. local abundance is proportional to visits to *C. punctata*;
4. visit rate translates monotonically to selection; and
5. SDM output magnitudes are quantitatively comparable among species.

Those assumptions are too strong. Presence-only SDMs are more naturally interpreted as relative occurrence intensity or relative habitat support than absolute abundance (Renner & Warton, 2013; Guillera-Arroita, 2015). Opportunistic records also mix ecological intensity with observer sampling bias; multi-species point-process approaches can partially separate a shared sampling process when the necessary information is available (Fithian et al., 2015; doi:10.1111/2041-210X.12242).

## Preferred active estimand: an all-species-low limitation gate

Each focal species is placed on its **own** within-species predicted-support rank. Cross-species magnitudes are never summed.

For flower cell `i`, define:

```text
best_Bombus_support_i = max_s(rank_is)
```

where `rank_is` is the within-species rank for focal species `s`.

The active manuscript-facing gate is:

```text
Bombus-limited:   best_Bombus_support <= 0.33
Bombus-available: best_Bombus_support >= 0.50
```

Thus a limited cell has **all five focal taxa in their lower third of predicted support**, while an available cell has **at least one focal taxon at or above its own median support**.

This construction has two advantages over a scalar pressure index.

First, the low state has a clear ecological interpretation without assuming cross-species calibration: every focal taxon is locally poorly supported on its own scale. Second, the high state only requires one plausible effective pollinator to be moderately available; it does not require all species to increase together or have equal contribution.

The 0.33 gate was adopted as the active gate after exploratory design development. The complete 0.10/0.20/0.25/0.33 grid and its multiplicity correction must therefore remain visible.

## Local design

The gate is tested only among response-blind local matches:

1. endpoints within 25 km;
2. same held-out flower-model fold;
3. five-species common SDM support;
4. environmental RMS distance <=0.75 on the four broad/within-50-km axes; and
5. one-to-one pair use.

Pairs are oriented `limited -> available` from *Bombus* predictions before flower colour is read.

Primary response:

```text
pigmented_share_available - pigmented_share_limited
```

Secondary response:

```text
pigmented_only_intensity_available - pigmented_only_intensity_limited
```

The biological expectation is stronger for pigmentation state than for conditional intensity because an occurrence-based availability gate more naturally represents whether an attraction benefit can operate than the magnitude of visitation-driven selection.

## Why environment and space are not fitted twice

The local contrast already restricts geography and matches measured environment. A second local regression containing the same environment and another spatial field is not the active estimand. Such a model would not establish a causal pollinator effect and could remove environmentally mediated variation that defines the SDM availability layer.

The national 1,000 flower environment-plus-SPDE maps are instead used as a **separate predictive reference**. The fixed matched pairs and Bombus orientation are replayed on those maps to ask whether the observed contrast is larger than broad natural geography commonly generates.

This reduces broad confounding but does not fully separate pollinators from environment. The *Bombus* surfaces are environmentally predicted and can encode unmeasured habitat, distribution history or sampling structure.

## Why strict all-species dominance is now secondary

An earlier exploratory design oriented pairs only when all five focal species had greater predicted support at the same endpoint. That comparison avoids arbitrary species weights, but it tests a different biological model: a monotonic increase in overall *Bombus* opportunity. In the 1,909 data the observed pigmentation direction was positive but did not exceed the natural-map reference.

The limitation hypothesis is more closely aligned with the natural history of *C. punctata*: the key transition may be loss of access to effective bumblebee pollination rather than a linear dose response once bumblebees are already available.

## Propagating SDM uncertainty in a future rebuild

The current gate uses fixed archived SDM surfaces. A stronger future version should rebuild spatially cross-fitted SDM ensembles and estimate the **probability that a site is limited or available**.

For SDM realization `b`, calculate each species rank and the gate state. For pair A/B define, for example:

```text
P(A limited and B available)
```

as the proportion of valid SDM realizations supporting that orientation. The main analysis could retain only pairs with high orientation probability or carry orientation uncertainty into a hierarchical matched-pair model.

This is preferable to averaging surfaces before classifying because the biologically relevant uncertainty is whether the site actually belongs on the limited or available side of the gate.

## Foraging accessibility can improve the latent availability state

A bumblebee can forage beyond a 1-km raster cell. A future source build can kernel-average each species' predicted support around a flower site before ranking:

```text
A_is = integral K_s(distance(i,x)) * support_s(x) dx
```

If species-specific foraging distances are poorly known, a common kernel with predeclared sensitivity radii is safer than assigning precise unsupported movement parameters.

## Terminology

Preferred terms:

- `predicted Bombus availability`;
- `Bombus-limited` / `Bombus-available`;
- `potential Bombus encounter opportunity`;
- `pollinator-availability proxy`.

Avoid without independent calibration:

- `Bombus abundance`;
- `visitation pressure`;
- `selection pressure`;
- `pollination service`.

## Ecological claim enabled by the active design

A positive environmentally matched contrast supports the statement:

> Pigmentation tends to be greater where at least one focal bumblebee taxon is moderately supported than where all focal taxa have low predicted support. This is consistent with relaxation of a bumblebee-associated attraction benefit under low predicted pollinator availability.

It does **not** establish that bumblebees visited the pigmented flowers more often or caused their fitness advantage. That mechanism requires direct species-resolved visitation and reproductive-success data.
