# Human-context flowering phenology diagnostic

## Question

Does the pigmented state appear earlier in the season than nearby white flowers, and is any local earliness of pigmented flowers concentrated in the isolated, human-exposed contexts already identified by the continuous-isolation analysis?

This is an exploratory provenance diagnostic for the human-impact section. It does **not** assume that horticultural material is necessarily early flowering and it does **not** infer cultivated origin from phenology alone.

## Measurement boundary

The available date is the date on which a flowering plant was photographed in YAMAP. Therefore the response is an **observation-date phenology proxy**. It is not flowering onset, peak flowering date, plant age, or duration of flowering.

## Frozen directional hypotheses

1. **H1 — local colour-state timing:** pigmented flowers are observed earlier than white flowers under local, same-year matching. Direction: `DOY_pigmented - DOY_white < 0`.
2. **H2 — human-context timing:** among locally matched pigmented cells, the number of days by which the pigmented member is earlier (`DOY_white - DOY_pigmented`) increases with the pre-existing **5-km WorldPop rank**.
3. **H3 — isolation-context timing:** the same earliness measure increases with the pre-existing **relative same-colour isolation** (`relative_isolation_nn`).

H2 and H3 use variables created before flowering dates are examined, so phenology does not enter candidate selection or the definition of human/isolation context.

## Analysis unit and matching

1. Collapse images to `exact_site_id × year × colour` median DOY.
2. Collapse exact sites to `1-km cell × year × colour` median DOY, weighting sites rather than images.
3. Within each year, construct **mutual nearest-neighbour white–pigmented cell pairs** in projected kilometre coordinates.
4. The **primary radius is 5 km**. This compares observations within a local phenological setting while retaining enough contrasts for inference.
5. Repeat at 10 and 20 km as sensitivity analyses.
6. If the same geometric pair appears in multiple years, collapse its contrasts across years before the sign-flip test. Repeated years therefore do not become independent replicates.

The national white-vs-pigmented date difference is reported only descriptively because pigmentation state is geographically structured and a national raw difference can reflect latitude, elevation, climate, or sampling geography.

## Primary tests

### H1

For each unique matched geometric pair:

`delta_DOY = median_DOY_pigmented - median_DOY_white`

The primary statistic is the mean paired delta after repeated-year collapse. Inference uses a paired sign-flip randomization test with the pre-specified one-sided alternative `delta_DOY < 0`. A two-sided p-value is retained as a guardrail.

### H2–H3

For each pigmented cell represented in the 5-km matched set:

`early_days = median_DOY_white - median_DOY_pigmented`

Positive values mean the pigmented member was observed earlier. Repeated white matches for the same pigmented cell are collapsed before testing.

The two primary context tests are one-sided Spearman associations:

- `early_days ~ population_5km_rank` (H2; expected positive)
- `early_days ~ relative_isolation_nn` (H3; expected positive)

Permutation is restricted within the five pre-existing spatial folds. Holm adjustment is reported across H2 and H3. Leave-one-spatial-fold-out correlations check whether any direction is driven by one geographic fold.

Raw same-colour distance, focal population, and 10/25/50-km population ranks are sensitivity outputs, not additional primary hypotheses.

## Interpretation

- **H1 supported alone:** colour state is associated with local observation-date phenology. This does not specifically support horticultural provenance.
- **H1 plus positive H2/H3:** phenological earliness is concentrated where pigmented geography is more human-associated and/or isolated. This is compatible with an anthropogenic-provenance hypothesis and provides an independent follow-up signal.
- **H1 absent but H2/H3 present:** no general colour-state phenology shift, but a restricted human-context subset may differ; interpret as exploratory targeting evidence only.
- **No directional support:** the early-flowering horticultural-provenance route is not supported by these data. The existing isolation–human result remains separable from phenology.

None of these outcomes establish cultivar origin. Provenance would still require field history, herbarium/planting records, and/or genetic evidence.

## Reproducible outputs

`R/flowering_phenology.R` contains the transformations and tests. `scripts/run_human_flowering_phenology.R` writes the cell-year tables, 5/10/20-km matched pairs, primary human-context correlations, leave-one-fold-out checks, and a compact `validation.txt` summary.
