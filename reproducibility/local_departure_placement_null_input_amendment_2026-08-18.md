# Pre-execution input amendment for v22 placement null

**Status:** predeclared before any v22 placement, scale-profile, neighbour-replay or reversed-detector statistic is computed.

**Applies to:** `v22.0_placement_null_human_context`.

**Purpose:** close one input-completeness gap without changing any biological hypothesis, detector, candidate identity, analysis direction or reporting rule.

## Gap found during implementation audit

The frozen local-departure/human-context artifact contains population ranks and raw population sums for 5, 10, 25 and 50 km. The predeclared scale ladder is 2, 5, 10 and 25 km. Therefore the 2-km exposure cannot be recovered from the three frozen artifacts in the parent specification alone.

This mismatch was identified before running any statistic in Sections 4–7 of the parent specification. Removing 2 km or replacing it with 50 km would weaken the predeclared short-range biological prediction. The ladder is therefore retained and the exact upstream raster from which the existing exposure variables were built is added as a frozen input.

## Additional frozen input

| Input | Frozen identity |
|---|---|
| WorldPop Japan 2020 population-count raster | provider: WorldPop Global 2000–2020 1-km, Japan 2020, people per cell; canonical analysis filename: `population_count_Japan_crop.tif`; source URL: `https://data.worldpop.org/GIS/Population/Global_2000_2020_1km/2020/JPN/jpn_ppp_2020_1km_Aggregated.tif`; expected MD5: `59eb41f5984239526e036e62c1f0a9cc`; recorded in input snapshot `analysis-input-snapshot-v2` whose release asset SHA-256 is `19b771feebb3fed212d45a8a75a629911b6fa445392fa43cc7de6b878f1be21c` |

The workflow must stop if the downloaded raster does not match the expected MD5. No alternate raster, year, resolution, interpolation or settlement variable may be substituted.

## Frozen construction rule

Population exposure is computed for radii 2, 5, 10 and 25 km using the existing `multiscale_point_context()` implementation with:

1. the same 1,305 cell coordinates;
2. native raster-cell values;
3. a site-latitude Euclidean distance approximation;
4. inclusion of raster cells whose centres fall inside the radius;
5. `summary_function = "sum"`;
6. `log1p(pmax(population_sum, 0))`;
7. one outcome-independent `v19_rank01()` transform over all 1,305 cells.

## Mandatory reconstruction check

Before the 2-km rank is admitted, newly reconstructed 5-, 10- and 25-km ranks must reproduce the corresponding columns in the frozen Broad/current human-feature table. The workflow reports maximum absolute differences and stops if any exceeds `1e-12`.

This check makes the new 2-km value an extension of the existing exposure ladder rather than a newly searched human variable.

## What remains unchanged

- 5 km remains the primary radius.
- The scale ladder remains 2 → 5 → 10 → 25 km.
- No new human mechanism or proxy is introduced.
- The 16 forward candidate identities remain fixed.
- The natural model, 10,000 maps, neighbour graph, detector geometry, reversed detector and one-sided direction remain fixed.
- All parent-specification reporting ceilings remain binding.
