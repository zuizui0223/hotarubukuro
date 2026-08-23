# Human-context flowering phenology diagnostic — 2026-08-23

## Status

Exploratory supplementary diagnostic for PR #67. The analysis asks whether the human-associated pigmented pattern has an independent observation-date phenology signature. YAMAP photo day-of-year (DOY) is an **observation-date phenology proxy**, not flowering onset, flowering peak, or direct evidence of horticultural provenance.

Authoritative execution: `submission-analysis-contract` run `32615504995`, `full-reproduction` job `97135452369`, artifact `human-flowering-phenology` (`9488718548`). The one-command public reconstruction, helper tests, flowering diagnostic, and artifact upload all completed successfully.

## Frozen design

The hypotheses and elevation guardrails were specified in PR #67 before the first result was inspected.

1. H1: within the same year, mutually nearest pigmented and white 1-km cells within 5 km should have `delta_DOY = pigmented - white < 0` if pigmented flowers are generally observed earlier.
2. H2: `early_days = -delta_DOY` should increase with the pre-existing 5-km WorldPop rank if local earliness is concentrated in human-associated contexts.
3. H3: `early_days` should increase with pre-existing relative same-colour isolation if the same local earliness characterises the isolated-pigmented pattern.
4. Exact sites are collapsed before cell-year-colour aggregation; repeated years of the same geometric pair are collapsed before inference; human/isolation features are joined only after phenotype-only pairs are fixed.
5. 10- and 20-km matching and non-primary population/isolation scales are sensitivity outputs.

The authoritative phenotype input contained 1,921 rows and generated 1,643 cell-year-colour records. The primary human-context join rate was 1.000.

## H1: no general earlier observation of pigmented flowers

| matching radius | unique geometric pairs | mean pigmented−white DOY | median | proportion pigmented earlier | one-sided sign-flip p |
|---:|---:|---:|---:|---:|---:|
| 5 km (primary) | 115 | -0.461 d | 0 d | 0.339 | 0.3670 |
| 10 km | 154 | -1.071 d | 0 d | 0.383 | 0.2070 |
| 20 km | 201 | -0.910 d | 0 d | 0.418 | 0.2349 |

H1 is not supported. The primary difference is less than half a day on average, has median zero, and is not directionally significant. Increasing the matching radius does not change that conclusion.

The unpaired national descriptive medians actually place pigmented observations later than white observations in every sampled year (2023: 204 vs 191 DOY; 2024: 205.25 vs 182; 2025: 181 vs 179). These national values are descriptive only and are expected to mix geography, elevation, access and population structure; they illustrate why the local same-year comparison is the inferential test.

## H2/H3: weak human-context timing signal, no isolation timing signal

The primary 5-km matched data collapsed to 111 unique pigmented cells for the human/isolation correlation tests.

| hypothesis / feature | Spearman rho with `early_days` | within-spatial-fold one-sided permutation p | Holm p across H2/H3 |
|---|---:|---:|---:|
| H3 relative isolation (`relative_isolation_nn`) | -0.0016 | 0.4556 | 0.4556 |
| H2 5-km WorldPop rank | +0.1712 | 0.0235 | 0.0470 |

Thus H3 is null. H2 is positive and passes the pre-specified two-test Holm correction, but it is a small association and it occurs in the absence of the general H1 early-flowering effect.

Population-scale sensitivities are directionally positive but weaker away from the focal/local scales: local population rho = 0.1675 (p = 0.0278), 10 km rho = 0.1084 (p = 0.0888), 25 km rho = 0.0908 (p = 0.1396), and 50 km rho = 0.1431 (p = 0.0635). Raw same-colour isolation is also null (rho = -0.0316, p = 0.5514).

Leave-one-spatial-fold-out estimates for the 5-km population association remain positive in all five omissions (rho = 0.100–0.306), whereas relative-isolation estimates remain close to zero and change sign. This supports describing H2 as spatially distributed rather than driven by a single fold, but does not turn it into provenance evidence.

## Pre-specified elevation guardrails

YAMAP sampling is enriched along mountain routes, so elevation matching was frozen before result inspection.

| guardrail | unique pairs | mean pigmented−white DOY | one-sided p |
|---|---:|---:|---:|
| same 1-km cell | 62 | +0.548 d | 0.7053 |
| absolute elevation difference <=100 m | 92 | -1.065 d | 0.2102 |
| absolute elevation difference <=250 m | 107 | -1.234 d | 0.1660 |

None supports general earlier observation of pigmented flowers. Moreover, signed elevation difference is associated with the matched DOY difference (Spearman rho = +0.2828, p = 0.00219, n = 115): when the pigmented member of a pair is higher than its white match, it tends to be observed later relative to that match. Elevation therefore remains an important source of timing structure even after local same-year matching.

## Interpretation

The proposed simple signature — **“pigmented flowers are generally earlier because some are horticultural”** — is not supported by these data. It should not be added as a main explanation for the human-associated isolated-pigmented pattern.

There is a narrower result: among locally matched pigmented cells, relative earliness increases weakly with 5-km human population exposure, but not with pigmented isolation itself. This is compatible with a local anthropogenic phenology association and is useful as a follow-up clue, yet it is neither necessary nor sufficient evidence for horticultural origin. The strong elevation–date relationship and the observational nature of YAMAP timing require that this result remain supplementary.

A direct horticultural-provenance test would require independent information such as cultivar/escape status, garden proximity, planting records, genotype assignment, or a validated comparison of cultivated versus wild *Campanula punctata* flowering phenology.

## Rejected fast shortcut

A PR-only shortcut attempted to reconstruct the final phenotype directly from `Data_S1.csv` and map observations to the frozen isolation cells. Its exact-match gate rejected the shortcut before any H1–H3 inference: 59 of 1,305 cells had count mismatches. Frozen cell counts summed to 1,922 observations, whereas the shortcut reconstructed 1,963 (+41), showing that the shortcut did not reproduce the downstream analysis-population filtering in addition to small adjacent-cell assignment differences.

The gate was not relaxed. The shortcut is not an accepted analysis path; the full public reconstruction above is authoritative.
