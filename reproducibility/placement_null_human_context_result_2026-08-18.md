# Locked result: v22 placement-null human-context reanalysis

**Status:** completed and validated.

**Specification:** `v22.0_placement_null_human_context`

**Pre-execution history:**

- specification commit: `cc98b187c04484262f34ba6c381daa5bb785fa30`;
- input-amendment commit: `5247cfd1c60a7600fc6a10e2238ff5689c2bf67b`;
- permanent pre-execution ref: `prereg/v22-placement-null-human-context-2026-08-18`.

**Validated execution:**

- workflow run: `32104531917`;
- tested branch head: `9c6920c98e56d4c6dd7c53970026a7605ff64f37`;
- artifact: `9312738211`;
- artifact name: `placement-null-human-context-2f6b46813a58d8ebc0a3dc35126cef2bc785bd05-32104531917`;
- artifact ZIP SHA-256: `4f726c2e346ec24e2e287919e4bcd017d69c50613d8ee2b3697300d5d053f362`;
- workflow conclusion: `success`.

The workflow verified preregistration ancestry, unit tests, all three frozen input artifacts, the WorldPop MD5, exact recovery of the 16 forward candidate identities, exact graph recovery, 5/10/25-km exposure reconstruction, retained v20/v21 results, output structure and output checksums. No gate depended on the direction or magnitude of a new v22 result.

## 1. Primary placement null

The primary statistic compares mean 5-km population-exposure rank among the 16 observed pigmented-among-white cells with the same statistic at cells selected by each of 10,000 fitted natural predictive maps.

| Quantity | Value |
|---|---:|
| Observed candidate cells | 16 |
| Observed mean exposure rank | 0.681748 |
| Natural-placement null mean | 0.591379 |
| Natural-placement null SD | 0.073841 |
| Central null interval, 2.5–97.5% | 0.442993–0.732006 |
| Observed minus null mean | **+0.090370** |
| Null percentile below observed | 89.23% |
| One-sided upper-tail Monte Carlo P | 0.107889 |
| Monte Carlo SE | 0.003102 |
| Four-radius maxT family P | 0.172983 |

All 10,000 natural maps contained at least one forward candidate, and all candidate exposure values were finite. Therefore no degenerate draw was excluded.

**Result statement.** The observed configurations occupy more population-exposed cells on average than the fitted natural process places them, but the observed value remains inside the central natural-placement interval. The direction is compatible with a human-placement contribution; the natural process produces enough spatial variation that this comparison does not separate the observed placement sharply from its natural expectation.

## 2. Predeclared 2/5/10/25-km profile

| Radius | Observed rank | Null mean | Observed − null | Upper-tail P | maxT P |
|---:|---:|---:|---:|---:|---:|
| 2 km | 0.667897 | 0.589964 | +0.077933 | 0.136286 | 0.212679 |
| 5 km | 0.681748 | 0.591379 | +0.090370 | 0.107889 | 0.172983 |
| 10 km | 0.685199 | 0.597836 | +0.087363 | 0.115688 | 0.187981 |
| 25 km | 0.642830 | 0.586354 | +0.056476 | 0.215778 | 0.322768 |

Predeclared profile classification: **non-monotone**.

The effect is positive at every radius and attenuates by 25 km, but the maximum occurs at 5 km rather than 2 km and the sequence 2 → 5 → 10 → 25 km is not monotone. It therefore does **not** satisfy the predeclared horticultural-introduction prediction of a short-radius maximum followed by monotone decay. It also does not show the flat-or-increasing profile predeclared for a simple regional-geography proxy. The defensible reading is a short-to-intermediate-range positive displacement with an unresolved scale shape, not a passed mechanistic scale test.

## 3. Reversed-detector negative control

The reversed detector identified 21 observed white-among-pigmented cells.

| Quantity at 5 km | Value |
|---|---:|
| Observed mean exposure rank | 0.416922 |
| Natural-placement null mean | 0.466854 |
| Natural-placement null SD | 0.064072 |
| Central null interval, 2.5–97.5% | 0.343775–0.594600 |
| Observed minus null mean | **−0.049931** |
| One-sided upper-tail Monte Carlo P | 0.781922 |
| Four-radius maxT family P | 0.885611 |

No reversed draw was degenerate. The reversed effect is not elevated at any radius: −0.036236, −0.049931, −0.024270 and −0.033270 at 2, 5, 10 and 25 km, respectively.

**Control reading.** The result is directionally consistent with the hypothesis: the positive placement displacement occurs for pigmented-among-white, not for white-among-pigmented. This is useful asymmetry evidence, but it is not a formal direct forward-minus-reverse test, and the two detectors have different opportunity counts and are not exact complements.

## 4. Relationship to retained v20 and v21 evidence

The workflow recovered the existing results without modification:

- v20 candidate-count calibration: P = 0.278972;
- v20 candidate-fraction calibration: P = 0.126087;
- v21 observed candidate-minus-local-white-neighbour contrast at 5 km: +0.067442;
- v21 one-sided P = 0.007999.

The three analyses now answer different questions:

1. **v20 count/fraction:** can the fitted natural process produce this many configurations? Yes; the event rule is calibrated, not an excess-anomaly detector.
2. **v21 local contrast:** are observed candidates more exposed than their own immediate white surroundings? The observed local contrast is positive.
3. **v22 placement null:** are observed candidates more exposed than the cells at which the fitted natural process places comparable configurations? The effect is positive but lies within a wide natural-placement distribution.

This combination narrows the interpretation. The human-context pattern is strongest as a **within-locality association**. There is a compatible but not sharply separated displacement relative to the fitted natural geography. The analysis therefore strengthens site prioritisation and the directional provenance hypothesis, while weakening any wording that treats population exposure as demonstrated anthropogenic origin.

## 5. Detector opportunity and reconstruction checks

| Direction | Observed supported focal opportunities | Observed candidates | Null mean candidates | Null SD candidates |
|---|---:|---:|---:|---:|
| Pigmented among white | 393 | 16 | 13.5908 | 3.4803 |
| White among pigmented | 313 | 21 | 18.6304 | 4.1624 |

The newly reconstructed population ranks reproduced the frozen values at 5, 10 and 25 km with maximum absolute difference `4.996004e-16`, below the preregistered `1e-12` gate. The WorldPop input MD5 was `59eb41f5984239526e036e62c1f0a9cc`.

## 6. Claim ceiling

The 16 forward cells remain field and provenance targets. The positive forward placement displacement does not establish horticultural origin, cultivar identity, establishment, pollen movement or gene flow. The non-monotone scale profile prevents claiming that a propagule-pressure mechanism passed its predeclared scale prediction.

## 7. Output checksum lock

| Output | SHA-256 |
|---|---|
| `RESULT_SUMMARY.md` | `195964ff518e6466a4778c46116a243dce67111d907551e0685e3a38c6b1cceb` |
| `placement_null_detector_opportunity.csv` | `b7bcb47246c0aa61417de7490bd5f2c2381d1d2f66c17719c3bccf1fcdf303a7` |
| `placement_null_draws.csv` | `45eb9126ff228c43f134f2467771a5fc97f7c4fcd1c74d03942598ce34c0aec3` |
| `placement_null_forward_candidates.csv` | `c04c5959a71183e3e1acb4cb12aca3b51d72df62d7c93dd8d8fd3961b4bdb6cd` |
| `placement_null_graph_registry.csv` | `7329e515f8dcf543ddc5cb8dadaf003d61ec028a2c6f0eb92ff1b7f75e1b0107` |
| `placement_null_neighbour_replay_summary.csv` | `5751326e3b73042f0c496440c5a9069429ef6c0870df9ae5ec62c538cccf212e` |
| `placement_null_population_exposure.csv` | `5ef31fa1af77862ec23e6ca52ba3fc7cd67d61ea0cba0085c22f152be43c06e8` |
| `placement_null_retained_v20_calibration.csv` | `e72c48ee098f63935628c55b8321baafff38194f1828c34d8722094ceb58f0b7` |
| `placement_null_reverse_candidates.csv` | `61dff4f9dcb80905ad4a20e3a43cbd1851e269c4475e7f02a37dc1353740f8e7` |
| `placement_null_scale_profile.csv` | `1604082cf837dbb354914a6b030dd47312d38b117370d8688642f764d6bd901e` |
| `placement_null_scale_profile.pdf` | `3472a09a6ca9be2994fc7296ec0aa0c07f74138314240aeaf2b91f1acae98f96` |
| `placement_null_scale_profile.png` | `4fc1dbcf02c64cc841424eb0bcc4d3c20e338688c33b9c0ed9bf1be828ad9289` |
| `placement_null_summary.csv` | `862cdb5828647c1f14c465d25aea305d6c0292838bb89cc7c55fd0df0a3cdbf0` |
| `placement_null_validation.json` | `27d5878a648fdd3a7a1d281728afc7d24f39698896788d888b492a0216db91ef` |
| `placement_null_worldpop_reconstruction_audit.csv` | `53486172f750a1458544ae1bb3f40e0a47917aee57ce586e80dba710963d6c83` |
