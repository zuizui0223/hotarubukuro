# Human-context public-raster design

No additional flower observations are required. All axes are defined without
using flower colour, residual rank, or candidate identity.

| Axis | Ecological construct | Operational variable | Inferential role |
|---|---|---|---|
| H | horticultural propagule opportunity | WorldPop 2020 population count | exposure proxy |
| R | receiving human–forest interface | shared boundary between MLIT 2021 forest and human-managed 100 m land-use cells, aggregated to the third-order mesh | prespecified effect modifier |
| N | natural seasonal progression | CHELSA GDD5 climatology | phenology control |
| A | observation/transport accessibility | negative distance to the nearest MLIT major-road cell | access sensitivity only |

H and N are available but imperfect construct proxies. R and A were built from
the authentication-free [MLIT National Land Numerical Information 2021 100 m
land-use mesh](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L03-b-2021.html).
The MLIT [land-use code list](https://nlftp.mlit.go.jp/ksj/gml/codelist/LandUseCd-09.html)
fixes the categories before the response is inspected. R is stored as nominal
kilometres of forest--human boundary per approximately 1 km primary mesh. A is
not evidence for horticulture; it asks whether easier roadside observation or
transport explains the H-by-R result.

## Current acquisition and coverage

- All 1,923 analysis observations have H, R, and N.
- A is finite for 1,922 observations.
- Sixty-three primary meshes cover all observations.
- Seventy-nine observations fall in primary-mesh boundary cells. They remain in
  the primary model and are removed only in a boundary sensitivity analysis.
- The downloaded MLIT archives live outside the repository cache. Compact
  processed tables, rasters, and manifests are under
  `results/public_rasters/mlit_human_forest_edge_2021/`.

## Locked use and interpretation

1. Test the centred `H * R` interaction on raw standardized CIELAB a*, with
   environment, Bombus W, and broad spatial smooth terms retained.
2. Evaluate West and East separately, then use one pooled East-minus-West
   interaction as the formal regional contrast.
3. Require spatial-fold predictive gain, not only an in-sample coefficient.
4. Repeat after observation date adjustment, primary-mesh boundary exclusion,
   and road-access adjustment.
5. Test median and upper-tail colour separately. Do not construct a weighted
   horticulture score after seeing results.
6. Model early observation date as a separate prediction; failure is retained
   as falsification, not removed from the story.
7. Use cross-fitted residuals only to rank specimens for follow-up. The
   horticultural test itself uses raw colour and response-independent H and R.
8. Call convergence a *human-associated spatial signature consistent with
   horticultural influence*. It cannot establish cultivar provenance, planting,
   introgression, or garden escape of an individual.

## Preferred future upgrades, not completion gates

- Replace H with 2020 e-Stat detached-household density.
- Replace R with a multiscale JAXA HRLULC forest--residential edge.
- Replace N with observation-year GDD and realised snow-off/green-up.
- Replace A with GSI road geometry and explicit distance-to-settlement.

Those upgrades sharpen construct validity; they do not justify changing the
definitions after inspecting the current response.
