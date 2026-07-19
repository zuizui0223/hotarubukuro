# Reproducibility and methods review

Review date: 2026-07-19; workflow documentation updated 2026-07-20

## Scope and decision rule

This review used the repository's current code, `Data_S1.csv`, retained SDM
rasters, and the identified photograph workbook. It did not use a thesis draft
or earlier narrative descriptions. Safe, testable corrections were implemented.
Models whose scientific specification or source inputs cannot be reconstructed
were disabled rather than reverse-engineered from reported results.

## Major findings and implemented decisions

| Area | Finding | Implemented decision |
|---|---|---|
| Photograph identity | The source workbook has 1,965 rows and 1,964 unique embedded PNGs. One image is mapped to two rows and two different coordinates. | Join worksheet cell → OOXML media → SHA-256 → stable `observation_id`. Retain both rows in the public audit table, but hard-exclude both from analysis until the conflict is resolved. Never use row position to join a separate table. |
| Coordinates | The workbook contains a coordinate for every row, but no GPX/photo timestamp manifest was found in the searched Mac, cloud, repository, or mounted external drive locations. | Carry the workbook coordinates with `coordinate_recomputed=false`, an assumed EPSG:4326 CRS, exact-coordinate site IDs, and 30-second grid IDs. Do not claim that coordinates were re-georeferenced. |
| Legacy colour | The prior `Data_S1.csv` had 1,926 rows and values produced by an older method; 39 workbook rows had no legacy RGB. A one-cluster k-means centre is only an arithmetic mean. | Preserve the byte-exact legacy CSV; rerun all 1,965 images; retain legacy, mean, median, and candidate modal estimates in separate columns. |
| White balance | Every embedded image is a post-processed RGBA PNG. The files contain an sRGB declaration but no usable camera, exposure, WB-gain, ICC, or neutral-reference information. | Do not apply retrospective WB. Record `white_balance_applied=false` and describe the outcome as uncalibrated display-referred apparent colour. |
| Petal mask | The compatible HSV mask supports blue/cyan-looking pixels but excludes saturated warm/yellow pixels. Broadening it blindly would also admit foliage. | Preserve the compatible mask for the primary median, quantify warm/cool exclusions, compute alpha-foreground and HSV-mask sensitivity peaks, and make disagreement/cast candidates mandatory QC. |
| Mean and outliers | Means are affected by shadows, clipping, black spots, semi-transparent edges, and mixed organs. Independent channel trimming can construct a colour that was not observed. | Add deterministic joint CIELAB density peaks. A sensitivity branch heuristically excludes rendered near-white pixels (all channels ≥250) and pixels with L* <25 before finding the peak. These are not proven noise or clipping, and genuine white petals may be removed. Keep candidate peaks experimental. |
| Automated QC selection | Warm/cool, multimodality, and mask-coverage flags can depend on the colour response. Excluding every flagged photograph before manual review can create selection bias. | Preparation retains both `ok` and `manual_review_required` by default. Treat this as unapproved input, then use completed visual review or report inclusive and explicit `ok`-only sensitivity analyses. |
| Duplicate photo/coordinate conflict | Colour QC cannot decide which of two incompatible coordinates belongs to one exact photograph. Including both would pseudoreplicate the same image and attach it to two environments. | Keep both source records for traceability, but always remove both before PCA, raster extraction, or site aggregation unless external evidence resolves the identity. |
| RGB/Lab | RGB must be decoded as sRGB, not treated as linear intensity; OpenCV-style HSV hue is 0–179. In the reviewed analysis cohort, R's prior `grDevices::convertColor` values differed by up to ΔE76 0.376 from the extraction conversion. | Use one explicit IEC sRGB/D65 conversion in Python and R, with a shared cross-language fixture and a recorded old/new implementation comparison. Test RGB/Hue order, ranges, and missing rows. |
| CFA/Pigment | A latent variable made from photograph-derived L*, a*, and b* is not an independently measured pigment quantity. CFA would not turn lighting-sensitive rendered colour into pigment. | Remove CFA from the reproducible pipeline. Retain a sign-oriented `apparent colour PC1` only as a documented descriptive summary, not as pigment or a canonical model outcome. |
| Repeated PCA | Re-estimating PCA at several stages changes the construct and risks double use. A reference PCA fitted on all records would leak outcome information if it were used in cross-validation. | The lightweight single-method preparation fits PCA once for that explicitly selected method. The canonical reanalysis fits one primary-photograph reference PCA only for descriptive cross-method projections; it does not model or cross-validate PC1. |
| Photograph/site grain | Multiple photographs can share a coordinate; silent coordinate deduplication or aggregation confuses photographs with sites. | Preserve photograph grain by default. Site analysis requires an explicit site ID and reports `n_photos`. |
| Raster extraction | Row-order assignment can silently shift environmental values after missing coordinates or filtering. | Extract and rejoin by unique record ID; test shuffled and missing-coordinate cases on miniature rasters. |
| `Bee_Richness` | The five rasters contain continuous 0–1 suitability, not binary occurrence or observed richness. Summing them does not give species richness. | Rename the value `Bombus_suitability_sum`, retain coverage count, and leave incomplete five-species rows as NA. |
| *Bombus* predictor interpretation | A suitability surface generated from climate and habitat can re-express the same environment used in a flower-colour model. It is not automatically an independent pollinator effect. | Treat it as a pollinator-associated suitability index only. No causal pollinator effect is claimed; circularity/collinearity must be tested in a future joint design. |
| SPDE/INLA | The old model specification cannot be defended without a metric projection, mesh sensitivity, priors, full standardization, blocked validation, and joint uncertainty. | Do not fit it in the default pipeline. `--run-heavy` stops with the missing requirements. |
| Variance partition | `var(fixed) + var(spatial)` is wrong when fixed and spatial predictions covary. | Add and test the `2 cov(fixed, spatial)` term per posterior draw. Negative covariance contributions remain possible and are not independent causal shares. |
| Residual qGAM | Fitting qGAM to estimated SPDE residuals treats a noisy first-stage quantity as observed and ignores joint uncertainty. | Do not run the two-stage model. A future analysis needs a joint model or explicit uncertainty propagation and spatially blocked validation. |

## Source workbook identification

The selected source is `Supplementary_Table_S1.xlsx`:

- size: 109,765,581 bytes;
- SHA-256: `89d49e63559134e98fc1ee1b90ca8c9370a6b6d2d29cd41768c437b79ac3f684`;
- sheet: `Sheet1`;
- 1,965 data rows and 8 columns: `url`, `date`, `latitude`, `longitude`,
  `petal`, `R`, `G`, `B`;
- 1,965 cell-to-image mappings and 1,964 unique media objects;
- 1,926 complete legacy RGB rows and 39 rows with all three legacy channels
  missing;
- 1,964 YAMAP activity references and one `現地調査` source reference;
- 1,927 unique exact coordinate pairs, 38 pairs repeated twice.

The mounted external-drive copy of this workbook has the same byte hash. A
separate 1,965-row `rgb2.xlsx` contains identical embedded image bytes, dates,
coordinates, and legacy RGB, but 403 older rows use activity titles rather than
normalized URLs. A OneDrive `rgb2.xlsx` is only a 917-row partial version. The
selected workbook is therefore the complete, standardized, duplicated-backed
candidate—not a filename guess.

Raw activity URLs remain in the ignored local extraction workbook for source
verification. They are excluded from the committed `Data_S1.csv`: publishing
account-linked activity pages together with exact dates and coordinates would
add linkage risk without being required by the analysis data contract.

The duplicate image occurs at workbook rows 515 and 569. Its SHA-256 is
`fafa2012e772ac85e075a44183e5753f18f0a619c73cb00b4347f874ba23f166`;
the two coordinate pairs are different. There is no evidence to choose one row
automatically. Both rows remain in `Data_S1.csv` for provenance, but both are
hard-excluded from downstream analysis until external evidence resolves the
conflict. `--accepted-qc-status` cannot override this identity safeguard.

## Colour extraction v2.2.2

The committed primary `R/G/B` remains the v2.1-compatible channel median. The
coordinate-wise median is robust per channel but is not a joint multivariate
estimator and need not equal any observed pixel colour. This is a new
extraction for all photographs, not the old CSV value. It is retained
as primary because the candidate mode can switch between biological/technical
subregions in multimodal images and has no physical ground-truth validation.

The additive candidates are:

1. `hsv_peak_*`: a fixed-bin, smoothed joint CIELAB density peak inside the HSV
   petal mask;
2. `hsv_exposure_filtered_peak_*`: the same estimator after heuristically
   excluding rendered near-white pixels (R, G, and B all ≥250) and L* <25
   dark pixels, if at least 50 pixels and 25% of the mask remain. This is an
   experimental sensitivity ablation, not evidence that excluded pixels are
   noise or clipped;
3. `alpha_peak_*`: the same estimator over the full visible alpha foreground,
   used to expose mask/organ disagreement rather than as petal ground truth.

Together with the primary channel median, arithmetic mask mean, and preserved
legacy RGB, these form the six named methods used by the reviewed workflow:
`primary`, `mean`, `legacy`, `hsv_peak`,
`hsv_exposure_filtered_peak`, and `alpha_peak`. The `legacy` method is missing
for the 39 photographs that had no old RGB, so fair method comparisons use
explicit common-ID cohorts rather than silently changing rows.

The representative RGB/Lab pair is the observed pixel nearest the selected
peak-region Lab centroid, so the reported RGB and Lab identify one internally
consistent colour. No R/G/B channel is trimmed independently. Exact thresholds
are in `config/color_extraction_v2.json`.

### Full-run outcome

- processed: 1,965;
- primary RGB complete and within 0–255: 1,965;
- automated `ok`: 1,180;
- `manual_review_required`: 785;
- QC sets rendered: 834 masks, 834 overlays, and 834 five-way panels;
- exposure-filtered peak available: 1,963;
- exact duplicate-image records: 2.

Leading automated review triggers overlap. Counts include 418 possible multiple
components, 230 alpha multimodal colours, 219 HSV multimodal colours, 208
exposure-filtered multimodal colours, 146 warm-exclusion/cast candidates, 113
high near-white-fraction cases, 95 low-mask-coverage cases, 93 possible
overexposures based separately on actual 255-valued pixels,
30 cool/blue candidates, 11 alpha-vs-HSV disagreements above ΔE76 10, and 7
dark-dominated masks.

The 834 QC sets contain every non-`ok` row, all 39 legacy-missing rows, the top
20 legacy changes, top 20 warm diagnostic values, top 20 cool diagnostic values,
and an additional deterministic 30-image stratified sample after deduplication.
`qc_sampled=true` means a panel was generated; it does not mean a human approved
the image.

### Estimator comparison

Every row now uses one comparable CIELAB ΔE76 metric; signed mean
ΔL*/Δa*/Δb* is retained in the CSV rather than mixing RGB and Lab distances.

| Comparison | n | Median ΔE76 | 95th percentile | Maximum |
|---|---:|---:|---:|---:|
| legacy vs v2 primary | 1,926 | 3.34 | 10.33 | 27.58 |
| mean vs median | 1,965 | 2.10 | 6.17 | 17.44 |
| median vs HSV joint peak, ΔE76 | 1,965 | 2.30 | 12.19 | 33.81 |
| median vs exposure-filtered peak, ΔE76 | 1,963 | 2.28 | 11.78 | 30.13 |
| HSV vs alpha peak, ΔE76 | 1,965 | 0.00 | 0.00 | 30.76 |

The fixed-bin estimator is bin-width dependent by construction. Its parameter
sensitivity has not yet been physically validated, so all peak outputs remain
experimental sensitivity estimates.

In an explicit `ok`-only sensitivity filter, the primary median and all three
peak branches each retained 1,180 observations. Their descriptive
apparent-colour PC1 correlation with the primary median was 0.993726 for the
HSV peak, 0.993747 for the exposure-filtered peak, and 0.993729 for the alpha
peak. This four-branch diagnostic is not the full six-method canonical
comparison, and PC1 is not a modeled or cross-validated outcome. High aggregate
agreement does not remove the large image-level discrepancies shown above.

## White balance and methodological novelty

A physically calibrated retrospective WB is not identifiable from these
processed cut-outs alone. Target-free algorithms could produce
assumption-dependent normalization, but cannot uniquely recover the illuminant
and camera response. White/cream/pink petals are the target trait, so using
their own mean or maximum as a neutral reference would define away the trait.
The transparent cut-outs also remove most scene information needed by
gray-world/gray-edge assumptions.

The general idea of a robust central colour or joint Lab density peak is not by
itself new. Existing work has compared citizen photographs with spectral colour
([Laitly et al. 2021](https://doi.org/10.1002/ece3.7307)), extracted flower
colour from iNaturalist images ([Perez-Udell et al. 2023](https://doi.org/10.1002/aps3.11505)),
used robust CIELAB centres at large scale
([McKenzie et al. 2026](https://doi.org/10.1086/739413)), and applied 3-D CIELAB
density estimation in plant images
([Li et al. 2022](https://doi.org/10.1007/978-1-0716-2537-8_9)).

A defensible methodological contribution would instead be an integrated and
validated workflow for low-chroma white-to-pink flowers: joint colour
estimation, illumination/exposure diagnostics, rejectable QC, measurement
uncertainty, and propagation to ecological inference. Validation still needs
the same flowers photographed under replicated sun/shade/sky/light/device
conditions, with RAW + ColorChecker or spectrophotometric reference. Mean,
channel median, geometric median, k-means, joint mode, and exposure-filtered
mode should be compared by ΔE00, within-flower repeatability, device bias, and
rank preservation. Image-analysis calibration methods such as
[Troscianko & Stevens 2015](https://doi.org/10.1111/2041-210X.12439) illustrate
why linearization and an in-frame standard are needed for physical colour.

Until that validation exists, the defensible phrase is **camera-rendered
apparent corolla colour**. Claims of true reflectance colour, pigment amount,
calibrated colour, or pollinator-perceived colour are unsupported.

## Public rasters

The acquisition registry covers:

- [CHELSA v2.1](https://www.chelsa-climate.org/datasets/chelsa_bioclim),
  1981–2010 climate layers;
- [SoilGrids 2.0](https://docs.isric.org/globaldata/soilgrids/), topsoil means;
- [WorldClim v2.1](https://www.worldclim.org/data/worldclim21.html), 30-second
  elevation;
- [WorldPop 2020](https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2020/JPN/),
  population count and derived density;
- ESA WorldCover 2021, registered but disabled unless an explicit land-cover
  hypothesis needs it.

The enabled public-data contract contains 19 layers: eight CHELSA, eight
SoilGrids, one WorldClim elevation layer, and WorldPop count plus derived
density. A successful canonical run must acquire/cache, align, hash, and
validate all 19 even though only six enter the configured association models.
The runner fails closed on a partial manifest or an empty/mismatched reviewed
cache hash; an intermediate manifest is not an analysis-ready result.

The canonical grid is EPSG:4326, 30 arc-seconds, 128–143°E and 30–42°N. A
configuration/cache/grid fingerprint prevents stale processed rasters from
being mislabeled as current. Temporary TIFFs are atomically promoted. The
registry records licenses; notably, WorldClim is not described as CC BY and
must not be redistributed contrary to its stated terms.

An asset-level metadata audit found that CHELSA `cmimean` is tagged with a 0.1
embedded scale and `kg m-2 month-1`; `terra` applies that embedded scale, so the
registry multiplier remains 1 and the corrected unit is recorded. For
`rsdsmean`, the current provider catalogue declares `W m-2`, the versioned old
asset applies a 0.001 scale and yields values consistent with daily energy, and
the relocated asset applies a different scale. The `swb` file's metadata and
published scale information also remain inconsistent. Both candidates stay
registered but disabled; the pipeline does not guess a physical-unit
conversion pending authoritative asset-level clarification.

The SoilGrids WCS response does not expose a usable NoData flag and encodes
ocean/non-soil as zero. An initial audit found that treating those zeros as data
materially diluted coastal bulk-density values. The reviewed pipeline now:

1. requests 11,000 × 5,400 cells (approximately the provider's 250 m native
   grid over the study extent) with server-side nearest-neighbour sampling;
2. uses `bdod > 0` as one common native-grid soil mask for all eight properties;
3. applies that mask before local 30-second area averaging;
4. verifies a common output missing mask, physically positive bulk density and
   pH, and records mask/cache/output hashes and spatial-library versions.

SoilGrids is still served from a mutable `latest` endpoint. An analysis-ready
registry must therefore content-pin the reviewed cache bytes in
`expected_sha256`. A future provider change then causes a hash failure and
requires an explicit reviewed lock refresh; the code does not silently relabel
new bytes as the same input version.

Hashes have distinct meanings. `cache_sha256` identifies the local artifact
actually consumed (for example a study-window COG subset, WCS response,
extracted archive member, or direct TIFF), not invariably the provider's full
HTTP asset. `processed_sha256` identifies the derived aligned TIFF. The
processing fingerprint additionally records pipeline, registry, processing
code, preparation-script, `terra`, GDAL, PROJ, and GEOS versions so stale
derived files are rejected. Large cache and raster bytes remain outside Git;
the small manifests retain source URL/page, dataset version, license, native
and target geometry, units, resampling/post-processing, checksums, ranges, and
missingness.

## Reviewed reanalysis design

The new canonical orchestration is `scripts/run_reanalysis.R`. It validates the
1,926-row legacy crosswalk, hard-excludes the two unresolved image/coordinate
records, attaches 19 public layers and five retained SDMs by stable ID, and
writes inclusive/strict-QC and photograph/exact-site row flows.

The primary response is a*. L* and b* are modeled only for the primary colour
method as lighting/exposure diagnostics. A common-reference PC1 is projected
from one PCA fitted on the inclusive primary photograph data, but it is retained
only for descriptive method comparison and is neither modeled nor
cross-validated. Old/new and six-estimator comparisons use common IDs. Model
sensitivities include both method-specific and all-method-common cohorts.
Environment-only and environment-plus-Bombus are compared on the same
Bombus-complete rows, with a separate full-cohort environment-only model
retained to expose non-random SDM missingness.

### Configured environmental predictors

The model set is fixed in `config/reanalysis.yml`. It was not selected by
screening the colour outcome. Nine candidate predictors were assessed only for
predictor dependence. Their maximum VIF was 95.56; `chelsa_bio12` and
`chelsa_cmimean` correlated 0.988. Removing the duplicated moisture-balance,
drying-demand, and elevation proxies reduced the maximum VIF to 4.79 (4.66 in
the Bombus-added model). Every run reproduces both candidate and retained VIF,
pairwise-correlation, condition-number, and exclusion records in
`predictor_screening.csv`. Source values are converted as registered and then
standardized within each fitted training cohort/fold.

| Predictor | Source unit | Defensible role in this analysis |
|---|---|---|
| `chelsa_bio10` | °C | Warmest-quarter temperature; growing-season thermal regime. |
| `chelsa_bio12` | kg m⁻² year⁻¹ | Annual precipitation/water input. |
| `soilgrids_bdod_0_5cm` | kg dm⁻³ | Surface bulk density; physical/rooting environment proxy. |
| `soilgrids_nitrogen_0_5cm` | g kg⁻¹ | Surface total nitrogen; fertility proxy. |
| `soilgrids_phh2o_0_5cm` | pH | Surface acidity/chemical environment. |
| `log_worldpop_2020_density` | `log1p` of people km⁻² | Human access/development and citizen-photo sampling-intensity proxy. |

`chelsa_cmimean` is excluded as a duplicate annual-water gradient,
`chelsa_vpdmean` as a temperature-correlated atmospheric-demand proxy, and
`worldclim_elevation_30s` as a temperature-correlated topographic proxy. They
remain in the explicit nine-variable candidate audit. The other enabled layers
stay in the provenance-checked stack but are not silently added to the model.
`Bombus_suitability_sum` is a separate sensitivity predictor, not one of the
six retained environmental/observation-process predictors.
None of these labels establishes a causal pathway; VIF, condition number, and
blocked prediction are diagnostics for dependence and transportability, not
causal identification.

Spatial validation uses deterministic equal-site bands along the principal
geographic axis. Repeated photographs cannot move a site's fold. Pooled Q2 is
relative to each fold's training mean; fold-level and macro RMSE/MAE/Q2 are
also saved. The bands have no spatial buffer, and linear-model standard errors
remain naive diagnostics, so the results are descriptive rather than spatially
corrected causal inference.

### Execution and artifact status

Restore the locked R environment and build the raster cache/manifest before the
canonical run:

```bash
Rscript -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv"); renv::restore(prompt = FALSE)'
Rscript scripts/download_rasters.R
Rscript scripts/prepare_rasters.R --force --no-download
Rscript scripts/run_reanalysis.R \
  --input Data_S1.csv \
  --legacy-input data/legacy/Data_S1_legacy.csv \
  --config config/reanalysis.yml \
  --pipeline config/pipeline.yml \
  --public-raster-manifest data/processed/raster_manifest.csv \
  --raster-registry config/raster_sources.csv \
  --sdm-dir sdm \
  --output-dir results/reanalysis/v2_2_2
```

The final directory should contain row flow/cohorts, colour and legacy
comparisons, PCA loadings/scores for descriptive comparison, raster and
missingness audits, folds, coefficients, fold and aggregate performance,
session information, `report.md`, `run_manifest.yml`, and
`output_manifest.csv`. Large joined inputs, record-level predictions, and the
execution log are reproducible local artifacts ignored by Git; small summary
tables and manifests are trackable.

The canonical command completed from clean commit `c712262`. The output
manifest verifies 49 artifacts: 21 trackable summaries and 28 local ignored
inputs, predictions, or logs. Primary photograph row flow was 1,965 source
rows, two hard-excluded duplicate-photo/coordinate conflicts, 1,963 analysis
photographs, and 1,955 complete rows for the six-predictor environment model.
The corresponding Bombus-complete cohort had 1,887 rows. The exact-site
sensitivity had 1,925 sites before predictor missingness and 1,917 complete
sites.

For apparent a*, blocked Q2 was 0.2156 in the full environment-only cohort,
0.2337 for environment-only on the identical Bombus-complete cohort, and
0.2521 after adding the Bombus suitability sum. Exact-site environment-only Q2
was 0.2324. Inclusive environment-only Q2 ranged from 0.2156 to 0.2234 across
the six colour methods; strict-`ok` values ranged from 0.2138 to 0.2231. This
method stability is useful sensitivity evidence, but it does not establish
physical colour accuracy or causation. Exact values and hashes are in
`results/reanalysis/v2_2_2/`.

## SDM decision

The five retained rasters passed structural checks: EPSG:4326, 1,616 × 1,359,
matching extent/resolution/NA mask, 381,606 valid cells each, unique hashes, and
finite values within 0–1. They are continuous suitability predictions, not
observed species presence or richness.

They were not rebuilt. The repository lacks the occurrence/background data,
predictor snapshots, model objects, tuning results, folds, and evaluation files
needed to reproduce or improve them safely. Their status is therefore:

- structural integrity: verified;
- model-generation reproducibility: `legacy_unverifiable`;
- rebuild: no.

The retained grid is internally aligned but is slightly offset from the new
canonical global-origin grid. Point extraction is valid after CRS checking;
cell-by-cell combination with newly prepared rasters requires explicit
resampling.

## Remaining mathematical and ecological limitations

- PCA is descriptive and data-dependent; it is not a pigment assay.
- Colour measurement error is likely correlated with date, light, elevation,
  photographer, device, and habitat. Environmental associations may therefore
  contain acquisition bias.
- Photographs at one exact coordinate are not necessarily the same individual;
  exact-site aggregation is a sensitivity analysis, not biological identity.
- *Bombus* suitability can be an environmental proxy. Its coefficient cannot be
  interpreted as a pollinator mechanism without independent pollinator data or
  an explicit causal/joint model.
- A covariance-aware variance identity is now available, but variance shares do
  not by themselves establish causal importance.
- A future spatial model needs a Japan-appropriate metric CRS, mesh and prior
  sensitivity, standardized predictors, blocked cross-validation, and joint
  measurement/model uncertainty.
- Manual review of the 785 flagged photographs and resolution of the duplicated
  photo-coordinate pair remain outstanding; the two conflicting rows are
  excluded from analysis in the meantime.
- Physical validation is required before promoting any modal estimator or WB
  method to the primary colour value.

## Defensible ecological interpretation

With these limits, the data can support descriptive and sensitivity analyses of
geographic variation in **apparent, camera-rendered corolla colour** among the
curated photographs. Analyses can compare photograph and explicit site grains,
report candidate colour estimators, and evaluate associations with environmental
rasters. They cannot yet establish pigment concentration, calibrated individual
colour, pollinator causation, evolutionary adaptation, or a mechanistic effect
of human population density.
