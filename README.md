# hotarubukuro

Reproducible preparation and quality control for georeferenced *Campanula
punctata* photograph colours, public environmental rasters, and five retained
legacy *Bombus* suitability rasters.

This repository documents what the current code and data can reproduce. It
does not reproduce or endorse earlier manuscript claims. Expensive SPDE/INLA,
CFA, qGAM, and SDM fitting are intentionally disabled until their model inputs,
priors, validation design, and uncertainty propagation can be audited.

## Current data contract

[`Data_S1.csv`](Data_S1.csv) is the standard input. Version 2.2.2 contains
1,965 photograph observations with stable observation IDs, source-row IDs,
image SHA-256 hashes, source-reference categories, dates, coordinates, colour
summaries, and QC fields.

Raw activity URLs remain in the ignored local extraction workbook for source
verification, but are deliberately excluded from the public CSV because
combining account-linked activity pages with exact dates and coordinates would
create unnecessary linkage risk. `source_reference_type` retains only the
source category.

- `R`, `G`, and `B` are the channel-wise median of the alpha-bounded legacy HSV
  petal mask. They are decoded, uncalibrated, display-referred sRGB values on
  0–255—not reflectance, pigment concentration, or pollinator vision.
- `mean_*`, `median_*`, `hsv_peak_*`,
  `hsv_exposure_filtered_peak_*`, and `alpha_peak_*` are retained separately.
  The joint CIELAB peaks are experimental sensitivity estimates and are not
  silently substituted for the primary median. The exposure-filtered branch
  removes rendered near-white pixels (all channels at least 250) and very dark
  pixels as a sensitivity ablation; this is not proof of clipping or denoised
  ground truth, and it may remove genuine white-petal pixels.
- Automated QC currently marks 1,180 records `ok` and 785
  `manual_review_required`. A QC flag is a review trigger, not proof that a
  flower or lighting condition was classified correctly.
- No white balance was applied. The source images are post-processed sRGB PNG
  cut-outs with no camera WB/exposure metadata, ICC profile, or independent
  neutral reference. Correcting a pale petal by treating the petal itself as
  white would erase the biological white-to-pink contrast under study.
- Coordinates were carried from the source workbook and remapped to photographs
  by worksheet cell and image hash. They were **not** independently
  re-georeferenced because no GPX/photo-timestamp manifest was found. The CRS is
  assumed to be EPSG:4326.
- One exact image is reused at source rows 515 and 569 with different
  coordinates. Both rows remain in the public table for auditability, but the
  analysis entry points hard-exclude both until the identity conflict is
  resolved.

The byte-exact previous six-column input is preserved as
[`data/legacy/Data_S1_legacy.csv`](data/legacy/Data_S1_legacy.csv). Aggregate
old/new and estimator comparisons are in
[`data/processed/colour_method_comparison.csv`](data/processed/colour_method_comparison.csv),
and the hashes and provenance are recorded in
[`data/processed/Data_S1_v2_manifest.json`](data/processed/Data_S1_v2_manifest.json).
Source photographs and the 105 MB source workbook are not redistributed.

## Repository layout

```text
R/                         tested analysis, raster, and SDM helpers
scripts/                   executable colour, analysis, raster, and audit entry points
tests/python/              synthetic image, workbook, coordinate, and export tests
tests/testthat/            R data-contract, colour, grain, raster, SDM, and CLI tests
config/                    colour and public-raster method registries
data/legacy/               preserved previous standard input
legacy/pre_review_ffb2125/ exact pre-review scripts; archival, never executed
data/cache/                ignored download cache
data/processed/            small manifests/comparisons; large rasters are ignored
results/local/             ignored full colour workbooks
results/qc/                ignored masks, overlays, and source-photo QC panels
sdm/                       retained five legacy suitability rasters and manifest
```

The pre-review scripts are retained under `legacy/` with immutable Git blob
IDs and hashes. CI deliberately excludes that directory because it contains
historically mislabelled and non-portable files. All current commands below use
only the reviewed `R/` and `scripts/` implementations. The archive is evidence,
not a second supported workflow: do not source or copy functions from it into a
current run.

## Python setup and colour extraction

Python dependencies are locked in `uv.lock`.

```bash
uv sync --locked --extra excel --extra test
uv run pytest
```

Run against the workbook containing URL, date, coordinates, and in-cell
photographs. Existing outputs are refused unless `--overwrite` is explicit.

```bash
uv run python Code_S2.py \
  --input-workbook "/absolute/path/to/Supplementary_Table_S1.xlsx" \
  --image-column petal \
  --output "/absolute/path/to/results/hotarubukuro_colour_extraction_v2_2_2.xlsx" \
  --qc-dir "/absolute/path/to/results/qc_v2_2_2" \
  --qc-sample-size 30
```

Directory input is also supported:

```bash
uv run python Code_S2.py \
  --input-dir "/absolute/path/to/images" \
  --output "/absolute/path/to/results/colour_v2_2_2.xlsx" \
  --qc-dir "/absolute/path/to/results/qc"
```

The exact HSV, CIELAB peak, exposure, and QC thresholds are versioned in
[`config/color_extraction_v2.json`](config/color_extraction_v2.json) and tested
against the code defaults. Set `SOURCE_DATE_EPOCH` to a fixed Unix timestamp
when byte-identical CSV/QC reruns are required; otherwise `processed_at` records
the actual run time while scientific values remain deterministic. To rebuild a
sanitized public table after reviewing a new derived workbook:

```bash
uv run python scripts/build_data_s1.py \
  --extraction "/absolute/path/to/verified_colour_v2_2_2.xlsx" \
  --output "/absolute/path/to/Data_S1_v2.csv" \
  --manifest "/absolute/path/to/Data_S1_v2_manifest.json" \
  --comparison "/absolute/path/to/colour_method_comparison.csv" \
  --source-workbook "/absolute/path/to/Supplementary_Table_S1.xlsx" \
  --legacy-input data/legacy/Data_S1_legacy.csv \
  --method-config config/color_extraction_v2.json
```

## Coordinate reconstruction

[`Code_S1.py`](Code_S1.py) joins photo timestamps to GPX tracks by explicit
`observation_id`, never by row position. A batch manifest must contain
`observation_id`, `photo_time`, and `gpx_path`; an optional `timezone` column is
supported. Interpolation never crosses GPX segments, large time gaps fail QC,
and output overwrite is refused.

```bash
uv run python Code_S1.py \
  --manifest "/absolute/path/to/photo_gpx_manifest.csv" \
  --timezone Asia/Tokyo \
  --output "/absolute/path/to/georeferenced_photos.csv"
```

This command was not run for the current table because the required GPX and
photo-time inputs are unavailable.

## Lightweight R analysis

R 4.5.2 and the direct/transitive R dependencies are recorded in
[`renv.lock`](renv.lock). Restore them before running the reviewed R workflow:

```bash
Rscript -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv"); renv::restore(prompt = FALSE)'
```

The main entry point validates the input, converts sRGB to CIELAB, calculates a
sign-oriented colour PCA once, preserves photograph grain, extracts SDM values
by record ID, and writes prepared data. The preparation default retains both
`ok` and `manual_review_required`: automated flags partly depend on colour, so
an `ok`-only default could create response-dependent selection. This is not a
substitute for manual approval; report an explicit strict-QC sensitivity run
with `--accepted-qc-status ok`. Independently of colour QC, both unresolved
duplicate-photo/multiple-coordinate records are always excluded. Existing
outputs are refused.

```bash
Rscript scripts/run_analysis.R \
  --input Data_S1.csv \
  --colour-method primary \
  --grain photo \
  --output results/analysis_input_primary.csv \
  --loadings-output results/colour_pca_loadings_primary.csv
```

Sensitivity runs must select a candidate explicitly:

```bash
Rscript scripts/run_analysis.R \
  --input Data_S1.csv \
  --colour-method hsv_peak \
  --output results/analysis_input_hsv_peak.csv \
  --loadings-output results/colour_pca_loadings_hsv_peak.csv
```

Site-grain analysis requires an explicit site column, for example
`--grain site --site-id exact_site_id`; coordinates are never silently
deduplicated. `Bombus_suitability_sum` is the sum of five continuous 0–1 SDM
predictions. It is not species richness, and rows with missing predictions do
not become false zeros.

## Public 30 arc-second rasters

[`config/raster_sources.csv`](config/raster_sources.csv) enables 19 layers:
eight CHELSA v2.1 layers, eight SoilGrids 2.0 topsoil layers, WorldClim v2.1
elevation, and WorldPop 2020 count and density. CHELSA `swb` and `rsdsmean` and
ESA WorldCover are registered but disabled. Downloads are cached outside Git,
hashed, and aligned to the EPSG:4326 30 arc-second grid in
[`config/pipeline.yml`](config/pipeline.yml). Source-specific aggregation is
used: SoilGrids uses area-aware averaging, WorldPop counts use summation,
continuous climate/elevation layers use bilinear interpolation, and categorical
layers use nearest neighbour.

The SoilGrids WCS has no usable NoData flag and represents ocean/non-soil cells
as numeric zero. The workflow therefore requests an approximately native
250 m grid with server-side nearest-neighbour sampling, applies one common
`bdod > 0` soil mask before reprojection, and only then calculates local
30-second area averages. This avoids treating ocean zero as soil and preserves
legitimate zero values in properties such as coarse fragments. The mask rule,
mask hash, request dimensions, cache hash, software versions, output missing
fraction, and processing algorithm version are recorded in the manifest.

The CHELSA `swb` and `rsdsmean` candidates are registered but disabled because
their file metadata, embedded scales, and current provider catalogue do not
support an unambiguous physical interpretation. The pipeline does not guess a
unit conversion. `cmimean` uses the provider-documented `kg m-2 month-1` unit.
SoilGrids uses a mutable `latest` endpoint, so an analysis-ready registry must
content-lock the exact retrieved cache artifact after reviewed acquisition. A
clean future download then fails closed if the provider changes those bytes;
refresh the lock only as an explicit reviewed data-version change.

`cache_sha256` identifies the exact local cache artifact consumed by the
pipeline. Depending on access type, that artifact is a study-window COG subset,
a WCS response, an extracted archive member, or a direct source TIFF; it should
not be described generically as the checksum of the provider's complete HTTP
asset. The registry's non-empty `expected_sha256` must equal the reviewed cache
artifact. The processed checksum is separate, because reprojection libraries
and output encoding can change derived TIFF bytes even when scientific values
and geometry agree.

```bash
Rscript scripts/download_rasters.R --dry-run
Rscript scripts/download_rasters.R
Rscript scripts/prepare_rasters.R

# Rebuild only from reviewed, already populated cache files.
Rscript scripts/prepare_rasters.R --force --no-download
```

Large downloads and processed rasters belong in ignored `data/cache/rasters/`
and `data/processed/rasters/`. Their URL, provider version, license, CRS,
resolution, checksum, processing fingerprint, and output geometry are recorded
in the small, reviewable `data/processed/raster_download_manifest.csv` and
`data/processed/raster_manifest.csv` generated by the scripts.

## Reviewed public-data reanalysis

The canonical reanalysis starts from the new photograph-level colour table,
verifies the preserved legacy-table crosswalk, requires and validates the
complete set of 19 public raster layers and cache/processed hashes, validates
(but does not rebuild) the five retained Bombus SDMs, and joins every predictor
by stable ID. It refuses a partial or misaligned public manifest and refuses to
overwrite a non-empty output directory.

```bash
Rscript scripts/prepare_rasters.R

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

The six colour methods are `primary` (channel-wise median in the HSV petal
mask), `mean` (arithmetic channel mean in that mask), `legacy` (the preserved
old workbook RGB), `hsv_peak` (joint CIELAB peak in the mask),
`hsv_exposure_filtered_peak` (the experimental near-white/dark-filtered peak),
and `alpha_peak` (joint peak over visible alpha foreground). They are kept as
separate measurement sensitivities and compared on common IDs with signed Lab
differences and DeltaE76; they are never blended into one response.

The primary model outcome is display-referred CIELAB a*. L* and b* are modeled
only for the primary method as lighting/exposure diagnostics. A PCA fitted to
the inclusive primary photograph data is used to project a common-reference
apparent-colour PC1 for **descriptive method comparison only**. PC1 is not a
model outcome and is not cross-validated, because a reference PCA fitted to the
full data would leak test-fold outcome information. Inclusive and strict-`ok`
QC cohorts and photograph/exact-site grains remain separate.

Nine candidate variables were screened without using the colour outcome. The
candidate set had maximum VIF 95.56: annual precipitation and CMI correlated
0.988, while temperature, VPD, and elevation represented nearly the same
geographic gradient. The reviewed model therefore retains six predictors with
maximum VIF 4.79 (4.66 after adding the Bombus sensitivity index). Both sets
and all exclusions are fixed in
[`config/reanalysis.yml`](config/reanalysis.yml), and every run writes
`predictor_screening.csv`. Retaining all 19 layers in the validated stack does
not make the other layers implicit covariates.

| Model predictor | Source unit before model standardization | Ecological/observation role |
|---|---|---|
| `chelsa_bio10` | °C | Warmest-quarter thermal regime; a growing-season climate proxy. |
| `chelsa_bio12` | kg m⁻² year⁻¹ | Annual precipitation/water-input regime. |
| `soilgrids_bdod_0_5cm` | kg dm⁻³ | 0–5 cm bulk density; soil physical/rooting environment proxy. |
| `soilgrids_nitrogen_0_5cm` | g kg⁻¹ | 0–5 cm total nitrogen; soil fertility proxy. |
| `soilgrids_phh2o_0_5cm` | pH | 0–5 cm acidity/soil chemical environment. |
| `log_worldpop_2020_density` | `log1p` of people km⁻² | Human access, development, and citizen-photo sampling-intensity proxy; not assumed to be a direct causal ecological driver. |

`chelsa_cmimean`, `chelsa_vpdmean`, and `worldclim_elevation_30s` remain in the
candidate audit but are excluded respectively as redundant with precipitation,
temperature, and the temperature/elevation gradient in these observations.

Predictors and outcomes are standardized from each training cohort (and, in
blocked validation, from each training fold), so coefficients are standardized
descriptive associations rather than effects in the source units. The workflow
records VIF and model-matrix condition numbers; that diagnostic does not make
correlated climate, soil, elevation, population, or *Bombus* surfaces
independent causal variables.

Models are standardized descriptive linear associations with deterministic,
equal-site geographic bands for blocked validation. The full environment-only
cohort is retained, while environment-only versus environment-plus-Bombus is
also compared on the identical Bombus-complete cohort. Colour-method model
comparisons include an all-method common cohort so a method change is not
confounded with a row change. Fold-level, macro, and pooled Q2/RMSE metrics are
written; naive standard errors are diagnostics only. No CFA, SPDE/INLA,
residual qGAM, or SDM refit is run.

The output directory contains row flow, cohorts, reference PCA, colour-method
and old/new comparisons, raster/missingness audits, model coefficients,
fold-level performance, a human-readable report, an execution log, a run
manifest, and SHA-256/shape metadata for every scientific artifact. Large
joined inputs and per-record predictions remain local and ignored by Git.

The reviewed run completed from clean commit `c712262` and is preserved under
[`results/reanalysis/v2_2_2`](results/reanalysis/v2_2_2). It retained 1,955
complete primary photograph records for the six-predictor environment model
and 1,887 records for the identical Bombus-complete comparison. Blocked Q2 for
apparent a* was 0.216 for the full environment-only cohort, 0.234 for
environment-only on the Bombus-complete cohort, and 0.252 after adding the
legacy Bombus suitability sum. The exact-site environment-only sensitivity
used 1,917 complete sites and had Q2 0.232. Environment-only Q2 across the six
colour methods was 0.216–0.223; this stability does not validate any estimator
as physically calibrated colour.

These are predictive/descriptive diagnostics, not causal effects. The modest
Bombus increment can reflect environmental information reused by the legacy
SDMs. Use [`report.md`](results/reanalysis/v2_2_2/report.md),
[`run_manifest.yml`](results/reanalysis/v2_2_2/run_manifest.yml), and
[`output_manifest.csv`](results/reanalysis/v2_2_2/output_manifest.csv) together;
do not quote values from an interrupted or partial replacement directory.

## Retained SDMs

The five files in `sdm/` are internally aligned continuous MaxNet cloglog
suitability rasters on EPSG:4326, with matching extent, 30 arc-second-like
resolution, and NA mask. Their hashes and ranges are in
[`sdm/manifest.csv`](sdm/manifest.csv).

```bash
Rscript scripts/validate_sdm.R
Rscript Code_S4.R
```

They were not rebuilt: occurrence data, fitted models, predictor snapshots,
fold definitions, and evaluation artefacts are absent. Structural validation
therefore passes while model-generation reproducibility remains
`legacy_unverifiable`.

## Continuous integration

GitHub Actions runs only lightweight checks: Python/R syntax, synthetic colour
and no-mask cases, deterministic output, workbook mapping, coordinate matching,
the `Data_S1.csv` contract, sRGB-to-Lab references, PCA direction, QC/missing
handling, record-safe raster joins, *Bombus* semantics, covariance-aware
variance arithmetic, SDM hashes/geometry, and miniature rasters. It does not
download the public raster stack or process private source photographs.

See [`docs/reproducibility_review.md`](docs/reproducibility_review.md) for the
audit decisions, mathematical limitations, and defensible interpretations.
