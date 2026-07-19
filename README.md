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
data/cache/                ignored download cache
data/processed/            small manifests/comparisons; large rasters are ignored
results/local/             ignored full colour workbooks
results/qc/                ignored masks, overlays, and source-photo QC panels
sdm/                       retained five legacy suitability rasters and manifest
```

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

[`config/raster_sources.csv`](config/raster_sources.csv) registers CHELSA v2.1,
SoilGrids 2.0, WorldClim v2.1 elevation, WorldPop 2020 count/density, and an
optional disabled WorldCover source. Downloads are cached outside Git, hashed,
and aligned to the EPSG:4326 30 arc-second grid in
[`config/pipeline.yml`](config/pipeline.yml). Source-specific aggregation is
used: SoilGrids uses area-aware averaging, WorldPop counts use summation,
continuous climate/elevation layers use bilinear interpolation, and categorical
layers use nearest neighbour.

The CHELSA `swb` and `rsdsmean` candidates are registered but disabled because
their file metadata, embedded scales, and current provider catalogue do not
support an unambiguous physical interpretation. The pipeline does not guess a
unit conversion. `cmimean` uses the provider-documented `kg m-2 month-1` unit.
SoilGrids uses a mutable `latest` endpoint, so exact retrieved bytes are
recorded by checksum but cannot be promised for a future clean download without
archiving or pinning that checksum.

```bash
Rscript scripts/download_rasters.R --dry-run
Rscript scripts/download_rasters.R
Rscript scripts/prepare_rasters.R
```

Large downloads and processed rasters belong in ignored `data/cache/rasters/`
and `data/processed/rasters/`. Their URL, provider version, license, CRS,
resolution, checksum, processing fingerprint, and output geometry are recorded
in the small, reviewable `data/processed/raster_download_manifest.csv` and
`data/processed/raster_manifest.csv` generated by the scripts.

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
