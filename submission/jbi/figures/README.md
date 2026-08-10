# Generated JBI figure bundle

The current manuscript figures are generated from checksum-locked analysis artifacts rather than committed as opaque binary files.

Run:

```bash
Rscript scripts/build_jbi_figure_bundle.R --output results/jbi_figure_bundle
Rscript validation/validate_jbi_figure_bundle.R --output results/jbi_figure_bundle
```

The GitHub Actions workflow `.github/workflows/jbi-figure-bundle.yml` restores the exact frozen inputs, renders the four Main figures and uploads the complete bundle as an artifact.

## Main files

- `Figure_1_measurement_two_part_phenotype.{png,pdf}`
- `Figure_2_broad_environment_spatial_template.{png,pdf}`
- `Figure_3_local_focal_bombus_boundaries.{png,pdf}`
- `Figure_4_calibrated_local_departures.{png,pdf}`

The PNG files are 600-dpi review copies. The PDFs are vector submission copies. The generated bundle also contains:

- `figure_manifest.csv` with dimensions, byte sizes and SHA-256 hashes;
- `figure_source_manifest.csv` with hashes of every input table;
- `figure_numerical_lock.csv` with the manuscript-facing values displayed in the figures;
- `figure_bundle_validation.csv` from the independent validator;
- `figure_data/panel_source_index.csv` describing the evidence source for each figure.

Captions are versioned separately in `submission/jbi/JBI_main_figure_captions.md` so wording changes are reviewable without altering graphics.

## Frozen input artifacts

- broad natural template and local-departure evidence: artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`;
- occurrence-referenced focal-pollinator support: artifact `9023137743`, SHA-256 `d04c561b09b652591b9b479f6e26a779bb562c7c1b5f9b14e61d5e7ca8e2794b`;
- local sharp-transition evidence: artifact `9023416810`, SHA-256 `3f7ac07ea90e2b732441a9f80a38ea49871014722d008769370524b947007e34`.

The five-species community-turnover and montane/elevation panels remain Supporting Information and are not inserted into the four Main figures.
