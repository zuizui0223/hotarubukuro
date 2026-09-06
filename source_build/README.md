# source_build

Retained builders for public inputs used by the publication pipeline.

## Canonical colour source

- `reproduce_from_zenodo.py` — orchestrates the frozen Zenodo workbook -> generated public table route.
- `extract_color.py` — resolves workbook images and extracts deterministic colour/QC measurements.
- `build_data_s1.py` — materializes the deterministic public observation table.
- `source_contract.py` — validates the generated table against `reproducibility/source_contract.json`.

## Other public-source builders

- `download_rasters.R` / `prepare_rasters.R` — public environmental rasters.
- `fetch_bombus_occurrences.R` / `build_bombus_sdm_mainland.R` — Bombus occurrence and SDM inputs.
- `build_human_raster.R` — public MLIT human/forest context layer.

Derived colour tables, downloaded source archives and analysis outputs are generated locally under ignored cache/results locations rather than committed as inputs.
